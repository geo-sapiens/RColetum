requestFunction <- function(query, token) {
  # Request function
  # Is used to make all the requests to the webservice.

  # API's URL
  url <- "https://coletum.com/api/graphql"

  # Request
  resp <- httr::GET(url = url,
                    config = httr::add_headers(Token = token),
                    query = list(query = query),
                    encode = "json")

  # Get the status code
  status_code <- toString(resp$status_code)
  # Get the json content from the response
  jsonContent <- httr::content(resp, "text", encoding = "UTF-8")

  # Convert the response to useful object
  resp <- jsonlite::fromJSON(
    txt = jsonContent,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE
  )

  # Catch some error from API
  if (!identical(status_code, "200")) {
    if (!is.null(resp$code)) {
      stop(paste0("Error ", status_code, ": ", resp$message, "\n"))
    } else {
      if (!is.null(resp$errors)) {
        stop(paste0("Error ", status_code, ": ", resp$errors$message, "\n"))
      } else {
        stop(paste0("Error ", status_code, ": ", resp$error$message, "\n"))
      }
    }

  }

  # Catch some another existing error or warning
  if (!is.null(resp$errors$message)) {
    warning(paste0("\nCheck careful the result, because something may have gone ",
                   "wrong: \n", resp$errors$message))
  }

  return(resp$data[[1]])
}

buildGroupTree <- function(dataFrame) {
  # Builds a named list mapping each group componentId to its direct child
  # group componentIds. Used by prepareAnswerDF to discover sub-groups of
  # empty groups whose rows are never iterated.
  tree <- list()
  for (i in seq_len(nrow(dataFrame))) {
    if (identical(dataFrame$type[i], "group")) {
      parentId <- dataFrame$componentId[i]
      children <- dataFrame$components[[i]]
      childGroups <- children$componentId[children$type == "group"]
      tree[[parentId]] <- childGroups
      tree <- c(tree, buildGroupTree(children))
    }
  }
  return(tree)
}

collectEmptyFields <- function(components, parentName, isRoot = TRUE) {
  # Recursively walks the form structure to collect:
  # - mainCols: root-level single-value field componentIds (go to main df)
  # - nestedDfs: named list of empty data.frames for groups and multivalue fields
  # - dictionary: parent-child relationships for all nested dfs
  mainCols <- character(0)
  nestedDfs <- list()
  dictionary <- data.frame(dfName = character(0),
                           parentDfName = character(0),
                           stringsAsFactors = FALSE)

  for (i in seq_len(nrow(components))) {
    cId   <- components$componentId[i]
    cType <- components$type[i]

    if (identical(cType, "group")) {
      maxVal <- if ("maximum" %in% names(components)) components$maximum[i] else 1L
      isMultivalue <- is.null(maxVal) || is.na(maxVal) || maxVal != 1
      if (!isRoot || isMultivalue) {
        sub <- collectEmptyFields(components$components[i][[1]], cId, TRUE)
        parentFKCol <- paste0(parentName, "_id")
        groupIdCol  <- paste0(cId, "_id")
        allGroupCols <- c(sub$mainCols, parentFKCol, groupIdCol)
        nestedDfs[[cId]] <- as.data.frame(
          setNames(
            replicate(length(allGroupCols), character(0), simplify = FALSE),
            allGroupCols
          ),
          stringsAsFactors = FALSE
        )
        dictionary <- rbind(dictionary,
                            data.frame(dfName = cId,
                                       parentDfName = parentName,
                                       stringsAsFactors = FALSE))
        nestedDfs  <- c(nestedDfs, sub$nestedDfs)
        dictionary <- rbind(dictionary, sub$dictionary)
      } else {
        # Single-value root group: jsonlite::flatten will flatten its fields
        # into dot-notation columns (groupId.fieldId) in the main data frame.
        sub <- collectEmptyFields(components$components[i][[1]], parentName, TRUE)
        mainCols <- c(mainCols, paste0(cId, ".", sub$mainCols))
        nestedDfs <- c(nestedDfs, sub$nestedDfs)
        dictionary <- rbind(dictionary, sub$dictionary)
      }

    } else if (isRoot) {
      maxVal      <- if ("maximum" %in% names(components)) components$maximum[i] else 1L
      isMultivalue <- is.null(maxVal) || is.na(maxVal) || maxVal != 1
      if (isMultivalue) {
        parentFKCol <- paste0(parentName, "_id")
        groupIdCol  <- paste0(cId, "_id")
        allCols <- c(parentFKCol, cId, groupIdCol)
        nestedDfs[[cId]] <- as.data.frame(
          setNames(
            replicate(length(allCols), character(0), simplify = FALSE),
            allCols
          ),
          stringsAsFactors = FALSE
        )
        dictionary <- rbind(dictionary,
                            data.frame(dfName = cId,
                                       parentDfName = parentName,
                                       stringsAsFactors = FALSE))
      } else {
        mainCols <- c(mainCols, cId)
      }
    }
    # Non-root leaf fields are columns within their parent group nested df;
    # they don't need a separate entry here.
  }

  return(list(mainCols = mainCols, nestedDfs = nestedDfs, dictionary = dictionary))
}

buildEmptyAnswerResult <- function(form_structure, groupTree) {
  # Builds the same list structure returned by GetAnswers when there are
  # answers, but with 0 rows everywhere. Used when the API returns no data.
  result <- collectEmptyFields(form_structure, "answer")

  metaCols <- c("userName", "userId", "source",
                "createdAt", "createdAtDevice",
                "createdAtCoordinates.latitude", "createdAtCoordinates.longitude",
                "updatedAt", "updatedAtCoordinates.latitude",
                "updatedAtCoordinates.longitude")
  allMainCols <- c("answer_id", result$mainCols, metaCols)
  mainDf <- as.data.frame(
    setNames(
      replicate(length(allMainCols), character(0), simplify = FALSE),
      allMainCols
    ),
    stringsAsFactors = FALSE
  )

  return(list(dictionary = result$dictionary,
              mainDf    = mainDf,
              nestedDfs = result$nestedDfs))
}

buildQueryFragment <- function(dataFrame, queryFragment = NULL) {
  # Recursively walks the form structure to build the GraphQL answer{...}
  # query fragment. Groups become nested { } blocks; leaf fields become
  # comma-separated componentIds.
  # Returns the fragment as a character string.
  n_components <- nrow(dataFrame)
  for (i in seq_len(n_components)) {

    if (identical(dataFrame$type[i], "group")) {
      queryFragment <- paste0(queryFragment, dataFrame$componentId[i], "{")
      queryFragment <- buildQueryFragment(dataFrame$components[[i]], queryFragment)
      queryFragment <- paste0(queryFragment, "}")
    } else {
      queryFragment <- paste0(queryFragment, dataFrame$componentId[i], ",")
    }
  }
  return(queryFragment)
}

prepareAnswerDF <- function(dataFrame, dataFrameName, groupTree = NULL) {
  # Separates N-cardinality columns from the main data frame.
  #
  # Iterates over every column of the data frame. If a column holds a nested
  # data frame or list (i.e., a group or multivalued field), it is extracted
  # into a separate data frame in nestedDfs. All extracted entries then go
  # through this same procedure to handle further nesting.

  dictionary <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE)
  names(dictionary) <- c("dfName", "parentDfName")

  nestedDfs <- list()

  # TRUE on the first pass (main data frame); FALSE for subsequent nested dfs
  first <- TRUE
  nestedIdx <- 1
  while (first || nestedIdx <= length(nestedDfs)) {
    extractedDfs <- list()

    if (!first) {
      dataFrame <- nestedDfs[[nestedIdx]]
      dataFrameName <- names(nestedDfs[nestedIdx])
    }

    # When processing an empty group, add its child groups to nestedDfs
    # so they are consistently represented even with no answer data.
    if (!first && nrow(dataFrame) == 0 && !is.null(groupTree[[dataFrameName]])) {
      for (childGroup in groupTree[[dataFrameName]]) {
        if (is.null(nestedDfs[[childGroup]])) {
          newEntry <- list(data.frame())
          names(newEntry) <- childGroup
          nestedDfs <- append(nestedDfs, newEntry)
          dictionary <- rbind(dictionary,
                              data.frame("dfName" = childGroup,
                                         "parentDfName" = dataFrameName,
                                         stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE)
        }
      }
    }

    # Extract N-cardinality columns into separate data frames
    nRow <- nrow(dataFrame)
    aux <- NULL
    for (i in seq_len(nRow)) {

      nCol <- length(dataFrame[i, ])
      for (j in seq_len(nCol)) {

        if (is.list(dataFrame[i, j])) {
          aux <- NULL
          columnId <- paste0(dataFrameName, "_id")
          if (is.data.frame(dataFrame[i, j][[1]])) {
            if (nrow(dataFrame[i, j][[1]]) != 0) {
              aux[[1]] <- cbind(dataFrame[i, j][[1]],
                                "temp" = dataFrame[i, columnId],
                                stringsAsFactors = FALSE)
              # Rename just the temp column
              names(aux[[1]])[names(aux[[1]]) == "temp"] <-
                paste0(dataFrameName, "_id")
            }

          } else {
            if (length(dataFrame[i, j][[1]]) != 0) {
              aux[[1]] <- data.frame(dataFrame[i, columnId], dataFrame[i, j],
                                     stringsAsFactors = FALSE)
              names(aux[[1]]) <- c(paste0(dataFrameName, "_id"),
                                   names(dataFrame[j]))
            }
          }

          col <- names(dataFrame[j])
          if (is.null(extractedDfs[[col]])) {
            extractedDfs[[col]] <- list()
          }
          extractedDfs[[col]] <- append(extractedDfs[[col]], aux)
          dictionary <- rbind(dictionary,
                              data.frame("dfName" = names(dataFrame[j]),
                                         "parentDfName" = dataFrameName,
                                         stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE)

        }
      }
    }
    # Bind equal data frames from all rows into one per column
    n <- length(extractedDfs)
    dfNames <- paste0(names(extractedDfs), "_id")

    for (i in seq_len(n)) {
      if (length(extractedDfs[[i]]) == 0) {
        # Nested field with no data across all answers: return empty data frame
        extractedDfs[[i]] <- data.frame()
      } else {
        # Save column order before jsonlite::flatten, which reorders columns
        ordered <- lapply(extractedDfs[[i]], names)
        # Flatten nested data frames (changes original column order)
        extractedDfs[[i]] <- lapply(extractedDfs[[i]], jsonlite::flatten)

        # Reordering the columns
        nDF <- length(ordered)
        for (j in seq_len(nDF)) {
          reordered <-
            unlist(lapply(ordered[[j]],
                          grep,
                          names(extractedDfs[[i]][[j]]),
                          value = TRUE))

          extractedDfs[[i]][[j]] <- dplyr::select(extractedDfs[[i]][[j]], dplyr::all_of(reordered))
        }

        # Bind the data frames and add a row id column
        extractedDfs[[i]] <- do.call(dplyr::bind_rows, extractedDfs[[i]])
        extractedDfs[[i]][dfNames[i]] <- rownames(extractedDfs[[i]])
      }
    }

    # Remove the extracted N-cardinality columns from the current data frame
    if (length(extractedDfs) != 0) {
      dataFrame <- dplyr::select(dataFrame, -dplyr::one_of(names(extractedDfs)))
    }

    if (first) {
      mainDf <- dataFrame
      nestedDfs <- extractedDfs
      first <- FALSE
    } else {
      nestedDfs[[nestedIdx]] <- dataFrame
      nestedDfs <- append(nestedDfs, extractedDfs)
      nestedIdx <- nestedIdx + 1
    }

  }
  dictionary <- dplyr::distinct(dictionary)
  return(list(dictionary = dictionary, mainDf = mainDf, nestedDfs = nestedDfs))
}

searchFormIdByName <- function(nameForm, token) {
  forms <- GetForms(token)
  idForm <- forms$id[forms$name == nameForm]
  n <- length(idForm)

  if (n == 0) {
    stop("Name not found.")
  }
  if (n > 1) {
    stop("More than one result found. FormIds: ", toString(idForm))
  }
  return(as.numeric(idForm))
}

createSingleDataFrame <- function(dataFrame, dictionary) {
  dataFrame <- append(list(answer = dataFrame[[1]]), dataFrame[[2]])
  names(dataFrame[[1]]) <- paste0(names(dataFrame[1]),
                                  ".",
                                  names(dataFrame[[1]]))
  singleDataFrame <- dataFrame[[1]]
  n <- length(dataFrame)

  if (n >= 2) {
    for (i in 2:n) {
      if (ncol(dataFrame[[i]]) == 0) {
        next
      }
      names(dataFrame[[i]]) <- paste0(names(dataFrame[i]),
                                      ".",
                                      names(dataFrame[[i]]))
      parentKey <- paste0(
        dictionary$parentDfName[dictionary$dfName == names(dataFrame[i])],
        ".",
        dictionary$parentDfName[dictionary$dfName == names(dataFrame[i])],
        "_id")
      dFKey <- paste0(
        names(dataFrame[i]),
        ".",
        dictionary$parentDfName[dictionary$dfName == names(dataFrame[i])], "_id")

      # dplyr::full_join's `by` expects setNames(y_column, x_column).
      # parentKey is the join column in the left (x) table;
      # dFKey is the join column in the right (y) table.
      singleDataFrame <- dplyr::full_join(singleDataFrame,
                                          dataFrame[[i]],
                                          by = stats::setNames(dFKey, parentKey),
                                          relationship = "many-to-many")
    }
  }

  return(singleDataFrame)

}

appendDateFilter <- function(filters, field_name, value) {
  # Validates an ISO 8601 date string and appends a "field_name:\"value\","
  # fragment to `filters`. Returns `filters` unchanged if `value` is NULL.
  # Stops with a descriptive error if the date format is invalid.
  if (is.null(value)) {
    return(filters)
  }
  if (!validDate_ISO8601(value)) {
    stop(paste0(
      "The date provided for '", field_name, "' is not in ISO 8601 format. ",
      "Accepted formats are: 'YYYY-MM-DD' or 'YYYY-MM-DDThh:mm:ssTZD'."
    ))
  }
  paste0(filters, field_name, ":\"", value, "\",")
}

validDate_ISO8601 <- function(userDate) {
  if (is.na(userDate)) {
    return(FALSE)
  }
  userDateSize <- nchar(userDate)
  if (userDateSize == nchar("YYYY/MM/DD")) {
    error <- try(as.Date(userDate), silent = TRUE)
    if (identical(class(error), "try-error")) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    # Normalize timezone colon: "+01:00" → "+0100"
    if (identical(substr(userDate, userDateSize - 2, userDateSize - 2), ":")) {
      userDate <- paste0(
        substr(userDate, 1, userDateSize - 3),
        substr(userDate, userDateSize - 1, userDateSize))
      userDateSize <- nchar(userDate)
    }

    # Normalize UTC "Z" suffix: "...Z" → "...+0000"
    if (identical(substr(userDate, userDateSize, userDateSize), "Z")) {
      userDate <- paste0(
        substr(userDate, 1, userDateSize - 1),
        "+0000")
      userDateSize <- nchar(userDate)
    }

    userDate <- try(
      as.POSIXlt(userDate, format = "%Y-%m-%dT%H:%M:%S%z"))
    if (is.na(userDate)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

removeColonDate_ISO8601 <- function(apiDate) {
  # Remove the colon from timezone offsets (e.g. "+01:00" → "+0100")
  # so that R's as.POSIXlt() can parse the result. NA values are preserved.
  sub("([-+]\\d{2}):(\\d{2})$", "\\1\\2", apiDate)
}
