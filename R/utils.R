restGetFunction <- function(url, token, params = NULL) {
  # Makes a GET request to the Coletum REST API v2.
  # Returns the full parsed JSON response (list with $data and $pagination,
  # or a plain object for single-resource endpoints like /forms/{id}).
  base_url <- "https://coletum.com/api/webservice/v2"
  full_url <- paste0(base_url, url)

  resp <- httr::GET(
    url    = full_url,
    config = httr::add_headers(Token = token),
    query  = params
  )

  status_code  <- resp$status_code
  json_content <- httr::content(resp, "text", encoding = "UTF-8")

  parsed <- jsonlite::fromJSON(
    txt               = json_content,
    simplifyVector    = TRUE,
    simplifyDataFrame = TRUE
  )

  if (!identical(as.character(status_code), "200")) {
    msg <- if (!is.null(parsed$message)) {
      parsed$message
    } else if (!is.null(parsed$error$message)) {
      parsed$error$message
    } else {
      toString(status_code)
    }
    stop(paste0("Error ", status_code, ": ", msg, "\n"))
  }

  return(parsed)
}

fetchAllPages <- function(url, token, params, all_pages, page, page_size) {
  # Fetches data from a paginated REST endpoint.
  # If all_pages = TRUE, loops through all pages and returns a combined data.frame.
  # If all_pages = FALSE, returns only the requested page as a data.frame.
  params$page_size <- page_size

  if (!isTRUE(all_pages)) {
    params$page <- page
    resp <- restGetFunction(url, token, params)
    data <- resp$data
    if (is.null(data) || length(data) == 0) return(data.frame())
    return(data)
  }

  params$page <- 1
  all_data <- list()

  repeat {
    resp <- restGetFunction(url, token, params)
    data <- resp$data
    if (!is.null(data) && length(data) > 0) {
      all_data <- c(all_data, list(data))
    }
    if (!isTRUE(resp$pagination$has_next)) break
    params$page <- params$page + 1
  }

  if (length(all_data) == 0) return(data.frame())
  dplyr::bind_rows(all_data)
}

normalizeComponents <- function(components) {
  # Renames v2 component fields to the internal names used by helper functions:
  #   id         → componentId
  #   help_block → helpBlock
  # Applies recursively to nested components (groups).
  if (is.null(components) || !is.data.frame(components) || nrow(components) == 0) {
    return(components)
  }

  if ("id" %in% names(components)) {
    names(components)[names(components) == "id"] <- "componentId"
  }
  if ("help_block" %in% names(components)) {
    names(components)[names(components) == "help_block"] <- "helpBlock"
  }

  if ("components" %in% names(components)) {
    for (i in seq_len(nrow(components))) {
      sub <- components$components[[i]]
      if (!is.null(sub) && is.data.frame(sub) && nrow(sub) > 0) {
        components$components[[i]] <- normalizeComponents(sub)
      }
    }
  }

  return(components)
}

extractCoordinates <- function(df, col_prefix, lng_col, lat_col) {
  # Extracts longitude and latitude from a GeoJSON Point column.
  # col_prefix: base column name, e.g. "metaData.created_at_coordinates"
  # After jsonlite::flatten, GeoJSON {type, coordinates:[lng,lat]} becomes:
  #   col_prefix.type  and  col_prefix.coordinates (list column)
  # Handles missing or all-null coordinate columns by producing NA columns.
  coords_col <- paste0(col_prefix, ".coordinates")
  type_col   <- paste0(col_prefix, ".type")

  if (coords_col %in% names(df)) {
    df[[lng_col]] <- sapply(df[[coords_col]], function(x) {
      if (is.null(x) || length(x) < 2) NA_real_ else as.numeric(x[1])
    })
    df[[lat_col]] <- sapply(df[[coords_col]], function(x) {
      if (is.null(x) || length(x) < 2) NA_real_ else as.numeric(x[2])
    })
    cols_to_drop <- intersect(c(type_col, coords_col), names(df))
    if (length(cols_to_drop) > 0) {
      df <- dplyr::select(df, -dplyr::all_of(cols_to_drop))
    }
  } else if (col_prefix %in% names(df)) {
    df[[lng_col]] <- NA_real_
    df[[lat_col]] <- NA_real_
    df <- dplyr::select(df, -dplyr::all_of(col_prefix))
  } else {
    df[[lng_col]] <- NA_real_
    df[[lat_col]] <- NA_real_
  }
  return(df)
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
        parentFKCol <- if (parentName == "answer") "main_df_id" else paste0(parentName, "_id")
        groupIdCol  <- paste0(cId, "_id")
        allGroupCols <- c(sub$mainCols, parentFKCol, groupIdCol)
        nestedDfs[[cId]] <- as.data.frame(
          stats::setNames(
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
        parentFKCol <- if (parentName == "answer") "main_df_id" else paste0(parentName, "_id")
        groupIdCol  <- paste0(cId, "_id")
        allCols <- c(parentFKCol, cId, groupIdCol)
        nestedDfs[[cId]] <- as.data.frame(
          stats::setNames(
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

  metaCols <- c("created_by_user_name", "created_by_user_id", "created_at_source",
                "created_at",
                "created_at_coordinates.latitude", "created_at_coordinates.longitude",
                "updated_at",
                "updated_at_coordinates.latitude", "updated_at_coordinates.longitude")
  allMainCols <- c("main_df_id", result$mainCols, metaCols)
  mainDf <- as.data.frame(
    stats::setNames(
      replicate(length(allMainCols), character(0), simplify = FALSE),
      allMainCols
    ),
    stringsAsFactors = FALSE
  )

  return(list(dictionary = result$dictionary,
              mainDf    = mainDf,
              nestedDfs = result$nestedDfs))
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
          # For the main df ("answer"), both the FK lookup column and the FK
          # column added to nested dfs are "main_df_id", avoiding collision with
          # relational answer_id (which references another form's submission).
          columnId <- if (dataFrameName == "answer") "main_df_id" else paste0(dataFrameName, "_id")
          if (is.data.frame(dataFrame[i, j][[1]])) {
            if (nrow(dataFrame[i, j][[1]]) != 0) {
              aux[[1]] <- cbind(dataFrame[i, j][[1]],
                                "temp" = dataFrame[i, columnId],
                                stringsAsFactors = FALSE)
              # Rename just the temp column
              names(aux[[1]])[names(aux[[1]]) == "temp"] <- columnId
            }

          } else {
            if (length(dataFrame[i, j][[1]]) != 0) {
              aux[[1]] <- data.frame(dataFrame[i, columnId], dataFrame[i, j],
                                     stringsAsFactors = FALSE)
              names(aux[[1]]) <- c(columnId, names(dataFrame[j]))
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
      dataFrame <- dplyr::select(dataFrame, -dplyr::all_of(names(extractedDfs)))
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
      parentDfN <- dictionary$parentDfName[dictionary$dfName == names(dataFrame[i])]
      parentFKName <- if (parentDfN == "answer") "main_df_id" else paste0(parentDfN, "_id")
      parentKey <- paste0(parentDfN, ".", parentFKName)
      dFKey     <- paste0(names(dataFrame[i]), ".", parentFKName)

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

validDate_ISO8601 <- function(userDate) {
  if (is.na(userDate)) {
    return(FALSE)
  }
  userDateSize <- nchar(userDate)
  if (userDateSize == nchar("YYYY-MM-DD")) {
    # Strict ISO 8601 date: must match YYYY-MM-DD exactly
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", userDate)) {
      return(FALSE)
    }
    parsed <- try(as.Date(userDate, format = "%Y-%m-%d"), silent = TRUE)
    if (identical(class(parsed), "try-error") || any(is.na(parsed))) {
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
