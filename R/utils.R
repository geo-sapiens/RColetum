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
    warning(paste0("\nCheck careful the result, because something may wents ",
                   "wrong: \n", resp$errors$message))
  }

  return(resp$data[[1]])
}

buildGroupTree <- function(dataFrame) {
  # Builds a named list mapping each group componentId to its direct child
  # group componentIds. Used by prepareAnswerDF to discover sub-groups of
  # empty groups whose rows are never iterated.
  tree <- list()
  i <- 1
  while (i <= nrow(dataFrame)) {
    if (identical(dataFrame$type[i], "group")) {
      parentId <- dataFrame$componentId[i]
      children <- dataFrame$components[i][[1]]
      childGroups <- children$componentId[children$type == "group"]
      tree[[parentId]] <- childGroups
      tree <- c(tree, buildGroupTree(children))
    }
    i <- i + 1
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
      nestedDfs[[cId]] <- data.frame()
      dictionary <- rbind(dictionary,
                          data.frame(dfName = cId,
                                     parentDfName = parentName,
                                     stringsAsFactors = FALSE))
      sub <- collectEmptyFields(components$components[i][[1]], cId, FALSE)
      nestedDfs  <- c(nestedDfs, sub$nestedDfs)
      dictionary <- rbind(dictionary, sub$dictionary)

    } else if (isRoot) {
      maxVal      <- if ("maximum" %in% names(components)) components$maximum[i] else 1L
      isMultivalue <- is.null(maxVal) || is.na(maxVal) || maxVal != 1
      if (isMultivalue) {
        nestedDfs[[cId]] <- data.frame()
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
  mainDf <- setNames(
    as.data.frame(matrix(nrow = 0, ncol = length(allMainCols))),
    allMainCols
  )

  return(list(dictionary = result$dictionary, mainDf, result$nestedDfs))
}

auxFunction <- function(dataFrame, idComponentsString = NULL) {
  # Auxiliar function
  # Is used to get the idComponents and create a dictionary with the componentId
  # and the question name of each answer from the answer schema.
  #
  # The idComponents is necessary to be possible use to get the answers after.
  # The dictionary is necessary to rename the columns from idComponents to
  # labels.
  #
  # Recursively, gets the idComponentes and the question name of all components,
  # including from the nested components.

  dictionary <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = FALSE)
  names(dictionary) <- c("idComponent", "label", "order")
  i <- 1
  nrow <- nrow(dataFrame)
  while (i <= nrow) {

    if (identical(dataFrame$type[i], "group")) {
      idComponentsString <- paste0(
        idComponentsString,
        dataFrame$componentId[i],
        "{")

      dictionary <- rbind(dictionary,
                          data.frame("idComponent" = dataFrame$componentId[i],
                                     "label" = dataFrame$label[i],
                                     "order" = dataFrame$order[i],
                                     stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE)

      aux <- auxFunction(dataFrame$components[i][[1]],
                         idComponentsString)

      idComponentsString <- aux[[1]]
      idComponentsString <- paste0(idComponentsString, "}")

      dictionary <- rbind(dictionary, aux[[2]],
                          stringsAsFactors = FALSE)

    } else {
      idComponentsString <- paste0(idComponentsString,
                                   dataFrame$componentId[i], ",")

      dictionary <- rbind(dictionary,
                          data.frame("idComponent" = dataFrame$componentId[i],
                                     "label" = dataFrame$label[i],
                                     "order" = dataFrame$order[i],
                                     stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE)
    }

    i <- i + 1
  }
  return(list(idComponentsString, dictionary))
}

prepareAnswerDF <- function(dataFrame, dataFrameName, groupTree = NULL) {
  # This function separeted the questions N from the principal data frame
  #
  # The main loop, pass through all the register in the data frame and verify if
  # is another data frame or a list. In both cases, this element is moved to the
  # another list called complementaryDF. All elements in the complementary DF
  # pass through this procediment too.

  dictionary <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = FALSE)
  names(dictionary) <- c("dfName", "parentDfName")

  complementaryDF <- list()

  first <- TRUE
  otherI <- 1
  while (first || otherI <= length(complementaryDF)) {
    otherDF <- list()

    if (!first) {
      dataFrame <- complementaryDF[[otherI]]
      dataFrameName <- names(complementaryDF[otherI])
    }

    # When processing an empty group, add its child groups to complementaryDF
    # so they are consistently represented even with no answer data.
    if (!first && nrow(dataFrame) == 0 && !is.null(groupTree[[dataFrameName]])) {
      for (childGroup in groupTree[[dataFrameName]]) {
        if (is.null(complementaryDF[[childGroup]])) {
          newEntry <- list(data.frame())
          names(newEntry) <- childGroup
          complementaryDF <- append(complementaryDF, newEntry)
          dictionary <- rbind(dictionary,
                              data.frame("dfName" = childGroup,
                                         "parentDfName" = dataFrameName,
                                         stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE)
        }
      }
    }

    # Moving N question to another place
    i <- 1
    nRow <- nrow(dataFrame)
    aux <- NULL
    while (i <= nRow) {

      j <- 1
      nCol <- length(dataFrame[i, ])
      while (j <= nCol) {

        if (is.list(dataFrame[i, j])) {
          aux <- NULL
          columnId <- paste0(dataFrameName, "_id")
          if (is.data.frame(dataFrame[i, j][[1]])) {
            # aux[[1]] <- dplyr::mutate(dataFrame[i,j][[1]],
            #                           parent_cod = dataFrame[i,"id"])
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
          if (is.null(otherDF[[col]])) {
            otherDF[[col]] <- list()
          }
          otherDF[[col]] <- append(otherDF[[col]], aux)
          dictionary <- rbind(dictionary,
                              data.frame("dfName" = names(dataFrame[j]),
                                         "parentDfName" = dataFrameName,
                                         stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE)

        }

        j <- j + 1
      }

      i <- i + 1
    }
    ###################
    # Binding all iqual data frames
    i <- 1
    n <- length(otherDF)
    dfNames <- paste0(names(otherDF), "_id")

    while (i <= n) {
      if (length(otherDF[[i]]) == 0) {
        # Nested field with no data across all answers: return empty data frame
        otherDF[[i]] <- data.frame()
      } else {
        # Registering the order of the names, because in next step, will lost
        ordered <- lapply(otherDF[[i]], names)
        # Unnesting the data frames
        ## The function flatten changes the original orders of the columns
        otherDF[[i]] <- lapply(otherDF[[i]], jsonlite::flatten)

        # Reordening the columns names
        j <- 1
        nDF <- length(ordered)
        while (j <= nDF) {
          reordered <-
            unlist(lapply(ordered[[j]],
                          grep,
                          names(otherDF[[i]][[j]]),
                          value = TRUE))

          otherDF[[i]][[j]] <- dplyr::select(otherDF[[i]][[j]], dplyr::all_of(reordered))

          j <- j + 1
        }

        # Bind the data frames
        otherDF[[i]] <- do.call(dplyr::bind_rows, otherDF[[i]])
        # Add the id
        otherDF[[i]][dfNames[i]] <- rownames(otherDF[[i]])
      }
      i <- i + 1
    }

    # Removing the columns with N answers from the principal Data Frame
    if (length(otherDF) != 0) {
      dataFrame <- dplyr::select(dataFrame, -dplyr::one_of(names(otherDF)))
    }

    if (first) {
      DFPrincipal <- dataFrame
      complementaryDF <- otherDF
      first <- FALSE
    } else {
      complementaryDF[[otherI]] <- dataFrame
      complementaryDF <- append(complementaryDF, otherDF)
      otherI <- otherI + 1
    }

  }
  dictionary <- dplyr::distinct(dictionary)
  return(list(dictionary = dictionary, DFPrincipal, complementaryDF))
}

searchFormIdByName <- function(nameForm, token) {
  forms <- GetForms(token)
  idForm <- forms$id[forms$name == nameForm]

  switch(format(length(idForm)),
         "0" = {
           stop("Name not found.")
         },
         "1" = {
           idForm <- as.numeric(idForm)
         },
         "2" = {
           stop("More than one result found. FormIds: ", toString(idForm))
         }

  )

  return(idForm)
}

createSingleDataFrame <- function(dataFrame, dictionary) {
  dataFrame <- append(list(answer = dataFrame[[1]]), dataFrame[[2]])
  names(dataFrame[[1]]) <- paste0(names(dataFrame[1]),
                                  ".",
                                  names(dataFrame[[1]]))
  singleDataFrame <- dataFrame[[1]]
  i <- 2
  n <- length(dataFrame)

  while (i <= n) {
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

    singleDataFrame <- dplyr::full_join(singleDataFrame,
                                        dataFrame[[i]],
                                        # Using setNames, is necessery invert
                                        # the order
                                        by = stats::setNames(dFKey, parentKey),
                                        relationship = "many-to-many")

    i <- i + 1
  }

  return(singleDataFrame)

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
    if (identical(substr(userDate, userDateSize - 2, userDateSize - 2), ":")) {
      userDate <- paste0(
        substr(userDate, 1, userDateSize - 3),
        substr(userDate, userDateSize - 1, userDateSize))
      userDateSize <- nchar(userDate)
    } else {
      if (identical(substr(userDate, userDateSize, userDateSize), "Z")) {
        userDate <- paste0(
          substr(userDate, 1, userDateSize - 1),
          "+0000")
        userDateSize <- nchar(userDate)
      }
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

  n <- length(apiDate)
  i <- 1

  while (i <= n) {

    if (!is.na(apiDate[[i]])) {
      apiDateSize <- nchar(apiDate[i])
      apiDate[i] <- paste0(
        substr(apiDate[i], 1, apiDateSize - 3),
        substr(apiDate[i], apiDateSize - 1, apiDateSize))
    }

    i <- i + 1

  }

  return(apiDate)
}
