requestFunction <- function(query, token) {
  # Request function
  # Is used to make all the requests to the webservice.

  #### TODO: Adjust conform right URL ####
  # Temporary url
  url <- "http://localhost:86/app_dev.php/api/graphql"

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
  if (!identical(status_code,'200')) {
    if (!is.null(resp$code)) {
      stop(paste0('Error ',status_code,': ',resp$message,'\n'))
    } else {
      stop(paste0('Error ',status_code,': ',resp$errors$message,'\n'))
    }

  }

  # Catch some another existing error or warning
  if (!is.null(resp$errors$message)) {
    warning(paste0("\nCheck careful the result, because something may wents ",
                   "wrong: \n", resp$errors$message))
  }

  return(resp$data[[1]])
}

auxFunction <- function(dataFrame, idComponentsString = NULL) {
  # Auxiliar function
  # Is used to get the idComponents and create a dictionary with the componentId
  # and the question name of each answer from the answer schema.
  #
  # The idComponents is necessary to be possible use to get the answers after.
  # The dictionary is necessary to rename the columns from idComponents to labels.
  #
  # Recursively, gets the idComponentes and the question name of all components,
  # including from the nested components.

  dictionary <- data.frame(matrix(ncol = 3, nrow = 0),stringsAsFactors = FALSE)
  names(dictionary) <- c('idComponent', 'label', 'order')
  i <- 1
  nrow <- nrow(dataFrame)
  while (i <= nrow) {

    if (identical(dataFrame$type[i], 'group')) {
      idComponentsString <- paste0(
        idComponentsString,
        dataFrame$componentId[i],
        '{')

      dictionary <- rbind(dictionary,
                          data.frame('idComponent' = dataFrame$componentId[i],
                                     'label' = dataFrame$name[i],
                                     'order' = dataFrame$order[i],
                                     stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE)

      if (is.na(dataFrame$maximum[i])) {
        flagGroupN = TRUE
      } else {
        flagGroupN = FALSE
      }

      aux <- auxFunction(dataFrame$components[i][[1]],
                         idComponentsString)

      idComponentsString <- aux[[1]]
      idComponentsString <- paste0(idComponentsString,'}')

      dictionary <- rbind(dictionary,aux[[2]],
                          stringsAsFactors = FALSE)

    } else {
      if (identical(toString(dataFrame$type[i]), 'separatorfield')) {
        # do nothing, because isn't a question on form.
      } else {
        idComponentsString <- paste0(idComponentsString,
                                     dataFrame$componentId[i],",")

        dictionary <- rbind(dictionary,
                            data.frame('idComponent' = dataFrame$componentId[i],
                                       'label' = dataFrame$label[i],
                                       'order' = dataFrame$order[i],
                                       stringsAsFactors = FALSE),
                            stringsAsFactors = FALSE)
      }
    }

    i <- i + 1
  }
  return(list(idComponentsString,dictionary))
}

prepareAnswerDF <- function(dataFrame, dataFrameName) {
  # This function separeted the questions N from the principal data frame
  #
  # The main loop, pass through all the register in the data frame and verify if
  # is another data frame or a list. In both cases, this element is moved to the
  # another list called complementaryDF. All elements in the complementary DF
  # pass through this procediment too.

  complementaryDF <- list()

  first <- TRUE
  otherI <- 1
  while (first | otherI <= length(complementaryDF)) {
    otherDF <- list()

    if (!first) {
      dataFrame <- complementaryDF[[otherI]]
      dataFrameName <- names(complementaryDF[otherI])
    }

    # Moving N question to another place
    i <- 1
    nRow <- nrow(dataFrame)
    aux <- NULL
    while (i <= nRow) {

      j <- 1
      nCol <- length(dataFrame[i,])
      while (j <= nCol) {

        if (is.list(dataFrame[i,j])) {
          aux <- NULL
          if (is.data.frame(dataFrame[i,j][[1]])) {
            # aux[[1]] <- dplyr::mutate(dataFrame[i,j][[1]],
            #                           parent_cod = dataFrame[i,"id"])
            if (nrow(dataFrame[i,j][[1]]) != 0) {
              aux[[1]] <- cbind(dataFrame[i,j][[1]],
                                'temp' = dataFrame[i,"id"],
                                stringsAsFactors = FALSE)
              # Rename just the temp column
              names(aux[[1]])[names(aux[[1]]) == 'temp'] <-
                paste0(dataFrameName,'_cod')
            }

          } else {
            if (length(dataFrame[i,j][[1]]) != 0) {
              aux[[1]] <- data.frame(dataFrame[i,"id"],dataFrame[i,j],
                                     stringsAsFactors = FALSE)
              names(aux[[1]]) <- c(paste0(dataFrameName,'_cod'),
                                   names(dataFrame[j]))
            }
          }

          otherDF[[names(dataFrame[j])]] <- append(otherDF[[names(dataFrame[j])]],
                                                   aux)

        }

        j <- j + 1
      }

      i <- i + 1
    }
    ###################
    # Binding all iqual data frames
    i <- 1
    n <- length(otherDF)

    while (i <= n) {
      otherDF[[i]] <- lapply(otherDF[[i]],jsonlite::flatten)
      otherDF[[i]] <- do.call(dplyr::bind_rows,otherDF[[i]])
      otherDF[[i]] <- dplyr::mutate(otherDF[[i]],
                                    id = rownames(otherDF[[i]]))
      i <- i + 1
    }

    # Removing the columns with N answers from the principal Data Frame
    if (length(otherDF) != 0) {
      dataFrame <- dplyr::select(dataFrame,-dplyr::one_of(names(otherDF)))
    }

    if (first) {
      DFPrincipal <- dataFrame
      complementaryDF <- otherDF
      first <- FALSE
    } else {
      complementaryDF[[otherI]] <- dataFrame
      complementaryDF <- append(complementaryDF,otherDF)
      otherI <- otherI + 1
    }

  }
  return(list(DFPrincipal,complementaryDF))
}

renameColumns <- function(dataFrame, dictionary) {
  # This function rename all the columns names from the componentId to the
  # label of the question, according with the parameter dictionary.
  names(dataFrame[[1]]) <- newNames(names(dataFrame[[1]]),
                                    dictionary)
  names(dataFrame[[2]]) <- newNames(names(dataFrame[[2]]),
                                    dictionary)

  i <- 1
  iMax <- length(dataFrame[[2]])
  while (i <= iMax) {
    names(dataFrame[[2]][[i]]) <- newNames(names(dataFrame[[2]][[i]]),
                                           dictionary)

    i <- i + 1
  }

  return(dataFrame)
}

newNames <- function(oldNames, dictionary) {
  return(sapply(oldNames,
                function(x,dictionary){
                  i <- 1
                  n <- nrow(dictionary)
                  while (i <= n) {
                    x <- gsub(dictionary[i,1],
                              dictionary[i,2],
                              x)
                    i <- i + 1
                  }
                  return(x)
                },
                dictionary))
}
