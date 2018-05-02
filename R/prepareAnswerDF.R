prepareAnswerDF <- function(dataFrame, dataFrameName) {
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
      otherDF[[i]] <- do.call("rbind",otherDF[[i]])
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
