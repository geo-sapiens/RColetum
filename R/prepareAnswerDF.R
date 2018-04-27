prepareAnswerDF <- function(dataFrame, dataFrameName) {
  # Moving N question to another place
  otherDF <- list()
  i <- 1
  nRow <- nrow(dataFrame)
  aux <- NULL
  while (i <= nRow ) {

    # the first column is friendly id
    j <- 2
    nCol <- length(dataFrame[i,])
    while (j <= nCol) {

      if (is.list(dataFrame[i,j])) {
        aux <- NULL
        if (is.data.frame(dataFrame[i,j][[1]])) {
          aux[[1]] <- dplyr::mutate(dataFrame[i,j][[1]],parent_cod = dataFrame[i,1])
        } else {
          if (length(dataFrame[i,j][[1]]) != 0) {
            aux[[1]] <- data.frame(dataFrame[i,1],dataFrame[i,j], stringsAsFactors = FALSE)
            names(aux[[1]]) <- c('principal_cod',names(dataFrame[j]))
          }
        }

        otherDF[[names(dataFrame[j])]] <- append(otherDF[[names(dataFrame[j])]],aux)

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
    otherDF[[i]] <- do.call("rbind",otherDF[[i]])
    otherDF[[i]] <- dplyr::mutate(otherDF[[i]],
                                  id = rownames(otherDF[[i]]))
    i <- i + 1
  }

  # Removing the columns with N answers from the principal Data Frame
  if (length(otherDF) != 0) {
    dataFrame <- dplyr::select(dataFrame,-one_of(names(otherDF)))
  }

  return(list(dataFrame,otherDF))
}
