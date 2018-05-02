# Auxiliar function
# Is used to get the idComponents and create a dictionary with the componentId
# and the question name of each answer from the answer schema.
#
# The idComponents is necessary to be possible use to get the answers after.
# The dictionary is necessary to rename the columns from idComponents to labels.
#
# Recursively, gets the idComponentes and the question name of all components,
# including from the nested components.

auxFunction2 <- function(dataFrame, idComponentsString = NULL) {

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

      aux <- auxFunction2(dataFrame$components[i][[1]],
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
