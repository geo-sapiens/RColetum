# Auxiliar function
# Is used to get the idComponents and the question name of each answer from the
# answer schema.

auxFunction2 <- function(dataFrame,
                        idComponentsString = NULL) {

  i <- 1
  nrow <- nrow(dataFrame)
  while (i <= nrow) {

    if (identical(dataFrame$type[i], 'group')) {
      idComponentsString <- paste0(
        idComponentsString,
        dataFrame$componentId[i],
        '{')

      if (is.na(dataFrame$maximum[i])) {
        flagGroupN = TRUE
      } else {
        flagGroupN = FALSE
      }

      aux <- auxFunction2(dataFrame$components[i][[1]],
                         idComponentsString)

      idComponentsString <- aux
      idComponentsString <- paste0(idComponentsString,'}')

    } else {
      switch(
        toString(dataFrame$type[i]),
        'moneyfield' = {
          idComponentsString <- paste0(idComponentsString,
                                       dataFrame$componentId[i],
                                       # "{currency,value}",
                                       ",")
        },

        'coordinatefield' = {
          idComponentsString <- paste0(idComponentsString,
                                       dataFrame$componentId[i],
                                       # "{coordinates}",
                                       ",")
        },
        'separatorfield' = {
          # do nothing, because isn't a question on form.
        },
        {
          idComponentsString <- paste0(idComponentsString,
                                       dataFrame$componentId[i],",")

        }
      )
    }

    i <- i + 1
  }
  return(idComponentsString)
}
