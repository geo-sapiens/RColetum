# Auxiliar function
# Is used to get the idComponents and the question name of each answer from the
# answer schema.
#
# The idComponents is necessary to be possible use to get the answers after.
# The arrayIdComponent works to re-ordened the coluns, after flatten.
# The questions names is useful to rename the coluns of dataframe of answers
# in the end.
#
# Recursively, gets the idComponentes and the question name of all components,
# including from the nested components.

auxFunction <- function(dataFrame,
                        idComponentsString = NULL,
                        groupName = NULL,
                        groupIdComponent = NULL) {
  arrayName <- vector()
  arrayIdComponent <- vector()
  arrayIdComponentNValues <- vector()
  i <- 1
  nrow <- nrow(dataFrame)
  while (i <= nrow) {

    if (identical(dataFrame$type[i], 'group')) {
      idComponentsString <- paste0(
        idComponentsString,
        dataFrame$componentId[i],
        '{')

      aux <- auxFunction(dataFrame$components[i][[1]],
                         idComponentsString,
                         paste0(groupName,
                                dataFrame$name[i],
                                '.'),
                         paste0(groupIdComponent,
                                dataFrame$componentId[i],
                                '.'))

      idComponentsString <- aux[1]
      idComponentsString <- paste0(idComponentsString,'}')

      arrayName <- append(arrayName, aux[2])
      arrayIdComponent <- append(arrayIdComponent, aux[3])

    } else {
      # Check if the question accept more than one value.
      ## Case yes, it's gonna be treat after
      if (is.na(dataFrame$maximum[i]) &
          !(identical(toString(dataFrame$type[i]),
                     'agreementfield') |
           identical(toString(dataFrame$type[i]),
                     'ratingfield') |
          identical(toString(dataFrame$type[i]),
                    'separatorfield') |
          identical(toString(dataFrame$type[i]),
                    'selectfield')
          )) {
        idComponentsString <- paste0(idComponentsString,
                                     dataFrame$componentId[i],",")
        arrayIdComponentNValues <- append(arrayIdComponentNValues,
                                          paste0(groupIdComponent,
                                                 dataFrame$componentId[i]))
      } else {
        switch(
          toString(dataFrame$type[i]),
          'moneyfield' = {
            idComponentsString <- paste0(idComponentsString,
                                         dataFrame$componentId[i],
                                         # "{currency,value}",
                                         ",")
            arrayName <- append(arrayName, paste0(groupName,
                                                  dataFrame$label[i],
                                                  '.',
                                                  'currency'))
            arrayName <- append(arrayName, paste0(groupName,
                                                  dataFrame$label[i],
                                                  '.',
                                                  'value'))
            arrayIdComponent <- append(arrayIdComponent,
                                       paste0(groupIdComponent,
                                              dataFrame$componentId[i],
                                              '.',
                                              'currency'))
            arrayIdComponent <- append(arrayIdComponent,
                                       paste0(groupIdComponent,
                                              dataFrame$componentId[i],
                                              '.',
                                              'value'))
          },

          'coordinatefield' = {
            idComponentsString <- paste0(idComponentsString,
                                         dataFrame$componentId[i],
                                         # "{coordinates}",
                                         ",")
            arrayName <- append(arrayName, paste0(groupName,
                                                  dataFrame$label[i],
                                                  '.',
                                                  'x'))
            arrayIdComponent <- append(arrayIdComponent,
                                       paste0(groupIdComponent,
                                              dataFrame$componentId[i],
                                              '.',
                                              'x'))
            arrayName <- append(arrayName, paste0(groupName,
                                                  dataFrame$label[i],
                                                  '.',
                                                  'y'))
            arrayIdComponent <- append(arrayIdComponent,
                                       paste0(groupIdComponent,
                                              dataFrame$componentId[i],
                                              '.',
                                              'y'))
          },
          'separatorfield' = {
            # do nothing, because isn't a question on form.
          },
          {
            idComponentsString <- paste0(idComponentsString,
                                         dataFrame$componentId[i],",")
            arrayName <- append(arrayName, paste0(groupName,dataFrame$label[i]))
            arrayIdComponent <- append(arrayIdComponent,
                                       paste0(groupIdComponent,
                                              dataFrame$componentId[i]))
          }
        )
      }
    }

    i <- i + 1
  }

  arrayName <- unlist(arrayName)
  arrayIdComponent <- unlist(arrayIdComponent)
  arrayIdComponentNValues <- unlist(arrayIdComponentNValues)
  return(list(idComponentsString,
              arrayName,
              arrayIdComponent,
              arrayIdComponentNValues))
}
