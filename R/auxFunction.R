# Auxiliar function
# Is used to get the idComponents and the question name of each answer from the
# answer schema .
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
                                ' > '),
                         paste0(groupIdComponent,
                                dataFrame$componentId[i],
                                '.'))

      idComponentsString <- aux[1]
      idComponentsString <- paste0(idComponentsString,'}')

      arrayName <- append(arrayName, aux[2])
      arrayIdComponent <- append(arrayIdComponent, aux[3])

    } else {
      switch(
        toString(dataFrame$type[i]),
        'moneyfield' = {
          idComponentsString <- paste0(idComponentsString,
                                       dataFrame$componentId[i],
                                       "{currency,value}",
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
                                       "{coordinates}",
                                       ",")
          arrayName <- append(arrayName, paste0(groupName,
                                                dataFrame$label[i],
                                                '.',
                                                'coordinates'))
          arrayIdComponent <- append(arrayIdComponent,
                                     paste0(groupIdComponent,
                                            dataFrame$componentId[i],
                                            '.',
                                            'coordinates'))
        },
        'separatorfield' = {

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

    i <- i + 1
  }

  arrayName <- unlist(arrayName)
  arrayIdComponent <- unlist(arrayIdComponent)
  return(list(idComponentsString, arrayName, arrayIdComponent))
}
