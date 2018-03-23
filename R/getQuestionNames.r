# Auxiliar function
# Is used to get the question name of each answer from the answer schema to be
# possible be used to rename the coluns of dataframe of answers.
#
# Recursively, gets the question names of all components, including from the
# nested components and return a simples vector.

getQuestionNames <- function(schema,  groupName = NULL) {
  arrayName <- vector()
  i <-  1
  nrow <- nrow(schema)
  while (i <= nrow) {

    if (!is.null(schema$label[i]) & !is.na(schema$label[i])) {
      if (is.null(groupName)) {
        arrayName <- append(arrayName, schema$label[i])
      } else {
        arrayName <- append(arrayName, paste0(groupName,'.',schema$label[i]))
      }
    } else {
      if (identical(schema$type[i], 'group')) {
        if (is.null(groupName)) {
          arrayName <- append(arrayName,
                              getQuestionNames(schema$components[i][[1]],
                                               schema$name[i]))
        } else {
          arrayName <- append(arrayName,
                              getQuestionNames(schema$components[i][[1]],
                                               paste0(groupName,'.',
                                                      schema$name[i])))
        }
      }
    }

    i <- i + 1
  }

  return(arrayName)
}
