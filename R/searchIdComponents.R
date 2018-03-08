# Is used to get the idComponents of each answer from the answer schema to be
# possible be used to get the answers after.
#
# Recursively, gets the idComponentes of all components, including from the
# nested components.

seachIdComponents <- function(dataFrame, idComponentsString = NULL) {
  i = 1
  nrow <- nrow(dataFrame)
  while (i <= nrow) {

    if (identical(dataFrame[[i,"type"]], 'group')) {
      idComponentsString <- paste0(
        idComponentsString,
        dataFrame[[i,"componentId"]],
        '{')
      idComponentsString <- seachIdComponents(dataFrame[[i,"components"]], idComponentsString)
      idComponentsString <- paste0(idComponentsString,'}')
    } else {
      idComponentsString <- paste0(idComponentsString,dataFrame[[i,"componentId"]],",")
    }

    i <- i + 1
  }
  return(idComponentsString)
}
