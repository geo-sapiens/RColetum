#' Get the form structure of a form.
#'
#' Get the structure of the questions of a specific form in the shape a nested
#' data frame, that contains all the needed information to request the answers
#' of the form.
#'
#' To get more details about the fields provided by the result, please visit the
#' \href{https://coletum.docs.apiary.io/#reference/0/formularios/definicao-de-formulario}{API documentation}.
#'
#' @param token A string access token.
#' @param idForm Numeric Id of the required form.
#' @param nameForm String name of the required form. Just is used when an idForm
#' are not supplied. When this parameter is used, are spent extra one access
#' quota.
#' @param componentId Optional filter. That is the field identifier, it's
#' possible use to filter to get a specific field.
#'
#' @return A possible nested data frame.
#' @examples
#' \donttest{
#' GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
#' GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg", , "RColetum Test - Iris")
#' GetFormStructure(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'                 nameForm = "RColetum Test - Iris")
#'}
#' @export

GetFormStructure <- function(token, idForm, nameForm = NULL,
                             componentId = NULL) {

  if (missing(idForm)) {
    if (!is.null(nameForm)) {
      idForm <- searchFormIdByName(nameForm, token)
    } else {
      stop("IdForm or nameForm should be provided.")
    }
  } else {
    if (!is.null(nameForm)) {
      warning("The idForm and nameForm are provided. Ignoring the nameForm.")
    }
  }

  # Applying optional filter
  filters <- NULL
  if (!is.null(componentId)) {
    componentId <- toString(componentId)
    filters <- ",filters:{"
    filters <- paste0(filters, "componentId:", componentId)
    filters <- paste0(filters, "}")
  }

  query <- paste0("{
      form_structure(formId:", idForm, filters, ") {
        label,
        componentId,
        type,
        helpBlock,
        order,
        components{
          label,
          componentId,
          type,
          helpBlock,
          order,
          components{
            label,
            type,
            componentId,
            helpBlock,
            order,
            components{
              label,
              componentId,
              type,
              helpBlock,
              order,
              components{
                label,
                componentId,
                type,
                helpBlock,
                order
              },
            },
          },
        },
      }
    }")

  # Request
  resp <- requestFunction(query = query, token = token)

  if (length(resp) == 0) {
    stop("Form not found.")
  }

  return(resp)
}
