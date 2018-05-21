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
#' \dontrun{
#' GetFormSchema('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 3345)
#' GetFormSchema('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', ,'form 123')
#' GetFormSchema(token = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9',
#'                 nameForm = 'form 123')
#' }
#' @export

GetFormStructure <- function(token, idForm, nameForm = NULL,
                             componentId = NULL) {

  if (missing(idForm)) {
    if (!is.null(nameForm)) {
      idForm <- searchFormIdByName(nameForm,token)
    } else {
      stop('IdForm or nameForm should be provided.')
    }
  } else {
    if (!is.null(nameForm)) {
      warning('The idForm and nameForm are provided. Ignoring the nameForm.')
    }
  }

  # Applying optional filter
  filters <- NULL
  if (!is.null(componentId)) {
    componentId <- toString(componentId)
    filters <- ',filters:{'
    filters <- paste0(filters,'componentId:',componentId)
    filters <- paste0(filters,'}')
  }

  query <- paste0("{
      form_structure(formId:",idForm,filters,"){
        label,
        name,
        componentId,
        type,
        helpBlock,
        order,
        components{
          label,
          name,
          componentId,
          type,
          helpBlock,
          order,
          components{
            label,
            name,
            type,
            componentId,
            helpBlock,
            order,
            components{
              label,
              name,
              componentId,
              type,
              helpBlock,
              order,
              components{
                label,
                name,
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
