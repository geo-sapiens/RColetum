#' Get the form structure of a form.
#'
#' Get the structure of the questions of a specific form in the shape a nested
#' data frame, that contains all the needed information to request the answers
#' of the form.
#'
#' @param token A string access token.
#' @param idForm Numeric Id of the required form.
#' @param nameForm String name of the required form. Just is used when an idForm
#' are not supplied. When this parameter is used, are spent extra one access
#' quota.
#'
#' @return A possible nested data frame.
#' @examples
#' \dontrun{
#' GetFormSchema('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 3345)
#' }
#' @export

GetFormStructure <- function(token, idForm, nameForm = NULL) {

  if (missing(idForm)) {
    idForm <- searchFormIdByName(nameForm,token)
  }

  #### TODO: Change the query to the conctract form, when avaible. ####
  query <- paste0("{
      form_definition(formId:",idForm,"){
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
