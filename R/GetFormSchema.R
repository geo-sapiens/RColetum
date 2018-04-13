#' Get the form schema of a form.
#'
#' Get the schema of the questions of a specific form in the shape a nested
#' data frame, that contain all the needed information to request the answers
#' of the form.
#'
#' @param token A string access token.
#' @param idAccount Numeric Id of the account.
#' @param idForm Numeric Id of the required form.
#'
#' @return A possible nested data frame.
#' @examples
#' GetFormSchema('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 1, 3345)
#' @export

GetFormSchema <- function(token, idAccount, idForm) {
  #### TODO: Change the query to the conctract form, when avaible. ####
  query <- paste0("{
      form_definition(formId:",idForm,"){
        formId,
        label,
        name,
        type,
        components{
          formId,
          label,
          name,
          type,
          components{
            formId,
            label,
            name,
            type,
            components{
              formId,
              label,
              name,
              type,
              components{
                formId,
                label,
                name,
                type,
                componentId
              },
              componentId
            },
            componentId
          },
          componentId
        },
        componentId
      }
    }")

  # Request
  resp <- requestFunction(query = query, token = token, idAccount = idAccount)

  return(resp)
}
