#' Get the form schema of a form.
#'
#' Get the schema of the questions of a especific form in the shape a nested data
#' frame, that contain all the needed information to request the answers of the
#' form.
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
  #### TODO: Adjust conform right URL ####
  # Temporary url
  url <- "http://localhost:86/app_dev.php/api/graphql"

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
  form_definition <- httr::GET(url,
                                 httr::add_headers(Token = token,
                                                   Account = idAccount),
                                 query = list(query = query),
                                 encode = "json")

  #### TODO: Check errors with in the API documentation ####
  # Catch some specific error
  switch(
    toString(form_definition$status_code),
    '404' = stop(
      paste0('Error 404: Something went wrong.',
             ' If the problem persist, please, contact us.')
    ),
    '403' = stop(
      paste0('Error 403: Acess Denied.',
             'Please check your credetials and the valid of your token',
             ' and try again.')
    ),
    '500' = stop(
      paste0('Error 500: Internal Server Error.',
             'Check your token and idAccount if they are correct.')
    )
  )

  # Convert the response to useful object
  form_definition <- jsonlite::fromJSON(httr::content(form_definition, "text", encoding = "UTF-8"))

  # Catch some another existing warning
  if (!is.null(form_definition$errors$message)) {
    warning(paste0("You may used a invalid argument: ", form_definition$errors$message))
  }

  # Return a nested data frame with the form schema
  form_definition <- form_definition$data$form_definition
  return(form_definition)
}
