#' Get the answer schema of a form.
#'
#' Get the schema of the answers of a especific form in the shape a nested data
#' frame, that contain all the needed information to request the answers of the
#' form.
#'
#' @param token A string access token.
#' @param idUser Numeric Id of the user.
#' @param idForm Numeric Id of the required form.
#'
#' @return A possible nested data frame.
#' @examples
#' GetAnswerSchema('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 1, 3345)
#' @export

GetAnswerSchema <- function(token, idUser, idForm) {
  #### TODO: Adjust conform right URL ####
  #### TODO: Send token together in the request ####
  # Temporary url
  url <- paste0("http://localhost:86/app_dev.php/api/test/account/",
                idUser,
                "/graphql")

  #### TODO: Change the query to the conctract form, when avaible. ####
  query <- paste0("{
      answer_definition(formId:",idForm,"){
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
  answer_definition <- httr::GET(url, query = list(query = query), encode = "json")

  #### TODO: Check errors with in the API documentation ####
  # Catch some specific error
  switch(
    toString(answer_definition$status_code),
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
             'Check your token and idUser if they are correct.')
    )
  )

  # Convert the response to useful object
  answer_definition <- jsonlite::fromJSON(httr::content(answer_definition, "text", encoding = "UTF-8"))

  # Catch some another existing warming
  if (!is.null(answer_definition$errors$message)) {
    warming(paste0("You may used a invalid argument: ", answer_definition$errors$message))
  }

  # Return a nested data frame with the answer schema
  answer_definition <- answer_definition$data$answer_definition
  return(answer_definition)
}
