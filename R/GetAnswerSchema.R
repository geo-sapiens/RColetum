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
  url <- paste0("http://XXX.XXX.X.XX/app_dev.php/api/test/account/",
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

  # Convert the response to useful object
  answer_definition <- jsonlite::fromJSON(httr::content(answer_definition, "text", encoding = "UTF-8"))

  # Return a nested data frame with the answer schema
  answer_definition <- answer_definition$data$answer_definition
  return(answer_definition)
}
