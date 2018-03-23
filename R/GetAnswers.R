#' Get all the answers of a form.
#'
#' Get all the currents answers of a especific form. This function make a call
#' to GetAnswerSchema().
#'
#' @param token A string access token.
#' @param idUser Numeric Id of the user.
#' @param idForm Numeric Id of the required form.
#'
#' @return A data frame.
#' @examples
#' GetAnswers('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 1, 3345)
#' @export

GetAnswers <- function(token, idUser, idForm) {
  url <- paste0("http://localhost:86/app_dev.php/api/test/account/",
                idUser,
                "/graphql")

  answer_definition <- GetAnswerSchema(token,idUser,idForm)
  componentsId <- seachIdComponents(answer_definition)

  query <- paste0(
    "{
    answers(formId:",idForm,"){
      answer{
        ",componentsId,
    "}
    }
  }"
  )

  # Request
  resp <- httr::GET(url, query = list(query = query), encode = "json")

  #### TODO: Check errors with in the API documentation ####
  # Catch some specific error
  switch(
    toString(resp$status_code),
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
  resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))

  # Catch some another existing warming
  if (!is.null(resp$errors$message)) {
    warming(paste0("You may used a invalid argument: ", resp$errors$message))
  }

  # Get just the data frame populated with the data
  resp <- resp$data$answers$answer

  #Unnesting the data frame
  resp <- jsonlite::flatten(resp)

  # Rename the coluns, changing the idComponents by the question names
  names(resp) <- getQuestionNames(answer_definition)

  # Return data frame with the answers
  return(resp)
}
