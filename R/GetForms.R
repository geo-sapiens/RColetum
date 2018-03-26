#' Get infos of all forms.
#'
#' Get the principals infos of all forms.
#'
#' @param token A string access token.
#' @param idUser Numeric Id of the user.
#'
#' @return A data frame.
#' @examples
#' GetForms('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 5)
#' @export

GetForms <- function(token, idUser) {
  #### TODO: Adjust conform right URL ####
  #### TODO: Send token together in the request ####
  # Temporary url
  url <- paste0("http://localhost:86/app_dev.php/api/test/account/",
                idUser,
                "/graphql")

  query <- "{
      forms{
        id,
        name,
        version,
        status,
        category,
        answer_tracking,
        tracking_path,
        public_answers,
        description
      }
    }"

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

  # Catch some another existing warning
  if (!is.null(resp$errors$message)) {
    warning(paste0("You may used a invalid argument: ", resp$errors$message))
  }

  # Return a data frame with the forms infos
  resp <- resp$data$forms
  return(resp)
}
