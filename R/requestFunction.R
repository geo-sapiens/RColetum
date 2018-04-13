# Request function
# Is used to make all the requests to the webservice.
# Was create to reuse code.

requestFunction <- function(query, token, idAccount) {
  #### TODO: Adjust conform right URL ####
  # Temporary url
  url <- "http://localhost:86/app_dev.php/api/graphql"

  # Request
  resp <- httr::GET(url = url,
                    config = httr::add_headers(Token = token,
                                               Account = idAccount),
                    query = list(query = query),
                    encode = "json")

  # Convert the response to useful object
  status_code <- toString(resp$status_code)
  resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))

  # Catch some error from API
  if (!identical(status_code,'200')) {
    stop(paste0('Error ',resp$errors$code,': ',resp$errors$message))
  }

  # Catch some another existing error or warning
  if (!is.null(resp$errors$message)) {
    warning(paste0("Check careful the result, because something may wents ",
                   "wrong: \n", resp$errors$message))
  }

  return(resp$data[[1]])
}
