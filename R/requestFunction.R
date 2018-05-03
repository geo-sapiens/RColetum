# Request function
# Is used to make all the requests to the webservice.
# Was create to reuse code.

requestFunction <- function(query, token, dictionary = NULL) {
  #### TODO: Adjust conform right URL ####
  # Temporary url
  url <- "http://localhost:86/app_dev.php/api/graphql"

  # Request
  resp <- httr::GET(url = url,
                    config = httr::add_headers(Token = token),
                    query = list(query = query),
                    encode = "json")

  # Get the status code
  status_code <- toString(resp$status_code)
  # Get the json content from the response
  jsonContent <- httr::content(resp, "text", encoding = "UTF-8")

  # Translate the columns of Json, if necessary
  if (!is.null(dictionary)) {
    # Make the labels unique, to work in jsonlite::fromJSON
    dictionary$label <- make.names(dictionary$label,unique = TRUE)

    i <- 1
    n <- nrow(dictionary)
    while (i <= n) {
      jsonContent <- gsub(dictionary[i,1],dictionary[i,2],jsonContent)
      i <- i + 1
    }
  }

  # Convert the response to useful object
  resp <- jsonlite::fromJSON(
    jsonContent,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE
  )

  # Catch some error from API
  if (!identical(status_code,'200')) {
    if (!is.null(resp$code)) {
      stop(paste0('Error ',resp$code,': ',resp$message,'\n'))
    } else {
      stop(paste0('Error ',resp$errors$code,': ',resp$errors$message,'\n'))
    }

  }

  # Catch some another existing error or warning
  if (!is.null(resp$errors$message)) {
    warning(paste0("\nCheck careful the result, because something may wents ",
                   "wrong: \n", resp$errors$message))
  }

  return(resp$data[[1]])
}
