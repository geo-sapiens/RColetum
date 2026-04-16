#' Get info of all forms.
#'
#' Retrieve information about all forms in the account.
#'
#' To get more details about the fields provided by the result, please visit the
#' \href{https://coletum.com/en_US/webservice/v2/docs}{API documentation}.
#'
#' @param token String access token.
#' @param status Optional filter. State of the form: \code{"enabled"} or
#'   \code{"disabled"}.
#' @param name Optional filter. Returns forms whose name contains this string
#'   (partial match, case-insensitive).
#' @param page Integer. Starting page when \code{all_pages = FALSE}. Default: 1.
#' @param page_size Integer. Number of records per API request (max 500).
#'   Default: 100.
#' @param all_pages Logical. If \code{TRUE} (default), fetches all pages
#'   automatically and returns a combined data frame. If \code{FALSE}, returns
#'   only the page specified by \code{page}.
#'
#' @return A data frame with columns: \code{id}, \code{name},
#'   \code{description}, \code{status}, \code{category}, \code{version},
#'   \code{public_answers}. Returns \code{NULL} with a warning if no forms are
#'   found.
#' @examples
#' \donttest{
#' GetForms("cizio7xeohwgc8k4g4koo008kkoocwg")
#' GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", status = "enabled")
#' GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", name = "Iris")
#' GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", page_size = 3, all_pages = FALSE)
#'}
#' @export

GetForms <- function(token,
                     status = NULL,
                     name = NULL,
                     page = 1,
                     page_size = 100,
                     all_pages = TRUE) {

  params <- list()

  if (!is.null(status)) {
    status <- tolower(status)
    if (!identical(status, "enabled") && !identical(status, "disabled")) {
      stop(paste0("The option '", status, "' is not available for the filter ",
                  "'status'. The available options are: 'enabled' or 'disabled'."))
    }
    params$status <- status
  }

  if (!is.null(name)) {
    params$name <- name
  }

  data <- fetchAllPages("/forms", token, params, all_pages, page, page_size)

  if (is.null(data) || length(data) == 0 ||
      (is.data.frame(data) && nrow(data) == 0)) {
    warning("No forms available. Returning NULL")
    return(NULL)
  }

  return(data)
}
