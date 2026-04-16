#' Get form metadata and field structure.
#'
#' Retrieves the metadata and complete component (field) structure of a
#' specific form. This function replaces the former \code{GetFormStructure}.
#'
#' To get more details about the fields provided by the result, please visit the
#' \href{https://coletum.com/en_US/webservice/v2/docs}{API documentation}.
#'
#' @param token A string access token.
#' @param idForm Numeric Id of the required form.
#' @param nameForm String name of the required form. Used only when
#'   \code{idForm} is not supplied. When this parameter is used, one extra
#'   access quota is spent.
#'
#' @return A list with elements: \code{id}, \code{name}, \code{description},
#'   \code{status}, \code{category}, \code{version}, \code{public_answers},
#'   and \code{components} (a nested data frame representing the form's field
#'   schema, where each row is a component with columns \code{id},
#'   \code{type}, \code{label}, \code{help_block}, \code{maximum},
#'   \code{minimum}, \code{visibility}, and — for group components —
#'   \code{components}).
#' @examples
#' \donttest{
#' GetForm("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
#' GetForm("cizio7xeohwgc8k4g4koo008kkoocwg", nameForm = "RColetum Test - Iris")
#' GetForm(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'         nameForm = "RColetum Test - Iris")
#'}
#' @export

GetForm <- function(token, idForm, nameForm = NULL) {

  if (missing(idForm)) {
    if (!is.null(nameForm)) {
      idForm <- searchFormIdByName(nameForm, token)
    } else {
      stop("idForm or nameForm should be provided.")
    }
  } else {
    if (!is.null(nameForm)) {
      warning("Both idForm and nameForm are provided. Ignoring nameForm.")
    }
  }

  resp <- restGetFunction(paste0("/forms/", idForm), token)

  if (is.null(resp) || length(resp) == 0) {
    stop("Form not found.")
  }

  return(resp)
}
