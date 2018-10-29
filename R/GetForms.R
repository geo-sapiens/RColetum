#' Get info of all forms.
#'
#' Get the principals info of all forms.
#'
#' To get more details about the fields provided by the result, please visit the
#' \href{https://coletum.docs.apiary.io/#reference/0/formularios/listar-formularios}{API documentation}.
#'
#' @param token String access token.
#' @param status Optional filter. That is the state of the form: accept
#' "enabled" or "disabled".
#' @param publicAnswers Optional filter. Indicates if the form is public or
#' not, is possible to use "true" or "false".
#' @param answerTracking Optional filter. Indicates if the form is saving the
#' local of fill, is possible use "true" or "false".
#'
#' @return A data frame.
#' @examples
#' \donttest{
#' GetForms("cizio7xeohwgc8k4g4koo008kkoocwg")
#' GetForms("cizio7xeohwgc8k4g4koo008kkoocwg","enabled","true","true")
#' GetForms(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'            status = "enabled",
#'            publicAnswers = "false",
#'            answerTracking = "true"
#'          )
#'}
#' @export

GetForms <- function(token,
                     status = NULL,
                     publicAnswers = NULL,
                     answerTracking = NULL) {

  # Applying optionals filters
  filters <- NULL
  if (!is.null(status) | !is.null(publicAnswers) | !is.null(answerTracking)) {
    filters <- "(filters:{"

    if (!is.null(status)) {
      status <- tolower(status)
      # Check if the option is valid
      if (identical(status, "enabled") | identical(status, "disabled")) {
        filters <- paste0(filters, "status:", status, ",")
      } else {
        stop(paste0("The option '", status, "' are not avaliable for the ",
                    "filter 'status'. The avaliable options to this filter ",
                    "are: 'enabled' or 'disabled'."
                    )
             )
      }
    }

    if (!is.null(publicAnswers)) {
      publicAnswers <- tolower(publicAnswers)
      # Check if the option is valid
      if (identical(publicAnswers, "true") |
          identical(publicAnswers, "false")) {
        filters <- paste0(filters, "publicAnswers:", publicAnswers, ",")
      } else {
        stop(paste0("The option '", publicAnswers, "' are not avaliable for",
                    " the filter 'publicAnswers'. The avaliable options to",
                    " this filter are: 'true' or 'false'."
                    )
             )
      }
    }

    if (!is.null(answerTracking)) {
      answerTracking <- tolower(answerTracking)
      # Check if the option is valid
      if (identical(answerTracking, "true") |
          identical(answerTracking, "false")) {
        filters <- paste0(filters, "answerTracking:", answerTracking)
      } else {
        stop(paste0("The option '", answerTracking, "' are not avaliable.",
                    "The avaliable options to this filter are: 'true' ",
                    "or 'false'."
                    )
             )
      }
    }
    filters <- paste0(filters, "})")
  }

  query <- paste0("{
      form
      ", filters, "
      {
        id,
        name,
        status,
        category,
        answerTracking,
        publicAnswers
      }
    }")

  # Request
  resp <- requestFunction(query = query, token = token)

  if (length(resp) == 0) {
    warning("No forms avaliable. Returning NULL")
    return(NULL)
  }

  return(resp)
}
