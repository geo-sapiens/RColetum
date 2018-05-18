#' Get info of all forms.
#'
#' Get the principals info of all forms.
#'
#' To get more details about the fields provided by the result, please visit the
#' \href{https://linkapiary}{API documentation}.
#'
#' @param token String access token.
#' @param status Optional filter. That is the state of the form: accept
#' 'enabled' or 'disabled'.
#' @param public_answers Optional filter. Indicates if the form is public or
#' not, is possible to use 'true' or 'false'.
#' @param answer_tracking Optional filter. Indicates if the form is saving the
#' local of fill, is possible use 'true' or 'false'.
#'
#' @return A data frame.
#' @examples
#' \dontrun{
#' GetForms('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9')
#' GetForms('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9','enabled','true','true')
#' GetForms(token = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9',
#'          status = 'enabled',
#'          public_answers = 'false',
#'          answer_tracking = 'true'
#'          )
#'}
#' @export

GetForms <- function(token,
                     status = NULL,
                     public_answers = NULL,
                     answer_tracking = NULL) {

  # Applying optionals filters
  filters <- NULL
  if (!is.null(status) | !is.null(public_answers) | !is.null(answer_tracking)) {
    filters <- '(filters:{'

    if (!is.null(status)) {
      status <- tolower(status)
      # Check if the option is valid
      if (identical(status,'enabled') | identical(status,'disabled')) {
        filters <- paste0(filters,'status:',status,',')
      } else {
        stop(paste0('The option \'',status,'\' are not avaliable for the ',
                    'filter \'status\'. The avaliable options to this filter ',
                    'are: \'enabled\' or \'disabled\'.'
                    )
             )
      }
    }

    if (!is.null(public_answers)) {
      public_answers <- tolower(public_answers)
      # Check if the option is valid
      if (identical(public_answers,'true') |
          identical(public_answers,'false')) {
        filters <- paste0(filters,'public_answers:',public_answers,',')
      } else {
        stop(paste0('The option \'',public_answers,'\' are not avaliable for',
                    ' the filter \'public_answers\'. The avaliable options to',
                    ' this filter are: \'true\' or \'false\'.'
                    )
             )
      }
    }

    if (!is.null(answer_tracking)) {
      answer_tracking <- tolower(answer_tracking)
      # Check if the option is valid
      if (identical(answer_tracking,'true') |
          identical(answer_tracking,'false')) {
        filters <- paste0(filters,'answer_tracking:',answer_tracking)
      } else {
        stop(paste0('The option \'',answer_tracking,'\' ','are not avaliable.',
                    'The avaliable options to this filter are: \'true\' ',
                    'or \'false\'.'
                    )
             )
      }
    }
    filters <- paste0(filters,'})')
  }

  query <- paste0("{
      form
      ",filters,"
      {
        id,
        name,
        status,
        category,
        answer_tracking,
        public_answers
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
