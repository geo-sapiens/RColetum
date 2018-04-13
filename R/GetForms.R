#' Get infos of all forms.
#'
#' Get the principals infos of all forms.
#'
#' @param token A string access token.
#' @param idAccount Numeric Id of the account.
#' @param status Optional filter. That is the state of the form: accept
#' 'enabled' or 'disabled'.
#' @param public_answers Optinal filter. If the form accept form anonymously, is
#' possible use 'true' or 'false'.
#' @param answer_tracking Optional filter. If the form is saving the local of
#' fill, is possible use 'true' or 'false'.
#'
#' @return A data frame.
#' @examples
#' GetForms('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 5)
#' GetForms('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 5,'enabled','true','true')
#' GetForms(token = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9',
#'          idAccount = 5,
#'          status = 'enabled',
#'          public_answers = 'false',
#'          answer_tracking = 'true'
#'          )
#' @export

GetForms <- function(token, idAccount,
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
        stop(paste0('The option \'',status,'\' are not avaliable for the filter',
                    ' \'status\'. The avaliable options to this filter are: ',
                    '\'enabled\' or \'disabled\'.'
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
        version,
        status,
        category,
        answer_tracking,
        tracking_path,
        public_answers,
        description
      }
    }")

  # Request
  resp <- requestFunction(query = query, token = token, idAccount = idAccount)

  return(resp)
}
