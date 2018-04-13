#' Get all the answers of a form.
#'
#' Get all the currents answers of a specific form. This function make a call
#' to GetFormSchema().
#'
#' @param token A string access token.
#' @param idAccount Numeric Id of the account.
#' @param idForm Numeric Id of the required form.
#' @param formSource Optional filter. Is the origin of the source of the answer
#' Can use 'web_public', 'web_private' and 'mobile'.
#' @param createdAfter Optional filter. This parameter filter the answers
#' that was answered after this date. Is acceptable the ISO8601 format
#' ("YYYY-MM-DD"). Also is possible specify another format, sending together
#' in a vector in the R especification, for example, "%d-%m-%Y" to "25-10-1995".
#' @param createdBefore Optional filter. This parameter filter the answers
#' that was answered before this date. Is acceptable the ISO8601 format
#' ("YYYY-MM-DD"). Also is possible specify another format, sending together
#' in a vector in the R especification, for example, "%d-%m-%Y" to "25-10-1995".
#'
#' @return A data frame.
#' @examples
#' GetAnswers('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 1, 3345)
#' GetAnswers(token = token,idAccount = 1,
#'              idForm = idForm,
#'              formSource = NULL,
#'              createdAfter = "2012-12-20",
#'              createdBefore = c("20-12-2018","%d-%m-%Y")
#'              )
#' GetAnswers(token = token,idAccount = 1,
#'              idForm = idForm,
#'              formSource = web_public,
#'              createdAfter = c("20-12-2012","%d-%m-%Y"),
#'              createdBefore = c("20-12-2018","%d-%m-%Y")
#'              )
#' GetAnswers(token = token,idAccount = 1,
#'              idForm = idForm,
#'              formSource = web_private,
#'              createdAfter = "2012-12-20",
#'              createdBefore = "2018-12-20",
#'              )
#' @export

GetAnswers <- function(token, idAccount, idForm,
                       formSource = NULL,
                       createdBefore = NULL,
                       createdAfter = NULL) {
  #### TODO: Adjust conform right URL ####
  # Temporary url
  url <- "http://localhost:86/app_dev.php/api/graphql"

  form_definition <- GetFormSchema(token,idAccount,idForm)
  aux <- auxFunction(form_definition)
  componentsId <- aux[[1]]

  # Applying optionals filters
  filters <- NULL
  if (!is.null(formSource) |
      !is.null(createdBefore) |
      !is.null(createdAfter)) {

    filters <- ',filters:{'
    if (!is.null(formSource)) {
      formSource <- tolower(formSource)
      # Check if the option is valid
      if (identical(formSource,'web_public') |
          identical(formSource,'web_private') |
          identical(formSource,'mobile')) {
        filters <- paste0(filters,'source:',formSource,',')
      } else {
        stop(paste0('The option \'',formSource,'\' are not avaliable for the ',
                    'filter \'formSource\'. The avaliable options to this ',
                    'filter are: \'web_public\' or \'web_private\' or \'mobile\'.'
                    )
             )
      }

    }

    if (!is.null(createdBefore)) {
      # Check if the option is valid
      if (is.na(createdBefore[2])) {
        error <- try(as.Date(createdBefore))
        if (identical(class(error), "try-error")) {
          stop(error[1])
        } else {
          filters <- paste0(filters,'createdBefore:"',createdBefore,'",')
        }
      } else {
        error <- try(as.Date(createdBefore[1],
                             format = createdBefore[2]))
        if (identical(class(error), "try-error")) {
          stop(error[1])
        } else {
          createdBefore <- as.Date(createdBefore[1],format = createdBefore[2])
          if (is.na(createdBefore)) {
            stop("Invalid data especification format.")
          } else {
            filters <- paste0(filters,'createdBefore:"',createdBefore,'",')
          }
        }
      }
    }

    if (!is.null(createdAfter)) {
      # Check if the option is valid
      if (is.na(createdAfter[2])) {
        error <- try(as.Date(createdAfter))
        if (identical(class(error), "try-error")) {
          stop(error[1])
        } else {
          filters <- paste0(filters,'createdAfter:"',createdAfter,'",')
        }
      } else {
        error <- try(as.Date(createdAfter[1],
                             format = createdAfter[2]))
        if (identical(class(error), "try-error")) {
          stop(error[1])
        } else {
          createdAfter <- as.Date(createdAfter[1],format = createdAfter[2])
          if (is.na(createdAfter)) {
            stop("Invalid data especification format.")
          } else {
            filters <- paste0(filters,'createdAfter:"',createdAfter,'",')
          }
        }
      }
    }
    filters <- paste0(filters,'}')
  }


  query <- paste0(
    "{
      answer(formId:",idForm,filters,"){
      answer{
        ",componentsId,
    "}
    }
  }"
  )

  # Request
  resp <- httr::GET(url = url,
                    config = httr::add_headers(Token = token,
                                               Account = idAccount),
                    query = list(query = query),
                    encode = "json")

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
             'Check your token and idAccount if they are correct.')
    )
  )

  # Convert the response to useful object
  resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))

  # Catch some another existing warning
  if (!is.null(resp$errors$message)) {
    warning(paste0("You may used a invalid argument: ", resp$errors$message))
  }

  # Get just the data frame populated with the data
  resp <- resp$data$answer$answer

  # Unnesting the data frame
  ## This function change the original orders of the columns
  resp <- jsonlite::flatten(resp)

  # Re-ordened the columns to the original order
  resp <- dplyr::select(resp, aux[[3]])

  # Rename the columns, changing the idComponents by the question names
  ## Cases with the repeted names receive a sufix
  names(resp) <- make.names(aux[[2]],unique = TRUE)

  # Return data frame with the answers
  return(resp)
}
