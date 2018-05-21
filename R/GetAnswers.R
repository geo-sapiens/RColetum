#' Get all the answers of a form.
#'
#' Get all the currents answers of a specific form. This function makes a call
#' to GetFormStructure and spent 2 quotas.
#'
#' To get more details about the fields provided by the result, please visit the
#' \href{https://coletum.docs.apiary.io/#reference/0/preenchimentos/listar-preenchimentos}{API documentation}.
#'
#' @param token String access token.
#' @param idForm Numeric Id of the required form.
#' @param nameForm String name of the required form. Just is used when an idForm
#' is not supplied. When this parameter is used, are spent extra one access
#' quota.
#' @param source Optional filter. Is the the source of the answer and can use
#' 'web_public', 'web_private' or 'mobile'.
#' @param createdAfter Optional filter. This parameter filters the answers that
#' were answered after this date. Is acceptable in the ISO8601 format
#' ("YYYY-MM-DD"). Also is possible to specify another format, sending together
#' in a vector in the R especification, for example, "\%d-\%m-\%Y" to
#' "25-10-1995".
#' @param createdBefore Optional filter. This parameter filters the answers
#' that were answered before this date. Is acceptable in the ISO8601 format
#' ("YYYY-MM-DD"). Also is possible to specify another format, sending together
#' in a vector in the R especification, for example, "\%d-\%m-\%Y" to
#' "25-10-1995".
#'
#' @return A list, with one or more data frames.
#' @examples
#' \dontrun{
#' GetAnswers('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 3345)
#' GetAnswers('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', ,'form 123')
#' GetAnswers(token = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9',
#'              nameForm = 'form 123')
#' GetAnswers(token = token,
#'              idForm = idForm,
#'              source = NULL,
#'              createdAfter = "2012-12-20",
#'              createdBefore = c("20-12-2018","%d-%m-%Y")
#'              )
#' GetAnswers(token = token,
#'              idForm = idForm,
#'              source = web_public,
#'              createdAfter = c("20-12-2012","%d-%m-%Y"),
#'              createdBefore = c("20-12-2018","%d-%m-%Y")
#'              )
#' GetAnswers(token = token,
#'              idForm = idForm,
#'              source = web_private,
#'              createdAfter = "2012-12-20",
#'              createdBefore = "2018-12-20",
#'              )
#'}
#' @export

GetAnswers <- function(token,
                       idForm,
                       nameForm = NULL,
                       source = NULL,
                       createdAfter = NULL,
                       createdBefore = NULL) {

    if (missing(idForm)) {
      if (!is.null(nameForm)) {
        idForm <- searchFormIdByName(nameForm,token)
      } else {
        stop('IdForm or nameForm should be provided.')
      }
    } else {
      if (!is.null(nameForm)) {
        warning('The idForm and nameForm are provided. Ignoring the nameForm.')
      }
    }

  form_structure <- GetFormStructure(token,idForm)
  aux <- auxFunction(form_structure)
  componentsId <- aux[[1]]
  # Add substituition to friendlyId to id
  aux[[2]] <- dplyr::bind_rows(c(idComponent = 'friendlyId',
                                 label = 'id',
                                 order = ''),
                               aux[[2]])

  # Applying optionals filters
  filters <- NULL
  if (!is.null(source) |
      !is.null(createdBefore) |
      !is.null(createdAfter)) {

    filters <- ',filters:{'
    if (!is.null(source)) {
      source <- tolower(source)
      # Check if the option is valid
      if (identical(source,'web_public') |
          identical(source,'web_private') |
          identical(source,'mobile')) {
        filters <- paste0(filters,'source:',source,',')
      } else {
        stop(paste0('The option \'',source,'\' are not avaliable for the ',
                    'filter \'source\'. The avaliable options to this ',
                    'filter are: \'web_public\' or \'web_private\' or ',
                    '\'mobile\'.')
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
        metaData{
            friendlyId,
            userName,
            source,
            createdAt,
            createdAtCoordinates
        },
        answer{
        ",componentsId,
    "}
    }
  }"
  )

  # Request
  resp <- requestFunction(query = query, token = token)

  # Get just the data frame populated with the data
  resp <- unname(resp)

  # Check if the form have some answer.
  if (length(resp) == 0) {
    warning("No answers avaliable. Returning NULL")
    return(NULL)
  }

  # Registering the order of the names, because in next step, will lost
  orderNames <- names(resp[[2]])

  # Unnesting the data frame
  ## This function change the original orders of the columns
  resp <- jsonlite::flatten(resp)

  # Reordening the columns names
  reorderNames <- unlist(lapply(orderNames,
                                grep,
                                names(resp), value = TRUE))

  ## Adding the metaData fields
  reorderNames <- c('friendlyId',
                    reorderNames,
                    'userName',
                    'source',
                    'createdAt',
                    'createdAtCoordinates.latitude',
                    'createdAtCoordinates.longitude')
  ### Reordering
  resp <- dplyr::select(resp,reorderNames)

  # Standardization of column id
  resp <- dplyr::rename(resp, answer_id = 'friendlyId')
  # This function will remove the N questions from the principal Data Frame
  resp <- prepareAnswerDF(resp,'answer')

  # Return data frames with the answers
  if (length(resp[[2]]) > 0) {
    return(resp)
  } else {
    return(resp[[1]])
  }

}
