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
#' @param singleDataFrame Boolean flag. Indicates the preference to create a
#' single data frame with all the answers. In this case, is possible to have
#' repeated values, according to the multiplicity of relationships.
#' @param source Optional filter. Is the the source of the answer and can use
#' "web_public", "web_private" or "mobile".
#' @param createdAfter Optional filter. This parameter filters the answers that
#' were answered after this date. Is acceptable in the ISO8601 format
#' ("YYYY-MM-DD" or "YYYY-MM-DDThh:mm:ssTZD").
#' @param createdBefore Optional filter. This parameter filters the answers
#' that were answered before this date. Is acceptable in the ISO8601 format
#' ("YYYY-MM-DD" or "YYYY-MM-DDThh:mm:ssTZD").
#' @param createdDeviceAfter Optional filter. This parameter filters the answers
#' that were answered after this date on device time. Is acceptable in the
#' ISO8601 format ("YYYY-MM-DD" or "YYYY-MM-DDThh:mm:ssTZD").
#' @param createdDeviceBefore Optional filter. This parameter filters the
#' answers that were answered before this date on device time. Is acceptable in
#' the ISO8601 format ("YYYY-MM-DD" or "YYYY-MM-DDThh:mm:ssTZD").
#' @param updatedAfter Optional filter. This parameter filters the answers that
#' were updated after this date. Is acceptable in the ISO8601 format
#' ("YYYY-MM-DD" or "YYYY-MM-DDThh:mm:ssTZD").
#' @param updatedBefore Optional filter. This parameter filters the answers
#' that were updated before this date. Is acceptable in the ISO8601 format
#' ("YYYY-MM-DD" or "YYYY-MM-DDThh:mm:ssTZD").
#'
#' @return A list, with one or more data frames.
#' @examples
#' \donttest{
#' GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
#' GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", ,"RColetum Test - Iris", TRUE)
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              nameForm = "RColetum Test - Iris")
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              source = NULL,
#'              createdAfter = "2012-12-20",
#'              createdBefore = "2018-12-20"
#'              )
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              source = NULL,
#'              createdAfter = "2012-12-20",
#'              createdBefore = "2018-12-20T19:20:30+01:00"
#'              )
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              source = NULL,
#'              createdAfter = "2012-12-20T19:20:30Z",
#'              createdBefore = "2018-12-20T19:20:30+01:00"
#'              )
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              source = "web_public",
#'              createdAfter = "2012-12-20T19:20:30+01:00",
#'              createdBefore = "2018-12-20T19:20:30+01:00"
#'              )
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              singleDataFrame = TRUE,
#'              source = "web_private",
#'              createdAfter = "2012-12-20T19:20:30Z",
#'              createdBefore = "2018-12-20T19:20:30Z"
#'              )
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              singleDataFrame = TRUE,
#'              source = "web_private",
#'              createdAfter = "2012-12-20T19:20:30Z",
#'              createdBefore = "2018-12-20T19:20:30Z",
#'              createdDeviceAfter = "2012-12-20T19:20:30Z",
#'              createdDeviceBefore = "2018-12-20T19:20:30Z",
#'              updatedAfter = "2018-05-20T19:20:30Z",
#'              updatedBefore = "2018-06-20T19:20:30Z"
#'              )
#'}
#'
#' @export

GetAnswers <- function(token,
                       idForm,
                       nameForm = NULL,
                       singleDataFrame = FALSE,
                       source = NULL,
                       createdAfter = NULL,
                       createdBefore = NULL,
                       createdDeviceAfter = NULL,
                       createdDeviceBefore = NULL,
                       updatedAfter = NULL,
                       updatedBefore = NULL) {

    if (missing(idForm)) {
      if (!is.null(nameForm)) {
        idForm <- searchFormIdByName(nameForm, token)
      } else {
        stop("IdForm or nameForm should be provided.")
      }
    } else {
      if (!is.null(nameForm)) {
        warning("The idForm and nameForm are provided. Ignoring the nameForm.")
      }
    }

  form_structure <- GetFormStructure(token, idForm)
  aux <- auxFunction(form_structure)
  componentsId <- aux[[1]]
  # Add substituition to friendlyId to id
  aux[[2]] <- dplyr::bind_rows(c(idComponent = "friendlyId",
                                 label = "id",
                                 order = ""),
                               aux[[2]])

  # Applying optionals filters
  filters <- NULL
  if (!is.null(source) |
      !is.null(createdBefore) |
      !is.null(createdAfter) |
      !is.null(createdDeviceBefore) |
      !is.null(createdDeviceAfter) |
      !is.null(updatedBefore) |
      !is.null(updatedAfter))  {

    filters <- ",filters:{"
    if (!is.null(source)) {
      source <- tolower(source)
      # Check if the option is valid
      if (identical(source, "web_public") |
          identical(source, "web_private") |
          identical(source, "mobile")) {
        filters <- paste0(filters, "source:", source, ",")
      } else {
        stop(paste0("The option '", source, "' are not avaliable for the ",
                    "filter 'source'. The avaliable options to this ",
                    "filter are: 'web_public' or 'web_private' or ",
                    "'mobile'.")
        )
      }

    }

    if (!is.null(createdBefore)) {
      # Check if the option is valid
      if (validDate_ISO8601(createdBefore)) {
        filters <- paste0(filters, "createdBefore:\"", createdBefore, "\",")
      } else {
        stop(
          paste0("The informed date is not in ISO 8601 standard format. The ",
                 "avaible formats are: 'YYYY-MM-DD' ou ",
                 "'YYYY-MM-DDThh:mm:ssTZD')"
                 )
          )
      }
    }

    if (!is.null(createdAfter)) {
      # Check if the option is valid
      if (validDate_ISO8601(createdAfter)) {
        filters <- paste0(filters, "createdAfter:\"", createdAfter, "\",")
      } else {
        stop(
          paste0("The informed date is not in ISO 8601 standard format. The ",
                 "avaible formats are: 'YYYY-MM-DD' ou ",
                 "'YYYY-MM-DDThh:mm:ssTZD')"
          )
        )
      }
    }

    if (!is.null(createdDeviceBefore)) {
      # Check if the option is valid
      if (validDate_ISO8601(createdDeviceBefore)) {
        filters <- paste0(filters,
                          "createdDeviceBefore:\"",
                          createdDeviceBefore,
                          "\",")
      } else {
        stop(
          paste0("The informed date is not in ISO 8601 standard format. The ",
                 "avaible formats are: 'YYYY-MM-DD' ou ",
                 "'YYYY-MM-DDThh:mm:ssTZD')"
          )
        )
      }
    }

    if (!is.null(createdDeviceAfter)) {
      # Check if the option is valid
      if (validDate_ISO8601(createdDeviceAfter)) {
        filters <- paste0(filters,
                          "createdDeviceAfter:\"",
                          createdDeviceAfter,
                          "\",")
      } else {
        stop(
          paste0("The informed date is not in ISO 8601 standard format. The ",
                 "avaible formats are: 'YYYY-MM-DD' ou ",
                 "'YYYY-MM-DDThh:mm:ssTZD')"
          )
        )
      }
    }

    if (!is.null(updatedBefore)) {
      # Check if the option is valid
      if (validDate_ISO8601(updatedBefore)) {
        filters <- paste0(filters, "updatedBefore:\"", updatedBefore, "\",")
      } else {
        stop(
          paste0("The informed date is not in ISO 8601 standard format. The ",
                 "avaible formats are: 'YYYY-MM-DD' ou ",
                 "'YYYY-MM-DDThh:mm:ssTZD')"
          )
        )
      }
    }

    if (!is.null(updatedAfter)) {
      # Check if the option is valid
      if (validDate_ISO8601(updatedAfter)) {
        filters <- paste0(filters, "updatedAfter:\"", updatedAfter, "\",")
      } else {
        stop(
          paste0("The informed date is not in ISO 8601 standard format. The ",
                 "avaible formats are: 'YYYY-MM-DD' ou ",
                 "'YYYY-MM-DDThh:mm:ssTZD')"
          )
        )
      }
    }


    filters <- paste0(filters, "}")
  }


  query <- paste0(
    "{
      answer(formId:", idForm, filters, ") {
        metaData{
            friendlyId,
            userName,
            userId,
            source,
            createdAt,
            createdAtDevice,
            createdAtCoordinates
            updatedAt,
            updatedAtCoordinates
        },
        answer{
        ", componentsId,
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
  reorderNames <- c("friendlyId",
                    reorderNames,
                    "userName",
                    "userId",
                    "source",
                    "createdAt",
                    "createdAtDevice",
                    "createdAtCoordinates.latitude",
                    "createdAtCoordinates.longitude",
                    "updatedAt",
                    "updatedAtCoordinates.latitude",
                    "updatedAtCoordinates.longitude")
  ### Reordering
  resp <- dplyr::select(resp, reorderNames)

  # Standardization of column id
  resp <- dplyr::rename(resp, answer_id = "friendlyId")
  # This function will remove the N questions from the principal Data Frame
  resp <- prepareAnswerDF(resp, "answer")

  # Extracting dictionary
  dictionary <- resp$dictionary
  resp$dictionary <- NULL

  # Removing ":" (colon) from resp into dates.
  ## This way is possible parse to a date format.
  resp[[1]]$createdAtDevice <-
    removeColonDate_ISO8601(resp[[1]]$createdAtDevice)
  resp[[1]]$createdAt <- removeColonDate_ISO8601(resp[[1]]$createdAt)
  resp[[1]]$updatedAt <- removeColonDate_ISO8601(resp[[1]]$updatedAt)

  if (singleDataFrame) {
    resp <- createSingleDataFrame(resp, dictionary)
  }

  # Return data frames with the answers
  if (length(resp[[2]]) > 0) {
    return(resp)
  } else {
    return(resp[[1]])
  }

}
