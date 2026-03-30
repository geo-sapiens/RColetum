#' Get all the answers of a form.
#'
#' Get all the current answers of a specific form. This function makes a call
#' to GetFormStructure and uses 2 API quotas.
#'
#' To get more details about the fields provided by the result, please visit the
#' \href{https://coletum.docs.apiary.io/#reference/0/preenchimentos/listar-preenchimentos}{API documentation}.
#'
#' @param token String access token.
#' @param idForm Numeric Id of the required form.
#' @param nameForm String name of the required form. Used only when idForm is
#' not supplied. When this parameter is used, one extra access quota is spent.
#' @param source Optional filter. Is the source of the answer and can use
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
#' @return A list with two elements when the form has groups or multivalued
#' fields: the first element is the main data frame (scalar fields and
#' metadata), the second is a named list of nested data frames linked by
#' \code{answer_id}. For simple forms with no groups or multivalued fields, a
#' single \code{data.frame} is returned directly. Use \code{\link{FlattenAnswers}}
#' to join everything into one flat table.
#' @examples
#' \donttest{
#' GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
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
  groupTree <- buildGroupTree(form_structure)
  componentsId <- buildQueryFragment(form_structure)

  # Applying optionals filters
  filters <- NULL
  if (!is.null(source) ||
      !is.null(createdBefore) ||
      !is.null(createdAfter) ||
      !is.null(createdDeviceBefore) ||
      !is.null(createdDeviceAfter) ||
      !is.null(updatedBefore) ||
      !is.null(updatedAfter))  {

    filters <- ",filters:{"
    if (!is.null(source)) {
      source <- tolower(source)
      # Check if the option is valid
      if (identical(source, "web_public") ||
          identical(source, "web_private") ||
          identical(source, "mobile")) {
        filters <- paste0(filters, "source:", source, ",")
      } else {
        stop(paste0("The option '", source, "' is not available for the ",
                    "filter 'source'. The available options to this ",
                    "filter are: 'web_public' or 'web_private' or ",
                    "'mobile'.")
        )
      }

    }

    filters <- appendDateFilter(filters, "createdBefore",       createdBefore)
    filters <- appendDateFilter(filters, "createdAfter",        createdAfter)
    filters <- appendDateFilter(filters, "createdDeviceBefore", createdDeviceBefore)
    filters <- appendDateFilter(filters, "createdDeviceAfter",  createdDeviceAfter)
    filters <- appendDateFilter(filters, "updatedBefore",       updatedBefore)
    filters <- appendDateFilter(filters, "updatedAfter",        updatedAfter)

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

  # Pre-compute the empty structure for structural reference (used in both paths)
  emptyResult <- buildEmptyAnswerResult(form_structure, groupTree)

  # Check if the form has any answers.
  if (length(resp) == 0) {
    if (length(emptyResult$nestedDfs) == 0) {
      return(emptyResult$mainDf)
    }
    return(list(emptyResult$mainDf, emptyResult$nestedDfs))
  }

  # Save column order before jsonlite::flatten, which reorders columns
  orderNames <- names(resp[[2]])

  # Flatten the nested answer structure (changes original column order)
  resp <- jsonlite::flatten(resp)

  # Reorder columns to match the original field order from the form structure
  reorderNames <- unlist(lapply(orderNames, grep, names(resp), value = TRUE))

  # Append metadata columns at the end
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
  resp <- dplyr::select(resp, reorderNames)

  # Rename the submission identifier to the standard column name
  resp <- dplyr::rename(resp, answer_id = "friendlyId")

  # Separate N-cardinality columns into their own nested data frames
  resp <- prepareAnswerDF(resp, "answer", groupTree)
  resp <- list(mainDf = resp$mainDf, nestedDfs = resp$nestedDfs)

  # Remove colon from timezone offsets in date columns (e.g. "+01:00" → "+0100")
  # so they can be parsed by R's date functions.
  resp$mainDf$createdAtDevice <- removeColonDate_ISO8601(resp$mainDf$createdAtDevice)
  resp$mainDf$createdAt       <- removeColonDate_ISO8601(resp$mainDf$createdAt)
  resp$mainDf$updatedAt       <- removeColonDate_ISO8601(resp$mainDf$updatedAt)

  # Supplement nested dfs that are absent or structureless (0 columns) with
  # correctly-structured empty dfs from the form structure, so the output always
  # has the same column set regardless of which groups happen to have data.
  # Some entries in emptyResult$nestedDfs use simple keys (e.g. "active66434")
  # while the actual resp$nestedDfs stores them under compound dot-notation keys
  # (e.g. "members66433.active66434") because jsonlite::flatten inlined their
  # single-value parent group. Detect this by checking endsWith on the keys.
  for (dfName in names(emptyResult$nestedDfs)) {
    emptyNested <- emptyResult$nestedDfs[[dfName]]
    if (is.null(emptyNested) || ncol(emptyNested) == 0) next
    if (any(endsWith(names(resp$nestedDfs), paste0(".", dfName)))) next
    if (is.null(resp$nestedDfs[[dfName]]) || ncol(resp$nestedDfs[[dfName]]) == 0) {
      resp$nestedDfs[[dfName]] <- emptyNested
    }
  }

  # Return data frames with the answers
  if (length(resp$nestedDfs) > 0) {
    return(structure(list(resp$mainDf, resp$nestedDfs), names = c("", "")))
  } else {
    return(resp$mainDf)
  }

}
