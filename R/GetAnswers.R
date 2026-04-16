#' Get all the answers of a form.
#'
#' Get all the current answers of a specific form. This function calls
#' \code{\link{GetForm}} internally to understand the form structure.
#'
#' To get more details about the fields provided by the result, please visit the
#' \href{https://coletum.com/en_US/webservice/v2/docs}{API documentation}.
#'
#' @param token String access token.
#' @param idForm Numeric Id of the required form.
#' @param nameForm String name of the required form. Used only when idForm is
#'   not supplied. When this parameter is used, one extra access quota is spent.
#' @param source Optional filter. Origin of the answer: \code{"web_public"},
#'   \code{"web_private"} or \code{"mobile"}.
#' @param createdAfter Optional filter. Returns answers created after this date
#'   (exclusive). Accepts ISO 8601 format (\code{"YYYY-MM-DD"} or
#'   \code{"YYYY-MM-DDThh:mm:ssTZD"}).
#' @param createdBefore Optional filter. Returns answers created before this
#'   date (exclusive). Accepts ISO 8601 format.
#' @param updatedAfter Optional filter. Returns answers updated after this date
#'   (exclusive). Accepts ISO 8601 format.
#' @param updatedBefore Optional filter. Returns answers updated before this
#'   date (exclusive). Accepts ISO 8601 format.
#' @param createdBy Optional filter. Returns answers created by the user with
#'   this numeric ID.
#' @param updatedBy Optional filter. Returns answers whose last update was made
#'   by the user with this numeric ID.
#' @param page Integer. Starting page when \code{all_pages = FALSE}. Default: 1.
#' @param page_size Integer. Number of records per API request (max 500).
#'   Default: 100.
#' @param all_pages Logical. If \code{TRUE} (default), fetches all pages
#'   automatically and returns a combined result. If \code{FALSE}, returns
#'   only the page specified by \code{page}.
#'
#' @return A list with two elements when the form has groups or multivalued
#'   fields: the first element is the main data frame (scalar fields and
#'   metadata), the second is a named list of nested data frames linked by
#'   \code{main_df_id}. For simple forms with no groups or multivalued fields, a
#'   single \code{data.frame} is returned directly. Use \code{\link{FlattenAnswers}}
#'   to join everything into one flat table.
#'
#'   Metadata columns in the main data frame:
#'   \code{main_df_id}, \code{created_by_user_name}, \code{created_by_user_id},
#'   \code{created_at_source}, \code{created_at},
#'   \code{created_at_coordinates.latitude},
#'   \code{created_at_coordinates.longitude}, \code{updated_at},
#'   \code{updated_at_coordinates.latitude},
#'   \code{updated_at_coordinates.longitude}.
#' @examples
#' \donttest{
#' GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              nameForm = "RColetum Test - Iris")
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              createdAfter = "2012-12-20",
#'              createdBefore = "2018-12-20")
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              source = "web_public",
#'              createdAfter = "2012-12-20T19:20:30+01:00",
#'              createdBefore = "2018-12-20T19:20:30+01:00")
#' GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
#'              idForm = 5705,
#'              all_pages = FALSE,
#'              page = 1,
#'              page_size = 3)
#'}
#'
#' @export

GetAnswers <- function(token,
                       idForm,
                       nameForm = NULL,
                       source = NULL,
                       createdAfter = NULL,
                       createdBefore = NULL,
                       updatedAfter = NULL,
                       updatedBefore = NULL,
                       createdBy = NULL,
                       updatedBy = NULL,
                       page = 1,
                       page_size = 100,
                       all_pages = TRUE) {

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

  # Get form structure and normalize component field names for internal helpers
  form          <- GetForm(token, idForm)
  form_structure <- normalizeComponents(form$components)
  groupTree     <- buildGroupTree(form_structure)

  # Pre-compute the empty structure (used when no answers are returned)
  emptyResult <- buildEmptyAnswerResult(form_structure, groupTree)

  # Build query parameters
  params <- list()

  if (!is.null(source)) {
    source_val <- tolower(source)
    if (!source_val %in% c("web_public", "web_private", "mobile")) {
      stop(paste0("The option '", source, "' is not available for the filter ",
                  "'source'. The available options are: 'web_public', ",
                  "'web_private' or 'mobile'."))
    }
    params$created_at_source <- source_val
  }

  for (date_arg in list(
    list(val = createdAfter,  name = "createdAfter",  param = "created_after"),
    list(val = createdBefore, name = "createdBefore", param = "created_before"),
    list(val = updatedAfter,  name = "updatedAfter",  param = "updated_after"),
    list(val = updatedBefore, name = "updatedBefore", param = "updated_before")
  )) {
    if (!is.null(date_arg$val)) {
      if (!validDate_ISO8601(date_arg$val)) {
        stop(paste0("The date provided for '", date_arg$name, "' is not in ",
                    "ISO 8601 format. Accepted formats are: 'YYYY-MM-DD' or ",
                    "'YYYY-MM-DDThh:mm:ssTZD'."))
      }
      params[[date_arg$param]] <- date_arg$val
    }
  }

  if (!is.null(createdBy)) params$created_by <- createdBy
  if (!is.null(updatedBy)) params$updated_by <- updatedBy

  # Fetch answers (all pages or a single page)
  resp <- fetchAllPages(
    paste0("/forms/", idForm, "/answers"),
    token, params, all_pages, page, page_size
  )

  # Handle empty result
  if (is.null(resp) || length(resp) == 0 ||
      (is.data.frame(resp) && nrow(resp) == 0)) {
    if (length(emptyResult$nestedDfs) == 0) {
      return(emptyResult$mainDf)
    }
    return(list(emptyResult$mainDf, emptyResult$nestedDfs))
  }

  # Save original answer field order (from form structure) before flatten.
  # resp$answer is a data frame whose column names are the top-level component IDs.
  orderNames <- names(resp$answer)

  # Flatten nested answer/meta_data columns.
  # After flatten, the real API produces:
  #   "id"                            ← answer identifier (friendlyId equivalent)
  #   "answer.<componentId>"          ← root-level field values
  #   "answer.<groupId>.<fieldId>"    ← values inside groups (inlined by flatten)
  #   "meta_data.created_at_source", "meta_data.created_at", etc.
  #   "meta_data.created_at_coordinates"  ← NA when null; GeoJSON list when set
  resp <- jsonlite::flatten(resp)

  # Extract GeoJSON coordinates from meta_data into flat lat/lng columns.
  # When all values are null, flatten keeps a single NA column (col_prefix).
  # When at least one row has coordinates, flatten produces .type + .coordinates.
  resp <- extractCoordinates(
    resp,
    "meta_data.created_at_coordinates",
    "created_at_coordinates.longitude",
    "created_at_coordinates.latitude"
  )
  resp <- extractCoordinates(
    resp,
    "meta_data.updated_at_coordinates",
    "updated_at_coordinates.longitude",
    "updated_at_coordinates.latitude"
  )

  # Strip the "answer." prefix from answer field columns so that
  # prepareAnswerDF, buildGroupTree, and collectEmptyFields see plain
  # component IDs (matching the normalized form_structure keys).
  answer_cols <- grep("^answer\\.", names(resp), value = TRUE)
  names(resp)[names(resp) %in% answer_cols] <- sub("^answer\\.", "", answer_cols)

  # Reorder columns: main_df_id + form fields (original order) + metadata.
  # grep matches both plain IDs and dot-notation group.field IDs.
  reorderNames <- unlist(lapply(orderNames, grep, names(resp), value = TRUE))
  reorderNames <- c(
    "id",
    reorderNames,
    "meta_data.created_by_user_name",
    "meta_data.created_by_user_id",
    "meta_data.created_at_source",
    "meta_data.created_at",
    "created_at_coordinates.latitude",
    "created_at_coordinates.longitude",
    "meta_data.updated_at",
    "updated_at_coordinates.latitude",
    "updated_at_coordinates.longitude"
  )
  resp <- dplyr::select(resp, dplyr::all_of(reorderNames))

  # Rename to the v2 output naming convention
  resp <- dplyr::rename(resp,
    main_df_id           = "id",
    created_by_user_name = "meta_data.created_by_user_name",
    created_by_user_id   = "meta_data.created_by_user_id",
    created_at_source    = "meta_data.created_at_source",
    created_at           = "meta_data.created_at",
    updated_at           = "meta_data.updated_at"
  )

  # Separate N-cardinality columns into their own nested data frames
  resp <- prepareAnswerDF(resp, "answer", groupTree)
  resp <- list(mainDf = resp$mainDf, nestedDfs = resp$nestedDfs)

  # Remove colon from timezone offsets in date columns (e.g. "+01:00" → "+0100")
  # so they can be parsed by R's date functions.
  resp$mainDf$created_at <- removeColonDate_ISO8601(resp$mainDf$created_at)
  resp$mainDf$updated_at <- removeColonDate_ISO8601(resp$mainDf$updated_at)

  # Supplement nested dfs that are absent or structureless (0 columns) with
  # correctly-structured empty dfs from the form structure, so the output always
  # has the same column set regardless of which groups happen to have data.
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
