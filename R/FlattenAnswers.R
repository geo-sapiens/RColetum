#' Flatten a GetAnswers result into a single data frame.
#'
#' Converts the output of \code{\link{GetAnswers}} into a single flat
#' \code{data.frame}. Works with any form: if the form is simple (no groups or
#' multivalued fields), \code{GetAnswers} already returns a plain
#' \code{data.frame} and this function returns it unchanged. For forms with
#' groups or multivalued fields, the main data frame and all nested data frames
#' are joined using \code{dplyr::full_join}.
#'
#' When the form contains multivalued groups or fields, each submission may
#' produce multiple rows in the nested data frames. Joining them to the main
#' data frame causes \strong{row duplication}: each combination of main row
#' and nested row is represented. Account for this in any aggregation.
#'
#' @param answers The return value of \code{\link{GetAnswers}}: either a plain
#'   \code{data.frame} (simple form) or a list with a main data frame and
#'   nested data frames (form with groups or multivalued fields).
#'
#' @return A single \code{data.frame}. For simple forms the columns keep their
#'   original names. For forms with groups, main-frame columns are prefixed
#'   with \code{"answer."} and nested-frame columns are prefixed with their
#'   group \code{componentId}.
#'
#' @examples
#' \donttest{
#' answers <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5719)
#' FlattenAnswers(answers)
#'
#' # Pipe-friendly
#' GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5719) |> FlattenAnswers()
#' }
#'
#' @export
FlattenAnswers <- function(answers) {
  if (is.data.frame(answers)) {
    return(answers)
  }
  if (!is.list(answers) || length(answers) < 2 || !is.list(answers[[2]])) {
    stop("'answers' must be the return value of GetAnswers().")
  }
  dictionary <- .buildDictionaryFromNestedDfs(answers[[2]])
  createSingleDataFrame(answers, dictionary)
}

# Reconstruct the parent-child dictionary from the column names of nested data
# frames. Each nested df has a {parentDfName}_id column (foreign key) and a
# {ownDfName}_id column (row identifier). The parent name is inferred by
# finding the _id column that does not match the df's own name.
.buildDictionaryFromNestedDfs <- function(nestedDfs) {
  dictionary <- data.frame(
    dfName = character(),
    parentDfName = character(),
    stringsAsFactors = FALSE
  )

  for (dfName in names(nestedDfs)) {
    df <- nestedDfs[[dfName]]
    if (ncol(df) == 0) next

    idCols <- grep("_id$", names(df), value = TRUE)
    selfCol <- paste0(dfName, "_id")
    parentIdCols <- idCols[idCols != selfCol]

    # main_df_id is the explicit FK to the main df ("answer"), added to avoid
    # collision with relational answer_id (which references another form).
    if ("main_df_id" %in% parentIdCols) {
      parentName <- "answer"
    } else {
      # For relational nested dfs, answer_id holds a reference to another form's
      # submission, not the hierarchical parent FK. Prefer any other _id column.
      nonAnswerParentCols <- parentIdCols[parentIdCols != "answer_id"]
      if (length(nonAnswerParentCols) >= 1) {
        parentIdCol <- nonAnswerParentCols[1]
      } else if (length(parentIdCols) == 1) {
        parentIdCol <- parentIdCols
      } else {
        next
      }
      parentName <- sub("_id$", "", parentIdCol)
    }
    dictionary <- rbind(
      dictionary,
      data.frame(
        dfName = dfName,
        parentDfName = parentName,
        stringsAsFactors = FALSE
      )
    )
  }
  dictionary
}
