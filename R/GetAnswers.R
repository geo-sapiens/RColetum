#' Get all the answers of a form.
#'
#' Get all the currents answers of a specific form. This function make a call
#' to GetFormSchema().
#'
#' @param token A string access token.
#' @param idForm Numeric Id of the required form.
#' @param repetedColunsNames Boolean flag, indicates if the repeted columns
#' names will stay or if gonna be rename with a suffix.
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
#' @return A list, with one or more data frames.
#' @examples
#' GetAnswers('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 3345)
#' GetAnswers(token = token,
#'              idForm = idForm,
#'              formSource = NULL,
#'              createdAfter = "2012-12-20",
#'              createdBefore = c("20-12-2018","%d-%m-%Y")
#'              )
#' GetAnswers(token = token,
#'              idForm = idForm,
#'              formSource = web_public,
#'              createdAfter = c("20-12-2012","%d-%m-%Y"),
#'              createdBefore = c("20-12-2018","%d-%m-%Y")
#'              )
#' GetAnswers(token = token,
#'              idForm = idForm,
#'              formSource = web_private,
#'              createdAfter = "2012-12-20",
#'              createdBefore = "2018-12-20",
#'              )
#' @export

GetAnswers <- function(token, idForm, repetedColunsNames = FALSE,
                       formSource = NULL,
                       createdBefore = NULL,
                       createdAfter = NULL) {

  form_definition <- GetFormSchema(token,idForm)
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
        metaData{
            friendlyId
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
  if (is.null(resp)) {
    warning("No answers avaliable. Returning NULL")
    return(NULL)
  }

  # Unnesting the data frame
  ## This function change the original orders of the columns
  resp <- jsonlite::flatten(resp)

  # Check if in this results have some question with N answers
  if (length(aux[[4]]) != 0) {
    ## Adding the friendly ID to all data frames nested with N answers
    i <- 1
    n <- nrow(resp)
    while (i <= n) {
      cod <- resp$friendlyId[i]
      j <- 1
      nAux <- length(aux[[4]])
      while (j <= nAux) {
        namesAux <- NA
        # check if the column have name
        if (is.null(names(resp[aux[[4]][j]][[1]][[i]]))) {
          namesAux <- aux[[4]][j + 1]
        } else {
          namesAux <- paste0(aux[[4]][j + 1],
                             '.', names(resp[aux[[4]][j]][[1]][[i]]))
        }

        # transforming in data frame, because in some types, this is just a array
        resp[aux[[4]][j]][[1]][[i]] <-
          as.data.frame(resp[aux[[4]][j]][[1]][[i]], stringsAsFactors = FALSE)

        # check if has some answer
        if (length(resp[aux[[4]][j]][[1]][[i]]) != 0) {
          # renaming the columns
          names(resp[aux[[4]][j]][[1]][[i]]) <- namesAux

          # adding the columns cod
          resp[aux[[4]][j]][[1]][[i]] <-
            dplyr::mutate(resp[aux[[4]][j]][[1]][[i]],cod = cod)
        }
        j <- j + 2
      }

      i <- i + 1
    }

    ## Getting the nested data frames with N answers and unificating the same
    ## column and add in array each one new data frame
    ### i: sequencial index do fill the list
    ### j: the index of aux[[4]], indicating the colunms to bind
    i <- 1
    j <- 1
    n <- length(aux[[4]])
    otherDF <- list()
    while (j <= n) {
      otherDF[[i]] <- do.call("rbind",resp[aux[[4]][j]][[1]])

      i <- i + 1
      j <- j + 2
    }
  }

  # Adjust aux, adding in aux[[2]] e aux[[3]] the colum name to id
  aux[[2]] <- append(aux[[2]],'cod',0)
  aux[[3]] <- append(aux[[3]],'friendlyId',0)

  # Re-ordened the columns to the original order
  resp <- dplyr::select(resp, aux[[3]])

  # Rename the columns, changing the idComponents by the question names
  ## Check the user preference about repeted names in the columns
  if (repetedColunsNames) {
    names(resp) <- aux[[2]]
  } else {
    ### Cases with the repeted names receive a sufix
    names(resp) <- make.names(aux[[2]],unique = TRUE)
  }

  # Return data frames with the answers
  if (length(aux[[4]]) != 0) {
    return(list(resp,otherDF))
  } else {
    return(resp)
  }
}
