#' Get the form schema of a form.
#'
#' Get the schema of the questions of a specific form in the shape a nested
#' data frame, that contain all the needed information to request the answers
#' of the form.
#'
#' @param token A string access token.
#' @param idForm Numeric Id of the required form.
#'
#' @return A possible nested data frame.
#' @examples
#' GetFormSchema('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9', 3345)
#' @export

GetFormSchema <- function(token, idForm) {
  #### TODO: Change the query to the conctract form, when avaible. ####
  query <- paste0("{
      form_definition(formId:",idForm,"){
        formId,
        label,
        name,
        type,
        componentId,
        components{
          formId,
          label,
          name,
          type,
          componentId,
          components{
            formId,
            label,
            name,
            type,
            componentId,
            components{
              formId,
              label,
              name,
              type,
              componentId,
              components{
                formId,
                label,
                name,
                type,
                componentId,
                minimum,
                maximum
              },
              minimum,
              maximum,
              options,
              order,
              helpBlock,
              currency,
              format,
              choices,
              rangeMin,
              rangeMax,
              step,
              unit
            },
            minimum,
            maximum,
            options,
            order,
            helpBlock,
            currency,
            format,
            choices,
            rangeMin,
            rangeMax,
            step,
            unit
          },
          minimum,
          maximum,
          options,
          order,
          helpBlock,
          currency,
          format,
          choices,
          rangeMin,
          rangeMax,
          step,
          unit
        },
        minimum,
        maximum,
        options,
        order,
        helpBlock,
        currency,
        format,
        choices,
        rangeMin,
        rangeMax,
        step,
        unit
      }
    }")

  # Request
  resp <- requestFunction(query = query, token = token)

  return(resp)
}
