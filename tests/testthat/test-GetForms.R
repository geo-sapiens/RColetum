context("GetForms")

# Create the expected Forms.
myExpectedFormsStringJson <-
  "{\"data\":{\"form\":[{\"id\":\"5704\",
\"name\":\"API Doc - Filmes preferidos\",\"status\":\"enabled\",
\"category\":null,\"answerTracking\":true,\"publicAnswers\":false},
{\"id\":\"5722\",\"name\":\"RColetum Test - Classic Rocks\",
\"status\":\"enabled\",\"category\":\"RColetum Tests\",
\"answerTracking\":false,\"publicAnswers\":false},{\"id\":\"5721\",
\"name\":\"RColetum Test - Classic Rocks (genres)\",\"status\":\"disabled\",
\"category\":null,\"answerTracking\":false,\"publicAnswers\":false},
  {\"id\":\"5723\",\"name\":\"RColetum Test - Classic Rocks (instruments)\",
  \"status\":\"disabled\",\"category\":null,\"answerTracking\":false,
  \"publicAnswers\":false},{\"id\":\"5705\",\"name\":\"RColetum Test - Iris\",
  \"status\":\"enabled\",\"category\":\"RColetum Tests\",
  \"answerTracking\":false,\"publicAnswers\":false},{\"id\":\"5713\",
  \"name\":\"RColetum Test - Star Wars\",\"status\":\"enabled\",
  \"category\":\"RColetum Tests\",\"answerTracking\":false,
  \"publicAnswers\":false},{\"id\":\"5712\",
  \"name\":\"RColetum Test - Star Wars (films)\",\"status\":\"disabled\",
  \"category\":null,\"answerTracking\":false,\"publicAnswers\":false},
  {\"id\":\"5711\",\"name\":\"RColetum Test - Star Wars (species)\",
  \"status\":\"disabled\",\"category\":null,\"answerTracking\":false,
  \"publicAnswers\":false},{\"id\":\"5719\",\"name\":\"RColetum Test - Storms\",
  \"status\":\"enabled\",\"category\":\"RColetum Tests\",
  \"answerTracking\":false,\"publicAnswers\":false}]}}"
myExpectedForms <-
  jsonlite::fromJSON(
    txt = myExpectedFormsStringJson,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE
  )
myExpectedForms <- myExpectedForms$data[[1]]

test_that("error by wrong token", {
  expect_error(
    GetForms("notexisttoken"),
    "Error 401: O token informado não é válido para o acesso."
  )
  expect_error(
    GetForms(""),
    "Error 401: Unauthorized"
  )
})

test_that("error in using incorrection the filters", {
  expect_error(
    GetForms(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
             status = "enable"),
    paste0("The option 'enable' are not avaliable for the filter 'status'. ",
    "The avaliable options to this filter are: 'enabled' or 'disabled'.")
  )

  expect_error(
    GetForms(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
             publicAnswers = "enabled"),
    paste0("The option 'enabled' are not avaliable for the filter ",
           "'publicAnswers'. The avaliable options to this filter are: 'true' ",
           "or 'false'.")
  )

  expect_error(
    GetForms(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
             answerTracking = "disabled"),
    paste0("The option 'disabled' are not avaliable.The avaliable options to ",
           "this filter are: 'true' or 'false'.")
  )

})

test_that("GetForms with no filter", {
    myForms <- GetForms("cizio7xeohwgc8k4g4koo008kkoocwg")
  expect_equal(myForms,myExpectedForms)
})

test_that("Get forms with the filters", {

    myFormnsDisabled <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", status = 'disabled')
  myFormnsDisabled2 <-
    dplyr::mutate(
      dplyr::filter(myExpectedForms, status == "disabled"),
      category = as.logical(category)
    )
  expect_equal(myFormnsDisabled,myFormnsDisabled2)

  myFormnsEnabled <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", status = 'enabled')
  myFormnsEnabled2 <-
    dplyr::filter(myExpectedForms, status == "enabled")
  expect_equal(myFormnsEnabled,myFormnsEnabled2)

  myFormsAnswerTracking <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", answerTracking = TRUE)
  myFormsAnswerTracking2 <-
    dplyr::mutate(
      dplyr::filter(myExpectedForms, answerTracking == TRUE),
      category = as.logical(category)
      )
  expect_equal(myFormsAnswerTracking, myFormsAnswerTracking2)

  myFormsAnswerNotTracking <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", answerTracking = FALSE)
  myFormsAnswerNotTracking2 <-
    dplyr::filter(myExpectedForms, answerTracking == FALSE)
  expect_equal(myFormsAnswerNotTracking, myFormsAnswerNotTracking2)

  myFormsPublicAnswers <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", publicAnswers = TRUE)
  expect_identical(myFormsPublicAnswers,NULL)

  myFormsNotPublicAnswers <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", publicAnswers = FALSE)
  myFormsNotPublicAnswers2 <-
    dplyr::filter(myExpectedForms, publicAnswers == FALSE)
  expect_equal(myFormsNotPublicAnswers,myFormsNotPublicAnswers2)

  myFormsMistFilters <- GetForms(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                                 status = "enabled",
                                 publicAnswers = FALSE,
                                 answerTracking = FALSE)
  myFormsMixFilters2 <-
    dplyr::filter(myExpectedForms,
                  status == "enabled",
                  publicAnswers == FALSE,
                  answerTracking == FALSE)
  expect_equal(myFormsMistFilters,myFormsMixFilters2)

})
