context("GetForms")

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
  myForms2 <-
    data.frame(id = c("5704", "5722", "5721", "5723", "5705", "5713", "5712",
                      "5711", "5719"),
               name = c("API Doc - Filmes preferidos",
                        "RColetum Test - Classic Rocks",
                        "RColetum Test - Classic Rocks (genres)",
                        "RColetum Test - Classic Rocks (instruments)",
                        "RColetum Test - Iris", "RColetum Test - Star Wars",
                        "RColetum Test - Star Wars (films)",
                        "RColetum Test - Star Wars (species)",
                        "RColetum Test - Storms"),
               status = c("enabled", "enabled", "disabled", "disabled",
                          "enabled", "enabled", "disabled", "disabled",
                          "enabled"),
               category = c(NA, "RColetum Tests", NA, NA, "RColetum Tests",
                            "RColetum Tests", NA, NA, "RColetum Tests"),
               answerTracking = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                  FALSE, FALSE, FALSE),
               publicAnswers = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                 FALSE, FALSE,FALSE),
               stringsAsFactors = FALSE)
  expect_equal(myForms,myForms2)
})

test_that("Get forms with the filters", {
  myFormsFull <-
    data.frame(id = c("5704", "5722", "5721", "5723", "5705", "5713", "5712",
                      "5711", "5719"),
               name = c("API Doc - Filmes preferidos",
                        "RColetum Test - Classic Rocks",
                        "RColetum Test - Classic Rocks (genres)",
                        "RColetum Test - Classic Rocks (instruments)",
                        "RColetum Test - Iris", "RColetum Test - Star Wars",
                        "RColetum Test - Star Wars (films)",
                        "RColetum Test - Star Wars (species)",
                        "RColetum Test - Storms"),
               status = c("enabled", "enabled", "disabled", "disabled",
                          "enabled", "enabled", "disabled", "disabled",
                          "enabled"),
               category = c(NA, "RColetum Tests", NA, NA, "RColetum Tests",
                            "RColetum Tests", NA, NA, "RColetum Tests"),
               answerTracking = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                  FALSE, FALSE, FALSE),
               publicAnswers = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                 FALSE, FALSE,FALSE),
               stringsAsFactors = FALSE)

  myFormnsDisabled <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", status = 'disabled')
  myFormnsDisabled2 <-
    dplyr::filter(myFormsFull, status == "disabled")
  expect_equal(myFormnsDisabled,myFormnsDisabled2)

  myFormnsEnabled <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", status = 'enabled')
  myFormnsEnabled2 <-
    dplyr::filter(myFormsFull, status == "enabled")
  expect_equal(myFormnsEnabled,myFormnsEnabled2)

  myFormsAnswerTracking <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", answerTracking = TRUE)
  myFormsAnswerTracking2 <-
    dplyr::filter(myFormsFull, answerTracking == TRUE)
  expect_equal(myFormsAnswerTracking, myFormsAnswerTracking2)

  myFormsAnswerNotTracking <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", answerTracking = FALSE)
  myFormsAnswerNotTracking2 <-
    dplyr::filter(myFormsFull, answerTracking == FALSE)
  expect_equal(myFormsAnswerNotTracking, myFormsAnswerNotTracking2)

  myFormsPublicAnswers <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", publicAnswers = TRUE)
  expect_identical(myFormsPublicAnswers,NULL)

  myFormsNotPublicAnswers <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", publicAnswers = FALSE)
  myFormsNotPublicAnswers2 <-
    dplyr::filter(myFormsFull, publicAnswers == FALSE)
  expect_equal(myFormsNotPublicAnswers,myFormsNotPublicAnswers2)

  myFormsMistFilters <- GetForms(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                                 status = "enabled",
                                 publicAnswers = FALSE,
                                 answerTracking = FALSE)
  myFormsMistFilters2 <-
    dplyr::filter(myFormsFull,
                  status == "enabled",
                  publicAnswers == FALSE,
                  answerTracking == FALSE)
  expect_equal(myFormsMistFilters,myFormsMistFilters2)

})
