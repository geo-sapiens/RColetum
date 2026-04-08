context("GetForms")

myExpectedForms <-
  structure(list(id = c(5704L, 5722L, 5721L, 5723L, 5705L, 5713L,
  5712L, 5711L, 5719L, 5744L, 5745L), name = c("API Doc - Filmes preferidos",
  "RColetum Test - Classic Rocks", "RColetum Test - Classic Rocks (genres)",
  "RColetum Test - Classic Rocks (instruments)", "RColetum Test - Iris",
  "RColetum Test - Star Wars", "RColetum Test - Star Wars (films)",
  "RColetum Test - Star Wars (species)", "RColetum Test - Storms",
  "RColetum Test - Westeros", "RColetum Test - Westeros"),
  description = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  status = c("enabled", "enabled", "disabled", "disabled", "enabled",
  "enabled", "disabled", "disabled", "enabled", "enabled", "enabled"),
  category = c("", "RColetum Tests", "", "", "RColetum Tests",
  "RColetum Tests", "", "", "RColetum Tests", "RColetum Tests",
  "RColetum Tests"),
  version = c("1.5", "1.3", "1.0", "1.0", "1.2", "1.4", "1.2",
  "1.1", "1.1", "1.0", "1.0"),
  public_answers = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, FALSE, FALSE)),
  class = "data.frame", row.names = c(NA, -11L))

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

test_that("error on invalid status filter", {
  expect_error(
    GetForms(token = "cizio7xeohwgc8k4g4koo008kkoocwg", status = "enable"),
    paste0("The option 'enable' is not available for the filter 'status'. ",
           "The available options are: 'enabled' or 'disabled'.")
  )
})

test_that("get forms with no filter", {
  myForms <- dplyr::arrange(
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg"), name, id)
  expect_equal(myForms, myExpectedForms)
})

test_that("get forms filtered by status", {
  myFormsDisabled <-
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", status = "disabled")
  myFormsDisabled2 <-
    dplyr::filter(myExpectedForms, status == "disabled")
  expect_equal(myFormsDisabled, myFormsDisabled2)

  myFormsEnabled <- dplyr::arrange(
    GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", status = "enabled"), name, id)
  myFormsEnabled2 <-
    dplyr::filter(myExpectedForms, status == "enabled")
  expect_equal(myFormsEnabled, myFormsEnabled2)
})

test_that("get forms filtered by name", {
  myForms <- GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", name = "Iris")
  expect_equal(nrow(myForms), 1L)
  expect_equal(myForms$name, "RColetum Test - Iris")

  myFormsRColetum <- GetForms("cizio7xeohwgc8k4g4koo008kkoocwg", name = "RColetum Test")
  expect_true(nrow(myFormsRColetum) >= 8L)
})

test_that("pagination works", {
  page1 <- GetForms("cizio7xeohwgc8k4g4koo008kkoocwg",
                    page_size = 3, all_pages = FALSE)
  expect_equal(nrow(page1), 3L)

  allForms <- GetForms("cizio7xeohwgc8k4g4koo008kkoocwg",
                       page_size = 3, all_pages = TRUE)
  expect_equal(nrow(allForms), nrow(myExpectedForms))
})

test_that("returns NULL with warning when no forms match", {
  expect_warning(
    result <- GetForms("cizio7xeohwgc8k4g4koo008kkoocwg",
                       name = "___form_that_does_not_exist___"),
    "No forms available"
  )
  expect_null(result)
})
