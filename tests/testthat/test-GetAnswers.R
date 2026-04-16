context("GetAnswers")

# Expected output for form 5705 (Iris – simple form with single-instance groups)
## Created using dput()
myExpectedAnswersIrisForm <-
  structure(list(
    main_df_id = c("1.121804", "1.121803", "1.121802",
                   "1.120620", "1.120619", "1.120617"),
    specie66137 =
      c("virginica", "versicolor", "setosa", "virginica", "versicolor", "setosa"),
    sepal66138.length66139 = c(5.8, 6.4, 4.9, 6.3, 7, 5.1),
    sepal66138.width66140  = c(2.7, 3.2, 3, 3.3, 3.2, 3.5),
    petal66141.length66142 = c(5.1, 4.5, 1.4, 6, 4.7, 1.4),
    petal66141.width66143  = c(1.9, 1.5, 0.2, 2.5, 1.4, 0.2),
    created_by_user_name = rep("André Smaniotto", 6),
    created_by_user_id   = rep(8403L, 6),
    created_at_source    = rep("web_private", 6),
    created_at = c("2018-05-30T11:05:08-0300", "2018-05-30T11:04:30-0300",
                   "2018-05-30T11:02:47-0300", "2018-05-24T17:01:06-0300",
                   "2018-05-24T17:00:39-0300", "2018-05-24T17:00:12-0300"),
    created_at_coordinates.latitude  = rep(NA_real_, 6),
    created_at_coordinates.longitude = rep(NA_real_, 6),
    updated_at = c(NA, NA, NA, "2018-05-24T17:03:04-0300", NA, NA),
    updated_at_coordinates.latitude  = rep(NA_real_, 6),
    updated_at_coordinates.longitude = rep(NA_real_, 6)),
  row.names = c(NA, 6L), class = "data.frame")

# Expected main df for form 5713 (Star Wars – single relational + multivalued relational)
## Created using dput()
myExpectedAnswersStarWarsMainDF <-
  structure(list(
    main_df_id = c("1.120986", "1.120985", "1.120984", "1.120978"),
    name66298 = c("R2-D2", "Yoda", "Anakin Skywalker", "Luke Skywalker"),
    height66299 = c(96L, 66L, 188L, 172L),
    mass66300   = c(32L, 17L, 84L, 77L),
    birth_year66380 = c(33, 896, 41.9, 19),
    gender66302 = c("none", "male", "male", "male"),
    specie66303.label    = c("Droid", "Yoda's species", "Human", "Human"),
    specie66303.answer_id = c("1.120773", "1.120777", "1.120771", "1.120771"),
    created_by_user_name = rep("André Smaniotto", 4),
    created_by_user_id   = rep(8403L, 4),
    created_at_source    = rep("web_private", 4),
    created_at = c("2018-05-28T08:44:48-0300", "2018-05-28T08:43:26-0300",
                   "2018-05-28T08:42:22-0300", "2018-05-28T08:27:40-0300"),
    created_at_coordinates.latitude  = rep(-26.9075, 4),
    created_at_coordinates.longitude = rep(-48.65393, 4),
    updated_at = c(NA, NA, NA, "2018-05-28T08:32:09-0300"),
    updated_at_coordinates.latitude  = c(NA, NA, NA, -26.9075),
    updated_at_coordinates.longitude = c(NA, NA, NA, -48.65393)),
  row.names = c(NA, 4L), class = "data.frame")

# Expected nested df for Star Wars films (multivalued relational field)
## Created using dput()
myExpectedAnswersStarWarsFilmsDF <-
  structure(list(
    label = c("A New Hope", "Attack of the Clones", "Return of the Jedi",
              "Revenge of the Sith", "The Empire Strikes Back",
              "The Force Awakens", "The Last Jedi", "The Phantom Menace",
              "A New Hope", "Attack of the Clones", "Revenge of the Sith",
              "The Empire Strikes Back", "The Phantom Menace",
              "Attack of the Clones", "Revenge of the Sith", "The Phantom Menace",
              "A New Hope", "Return of the Jedi", "The Empire Strikes Back",
              "The Force Awakens", "The Last Jedi"),
    answer_id = c("1.120782", "1.120786", "1.120784", "1.120787", "1.120783",
                  "1.120788", "1.120789", "1.120785", "1.120782", "1.120786",
                  "1.120787", "1.120783", "1.120785", "1.120786", "1.120787",
                  "1.120785", "1.120782", "1.120784", "1.120783", "1.120788",
                  "1.120789"),
    main_df_id = c("1.120986", "1.120986", "1.120986", "1.120986", "1.120986",
                   "1.120986", "1.120986", "1.120986",
                   "1.120985", "1.120985", "1.120985", "1.120985", "1.120985",
                   "1.120984", "1.120984", "1.120984",
                   "1.120978", "1.120978", "1.120978", "1.120978", "1.120978"),
    films66304_id = as.character(1:21)),
  row.names = c(NA, -21L), class = "data.frame")

# ---------------------------------------------------------------------------

test_that("error by wrong token", {
  expect_error(
    GetAnswers("notexisttoken", 5705),
    "Error 401: O token informado não é válido para o acesso."
  )
  expect_error(
    GetAnswers("", 5705),
    "Error 401: Unauthorized"
  )
})

test_that("error when neither idForm nor nameForm is provided", {
  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg"),
    "IdForm or nameForm should be provided."
  )
})

test_that("error on invalid source filter", {
  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705, source = "invalid"),
    "The option 'invalid' is not available"
  )
})

test_that("error on invalid date format", {
  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705,
               createdAfter = "20-01-2018"),
    "ISO 8601 format"
  )
})

test_that("warning when both idForm and nameForm are provided", {
  expect_warning(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705,
               nameForm = "RColetum Test - Iris"),
    "Ignoring the nameForm"
  )
})

test_that("simple form (Iris 5705) returns correct data frame", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
  expect_true(is.data.frame(result))
  expect_equal(result, myExpectedAnswersIrisForm)
})

test_that("simple form works via nameForm", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
                       nameForm = "RColetum Test - Iris")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 6L)
})

test_that("form with multivalued group (Storms 5719) returns list", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5719)
  expect_true(is.list(result))
  expect_equal(length(result), 2L)

  mainDf <- result[[1]]
  expect_true(is.data.frame(mainDf))
  expect_equal(nrow(mainDf), 4L)
  expect_equal(mainDf$main_df_id, c("1.120992", "1.120990", "1.120989", "1.120988"))
  expect_equal(mainDf$name66415, c("Otto", "Bonnie", "Doris", "Amy"))

  nestedDfs <- result[[2]]
  expect_true(is.list(nestedDfs))
  infosKey <- "infos66416"
  expect_true(infosKey %in% names(nestedDfs))
  expect_equal(nrow(nestedDfs[[infosKey]]), 12L)
})

test_that("form with relational fields (Star Wars 5713) returns correct list", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5713)
  expect_true(is.list(result))

  expect_equal(result[[1]], myExpectedAnswersStarWarsMainDF)
  expect_equal(result[[2]][["films66304"]],
               myExpectedAnswersStarWarsFilmsDF)
})

test_that("pagination: all_pages = FALSE returns only one page", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705,
                       all_pages = FALSE, page_size = 3)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
})

test_that("date filters narrow results", {
  full    <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
  filtered <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705,
                         createdAfter  = "2018-05-29",
                         createdBefore = "2018-05-31")
  expect_true(nrow(filtered) < nrow(full))
  expect_equal(nrow(filtered), 3L)
})

test_that("source filter works", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705,
                       source = "web_private")
  expect_true(nrow(result) > 0)
  expect_true(all(result$created_at_source == "web_private"))
})

test_that("empty result returns correct structure for simple form", {
  result <- suppressWarnings(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705,
               createdAfter = "2099-01-01")
  )
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0L)
  expect_true("main_df_id" %in% names(result))
  expect_true("created_at" %in% names(result))
})
