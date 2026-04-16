context("GetForm")

test_that("error by wrong token", {
  expect_error(
    GetForm("notexisttoken", 5705),
    "Error 401: O token informado não é válido para o acesso."
  )
  expect_error(
    GetForm("", 5705),
    "Error 401: Unauthorized"
  )
})

test_that("error by wrong idForm or nameForm", {
  expect_error(
    GetForm("cizio7xeohwgc8k4g4koo008kkoocwg", 9999),
    "Error 404"
  )

  expect_error(
    GetForm("cizio7xeohwgc8k4g4koo008kkoocwg",
            nameForm = "RColetum Test - NaN"),
    "Name not found."
  )

  expect_error(
    GetForm("cizio7xeohwgc8k4g4koo008kkoocwg"),
    "idForm or nameForm should be provided."
  )
})

test_that("warning when idForm and nameForm are both provided", {
  expect_warning(
    GetForm("cizio7xeohwgc8k4g4koo008kkoocwg", 5705, "RColetum Test - Iris"),
    "Both idForm and nameForm are provided. Ignoring nameForm."
  )
})

test_that("error by duplicated form name", {
  expect_error(
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               nameForm = "RColetum Test - Westeros")
  )
})

test_that("GetForm returns correct metadata for Iris (5705)", {
  form <- GetForm("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)

  expect_equal(form$id, 5705L)
  expect_equal(form$name, "RColetum Test - Iris")
  expect_equal(form$status, "enabled")
  expect_equal(form$category, "RColetum Tests")
  expect_false(form$public_answers)
})

test_that("GetForm returns correct component structure for Iris (5705)", {
  form <- GetForm("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
  components <- form$components

  expect_true(is.data.frame(components))
  expect_equal(nrow(components), 3L)
  expect_equal(components$type, c("select", "group", "group"))
  expect_equal(components$label, c("Specie", "Sepal", "Petal"))
  expect_equal(components$id,
               c("specie66137", "sepal66138", "petal66141"))

  # Sepal group has 2 float children
  sepal <- components$components[[2]]
  expect_true(is.data.frame(sepal))
  expect_equal(nrow(sepal), 2L)
  expect_equal(sepal$type, c("float", "float"))
  expect_equal(sepal$label, c("Length", "Width"))
})

test_that("GetForm works with nameForm for Iris", {
  form <- GetForm("cizio7xeohwgc8k4g4koo008kkoocwg",
                  nameForm = "RColetum Test - Iris")
  expect_equal(form$id, 5705L)
})

test_that("GetForm returns correct structure for Classic Rocks (5722)", {
  form <- GetForm("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)

  expect_equal(form$id, 5722L)
  expect_equal(nrow(form$components), 5L)
  expect_equal(form$components$label,
               c("Artist name", "Active", "Origin localition", "Members", "Music"))

  # Music group has 1 nested group (Album)
  music <- form$components$components[[5]]
  expect_equal(nrow(music), 1L)
  expect_equal(music$label, "Album")
  expect_equal(music$type, "group")
})
