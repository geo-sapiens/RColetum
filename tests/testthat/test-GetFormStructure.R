context("GetFormStructure")

test_that("error by wrong token", {
  expect_error(
    GetFormStructure("notexisttoken", 5705),
    "Error 401: O token informado não é válido para o acesso."
  )
  expect_error(
    GetFormStructure("", 5705),
    "Error 401: Unauthorized"
  )
})

test_that("error by wrong idForm or nameForm", {
  expect_error(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg", 5715),
    "Form not found."
  )

  expect_error(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg",
                     nameForm = "RColetum Test - NaN"),
    "Name not found."
  )

  expect_error(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg",
                     5841,
                     "RColetum Test - NaN"),
    "Form not found."
  )

  expect_error(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg",
                     5715,
                     "RColetum Test - Iris"),
    "Form not found."
  )

  expect_error(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg"),
    "IdForm or nameForm should be provided."
  )

})

test_that("Warming when is informed the idForm and nameForm", {
  expect_warning(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg",
                     5705,
                     "RColetum Test - Iris"),
    "The idForm and nameForm are provided. Ignoring the nameForm."
  )
})

test_that("GetFormStructure in simple form", {
  myExpectedFormStructureStringJson <-
    "{\"data\":{\"form_structure\":[{\"label\":\"Specie\",\"name\":null,
  \"componentId\":\"specie66137\",\"type\":\"selectfield\",\"helpBlock\":null,
  \"order\":\"0\",\"components\":null},{\"label\":null,\"name\":\"Sepal\",
  \"componentId\":\"sepal66138\",\"type\":\"group\",\"helpBlock\":null,
  \"order\":\"1\",\"components\":[{\"label\":\"Length\",\"name\":null,
  \"componentId\":\"length66139\",\"type\":\"floatfield\",\"helpBlock\":null,
  \"order\":\"0\",\"components\":null},{\"label\":\"Width\",\"name\":null,
  \"componentId\":\"width66140\",\"type\":\"floatfield\",\"helpBlock\":null,
  \"order\":\"1\",\"components\":null}]},{\"label\":null,\"name\":\"Petal\",
  \"componentId\":\"petal66141\",\"type\":\"group\",\"helpBlock\":null,
  \"order\":\"2\",\"components\":[{\"label\":\"Length\",\"name\":null,
  \"componentId\":\"length66142\",\"type\":\"floatfield\",\"helpBlock\":null,
  \"order\":\"0\",\"components\":null},{\"label\":\"Width\",\"name\":null,
  \"componentId\":\"width66143\",\"type\":\"floatfield\",\"helpBlock\":null,
  \"order\":\"1\",\"components\":null}]}]}}"
  myExpectedFormStructure <-
    jsonlite::fromJSON(
      txt = myExpectedFormStructureStringJson,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE
    )
  myExpectedFormStructure <- myExpectedFormStructure$data[[1]]

  myFullFormStructure <-
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
  expect_equal(myFullFormStructure, myExpectedFormStructure)

  myFormStructureFiltered <-
    GetFormStructure(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                     idForm = 5705,
                     componentId = "width66140")
  myFormStructureFiltered2 <-
    dplyr::filter(myExpectedFormStructure$components[[2]],
                  componentId == "width66140")
  expect_equal(myFormStructureFiltered, myFormStructureFiltered2)

  })

test_that("GetFormStructure in complex and nested form", {
  myExpectedFormStructureStringJson <-
    "{\"data\":{\"form_structure\":[{\"label\":\"Artist name\",\"name\":null,
  \"componentId\":\"artistName66429\",\"type\":\"textfield\",\"helpBlock\":null,
  \"order\":\"0\",\"components\":null},{\"label\":\"Active\",\"name\":null,
  \"componentId\":\"active66430\",\"type\":\"agreementfield\",
  \"helpBlock\":null,\"order\":\"1\",\"components\":null},{\"label\":null,
  \"name\":\"Origin localition\",\"componentId\":\"originLocalition66431\",
  \"type\":\"group\",\"helpBlock\":null,\"order\":\"2\",
  \"components\":[{\"label\":\"Country\",\"name\":null,
  \"componentId\":\"country66432\",\"type\":\"countryfield\",\"helpBlock\":null,
  \"order\":\"0\",\"components\":null}]},{\"label\":null,\"name\":\"Members\",
  \"componentId\":\"members66433\",\"type\":\"group\",\"helpBlock\":null,
  \"order\":\"3\",\"components\":[{\"label\":\"Active\",\"name\":null,
  \"componentId\":\"active66434\",\"type\":\"textfield\",\"helpBlock\":null,
  \"order\":\"0\",\"components\":null},{\"label\":\"Past\",\"name\":null,
  \"componentId\":\"past66435\",\"type\":\"textfield\",\"helpBlock\":null,
  \"order\":\"1\",\"components\":null}]},{\"label\":null,\"name\":\"Music\",
  \"componentId\":\"music66436\",\"type\":\"group\",\"helpBlock\":null,
  \"order\":\"4\",\"components\":[{\"label\":null,\"name\":\"Album\",
  \"componentId\":\"album66437\",\"type\":\"group\",\"helpBlock\":null,
  \"order\":\"0\",\"components\":[{\"label\":\"Name\",\"name\":null,
  \"type\":\"textfield\",\"componentId\":\"name66438\",\"helpBlock\":null,
  \"order\":\"0\",\"components\":null},{\"label\":\"Year\",\"name\":null,
  \"type\":\"integerfield\",\"componentId\":\"year66439\",\"helpBlock\":null,
  \"order\":\"1\",\"components\":null},{\"label\":\"Genres\",\"name\":null,
  \"type\":\"relationalfield\",\"componentId\":\"genres66441\",
  \"helpBlock\":null,\"order\":\"2\",\"components\":null},{\"label\":null,
  \"name\":\"Members\",\"type\":\"group\",\"componentId\":\"members66443\",
  \"helpBlock\":null,\"order\":\"3\",\"components\":[{\"label\":\"Name\",
  \"name\":null,\"componentId\":\"name66444\",\"type\":\"textfield\",
  \"helpBlock\":null,\"order\":\"0\",\"components\":null},
  {\"label\":\"Instruments\",\"name\":null,\"componentId\":\"instruments66446\",
  \"type\":\"relationalfield\",\"helpBlock\":null,\"order\":\"1\",
  \"components\":null}]}]}]}]}}"
  myExpectedComplexNestedFormStructure <-
    jsonlite::fromJSON(
      txt = myExpectedFormStructureStringJson,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE
    )
  myExpectedComplexNestedFormStructure <-
    myExpectedComplexNestedFormStructure$data[[1]]

  myFullFormStructure <-
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)
  expect_equal(myExpectedComplexNestedFormStructure, myFullFormStructure)

  myFilteredFormStructure <-
    GetFormStructure(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                     idForm = 5722,
                     componentId = "past66435")
  myFilteredFormStructure2 <-
    dplyr::filter(myExpectedComplexNestedFormStructure$components[[4]],
                  componentId == "past66435")
  expect_equal(myFilteredFormStructure, myFilteredFormStructure2)

})
