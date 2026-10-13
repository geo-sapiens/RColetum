context("User-Agent")

test_that("User-Agent identifies the package and its version", {
  expect_identical(
    buildUserAgent(),
    paste0("RColetum/", utils::packageVersion("RColetum"))
  )
})

test_that("requests send the User-Agent and the token", {
  sentConfig <- NULL
  local_mocked_bindings(
    GET = function(url, config = list(), ...) {
      sentConfig <<- config
      list(status_code = 200L)
    },
    content = function(x, ...) '{"id": 1}',
    .package = "httr"
  )

  restGetFunction("/forms/1", "my_token")

  expect_identical(sentConfig$options$useragent, buildUserAgent())
  expect_identical(sentConfig$headers[["Token"]], "my_token")
})
