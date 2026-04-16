context("GetAnswers – Classic Rocks (complex nested form)")

# Expected main df for form 5722 (Classic Rocks)
## Created using dput()
myExpectedClassicRocksMainDF <-
  structure(list(
    main_df_id = c("1.121101", "1.121093", "1.121042"),
    artist_name66429 = c("Pink Floyd", "Bon jovi", "Lynyrd Skynyrd"),
    active66430 = c(TRUE, TRUE, TRUE),
    origin_localition66431.country66432 =
      c("Reino Unido", "Estados Unidos", "Estados Unidos"),
    created_by_user_name = rep("André Smaniotto", 3),
    created_by_user_id   = rep(8403L, 3),
    created_at_source    = rep("web_private", 3),
    created_at = c("2018-05-28T14:04:53-0300", "2018-05-28T13:49:14-0300",
                   "2018-05-28T11:49:03-0300"),
    created_at_coordinates.latitude  = rep(NA_real_, 3),
    created_at_coordinates.longitude = rep(NA_real_, 3),
    updated_at = c("2018-05-28T14:16:34-0300", "2018-05-28T13:50:54-0300",
                   "2018-05-28T13:40:26-0300"),
    updated_at_coordinates.latitude  = rep(NA_real_, 3),
    updated_at_coordinates.longitude = rep(NA_real_, 3)),
  row.names = c(NA, 3L), class = "data.frame")

# Expected nested df: Members Active (multivalued text inside Members group)
## Created using dput()
myExpectedMembersActiveDF <-
  structure(list(
    main_df_id = c("1.121101", "1.121101", "1.121101",
                   "1.121093", "1.121093", "1.121093", "1.121093", "1.121093",
                   "1.121042", "1.121042", "1.121042", "1.121042", "1.121042",
                   "1.121042", "1.121042"),
    members66433.active66434 =
      c("Nick Mason", "Roger Waters", "David Gilmour",
        "Jon Bon Jovi", "David Bryan", "Tico Torres", "Hugh McDonald", "Phil X",
        "Gary Rossington", "Rickey Medlocke", "Johnny Van Zant",
        "Michael Cartellone", "Mark Matejka", "Peter Keys", "Keith Christopher"),
    members66433.active66434_id = as.character(1:15)),
  row.names = c(NA, -15L), class = "data.frame")

# Expected nested df: Albums (multivalued group inside Music group)
## Created using dput()
myExpectedAlbumsDF <-
  structure(list(
    name66438 =
      c("The Dark Side of the Moon", "Wish You Were Here",
        "Slippery When Wet",
        "(Pronounced 'Lĕh-'nérd 'Skin-'nérd)", "Second Helping",
        "Nuthin' Fancy", "Gimme Back My Bullets", "Street Survivors"),
    year66439 = c(1973L, 1975L, 1986L, 1973L, 1974L, 1975L, 1976L, 1977L),
    main_df_id = c("1.121101", "1.121101", "1.121093",
                   "1.121042", "1.121042", "1.121042", "1.121042", "1.121042"),
    music66436.album66437_id = as.character(1:8)),
  row.names = c(NA, -8L), class = "data.frame")

# ---------------------------------------------------------------------------

test_that("Classic Rocks main df matches expected", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)
  expect_equal(result[[1]], myExpectedClassicRocksMainDF)
})

test_that("Classic Rocks nested dfs have correct names and row counts", {
  result  <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)
  nested  <- result[[2]]

  expectedKeys <- c(
    "members66433.active66434", # Members Active
    "members66433.past66435",   # Members Past
    "music66436.album66437",    # Albums
    "genres66441",              # Genres (relational)
    "members66443",             # Album Members
    "instruments66446"          # Instruments (relational)
  )
  expect_true(all(expectedKeys %in% names(nested)))

  activeKey <- "members66433.active66434"
  expect_equal(nrow(nested[[activeKey]]), 15L)

  albumKey <- "music66436.album66437"
  expect_equal(nrow(nested[[albumKey]]), 8L)
})

test_that("Classic Rocks Members Active nested df matches expected", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)
  activeKey <- "members66433.active66434"
  expect_equal(result[[2]][[activeKey]], myExpectedMembersActiveDF)
})

test_that("Classic Rocks Albums nested df matches expected", {
  result <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)
  albumKey <- "music66436.album66437"
  expect_equal(result[[2]][[albumKey]], myExpectedAlbumsDF)
})

test_that("Classic Rocks Genres relational nested df has correct columns", {
  result   <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)
  genreKey <- "genres66441"
  genres   <- result[[2]][[genreKey]]

  expect_true(is.data.frame(genres))
  expect_true("label"    %in% names(genres))
  expect_true("answer_id" %in% names(genres))
  expect_true(nrow(genres) > 0)
})

test_that("Classic Rocks FlattenAnswers produces a single data frame", {
  flat <- FlattenAnswers(GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5722))
  expect_true(is.data.frame(flat))
  expect_true("answer.main_df_id" %in% names(flat))
})
