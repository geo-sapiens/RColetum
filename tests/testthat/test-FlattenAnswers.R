context("FlattenAnswers")

# Expected flat output for form 5713 (Star Wars – relational multivalued field)
## Created using dput()
## Join now correctly links each character to their selected films via main_df_id.
## 21 rows: one per character × film combination (no orphaned rows).
myExpectedStarWarsFlatDF <-
  structure(list(
    answer.main_df_id =
      c("1.120986", "1.120986", "1.120986", "1.120986", "1.120986",
        "1.120986", "1.120986", "1.120986",
        "1.120985", "1.120985", "1.120985", "1.120985", "1.120985",
        "1.120984", "1.120984", "1.120984",
        "1.120978", "1.120978", "1.120978", "1.120978", "1.120978"),
    answer.name66298 =
      c("R2-D2", "R2-D2", "R2-D2", "R2-D2", "R2-D2", "R2-D2", "R2-D2", "R2-D2",
        "Yoda", "Yoda", "Yoda", "Yoda", "Yoda",
        "Anakin Skywalker", "Anakin Skywalker", "Anakin Skywalker",
        "Luke Skywalker", "Luke Skywalker", "Luke Skywalker",
        "Luke Skywalker", "Luke Skywalker"),
    answer.height66299 =
      c(96L, 96L, 96L, 96L, 96L, 96L, 96L, 96L,
        66L, 66L, 66L, 66L, 66L,
        188L, 188L, 188L,
        172L, 172L, 172L, 172L, 172L),
    answer.mass66300 =
      c(32L, 32L, 32L, 32L, 32L, 32L, 32L, 32L,
        17L, 17L, 17L, 17L, 17L,
        84L, 84L, 84L,
        77L, 77L, 77L, 77L, 77L),
    answer.birth_year66380 =
      c(33, 33, 33, 33, 33, 33, 33, 33,
        896, 896, 896, 896, 896,
        41.9, 41.9, 41.9,
        19, 19, 19, 19, 19),
    answer.gender66302 =
      c("none", "none", "none", "none", "none", "none", "none", "none",
        "male", "male", "male", "male", "male",
        "male", "male", "male",
        "male", "male", "male", "male", "male"),
    answer.specie66303.label =
      c("Droid", "Droid", "Droid", "Droid", "Droid", "Droid", "Droid", "Droid",
        "Yoda's species", "Yoda's species", "Yoda's species",
        "Yoda's species", "Yoda's species",
        "Human", "Human", "Human",
        "Human", "Human", "Human", "Human", "Human"),
    answer.specie66303.answer_id =
      c("1.120773", "1.120773", "1.120773", "1.120773",
        "1.120773", "1.120773", "1.120773", "1.120773",
        "1.120777", "1.120777", "1.120777", "1.120777", "1.120777",
        "1.120771", "1.120771", "1.120771",
        "1.120771", "1.120771", "1.120771", "1.120771", "1.120771"),
    answer.created_by_user_name =
      rep("André Smaniotto", 21),
    answer.created_by_user_id =
      rep(8403L, 21),
    answer.created_at_source =
      rep("web_private", 21),
    answer.created_at =
      c(rep("2018-05-28T08:44:48-0300", 8),
        rep("2018-05-28T08:43:26-0300", 5),
        rep("2018-05-28T08:42:22-0300", 3),
        rep("2018-05-28T08:27:40-0300", 5)),
    answer.created_at_coordinates.latitude =
      rep(-26.9075, 21),
    answer.created_at_coordinates.longitude =
      rep(-48.65393, 21),
    answer.updated_at =
      c(rep(NA_character_, 16),
        rep("2018-05-28T08:32:09-0300", 5)),
    answer.updated_at_coordinates.latitude =
      c(rep(NA_real_, 16), rep(-26.9075, 5)),
    answer.updated_at_coordinates.longitude =
      c(rep(NA_real_, 16), rep(-48.65393, 5)),
    films66304.label =
      c("A New Hope", "Attack of the Clones", "Return of the Jedi",
        "Revenge of the Sith", "The Empire Strikes Back",
        "The Force Awakens", "The Last Jedi", "The Phantom Menace",
        "A New Hope", "Attack of the Clones", "Revenge of the Sith",
        "The Empire Strikes Back", "The Phantom Menace",
        "Attack of the Clones", "Revenge of the Sith", "The Phantom Menace",
        "A New Hope", "Return of the Jedi", "The Empire Strikes Back",
        "The Force Awakens", "The Last Jedi"),
    films66304.answer_id =
      c("1.120782", "1.120786", "1.120784", "1.120787", "1.120783",
        "1.120788", "1.120789", "1.120785",
        "1.120782", "1.120786", "1.120787", "1.120783", "1.120785",
        "1.120786", "1.120787", "1.120785",
        "1.120782", "1.120784", "1.120783", "1.120788", "1.120789"),
    films66304.films66304_id = as.character(1:21)),
  row.names = c(NA, -21L), class = "data.frame")

# ---------------------------------------------------------------------------

test_that("FlattenAnswers with multivalued group (Storms 5719)", {
  flat <- FlattenAnswers(GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5719))

  expect_true(is.data.frame(flat))
  # Main-frame columns are prefixed with "answer."
  expect_true("answer.main_df_id" %in% names(flat))
  expect_true("answer.name66415" %in% names(flat))
  # Nested group columns are present
  expect_true("infos66416.status66420" %in% names(flat))
  # Row count: 4 storms × multiple infos each
  expect_true(nrow(flat) > 4L)
})

test_that("FlattenAnswers with relational multivalued field (Star Wars 5713)", {
  flat <- FlattenAnswers(GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5713))
  expect_equal(flat, myExpectedStarWarsFlatDF)
})

test_that("FlattenAnswers passes through a plain data.frame unchanged", {
  answers <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
  expect_equal(FlattenAnswers(answers), answers)
})

test_that("FlattenAnswers errors on invalid input", {
  expect_error(FlattenAnswers("not a list"), "return value of GetAnswers")
  expect_error(FlattenAnswers(list(1, 2, 3)), "return value of GetAnswers")
})
