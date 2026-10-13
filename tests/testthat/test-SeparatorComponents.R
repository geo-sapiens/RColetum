context("Separator components")

# Form structure as returned by GET /forms/{id}, with separators at the root,
# inside a single-value group and inside a multivalued group. Separators carry
# only id, label, help_block and type: no maximum, minimum or visibility.
formWithSeparators <- '[
  {"id": "name1", "label": "Name", "type": "text", "maximum": 1},
  {"id": "section_break1", "label": "Section break", "help_block": "Help",
   "type": "separator"},
  {"id": "address2", "label": "Address", "type": "group", "maximum": 1,
   "components": [
     {"id": "street3", "label": "Street", "type": "text", "maximum": 1},
     {"id": "inner_break4", "label": "Inner break", "help_block": null,
      "type": "separator"}
   ]},
  {"id": "contacts5", "label": "Contacts", "type": "group", "maximum": 0,
   "components": [
     {"id": "first_break6", "label": "First break", "help_block": null,
      "type": "separator"},
     {"id": "phone7", "label": "Phone", "type": "phone", "maximum": 1}
   ]},
  {"id": "tags8", "label": "Tags", "type": "text", "maximum": 0}
]'

formWithoutSeparators <- '[
  {"id": "name1", "label": "Name", "type": "text", "maximum": 1},
  {"id": "address2", "label": "Address", "type": "group", "maximum": 1,
   "components": [
     {"id": "street3", "label": "Street", "type": "text", "maximum": 1}
   ]},
  {"id": "contacts5", "label": "Contacts", "type": "group", "maximum": 0,
   "components": [
     {"id": "phone7", "label": "Phone", "type": "phone", "maximum": 1}
   ]},
  {"id": "tags8", "label": "Tags", "type": "text", "maximum": 0}
]'

parseComponents <- function(json) {
  normalizeComponents(jsonlite::fromJSON(json, simplifyDataFrame = TRUE))
}

test_that("separators do not produce columns or nested data frames", {
  structure <- parseComponents(formWithSeparators)
  result <- collectEmptyFields(structure, "answer")

  separatorIds <- c("section_break1", "inner_break4", "first_break6")
  expect_false(any(separatorIds %in% result$mainCols))
  expect_false(any(separatorIds %in% names(result$nestedDfs)))
  expect_false(any(separatorIds %in% result$dictionary$dfName))
  expect_false(any(unlist(lapply(result$nestedDfs, names)) %in% separatorIds))
})

test_that("empty answer structure is the same with or without separators", {
  with    <- parseComponents(formWithSeparators)
  without <- parseComponents(formWithoutSeparators)

  expect_identical(
    buildEmptyAnswerResult(with, buildGroupTree(with)),
    buildEmptyAnswerResult(without, buildGroupTree(without))
  )
})

test_that("group tree is the same with or without separators", {
  expect_identical(
    buildGroupTree(parseComponents(formWithSeparators)),
    buildGroupTree(parseComponents(formWithoutSeparators))
  )
})
