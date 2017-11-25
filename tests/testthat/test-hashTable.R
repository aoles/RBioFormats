context("Check hashtable conversion")

test_that("Data types are retained", {
  ref = list(logical = FALSE, integer = 1L, numeric = 2.0, character = "3")
  res = ref %>% 
    (RBioFormats:::.listToHashtable) %>%
    (RBioFormats:::.hashtableToList)
  expect_identical(res[match(res, ref)], ref)
})
