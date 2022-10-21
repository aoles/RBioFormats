test_that("Bioformats version can be obtained",{
  version <- BioFormats.version()
  parsed_version = numeric_version(version,strict=TRUE)
  expect_type(parsed_version,"list")
})

test_that("Memory check works",{
  mem <- checkJavaMemory()
  expect_type(mem,"double")
})