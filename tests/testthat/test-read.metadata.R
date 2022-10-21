context("Check read.metadata")

test_that("Metadata can be read",{
  f = system.file("images", "nuclei.tif", package="EBImage")
  metadata = read.metadata(f)

  expect_true("coreMetadata" %in% names(metadata))

  # Exercise metadata accessors
  sink(nullfile())
  print(metadata)
  sink()
})
