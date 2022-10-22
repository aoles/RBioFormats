context("Check read.metadata")

test_that("Metadata can be read",{
  f = mockFile(sizeX=16, sizeY=16)
  metadata = read.metadata(f)

  expect_true("coreMetadata" %in% names(metadata))

  # Exercise metadata accessors
  sink(nullfile())
  print(metadata)
  sink()
})
