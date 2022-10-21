test_that("read.ome succeeds",{
  f = system.file("images", "nuclei.tif", package="EBImage")
  omexml = read.omexml(f)
  expect_type(omexml,"character")
})