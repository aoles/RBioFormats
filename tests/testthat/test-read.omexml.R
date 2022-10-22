test_that("read.ome succeeds",{
  f = mockFile(sizeX=16, sizeY=16)
  omexml = read.omexml(f)
  expect_type(omexml,"character")
})