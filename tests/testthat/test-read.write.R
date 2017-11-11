context("Check image I/O")

img_dir = tempdir()

## TODO: switch to pre-generated files
for(type in pixelTypes$pixelType) {
  img = testImage(type)
  filename = file.path(img_dir, paste0(type,".tif"))
  write.image(img, filename, force = TRUE, pixelType = type)
}

test_that("Image reader", {
  for(type in pixelTypes$pixelType) {
    ref = as.vector(testImage(type))
    filename = file.path(img_dir, paste0(type,".tif"))
    res = as.vector(read.image(filename, normalize = FALSE))
    expect_identical(res, ref)
  }
})
