context("Check image I/O")

img_dir = tempdir()

## TODO: switch to pre-generated files
for(type in pixelTypes$pixelType) {
  img = testImage(type)
  filename = file.path(img_dir, paste0(type,".tif"))
  write.image(img, filename, force = TRUE, pixelType = type)
}

for(type in pixelTypes$pixelType) {
  with(pixelTypes[type,], {
    test_that(sprintf('Image reader for pixel type "%s"', type), {
      ref = as.vector(testImage(type))
      filename = file.path(img_dir, paste0(type,".tif"))
      res = as.vector(read.image(filename, normalize = FALSE))
      if (!isFloatingPoint)
        expect_identical(res, ref)
      else
        expect_equal(res, ref)
    })
  })
}

