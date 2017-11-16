context("Check write.image")

for(type in pixelTypes$pixelType) {
  with(pixelTypes[type,], {
    test_that(sprintf('Image writer for pixel type "%s"', type), {
      img = testImage(type)
      ref = as.vector(img)
      res = as.vector(read.image(write.image(img, tempfile('',,'.tif'), pixelType=type), normalize=FALSE))
      if (!isFloatingPoint)
        expect_identical(res, ref)
      else
        expect_equal(res, ref)
    })
  })
}
