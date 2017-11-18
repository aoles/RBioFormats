context("Check write.image")

for(type in pixelTypes$pixelType) {
  with(pixelTypes[type,], {
    test_that(sprintf('Image writer for pixel type "%s"', type), {
      img = testImage(type)
      ref = as.vector(img)
      res = as.vector(read.image(write.image(img, tempimg(), pixelType=type), normalize=FALSE))
      if (!isFloatingPoint)
        expect_identical(res, ref)
      else
        expect_equal(res, ref)
    })
  })
}

test_that('Write single image plane', {
  img = read.image(write.image(mockImage(series=1L), tempimg()))
  expect_s4_class(img, "AnnotatedImage")
  expect_identical(seriesCount(img), 1L)
})

test_that('Write image series', {
  img = read.image(write.image(mockImage(series=2L), tempimg()))
  expect_s4_class(img, "AnnotatedImage")
  expect_identical(seriesCount(img), 2L)
})
