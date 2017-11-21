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

test_that('Write 5D image stack', {
  img = mockImage(sizeX=16,
                  sizeY=16,
                  sizeC=3,
                  sizeZ=5,
                  sizeT=10,
                  series=1)
  img = read.image(write.image(img, tempimg()))
  expect_s4_class(img, "AnnotatedImage")
  expect_identical(dim(img), c(16L, 16L, 3L, 5L, 10L))
})

test_that('Write single image plane', {
  img = read.image(write.image(mockImage(series=1L), tempimg()))
  expect_s4_class(img, "AnnotatedImage")
  expect_identical(seriesCount(img), 1L)
})

test_that('Write image series', {
  img = read.image(write.image(mockImage(series=2L), tempimg()))
  expect_s4_class(img, "AnnotatedImageList")
  expect_identical(seriesCount(img), 2L)
})
