context("Check read.image")

MAX_INT = min(.Machine$integer.max, 2147483647)

pixelTypes = within(pixelTypes, {
  n = 256^bytesPerPixel
  x = pmin(n, MAX_INT)
  scaleFactor = ifelse(bytesPerPixel>2, bytesPerPixel/2, 1)
  refMin = ifelse(isSigned & !isFloatingPoint, -n/2, 0)
  refMax = refMin + n - scaleFactor
})

for(type in pixelTypes$pixelType) {
  with(pixelTypes[type,], {
    test_that(sprintf('Extreme pixel values correspond to the range defined by pixelType "%s"', type), {
      ## minimal value allowed by pixel type
      mockfile = mockFile(pixelType=pixelType)
      res = as.double(read.image(mockfile, subset = list(x=1, y=16), normalize = FALSE))
      expect_identical(res, refMin)
      ## maximal value allowed by pixel type modulo scaleFactor
      mockfile = mockFile(pixelType=pixelType, sizeX=x, scaleFactor=scaleFactor)
      res = as.double(read.image(mockfile, subset = list(x=x, y=16), normalize = FALSE))
      expect_identical(res, refMax)
    })
  })
}
