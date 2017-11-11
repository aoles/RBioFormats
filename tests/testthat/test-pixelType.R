context("Check pixel types")

pixelTypes = within(pixelTypes, {
  n = 256^bytesPerPixel
  x = pmin(n, .Machine$integer.max)
  y = 16
  defaultMin = ifelse(isSigned & !isFloatingPoint, -n/2, 0)
  defaultMax = defaultMin + x - 1
  mockFile = mapply(mockFile, pixelType=pixelType, sizeX=n, sizeY=y)
})
  
test_that("Extreme pixel values correspond to the range defined by pixelType", {
  for (i in seq_len(nrow(pixelTypes))) {
    with(pixelTypes[i,],{
      minValue = as.double(read.image(mockFile, subset = list(x = 1, y = y), normalize = FALSE))
      maxValue = as.double(read.image(mockFile, subset = list(x = x, y = y), normalize = FALSE))
      expect_equal(minValue, defaultMin)
      expect_equal(maxValue, defaultMax)
    })
  }
})
