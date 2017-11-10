FT = FormatTools()

pixelType = 0:7
pixelTypes = data.frame(
  pixelType = vapply(pixelType, FT$getPixelTypeString, character(1L)),
  bytesPerPixel = vapply(pixelType, FT$getBytesPerPixel, integer(1L)),
  isSigned = vapply(pixelType, FT$isSigned, logical(1L)),
  isFloatingPoint = vapply(pixelType, FT$isFloatingPoint, logical(1L)),
  stringsAsFactors = FALSE
)
