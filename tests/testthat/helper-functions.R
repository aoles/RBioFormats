pixelType = 0:7
pixelTypes = data.frame(
  pixelType = vapply(pixelType, FormatTools$getPixelTypeString, character(1L)),
  bytesPerPixel = vapply(pixelType, FormatTools$getBytesPerPixel, integer(1L)),
  isSigned = vapply(pixelType, FormatTools$isSigned, logical(1L)),
  isFloatingPoint = vapply(pixelType, FormatTools$isFloatingPoint, logical(1L)),
  stringsAsFactors = FALSE
)

