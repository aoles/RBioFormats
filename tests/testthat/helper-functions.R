pixelType = 0:7

pixelTypes = data.frame(
  pixelType = setNames(nm=vapply(pixelType, FormatTools$getPixelTypeString, character(1L))),
  bytesPerPixel = vapply(pixelType, FormatTools$getBytesPerPixel, integer(1L)),
  isSigned = vapply(pixelType, FormatTools$isSigned, logical(1L)),
  isFloatingPoint = vapply(pixelType, FormatTools$isFloatingPoint, logical(1L)),
  stringsAsFactors = FALSE
)

## generates a 16x16 test gradient with two first pixels containing the min and
## max values corresponding to the specified pixelType
testImage = function(pixelType, ...)  {
  pixelType = pixelTypes[pixelType,]
  n = 256^pixelType$bytesPerPixel
  minVal = ifelse(pixelType$isSigned & !pixelType$isFloatingPoint, -n/2, 0)
  maxVal = minVal + n - 1
  minSeq = if (pixelType$isSigned) -128 else 0
  values = c(minVal, maxVal, seq_len(254L) + minSeq)
  mode = if (isTRUE(pixelType$bytesPerPixel <= 2)) "integer" else "double"
  storage.mode(values) = mode 
  matrix(values, 16L, 16L)
}

## convience function wich returns the result of reading a mockFile 
mockImage = function(..., normalize=FALSE) {
  read.image(mockFile(...), normalize=normalize)
}

tempimg = function() tempfile('',,'.tif')
