library("RBioFormats")

parameters = list (
  sizeX = 32L,
  sizeY = 32L,
  sizeZ = 5L,
  sizeC = 1L,
  sizeT = 2L,
  dimOrder = "XYTZC",
  interleaved = TRUE,
  indexed = FALSE
)

img = read.image(do.call(mockFile, parameters))

metadata = coreMetadata(img)
# metadata$indexed
# metadata
stopifnot(metadata$sizeC == 1L)
stopifnot(metadata$rgb == FALSE)
# stopifnot(.jcall(reader, "Z", "isIndexed") == FALSE)
# stopifnot(.jcall(reader, "I", "getImageCount") == 30)
# stopifnot(.jcall(reader, "S", "getDimensionOrder") == "XYCZT")
# 
# metadata2 = .metadata(reader)
# metadata = RBioFormats:::.getCoreMetadata(reader)
