library("RBioFormats")
library("rJava")

parameters = list (
  sizeX = 32L,
  sizeY = 32L,
  sizeZ = 5L,
  sizeC = 3L,
  sizeT = 2L,
  dimOrder = "XYTZC",
  interleaved = TRUE,
  rgb = 1L,
  indexed = TRUE
)

file = do.call(mockFile, parameters)

# reduce verbosity
.jcall("loci.common.DebugTools", "Z", "enableLogging", "ERROR")

## raw reader
reader = .jnew("loci.formats.ImageReader")

# initialize file
.jcall(reader, , "setId", file)

metadata = list (
    sizeX = .jcall(reader, "I", "getSizeX"),
    sizeY = .jcall(reader, "I", "getSizeY"),
    sizeZ = .jcall(reader, "I", "getSizeZ"),
    sizeC = .jcall(reader, "I", "getSizeC"),
    sizeT = .jcall(reader, "I", "getSizeT"),
    dimOrder = .jcall(reader, "S", "getDimensionOrder"),
    interleaved = .jcall(reader, "Z", "isInterleaved"),
    rgb = .jcall(reader, "I", "getRGBChannelCount"),
    indexed = .jcall(reader, "Z", "isIndexed")
  )

stopifnot(identical(names(parameters), names(metadata)))
stopifnot(identical(parameters, metadata))
