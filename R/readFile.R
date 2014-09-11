#' Read Images
#' 
#' Read image files using the Bio-Formats library
#' 
#' @param file character, file name
#' @param filter.metadata logical, specifies whether ugly metadata (entries with unprintable characters, and extremely large entries) should be discarded from the metadata table
#' @param proprietary.metadata logical, should proprietary metadata be populated to OME-XML
#' @param normalize logical, should the original image data be mapped to the [0,1] range
#' @return A \code{\link{BFImage-class}} object.
#' @importFrom EBImage Color normalize
#' @examples
#' require(EBImage)
#' f = system.file("images", "lena-color.png", package="EBImage")
#' 
#' img = read.image(f)
#' img
#' 
#' @author Andrzej Oles \email{andrzej.oles@@embl.de}, 2014
#' @export
read.image <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, normalize = TRUE) {
  file = path.expand(file)
  
  # reduce verbosity
  .jcall("loci.common.DebugTools", "Z", "enableLogging", "ERROR")
  
  # create a reader that will automatically handle any supported format
  reader = .jcast(.jnew("loci.formats.ImageReader"), "loci.formats.IFormatReader")
  
  # convert indexed color images to RGB images.
  reader = .jcast(.jnew("loci.formats.ChannelFiller", reader), "loci.formats.IFormatReader")
  
  # split RGB images into 3 separate grayscale images
  reader = .jcast(.jnew("loci.formats.ChannelSeparator", reader), "loci.formats.IFormatReader")
  
  # enable setting output dimension order
  reader = .jcast(.jnew("loci.formats.DimensionSwapper", reader), "loci.formats.IFormatReader")
    
  # set metadata options
  .jcall(reader, , "setMetadataFiltered", isTRUE(filter.metadata))
  .jcall(reader, , "setOriginalMetadataPopulated", isTRUE(proprietary.metadata))

  # create OME-XML metadata store
  factory = .jnew("loci.common.services.ServiceFactory")
  service = .jcall(factory, "Lloci/common/services/Service;", "getInstance", J("loci.formats.services.OMEXMLService")$class)
  meta = .jcall(service, "Lloci/formats/ome/OMEXMLMetadata;", "createOMEXMLMetadata")
  .jcall(reader, , "setMetadataStore", .jcast(meta, "loci.formats.meta.MetadataStore"))
  
  # initialize file
  .jcall(reader, , "setId", file)
  .jcall(reader, , "setOutputOrder", "XYCZT")
  
  # harvest core metadata
  metadata = .getCoreMetadata(reader)
    
  seriesMetadata = .getSeriesMetadata(reader)
  globalMetadata = .getGlobalMetadata(reader)
  
  # dump OME XML
  omexml = .jcall(meta, "S", "dumpXML")

  ## pixel data redout
  FormatTools = J("loci.formats.FormatTools")
  DataTools = J("loci.common.DataTools")
  
  type = .jcall(reader, "I", "getPixelType")
  bpp = .jcall(FormatTools, "I", "getBytesPerPixel", type)
  fp = .jcall(FormatTools, "Z", "isFloatingPoint", type)
  little = .jcall(reader, "Z", "isLittleEndian")
  signed = .jcall(FormatTools, "Z", "isSigned", type)
  range = .jcall(FormatTools, "[J", "defaultMinMax", type)
    
  # read image planes as byte vector
  bytes = unlist(lapply(seq_len(metadata$imageCount)-1L, function(i) .jcall(reader, "[B", "openBytes", i)) )
     
  # data = .jcall(DataTools, "Ljava/lang/Object;", "makeDataArray", bytes, bpp, fp, little, use.true.class = TRUE, evalArray = TRUE)
  
  # convert bytes to numbers
  data = readBin(bytes,
                 what = if (fp) "double" else "integer",
                 n = length(bytes),
                 size = bpp,
                 signed = signed,
                 endian = if(little) "little" else "big")
  rm(bytes)
  
  ## set Image parameters
  colormode = if (metadata$sizeC == 1) 0L else 2L
  dim = .setOutputDim(metadata)

  data = array(data, dim)
  
  if ( isTRUE(normalize) )
    data = normalize(data, separate = FALSE, ft = c(0, 1), inputRange = range)
  
  new("BFImage", 
      .Data = data,
      colormode = colormode,
      coreMetadata = metadata,
      globalMetadata = globalMetadata,
      seriesMetadata = seriesMetadata,
      omexml = omexml)
}

.setOutputDim = function(metadata) {
  czt = c(C = metadata$sizeC, Z = metadata$sizeZ, T = metadata$sizeT)
  dim = c(X = metadata$sizeX, Y = metadata$sizeY, czt[czt > 1])
  dim  
}

# retrieve core metadata needed to work with the planes in a file
.getCoreMetadata = function (reader) {
  metadata = list()
  
  # core metadata fields
  metadata$sizeX = .jcall(reader, "I", "getSizeX") # image width
  metadata$sizeY = .jcall(reader, "I", "getSizeY") # image height
  metadata$sizeZ = .jcall(reader, "I", "getSizeZ") # number of slices in the current series
  metadata$sizeC = .jcall(reader, "I", "getSizeC") # number of actual channels in the current series
  metadata$sizeT = .jcall(reader, "I", "getSizeT") # number of timepoints in the current series
  metadata$seriesCount = .jcall(reader, "I", "getSeriesCount") # number of series per file
  metadata$imageCount = .jcall(reader, "I", "getImageCount") # total number of images per series
  metadata$RGBChannelCount = .jcall(reader, "I", "getRGBChannelCount") # number of channels per image
  metadata$dimensionOrder = .jcall(reader, "S", "getDimensionOrder") # the ordering of the images within the current series
  metadata$isRGB = .jcall(reader, "Z", "isRGB") # whether each image is RGB
  metadata$isLittleEndian = .jcall(reader, "Z", "isLittleEndian") # whether the pixel bytes are in little-endian order
  metadata$isInterleaved = .jcall(reader, "Z", "isInterleaved") # whether the channels in an image are interleaved
  metadata$pixelType = .jcall("loci/formats/FormatTools", "S", "getPixelTypeString", .jcall(reader, "I", "getPixelType")) # the type of pixel data in this file
  
  metadata
}

.getCoreMetadataList = function (reader) {
  metadata = list()
  
  l = .jcall(reader, "Ljava/util/List;", "getCoreMetadataList", use.true.class = TRUE)
  
  h = .getGlobalMetadata(reader)
  
  
  opts = reader$getMetadataOptions()
  opts$getMetadataLevel()
  
  # lapply over a Vector (implements Iterable)
  v <- .jnew("java/util/ArrayList")
  v$add( "foo" )
  v$add( .jnew("java/lang/Double", 10.2 ) )
  sapply( h, function(item) item$getClass()$getName() )
  v
  
  metadata
}

.getGlobalMetadata = function (reader) {
  .hashtableToList( .jcall(reader, "Ljava/util/Hashtable;", "getGlobalMetadata") )
}

.getSeriesMetadata = function (reader) {
  .hashtableToList( .jcall(reader, "Ljava/util/Hashtable;", "getSeriesMetadata") )
}

.hashtableToList = function (hashtable) {
  keys = .jcall(hashtable, "Ljava/util/Set;", "keySet")  
  setNames(lapply(keys, function(key) hashtable$get(key)), sapply(keys, .jsimplify))
}
