#' Read Images
#' 
#' Read image files using the Bio-Formats library. A list of supported formats can be found on the \href{http://www.openmicroscopy.org/site/support/bio-formats5/supported-formats.html}{Bio-Formats website}.
#' 
#' @param file character, file name
#' @param filter.metadata logical, specifies whether ugly metadata (entries with unprintable characters, and extremely large entries) should be discarded from the metadata table
#' @param proprietary.metadata logical, should proprietary metadata be populated to OME-XML
#' @param normalize logical, should the original image data be mapped to the [0,1] range
#' @param series integer vector specifying series to be read; if set to NA all series included in the file are read
#' @param subset named list specifing the image subsetting
#' @return A \code{\link{BFImage-class}} object or a list of \code{\link{BFImage-class}} objects in case of multi-series data.
#' @importFrom EBImage Color normalize
#' @examples
#' require(EBImage)
#' f = system.file("images", "sample-color.png", package="EBImage")
#' 
#' img = read.image(f)
#' img
#' 
#' @author Andrzej Oles \email{andrzej.oles@@embl.de}, 2014
#' @export
read.image <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, normalize = TRUE, series, subset) {
  if ( missing(subset) ) subset = list()
  
  # reduce verbosity
  .jcall("loci.common.DebugTools", "Z", "enableLogging", "ERROR")
  
  # setup reader
  reader = .setupReader(filter.metadata, proprietary.metadata)
  
  # create OME-XML metadata store
  factory = .jnew("loci.common.services.ServiceFactory")
  service = .jcall(factory, "Lloci/common/services/Service;", "getInstance", J("loci.formats.services.OMEXMLService")$class)
  meta = .jcall(service, "Lloci/formats/ome/OMEXMLMetadata;", "createOMEXMLMetadata")
  .jcall(reader, , "setMetadataStore", .jcast(meta, "loci.formats.meta.MetadataStore"))
  
  # initialize file
  .fileInit(reader, file)
  
  # harvest core metadata
  metadata = .getCoreMetadata(reader)
  
  # check series specification
  seriesCount = .jcall(reader, "I", "getSeriesCount") # number of series per file
  if ( missing(series) ) {
    # dafault case: read all series
    series = seq_len(seriesCount)
  } else {
    # fail if not coercible to an integer within the (1, seriesCount) range
    w = options(warn=2)
    series = tryCatch(as.integer(series), silent = TRUE)
    options(w)
    if ( inherits(series, "try-error") || !isTRUE(all(series > 0 & series <= seriesCount)) )
      stop("Invalid series specification.")
  }

  metadata = .getMetadataList(reader)
  
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
  
  # iterate over series
  res = lapply(series, function(s) {
    .jcall(reader, , "setSeries", s-1L)
    metadata = metadata[[s]]
    # read image planes as byte vector
    imageCount = metadata$coreMetadata$imageCount
    
    ## get indices of image planes to read    
    czt = c(C = metadata$coreMetadata$sizeC, Z = metadata$coreMetadata$sizeZ, T = metadata$coreMetadata$sizeT)
    subset = setNames(lapply(names(czt), function(d) {
      if ( is.null(subset[[d]]) ) 
        seq_len(czt[d])
      else {
        sub = subset[[d]]
        sub[ sub >= 1 & sub <= czt[d] ]
        }
      }), names(czt))
        
    indices = subset[[1L]]
    for (d in 2L:length(czt)) {
      czt[d] = czt[d] * czt[d-1] # instead of cumprod to preserve integers
      indices = as.vector(sapply( (subset[[d]] - 1L) * czt[d-1], function (i) i + indices))
    }
    indices = as.integer(indices)
    
    bytes = unlist(lapply(indices-1L, function(i) .jcall(reader, "[B", "openBytes", i)) )
    
    # seriesMetadata = metadata$.getSeriesMetadata(reader)  
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
    colormode = if (length(subset$C) == 1) 0L else 2L
    czt = sapply(subset, length)    
    dim = c(X = metadata$coreMetadata$sizeX, Y = metadata$coreMetadata$sizeY, czt[czt > 1]) 
  
    data = array(data, setNames(dim, NULL), dimnames = setNames(vector("list", length(dim) ), names(dim)) )
    
    if ( isTRUE(normalize) ) {
      ## adjust range for UINT
      if ( !signed && !fp ) range = c(0, 2^metadata$coreMetadata$bitsPerPixel-1)
      data = normalize(data, separate = FALSE, ft = c(0, 1), inputRange = range)
    }
    new("BFImage", 
        .Data = data,
        colormode = colormode,
        metadata = metadata,
        omexml = omexml)
  })
  
  if ( length(res) == 1L) return(res[[1L]])
  else return(res)
}

.setupReader <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE) {
  
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
  
  reader
}

.fileInit <- function (reader, file) {
  file = normalizePath(file, mustWork = FALSE)
  .jcall(reader, , "setId", file)
  .jcall(reader, , "setOutputOrder", "XYCZT")
  invisible(NULL)
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

.getMetadataList = function (reader) {
  coreMetadataFields = list(
    sizeX = "I",
    sizeY = "I",
    sizeZ = "I",
    sizeC = "I",
    sizeT = "I",
    thumbSizeX = "I",
    thumbSizeY = "I",
    pixelType = "I",
    bitsPerPixel = "I",
    imageCount = "I",
    dimensionOrder = "S",
    orderCertain = "Z",
    rgb = "Z",
    littleEndian = "Z",
    interleaved = "Z",
    falseColor = "Z",
    metadataComplete = "Z",
    thumbnail = "Z"
  )
    
  globalMetadata = .getGlobalMetadata(reader)
  
  coreMetadataList = .jcall(reader, "Ljava/util/List;", "getCoreMetadataList", use.true.class = TRUE)
  series = .jcall(coreMetadataList, "I", "size")
  
  metadata = lapply(seq_len(series)-1L, function (i) {
    seriesCoreMetadata = .jcall(coreMetadataList, "Ljava/lang/Object;", "get", i, use.true.class = TRUE)
    coreMetadata = lapply(names(coreMetadataFields), function(field) {
      .jfield(seriesCoreMetadata, coreMetadataFields$field, field)
    })
    names(coreMetadata) = names(coreMetadataFields)
    ## 
    coreMetadata$pixelType = .jcall("loci/formats/FormatTools", "S", "getPixelTypeString", coreMetadata$pixelType)
    seriesMetadata = .hashtableToList( .jfield(seriesCoreMetadata, "Ljava/util/Hashtable;", "seriesMetadata") )
    
    list ( coreMetadata = coreMetadata,
           seriesMetadata = seriesMetadata,
           globalMetadata = globalMetadata)
  })

  ## if no series present
  #if ( length(metadata) == 1 ) metadata = metadata[[1]]
    
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
  setNames(lapply(keys, function(key) {
    val = hashtable$get(key)
    if ( is(val, "jobjRef") ) .jcall(val, "S", "toString")
    else val
  }), sapply(keys, .jsimplify))
}
