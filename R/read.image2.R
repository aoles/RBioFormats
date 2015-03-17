.getReader = function() .jfield("RBioFormats", "Lloci/formats/DimensionSwapper;", "reader", true.class=FALSE, convert=TRUE)
.closeReader = function(reader) .jcall(reader, "V", "close")

#' @export
read.image2 <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, normalize = TRUE, series, subset, read.metadata = TRUE, strategy = 1L) {
  reader = .getReader()
  on.exit(.closeReader(reader))
  .setupReader2(file, filter.metadata, proprietary.metadata)
  
  if ( missing(subset) ) subset = list()
  
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
  
  metadata = if (read.metadata) .getMetadataList2(reader) else .getCoreMetadata(reader)
  
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
    
    ## set Image parameters
    colormode = if (length(subset$C) == 1) 0L else 2L
    czt = sapply(subset, length)    
    dim = c(X = metadata$coreMetadata$sizeX, Y = metadata$coreMetadata$sizeY, czt[czt > 1]) 
    
    new("AnnotatedImage", 
        .Data = array(
          data = switch(strategy,
            unlist(lapply(indices-1L, function(i) .jcall("RBioFormats", "Ljava/lang/Object;", "readPixels", i, isTRUE(normalize), evalArray=TRUE, use.true.class=TRUE) )),
            unlist(lapply(indices-1L, function(i)
              if ( isTRUE(normalize) ) .jcall("RBioFormats", "[D", "getNormalizedPixels", i, evalArray=TRUE)
              else .jcall("RBioFormats", "Ljava/lang/Object;", "getRawPixels", i, evalArray=TRUE, use.true.class=TRUE)
            )),
            if ( isTRUE(normalize) ) .jcall("RBioFormats", "[D", "getNormalizedPixels2", .jarray(indices-1L), evalArray=TRUE)
            else .jcall("RBioFormats", "Ljava/lang/Object;", "getRawPixels2", .jarray(indices-1L), evalArray=TRUE, use.true.class=TRUE)
          ),
          dim = setNames(dim, NULL),
          dimnames = setNames(vector("list", length(dim)), names(dim))
        ),
        colormode = colormode,
        metadata = ImageMetadata(metadata)
    )
  })
  
  if ( length(res) == 1L) return(res[[1L]])
  else return(AnnotatedImageList(res))
}

.setupReader2 <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, omexml = FALSE) {
  file = normalizePath(file, mustWork = FALSE)
  .jcall("RBioFormats", "V", "setupReader", file, filter.metadata, proprietary.metadata, omexml)
}

.getMetadataList2 = function (reader) {
  coreMetadataFields = list(
    sizeX = "I",
    sizeY = "I",
    sizeZ = "I",
    sizeC = "I",
    sizeT = "I",
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
    thumbnail = "Z",
    resolutionCount = "I",
    thumbnail = "Z"
  )
  
  globalMetadata = .getGlobalMetadata2(reader)
  
  coreMetadataList = .jcall(reader, "Ljava/util/List;", "getCoreMetadataList", use.true.class = TRUE)
  series = .jcall(coreMetadataList, "I", "size")
  
  ImageMetadataList(
    lapply(seq_len(series)-1L, function (i) {
      seriesCoreMetadata = .jcall(coreMetadataList, "Ljava/lang/Object;", "get", i, use.true.class = TRUE)
      coreMetadata = lapply(names(coreMetadataFields), function(field) {
        .jfield(seriesCoreMetadata, coreMetadataFields[[field]], field, FALSE)
      })
      names(coreMetadata) = names(coreMetadataFields)
      ## 
      coreMetadata$pixelType = .jcall("loci/formats/FormatTools", "S", "getPixelTypeString", coreMetadata$pixelType)
      seriesMetadata = .hashtableToList2( .jfield(seriesCoreMetadata, "Ljava/util/Hashtable;", "seriesMetadata") )
      
      ImageMetadata( list(
        coreMetadata = coreMetadata,
        seriesMetadata = seriesMetadata,
        globalMetadata = globalMetadata)
      )
    })
  )
}

.getGlobalMetadata2 = function (reader) {
  .hashtableToList2( .jcall(reader, "Ljava/util/Hashtable;", "getGlobalMetadata") )
}

.getSeriesMetadata2 = function (reader) {
  .hashtableToList2( .jcall(reader, "Ljava/util/Hashtable;", "getSeriesMetadata") )
}

.hashtableToList2= function (hashtable) {
  entries = .jcall(hashtable, "Ljava/util/Set;", "entrySet")
  setNames(
    lapply(entries, function(e) {
      val = .jsimplify(.jcall(e, "Ljava/lang/Object;", "getValue", use.true.class=TRUE))
      if ( inherits(val, "jobjRef") ) .jcall(val, "S", "toString") else val
    }),
    sapply(entries, function(e) .jcall(e, "Ljava/lang/Object;", "getKey", use.true.class=TRUE))
  )
}

.getCoreMetadata = function (reader) {
  coreMetadataFields = list(
    sizeX = "I",
    sizeY = "I",
    sizeZ = "I",
    sizeC = "I",
    sizeT = "I",
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
    thumbnail = "Z",
    resolutionCount = "I",
    thumbnail = "Z"
  )
  
  coreMetadataList = .jcall(reader, "Ljava/util/List;", "getCoreMetadataList", use.true.class = TRUE)
  series = .jcall(coreMetadataList, "I", "size")
  
  ImageMetadataList(
    lapply(seq_len(series)-1L, function (i) {
      seriesCoreMetadata = .jcall(coreMetadataList, "Ljava/lang/Object;", "get", i, use.true.class = TRUE)
      coreMetadata = lapply(names(coreMetadataFields), function(field) {
        .jfield(seriesCoreMetadata, coreMetadataFields[[field]], field, FALSE)
      })
      names(coreMetadata) = names(coreMetadataFields)
      ## 
      coreMetadata$pixelType = .jcall("loci/formats/FormatTools", "S", "getPixelTypeString", coreMetadata$pixelType)
      
      ImageMetadata( list(
        coreMetadata = coreMetadata,
        seriesMetadata = NULL,
        globalMetadata = NULL)
      )
    })
  )
}
