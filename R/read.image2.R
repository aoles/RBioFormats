.getReader = function() .jfield("RBioFormats", "Lloci/formats/DimensionSwapper;", "reader", true.class=FALSE, convert=TRUE)
.closeReader = function(reader) .jcall(reader, "V", "close")

.parseSeriesResolutions = function(reader, series, resolution) {
  .integerIndices = function(x, max, name) {
    # fail if not coercible to an integer within the (1, max) range
    w = options(warn=2L)
    x = tryCatch(as.integer(x), silent=TRUE)
    options(w)
    if ( inherits(x, "try-error") || !isTRUE(all(x > 0 & x <= max)) )
      stop(sprintf("Invalid %s specification.", name))
    else x
  }
  
  # check series specification
  seriesCount = .jcall(reader, "I", "getSeriesCount") # number of series per file
  series = 
    if ( missing(series) )
      # dafault case: read all series
      seq_len(seriesCount)
  else
    .integerIndices(series, seriesCount, "series")
  
  resolution = 
    if ( missing(resolution) )
      # dafault case: read all resolutions
      lapply(series, function (s) {
        .jcall(reader, , "setSeries", s-1L)
        rc = .jcall(reader, "I", "getResolutionCount")
        seq_len(rc)
      })
    else
      mapply(function(s, r) {
        .jcall(reader, , "setSeries", s-1L)
        res = .integerIndices(r, .jcall(reader, "I", "getResolutionCount"), "resolution")
        attr(res, "series") = s
      }, series, resolution, , SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  setNames(resolution, series)
}
  

#' @export
read.image2 <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, normalize = TRUE, series, resolution, subset, read.metadata = TRUE, strategy = 1L) {
  reader = .getReader()
  on.exit(.closeReader(reader))
  .setupReader2(file, filter.metadata, proprietary.metadata)
  
  if ( missing(subset) ) subset = list()
  
  resolutions = .parseSeriesResolutions(reader, series, resolution)
  
  metadata = 
    if ( isTRUE(read.metadata)) 
      .getMetadataList2(reader, resolutions)
    else
      .jcall(reader, "Ljava/util/List;", "getCoreMetadataList", use.true.class = TRUE)

  
  # create a list of (series, resolution) pairs
  series_resolution = unlist(mapply(function(s, r) mapply(c, s, r, SIMPLIFY=FALSE), as.integer(names(resolutions)), resolutions, SIMPLIFY=FALSE, USE.NAMES=FALSE), recursive=FALSE)
  
  # iterate over series and resolutions
  res = lapply(seq_along(series_resolution), function(i) {
    sr = series_resolution[[i]]
    .jcall(reader, , "setSeries", sr[1]-1L)
    .jcall(reader, , "setResolution", sr[2]-1L)
    
    metadata =
      if ( isTRUE(read.metadata) ) {
        metadata[[i]]        
      }
      else {
        coreMetadata = .jcall(metadata, "Ljava/lang/Object;", "get", .jcall(reader, "I", "getCoreIndex"), use.true.class = TRUE)
        coreMetadata = lapply(names(.coreMetadataFields), function(field) {
          .jfield(coreMetadata, .coreMetadataFields[[field]], field, true.class=FALSE)
        })
        names(coreMetadata) = names(.coreMetadataFields)
        ImageMetadata( list(
          coreMetadata = coreMetadata,
          seriesMetadata = NULL,
          globalMetadata = NULL
        ))
      }
    
    coreMetadata = metadata[["coreMetadata"]]
    
    ## get indices of image planes to read    
    czt = c(C = coreMetadata[["sizeC"]], Z = coreMetadata[["sizeZ"]], T = coreMetadata[["sizeT"]])
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
    dim = c(X = coreMetadata[["sizeX"]], Y = coreMetadata[["sizeY"]], czt[czt > 1]) 
    
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
        metadata = metadata
    )
  })
  
  if ( length(res) == 1L) return(res[[1L]])
  else return(AnnotatedImageList(res))
}

.setupReader2 <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, omexml = FALSE) {
  file = normalizePath(file, mustWork = FALSE)
  .jcall("RBioFormats", "V", "setupReader", file, filter.metadata, proprietary.metadata, omexml)
}

.getMetadataList2 = function (reader, resolutions) {
  if ( missing(resolutions) ) resolutions = .parseSeriesResolutions(reader)
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
    thumbnail = "Z"
  )
    
  series = as.integer(names(resolutions))
  
  globalMetadata = .getGlobalMetadata2(reader)
  coreMetadataList = .jcall(reader, "Ljava/util/List;", "getCoreMetadataList", use.true.class = TRUE)
  
  # iterate over series and resolutions
  
  ImageMetadataList(unlist(
    lapply(seq_along(series), function(i) {
      s = series[[i]]
      .jcall(reader, , "setSeries", s-1L)
      seriesMetadata = .getSeriesMetadata2(reader)
      
      lapply(resolutions[[i]], function(r) {
        .jcall(reader, , "setResolution", r-1L)
        coreMetadata = .jcall(coreMetadataList, "Ljava/lang/Object;", "get", .jcall(reader, "I", "getCoreIndex"), use.true.class = TRUE)
        coreMetadata = lapply(names(coreMetadataFields), function(field) {
          .jfield(coreMetadata, coreMetadataFields[[field]], field, FALSE)
        })
        names(coreMetadata) = names(coreMetadataFields)
        
        coreMetadata[["pixelType"]] = .jcall("loci/formats/FormatTools", "S", "getPixelTypeString", coreMetadata[["pixelType"]])
        #coreMetadata[["series"]] = s
        #coreMetadata[["resolutionLevel"]] = r
        
        ImageMetadata( list(
          coreMetadata = coreMetadata,
          seriesMetadata = seriesMetadata,
          globalMetadata = globalMetadata
        ))
      })
    }), recursive=FALSE)
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

.coreMetadataFields = list(
    sizeX = "I",
    sizeY = "I",
    sizeZ = "I",
    sizeC = "I",
    sizeT = "I"
  )

