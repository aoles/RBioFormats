#' Read Images
#' 
#' Read image files using the Bio-Formats library. A list of supported formats can be found on the \href{http://www.openmicroscopy.org/site/support/bio-formats5/supported-formats.html}{Bio-Formats website}.
#' 
#' @param file character, file name
#' @param filter.metadata logical, specifies whether ugly metadata (entries with unprintable characters, and extremely large entries) should be discarded from the metadata table
#' @param proprietary.metadata logical, should proprietary metadata be populated to OME-XML
#' @param normalize logical, should the original image data be mapped to the [0,1] range
#' @param series integer vector specifying series to read; if missing all series included in the file are read
#' @param resolution integer vector specifying resolution levels to read; if missing all levels read
#' @param subset named list specifing image subsetting
#' @param read.metadata logical, should image metadata be read
#' @return A \code{\link{AnnotatedImage}} object or a \code{\link{AnnotatedImageList}} object in case of multi-series data.
#' @importFrom EBImage Color normalize
#' @examples
#' require(EBImage)
#' f = system.file("images", "sample-color.png", package="EBImage")
#' 
#' img = read.image(f)
#' img
#' 
#' @template author
#' @seealso \code{\link{read.metadata}} for reading image metadata, \code{\link{read.omexml}} for reading image metadata as OME-XML
#' @export
read.image <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, normalize = TRUE, series, resolution, subset, read.metadata = TRUE) {
  reader = .getReader()
  on.exit( .close(reader) )
  .setupReader(file, filter.metadata, proprietary.metadata)
  
  if ( missing(subset) ) subset = list()
  
  resolutions = .parseSeriesResolutions(reader, series, resolution)
  
  metadata = 
    if ( isTRUE(read.metadata)) 
      .getMetadataList(reader, resolutions)
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
    xyczt = c(X = coreMetadata[["sizeX"]], Y = coreMetadata[["sizeY"]], C = coreMetadata[["sizeC"]], Z = coreMetadata[["sizeZ"]], T = coreMetadata[["sizeT"]])
    subset = setNames(lapply(names(xyczt), function(d) {
      if ( is.null(subset[[d]]) )
        seq_len(xyczt[d])
      else {
        sub = subset[[d]]
        sub[ sub >= 1L & sub <= xyczt[d] ]
      }
    }), names(xyczt))
    
    indices = subset[[3L]]
    for (d in 4:5) {
      xyczt[d] = xyczt[d] * xyczt[d-1] # instead of cumprod to preserve integers
      indices = as.vector(sapply( (subset[[d]] - 1L) * xyczt[d-1], function (i) i + indices))
    }
    indices = as.integer(indices)
    
    ## set Image parameters
    colormode = if (length(subset$C) == 1) 0L else 2L
    xyczt = vapply(subset, length, integer(1L))
    xy = vapply(subset[c("X", "Y")], function(x) x[1L], integer(1L)) - 1L
    wh = xyczt[1:2]
    czt = xyczt[3:5]
    dim = c(wh, czt[czt > 1L])
    
    new("AnnotatedImage", 
        .Data = array(
          data = unlist(lapply(indices-1L, function(i) .jcall("RBioFormats", "Ljava/lang/Object;", "readPixels", i, xy[1L], xy[2L], wh[1L], wh[2L], isTRUE(normalize), evalArray=TRUE, use.true.class=TRUE) )),
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

.setupReader <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, omexml = FALSE) {
  file = normalizePath(file, mustWork = FALSE)
  .jcall("RBioFormats", "V", "setupReader", file, filter.metadata, proprietary.metadata, omexml)
}

.getMetadataList = function (reader, resolutions) {
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
  
  globalMetadata = .getGlobalMetadata(reader)
  coreMetadataList = .jcall(reader, "Ljava/util/List;", "getCoreMetadataList", use.true.class = TRUE)
  
  # iterate over series and resolutions
  
  ImageMetadataList(unlist(
    lapply(seq_along(series), function(i) {
      s = series[[i]]
      .jcall(reader, , "setSeries", s-1L)
      seriesMetadata = .getSeriesMetadata(reader)
      
      lapply(resolutions[[i]], function(r) {
        .jcall(reader, , "setResolution", r-1L)
        coreMetadata = .jcall(coreMetadataList, "Ljava/lang/Object;", "get", .jcall(reader, "I", "getCoreIndex"), use.true.class = TRUE)
        coreMetadata = lapply(names(coreMetadataFields), function(field) {
          .jfield(coreMetadata, coreMetadataFields[[field]], field, FALSE)
        })
        names(coreMetadata) = names(coreMetadataFields)
        
        coreMetadata[["pixelType"]] = .jcall("loci/formats/FormatTools", "S", "getPixelTypeString", coreMetadata[["pixelType"]])
        coreMetadata[["series"]] = s
        coreMetadata[["resolutionLevel"]] = r
        
        ImageMetadata( list(
          coreMetadata = coreMetadata,
          seriesMetadata = seriesMetadata,
          globalMetadata = globalMetadata
        ))
      })
    }), recursive=FALSE)
  )
}

.getGlobalMetadata = function (reader) {
  .hashtableToList( .jcall(reader, "Ljava/util/Hashtable;", "getGlobalMetadata") )
}

.getSeriesMetadata = function (reader) {
  .hashtableToList( .jcall(reader, "Ljava/util/Hashtable;", "getSeriesMetadata") )
}

.hashtableToList = function (hashtable) {
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

.getReader = function() .jcall("RBioFormats", "Lloci/formats/IFormatReader;", "getReader")

.close = function(object) .jcall(object, "V", "close")

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
    if ( missing(series) )  # dafault case: read all series
      seq_len(seriesCount)
    else
      .integerIndices(series, seriesCount, "series")
  
  resolutions = lapply(series, function (s, r) {
    .jcall(reader, , "setSeries", s-1L)
    resolutionCount = .jcall(reader, "I", "getResolutionCount")
    
    if ( missing(r) ) # dafault case: read all resolutions
      seq_len(resolutionCount)
    else
      .integerIndices(r, resolutionCount, "resolution")
  }, resolution)
  
  setNames(resolutions, series)
}
