#' Metadata Accessors
#'
#' Get and set image metadata.
#'
#' The \ldots arguments are passed to \code{\link[base]{grep}} called on metadata names allowing for convenient subsetting.
#'
#' @rdname metadataAccessors
#' @param x an \linkS4class{AnnotatedImage}, \linkS4class{AnnotatedImageList}, \linkS4class{ImageMetadata}, or \linkS4class{ImageMetadataList} object
#' @param series series ID
#' @param ... arguments passed to \code{\link[base]{grep}}
#' @return Named list consisting of key value pairs.
#' @examples
#' img = read.image(system.file("images", "nuclei.tif", package="EBImage"))
#'
#' coreMetadata(img)
#'
#' # subset for specific names
#'
#' globalMetadata(img, pattern="Image")
#' @template author
#' @importMethodsFrom S4Vectors metadata "metadata<-"
#' @export
coreMetadata = function (x, series, ...) .getMetadata(x, series, ...)

#' @rdname metadataAccessors
#' @export
globalMetadata = function (x, series, ...) .getMetadata(x, series, ...)

#' @rdname metadataAccessors
#' @export
seriesMetadata = function (x, series, ...) .getMetadata(x, series, ...)

.getMetadata = function (x, series, ...) {
  ## metadata type equals accessor name
  sys_call <- sys.call(-1L)[[1L]]
  type = as.character(sys_call[[length(sys_call)]])# account for calls via `::`

  metadata = metadata(x)

  if ( inherits(x, "ImageMetadataList") ) {
    if ( !missing(series) ) metadata = metadata[series]
  }
  else metadata = list(metadata)

  metadata = setNames(lapply(metadata, function(x) x[[type]]), seq_along(metadata))

  ## grep
  if ( length(list(...)) > 0L )
    metadata = lapply(metadata, function(y) y[rev(grep(x=names(y), ...))])

  if ( length(metadata)==1L ) metadata = metadata[[1L]]

  metadata
}

#' Number of Image Series
#'
#' Get the number of image series contained in an object.
#'
#' Image series are encoded by \linkS4class{AnnotatedImageList} or \linkS4class{ImageMetadataList} objects. Therefore, only these objects can possibly yield image series count higher than 1 while for all the rest of image objects this number is expected to be 1.
#' @param x an images object.
#' @return The number of image series the object contains, see Details.
#' @template author
#' @example man-roxygen/ex-mockFileSeries.R
#' @examples
#' seriesCount(img)
#'
#' meta <- metadata(img)
#' seriesCount(meta)
#' @export
setGeneric("seriesCount", function(x) {
  standardGeneric("seriesCount")
})

#' @rdname seriesCount
#' @export
setMethod("seriesCount", "ANY", function(x) NA)

.singleSeries = function(x) 1L

.seriesList = function(x) length(x)

#' @rdname seriesCount
#' @export
setMethod("seriesCount", "matrix", .singleSeries)

#' @rdname seriesCount
#' @export
setMethod("seriesCount", "array", .singleSeries)

#' @rdname seriesCount
#' @export
setMethod("seriesCount", "ImageMetadata", .singleSeries)

#' @rdname seriesCount
#' @export
setMethod("seriesCount", "AnnotatedImageList", .seriesList)

#' @rdname seriesCount
#' @export
setMethod("seriesCount", "ImageMetadataList", .seriesList)

#' @rdname metadataAccessors
#' @export
setMethod("metadata", "ANY", function(x) NULL)

#' @rdname metadataAccessors
#' @export
setMethod("metadata", "AnnotatedImage", function(x) x@metadata)

#' @rdname metadataAccessors
#' @export
setMethod("metadata", "AnnotatedImageList", function(x) ImageMetadataList(lapply(x, metadata)))

#' @rdname metadataAccessors
#' @export
setMethod("metadata", "ImageMetadata", identity)

#' @rdname metadataAccessors
#' @export
setMethod("metadata", "ImageMetadataList", identity)

## setters

#' @rdname metadataAccessors
#' @param y an \linkS4class{AnnotatedImage} or \linkS4class{ImageMetadata} object
#' @param value depending on the context, an \linkS4class{ImageMetadata} object or a list
#' @importMethodsFrom S4Vectors metadata "metadata<-"
#' @export
setReplaceMethod("metadata", "AnnotatedImage", function (x, value) {
  x@metadata = value
  x
})

#' @rdname metadataAccessors
#' @export
setGeneric("coreMetadata<-", function(y, value) standardGeneric("coreMetadata<-"))

#' @rdname metadataAccessors
#' @export
setReplaceMethod("coreMetadata", "AnnotatedImage", function (y, value) {
  y@metadata$coreMetadata = value
  y
})

#' @rdname metadataAccessors
#' @export
setReplaceMethod("coreMetadata", "ImageMetadata", function (y, value) {
  y$coreMetadata = value
  y
})

#' @rdname metadataAccessors
#' @export
setGeneric("globalMetadata<-", function(y, value) standardGeneric("globalMetadata<-"))

#' @rdname metadataAccessors
#' @export
setReplaceMethod("globalMetadata", "AnnotatedImage", function (y, value) {
  y@metadata$globalMetadata = value
  y
})

#' @rdname metadataAccessors
#' @export
setReplaceMethod("globalMetadata", "ImageMetadata", function (y, value) {
  y$globalMetadata = value
  y
})

#' @rdname metadataAccessors
#' @export
setGeneric("seriesMetadata<-", function(y, value) standardGeneric("seriesMetadata<-"))

#' @rdname metadataAccessors
#' @export
setReplaceMethod("seriesMetadata", "AnnotatedImage", function (y, value) {
  y@metadata$seriesMetadata = value
  y
})

#' @rdname metadataAccessors
#' @export
setReplaceMethod("seriesMetadata", "ImageMetadata", function (y, value) {
  y$seriesMetadata = value
  y
})
