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
#' @export
coreMetadata = function (x, series, ...) .getMetadata(x, series, ...)

#' @rdname metadataAccessors
#' @inheritParams coreMetadata
#' @export
globalMetadata = function (x, series, ...) .getMetadata(x, series, ...)

#' @rdname metadataAccessors
#' @inheritParams coreMetadata
#' @export
seriesMetadata = function (x, series, ...) .getMetadata(x, series, ...)

.getMetadata = function (x, series, ...) {
  ## metadata type equals accessor name
  type = as.character(sys.call(-1L)[[1L]])

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
#' Get the number of image series
#' @param x an \code{AnnotatedImageList} or \code{ImageMetadataList} object
#' @return The number of image series.
#' @template author
#' @example man-roxygen/ex-mockFileSeries.R
#' @examples
#' seriesCount(img)
#'
#' meta <- metadata(img)
#' seriesCount(meta)
#' @export
seriesCount = function(x) UseMethod("seriesCount")

#' @export
seriesCount.default = function(x) NA

.singleSeries = function(x) 1L

.seriesList = function(x) length(x)

#' @export
seriesCount.matrix = .singleSeries

#' @export
seriesCount.array = .singleSeries

#' @export
seriesCount.ImageMetadata = .singleSeries

#' @export
seriesCount.AnnotatedImageList = .seriesList

#' @export
seriesCount.ImageMetadataList = .seriesList

#' @rdname metadataAccessors
#' @inheritParams seriesCount
#' @export
metadata <- function(x) UseMethod("metadata")

#' @export
metadata.default = function(x) NULL

#' @export
metadata.AnnotatedImage = function(x) x@metadata

#' @export
metadata.AnnotatedImageList = function(x) ImageMetadataList(lapply(x, metadata))

#' @export
metadata.ImageMetadata = identity

#' @export
metadata.ImageMetadataList = identity

## setters

#' @rdname metadataAccessors
#' @param y an \linkS4class{AnnotatedImage} or \linkS4class{ImageMetadata} object
#' @param value depending on the context, an \linkS4class{ImageMetadata} object or a list
#' @export
setGeneric("metadata<-", function(y, value) standardGeneric("metadata<-"))

#' @rdname metadataAccessors
#' @export
setReplaceMethod("metadata", "AnnotatedImage", function (y, value) {
  y@metadata = value
  y
})

#' @rdname metadataAccessors
#' @inheritParams metadata<-
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
#' @inheritParams metadata<-
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
#' @inheritParams metadata<-
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
