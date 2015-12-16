#' Metadata Accessors
#' 
#' Get image metadata.
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
#' @param x an \code{AnnotatedImage} or \code{ImageMetadataList} object
#' @template author
#' @export
seriesCount = function(x) UseMethod("seriesCount")

#' @export
seriesCount.default = function(x) NA

#' @export
seriesCount.AnnotatedImage = function(x) 1L

#' @export
seriesCount.ImageMetadata = function(x) 1L

#' @export
seriesCount.AnnotatedImageList = function(x) length(x)

#' @export
seriesCount.ImageMetadataList = function(x) length(x)


#' Image Metadata
#' 
#' @rdname metadata
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
