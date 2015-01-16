#' ImageMetadata Class
#' 
#' Formal representation of image metadata.
#' @export
setClass ("ImageMetadata", contains = "list")

#' @param metadata list containing image metadata
#' @rdname ImageMetadata-class
#' @export
ImageMetadata = function(metadata = list()) {
  return(
    new("ImageMetadata", .Data = metadata)
  )
}

#' @rdname ImageMetadata-class
#' @param object An ImageMetadata object
#' @export
setMethod ("show", signature(object = "ImageMetadata"), function(object) {
  str(object@.Data)
})

#' @rdname ImageMetadata-class
#' @param x An ImageMetadata object
#' @param ... further arguments to be passed to other methods
#' @export
print.ImageMetadata <- function(x, ...) {
  show(x)
}
