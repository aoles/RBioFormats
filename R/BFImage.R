#' Bio-Formats Image Class
#' 
#' Extends the \code{\link[EBImage]{Image}} class from the \pkg{EBImage} package.
#' 
#' @slot omexml a string containing a dumped OME-XML DOM tree
#' @importClassesFrom EBImage Image
#' @importClassesFrom S4Vectors Annotated
#' @export
setClass ("BFImage",
          contains = c("Annotated", "Image"),
          representation (
            omexml = "character"
          )
)

#' @param image an Image object
#' @param metadata an ImageMetadata object containing image metadata
#' @param omexml a string containing a dumped OME-XML DOM tree
#' @rdname BFImage-class
#' @importFrom EBImage imageData colorMode
#' @export
BFImage = function(image = Image(), 
                   metadata = ImageMetadata(),
                   omexml = "") {
  return(
    new("BFImage", 
      .Data = imageData(image),
      colormode = colorMode(image),
      metadata = metadata,
      omexml = omexml
    )
  )
}

#' Get the ordering of the image frames
#' 
#' @param x An Image object or an array
#' @return A character vector giving the dimension labels.
#' @author Andrzej Oles \email{andrzej.oles@@embl.de}, 2014
#' @examples
#' # sample timelapse image
#' img = read.image(mockFile(sizeC = 3, sizeT = 10))
#' 
#' dimorder(img)
#' 
#' @export
dimorder = function(x) names(dimnames(x))

#' @rdname BFImage-class
#' @param object A BFImage object
#' @export
setMethod ("show", signature(object = "BFImage"), function(object) {
  callNextMethod()
  
  printMetadata(object)
})

printMetadata <- function(object) {
  cat('\n')
  for (s in c("coreMetadata", "globalMetadata", "seriesMetadata")) {
    meta = do.call(s, list(object))
    if ( (l=length(meta)) > 0 ) {
      cat(s, ': ')
      str(meta, max.level = 0, list.len = l)
    } 
  }
}

#' @rdname BFImage-class
#' @inheritParams as.Image.BFImage
#' @param ... further arguments to be passed to other methods
#' @param short turns off image data preview
#' @export
print.BFImage <- function(x, short=FALSE, ...) {
  NextMethod(x, short, ...)
  printMetadata(x)
}

#' Metadata Accessor Functions
#' 
#' @rdname metadataAccessors
#' @param y A BFImage object or metadata list
#' @param series series ID
#' @param ... arguments passed to \code{grep}
#' @return Named list consisting of key value pairs.
#' @author Andrzej Oles \email{andrzej.oles@@embl.de}, 2014
#' @examples
#' img = read.image(system.file("images", "nuclei.tif", package="EBImage"))
#'  
#' coreMetadata(img)
#' 
#' # subset for specific names 
#' 
#' globalMetadata(img, pattern="Image")
#' @export
coreMetadata = function (y, series=1L, ...) .getMetadata(y, series, "coreMetadata", ...)

#' @rdname metadataAccessors
#' @inheritParams coreMetadata
#' @export
globalMetadata = function (y, series=1L, ...) .getMetadata(y, series, "globalMetadata", ...)

#' @rdname metadataAccessors
#' @inheritParams coreMetadata
#' @export
seriesMetadata = function (y, series=1L, ...) .getMetadata(y, series, "seriesMetadata", ...)

.getMetadata = function (x, series, type, ...) {
  metadata = 
    if (is(x, 'BFImage')) metadata(x)[[type]]
    else if (is.list(x)) x[[series]][[type]]
    else NULL
  
  if ( length(list(...)) > 0 )
    metadata = metadata[grep(x=names(metadata), ...)]
  
  metadata
}

#' @param x a BFImage object
#' @importFrom EBImage as.Image
#' @rdname BFImage-class
#' @export
as.Image.BFImage = function(x) {
  y = as(x, "Image")
  dimnames(y) = NULL
  y
}

