#' Bio-Formats Image Class
#' 
#' Extends the \code{\link[EBImage]{Image}} class from the \pkg{EBImage} package.
#' 
#' @slot metadata named list containing image metadata
#' @slot omexml a string containing a dumped OME-XML DOM tree
#' @importClassesFrom EBImage Image
#' @export
setClass ("BFImage",
          contains = "Image",
          representation (
            metadata = "list",
            omexml = "character"
          )
)

#' @param image an Image object
#' @param metadata named list containing image metadata
#' @param omexml a string containing a dumped OME-XML DOM tree
#' @rdname BFImage-class
#' @importFrom EBImage imageData colorMode
#' @export
BFImage = function(image = Image(), 
                   metadata = list(),
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
  
  cat('\n')
  for (s in c("coreMetadata", "globalMetadata", "seriesMetadata")) {
    meta = do.call(s, list(object))
    if ( (l=length(meta)) > 0 ) {
      cat(s, ': ')
      str(meta, max.level = 0, list.len = l)
    } 
  }
})

#' @rdname BFImage-class
#' @param y A BFImage object
#' @return Named list consisting of key value pairs.
#' @author Andrzej Oles \email{andrzej.oles@@embl.de}, 2014
#' @examples
#' img = read.image(mockFile())
#' 
#' coreMetadata(img)
#' 
#' @export
coreMetadata = function (y) {
  if (is(y, 'BFImage')) y@metadata$coreMetadata
  else NULL
}

#' @rdname BFImage-class
#' @inheritParams coreMetadata
#' @export
globalMetadata = function (y) {
  if (is(y, 'BFImage')) y@metadata$globalMetadata
  else NULL
}

#' @rdname BFImage-class
#' @inheritParams coreMetadata
#' @export
seriesMetadata = function (y) {
  if (is(y, 'BFImage')) y@metadata$seriesMetadata
  else NULL
}

#' @param x BFImage objects
#' @importFrom EBImage as.Image
#' @rdname BFImage-class
#' @export
as.Image.BFImage = function(x) {
  y = as(x, "Image")
  dimnames(y) = NULL
  y
}

