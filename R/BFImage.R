#' Bio-Formats Image Class
#' 
#' Extends the \code{\link[EBImage]{Image}} class from the \pkg{EBImage} package.
#' 
#' @slot coreMetadata named list containing image metadata
#' @slot globalMetadata named list containing image metadata
#' @slot seriesMetadata named list containing image metadata
#' @slot omexml a string containing a dumped OME-XML DOM tree
#' @importClassesFrom EBImage Image
#' @export
setClass ("BFImage",
          contains = "Image",
          representation (
            coreMetadata = "list",
            globalMetadata = "list",
            seriesMetadata = "list",
            omexml = "character"
          )
)

#' @param image an Image object
#' @param coreMetadata named list containing image metadata
#' @param globalMetadata named list containing image metadata
#' @param seriesMetadata named list containing image metadata
#' @param omexml a string containing a dumped OME-XML DOM tree
#' @importFrom EBImage Image
#' @rdname BFImage-class
#' @importFrom EBImage imageData colorMode
#' @export
BFImage = function(image = Image(), 
                   coreMetadata = list(),
                   globalMetadata = list(),
                   seriesMetadata = list(),
                   omexml = "") {
  return(
    new("BFImage", 
      .Data = imageData(image),
      colormode = colorMode(image),
      coreMetadata = coreMetadata,
      globalMetadata = globalMetadata,
      seriesMetadata = seriesMetadata,
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
#' @export
setMethod ("show", signature(object = "BFImage"), function(object) {
  callNextMethod()
  cat('\ncoreMetadata: ')
  str(coreMetadata(object))
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
  if (is(y, 'BFImage')) y@coreMetadata
  else NULL
}
