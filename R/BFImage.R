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
#' @export
dimorder = function(x) names(dim(x))
