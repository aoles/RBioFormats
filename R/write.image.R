#' Write Images
#' 
#' Save image files using the Bio-Formats library. A list of supported formats can be found on the \href{http://www.openmicroscopy.org/site/support/bio-formats5/supported-formats.html}{Bio-Formats website}.
#' 
#' @param x an \code{Image} or \code{\link{AnnotatedImage}} object
#' @param file character, file name
#' @param force logical(1), if \code{link} overwrite existing file
#' @param pixelType character(1), data type to store pixel values
#' 
#' @template author
#' @seealso \code{\link{read.image}} for reading images.
#' @export
write.image <- function(x, file, force = FALSE, pixelType) {
  
  #writer = .getWriter()
  #on.exit( .close(writer) )

  ## setup writer
  file = normalizePath(file, mustWork = FALSE)
  #.jcall(writer, "V", "setId", file)
  
  if (file.exists(file))
    if (isTRUE(force))
      file.remove(file)
    else
      stop(sprintf('File %s already exists: use "force = TRUE" to overwrite', file))
  
  dimargs = c(X = 1L, Y = 1L, C = 1L, Z = 1L, T = 1L)
  d = dim(x)
  o = dimorder(x)
  if (is.null(o)) 
    o = seq_along(d)
  dimargs[o] = d
  
  if (missing(pixelType))
    pixelType = coreMetadata(x)$pixelType
  if (is.null(pixelType))
    pixelType = "uint8"
  
  ## iterate over image series
  ## this will require to initialize the writer outside of 'writePixels'
  series = 0L
  
  .jcall("RBioFormats", "V", "writePixels", file, .jarray(x), .jarray(dimargs), series, pixelType)
  
  invisible()
}

.getWriter = function() .jcall("RBioFormats", "Lloci/formats/IFormatWriter;", "getWriter")
