#' Write Images
#' 
#' Save image files using the Bio-Formats library. A list of supported formats can be found on the \href{http://www.openmicroscopy.org/site/support/bio-formats5/supported-formats.html}{Bio-Formats website}.
#' 
#' @param x an \code{Image} or \code{\link{AnnotatedImage}} object
#' @param file character, file name
#' @param force logical(1), if \code{link} overwrite existing file
#' @param pixelType character(1), data type to store pixel values
#' @return File path to \code{file} is returned invisibly.
#' @template author
#' @seealso \code{\link{read.image}} for reading images.
#' @export
write.image <- function(x, file, force = FALSE, pixelType) {
  writer = .getWriter()
  on.exit( .close(writer) )

  file = normalizePath(file, mustWork = FALSE)
  if (file.exists(file))
    if (isTRUE(force))
      file.remove(file)
    else
      stop(sprintf('File %s already exists: use "force = TRUE" to overwrite', file))
  
  dimargs = c(x = 1L, y = 1L, c = 1L, z = 1L, t = 1L)
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
  for (series in seq_len(seriesCount(x))) {
    .setupWriter(file, dimargs, series-1L, pixelType)
    .jcall("RBioFormats", "V", "writePixels", file, .jarray(x), .jarray(dimargs), series-1L, pixelType)
  }
  
  invisible(file)
}

.getWriter = function() .jcall("RBioFormats", "Lloci/formats/IFormatWriter;", "getWriter")

.setupWriter <- function(file, dimargs, series, pixelType) {
  .jcall("RBioFormats", "V", "setupWriter", file, .jarray(dimargs), series, pixelType)
}
