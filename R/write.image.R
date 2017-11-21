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
  
  .jcall("RBioFormats", "V", "initializeMetadata")
  
  ## iterate over image series
  for (series in seq_len(seriesCount(x))) {
    y = if (is(x, "AnnotatedImageList")) x[[series]] else x
    
    dims = c(x = 1L, y = 1L, c = 1L, z = 1L, t = 1L)
    d = dim(y)
    o = dimorder(y)
    if (is.null(o)) 
      o = seq_along(d)
    dims[o] = d
    
    if (missing(pixelType))
      pixelType = coreMetadata(y)$pixelType
    if (is.null(pixelType))
      pixelType = "uint8"
    
    .jcall("RBioFormats", "V", "populateMetadata", .jarray(dims), series-1L, pixelType)
  }
  
  .jcall("RBioFormats", "V", "setupWriter", file)
  
  for (series in seq_len(seriesCount(x))) {
    y = if (is(x, "AnnotatedImageList")) x[[series]] else x
    
    dims = c(x = 1L, y = 1L, c = 1L, z = 1L, t = 1L)
    d = dim(y)
    o = dimorder(y)
    if (is.null(o)) 
      o = seq_along(d)
    dims[o] = d
    
    if (missing(pixelType))
      pixelType = coreMetadata(y)$pixelType
    if (is.null(pixelType))
      pixelType = "uint8"
    
    .jcall(writer, "V", "setSeries", series-1L)
    
    .jcall("RBioFormats", "V", "writePixels", .jarray(y), as.integer(prod(dims[3:5])), pixelType)
  }
  
  invisible(file)
}

.getWriter = function() .jcall("RBioFormats", "Lloci/formats/IFormatWriter;", "getWriter")
