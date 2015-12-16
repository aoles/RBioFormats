#' Read Image Metadata
#' 
#' Read image metadata using the Bio-Formats library. The list of supported file formats can be found on the \href{http://www.openmicroscopy.org/site/support/bio-formats5/supported-formats.html}{Bio-Formats website}.
#' 
#' @inheritParams read.image
#' @return An \linkS4class{ImageMetadata} or \linkS4class{ImageMetadataList} object.
#' @examples
#' require(EBImage)
#' f = system.file("images", "nuclei.tif", package="EBImage")
#' 
#' metadata = read.metadata(f)
#' str(metadata)
#' 
#' @template author
#' @seealso \code{\link{read.omexml}} for reading image metadata as OME-XML, \code{\link{read.image}} for reading image data
#' @export
read.metadata <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, omexml = FALSE) {
  reader = .getReader()
  on.exit(.closeReader(reader))
  .setupReader(file, filter.metadata, proprietary.metadata, omexml)
  
  # harvest metadata
  metadata = .getMetadataList(reader)
  
  if ( length(metadata)==1L ) 
    # return ImageMetadata object
    metadata[[1L]]
  else  
    # return ImageMetadataList object
    metadata
}
