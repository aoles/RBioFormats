#' Read Image Metadata
#' 
#' Read image metadata using the Bio-Formats library. A list of supported formats can be found on the \href{http://www.openmicroscopy.org/site/support/bio-formats5/supported-formats.html}{Bio-Formats website}.
#' 
#' @param file character, file name
#' @param filter.metadata logical, specifies whether ugly metadata (entries with unprintable characters, and extremely large entries) should be discarded from the metadata table
#' @return A list containing image meta-data.
#' @examples
#' require(EBImage)
#' f = system.file("images", "nuclei.tif", package="EBImage")
#' 
#' metadata = read.metadata(f)
#' str(metadata)
#' 
#' @author Andrzej Oles \email{andrzej.oles@@embl.de}, 2014
#' @export
read.metadata <- function(file, filter.metadata = FALSE) {
  # reduce verbosity
  .jcall("loci.common.DebugTools", "Z", "enableLogging", "ERROR")
  
  # setup reader
  reader = .setupReader(file, filter.metadata)
  
  # initialize file
  .fileInit(reader, file)
  
  # harvest core metadata
  metadata = .getMetadataList(reader)
  
  if ( length(metadata) == 1 ) metadata = metadata[[1]]
  
  ## if no series present
  metadata
}

#' Image Metadata
#' 
#' @rdname metadata
#' @param x Object
#' @author Andrzej Oles \email{andrzej.oles@@embl.de}, 2014
#' @export
metadata <- function(x) UseMethod("metadata")

#' @rdname metadata
#' @export
metadata.default = function(x) NULL

#' @rdname metadata
#' @export
metadata.BFImage = function(x) x@metadata
