#' @export
read.metadata2 <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE, omexml = FALSE) {
  reader = .getReader()
  on.exit(.closeReader(reader))
  .setupReader2(file, filter.metadata, proprietary.metadata, omexml)
  
  # harvest metadata
  metadata = .getMetadataList2(reader)
  
  if ( length(metadata)==1L ) 
    # return ImageMetadata object
    metadata[[1L]]
  else  
    # return ImageMetadataList object
    metadata
}
