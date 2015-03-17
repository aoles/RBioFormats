#' @export
read.omexml2 <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE) {
  on.exit(.closeReader(.getReader()))
  
  .setupReader2(file, filter.metadata, proprietary.metadata, omexml=TRUE)
  
  # dump OME XML
  .jcall(.jfield("RBioFormats", "Lloci/formats/meta/MetadataStore;", "omexml"), "S", "dumpXML")
}
