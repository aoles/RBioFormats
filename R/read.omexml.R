#' Read OME-XML Metadata
#' 
#' Read the OME-XML DOM tree.
#' 
#' @inheritParams read.image
#' @return A string containing a dumped OME-XML DOM tree.
#' @examples
#' require(EBImage)
#' f = system.file("images", "nuclei.tif", package="EBImage")
#' 
#' omexml = read.omexml(f)
#' omexml
#' 
#' @template author
#' @seealso \code{\link{read.metadata}} for reading image metadata, \code{\link{read.image}} for reading image data
#' @export
read.omexml <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE) {
  # setup reader
  reader = .setupReader(filter.metadata, proprietary.metadata)
  
  # create OME-XML metadata store
  factory = .jnew("loci.common.services.ServiceFactory")
  service = .jcall(factory, "Lloci/common/services/Service;", "getInstance", J("loci.formats.services.OMEXMLService")$class)
  meta = .jcall(service, "Lloci/formats/ome/OMEXMLMetadata;", "createOMEXMLMetadata")
  .jcall(reader, , "setMetadataStore", .jcast(meta, "loci.formats.meta.MetadataStore"))
  
  # initialize file
  .fileInit(reader, file)
  
  # dump OME XML
  .jcall(meta, "S", "dumpXML")
}
