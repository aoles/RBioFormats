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
#' @template author
#' @seealso \code{\link{read.metadata}} for reading image metadata, \code{\link{read.image}} for reading image data
#' @export
read.omexml <- function(file, filter.metadata = FALSE, proprietary.metadata = TRUE) {
  on.exit( .close(.getReader()) )

  .setupReader(file, filter.metadata, proprietary.metadata, omexml=TRUE)

  # dump OME XML
  .jcall(.jcall("RBioFormats", "Lloci/formats/meta/MetadataStore;", "getOMEXML"), "S", "dumpXML")
}
