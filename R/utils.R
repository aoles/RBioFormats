#' Bio-Formats Version Number
#'
#' Provides the version of the Bio-Formats library embedded in the package.
#' @export
BioFormats.version = function() .jfield("loci/formats/FormatTools", "S", "VERSION")
