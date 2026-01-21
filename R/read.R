####
# Methods ####
####

#' Methods for OMEZarrArray
#'
#' Methods for \code{OMEZarrArray} objects
#'
#' @param x,object An OMEZarrArray object
#' @param i Depends on the usage
#' \describe{
#'  \item{\code{[[}}{
#'    Here \code{i} is the level of the image pyramid.
#'    You can use the \code{length} function to get the
#'    number of the layers in the pyramid.
#'  }
#' }
#'
#' @name OMEZarrArray-methods
#' @rdname OMEZarrArray-methods
#'
#' @aliases
#' [[,OMEZarrArray,numeric-method
#' meta
#' meta,ImageArray-method
#'
NULL

#' @describeIn OMEZarrArray-methods Layer access
#' for \code{OMEZarrArray} objects
#'
#' @export
setMethod(
  f = '[[',
  signature = c('OMEZarrArray', "numeric"),
  definition = function(x, i) {
    return(x@levels[[i]])
  }
)

#' @describeIn OMEZarrArray-methods dimensions of an OMEZarrArray
#' @export
#' @returns dim of the first level of the OMEZarrArray object
setMethod("dim", "OMEZarrArray", function(x) dim(x[[1]]))

#' @describeIn OMEZarrArray-methods dimensions of an OMEZarrArray
#' @export
#' @importFrom DelayedArray type
#' @returns type of OMEZarrArray object
setMethod("type", "OMEZarrArray", function(x) type(x[[1]]))

#' @describeIn OMEZarrArray-methods length of an OMEZarrArray
#' @export
#' @returns length of OMEZarrArray object
setMethod("length", signature = "OMEZarrArray", function(x) length(x@levels))

#' @describeIn OMEZarrArray-methods length of an OMEZarrArray
#' @export
#' @returns length of OMEZarrArray object
setMethod("meta", signature = "OMEZarrArray", function(object) meta(object@meta))

#' @describeIn OMEZarrArray-methods OMEZarrArray constructor method
#'
#' A function for creating objects of OMEZarrArray class
#'
#' @param group description
#' @param s3_client Object created by \code{paws.storage::s3()}. 
#' Only required for a file on S3. Leave as NULL for a file on local storage.
#' 
#' @importFrom S4Vectors new2
#' @importFrom Rarr read_zarr_array
#' @export
#' @return An OMEZarrArray object
read_image <- function(group){
  
  # read metadata
  meta <- read_ome_metadata(group = group, 
                            s3_client = Rarr:::.create_s3_client(group))
  
  # read levels
  levels <- lapply(datasets(meta) , function(dataset){
    Rarr::ZarrArray(
      zarr_array_path = file.path(group, dataset)
    )
  })
  
  # construct class
  S4Vectors::new2("OMEZarrArray", meta = meta, levels = levels)
}
