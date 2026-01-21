setMethod("ome_version", 
          "OMEZarrMeta", 
          function(object) {
            object@version
          })

setMethod("meta", 
          "OMEZarrMeta", 
          function(object) {
            object@listData
          })

setMethod("datasets", 
          "OMEZarrMeta", 
          function(object) {
            meta(object)$multiscales$datasets[[1]]$path
          })

read_ome_metadata <- function(group, s3_client = NULL) {
  zarr_meta <- Rarr::read_zarr_attributes(zarr_path = group, 
                                          s3_client = s3_client)
  # construct class
  S4Vectors::new2(
    "OMEZarrMeta", 
    listData = zarr_meta,
    version = .parse_ome_zarr_version(zarr_meta))
}

#' @noRd
.parse_ome_zarr_version <- function(x) {
  if (!is.null(ms <- x$multiscales)) 
    return(ms$version)
  return(NA_character_)
}

#' @noRd
.get_multiscale_datasets_path <- function(x) {
  x$multiscales$datasets[[1]]$path
}

#' @noRd
.get_axes <- function(x) {
  x$multiscales$axes[[1]]
}