#' @importFrom S4Vectors coolcat
setMethod("show", 
          "OMEZarrMeta", 
          function(object) {
            cat("class:", 
                class(object), 
                ome_version(object),
                # sprintf("(%s)", 
                #         paste0(.get_axes(meta(object)), collapse=",")), 
                "\n")
          })