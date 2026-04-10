#' @importFrom S4Vectors coolcat
setMethod("show", 
          "OMEZarrMeta", 
          function(object) {
            cat("class:", 
                class(object), 
                paste0("(v", ome_version(object), ")"),
                "\n")
          })

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
            meta(object)$multiscales[[1]]$datasets[[1]]$path
          })