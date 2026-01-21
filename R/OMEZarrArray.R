#' @importFrom S4Vectors coolcat
setMethod("show", 
          "OMEZarrArray", 
          function(object) {
            n.object <- length(object@levels)
            cat("class:", class(object), sprintf("(%s)", paste0(.get_axes(meta(object)), collapse=",")), "\n")
            scales <- vapply(object@levels, \(x) sprintf("(%s)", paste0(dim(x), collapse=",")), character(1))
            coolcat("Scales (%d): %s", scales)
          })