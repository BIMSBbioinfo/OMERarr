#' @importFrom S4Vectors coolcat
setMethod("show", 
          "OMEZarrArray", 
          function(object) {
            n.object <- length(object@levels)
            cat("class:", class(object), "\n")
            axes <- .get_axes(meta(object))
            coolcat("Axes (%d): %s", axes)
            scales <- vapply(object@levels, \(x) sprintf("(%s)", paste0(dim(x), collapse=",")), character(1))
            coolcat("Scales (%d): %s", scales)
          })