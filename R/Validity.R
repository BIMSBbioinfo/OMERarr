setValidity("OMEZarrArray", function(object) {
  TRUE
})

setValidity("OMEZarrMeta", function(object) {
  meta_data <- object@listData
  # validate multiscales
  if (is.null(ms <- meta_data$multiscales)){
    stop("multiscales attribute is missing")
  } else {
    # validate datasets
    if (length(ds <- ms$datasets[[1]]) < 1) {
      stop("multiscales must contain at least one dataset")
    }
  }
  TRUE
})