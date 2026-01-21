#' @importFrom methods new

.OMEZarrMeta <- setClass(
  Class = "OMEZarrMeta",
  slots = c(
    listData = "list",
    version = "character"
  )
)

.OMEZarrArray <- setClass(
  Class = "OMEZarrArray",
  slots = c(
    meta = "OMEZarrMeta",
    levels = "list"
  )
)
