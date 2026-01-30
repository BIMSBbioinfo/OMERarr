library(reticulate)
py_require("napari[all]")
py_require("imagecodecs")
# py_require("napari-ome-zarr")
nap = import("napari")
v = nap$Viewer()
# file_name <- "https://uk1s3.embassy.ebi.ac.uk/idr/zarr/v0.4/idr0073A/9798462.zarr"
# v$open(file_name, plugin="napari-ome-zarr")
file_name <- "/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/ImageArray/extdata/xy_12bit__plant.ome.tiff"
v$open(file_name)
nap$run()