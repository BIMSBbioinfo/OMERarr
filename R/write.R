
#' write_image
#' 
#' Writes an image to the zarr store according to ome-zarr specification
#'
#' @param image an n-dimensional (1<n<6) array representing the image data 
#' @param group the zarr group to write the image to
#' @param axes a character vector specifying the axes of the image 
#' (e.g. c("t", "c", "z", "y", "x"))
#' @param scalefactor Scaler implementation for downsampling the image argument. 
#' If None, no downsampling will be performed.
#' @param max_layer Maximum number of layers in the image pyramid
#' @param format OME-ZARR format (v0.4 or v0.5)
#' @param storage_options a list of storage options for the zarr array 
#' (e.g. chunks)
#'
#' @rdname write_image
#' 
#' @export
write_image <- function(image, 
                        group="/", 
                        axes = NULL,  
                        scalefactor = 2,
                        max_layer = 5,
                        format = "v0.4", # for now, we can only do zarr v2
                        storage_options = NULL){
  
  # Generate a downsampled pyramid of images.
  image_pyramid <- .create_mip(image, format, scalefactor, axes)
  
  # write image
  .write_multiscale_image(image_pyramid = image_pyramid, 
                          group = group, 
                          axes = axes, 
                          format = format, 
                          storage_options = storage_options)
}

#' .create_mip
#' 
#' Generate a downsampled pyramid of images.
#'
#' @inheritParams write_image
#' 
#' @noRd
.create_mip <- function(image,
                        format,
                        scalefactor = 2,
                        axes, 
                        max_layer = 5){
  
  
  # check dim
  ndim <- length(dim(image))
  if (ndim > 5) {
    stop("Only images of 5D or less are supported")
  }
  
  # check format
  # v0.1 and v0.2 are strictly 5D
  if (format %in% c("0.1", "0.2")) {
    shape_5d <- c(rep(1, 5 - ndim), dim(image))
    dim(image) <- shape_5d
  }
  
  # validate axes
  axes <- .get_valid_axes(ndim = length(dim(image)), 
                          axes = axes, 
                          format = format)
  
  # get x y dimensions for EBImage
  dim_image <- setNames(dim(image), axes)
  dim_image <- dim_image[c("x", "y")]
  
  # downscale image
  image_list <- list(image)
  if (max_layer > 1) {
    cur_image <- aperm(image, 
                       perm = rev(seq_len(length(axes))))
    for (i in 2:max_layer) {
      dim_image <- ceiling(dim_image / scalefactor)
      image_list[[i]] <- 
        aperm(EBImage::resize(cur_image,
                              w = dim_image[1],
                              h = dim_image[2]), 
              perm = rev(seq_len(length(axes))))
    }
  }
  image_list
}

#' .write_multiscale_image
#' 
#' Write a pyramid with multiscale metadata to disk.
#'
#' @inheritParams write_image
#' 
#' @noRd
.write_multiscale_image <- function(image_pyramid,
                                    group,
                                    axes,
                                    format,
                                    storage_options){
  
  # create zarr
  if(!zarr_path_exists(group, target_path = "/"))
    create_zarr(store = group, 
                version = if(format == "v0.4") "v2" else "v3")
  
  # write ome metadata 
  write_ome_metadata(group = group)
  
  # check storage options
  if(!"chunk_dim" %in% names(storage_options))
    stop("'chunk_dim' must be provided in storage_options")
  
  # write multiscale image
  for(i in seq_len(length(image_pyramid))){
    image <- image_pyramid[[i]]
    dataset_name <- paste0("0", i - 1)
    Rarr::write_zarr_array(
      x = image_pyramid[[i]],
      zarr_array_path = paste(group, paste0(i), sep = "/"),
      chunk_dim = .get_scale_chunk_dim(
        chunk_dim = storage_options$chunk_dim,
        dim = dim(image)
      ),
    )
  }
}

#' .get_valid_axes
#' 
#' Get validated axes
#'
#' @inheritParams write_image
#' 
#' @noRd
.get_valid_axes <- function(
    ndim = NULL,
    axes = NULL,
    format = "v0.4"
) {
  
  if (!is.null(format) && format %in% c("0.1", "0.2")) {
    if (!is.null(axes)) {
      message("axes ignored for version 0.1 or 0.2")
    }
    return(NULL)
  }
  
  # We can guess axes for 2D and 5D data
  if (is.null(axes)) {
    if (!is.null(ndim) && ndim == 2) {
      axes <- c("y", "x")
      message(sprintf("Auto using axes %s for 2D data", 
                      paste(axes, collapse = ", ")))
    } else if (!is.null(ndim) && ndim == 5) {
      axes <- c("t", "c", "z", "y", "x")
      message(sprintf("Auto using axes %s for 5D data", 
                      paste(axes, collapse = ", ")))
    } else {
      stop("axes must be provided. Can't be guessed for 3D or 4D data", 
           call. = FALSE)
    }
  }
  
  # axes may be string e.g. "tczyx"
  if (is.character(axes) && length(axes) == 1L) 
    axes <- strsplit(axes, "", fixed = TRUE)[[1]]
  
  if (!is.null(ndim) && length(axes) != ndim) {
    stop(
      sprintf("axes length (%d) must match number of dimensions (%d)", 
              length(axes), ndim),
      call. = FALSE
    )
  }
  
  axes
}

.get_scale_chunk_dim <- function(chunk_dim, dim){
  mapply(function(x, y) min(x, y), dim, chunk_dim)
}