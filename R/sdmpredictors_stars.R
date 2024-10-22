#' List the available predictors in the sdmpredictors_datadir
#' 
#' @param path chr, the sdmpredictors_datadir path (hopefully set as an option)
#' @return a charcater vector of layercodes
list_sdmpredictors = function(path = options("sdmpredictors_datadir")){
  if (is.null(datadir)){
    stop("please set 'sdmpredictors_datadir' option first")
  }

  ff = list.files(path[[1]], full.names = FALSE) |>
    strsplit(".", fixed = TRUE)
  sapply(ff, "[", 1)
}

#' Read a list of rasters as stars
#' 
#' @param layercodes chr one or more layer codes
#' @param datadir chr path to the data directory
#' @param bb NULL or some object from which an st_bbox can be derived.  Thge output is 
#'   cropped to this extent. Ignored if NULL.
#' @param bind logical, if TRUE bind the attributes (variables) if possible
read_sdmpredictors = function(layercodes = list_sdmpredictors()[1:3], 
                           datadir = options("sdmpredictors_datadir")[[1]],
                           bb = NULL,
                           bind = TRUE){
  
  if (is.null(datadir)){
    stop("please set 'sdmpredictors_datadir' option first")
  }
  
  ff = list.files(datadir, full.names = TRUE)
  
  xx = lapply(seq_along(layercodes),
    function(i){
      layercode = layercodes[i]
      ix = grep(layercode, ff, fixed = TRUE)
      if (length(ix) > 0){
        f = ff[[i]]
        if (grepl(".zip", f, fixed = TRUE)){
          f = paste0("/vsizip/", f)
        }
        x = stars::read_stars(f) |>
          rlang::set_names(layercode)
        if (!is.null(bb)) x <- stars::st_crop(x, bb)
        x
      }
    })
  
  if (bind){
    xx = try(do.call(c, append(xx, list(along = NA_integer_))))
  }
  
  xx
}