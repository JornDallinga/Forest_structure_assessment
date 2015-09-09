Mosaic_Raster <- function(x_list, dir, extract, buffer, pr_filename){
  rast.list <- list()
  for(i in 1:length(x_list)) {
    list_raster <- sprintf("%s%s%s", dir, extract, x_list[i])
    rast.list[[i]] <- raster(list_raster) 
  }
  
  
  if (length(rast.list) > 1){
    t <- 1
    new_list <- list()
    for (i in rast.list){
      
      Raster <- rast.list[[t]]
      plot_Raster <- spTransform(buffer, CRS(proj4string(Raster))) 
      plot_crop <- crop(Raster, plot_Raster)
      reproject <- projectRaster(from = plot_crop, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method = 'ngb')
      new_list[t] <- reproject
      t <- t + 1
    }

    # Bug fix in data sets, when resolutions dont match.
    res_frame <- sapply(new_list, res)
    find_uniq <- unique(res_frame)
    if (ncol(find_uniq) > 1){
      new_list <- lapply(new_list, resample, y = new_list[[1]], method = "ngb")
      
      #ind <- table(apply(res_frame, 1, paste, collapse = "/")) 
      #ind <- which.max(ind) 
      #mode_res <- as.numeric(strsplit(names(ind), "/")[[1]])

    } else {
      
    }
    
    new_list$fun <- max
    new_list$tolerance <- 0.5
    mosaic_dataset <- do.call(mosaic, new_list)
    mask_plot <- mask(mosaic_dataset, buffer)
    
  } else {
    Raster <- raster(sprintf("%s%s%s", dir, extract, pr_filename))
    plot_Raster <- spTransform(buffer, CRS(proj4string(Raster)))
    plot_crop <- crop(Raster, plot_Raster)
    reproject <- projectRaster(from = plot_crop, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method = 'ngb')
    mask_plot <- mask(reproject, buffer)
  }
    return (mask_plot)
  
}

