Callibrate_ras <- function(mask_gfc, Masked_Raster){
  
  ## using resample to create similar dimensions
  new_mask_gfc <- resample(mask_gfc, Masked_Raster, method = "ngb")
  
  ## stacking testa and useing layerStats
  stack_ras <- stack(new_mask_gfc, Masked_Raster)
  layerStats(stack_ras, 'pearson', na.rm = T)
  
  ## Linear model
  test <- lm(getValues(new_mask_gfc) ~ getValues(Masked_Raster), na.action=na.exclude)
  test <- lm(getValues(Hansen_mask) ~ getValues(Sexton_mask), na.action=na.exclude)
  
  test2 <- lm(getValues(new_mask_gfc) ~ getValues(Masked_Raster) + Time2, na.action=na.exclude)
  test2 <- lm(getValues(Hansen_mask) ~ getValues(Sexton_mask) + Time2, na.action=na.exclude)
  
  test3 <- lm(getValues(new_mask_gfc) ~ getValues(Masked_Raster) + Time2 + Time3, na.action=na.exclude)
  Time2 <- getValues(Hansen_mask)^2
  Time3 <- getValues(new_mask_gfc)^3

  plot(test)
  abline(test)
  summary(test)
  
  ## Get extent
  extent_test <- drawExtent(show=TRUE, col="red")
  plot(extent_test)
  
  ## crop the masks to new extent
  Hansen_mask <- crop(new_mask_gfc, extent_test)
  Sexton_mask <- crop(Masked_Raster, extent_test)
  
  ## aggregation of data sets
  Hansen_aggregated <- aggregate(Hansen_mask, fact = 2, fun = mean)
  Sexton_aggregated <- aggregate(Sexton_mask, fact = 2, fun = mean)
  
  ## testing aggregation
  test <- lm(getValues(Sexton_aggregated) ~ getValues(Hansen_aggregated), na.action=na.exclude)
  plot(getValues(Sexton_aggregated) ~ getValues(Hansen_aggregated))
  poly_test1 <- lm(getValues(Sexton_aggregated) ~ poly(getValues(Hansen_aggregated), degree = 3, raw = T), na.action=na.exclude)
  lines(lowess(getValues(Sexton_aggregated) ~ getValues(Hansen_aggregated)), col = 2)
  
  aggr_brick <- brick(Sexton_aggregated, Hansen_aggregated)
  pairs(aggr_brick)

  ## plot and line a polynomial or quadtritic regression
  plot(getValues(new_mask_gfc) ~ getValues(Masked_Raster))
  lines(lowess(getValues(Hansen_mask) ~ getValues(Sexton_mask)), col = 2)
  
  ## plot new line
  
  lines(getValues(Sexton_mask), predict(poly_test1))
  
  ## Polynominal formula
  
  y = β0 + β1 * x^2 + β2 * x^2 + .. + βnxn
  
  
  ## polynomial test
  
  poly_test <- lm(getValues(Hansen_mask) ~ poly(getValues(Sexton_mask), degree = 2, raw = T), na.action=na.exclude)
  poly_test1 <- lm(getValues(Hansen_mask) ~ poly(getValues(Sexton_mask), degree = 3, raw = T), na.action=na.exclude)
  poly_test1 <- lm(getValues(Sexton_mask) ~ poly(getValues(Hansen_mask), degree = 3, raw = T), na.action=na.exclude)

  summary(poly_test1)
  plot(poly_test1)
  
  
  new_ras1 <- predict(new_mask_gfc, model = test3)
  new_ras1 <- calc(Hansen_mask, model = poly_test1)
  
  ## regression formula
  regression = intercept + slope * x
  fun <- 15.9868 + 1.19487 * getValues(Masked_Raster)
  
  ## calculate new raster based on regression
  new_ras <- calc(x = new_mask_gfc, fun=function(x){15.9868 + 1.19487 * x})
  
  ## setting threshold for hansen
  new_ras[new_ras < Threshold] <- 0
  new_ras[new_ras >= Threshold] <- 1
  new_ras[new_ras > 100] <- NA
  
  ## histograms
  hist(Masked_Raster)
  hist(new_mask_gfc)
  brick_ras <- brick(new_mask_gfc, Masked_Raster)
  pairs(brick_ras)

  
  test_plot <- plot(values(new_mask_gfc), values(Masked_Raster))

  return(cor_ras)
}