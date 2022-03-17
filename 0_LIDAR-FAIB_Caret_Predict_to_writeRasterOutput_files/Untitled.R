
setwd('/Volumes/128GB_WORKD/EFI-TCC/0_Caret_Predict_to_writeRasterOutput')
getwd()

library(olsrr)
library(stars)
library(mlr)
library(MASS)
library(DMwR2)
library(caret)
library(caretEnsemble)
library(e1071)
library(psych)
library(terra)
library(raster)
library(sf)
library(sp)
sf_extSoftVersion()[1:3]
library(gdalUtils)
library(dplyr)
library(tidyverse)
library(car)
library(np)
library(readr)
library(DescTools)
library(ggplot2)
library(RColorBrewer)
library(psych)
library(useful)
library(caret)
library(npreg)
library(ModelMetrics)
library(Metrics)
library(animation)
library(olsrr)
library(lmtest)
library(car)
library(lme4)
library(MASS)
library(rgl)
library(rmarkdown)
library(nortest)
library(latex2exp)
library(pca3d)
library(ISLR)
library(pls)
library(corrplot)
library(glmnet)
library(mvtnorm)
library(biglm)
library(leaps)
library(viridis)
library(ffbase)
library(ks)
library(KernSmooth)
library(nor1mix)
library(locfit)
library(sf)
library(sp)
library(spatial)
library(tibble)
library(stringr)
library(ForestTools)
library(conflicted)
set.seed(23)

aoi_sf <- read_sf("./Data/BCTS_OPERATING_AREAS_SP/BCTS_OP_AR_polygon.shp")
aoi_sf = rename(aoi_sf, AOI_Boundary = SHAPE)
aoi_sf = aoi_sf[1, "AOI_Boundary"]
plot(aoi_sf)

lead_htop = terra::rast("./Data/Raster_Covariates/lead_htop_raster.tif")
lead_htop = terra::mask(lead_htop, vect(aoi_sf))
lead_htop[lead_htop < 1.3] <- NA
elev = terra::rast("./Data/Raster_Covariates/elev_raster.tif")
elev = terra::mask(elev, vect(aoi_sf))
elev = terra::mask(elev, lead_htop, inverse=FALSE)

terra::aggregate(elev, fact = 20, fun = mean)
slope = terra::terrain(elev, v="slope", unit="degrees", neighbors=8)
aspect = terra::terrain(elev, v="aspect", unit="degrees", neighbors=8)
asp_cos = cos((aspect*pi)/180)
asp_sin = sin((aspect*pi)/180)

plot(lead_htop, main = "canopy height (m)")
plot(elev, main = "elevation (m asL)")
plot(slope, main = "slope")
plot(asp_cos, main = "asp_cos")
plot(asp_sin, main = "asp_sin")

crs(lead_htop) = "epsg:3153"
crs(elev) = "epsg:3153"
crs(slope) = "epsg:3153"
crs(asp_cos) = "epsg:3153"
crs(asp_sin) = "epsg:3153"

writeRaster(stemsha_L_raster, filename = "./Data/Raster_Covariates/stemsha_L_raster.tif", overwrite=TRUE)



stems = raster("./Data/Raster_Covariates/stemsha_L_raster.tif")
stems = terra::resample(stems, elev, method="bilinear")
stems = terra::mask(stems, vect(aoi_sf))
stems = terra::mask(stems, elev, inverse=TRUE)


lead_htop = aggregate(lead_htop, fact = 20, fun = mean) 



crs(stemsha_L_rast) = "epsg:3153"
stemsha_L_rast = terra::mask(stemsha_L_rast, vect(aoi_sf))
terra::crop(stemsha_L_rast, lead_htop)
terra::crop(lead_htop, stemsha_L_rast)
stemsha_L = terra::mask(stemsha_L_rast, lead_htop, inverse=FALSE)
plot(stemsha_L_rast)

par(mfrow = c(4,4))

wilcox.test(faib_vri_true_m1_df$elev) # p<0.0001
wilcox.test(faib_vri_true_m1_df$slope) # p<0.0001
wilcox.test(faib_vri_true_m1_df$asp_cos) # p=0.8749
wilcox.test(faib_vri_true_m1_df$asp_sin) # p<0.0001
wilcox.test(faib_vri_true_m1_df$lead_htop) # p<0.0001
wilcox.test(faib_vri_true_m1_df$stemsha_L) # p<0.0001
wilcox.test(faib_vri_true_m1_df$wsvha_L) # p<0.0001

truehist(faib_vri_true_m1_df$elev, main="DEM (faib)", maxpixels=22000000)
hist(elev, main="DEM (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$slope, main="Slope (faib)", maxpixels=22000000)
hist(slope, main="Slope (raster)", maxpixels=22000000) 
truehist(faib_vri_true_m1_df$asp_cos, main="Northness (faib)", maxpixels=22000000)
hist(asp_cos, main="Northness (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$asp_sin, main="Eastness (faib)", maxpixels=22000000)
hist(asp_sin, main="Eastness (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$stemsha_L, main="Stems/ha (faib)", maxpixels=22000000)
hist(stems, main="Stems/ha (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$species_class, main="Lead Species (faib)", maxpixels=22000000)
hist(species, main="Lead Species (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$lead_htop, main="Mean Tree Height (faib)", maxpixels=22000000)
hist(lead_htop, main="Mean Tree Height (raster)", maxpixels=22000000) 
truehist(faib_vri_true_m1_df$wsvha_L, main="Whole Stem Vol (faib)", maxpixels=22000000)



model1_svmRadial_eps = raster::raster('./Results/Rasters/tunedModel_svm_m1_eps_to_raster.tif')

library(tmap)
tm_shape(model1_svmRadial_eps)+
  tm_raster(style= "cat", title="Whole Stem Volume (m^3/ha)")+
  tm_layout(legend.outside = TRUE)+
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 2.5, 5, 10), text.size = 0.5)+
  tm_grid()

plot(model1_svmRadial_eps, 
  main= "Estimated Whole Stem Volume Gaspard OA (m3/ha)\nModel 1: Support Vector Machine (Radial Kernel & Epsilon)\n10k-fold Cross-Validated", 
  cex.main = 0.75)
title(main ="MAE:9.424\nRMSE:10.830\nRMSEratio:0.776\ngamma=0.5\nepsilon=0.10\nC=20", adj = 0.05, line = -5, cex.main = 0.75)
addscalebar(plotepsg=3153)
addnortharrow(pos = "topright", scale=0.75)


model1_svmRadial_eps = raster::raster('./Results/Rasters/tunedModel_svm_m1_eps_to_raster.tif')



