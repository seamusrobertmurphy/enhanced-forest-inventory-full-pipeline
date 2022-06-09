
library(sf)
library(sp)
library(terra)
library(raster)
library(dplyr)
library(caret)
library(caretEnsemble)
library(ForestTools)
library(lidR)
library(randomForest)
library(e1071)
library(rgdal)
library(rgeos)
library(MASS)
library(car)
library(gdalUtils)
library(glmnet)
library(coefplot)
library(ipred)
library(rpart)
library(doParallel)
library(foreach)
set.seed(123)

lead_htop_raster_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/LiDAR_raw/quesnel_region/lead_htop_raster_1m_quesnel.tif")
lead_htop_raster_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/LiDAR_raw/gaspard_region/lead_htop_raster_1m_gaspard.tif")
elev_raster_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/LiDAR_raw/quesnel_region/elev_raster_1m_quesnel.tif")
elev_raster_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/LiDAR_raw/gaspard_region/elev_raster_1m_gaspard.tif")
lead_htop_rast_quesnel = terra::rast(lead_htop_raster_quesnel)
lead_htop_rast_gaspard = terra::rast(lead_htop_raster_gaspard)
elev_rast_quesnel = terra::rast(elev_raster_quesnel)
elev_rast_gaspard = terra::rast(elev_raster_gaspard)
terra::crs(lead_htop_rast_gaspard) = "epsg:3005"
terra::crs(lead_htop_rast_quesnel) = "epsg:3005"
terra::crs(elev_rast_gaspard) = "epsg:3005"
terra::crs(elev_rast_quesnel) = "epsg:3005"
terra::plot(lead_htop_rast_gaspard, main = 'CHM Raw (1m res)')
terra::plot(lead_htop_rast_quesnel, main = 'CHM Raw (1m res)')
terra::plot(elev_rast_gaspard, main = 'Elevation Raw (1m res)')
terra::plot(elev_rast_quesnel, main = 'Elevation Raw (1m res)')

elev_rast_quesnel = terra::aggregate(elev_rast_quesnel, fact = 100, fun = mean)
elev_rast_gaspard = terra::aggregate(elev_rast_gaspard, fact = 100, fun = mean)
slope_rast_quesnel = terra::terrain(elev_rast_quesnel, v="slope", unit="degrees", neighbors=8)
slope_rast_gaspard = terra::terrain(elev_rast_gaspard, v="slope", unit="degrees", neighbors=8)
aspect_rast_quesnel = terra::terrain(elev_rast_quesnel, v="aspect", unit="degrees", neighbors=8)
aspect_rast_gaspard = terra::terrain(elev_rast_gaspard, v="aspect", unit="degrees", neighbors=8)
asp_cos_rast_quesnel = cos((aspect_rast_quesnel*pi)/180)
asp_cos_rast_gaspard = cos((aspect_rast_gaspard*pi)/180)
asp_sin_rast_quesnel = sin((aspect_rast_quesnel*pi)/180)
asp_sin_rast_gaspard = sin((aspect_rast_gaspard*pi)/180)
lead_htop_rast_quesnel = terra::aggregate(lead_htop_rast_quesnel, fact = 100, fun = mean)
lead_htop_rast_gaspard = terra::aggregate(lead_htop_rast_gaspard, fact = 100, fun = mean)

lead_htop_rast_quesnel = terra::resample(lead_htop_rast_quesnel, elev_rast_quesnel)
lead_htop_rast_gaspard = terra::resample(lead_htop_rast_gaspard, elev_rast_gaspard)
species_class_rast_quesnel = terra::resample(species_class_rast_quesnel, elev_rast_quesnel)
species_class_rast_gaspard = terra::resample(species_class_rast_gaspard, elev_rast_gaspard)
elev_rast_quesnel = terra::mask(elev_rast_quesnel, lead_htop_rast_quesnel)
elev_rast_gaspard = terra::mask(elev_rast_gaspard, lead_htop_rast_gaspard)
slope_rast_quesnel = terra::mask(slope_rast_quesnel, lead_htop_rast_quesnel)
slope_rast_gaspard = terra::mask(slope_rast_gaspard, lead_htop_rast_gaspard)
asp_cos_rast_quesnel = terra::mask(asp_cos_rast_quesnel, lead_htop_rast_quesnel)
asp_cos_rast_gaspard = terra::mask(asp_cos_rast_gaspard, lead_htop_rast_gaspard)
asp_sin_rast_quesnel = terra::mask(asp_sin_rast_quesnel, lead_htop_rast_quesnel)
asp_sin_rast_gaspard = terra::mask(asp_sin_rast_gaspard, lead_htop_rast_gaspard)
species_class_rast_quesnel = terra::mask(species_class_rast_quesnel, lead_htop_rast_quesnel)
species_class_rast_gaspard = terra::mask(species_class_rast_gaspard, lead_htop_rast_gaspard)

writeRaster(elev_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/elev_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(elev_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/elev_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(slope_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/slope_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(slope_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/slope_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(asp_cos_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/asp_cos_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(asp_cos_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/asp_cos_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(asp_sin_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/asp_sin_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(asp_sin_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/asp_sin_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(lead_htop_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/lead_htop_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(lead_htop_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/lead_htop_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(species_class_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/species_class_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(species_class_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/species_class_raster_100m_gaspard.tif", overwrite=TRUE)

#...........................................

elev_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/elev_raster_100m_quesnel.tif")
elev_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/elev_raster_100m_gaspard.tif")
slope_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/slope_raster_100m_quesnel.tif")
slope_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/slope_raster_100m_gaspard.tif")
asp_cos_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/asp_cos_raster_100m_quesnel.tif")
asp_cos_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/asp_cos_raster_100m_gaspard.tif")
asp_sin_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/asp_sin_raster_100m_quesnel.tif")
asp_sin_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/asp_sin_raster_100m_gaspard.tif")
species_class_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/species_class_raster_100m_quesnel.tif")
species_class_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/species_class_raster_100m_gaspard.tif")
lead_htop_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/lead_htop_raster_100m_quesnel.tif")
lead_htop_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/unmasked-covariates/dem-based/lead_htop_raster_100m_gaspard.tif")
masks_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/mask/mask_raster_100m_quesnel.tif")
masks_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/mask/mask_raster_100m_gaspard.tif")

# masking by mask
masks_rast_quesnel = terra::resample(masks_rast_quesnel, lead_htop_rast_quesnel)
masks_rast_gaspard = terra::resample(masks_rast_gaspard, lead_htop_rast_gaspard)
masks_rast_quesnel = terra::resample(masks_rast_quesnel, elev_rast_quesnel)
masks_rast_gaspard = terra::resample(masks_rast_gaspard, elev_rast_gaspard)
lead_htop_rast_quesnel = mask(lead_htop_rast_quesnel, masks_rast_quesnel, inverse=TRUE)
lead_htop_rast_gaspard = mask(lead_htop_rast_gaspard, masks_rast_gaspard, inverse=TRUE)
elev_rast_quesnel = mask(elev_rast_quesnel, masks_rast_quesnel, inverse=TRUE)
elev_rast_gaspard = mask(elev_rast_gaspard, masks_rast_gaspard, inverse=TRUE)
slope_rast_quesnel = mask(slope_rast_quesnel, masks_rast_quesnel, inverse=TRUE)
slope_rast_gaspard = mask(slope_rast_gaspard, masks_rast_gaspard, inverse=TRUE)
asp_cos_rast_quesnel = mask(asp_cos_rast_quesnel, masks_rast_quesnel, inverse=TRUE)
asp_cos_rast_gaspard = mask(asp_cos_rast_gaspard, masks_rast_gaspard, inverse=TRUE)
asp_sin_rast_quesnel = mask(asp_sin_rast_quesnel, masks_rast_quesnel, inverse=TRUE)
asp_sin_rast_gaspard = mask(asp_sin_rast_gaspard, masks_rast_gaspard, inverse=TRUE)
species_class_rast_quesnel = mask(species_class_rast_quesnel, masks_rast_quesnel, inverse=TRUE)
species_class_rast_gaspard = mask(species_class_rast_gaspard, masks_rast_gaspard, inverse=TRUE)

# masking by species
lead_htop_rast_quesnel = mask(lead_htop_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
lead_htop_rast_gaspard = mask(lead_htop_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
elev_rast_quesnel = mask(elev_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
elev_rast_gaspard = mask(elev_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
slope_rast_quesnel = mask(slope_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
slope_rast_gaspard = mask(slope_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
asp_cos_rast_quesnel = mask(asp_cos_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
asp_cos_rast_gaspard = mask(asp_cos_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
asp_sin_rast_quesnel = mask(asp_sin_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
asp_sin_rast_gaspard = mask(asp_sin_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)

writeRaster(elev_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/elev_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(elev_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/elev_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(slope_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/slope_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(slope_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/slope_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(asp_cos_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_cos_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(asp_cos_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_cos_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(asp_sin_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_sin_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(asp_sin_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_sin_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(species_class_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/species_class_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(species_class_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/species_class_raster_100m_gaspard.tif", overwrite=TRUE)
writeRaster(lead_htop_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/lead_htop_raster_100m_quesnel.tif", overwrite=TRUE)
writeRaster(lead_htop_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/lead_htop_raster_100m_gaspard.tif", overwrite=TRUE)


#...........................................


elev_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/elev_raster_100m_quesnel.tif")
elev_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/elev_raster_100m_gaspard.tif")
slope_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/slope_raster_100m_quesnel.tif")
slope_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/slope_raster_100m_gaspard.tif")
asp_cos_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_cos_raster_100m_quesnel.tif")
asp_cos_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_cos_raster_100m_gaspard.tif")
asp_sin_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_sin_raster_100m_quesnel.tif")
asp_sin_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_sin_raster_100m_gaspard.tif")
species_class_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/species_class_raster_100m_quesnel.tif")
species_class_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/species_class_raster_100m_gaspard.tif")
lead_htop_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/lead_htop_raster_100m_quesnel.tif")
lead_htop_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/lead_htop_raster_100m_gaspard.tif")

names(elev_rast_quesnel) = "elev"
names(slope_rast_quesnel) = "slope"
names(asp_cos_rast_quesnel) = "asp_cos"
names(asp_sin_rast_quesnel) = "asp_sin"
names(species_class_rast_quesnel) = "species_class"
names(lead_htop_rast_quesnel) = "lead_htop"

names(elev_rast_gaspard) = "elev"
names(slope_rast_gaspard) = "slope"
names(asp_cos_rast_gaspard) = "asp_cos"
names(asp_sin_rast_gaspard) = "asp_sin"
names(species_class_rast_gaspard) = "species_class"
names(lead_htop_rast_gaspard) = "lead_htop"

elev_raster_quesnel = raster::raster(elev_rast_quesnel)
slope_raster_quesnel = raster::raster(slope_rast_quesnel)
asp_cos_raster_quesnel = raster::raster(asp_cos_rast_quesnel)
asp_sin_raster_quesnel = raster::raster(asp_sin_rast_quesnel)
species_class_raster_quesnel = raster::raster(species_class_rast_quesnel)
lead_htop_raster_quesnel = raster::raster(lead_htop_rast_quesnel)

elev_raster_gaspard = raster::raster(elev_rast_gaspard)
slope_raster_gaspard = raster::raster(slope_rast_gaspard)
asp_cos_raster_gaspard = raster::raster(asp_cos_rast_gaspard)
asp_sin_raster_gaspard = raster::raster(asp_sin_rast_gaspard)
species_class_raster_gaspard = raster::raster(species_class_rast_gaspard)
lead_htop_raster_gaspard = raster::raster(lead_htop_rast_gaspard)

elev_raster_list = list(elev_raster_quesnel, elev_raster_gaspard)
slope_raster_list = list(slope_raster_quesnel, slope_raster_gaspard)
asp_cos_raster_list = list(asp_cos_raster_quesnel, asp_cos_raster_gaspard)
asp_sin_raster_list = list(asp_sin_raster_quesnel, asp_sin_raster_gaspard)
species_class_raster_list = list(species_class_raster_quesnel, species_class_raster_gaspard)
lead_htop_raster_list = list(lead_htop_raster_quesnel, lead_htop_raster_gaspard)

elev_raster = do.call(merge, c(elev_raster_list, tolerance = 1))
slope_raster = do.call(merge, c(slope_raster_list, tolerance = 1))
asp_cos_raster = do.call(merge, c(asp_cos_raster_list, tolerance = 1))
asp_sin_raster = do.call(merge, c(asp_sin_raster_list, tolerance = 1))
species_class_raster = do.call(merge, c(species_class_raster_list, tolerance = 1))
lead_htop_raster = do.call(merge, c(lead_htop_raster_list, tolerance = 1))

names(elev_raster) = "elev"
names(slope_raster) = "slope"
names(asp_cos_raster) = "asp_cos"
names(asp_sin_raster) = "asp_sin"
names(species_class_raster) = "species_class"
names(lead_htop_raster) = "lead_htop"

writeRaster(elev_raster, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/elev_raster_100m_allSites.tif", overwrite=TRUE)
writeRaster(slope_raster, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/slope_raster_100m_allSites.tif", overwrite=TRUE)
writeRaster(asp_cos_raster, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_cos_raster_100m_allSites.tif", overwrite=TRUE)
writeRaster(asp_sin_raster, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/asp_sin_raster_100m_allSites.tif", overwrite=TRUE)
writeRaster(species_class_raster, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/species_class_raster_100m_allSites.tif", overwrite=TRUE)
writeRaster(lead_htop_raster, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/dem-based/lead_htop_raster_100m_allSites.tif", overwrite=TRUE)

covs_m1_quesnel = raster::stack(
  elev_raster_quesnel, 
  slope_raster_quesnel,
  asp_cos_raster_quesnel,
  asp_sin_raster_quesnel,
  species_class_raster_quesnel,
  lead_htop_raster_quesnel)

covs_m1_gaspard = raster::stack(
  elev_raster_gaspard, 
  slope_raster_gaspard,
  asp_cos_raster_gaspard,
  asp_sin_raster_gaspard,
  species_class_raster_gaspard,
  lead_htop_raster_gaspard)

covs_m1 = raster::stack(
  elev_raster, 
  slope_raster,
  asp_cos_raster,
  asp_sin_raster,
  lead_htop_raster,
  species_class_raster)

rasterVis::levelplot(covs_m1_gaspard)
rasterVis::levelplot(covs_m1_quesnel)


#...........................................





faib_psp <- read.csv("/media/seamus/128GB_WORKD/EFI-TCC/0_Caret_Predict_to_writeRasterOutput/Data/FAIB_PSP_20211028.csv")
faib_psp = subset(faib_psp, util == '12.5')
faib_psp$spc_live1 = as.factor(faib_psp$spc_live1)
faib_psp =  subset(faib_psp, 
                   spc_live1=='PL' | spc_live1=='PLI' | spc_live1=='FD'| spc_live1=='FDI' | 
                     spc_live1=='SB' | spc_live1=='SE' | spc_live1=='SW' | spc_live1=='SX' | 
                     spc_live1=='CW' | spc_live1=='HW' | spc_live1=='BL' | spc_live1=='LW')
faib_psp$species_class = dplyr::recode(faib_psp$spc_live1, 
                                       PL = 1, PLI = 1, SB = 2, SE = 2, SX = 2, 
                                       FD = 3, FDI = 3, CW = 3, HW = 4, BL = 5, LW = 6)
faib_psp$asp_cos = cos((faib_psp$aspect * pi) / 180)
faib_psp$asp_sin = sin((faib_psp$aspect * pi) / 180)

faib_psp$elev[faib_psp$elev <= 0] = NA
faib_psp$slope[faib_psp$slope <= 0] = NA
faib_psp$lead_htop[faib_psp$lead_htop < 2] = NA
faib_psp$stemsha_L[faib_psp$stemsha_L <= 0] = NA
faib_psp$wsvha_L[faib_psp$wsvha_L <= 0] = NA
#faib_psp = subset(faib_psp, stemsha_L < 864)

faib_psp$elev = as.numeric(faib_psp$elev)
faib_psp$slope = as.numeric(faib_psp$slope)
faib_psp$asp_cos = as.numeric(faib_psp$asp_cos)
faib_psp$asp_sin = as.numeric(faib_psp$asp_sin)
faib_psp$lead_htop = as.numeric(faib_psp$lead_htop)
faib_psp$species_class = as.numeric(faib_psp$species_class)
faib_psp$stemsha_L = as.numeric(faib_psp$stemsha_L)
faib_psp$wsvha_L = as.numeric(faib_psp$wsvha_L)
faib_vri_true_m1_df = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "wsvha_L", "stemsha_L")] 

stemsha_L_raster = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/inputs/masked-covariates/chm-based/stemsha_L_raster_100m_allSites.tif")
stemsha_L_raster_df = as.data.frame(rasterToPoints(stemsha_L_raster))
dens.fun = approxfun(density(stemsha_L_raster_df$stemsha_L, adjust=0.8))
B = 1000
n = 4
faib_vri_true_m1_df_boot = dplyr::sample_n(faib_vri_true_m1_df, B * n, weight_by = dist.fun(faib_vri_true_m1_df$stemsha_L), replace = TRUE)
#faib_psp = dplyr::sample_n(faib_psp, 600, weight_by = dist.fun, replace = TRUE)
truehist(faib_vri_true_m1_df$stemsha_L, main="Stems/ha (un-Bootstrapped FAIB)")
truehist(faib_vri_true_m1_df_boot$stemsha_L, main="Stems/ha (Bootstrapped FAIB)")

faib_vri_true_m1_df_boot_stemfree = faib_vri_true_m1_df_boot[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "wsvha_L")] 
faib_vri_true_m1_df_boot_stemfree = na.omit(faib_vri_true_m1_df_boot_stemfree)
faib_vri_true_m1_df_boot_stemfree_split = createDataPartition(faib_vri_true_m1_df_boot_stemfree$wsvha_L, p=0.80, list=F)
train_m1_boot = faib_vri_true_m1_df_boot_stemfree[faib_vri_true_m1_df_boot_stemfree_split, ]
test_m1_boot = faib_vri_true_m1_df_boot_stemfree[-faib_vri_true_m1_df_boot_stemfree_split, ]
X_train_m1_boot = train_m1_boot[,-7]
y_train_m1_boot = train_m1_boot[, 7]
X_test_m1_boot = test_m1_boot[,-7]
y_test_m1_boot = test_m1_boot[, 7]
X_m1_boot = faib_vri_true_m1_df_boot_stemfree[,-7]
y_m1_boot = faib_vri_true_m1_df_boot_stemfree[, 7]


faib_vri_true_m1_df = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "wsvha_L")] 
faib_vri_true_m1_df = na.omit(faib_vri_true_m1_df)
faib_vri_true_m1_df_split = createDataPartition(faib_vri_true_m1_df$wsvha_L, p=0.80, list=F)
train_m1 = faib_vri_true_m1_df[faib_vri_true_m1_df_split, ]
test_m1 = faib_vri_true_m1_df[-faib_vri_true_m1_df_split, ]
X_train_m1 = train_m1[,-7]
y_train_m1 = train_m1[, 7]
X_test_m1 = test_m1[,-7]
y_test_m1 = test_m1[, 7]
X_m1 = faib_vri_true_m1_df[,-7]
y_m1 = faib_vri_true_m1_df[, 7]


fitControl_YeoJx1 = caret::trainControl(method="cv", number=10)
fitControl_YeoJx3 = caret::trainControl(method="repeatedcv", number=10, repeats=3)
fitControl_YeoJx5 = caret::trainControl(method="repeatedcv", number=10, repeats=5)
fitControl_YeoJx10 = caret::trainControl(method="repeatedcv", number=10, repeats=10)


## Model: Random Forest e1071-trained
tuneResult_rf_m1_full <- tune.randomForest(
  X_m1_boot, y_m1_boot,
  mtry = c(2:10), ntree = 50,
  tunecontrol = tune.control(sampling = "cross", cross = 10),
  preProcess = c('YeoJohnson', 'scale', 'center', 'corr'))

tuneResult_rf_m1_train <- tune.randomForest(
  X_train_m1_boot, y_train_m1_boot, 
  mtry = c(2:10), ntree = 50,
  tunecontrol = tune.control(sampling = "cross", cross = 10), 
  preProcess = c('YeoJohnson', 'scale', 'center', 'corr'))

tunedModel_rf_m1_full <- tuneResult_rf_m1_full$best.model
tunedModel_rf_m1_train <- tuneResult_rf_m1_train$best.model
tunedModel_rf_m1 = predict(tunedModel_rf_m1_full, newdata=faib_vri_true_m1_df, type = "response")
tunedModel_rf_m1_test = predict(tunedModel_rf_m1_train, newdata=test_m1, type = "response")
save(tunedModel_rf_m1_full, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_randomForest_bootstrapped_demBased_e1071.RData")

R2(tunedModel_rf_m1, faib_vri_true_m1_df$wsvha_L)
MAE(tunedModel_rf_m1, faib_vri_true_m1_df$wsvha_L)
RMSE(tunedModel_rf_m1, faib_vri_true_m1_df$wsvha_L)
MAE(tunedModel_rf_m1_test, test_m1$wsvha_L)
RMSE(tunedModel_rf_m1_test, test_m1$wsvha_L)

wsvha_model1_randomForest_bootstrapped_demBased_100m_allAreas <- raster::predict(covs_m1, tunedModel_rf_m1_full)
wsvha_model1_randomForest_bootstrapped_demBased_100m_gaspard <- raster::predict(covs_m1_gaspard, tunedModel_rf_m1_full)
wsvha_model1_randomForest_bootstrapped_demBased_100m_quesnel <- raster::predict(covs_m1_quesnel, tunedModel_rf_m1_full)
writeRaster(wsvha_model1_randomForest_bootstrapped_demBased_100m_allAreas, overwrite=TRUE,
  filename = "/media/seamus/128GB_WORKD/data/raster/tcc/outputs/wsvha/wsvha_model1_randomForest_bootstrapped_demBased_100m_allAreas.tif")
writeRaster(wsvha_model1_randomForest_bootstrapped_demBased_100m_gaspard, overwrite=TRUE,
  filename = "/media/seamus/128GB_WORKD/data/raster/tcc/outputs/wsvha/bootstrapped/wsvha_model1_randomForest_bootstrapped_demBased_100m_gaspard.tif")
writeRaster(wsvha_model1_randomForest_bootstrapped_demBased_100m_quesnel,  overwrite=TRUE,
  filename = "/media/seamus/128GB_WORKD/data/raster/tcc/outputs/wsvha/bootstrapped/wsvha_model1_randomForest_bootstrapped_demBased_100m_quesnel.tif")
wsvha_model1_randomForest_bootstrapped_demBased_100m_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/outputs/wsvha/bootstrapped/wsvha_model1_randomForest_bootstrapped_demBased_100m_gaspard.tif")
wsvha_model1_randomForest_bootstrapped_demBased_100m_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/outputs/wsvha/bootstrapped/wsvha_model1_randomForest_bootstrapped_demBased_100m_quesnel.tif")

par(mfrow = c(2, 2)) # Visualization
plot(wsvha_model1_randomForest_bootstrapped_demBased_100m_gaspard, main="Gaspard WSVHA: Random Forest", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_randomForest_bootstrapped_demBased_100m_quesnel, main="Quesnel WSVHA: Random Forest", cex.main=0.8, maxpixels=22000000)
hist(wsvha_model1_randomForest_bootstrapped_demBased_100m_gaspard, main="Gaspard WSVHA: Random Forest", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_randomForest_bootstrapped_demBased_100m_quesnel, main="Quesnel WSVHA: Random Forest", cex.main=0.8, maxpixels=22000000) 




## Model: Support Vector Machine Radial Kernel

tuneResult_svm_m1_full <- train(
  wsvha_L~., data=faib_vri_true_m1_df_boot_stemfree,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)), tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'), verbose=F)

tuneResult_svm_m1_train <- train(
  wsvha_L~., data=train_m1_boot,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)), tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'), verbose=F)


tunedModel_svmRadial_m1 = predict(tuneResult_svm_m1_full, newdata=faib_vri_true_m1_df)
tunedModel_svmRadial_m1_test = predict(tuneResult_svm_m1_train, newdata=test_m1)
R2(tunedModel_svmRadial_m1, faib_vri_true_m1_df$wsvha_L)
MAE(tunedModel_svmRadial_m1, faib_vri_true_m1_df$wsvha_L)
RMSE(tunedModel_svmRadial_m1, faib_vri_true_m1_df$wsvha_L)
MAE(tunedModel_svmRadial_m1_test, test_m1$wsvha_L)
RMSE(tunedModel_svmRadial_m1_test, test_m1$wsvha_L)






## Model: Support Vector Machine Radial Kernel - E1071


tuneResult_svm_m1_train <- best.svm(
  x=X_train_m1_boot, y=y_train_m1_boot,
  tunecontrol = tune.control(sampling = "cross", cross = 10),
  preProcess = c('YeoJohnson', 'scale', 'center', 'corr'))

tunedModel_svm_m1 = predict(tuneResult_svm_m1_full, data=faib_vri_true_m1_df)
tunedModel_svm_m1_test = predict(tuneResult_svm_m1_train, data=faib_vri_true_m1_df)
save(tunedModel_svm_m1, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_svmRadial_bootstrapped_demBased_e1071.RData")

R2(tunedModel_svm_m1, faib_vri_true_m1_df_boot_stemfree$wsvha_L)
MAE(tunedModel_svm_m1, y_m1_boot)
RMSE(tunedModel_svm_m1, y_m1_boot)
MAE(tunedModel_svm_m1_test, y_test_m1_boot)
RMSE(tunedModel_svm_m1_test, y_test_m1_boot)

wsvha_model1_svmRadial_bootstrapped_demBased_100m_quesnel <- predict(covs_m1_quesnel, tunedModel_svm_m1_full)
wsvha_model1_svmRadial_bootstrapped_demBased_100m_gaspard <- predict(covs_m1_gaspard, tunedModel_svm_m1_full)
par(mfrow = c(2, 2)) # Visualization
plot(tunedModel_svm_m1_to_raster_gaspard, main="Gaspard: Model1 SVMRadial", cex.main=0.8, maxpixels=22000000)
hist(tunedModel_svm_m1_to_raster_gaspard, main="Gaspard: Model1 SVMRadial", cex.main=0.8, maxpixels=22000000) 
plot(tunedModel_svm_m1_to_raster_quesnel, main="Quesnel: Model1 SVMRadial" cex.main=0.8, maxpixels=22000000) 
hist(tunedModel_svm_m1_to_raster_quesnel, main="Quesnel: Model1 SVMRadial", cex.main=0.8, maxpixels=22000000) 


## Model: Support Vector Machine Radial Kernel (epsilon-untuned)
tuneResult_svm_m1_full <- train(
  X_m1_boot, y_m1_boot, 
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m1_train <- train(
  X_train_m1_boot, y_train_m1_boot,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'), 
  verbose=F)

#tunedModel_svm_m1 <- tuneResult_svm_m1_full$finalModel
#tunedModel_svm_m1_train_pred <- tuneResult_svm_m1_train$finalModel

tunedModel_svmRadial_m1 = predict(tuneResult_svm_m1_full, data = faib_vri_true_m1_df_boot_stemfree)
tunedModel_svmRadial_m1_test = predict(tuneResult_svm_m1_train, data = test_m1_boot)
save(tunedModel_svm_m1_full, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_svmRadial.RData")

R2(tunedModel_svmRadial_m1, faib_vri_true_m1_df$wsvha_L)
MAE(tunedModel_svmRadial_m1, faib_vri_true_m1_df$wsvha_L)
RMSE(tunedModel_svmRadial_m1, faib_vri_true_m1_df$wsvha_L)
MAE(tunedModel_svmRadial_m1_test, test_m1$wsvha_L)
RMSE(tunedModel_svmRadial_m1_test, test_m1$wsvha_L)

wsvha_model1_svmRadial_bootstrapped_demBased_100m_gaspard <- raster::predict(covs_m1_gaspard, tuneResult_svm_m1_full)
wsvha_model1_svmRadial_bootstrapped_demBased_100m_quesnel <- raster::predict(covs_m1_quesnel, tuneResult_svm_m1_full)
wsvha_model1_svmRadial_bootstrapped_demBased_100m_gaspard$layer[wsvha_model1_svmRadial_bootstrapped_demBased_100m_gaspard$layer <= 0] = 0
wsvha_model1_svmRadial_bootstrapped_demBased_100m_quesnel$layer[wsvha_model1_svmRadial_bootstrapped_demBased_100m_quesnel$layer <= 0] = 0
plot(wsvha_model1_svmRadial_bootstrapped_demBased_100m_gaspard, main="Model1 NO-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_svmRadial_bootstrapped_demBased_100m_quesnel, main="Model1 NO-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000)
hist(wsvha_model1_svmRadial_bootstrapped_demBased_100m_gaspard, main="Model1 NO-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_svmRadial_bootstrapped_demBased_100m_quesnel, main="Model1 NO-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000) 




save(tunedModel_svm_m1_full, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_svmRadial.RData")
save(tunedModel_svm_m2_full, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model2_svmRadial.RData")
tunedModel_svm_m1_to_raster$layer[tunedModel_svm_m1_to_raster$layer <= 0] = 0
tunedModel_svm_m2_to_raster$layer[tunedModel_svm_m2_to_raster$layer <= 0] = 0
tunedModel_svm_m1_to_raster_gaspard$layer[tunedModel_svm_m1_to_raster_gaspard$layer <= 0] = 0
tunedModel_svm_m2_to_raster_gaspard$layer[tunedModel_svm_m2_to_raster_gaspard$layer <= 0] = 0
tunedModel_svm_m1_to_raster_quesnel$layer[tunedModel_svm_m1_to_raster_quesnel$layer <= 0] = 0
tunedModel_svm_m2_to_raster_quesnel$layer[tunedModel_svm_m2_to_raster_quesnel$layer <= 0] = 0
writeRaster(tunedModel_svm_m1_to_raster, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_100m_allAreas.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_100m_allAreas.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_100m_gaspard.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_100m_gaspard.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_100m_quesnel.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_100m_quesnel.tif", overwrite=TRUE)



```{r, fig.show='hold', out.width="25%", echo=FALSE}
wsvha_model1_svmRadial_100m_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_100m_gaspard.tif")
wsvha_model2_svmRadial_100m_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_100m_gaspard.tif")
wsvha_model1_svmRadial_100m_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_100m_quesnel.tif")
wsvha_model2_svmRadial_100m_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_100m_quesnel.tif")
plot(wsvha_model1_svmRadial_100m_gaspard, main="Model1 NO-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_svmRadial_100m_quesnel, main="Model1 NO-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model2_svmRadial_100m_gaspard, main="Model2 WITH-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model2_svmRadial_100m_quesnel, main="Model2 WITH-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000)
hist(wsvha_model1_svmRadial_100m_gaspard, main="Model1 NO-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_svmRadial_100m_quesnel, main="Model1 NO-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model2_svmRadial_100m_gaspard, main="Model2 WITH-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model2_svmRadial_100m_quesnel, main="Model2 WITH-STEMS (SVM Radial)", cex.main=0.8, maxpixels=22000000) 
```

## Model: Support Vector Machine Radial Kernel (epsilon-tuned)

```{r, eval=FALSE}
tuneResult_svm_m1_full_eps <- train(
  X_m1, y_m1, 
  trControl = fitControl_YeoJx10,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)), tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m2_full_eps = train(
  X_m2, y_m2,
  trControl = fitControl_YeoJx10,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full_eps <- tuneResult_svm_m2_full_eps$finalModel
tunedModel_svm_m1_full_eps <- tuneResult_svm_m1_full_eps$finalModel
save(tunedModel_svm_m1_full_eps, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_svmRadial_e.RData")
save(tunedModel_svm_m2_full_eps, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model2_svmRadial_e.RData")
tunedModel_svm_m1_to_raster_eps <- raster::predict(covs_m1, tuneResult_svm_m1_full_eps)
tunedModel_svm_m2_to_raster_eps <- raster::predict(covs_m2, tuneResult_svm_m2_full_eps)
tunedModel_svm_m1_to_raster_eps_gaspard <- raster::predict(covs_m1_gaspard, tuneResult_svm_m1_full_eps)
tunedModel_svm_m2_to_raster_eps_gaspard <- raster::predict(covs_m2_gaspard, tuneResult_svm_m2_full_eps)
tunedModel_svm_m1_to_raster_eps_quesnel <- raster::predict(covs_m1_quesnel, tuneResult_svm_m1_full_eps)
tunedModel_svm_m2_to_raster_eps_quesnel <- raster::predict(covs_m2_quesnel, tuneResult_svm_m2_full_eps)
tunedModel_svm_m1_to_raster_eps$layer[tunedModel_svm_m1_to_raster_eps$layer <= 0] = 0
tunedModel_svm_m2_to_raster_eps$layer[tunedModel_svm_m2_to_raster_eps$layer <= 0] = 0
tunedModel_svm_m1_to_raster_eps_gaspard$layer[tunedModel_svm_m1_to_raster_eps_gaspard$layer <= 0] = 0
tunedModel_svm_m2_to_raster_eps_gaspard$layer[tunedModel_svm_m2_to_raster_eps_gaspard$layer <= 0] = 0
tunedModel_svm_m1_to_raster_eps_quesnel$layer[tunedModel_svm_m1_to_raster_eps_quesnel$layer <= 0] = 0
tunedModel_svm_m2_to_raster_eps_quesnel$layer[tunedModel_svm_m2_to_raster_eps_quesnel$layer <= 0] = 0
writeRaster(tunedModel_svm_m1_to_raster_eps, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_e_100m_allAreas.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster_eps, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_e_100m_allAreas.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_eps_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_e_100m_gaspard.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster_eps_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_e_100m_gaspard.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_eps_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_e_100m_quesnel.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster_eps_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_e_100m_quesnel.tif", overwrite=TRUE)
```


```{r, fig.show='hold', out.width="25%", echo=FALSE}
wsvha_model1_svmRadial_e_100m_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_e_100m_gaspard.tif")
wsvha_model2_svmRadial_e_100m_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_e_100m_gaspard.tif")
wsvha_model1_svmRadial_e_100m_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_e_100m_quesnel.tif")
wsvha_model2_svmRadial_e_100m_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmRadial_e_100m_quesnel.tif")
plot(wsvha_model1_svmRadial_e_100m_gaspard, main="Model1 NO-STEMS (SVM Radial epsilon-tuned)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_svmRadial_e_100m_quesnel, main="Model1 NO-STEMS (SVM Radial epsilon-tuned)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model2_svmRadial_e_100m_gaspard, main="Model2 WITH-STEMS (SVM Radial epsilon-tuned)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model2_svmRadial_e_100m_quesnel, main="Model2 WITH-STEMS (SVM Radial epsilon-tuned)", cex.main=0.8, maxpixels=22000000)
hist(wsvha_model1_svmRadial_e_100m_gaspard, main="Model1 NO-STEMS (SVM Radial epsilon-tuned)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_svmRadial_e_100m_quesnel, main="Model1 NO-STEMS (SVM Radial epsilon-tuned)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model2_svmRadial_e_100m_gaspard, main="Model2 WITH-STEMS (SVM Radial epsilon-tuned)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model2_svmRadial_e_100m_quesnel, main="Model2 WITH-STEMS (SVM Radial epsilon-tuned)", cex.main=0.8, maxpixels=22000000) 
```

## Model: Support Vector Machine Linear Kernel

```{r, eval=FALSE}
tuneResult_svm_m1_full_linear <- train(
  X_m1, y_m1,
  trControl = fitControl_YeoJx10,
  method = 'svmLinear', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m2_full_linear <- train(
  X_m2, y_m2,
  trControl = fitControl_YeoJx10,
  method = 'svmLinear',metric = 'RMSE', 
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full_linear <- tuneResult_svm_m2_full_linear$finalModel
tunedModel_svm_m1_full_linear <- tuneResult_svm_m1_full_linear$finalModel
save(tunedModel_svm_m1_full_linear, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_svmLinear.RData")
save(tunedModel_svm_m2_full_linear, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model2_svmLinear.RData")

tuneResult_svmLinear_m2_10k_train <- train(
  X_train_m2, y_train_m2,
  trControl = fitControl_YeoJx10,
  method = 'svmLinear', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svmLinear_m1_10k_train <- train(
  X_train_m1, y_train_m1,
  trControl = fitControl_YeoJx10,
  method = 'svmLinear',metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svmLinear_m2_test = predict(tuneResult_svmLinear_m2_10k_train, data = test_m2)
tunedModel_svmLinear_m1_test = predict(tuneResult_svmLinear_m1_10k_train, data = test_m1)
tunedModel_svmLinear_m2_test_MAE = MAE(tunedModel_svmLinear_m2_test, test_m2$wsvha_L)
tunedModel_svmLinear_m1_test_MAE = MAE(tunedModel_svmLinear_m1_test, test_m1$wsvha_L)
tunedModel_svmLinear_m2_test_RMSE = RMSE(tunedModel_svmLinear_m2_test, test_m2$wsvha_L)
tunedModel_svmLinear_m1_test_RMSE = RMSE(tunedModel_svmLinear_m1_test, test_m1$wsvha_L)

tunedModel_svmLinear_m2 = predict(tuneResult_svm_m2_full_linear, data = faib_vri_true_m2_df)
tunedModel_svmLinear_m1 = predict(tuneResult_svm_m1_full_linear, data = faib_vri_true_m1_df)
tunedModel_svmLinear_m2_MAE = MAE(tunedModel_svmLinear_m2, faib_vri_true_m2_df$wsvha_L)
tunedModel_svmLinear_m1_MAE = MAE(tunedModel_svmLinear_m1, faib_vri_true_m1_df$wsvha_L)
tunedModel_svmLinear_m2_RMSE = RMSE(tunedModel_svmLinear_m2, faib_vri_true_m2_df$wsvha_L)
tunedModel_svmLinear_m1_RMSE = RMSE(tunedModel_svmLinear_m1, faib_vri_true_m1_df$wsvha_L)

tunedModel_svm_m2_full_linear
R2(tunedModel_svmLinear_m2, y_m2)
tunedModel_svmLinear_m2_MAE
tunedModel_svmLinear_m2_RMSE 
tunedModel_svmLinear_m2_test_MAE
tunedModel_svmLinear_m2_test_RMSE
tunedModel_svmLinear_m2_RMSE/tunedModel_svmLinear_m2_test_RMSE

tunedModel_svm_m1_full_linear
R2(tunedModel_svmLinear_m1, y_m1)
tunedModel_svmLinear_m1_MAE
tunedModel_svmLinear_m1_RMSE 
tunedModel_svmLinear_m1_test_MAE
tunedModel_svmLinear_m1_test_RMSE
tunedModel_svmLinear_m1_RMSE/tunedModel_svmLinear_m1_test_RMSE

tunedModel_svm_m1_to_raster_linear <- raster::predict(covs_m1, tuneResult_svm_m1_full_linear)
tunedModel_svm_m2_to_raster_linear <- raster::predict(covs_m2, tuneResult_svm_m2_full_linear)
tunedModel_svm_m1_to_raster_linear_gaspard <- raster::predict(covs_m1_gaspard, tuneResult_svm_m1_full_linear)
tunedModel_svm_m2_to_raster_linear_gaspard <- raster::predict(covs_m2_gaspard, tuneResult_svm_m2_full_linear)
tunedModel_svm_m1_to_raster_linear_quesnel <- raster::predict(covs_m1_quesnel, tuneResult_svm_m1_full_linear)
tunedModel_svm_m2_to_raster_linear_quesnel <- raster::predict(covs_m2_quesnel, tuneResult_svm_m2_full_linear)
tunedModel_svm_m1_to_raster_linear$layer[tunedModel_svm_m1_to_raster_linear$layer <= 0] = 0
tunedModel_svm_m2_to_raster_linear$layer[tunedModel_svm_m2_to_raster_linear$layer <= 0] = 0
tunedModel_svm_m1_to_raster_linear_gaspard$layer[tunedModel_svm_m1_to_raster_linear_gaspard$layer <= 0] = 0
tunedModel_svm_m2_to_raster_linear_gaspard$layer[tunedModel_svm_m2_to_raster_linear_gaspard$layer <= 0] = 0
tunedModel_svm_m1_to_raster_linear_quesnel$layer[tunedModel_svm_m1_to_raster_linear_quesnel$layer <= 0] = 0
tunedModel_svm_m2_to_raster_linear_quesnel$layer[tunedModel_svm_m2_to_raster_linear_quesnel$layer <= 0] = 0
writeRaster(tunedModel_svm_m1_to_raster_linear, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmLinear_100m.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster_linear, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmLinear_100m.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_linear_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmLinear_100m_gaspard.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster_linear_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmLinear_100m_gaspard.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_linear_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmLinear_100m_quesnel.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m2_to_raster_linear_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmLinear_100m_quesnel.tif", overwrite=TRUE)
```

```{r, fig.show='hold', out.width="25%", echo=FALSE}
wsvha_model1_svmLinear_100m_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmLinear_100m_gaspard.tif")
wsvha_model2_svmLinear_100m_gaspard = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmLinear_100m_gaspard.tif")
wsvha_model1_svmLinear_100m_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmLinear_100m_quesnel.tif")
wsvha_model2_svmLinear_100m_quesnel = raster::raster("/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model2_svmLinear_100m_quesnel.tif")
plot(wsvha_model1_svmLinear_100m_gaspard, main="Model1 NO-STEMS (SVM Linear)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model2_svmLinear_100m_gaspard, main="Model1 NO-STEMS (SVM Linear)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_svmLinear_100m_quesnel, main="Model2 WITH-STEMS (SVM Linear)", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model2_svmLinear_100m_quesnel, main="Model2 WITH-STEMS (SVM Linear)", cex.main=0.8, maxpixels=22000000)
hist(wsvha_model1_svmLinear_100m_gaspard, main="Model1 NO-STEMS (SVM Linear)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model2_svmLinear_100m_gaspard, main="Model1 NO-STEMS (SVM Linear)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_svmLinear_100m_quesnel, main="Model2 WITH-STEMS (SVM Linear)", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model2_svmLinear_100m_quesnel, main="Model2 WITH-STEMS (SVM Linear)", cex.main=0.8, maxpixels=22000000) 
```


## Model: Random Forest caret-trained
grid_rf = expand.grid(.mtry=c(2,4))    
rfbag_m1_tuned = train(
  X_m1_boot, y_m1_boot,
  method="rf", metric="RMSE", ntree=50,
  trControl = fitControl_YeoJx1,
  tuneGrid = grid_rf)

rfbag_m1_tuned_train = train(
  X_train_m1_boot, y_train_m1_boot,
  method="rf", metric="RMSE", ntree=50,
  trControl = fitControl_YeoJx1,
  tuneGrid = grid_rf)


rfbag_m1_tuned
rfbag_m1_tuned_full = predict(rfbag_m1_tuned, newdata=faib_vri_true_m1_df, type="raw")
rfbag_m1_tuned_test = predict(rfbag_m1_tuned_train, newdata=test_m1, type="raw")
R2(rfbag_m1_tuned_full, faib_vri_true_m1_df$wsvha_L)
MAE(rfbag_m1_tuned_full, faib_vri_true_m1_df$wsvha_L)
RMSE(rfbag_m1_tuned_full, faib_vri_true_m1_df$wsvha_L)
MAE(rfbag_m1_tuned_test, test_m1$wsvha_L)
RMSE(rfbag_m1_tuned_test, test_m1$wsvha_L)

wsvha_model1_randomForest_bootstrapped_demBased <- rfbag_m1_tuned
save(wsvha_model1_randomForest_bootstrapped_demBased, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_randomForest_bootstrapped_demBased.RData")
wsvha_model1_randomForest_bootstrapped_demBased_100m_allAreas <- raster::predict(covs_m1, wsvha_model1_randomForest_bootstrapped_demBased)
wsvha_model1_randomForest_bootstrapped_demBased_100m_gaspard <- raster::predict(covs_m1_gaspard, wsvha_model1_randomForest_bootstrapped_demBased)
wsvha_model1_randomForest_bootstrapped_demBased_100m_quesnel <- raster::predict(covs_m1_quesnel, wsvha_model1_randomForest_bootstrapped_demBased)
##................................................
wsvha_model1_randomForest_bagged_100m_gaspard$layer[wsvha_model1_randomForest_bagged_100m_gaspard$layer <= 0] = 0
wsvha_model1_randomForest_bagged_100m_gaspard$layer[wsvha_model1_randomForest_bagged_100m_gaspard$layer <= 0] = 0
wsvha_model1_randomForest_bagged_100m_quesnel$layer[wsvha_model1_randomForest_bagged_100m_quesnel$layer <= 0] = 0
writeRaster(wsvha_model1_randomForest_bagged_100m_allAreas, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_randomForest_bagged_100m_allAreas.tif", overwrite=TRUE)
writeRaster(wsvha_model1_randomForest_bagged_100m_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_randomForest_bagged_100m_gaspard.tif", overwrite=TRUE)
writeRaster(wsvha_model1_randomForest_bagged_100m_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_randomForest_bagged_100m_quesnel.tif", overwrite=TRUE)
graphics.off()
par(mfrow = c(3, 1)) 
plot(wsvha_model1_randomForest_bagged_100m_gaspard, main="Gaspard: Model1 RandomForest", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_randomForest_bagged_100m_quesnel, main="Quesnel: Model1 RandomForest", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_randomForest_bagged_100m_allAreas, main="All Areas: Model1 RandomForest", cex.main=0.8, maxpixels=22000000) 
