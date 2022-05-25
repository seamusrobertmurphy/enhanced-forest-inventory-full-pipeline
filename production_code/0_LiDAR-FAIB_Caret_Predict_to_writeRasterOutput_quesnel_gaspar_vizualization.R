
library(devtools)
library(rgdal)
library(rgeos)
library(tibble)
library(conflicted)
library(sf)
library(terra)
library(raster)
library(rasterVis)
library(lattice)
library(latticeExtra)
library(ggplot2)
library(dplyr)
library(caret)
library(caretEnsemble)
library(tibble)
library(pre)
library(glmnet)
library(purrr)
library(imager)
library(ForestTools)
library(tibble)
library(randomForest)
library(e1071)
set.seed(123)

# Import raster stacks
lead_htop_raster_quesnel = raster::raster("./data_quesnel/covariates/lead_htop_ttops_100cell.tif")
stemsha_L_raster_quesnel = raster::raster("./data_quesnel/covariates/stemsha_L_ttops_100cell.tif")
elev_raster_quesnel = raster::raster("./data_quesnel/covariates/elev_raster.tif")
slope_raster_quesnel = raster::raster("./data_quesnel/covariates/slope_raster.tif")
asp_cos_raster_quesnel = raster::raster("./data_quesnel/covariates/asp_cos_raster.tif")
asp_sin_raster_quesnel = raster::raster("./data_quesnel/covariates/asp_sin_raster.tif")
species_class_raster_quesnel = raster::raster("./data_quesnel/covariates/species_class_raster.tif")

lead_htop_raster_gaspard = raster::raster("./data_gaspard/covariates/lead_htop_ttops_100cell.tif")
stemsha_L_raster_gaspard = raster::raster("./data_gaspard/covariates/stemsha_L_ttops_100cell.tif")
elev_raster_gaspard = raster::raster("./data_gaspard/covariates/elev_raster.tif")
slope_raster_gaspard = raster::raster("./data_gaspard/covariates/slope_raster.tif")
asp_cos_raster_gaspard = raster::raster("./data_gaspard/covariates/asp_cos_raster.tif")
asp_sin_raster_gaspard = raster::raster("./data_gaspard/covariates/asp_sin_raster.tif")
species_class_raster_gaspard = raster::raster("./data_gaspard/covariates/species_class_raster.tif")

lead_htop_rast_quesnel = terra::rast(lead_htop_raster_quesnel)
stemsha_L_rast_quesnel = terra::rast(stemsha_L_raster_quesnel)
elev_rast_quesnel = terra::rast(elev_raster_quesnel)
slope_rast_quesnel = terra::rast(slope_raster_quesnel)
asp_cos_rast_quesnel = terra::rast(asp_cos_raster_quesnel)
asp_sin_rast_quesnel = terra::rast(asp_sin_raster_quesnel)
species_class_rast_quesnel = terra::rast(species_class_raster_quesnel)

lead_htop_rast_gaspard = terra::rast(lead_htop_raster_gaspard)
stemsha_L_rast_gaspard = terra::rast(stemsha_L_raster_gaspard)
elev_rast_gaspard = terra::rast(elev_raster_gaspard)
slope_rast_gaspard = terra::rast(slope_raster_gaspard)
asp_cos_rast_gaspard = terra::rast(asp_cos_raster_gaspard)
asp_sin_rast_gaspard = terra::rast(asp_sin_raster_gaspard)
species_class_rast_gaspard = terra::rast(species_class_raster_gaspard)

lead_htop_rast_quesnel = terra::resample(lead_htop_rast_quesnel, elev_rast_quesnel)
stemsha_L_rast_quesnel = terra::resample(stemsha_L_rast_quesnel, elev_rast_quesnel)
species_class_rast_quesnel = terra::resample(species_class_rast_quesnel, elev_rast_quesnel)
elev_rast_quesnel = terra::mask(elev_rast_quesnel, lead_htop_rast_quesnel)
slope_rast_quesnel = terra::mask(slope_rast_quesnel, lead_htop_rast_quesnel)
asp_cos_rast_quesnel = terra::mask(asp_cos_rast_quesnel, lead_htop_rast_quesnel)
asp_sin_rast_quesnel = terra::mask(asp_sin_rast_quesnel, lead_htop_rast_quesnel)
species_class_rast_quesnel = terra::mask(species_class_rast_quesnel, lead_htop_rast_quesnel)
lead_htop_rast_quesnel = terra::mask(lead_htop_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
elev_rast_quesnel = terra::mask(elev_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
slope_rast_quesnel = terra::mask(slope_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
asp_cos_rast_quesnel = terra::mask(asp_cos_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
asp_sin_rast_quesnel = terra::mask(asp_sin_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
stemsha_L_rast_quesnel = terra::mask(stemsha_L_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)

lead_htop_rast_gaspard = terra::resample(lead_htop_rast_gaspard, elev_rast_gaspard)
stemsha_L_rast_gaspard = terra::resample(stemsha_L_rast_gaspard, elev_rast_gaspard)
species_class_rast_gaspard = terra::resample(species_class_rast_gaspard, elev_rast_gaspard)
elev_rast_gaspard = terra::mask(elev_rast_gaspard, lead_htop_rast_gaspard)
slope_rast_gaspard = terra::mask(slope_rast_gaspard, lead_htop_rast_gaspard)
asp_cos_rast_gaspard = terra::mask(asp_cos_rast_gaspard, lead_htop_rast_gaspard)
asp_sin_rast_gaspard = terra::mask(asp_sin_rast_gaspard, lead_htop_rast_gaspard)
species_class_rast_gaspard = terra::mask(species_class_rast_gaspard, lead_htop_rast_gaspard)
lead_htop_rast_gaspard = terra::mask(lead_htop_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
elev_rast_gaspard = terra::mask(elev_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
slope_rast_gaspard = terra::mask(slope_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
asp_cos_rast_gaspard = terra::mask(asp_cos_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
asp_sin_rast_gaspard = terra::mask(asp_sin_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)
stemsha_L_rast_gaspard = terra::mask(stemsha_L_rast_gaspard, species_class_rast_gaspard, inverse=FALSE)

names(elev_rast_quesnel) = "elev"
names(slope_rast_quesnel) = "slope"
names(asp_cos_rast_quesnel) = "asp_cos"
names(asp_sin_rast_quesnel) = "asp_sin"
names(species_class_rast_quesnel) = "species_class"
names(stemsha_L_rast_quesnel) = "stemsha_L"
names(lead_htop_rast_quesnel) = "lead_htop"

names(elev_rast_gaspard) = "elev"
names(slope_rast_gaspard) = "slope"
names(asp_cos_rast_gaspard) = "asp_cos"
names(asp_sin_rast_gaspard) = "asp_sin"
names(species_class_rast_gaspard) = "species_class"
names(stemsha_L_rast_gaspard) = "stemsha_L"
names(lead_htop_rast_gaspard) = "lead_htop"

elev_raster_quesnel = raster::raster(elev_rast_quesnel)
slope_raster_quesnel = raster::raster(slope_rast_quesnel)
asp_cos_raster_quesnel = raster::raster(asp_cos_rast_quesnel)
asp_sin_raster_quesnel = raster::raster(asp_sin_rast_quesnel)
species_class_raster_quesnel = raster::raster(species_class_rast_quesnel)
stemsha_L_raster_quesnel = raster::raster(stemsha_L_rast_quesnel)
lead_htop_raster_quesnel = raster::raster(lead_htop_rast_quesnel)

elev_raster_gaspard = raster::raster(elev_rast_gaspard)
slope_raster_gaspard = raster::raster(slope_rast_gaspard)
asp_cos_raster_gaspard = raster::raster(asp_cos_rast_gaspard)
asp_sin_raster_gaspard = raster::raster(asp_sin_rast_gaspard)
species_class_raster_gaspard = raster::raster(species_class_rast_gaspard)
stemsha_L_raster_gaspard = raster::raster(stemsha_L_rast_gaspard)
lead_htop_raster_gaspard = raster::raster(lead_htop_rast_gaspard)

# merge raster layers across sites
elev_raster_list = list(elev_raster_quesnel, elev_raster_gaspard)
slope_raster_list = list(slope_raster_quesnel, slope_raster_gaspard)
asp_cos_raster_list = list(asp_cos_raster_quesnel, asp_cos_raster_gaspard)
asp_sin_raster_list = list(asp_sin_raster_quesnel, asp_sin_raster_gaspard)
species_class_raster_list = list(species_class_raster_quesnel, species_class_raster_gaspard)
stemsha_L_raster_list = list(stemsha_L_raster_quesnel, stemsha_L_raster_gaspard)
lead_htop_raster_list = list(lead_htop_raster_quesnel, lead_htop_raster_gaspard)

elev_raster = do.call(merge, c(elev_raster_list, tolerance = 1))
slope_raster = do.call(merge, c(slope_raster_list, tolerance = 1))
asp_cos_raster = do.call(merge, c(asp_cos_raster_list, tolerance = 1))
asp_sin_raster = do.call(merge, c(asp_sin_raster_list, tolerance = 1))
species_class_raster = do.call(merge, c(species_class_raster_list, tolerance = 1))
stemsha_L_raster = do.call(merge, c(stemsha_L_raster_list, tolerance = 1))
lead_htop_raster = do.call(merge, c(lead_htop_raster_list, tolerance = 1))

writeRaster(elev_raster, filename = "./data_quesnel_gaspard/covariates/elev_raster.tif", overwrite=TRUE)
writeRaster(slope_raster, filename = "./data_quesnel_gaspard/covariates/slope_raster.tif", overwrite=TRUE)
writeRaster(asp_cos_raster, filename = "./data_quesnel_gaspard/covariates/asp_cos_raster.tif", overwrite=TRUE)
writeRaster(asp_sin_raster, filename = "./data_quesnel_gaspard/covariates/asp_sin_raster.tif", overwrite=TRUE)
writeRaster(species_class_raster, filename = "./data_quesnel_gaspard/covariates/species_class_raster.tif", overwrite=TRUE)
writeRaster(stemsha_L_raster, filename = "./data_quesnel_gaspard/covariates/stemsha_L_raster.tif", overwrite=TRUE)
writeRaster(lead_htop_raster, filename = "./data_quesnel_gaspard/covariates/lead_htop_raster.tif", overwrite=TRUE)

lead_htop_rast = terra::rast(lead_htop_raster)
stemsha_L_rast = terra::rast(stemsha_L_raster)
elev_rast = terra::rast(elev_raster)
slope_rast = terra::rast(slope_raster)
asp_cos_rast = terra::rast(asp_cos_raster)
asp_sin_rast = terra::rast(asp_sin_raster)
species_class_rast = terra::rast(species_class_raster)
plot(species_class_rast)
plot(elev_rast)

aspect_rast = terra::terrain(elev_rast, v="aspect", unit="degrees", neighbors=8) # generate aspect for glm-fit
aspect_rast_gaspard = terra::terrain(elev_rast_gaspard, v="aspect", unit="degrees", neighbors=8) # generate aspect for glm-fit
aspect_rast_quesnel = terra::terrain(elev_rast_quesnel, v="aspect", unit="degrees", neighbors=8) # generate aspect for glm-fit
aspect_raster = raster::raster(aspect_rast)
aspect_raster_gaspard = raster::raster(aspect_rast_gaspard)
aspect_raster_quesnel = raster::raster(aspect_rast_quesnel)

terra::plot(lead_htop_rast, main = "lead_htop (all sites)")
terra::plot(stemsha_L_rast, main = "stemsha_L (all sites)")
terra::plot(elev_rast, main = "elevation (all sites)")
terra::plot(slope_rast, main = "slope (all sites)")
terra::plot(asp_cos_rast, main = "asp_cos (all sites)")
terra::plot(asp_sin_rast, main = "asp_sin (all sites)")
terra::plot(aspect_rast, main = "aspect (all sites)")
terra::plot(species_class_rast, main = "species_class (all sites)")

terra::plot(lead_htop_rast_gaspard, main = "lead_htop (Gaspard)")
terra::plot(stemsha_L_rast_gaspard, main = "stemsha_L (Gaspard)")
terra::plot(elev_rast_gaspard, main = "elevation (Gaspard)")
terra::plot(slope_rast_gaspard, main = "slope (Gaspard)")
terra::plot(asp_cos_rast_gaspard, main = "asp_cos (Gaspard)")
terra::plot(asp_sin_rast_gaspard, main = "asp_sin (Gaspard)")
terra::plot(aspect_rast_gaspard, main = "aspect (Gaspard)")
terra::plot(species_class_rast_gaspard, main = "species_class (Gaspard)")

terra::plot(lead_htop_rast_quesnel, main = "lead_htop (Quesnel)")
terra::plot(stemsha_L_rast_quesnel, main = "stemsha_L (Quesnel)")
terra::plot(elev_rast_quesnel, main = "elevation (Quesnel)")
terra::plot(slope_rast_quesnel, main = "slope (Quesnel)")
terra::plot(asp_cos_rast_quesnel, main = "asp_cos (Quesnel)")
terra::plot(asp_sin_rast_quesnel, main = "asp_sin (Quesnel)")
terra::plot(aspect_rast_quesnel, main = "aspect (Quesnel)")
terra::plot(species_class_rast_quesnel, main = "species_class (Quesnel)")

# refactor species covariate to enable glmGamma modelling
#species_class_sv = terra::as.polygons(species_class_rast)
#species_class_sf = sf::st_as_sf(species_class_sv)
#species_class_sf$layer = dplyr::recode(species_class_sf$layer, '0' = 1, '1' = 2, '2' = 3, '3' = 4, '4' = 5, '5' = 6, '6' = 7, '7' = 8)
#species_class_rast = terra::rasterize(terra::vect(species_class_sf), species_class_rast, field = "layer", touches = TRUE)

#species_class_sv_gaspard = terra::as.polygons(species_class_rast_gaspard)
#species_class_sf_gaspard = sf::st_as_sf(species_class_sv_gaspard)
#species_class_sf_gaspard$layer = dplyr::recode(species_class_sf_gaspard$layer, '0' = 1, '1' = 2, '2' = 3, '3' = 4, '4' = 5, '5' = 6, '6' = 7, '7' = 8)
#species_class_rast_gaspard = terra::rasterize(terra::vect(species_class_sf_gaspard), species_class_rast_gaspard, field = "layer", touches = TRUE)

#species_class_sv_quesnel = terra::as.polygons(species_class_rast_quesnel)
#species_class_sf_quesnel = sf::st_as_sf(species_class_sv_quesnel)
#species_class_sf_quesnel$layer = dplyr::recode(species_class_sf_quesnel$layer, '0' = 1, '1' = 2, '2' = 3, '3' = 4, '4' = 5, '5' = 6, '6' = 7, '7' = 8)
#species_class_rast_quesnel = terra::rasterize(terra::vect(species_class_sf_quesnel), species_class_rast_quesnel, field = "layer", touches = TRUE)

names(elev_rast) = "elev"
names(slope_rast) = "slope"
names(asp_cos_rast) = "asp_cos"
names(asp_sin_rast) = "asp_sin"
names(stemsha_L_rast) = "stemsha_L"
names(lead_htop_rast) = "lead_htop"
names(species_class_rast) = "species_class"

names(elev_rast_gaspard) = "elev"
names(slope_rast_gaspard) = "slope"
names(asp_cos_rast_gaspard) = "asp_cos"
names(asp_sin_rast_gaspard) = "asp_sin"
names(stemsha_L_rast_gaspard) = "stemsha_L"
names(lead_htop_rast_gaspard) = "lead_htop"
names(species_class_rast_gaspard) = "species_class"

names(elev_rast_quesnel) = "elev"
names(slope_rast_quesnel) = "slope"
names(asp_cos_rast_quesnel) = "asp_cos"
names(asp_sin_rast_quesnel) = "asp_sin"
names(stemsha_L_rast_quesnel) = "stemsha_L"
names(lead_htop_rast_quesnel) = "lead_htop"
names(species_class_rast_quesnel) = "species_class"

elev_raster = raster::raster(elev_rast)
slope_raster = raster::raster(slope_rast)
asp_cos_raster = raster::raster(asp_cos_rast)
asp_sin_raster = raster::raster(asp_sin_rast)
species_class_raster = raster::raster(species_class_rast)
stemsha_L_raster = raster::raster(stemsha_L_rast)
lead_htop_raster = raster::raster(lead_htop_rast)

elev_raster_quesnel = raster::raster(elev_rast_quesnel)
slope_raster_quesnel = raster::raster(slope_rast_quesnel)
asp_cos_raster_quesnel = raster::raster(asp_cos_rast_quesnel)
asp_sin_raster_quesnel = raster::raster(asp_sin_rast_quesnel)
species_class_raster_quesnel = raster::raster(species_class_rast_quesnel)
stemsha_L_raster_quesnel = raster::raster(stemsha_L_rast_quesnel)
lead_htop_raster_quesnel = raster::raster(lead_htop_rast_quesnel)

elev_raster_gaspard = raster::raster(elev_rast_gaspard)
slope_raster_gaspard = raster::raster(slope_rast_gaspard)
asp_cos_raster_gaspard = raster::raster(asp_cos_rast_gaspard)
asp_sin_raster_gaspard = raster::raster(asp_sin_rast_gaspard)
species_class_raster_gaspard = raster::raster(species_class_rast_gaspard)
stemsha_L_raster_gaspard = raster::raster(stemsha_L_rast_gaspard)
lead_htop_raster_gaspard = raster::raster(lead_htop_rast_gaspard)

covs_m1_quesnel = raster::stack(
  elev_raster_quesnel, 
  slope_raster_quesnel,
  #aspect_raster_quesnel,
  asp_cos_raster_quesnel,
  asp_sin_raster_quesnel,
  species_class_raster_quesnel,
  lead_htop_raster_quesnel,
  stemsha_L_raster_quesnel)

covs_m1_gaspard = raster::stack(
  elev_raster_gaspard, 
  slope_raster_gaspard,
  #aspect_raster_gaspard,
  asp_cos_raster_gaspard,
  asp_sin_raster_gaspard,
  species_class_raster_gaspard,
  lead_htop_raster_gaspard,
  stemsha_L_raster_gaspard)

covs_m1 = raster::stack(
  elev_raster, 
  slope_raster,
  #aspect_raster, 
  asp_cos_raster,
  asp_sin_raster,
  lead_htop_raster,
  species_class_raster,
  stemsha_L_raster)

covs_m2_quesnel = raster::stack(
  elev_raster_quesnel, 
  slope_raster_quesnel,
  #aspect_raster_quesnel,
  asp_cos_raster_quesnel,
  asp_sin_raster_quesnel,
  species_class_raster_quesnel,
  lead_htop_raster_quesnel)

covs_m2_gaspard = raster::stack(
  elev_raster_gaspard, 
  slope_raster_gaspard,
  #aspect_raster_gaspard,
  asp_cos_raster_gaspard,
  asp_sin_raster_gaspard,
  species_class_raster_gaspard,
  lead_htop_raster_gaspard)

covs_m2 = raster::stack(
  elev_raster, 
  slope_raster,
  #aspect_raster, 
  asp_cos_raster,
  asp_sin_raster,
  lead_htop_raster,
  species_class_raster)

names(covs_m1)
names(covs_m2)
# plot scatter matrix and distribution grids
rasterVis::splom(covs_m1_gaspard)
rasterVis::splom(covs_m1_quesnel)
rasterVis::splom(covs_m1)

rasterVis::histogram(covs_m1_gaspard)
rasterVis::histogram(covs_m1_quesnel)
rasterVis::histogram(covs_m1)

rasterVis::bwplot(covs_m1_gaspard)
rasterVis::bwplot(covs_m1_quesnel)
rasterVis::bwplot(covs_m1)

rasterVis::levelplot(covs_m1_gaspard)
rasterVis::levelplot(covs_m1_quesnel)
rasterVis::levelplot(covs_m1)

# Plot log scale transformation of rasters using zscaleLog argument and panel function. 
# Defaults to ‘NULL’, in which case the Raster* is not transformed. Other possible 
# values are any number that works as a base for taking logarithm, ‘TRUE’ (which is 
# equivalent to 10), and ‘“e”’ (for the natural logarithm). 
rasterVis::levelplot(lead_htop_raster_gaspard^2, zscaleLog=TRUE) 
rasterVis::levelplot(stemsha_L_raster_gaspard^2, zscaleLog=TRUE) 
rasterVis::levelplot(elev_raster_gaspard^2, zscaleLog=TRUE) 
rasterVis::levelplot(slope_raster_gaspard^2, zscaleLog=TRUE) 
rasterVis::levelplot(asp_cos_raster_gaspard^2, zscaleLog=TRUE) 
rasterVis::levelplot(asp_sin_raster_gaspard^2, zscaleLog=TRUE) 
rasterVis::levelplot(species_class_raster_gaspard^2, zscaleLog=TRUE) 

rasterVis::levelplot(lead_htop_raster_quesnel^2, zscaleLog=TRUE, main='lead_htop') 
rasterVis::levelplot(stemsha_L_raster_quesnel^2, zscaleLog=TRUE, main='stemsha_L') 
rasterVis::levelplot(elev_raster_quesnel^2, zscaleLog=TRUE, main='elev') 
rasterVis::levelplot(slope_raster_quesnel^2, zscaleLog=TRUE, main='slope') 
rasterVis::levelplot(aspect_raster_quesnel^2, zscaleLog=TRUE, main='aspect') 
#rasterVis::levelplot(species_class_raster_quesnel^2, zscaleLog=TRUE, main='species') 

rasterVis::levelplot(lead_htop_raster^2, zscaleLog=TRUE, main='lead_htop') 
rasterVis::levelplot(stemsha_L_raster^2, zscaleLog=TRUE, main='stemsha_L') 
rasterVis::levelplot(elev_raster^2, zscaleLog=TRUE, main='elev') 
rasterVis::levelplot(slope_raster^2, zscaleLog=TRUE, main='slope') 
rasterVis::levelplot(aspect_raster^2, zscaleLog=TRUE, main='aspect') 

rasterVis::levelplot(lead_htop_raster_gaspard^2, zscaleLog='e', main='lead_htop; log(n)') 
rasterVis::levelplot(stemsha_L_raster_gaspard^2, zscaleLog='e', main='stemsha_L; log(n)') 
rasterVis::levelplot(elev_raster_gaspard^2, zscaleLog='e', main='elev; log(n)') 
rasterVis::levelplot(slope_raster_gaspard^2, zscaleLog='e', main='slope; log(n)') 
rasterVis::levelplot(aspect_raster_gaspard^2, zscaleLog='e', main='aspect; log(n)') 

rasterVis::levelplot(lead_htop_raster_quesnel^2, zscaleLog='e', main='lead_htop; log(n)') 
rasterVis::levelplot(stemsha_L_raster_quesnel^2, zscaleLog='e', main='stemsha; log(n)') 
rasterVis::levelplot(elev_raster_quesnel^2, zscaleLog='e', main='elev; log(n)') 
rasterVis::levelplot(slope_raster_quesnel^2, zscaleLog='e', main='slope; log(n)') 
rasterVis::levelplot(aspect_raster_quesnel^2, zscaleLog='e', main='aspect; log(n)') 

rasterVis::levelplot(lead_htop_raster^2, zscaleLog='e', main='lead_htop; log(n)') 
rasterVis::levelplot(stemsha_L_raster^2, zscaleLog='e', main='stemsha; log(n)') 
rasterVis::levelplot(elev_raster^2, zscaleLog='e', main='elev; log(n)') 
rasterVis::levelplot(slope_raster^2, zscaleLog='e', main='slope; log(n)') 
rasterVis::levelplot(aspect_raster^2, zscaleLog='e', main='aspect; log(n)') 

#mean_covs_m1_gaspard = raster::cellStats(covs_m1_gaspard, mean)
#mean_covs_m1_quesnel = raster::cellStats(covs_m1_quesnel, mean)
#mean_covs_m1 = raster::cellStats(covs_m1, mean)
#rasterVis::levelplot(covs_m1_gaspard - mean_covs_m1_gaspard, par.settings = RdBuTheme())
#rasterVis::levelplot(covs_m1_quesnel - mean_covs_m1_quesnel, par.settings = RdBuTheme())
#rasterVis::levelplot(covs_m1 - mean_covs_m1, par.settings = RdBuTheme())


rasterVis::levelplot(lead_htop_raster_gaspard^2, zscaleLog='e', main='lead_htop; log(n)') 
rasterVis::levelplot(stemsha_L_raster_gaspard^2, zscaleLog='e', main='stemsha_L; log(n)') 
rasterVis::levelplot(elev_raster_gaspard^2, zscaleLog='e', main='elev; log(n)') 
rasterVis::levelplot(slope_raster_gaspard^2, zscaleLog='e', main='slope; log(n)') 
rasterVis::levelplot(aspect_raster_gaspard^2, zscaleLog='e', main='aspect; log(n)') 

rasterVis::levelplot(lead_htop_raster_quesnel^2, zscaleLog='e', main='lead_htop; log(n)') 
rasterVis::levelplot(stemsha_L_raster_quesnel^2, zscaleLog='e', main='stemsha; log(n)') 
rasterVis::levelplot(elev_raster_quesnel^2, zscaleLog='e', main='elev; log(n)') 
rasterVis::levelplot(slope_raster_quesnel^2, zscaleLog='e', main='slope; log(n)') 
rasterVis::levelplot(aspect_raster_quesnel^2, zscaleLog='e', main='aspect; log(n)') 

rasterVis::levelplot(lead_htop_raster^2, zscaleLog='e', main='lead_htop; log(n)') 
rasterVis::levelplot(stemsha_L_raster^2, zscaleLog='e', main='stemsha; log(n)') 
rasterVis::levelplot(elev_raster^2, zscaleLog='e', main='elev; log(n)') 
rasterVis::levelplot(slope_raster^2, zscaleLog='e', main='slope; log(n)') 
rasterVis::levelplot(aspect_raster^2, zscaleLog='e', main='aspect; log(n)') 

rasterVis::splom(covs_m1_gaspard, main='Gaspard')
rasterVis::splom(covs_m1_quesnel, main='Quesnel')
rasterVis::splom(covs_m1, main='all sites')

rasterVis::levelplot(covs_m1_gaspard, main='Gaspard')
rasterVis::levelplot(covs_m1_quesnel, main='Quesnel')
rasterVis::levelplot(covs_m1, main='all sites')


##########################
# Import ground plot data
faib_psp <- read.csv("/media/seamus/128GB_WORKD/EFI-TCC/0_Caret_Predict_to_writeRasterOutput/Data/FAIB_PSP_20211028.csv")
print(as_tibble(faib_psp), n = 10)
str(faib_psp)
summary.factor(faib_psp$util)
faib_psp = subset(faib_psp, util == '12.5')
summary.factor(faib_psp$util)
psych::describe(faib_psp)

faib_psp$spc_live1 = as.factor(faib_psp$spc_live1)
base::table(faib_psp$spc_live1, faib_psp$beclabel)
faib_psp =  subset(faib_psp, spc_live1=='PL' | spc_live1=='PLI' | spc_live1=='SB' | spc_live1=='SE' | spc_live1=='SW' | spc_live1=='SX' | spc_live1=='FD'| spc_live1=='FDI' | spc_live1=='CW' | spc_live1=='HW' | spc_live1=='BL' | spc_live1=='LW')
#faib_psp = faib_psp[!(faib_psp$species_class==2 & faib_psp$bgc_zone == 'SBS' | faib_psp$species_class==2 & faib_psp$bgc_zone =='SBPS' | faib_psp$species_class==2 & faib_psp$bgc_zone =='ICH'),]
faib_psp$species_class = dplyr::recode(faib_psp$spc_live1, PL = 1, PLI = 1, SB = 2, SE = 2, SX = 2, FD = 3, FDI = 3, CW = 3, HW = 4, BL = 5, LW = 6)
base::table(faib_psp$species_class, faib_psp$beclabel)


stemsha_L_raster_df = as.data.frame(rasterToPoints(stemsha_L_raster))
psych::describe(stemsha_L_raster_df)
faib_psp = subset(faib_psp, stemsha_L < 834)
dist.fun = approxfun(density(stemsha_L_raster_df$stemsha_L))

psych::describe(faib_psp$stemsha_L)
hist(faib_psp$stemsha_L)
faib_psp = faib_psp %>% 
  slice_sample(n=1186, weight_by = dist.fun(faib_psp$stemsha_L), replace = T)
psych::describe(faib_psp$stemsha_L)
hist(faib_psp$stemsha_L)

faib_psp$elev = as.numeric(faib_psp$elev)
faib_psp$slope = as.numeric(faib_psp$slope)
faib_psp$aspect = as.numeric(faib_psp$aspect)
faib_psp$lead_htop = as.numeric(faib_psp$lead_htop)
faib_psp$species_class = as.numeric(faib_psp$species_class)
faib_psp$stemsha_L = as.numeric(faib_psp$stemsha_L)
faib_psp$wsvha_L = as.numeric(faib_psp$wsvha_L)

faib_psp$elev[faib_psp$elev <= 0] = NA
faib_psp$slope[faib_psp$slope <= 0] = NA
faib_psp$aspect[faib_psp$aspect <= 0] = NA
faib_psp$asp_cos = cos((faib_psp$aspect * pi) / 180)
faib_psp$asp_sin = sin((faib_psp$aspect * pi) / 180)
faib_psp$lead_htop[faib_psp$lead_htop < 2] = NA
faib_psp$stemsha_L[faib_psp$stemsha_L <= 0] = NA
#faib_psp$species_class[faib_psp$species_class <= 0] = NA
faib_psp$wsvha_L[faib_psp$wsvha_L <= 0] = NA

#faib_psp = faib_psp[c("elev", "slope", "aspect", "lead_htop", "species_class", "stemsha_L", "wsvha_L", "baha_L", )]
faib_vri_true_m1_df = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "stemsha_L", "wsvha_L")]
faib_vri_true_m2_df = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "wsvha_L")] 
faib_vri_true_m1_df = na.omit(faib_vri_true_m1_df)
faib_vri_true_m2_df = na.omit(faib_vri_true_m2_df)
sum(is.na(faib_vri_true_m1_df))
sum(is.na(faib_vri_true_m2_df))
psych::describe(faib_vri_true_m1_df)
psych::describe(faib_vri_true_m2_df)

n <- nrow(faib_vri_true_m1_df)
frac <- 0.8
ix <- sample(n, frac * n)
train_m1 = faib_vri_true_m1_df[ix,]
test_m1 = faib_vri_true_m1_df[-ix,]
train_m2 = faib_vri_true_m2_df[ix,]
test_m2 = faib_vri_true_m2_df[-ix,]

X_train_m1=train_m1[,-8]
X_test_m1=test_m1[,-8]
y_train_m1=train_m1[,8]
y_test_m1=test_m1[,8]

X_train_m2=train_m2[,-7]
X_test_m2=test_m2[,-7]
y_train_m2=train_m2[,7]
y_test_m2=test_m2[,7]

X_m1 = faib_vri_true_m1_df[,-8]
y_m1 = faib_vri_true_m1_df[,8]
X_m2 = faib_vri_true_m2_df[,-7]
y_m2 = faib_vri_true_m2_df[,7]

names(covs_m1)
names(covs_m2)
names(X_m1)
names(X_m2)
names(faib_vri_true_m1_df)
names(faib_vri_true_m1_df)

hist(faib_vri_true_m1_df$stemsha_L)
hist(train_m1$stemsha_L)
hist(test_m1$stemsha_L)

# compare rasters with faib permanent sample plot data
library(MASS)
graphics.off()
par(mfrow=c(4,4))
#truehist(faib_vri_true_m1_df$wsvha_L, main="WSVHA (faib)", maxpixels=22000000)
#hist(tuneResult_GLM_m1_logLink_gamma_to_raster, main="WSVHA (all sites)")
#hist(tuneResult_GLM_m1_logLink_gamma_to_raster_gaspard, main="WSVHA (Gaspard)")
#hist(tuneResult_GLM_m1_logLink_gamma_to_raster_quesnel, main="WSVHA (Quesnel)")

truehist(faib_vri_true_m1_df$elev, main="DEM (faib)", maxpixels=22000000)
hist(elev_raster, main="DEM (all sites)", maxpixels=22000000)
hist(elev_raster_gaspard, main="DEM (Gaspard)", maxpixels=22000000)
hist(elev_raster_quesnel, main="DEM (Quesnel)", maxpixels=22000000)

truehist(faib_vri_true_m1_df$slope, main="Slope (faib)", maxpixels=22000000)
hist(slope_raster, main="Slope (all sites)", maxpixels=22000000) 
hist(slope_raster_gaspard, main="Slope (Gaspard)", maxpixels=22000000) 
hist(slope_raster_quesnel, main="Slope (Quesnel)", maxpixels=22000000) 

#truehist(faib_vri_true_m1_df$aspect, main="Aspect (faib)", maxpixels=22000000)
#hist(aspect_raster, main="Aspect (all sites)", maxpixels=22000000)
#hist(aspect_raster_gaspard, main="Aspect (Gaspard)", maxpixels=22000000)
#hist(aspect_rast_quesnel, main="Aspect (Quesnel)", maxpixels=22000000)

truehist(faib_vri_true_m1_df$asp_cos, main="Northness (faib)", maxpixels=22000000)
hist(asp_cos_raster, main="Northness (all sites)", maxpixels=22000000)
hist(asp_cos_raster_gaspard, main="Northness (Gaspard)", maxpixels=22000000)
hist(asp_cos_rast_quesnel, main="Northness (Quesnel)", maxpixels=22000000)

truehist(faib_vri_true_m1_df$asp_cos, main="Eastness (faib)", maxpixels=22000000)
hist(asp_cos_raster, main="Eastness (all sites)", maxpixels=22000000)
hist(asp_cos_raster_gaspard, main="Eastness (Gaspard)", maxpixels=22000000)
hist(asp_cos_rast_quesnel, main="Eastness (Quesnel)", maxpixels=22000000)

truehist(faib_vri_true_m1_df$stemsha_L, main="Stems/ha (faib)", maxpixels=22000000)
hist(stemsha_L_raster, main="Stems/ha (all sites)", maxpixels=22000000)
hist(stemsha_L_raster_gaspard, main="Stems/ha (Gaspard)", maxpixels=22000000)
hist(stemsha_L_raster_quesnel, main="Stems/ha (Quesnel)", maxpixels=22000000)

faib_vri_true_m1_df$species_class = as.numeric(faib_vri_true_m1_df$species_class)
truehist(faib_vri_true_m1_df$species_class, main="Lead Species (faib)", maxpixels=22000000)
hist(species_class_raster, main="Lead Species (all sites)", maxpixels=22000000)
hist(species_class_raster_gaspard, main="Lead Species (Gaspard)", maxpixels=22000000)
hist(species_class_raster_quesnel, main="Lead Species (Quesnel)", maxpixels=22000000)

truehist(faib_vri_true_m1_df$lead_htop, main="CHM 95th% (faib)", maxpixels=22000000)
hist(lead_htop_raster, main="CHM 95th% (all sites)", maxpixels=22000000) 
hist(lead_htop_raster_gaspard, main="CHM 95th% (Gaspard)", maxpixels=22000000) 
hist(lead_htop_raster_quesnel, main="CHM 95th% (Quesnel)", maxpixels=22000000) 

#plot residual trends
elev_wsvha_lm = lm(wsvha_L ~ elev, data = faib_vri_true_m1_df)
slope_wsvha_lm = lm(wsvha_L ~ slope, data = faib_vri_true_m1_df)
asp_cos_wsvha_lm = lm(wsvha_L ~ asp_cos, data = faib_vri_true_m1_df)
asp_sin_wsvha_lm = lm(wsvha_L ~ asp_sin, data = faib_vri_true_m1_df)
lead_htop_wsvha_lm = lm(wsvha_L ~ lead_htop, data = faib_vri_true_m1_df)
species_class_wsvha_lm = lm(wsvha_L ~ species_class, data = faib_vri_true_m1_df)
stemsha_L_wsvha_lm = lm(wsvha_L ~ stemsha_L, data = faib_vri_true_m1_df)

par(mfrow = c(4, 4)) 
summary(elev_wsvha_lm)
truehist(faib_vri_true_m1_df$elev)
truehist(elev_wsvha_lm$residuals)
plot(wsvha_L ~ elev, data = faib_vri_true_m1_df,
     main="Linear function showing negative correlation:\nR^2=0.011, ρ=-0.0954, p<0.0000",
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.8, cex.axis=0.8, adj=1,
     ylab = "wsvha_L (m3/ha)", xlab = "DEM")
abline(elev_wsvha_lm, col = "red")
plot(elev_wsvha_lm, which=1, 
     main="Residuals showing increasing trend\n clustering at larger fitted values", 
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1) # Residuals vs Fitted Plot
summary(slope_wsvha_lm)
truehist(faib_vri_true_m1_df$slope)
truehist(slope_wsvha_lm$residuals)
plot(wsvha_L ~ slope, data = faib_vri_true_m1_df,
     main="Linear function showing negative correlation:\nR^2=0.0013, ρ=-0.5171: p=0.0009", 
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.8, cex.axis=0.8, adj=1,
     ylab = "wsvha_L (m3/ha)", xlab = "slope")
abline(slope_wsvha_lm, col = "red")
plot(slope_wsvha_lm, which=1, 
     main="Residuals showing increasing trend of\nnegative errors near larger fitted values", 
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1) 
summary(aspect_wsvha_lm)
#truehist(faib_vri_true_m1_df$aspect)
#truehist(aspect_wsvha_lm$residuals)
#plot(wsvha_L ~ aspect, data = faib_vri_true_m1_df,
  #   main="Linear function showing positive correlation:\nR^2=0.005, ρ=15.197, p<0.000",
   #  col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.8, cex.axis=0.8, adj=1,
    # ylab = "wsvha_L (m3/ha)", xlab = "asp_cos")
#abline(aspect_wsvha_lm, col = "red")
summary(asp_cos_wsvha_lm)
truehist(faib_vri_true_m1_df$asp_cos)
truehist(asp_cos_wsvha_lm$residuals)
plot(wsvha_L ~ asp_cos, data = faib_vri_true_m1_df,
     main="Linear function showing positive correlation:\nR^2=0.005, ρ=15.197, p<0.000",
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.8, cex.axis=0.8, adj=1,
     ylab = "wsvha_L (m3/ha)", xlab = "asp_cos")
abline(asp_cos_wsvha_lm, col = "red")
plot(asp_cos_wsvha_lm, which=1, 
     main="Residuals showing almost constant variance", 
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1) 

summary(asp_sin_wsvha_lm)
truehist(faib_vri_true_m1_df$asp_sin)
truehist(asp_sin_wsvha_lm$residuals)
plot(wsvha_L ~ asp_sin, data = faib_vri_true_m1_df,
     main="Linear function showing positive correlation:\nR^2=0.005, ρ=15.197, p<0.000",
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.8, cex.axis=0.8, adj=1,
     ylab = "wsvha_L (m3/ha)", xlab = "asp_sin")
abline(asp_sin_wsvha_lm, col = "red")
plot(asp_sin_wsvha_lm, which=1, 
     main="Residuals showing almost constant variance", 
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1) 

summary(lead_htop_wsvha_lm)
truehist(faib_vri_true_m1_df$lead_htop)
truehist(lead_htop_wsvha_lm$residuals)
plot(wsvha_L ~ lead_htop, data = faib_vri_true_m1_df,
     main="Linear function shows positive correlation:\n R^2=0.6508, ρ=20.6829, p<0.0000",
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.8, cex.axis=0.8, adj=1,
     ylab = "wsvha_L (m3/ha)", xlab = "lead_htop")
abline(lead_htop_wsvha_lm, col = "red")
plot(lead_htop_wsvha_lm, which=1, 
     main="Residuals showing non-constant variance with\n increasing trends at smallest and largest fitted values", 
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1) 
summary(stemsha_L_wsvha_lm)
truehist(faib_vri_true_m1_df$stemsha_L)
truehist(stemsha_L_wsvha_lm$residuals)
plot(wsvha_L ~ stemsha_L, data = faib_vri_true_m1_df,
     main="No significant relationship with response variable:\nR^2=0.0001, ρ=-0.0008, p<0.4743",
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1,
     ylab = "wsvha_L (m3/ha)", xlab = "stemsha_L")
abline(stemsha_L_wsvha_lm, col = "red")
plot(stemsha_L_wsvha_lm, which=1, 
     main="Residuals showing non-constant variance with negative\nand positive errors clusters at larger fitted values", 
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1) 

wsvha_L_lm = lm(wsvha_L ~ ., data = faib_vri_true_m1_df)
truehist(faib_vri_true_m1_df$wsvha_L)
truehist(wsvha_L_lm$residuals)
plot(wsvha_L_lm$residuals,
     main="",
     col="blue", pch=20, cex=0.8, cex.main=0.8, cex.lab=0.8, cex.axis=0.8, adj=1,
     ylab = "wsvha_L (m3/ha)", xlab = "predictors")     
abline(wsvha_L_lm, col="red")

#summary(baha_L_wsvha_lm)
#truehist(faib_vri_true_m1_df$baha_L)
#truehist(baha_L_wsvha_lm$residuals)
#plot(wsvha_L ~ baha_L, data = faib_vri_true_m1_df,
 #    main="Linear function shows signficantly positive correlation:\nR^2=0.8396, ρ=9.9781, p<0.0000",
  #   col="blue", pch=20, cex=0.8, cex.main=0.8, cex.lab=0.8, cex.axis=0.8, adj=1,
   #  ylab = "wsvha_L (m3/ha)", xlab = "baha_L")
#abline(baha_L_wsvha_lm, col = "red")
#plot(baha_L_wsvha_lm, which=1, 
 #    main="Residuals showing increasing trend\n of variances towards larger fitted values", 
  #   col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1) 
summary(species_class_wsvha_lm)
faib_vri_true_m1_df$species_class = as.numeric(faib_vri_true_m1_df$species_class)
truehist(faib_vri_true_m1_df$species_class)
truehist(species_class_wsvha_lm$residuals)
plot(species_class_wsvha_lm, which=1, 
     main="Residuals showing decreasing trend with True-fir group:\nR^2=0.0025, ρ=6.584, p<0.000", 
     col="blue", pch=20, cex=0.5, cex.main=0.6, cex.lab=0.5, cex.axis=0.5, adj=1) 
car::residualPlots(elev_wsvha_lm, terms= ~ 1 | species_class, cex=0.1, pch=19) # plot vs. yhat grouping by type


######## Models #########

#svmRadial with epsilon
fitControl_YeoJx1 = caret::trainControl(method="repeatedcv", number=10, repeats=1)
fitControl_YeoJx3 = caret::trainControl(method="repeatedcv", number=10, repeats=3)
fitControl_10x5 = caret::trainControl(method="repeatedcv", number=10, repeats=5)
fitControl_10x10 = caret::trainControl(method="repeatedcv", number=10, repeats=10)

tuneResult_svm_m1_full_eps <- train(
  X_m1, y_m1, 
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)), tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m2_full_eps = train(
  X_m2, y_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full_eps <- tuneResult_svm_m2_full_eps$finalModel
tunedModel_svm_m1_full_eps <- tuneResult_svm_m1_full_eps$finalModel
tunedModel_svm_m2_to_raster_eps <- raster::predict(covs_m2, tuneResult_svm_m2_full_eps)
tunedModel_svm_m1_to_raster_eps <- raster::predict(covs_m1, tuneResult_svm_m1_full_eps)
save(tunedModel_svm_m2_full_eps, file = "./models/tunedModel_svmRadial_m2_10k_eps.RData")
save(tunedModel_svm_m1_full_eps, file = "./models/tunedModel_svmRadial_m1_10k_eps.RData")
writeRaster(tunedModel_svm_m2_to_raster_eps, filename = "./results/model1_svmRadial_100m_eps.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_eps, filename = "./results/model2_svmRadial_100m_eps.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full_eps)
print(tunedModel_svm_m1_full_eps)
graphics.off()
par(mfrow = c(2,2))
model1_svmRadial_100cell_eps = raster::raster("./results/model1_svmRadial_100m_eps.tif")
model2_svmRadial_100cell_eps = raster::raster("./results/model2_svmRadial_100m_eps.tif")
plot(model1_svmRadial_100cell_eps, main="Model1 SVM-radial-eps NO-STEMS", cex.main=0.9)
hist(model1_svmRadial_100cell_eps, main="Model1 SVM-radial-eps NO-STEMS", cex.main=0.8, maxpixels=22000000) 
plot(model2_svmRadial_100cell_eps, main="Model2 SVM-radial-eps WITH-STEMS", cex.main=0.8)
hist(model2_svmRadial_100cell_eps, main="Model2 SVM-radial-eps WITH-STEMS", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell_eps, main="Model1 NO-STEMS")
rasterVis::densityplot(model2_svmRadial_100cell_eps, main="Model2 WITH-STEMS")

tuneResult_svmRadial_m2_10k_train_eps <- train(
  X_train_m2, y_train_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svmRadial_m1_10k_train_eps <- train(
  X_train_m1, y_train_m1,
  trControl = fitControl_10x10,
  method = 'svmRadial',metric = 'RMSE',
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svmRadial_m2_test_eps = predict(tuneResult_svmRadial_m2_10k_train_eps, data = test_m2)
tunedModel_svmRadial_m1_test_eps = predict(tuneResult_svmRadial_m1_10k_train_eps, data = test_m1)
tunedModel_svmRadial_m2_test_MAE_eps = MAE(tunedModel_svmRadial_m2_test_eps, test_m2$wsvha_L)
tunedModel_svmRadial_m1_test_MAE_eps = MAE(tunedModel_svmRadial_m1_test_eps, test_m1$wsvha_L)
tunedModel_svmRadial_m2_test_RMSE_eps = RMSE(tunedModel_svmRadial_m2_test_eps, test_m2$wsvha_L)
tunedModel_svmRadial_m1_test_RMSE_eps = RMSE(tunedModel_svmRadial_m1_test_eps, test_m1$wsvha_L)

tunedModel_svmRadial_m2_eps = predict(tuneResult_svm_m2_full_eps, data = faib_vri_true_m2_df)
tunedModel_svmRadial_m1_eps = predict(tuneResult_svm_m1_full_eps, data = faib_vri_true_m1_df)
tunedModel_svmRadial_m2_MAE_eps = MAE(tunedModel_svmRadial_m2_eps, faib_vri_true_m2_df$wsvha_L)
tunedModel_svmRadial_m1_MAE_eps = MAE(tunedModel_svmRadial_m1_eps, faib_vri_true_m1_df$wsvha_L)
tunedModel_svmRadial_m2_RMSE_eps = RMSE(tunedModel_svmRadial_m2_eps, faib_vri_true_m2_df$wsvha_L)
tunedModel_svmRadial_m1_RMSE_eps = RMSE(tunedModel_svmRadial_m1_eps, faib_vri_true_m1_df$wsvha_L)

tunedModel_svmRadial_m2_MAE_eps
tunedModel_svmRadial_m2_RMSE_eps 
tunedModel_svmRadial_m2_test_MAE_eps
tunedModel_svmRadial_m2_test_RMSE_eps
tunedModel_svmRadial_m2_RMSE_eps/tunedModel_svmRadial_m2_test_RMSE_eps

tunedModel_svmRadial_m1_MAE_eps
tunedModel_svmRadial_m1_RMSE_eps 
tunedModel_svmRadial_m1_test_MAE_eps
tunedModel_svmRadial_m1_test_RMSE_eps
tunedModel_svmRadial_m1_RMSE_eps/tunedModel_svmRadial_m1_test_RMSE_eps



# svmRadial epsilon-free
tuneResult_svm_m2_full <- train(
  X_m2, y_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m1_full <- train(
  X_m1, y_m1,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)), tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full <- tuneResult_svm_m2_full$finalModel
tunedModel_svm_m1_full <- tuneResult_svm_m1_full$finalModel
tunedModel_svm_m2_to_raster <- raster::predict(covs_m2, tuneResult_svm_m2_full)
tunedModel_svm_m1_to_raster <- raster::predict(covs_m1, tuneResult_svm_m1_full)
save(tunedModel_svm_m2_full, file = "./models/tunedModel_svmRadial_m2_10k.RData")
save(tunedModel_svm_m1_full, file = "./models/tunedModel_svmRadial_m1_10k.RData")
writeRaster(tunedModel_svm_m2_to_raster, filename = "./results/model1_svmRadial_100m.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster, filename = "./results/model2_svmRadial_100m.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full)
print(tunedModel_svm_m1_full)


par(mfrow = c(2,2))
model1_svmRadial_100cell = raster::raster("./results/model1_svmRadial_100m.tif")
model2_svmRadial_100cell = raster::raster("./results/model2_svmRadial_100m.tif")
plot(model1_svmRadial_100cell, main="Model1 NO-STEMS (SVMRad Eps-Free)", cex.main=0.9)
hist(model1_svmRadial_100cell, main="Model1 NO-STEMS (SVMRad Eps-Free)", cex.main=0.8, maxpixels=22000000) 
plot(model2_svmRadial_100cell, main="Model2 WITH-STEMS (SVMRad Eps-Free)", cex.main=0.8)
hist(model2_svmRadial_100cell, main="Model2 WITH-STEMS (SVMRad Eps-Free)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell, main="Model1 NO-STEMS (SVMRad Eps-Free)")
rasterVis::densityplot(model2_svmRadial_100cell, main="Model2 WITH-STEMS (SVMRad Eps-Free)")

tuneResult_svmRadial_m2_10k_train <- train(
  X_train_m2, y_train_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  ranges = list(
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svmRadial_m1_10k_train <- train(
  X_train_m1, y_train_m1,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  ranges = list(
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svmRadial_m2_test = predict(tuneResult_svmRadial_m2_10k_train, data = test_m2)
tunedModel_svmRadial_m1_test = predict(tuneResult_svmRadial_m1_10k_train, data = test_m1)
tunedModel_svmRadial_m2_test_MAE = MAE(tunedModel_svmRadial_m2_test, test_m2$wsvha_L)
tunedModel_svmRadial_m1_test_MAE = MAE(tunedModel_svmRadial_m1_test, test_m1$wsvha_L)
tunedModel_svmRadial_m2_test_RMSE = RMSE(tunedModel_svmRadial_m2_test, test_m2$wsvha_L)
tunedModel_svmRadial_m1_test_RMSE = RMSE(tunedModel_svmRadial_m1_test, test_m1$wsvha_L)

tunedModel_svmRadial_m2 = predict(tuneResult_svm_m2_full, data = faib_vri_true_m2_df)
tunedModel_svmRadial_m1 = predict(tuneResult_svm_m1_full, data = faib_vri_true_m1_df)
tunedModel_svmRadial_m2_MAE = MAE(tunedModel_svmRadial_m2, faib_vri_true_m2_df$wsvha_L)
tunedModel_svmRadial_m1_MAE = MAE(tunedModel_svmRadial_m1, faib_vri_true_m1_df$wsvha_L)
tunedModel_svmRadial_m2_RMSE = RMSE(tunedModel_svmRadial_m2, faib_vri_true_m2_df$wsvha_L)
tunedModel_svmRadial_m1_RMSE = RMSE(tunedModel_svmRadial_m1, faib_vri_true_m1_df$wsvha_L)

tunedModel_svmRadial_m2_MAE
tunedModel_svmRadial_m2_RMSE 
tunedModel_svmRadial_m2_test_MAE
tunedModel_svmRadial_m2_test_RMSE
tunedModel_svmRadial_m2_RMSE/tunedModel_svmRadial_m2_test_RMSE

tunedModel_svmRadial_m1_MAE
tunedModel_svmRadial_m1_RMSE 
tunedModel_svmRadial_m1_test_MAE
tunedModel_svmRadial_m1_test_RMSE
tunedModel_svmRadial_m1_RMSE/tunedModel_svmRadial_m1_test_RMSE



# svmLinear Eps-Free
tuneResult_svm_m2_full_linear <- train(
  X_m2, y_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear',metric = 'RMSE', 
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m1_full_linear <- train(
  X_m1, y_m1,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full_linear <- tuneResult_svm_m2_full_linear$finalModel
tunedModel_svm_m1_full_linear <- tuneResult_svm_m1_full_linear$finalModel
tunedModel_svm_m2_to_raster_linear <- raster::predict(covs_m2, tuneResult_svm_m2_full_linear)
tunedModel_svm_m1_to_raster_linear <- raster::predict(covs_m1, tuneResult_svm_m1_full_linear)
save(tunedModel_svm_m2_full_linear, file = "./models/tunedModel_svmRadial_m2_10k_linear.RData")
save(tunedModel_svm_m1_full_linear, file = "./models/tunedModel_svmRadial_m1_10k_linear.RData")
tunedModel_svm_m2_to_raster_linear$layer[tunedModel_svm_m2_to_raster_linear$layer < 0] = NA
tunedModel_svm_m1_to_raster_linear$layer[tunedModel_svm_m1_to_raster_linear$layer < 0] = NA
writeRaster(tunedModel_svm_m2_to_raster_linear, filename = "./results/model1_svmRadial_100m_linear.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_linear, filename = "./results/model2_svmRadial_100m_linear.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full_linear)
print(tunedModel_svm_m1_full_linear)

graphics.off()
par(mfrow = c(2,1))
model1_svmLinear_100cell = raster::raster("./results/model1_svmRadial_100m_linear.tif")
model2_svmLinear_100cell = raster::raster("./results/model2_svmRadial_100m_linear.tif")
plot(model1_svmLinear_100cell, main="Model1 NO-STEMS (SVMLinear)", cex.main=0.9)
hist(model1_svmLinear_100cell, main="Model1 NO-STEMS (SVMLinear)", cex.main=0.8, maxpixels=22000000) 
plot(model2_svmLinear_100cell, main="Model2 WITH-STEMS (SVMLinear)", cex.main=0.8)
hist(model2_svmLinear_100cell, main="Model2 WITH-STEMS (SVMLinear)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmLinear_100cell, main="Model1 NO-STEMS (SVMLinear)")
rasterVis::densityplot(model2_svmLinear_100cell, main="Model2 WITH-STEMS (SVMLinear)")

tuneResult_svmLinear_m2_10k_train <- train(
  X_train_m2, y_train_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svmLinear_m1_10k_train <- train(
  X_train_m1, y_train_m1,
  trControl = fitControl_YeoJx1,
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


# glmGamma-caret
tuneResult_glm_m2_full_gamma <- train(
  X_m2, y_m2,
  trControl = fitControl_YeoJx1,
  method = 'glm',metric = 'RMSE', 
  ranges = list(cost = c(1,5,7,15,20), gamma = 5^(-1:1)), tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m1_full_linear <- train(
  X_m1, y_m1,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 5^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full_linear <- tuneResult_svm_m2_full_linear$finalModel
tunedModel_svm_m1_full_linear <- tuneResult_svm_m1_full_linear$finalModel
tunedModel_svm_m2_to_raster_linear <- raster::predict(covs_m2, tuneResult_svm_m2_full_linear)
tunedModel_svm_m1_to_raster_linear <- raster::predict(covs_m1, tuneResult_svm_m1_full_linear)
save(tunedModel_svm_m2_full_linear, file = "./models/tunedModel_svmRadial_m2_10k_linear.RData")
save(tunedModel_svm_m1_full_linear, file = "./models/tunedModel_svmRadial_m1_10k_linear.RData")
tunedModel_svm_m2_to_raster_linear$layer[tunedModel_svm_m2_to_raster_linear$layer < 0] = NA
tunedModel_svm_m1_to_raster_linear$layer[tunedModel_svm_m1_to_raster_linear$layer < 0] = NA
writeRaster(tunedModel_svm_m2_to_raster_linear, filename = "./results/model1_svmRadial_100m_linear.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_linear, filename = "./results/model2_svmRadial_100m_linear.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full_linear)
print(tunedModel_svm_m1_full_linear)

graphics.off()
par(mfrow = c(2,1))
model1_svmLinear_100cell = raster::raster("./results/model1_svmRadial_100m_linear.tif")
model2_svmLinear_100cell = raster::raster("./results/model2_svmRadial_100m_linear.tif")
plot(model1_svmLinear_100cell, main="Model1 NO-STEMS (SVMLinear)", cex.main=0.9)
hist(model1_svmLinear_100cell, main="Model1 NO-STEMS (SVMLinear)", cex.main=0.8, maxpixels=22000000) 
plot(model2_svmLinear_100cell, main="Model2 WITH-STEMS (SVMLinear)", cex.main=0.8)
hist(model2_svmLinear_100cell, main="Model2 WITH-STEMS (SVMLinear)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmLinear_100cell, main="Model1 NO-STEMS (SVMLinear)")
rasterVis::densityplot(model2_svmLinear_100cell, main="Model2 WITH-STEMS (SVMLinear)")

tuneResult_svmLinear_m2_10k_train <- train(
  X_train_m2, y_train_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear', metric = 'RMSE',
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svmLinear_m1_10k_train <- train(
  X_train_m1, y_train_m1,
  trControl = fitControl_YeoJx1,
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


# random forest
tuneResult_rfCaret_m2_10k_full <- train(
  X_m2, y_m2,
  trControl = fitControl_YeoJx1,
  method = 'rf', metric = 'RMSE',
  tuneLength = 10, 
  preProcess = c("BoxCox",'center', 'scale'))

tuneResult_rfCaret_m2_10k_train <- train(
  X_train_m2,y_train_m2,
  trControl = fitControl_YeoJx1,
  method = 'rf', metric = 'RMSE',
  tuneLength = 10,
  preProcess = c("BoxCox",'center', 'scale'))

tunedModel_rfCaret_m2 = predict(tuneResult_rfCaret_m2_10k_full, data = faib_vri_true_m2_df)
tunedModel_rfCaret_m2_MAE = MAE(tunedModel_rfCaret_m2, faib_vri_true_m2_df$wsvha_L)
tunedModel_rfCaret_m2_RMSE = RMSE(tunedModel_rfCaret_m2, faib_vri_true_m2_df$wsvha_L)
tunedModel_rfCaret_m2_test = predict(tuneResult_rfCaret_m2_10k_train, data = test_m2)
tunedModel_rfCaret_m2_test_MAE = MAE(tunedModel_rfCaret_m2_test, test_m2$wsvha_L)
tunedModel_rfCaret_m2_test_RMSE = RMSE(tunedModel_rfCaret_m2_test, test_m2$wsvha_L)

tunedModel_rfCaret_m2_MAE
tunedModel_rfCaret_m2_RMSE
tunedModel_rfCaret_m2_test_MAE
tunedModel_rfCaret_m2_test_RMSE
tunedModel_rfCaret_m2_RMSE/tunedModel_rfCaret_m2_test_RMSE

trellis.par.set(caretTheme())
tuneResult_rfCaret_m2_10k_full
tuneResult_rfCaret_m2_10k_full$finalModel
plot(tuneResult_rfCaret_m2_10k_full)
lattice::densityplot(tuneResult_rfCaret_m2_10k_full)
tunedModel_rfCaret_m2_10k <- tuneResult_rfCaret_m2_10k_full$finalModel
save(tunedModel_rfCaret_m2_10k, file = "./results/tunedModel_rfCaret_m2_10k.RData")
print(tunedModel_rfCaret_m2_10k)
tunedModel_rfCaret_m2_10k_to_raster <- predict(covs_m2, tunedModel_rfCaret_m2_10k)
writeRaster(tunedModel_rfCaret_m2_10k_to_raster, filename = "./results/tunedModel_rfCaret_m2_10k_to_raster.tif", overwrite=TRUE)
plot(tunedModel_rfCaret_m2_10k_to_raster, main="Estimated Whole Stem Volume Gaspard OA (m3/ha)")



# caretEnsemble 
model_list_m2_10k <- caretList(
  X_m2, y_m2,
  trControl = fitControl_YeoJx1,
  methodList = c('glm', 'svmLinear', 'svmRadial', 'rf'),
  tuneLength=10,
  continue_on_fail = FALSE, 
  preProcess = c('BoxCox', 'center','scale'))

model_list_m1_10k <- caretList(
  X_m1, y_m1,
  trControl = fitControl_YeoJx1,
  methodList = c('glm', 'svmLinear', 'svmRadial', 'rf'),
  tuneLength=10,
  continue_on_fail = FALSE, 
  preProcess = c('BoxCox', 'center','scale'))

model_results_m2_10k <- data.frame(
  GLM = min(model_list_m2_10k$glm$results$RMSE),
  SVMLINEAR = min(model_list_m2_10k$svmLinear$results$RMSE),
  SVMRADIAL = min(model_list_m2_10k$svmRadial$results$RMSE),
  RF = min(model_list_m2_10k$rf$results$RMSE))

model_results_m1_10k <- data.frame(
  GLM = min(model_list_m1_10k$glm$results$RMSE),
  SVMLINEAR = min(model_list_m1_10k$svmLinear$results$RMSE),
  SVMRADIAL = min(model_list_m1_10k$svmRadial$results$RMSE),
  RF = min(model_list_m1_10k$rf$results$RMSE))

resamples_m2_10k = resamples(model_list_m2_10k)
dotplot(resamples_m2_10k, metric = 'RMSE')
summary(resamples_m2_10k)

resamples_m1_10k = resamples(model_list_m1_10k)
dotplot(resamples_m1_10k, metric = 'RMSE')
summary(resamples_m1_10k)

stack_m2_10k = caretStack(
  model_list_m2_10k, 
  method = 'glmnet', metric = 'RMSE', 
  trControl = trainControl(method = "repeatedcv", savePredictions = "final", summaryFunction=defaultSummary),
  tuneLength=6)
plot(stack_m2_10k)
print(stack_m2_10k)
stack_m2_10k_model = stack_m2_10k$model
stack_m2_10k_model

stack_m2_10k_result = predict(stack_m2_10k_model, data = faib_vri_true_m2_df)
stack_m2_10k_result_MAE = MAE(stack_m2_10k_result, faib_vri_true_m2_df$wsvha_L)
stack_m2_10k_result_RMSE = RMSE(stack_m2_10k_result, faib_vri_true_m2_df$wsvha_L)

caretensemble_m2_10k_to_raster <- predict(covs_m2, stack_m2_10k)
writeRaster(caretensemble_m2_10k_to_raster, filename = "./results/caretensemble_m2_10k_to_raster.tif", overwrite=TRUE)
plot(caretensemble_m2_10k_to_raster)

par(mfrow = c(2,1))
model1_ensemble = raster::raster("./results/model1_svmRadial_100m_linear.tif")
model2_ensemble = raster::raster("./results/model2_svmRadial_100m_linear.tif")
plot(model1_ensemble, main="Model1 NO-STEMS (Ensemble Net)", cex.main=0.9)
hist(model1_ensemble, main="Model1 NO-STEMS (Ensemble Net)", cex.main=0.8, maxpixels=22000000) 
plot(model2_ensemble, main="Model2 WITH-STEMS (Ensemble Net)", cex.main=0.8)
hist(model2_ensemble, main="Model2 WITH-STEMS (Ensemble Net)", cex.main=0.8, maxpixels=22000000) 



# glm Gamma & Gaussian fit
tuneResult_GLM_m2_logLink_gamma <- glm(
  formula = wsvha_L ~ lead_htop + elev + slope + asp_cos + asp_sin + species_class, 
  family = Gamma(link = "log"),
  data=faib_vri_true_m2_df)

tuneResult_GLM_m1_logLink_gamma <- glm(
  formula = wsvha_L ~ lead_htop + stemsha_L + elev + slope + asp_cos + asp_sin + species_class, 
  family = Gamma(link = "log"),
  data=faib_vri_true_m1_df)


tuneResult_GLM_m2_logLink_gamma_train <- glm(
  formula = wsvha_L ~ lead_htop + elev + slope + asp_cos + asp_sin + species_class, 
  family = Gamma(link = "log"),
  data=train_m2)

tuneResult_GLM_m1_logLink_gamma_train <- glm(
  formula = wsvha_L ~ lead_htop + stemsha_L + elev + slope + asp_cos + asp_sin + species_class, 
  family = Gamma(link = "log"),
  data=train_m1)

tunedModel_GLM_m1_logLink_gamma = predict(tuneResult_GLM_m1_logLink_gamma, data = faib_vri_true_m2_df)
tunedModel_GLM_m1_logLink_gamma_MAE = MAE(tunedModel_GLM_m1_logLink_gamma, faib_vri_true_m2_df$wsvha_L)
tunedModel_GLM_m1_logLink_gamma_RMSE = RMSE(tunedModel_GLM_m1_logLink_gamma, faib_vri_true_m2_df$wsvha_L)
tunedModel_GLM_m1_logLink_gamma_test = predict(tuneResult_GLM_m1_logLink_gamma, data = test_m1)
tunedModel_GLM_m1_logLink_gamma_test_MAE = MAE(tunedModel_GLM_m1_logLink_gamma_test, test_m1$wsvha_L)
tunedModel_GLM_m1_logLink_gamma_test_RMSE = RMSE(tunedModel_GLM_m1_logLink_gamma_test, test_m1$wsvha_L)

tunedModel_GLM_m2_logLink_gamma = predict(tuneResult_GLM_m2_logLink_gamma, data = faib_vri_true_m2_df)
tunedModel_GLM_m2_logLink_gamma_MAE = MAE(tunedModel_GLM_m2_logLink_gamma, faib_vri_true_m2_df$wsvha_L)
tunedModel_GLM_m2_logLink_gamma_RMSE = RMSE(tunedModel_GLM_m2_logLink_gamma, faib_vri_true_m2_df$wsvha_L)
tunedModel_GLM_m2_logLink_gamma_test = predict(tuneResult_GLM_m2_logLink_gamma, data = test_m2)
tunedModel_GLM_m2_logLink_gamma_test_MAE = MAE(tunedModel_GLM_m2_logLink_gamma_test, test_m2$wsvha_L)
tunedModel_GLM_m2_logLink_gamma_test_RMSE = RMSE(tunedModel_GLM_m2_logLink_gamma_test, test_m2$wsvha_L)

tuneResult_GLM_m1_logLink_gamma
MAE(tunedModel_GLM_m1_logLink_gamma, faib_vri_true_m1_df$wsvha_L)
RMSE(tunedModel_GLM_m1_logLink_gamma, faib_vri_true_m1_df$wsvha_L)
MAE(tunedModel_GLM_m1_logLink_gamma_test, test_m1$wsvha_L)
RMSE(tunedModel_GLM_m1_logLink_gamma_test, test_m1$wsvha_L)
tunedModel_GLM_m1_logLink_gamma_test_RMSE/tunedModel_GLM_m1_logLink_gamma_RMSE

tuneResult_GLM_m2_logLink_gamma
MAE(tunedModel_GLM_m2_logLink_gamma, faib_vri_true_m2_df$wsvha_L)
RMSE(tunedModel_GLM_m2_logLink_gamma, faib_vri_true_m2_df$wsvha_L)
MAE(tunedModel_GLM_m2_logLink_gamma_test, test_m2$wsvha_L)
RMSE(tunedModel_GLM_m2_logLink_gamma_test, test_m2$wsvha_L)
tunedModel_GLM_m2_logLink_gamma_test_RMSE/tunedModel_GLM_m2_logLink_gamma_RMSE


base::print(tuneResult_GLM_m1_logLink_gamma)
performance::model_performance(tuneResult_GLM_m1_logLink_gamma)
performance::performance_mae(tuneResult_GLM_m1_logLink_gamma)
performance::performance_rmse(tuneResult_GLM_m1_logLink_gamma)
save(tuneResult_GLM_m2_logLink_gamma, file = "./models/tuneResult_GLM_m2_logLink_gamma.RData")
save(tuneResult_GLM_m1_logLink_gamma, file = "./models/tuneResult_GLM_m1_logLink_gamma.RData")
tuneResult_GLM_m2_logLink_gamma_to_raster = raster::predict(covs_m2, tuneResult_GLM_m2_logLink_gamma, type="response")
tuneResult_GLM_m1_logLink_gamma_to_raster = raster::predict(covs_m1, tuneResult_GLM_m1_logLink_gamma, type="response")


writeRaster(tuneResult_GLM_m2_logLink_gamma_to_raster, filename = "./results/model2_glm_loglink_gamma.tif", overwrite=TRUE)
writeRaster(tuneResult_GLM_m1_logLink_gamma_to_raster, filename = "./results/model1_glm_loglink_gamma.tif", overwrite=TRUE)
model1_glm_loglink_gamma = raster::raster("./results/model1_glm_loglink_gamma.tif")
model2_glm_loglink_gamma = raster::raster("./results/model2_glm_loglink_gamma.tif")
plot(model1_glm_loglink_gamma, main="Model1 NO-STEMS (GLM-Gamma)", cex.main=0.9)
hist(model1_glm_loglink_gamma, main="Model1 NO-STEMS (GLM-Gamma)", cex.main=0.8, maxpixels=22000000) 
plot(model2_glm_loglink_gauss, main="Model2 WITH-STEMS (GLM-Gamma)", cex.main=0.8)
hist(model2_glm_loglink_gauss, main="Model2 WITH-STEMS (GLM-Gamma)", cex.main=0.8, maxpixels=22000000) 
tuneResult_GLM_m1_logLink_gamma_to_raster_gaspard = raster::predict(covs_m1_gaspard, tuneResult_GLM_m1_logLink_gamma)
tuneResult_GLM_m1_logLink_gamma_to_raster_quesnel = raster::predict(covs_m1_quesnel, tuneResult_GLM_m1_logLink_gamma)




# e1071 - SVM radial
tuneResult_svm_m2_full <- tune(
  svm, X_m2, y_m2,
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),
  preProcess = c("BoxCox","center","scale"))

tuneResult_svm_m1_full <- tune(
  svm, X_m1, y_m1,
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),
  preProcess = c("BoxCox","center","scale"))

tunedModel_svm_m2_full <- tuneResult_svm_m2_full$best.model
tunedModel_svm_m1_full <- tuneResult_svm_m1_full$best.model
print(summary(tunedModel_svm_m2_full))
print(summary(tunedModel_svm_m1_full))
save(tunedModel_svm_m2_full, file = "./results/tunedModel_svm_m2_full.RData")
save(tunedModel_svm_m1_full, file = "./results/tunedModel_svm_m1_full.RData")
tunedModel_svm_m2 = predict(tunedModel_svm_m2_full, data=faib_vri_true_m2_df)
tunedModel_svm_m1 = predict(tunedModel_svm_m1_full, data=faib_vri_true_m1_df)

tuneResult_svm_m2_train <- tune(
  svm, X_train_m2, y_train_m2,
  ranges = list(cost = c(5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),
  preProcess = c("BoxCox","center","scale"))

tuneResult_svm_m1_train <- tune(
  svm, X_train_m1, y_train_m1,
  ranges = list(cost = c(5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),
  preProcess = c("BoxCox","center","scale"))

tunedModel_svm_m2_train <- tuneResult_svm_m2_train$best.model
tunedModel_svm_m2_train <- tuneResult_svm_m2_train$best.model
tunedModel_svm_m1_test = predict(tunedModel_svm_m2_train, data=test_m1)
tunedModel_svm_m1_test = predict(tunedModel_svm_m2_train, data=test_m1)

tunedModel_svm_m2_test_MAE = MAE(tunedModel_svm_m2_test, y_test_m2)
tunedModel_svm_m2_test_RMSE = RMSE(tunedModel_svm_m2_test, y_test_m2)
tunedModel_svm_m2_full_MAE = MAE(tunedModel_svm_m2, y_m2)
tunedModel_svm_m2_full_RMSE = RMSE(tunedModel_svm_m2, y_m2)

tunedModel_svm_m1_test_MAE = MAE(tunedModel_svm_m1_test, y_test_m1)
tunedModel_svm_m1_test_RMSE = RMSE(tunedModel_svm_m1_test, y_test_m1)
tunedModel_svm_m1_full_MAE = MAE(tunedModel_svm_m1, y_m1)
tunedModel_svm_m1_full_RMSE = RMSE(tunedModel_svm_m1, y_m1)

tunedModel_svm_m2_full
R2(tunedModel_svm_m2, y_m2)
MAE(tunedModel_svm_m2, y_m2)
RMSE(tunedModel_svm_m2, y_m2)
MAE(tunedModel_svm_m2_test, y_test_m2)
RMSE(tunedModel_svm_m2_test, y_test_m2)
tunedModel_svm_m2_full_RMSE/tunedModel_svm_m2_test_RMSE

tunedModel_svm_m1_full
R2(tunedModel_svm_m1, y_m1)
MAE(tunedModel_svm_m1, y_m1)
RMSE(tunedModel_svm_m1, y_m1)
MAE(tunedModel_svm_m1_test, y_test_m1)
RMSE(tunedModel_svm_m1_test, y_test_m1)
tunedModel_svm_m1_full_RMSE/tunedModel_svm_m1_test_RMSE

tuneResult_svm_m2_full$performances
plot(tuneResult_svm_m2_full, type = "perspective", theta = 120, phi = 45)
tunedModel_svm_m2_to_raster <- predict(covs_m2, tunedModel_svm_m2_full)
writeRaster(tunedModel_svm_m2_to_raster, filename = "./results/tunedModel_svm_m2_to_raster.tif", overwrite=TRUE)


# e1071 - SVM radial & epsilon tuned
tuneResult_svm_m2_full_eps <- tune(
  svm, X_m2, y_m2,
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),
  preProcess = c("BoxCox","center","scale"))

tuneResult_svm_m1_full_eps <- tune(
  svm, X_m1, y_m1,
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),
  preProcess = c("BoxCox","center","scale"))

tunedModel_svm_m2_full_eps <- tuneResult_svm_m2_full_eps$best.model
tunedModel_svm_m1_full_eps <- tuneResult_svm_m1_full_eps$best.model
print(summary(tunedModel_svm_m2_full_eps))
print(summary(tunedModel_svm_m1_full_eps))
save(tunedModel_svm_m2_full_eps, file = "./results/tunedModel_svm_m2_full_eps.RData")
save(tunedModel_svm_m1_full_eps, file = "./results/tunedModel_svm_m1_full_eps.RData")
tunedModel_svm_m2_eps = predict(tunedModel_svm_m2_full_eps, data=faib_vri_true_m2_df)
tunedModel_svm_m1_eps = predict(tunedModel_svm_m1_full_eps, data=faib_vri_true_m1_df)

tuneResult_svm_m2_train_eps <- tune(
  svm, X_train_m2, y_train_m2,
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),
  preProcess = c("BoxCox","center","scale"))

tuneResult_svm_m1_train_eps <- tune(
  svm, X_train_m1, y_train_m1,
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),
  preProcess = c("BoxCox","center","scale"))

tunedModel_svm_m2_train_eps <- tuneResult_svm_m2_train_eps$best.model
tunedModel_svm_m1_train_eps <- tuneResult_svm_m1_train_eps$best.model
tunedModel_svm_m2_test_eps = predict(tunedModel_svm_m2_train_eps, data=test_m2)
tunedModel_svm_m1_test_eps = predict(tunedModel_svm_m1_train_eps, data=test_m1)

tunedModel_svm_m2_test_eps_MAE = MAE(tunedModel_svm_m2_test_eps, y_test_m2)
tunedModel_svm_m2_test_eps_RMSE = RMSE(tunedModel_svm_m2_test_eps, y_test_m2)
tunedModel_svm_m2_full_eps_MAE = MAE(tunedModel_svm_m2_eps, y_m2)
tunedModel_svm_m2_full_eps_RMSE = RMSE(tunedModel_svm_m2_eps, y_m2)

tunedModel_svm_m1_test_eps_MAE = MAE(tunedModel_svm_m1_test_eps, y_test_m1)
tunedModel_svm_m1_test_eps_RMSE = RMSE(tunedModel_svm_m1_test_eps, y_test_m1)
tunedModel_svm_m1_full_eps_MAE = MAE(tunedModel_svm_m1_eps, y_m1)
tunedModel_svm_m1_full_eps_RMSE = RMSE(tunedModel_svm_m1_eps, y_m1)

tunedModel_svm_m2_full_eps
R2(tunedModel_svm_m2_eps, y_m2)
MAE(tunedModel_svm_m2_eps, y_m2)
RMSE(tunedModel_svm_m2_eps, y_m2)
MAE(tunedModel_svm_m2_test_eps, y_test_m2)
RMSE(tunedModel_svm_m2_test_eps, y_test_m2)
tunedModel_svm_m2_full_eps_RMSE/tunedModel_svm_m2_test_eps_RMSE

tunedModel_svm_m1_full_eps
R2(tunedModel_svm_m1_eps, y_m1)
MAE(tunedModel_svm_m1_eps, y_m1)
RMSE(tunedModel_svm_m1_eps, y_m1)
MAE(tunedModel_svm_m1_test_eps, y_test_m1)
RMSE(tunedModel_svm_m1_test_eps, y_test_m1)
tunedModel_svm_m1_full_eps_RMSE/tunedModel_svm_m1_test_eps_RMSE

tuneResult_svm_m2_full_eps$performances
plot(tuneResult_svm_m2_full, type = "perspective", theta = 120, phi = 45)
tunedModel_svm_m2_eps_to_raster <- predict(covs_m2, tunedModel_svm_m2_full_eps)
writeRaster(tunedModel_svm_m2_eps_to_raster, filename = "./results/tunedModel_svm_m2_eps_to_raster.tif", overwrite=TRUE)

tuneResult_svm_m1_full_eps$performances
plot(tunedModel_svm_m1_train_eps, type = "perspective", theta = 120, phi = 45)
tunedModel_svm_m1_eps_to_raster <- predict(covs_m1, tunedModel_svm_m1_full_eps)
writeRaster(tunedModel_svm_m1_eps_to_raster, filename = "./results/tunedModel_svm_m1_eps_to_raster.tif", overwrite=TRUE)

par(mfrow=c(2,1))
plot(tunedModel_svm_m1_eps_to_raster, main="Model2 WITH-STEMS (SVM-Radial +eps.tune)", cex.main=0.9)
hist(tunedModel_svm_m1_eps_to_raster, main="Model2 WITH-STEMS (SVM-Radial +eps.tune)", cex.main=0.8, maxpixels=22000000) 

tunedModel_svm_m2_eps_to_raster$layer[tunedModel_svm_m2_eps_to_raster$layer < 0] = NA
plot(tunedModel_svm_m2_eps_to_raster, main="Model1 NO-STEMS (SVM-Radial +eps.tune)", cex.main=0.8)
hist(tunedModel_svm_m2_eps_to_raster, main="Model1 NO-STEMS (SVM-Radial +eps.tune)", cex.main=0.8, maxpixels=22000000) 

# e1071 - randomforest
tuneResult_rf_m2_full <- tune.randomForest(
  X_m2, y_m2,
  mtry = c(2:10), ntree = 50,
  tunecontrol = tune.control(sampling = "cross", cross = 10),
  preProcess = c("BoxCox","center","scale"))

tuneResult_rf_m1_full <- tune.randomForest(
  X_m1, y_m1,
  mtry = c(2:10), ntree = 50,
  tunecontrol = tune.control(sampling = "cross", cross = 10),
  preProcess = c("BoxCox","center","scale"))

tunedModel_rf_m2_full <- tuneResult_rf_m2_full$best.model
tunedModel_rf_m1_full <- tuneResult_rf_m1_full$best.model
print(summary(tunedModel_rf_m2_full))
print(summary(tunedModel_rf_m1_full))
tunedModel_rf_m2_full
tunedModel_rf_m1_full
save(tunedModel_rf_m2_full, file = "./results/tunedModel_rf_m2_full.RData")
save(tunedModel_rf_m1_full, file = "./results/tunedModel_rf_m1_full.RData")

tunedModel_rf_m2 = predict(
  tunedModel_rf_m2_full,
  X_m2, y_m2,
  type = "response")

tunedModel_rf_m1 = predict(
  tunedModel_rf_m1_full,
  X_m1, y_m1,
  type = "response")

tuneResult_rf_m2_train <- tune.randomForest(
  X_train_m2, y_train_m2,
  mtry = c(2:10), ntree = 50,
  tunecontrol = tune.control(sampling = "cross", cross = 10),
  preProcess = c("BoxCox","center","scale"))

tuneResult_rf_m1_train <- tune.randomForest(
  X_train_m1, y_train_m1,
  mtry = c(2:10), ntree = 50,
  tunecontrol = tune.control(sampling = "cross", cross = 10),
  preProcess = c("BoxCox","center","scale"))

tunedModel_rf_m2_train <- tuneResult_rf_m2_train$best.model
print(summary(tunedModel_rf_m2_train))
tunedModel_rf_m2_train

tunedModel_rf_m1_train <- tuneResult_rf_m1_train$best.model
print(summary(tunedModel_rf_m1_train))
tunedModel_rf_m1_train

tunedModel_rf_m2_test = predict(
  tunedModel_rf_m2_train,
  X_test_m2, y_test_m2,
  type="response")

tunedModel_rf_m1_test = predict(
  tunedModel_rf_m1_train,
  X_test_m1, y_test_m1,
  type="response")

tunedModel_rf_m2_test_MAE = MAE(tunedModel_rf_m2_test, y_test_m2)
tunedModel_rf_m2_test_RMSE = RMSE(tunedModel_rf_m2_test, y_test_m2)
tunedModel_rf_m2_full_MAE = MAE(tunedModel_rf_m2, y_m2)
tunedModel_rf_m2_full_RMSE = RMSE(tunedModel_rf_m2, y_m2)

tunedModel_rf_m1_test_MAE = MAE(tunedModel_rf_m1_test, y_test_m2)
tunedModel_rf_m1_test_RMSE = RMSE(tunedModel_rf_m1_test, y_test_m2)
tunedModel_rf_m1_full_MAE = MAE(tunedModel_rf_m1, y_m2)
tunedModel_rf_m1_full_RMSE = RMSE(tunedModel_rf_m1, y_m2)

MAE(tunedModel_rf_m2, y_m2)
RMSE(tunedModel_rf_m2, y_m2)
MAE(tunedModel_rf_m2_test, y_test_m2)
RMSE(tunedModel_rf_m2_test, y_test_m2)
R2(tunedModel_rf_m2, y_m2)
tunedModel_rf_m2_full_RMSE/tunedModel_rf_m2_test_RMSE

MAE(tunedModel_rf_m1, y_m1)
RMSE(tunedModel_rf_m1, y_m1)
MAE(tunedModel_rf_m1_test, y_test_m1)
RMSE(tunedModel_rf_m1_test, y_test_m1)
R2(tunedModel_rf_m1, y_m1)
tunedModel_rf_m1_full_RMSE/tunedModel_rf_m1_test_RMSE

tuneResult_rf_m2_full$performances
tunedModel_rf_m2_to_raster <- predict(covs_m2, tunedModel_rf_m2_full)
writeRaster(tunedModel_rf_m2_to_raster, filename = "./results/tunedModel_rf_m2_to_raster.tif", overwrite=TRUE)

tuneResult_rf_m1_full$performances
tunedModel_rf_m1_to_raster <- predict(covs_m1, tunedModel_rf_m1_full)
writeRaster(tunedModel_rf_m1_to_raster, filename = "./results/tunedModel_rf_m1_to_raster.tif", overwrite=TRUE)

par(mfrow=c(2,2))
plot(tunedModel_rf_m1_to_raster, main="Model2 WITH-STEMS (Random Forest)", cex.main=0.9)
hist(tunedModel_rf_m1_to_raster, main="Model2 WITH-STEMS (Random Forest)", cex.main=0.8, maxpixels=22000000) 

plot(tunedModel_rf_m2_to_raster, main="Model1 NO-STEMS (Random Forest)", cex.main=0.8)
hist(tunedModel_rf_m2_to_raster, main="Model1 NO-STEMS (Random Forest)", cex.main=0.8, maxpixels=22000000) 

