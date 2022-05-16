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
library(tibble)
library(pre)
library(glmnet)
library(purrr)
library(imager)
library(ForestTools)
library(tibble)
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
terra::plot(species_class_rast, main = "species_class (all sites)")

terra::plot(lead_htop_rast_gaspard, main = "lead_htop")
terra::plot(stemsha_L_rast_gaspard, main = "stemsha_L")
terra::plot(elev_rast_gaspard, main = "elevation")
terra::plot(slope_rast_gaspard, main = "slope")
terra::plot(asp_cos_rast_gaspard, main = "asp_cos")
terra::plot(asp_sin_rast_gaspard, main = "asp_sin")
terra::plot(species_class_rast_gaspard, main = "species_class")

terra::plot(lead_htop_rast_quesnel, main = "lead_htop")
terra::plot(stemsha_L_rast_quesnel, main = "stemsha_L")
terra::plot(elev_rast_quesnel, main = "elevation")
terra::plot(slope_rast_quesnel, main = "slope")
terra::plot(asp_cos_rast_quesnel, main = "asp_cos")
terra::plot(asp_sin_rast_quesnel, main = "asp_sin")
terra::plot(species_class_rast_quesnel, main = "species_class")

covs_m1_quesnel = raster::stack(
  lead_htop_raster_quesnel,
  stemsha_L_raster_quesnel,
  elev_raster_quesnel, 
  slope_raster_quesnel,
  aspect_raster_quesnel, 
  species_class_raster_quesnel)

covs_m1_gaspard = raster::stack(
  lead_htop_raster_gaspard,
  stemsha_L_raster_gaspard,
  elev_raster_gaspard, 
  slope_raster_gaspard,
  aspect_raster_gaspard,
  species_class_raster_gaspard)

covs_m1 = raster::stack(
  lead_htop_raster,
  stemsha_L_raster,
  elev_raster, 
  slope_raster,
  aspect_raster,
  species_class_raster)


# refactor species covariate to enable glmGamma modelling
species_class_sv = terra::as.polygons(species_class_rast)
species_class_sf = sf::st_as_sf(species_class_sv)
species_class_sf$layer = dplyr::recode(species_class_sf$layer, '0' = 1, '1' = 2, '2' = 3, '3' = 4, '4' = 5, '5' = 6, '6' = 7, '7' = 8)
species_class_rast = terra::rasterize(terra::vect(species_class_sf), species_class_rast, field = "layer", touches = TRUE)

species_class_sv_gaspard = terra::as.polygons(species_class_rast_gaspard)
species_class_sf_gaspard = sf::st_as_sf(species_class_sv_gaspard)
species_class_sf_gaspard$layer = dplyr::recode(species_class_sf_gaspard$layer, '0' = 1, '1' = 2, '2' = 3, '3' = 4, '4' = 5, '5' = 6, '6' = 7, '7' = 8)
species_class_rast_gaspard = terra::rasterize(terra::vect(species_class_sf_gaspard), species_class_rast_gaspard, field = "layer", touches = TRUE)

species_class_sv_quesnel = terra::as.polygons(species_class_rast_quesnel)
species_class_sf_quesnel = sf::st_as_sf(species_class_sv_quesnel)
species_class_sf_quesnel$layer = dplyr::recode(species_class_sf_quesnel$layer, '0' = 1, '1' = 2, '2' = 3, '3' = 4, '4' = 5, '5' = 6, '6' = 7, '7' = 8)
species_class_rast_quesnel = terra::rasterize(terra::vect(species_class_sf_quesnel), species_class_rast_quesnel, field = "layer", touches = TRUE)

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
  lead_htop_raster_quesnel,
  stemsha_L_raster_quesnel,
  elev_raster_quesnel, 
  slope_raster_quesnel,
  aspect_raster_quesnel,
  species_class_raster_quesnel)

covs_m1_gaspard = raster::stack(
  lead_htop_raster_gaspard,
  stemsha_L_raster_gaspard,
  elev_raster_gaspard, 
  slope_raster_gaspard,
  aspect_raster_gaspard,
  species_class_raster_gaspard)

covs_m1 = raster::stack(
  lead_htop_raster,
  stemsha_L_raster,
  elev_raster, 
  slope_raster,
  aspect_raster, 
  species_class_raster)

#rasterVis::levelplot(covs_m1_quesnel, layers=1, margin = list(FUN = median), main= 'lead_htop (Quesnel)')
#rasterVis::levelplot(covs_m1_quesnel, layers=2, margin = list(FUN = median), main= 'stemsha_L (Quesnel)')
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

# Import ground plot data
faib_psp <- read.csv("/media/seamus/128GB_WORKD/EFI-TCC/0_Caret_Predict_to_writeRasterOutput/Data/FAIB_PSP_20211028.csv")
print(as_tibble(faib_psp), n = 10)
faib_psp$spc_live1 = as.factor(faib_psp$spc_live1)
base::table(faib_psp$spc_live1, faib_psp$beclabel)
faib_psp =  subset(faib_psp, spc_live1=='PL' | spc_live1=='PLI' | spc_live1=='SB' | spc_live1=='SE' | spc_live1=='SW' | spc_live1=='SX' | spc_live1=='FD'| spc_live1=='FDI' | spc_live1=='CW' | spc_live1=='HW' | spc_live1=='BL' | spc_live1=='LW')
faib_psp = faib_psp[!(faib_psp$species_class==2 & faib_psp$bgc_zone == 'SBS' | faib_psp$species_class==2 & faib_psp$bgc_zone =='SBPS' | faib_psp$species_class==2 & faib_psp$bgc_zone =='ICH'),]
faib_psp$species_class = dplyr::recode(faib_psp$spc_live1, PL = 1, PLI = 1, SB = 2, SE = 2, SX = 2, FD = 3, FDI = 3, CW = 3, HW = 4, BL = 5, LW = 6)
base::table(faib_psp$species_class, faib_psp$beclabel)

faib_psp$elev = as.numeric(faib_psp$elev)
faib_psp$slope = as.numeric(faib_psp$slope)
faib_psp$aspect = as.numeric(faib_psp$aspect)
faib_psp$lead_htop = as.numeric(faib_psp$lead_htop)
faib_psp$species_class = as.factor(faib_psp$species_class)
faib_psp$stemsha_L = as.numeric(faib_psp$stemsha_L)
faib_psp$wsvha_L = as.numeric(faib_psp$wsvha_L)

faib_vri_true_m1_df = faib_psp[c("elev", "slope", "aspect", "lead_htop", "species_class", "stemsha_L", "wsvha_L")]
faib_vri_true_m2_df = faib_psp[c("elev", "slope", "aspect", "lead_htop", "species_class", "wsvha_L")] 

faib_vri_true_m1_df$elev[faib_vri_true_m1_df$elev <= 0] = NA
faib_vri_true_m1_df$slope[faib_vri_true_m1_df$slope <= 0] = NA
faib_vri_true_m1_df$aspect[faib_vri_true_m1_df$aspect <= 0] = NA
faib_vri_true_m1_df$lead_htop[faib_vri_true_m1_df$lead_htop < 2] = NA
faib_vri_true_m1_df$stemsha_L[faib_vri_true_m1_df$stemsha_L <= 0] = NA
faib_vri_true_m1_df$species_class[faib_vri_true_m1_df$species_class <= 0] = NA
faib_vri_true_m1_df$wsvha_L[faib_vri_true_m1_df$wsvha_L <= 0] = NA

faib_vri_true_m1_df = na.omit(faib_vri_true_m1_df)
faib_vri_true_m2_df = na.omit(faib_vri_true_m2_df)
sum(is.na(faib_vri_true_m1_df))
sum(is.na(faib_vri_true_m2_df))
print(as_tibble(faib_vri_true_m1_df), n = 10)
print(as_tibble(faib_vri_true_m2_df), n = 10)

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

# fit models: 
tuneResult_GLM_m1_logLink_gamma <- glm(
  formula = wsvha_L ~ lead_htop + stemsha_L + elev + slope + aspect + species_class, 
  family = Gamma(link = "log"),
  data=faib_vri_true_m1_df)

tuneResult_GLM_m1_logLink_gauss <- glm(
  formula = wsvha_L ~ lead_htop + stemsha_L + elev + slope + aspect + species_class, 
  family = gaussian(link = "log"),
  data=faib_vri_true_m1_df)

base::print(tuneResult_GLM_m1_logLink_gamma)
base::print(tuneResult_GLM_m1_logLink_gauss)
performance::model_performance(tuneResult_GLM_m1_logLink_gamma)
performance::model_performance(tuneResult_GLM_m1_logLink_gauss)
performance::performance_mae(tuneResult_GLM_m1_logLink_gamma)
performance::performance_mae(tuneResult_GLM_m1_logLink_gauss)
performance::performance_rmse(tuneResult_GLM_m1_logLink_gamma)
performance::performance_rmse(tuneResult_GLM_m1_logLink_gauss)

save(tuneResult_GLM_m1_logLink_gamma, file = "./models/tuneResult_GLM_m1_logLink_gamma.RData")
save(tuneResult_GLM_m1_logLink_gauss, file = "./models/tuneResult_GLM_m1_logLink_gauss.RData")
tuneResult_GLM_m1_logLink_gamma_to_raster = raster::predict(covs_m1, tuneResult_GLM_m1_logLink_gamma)
tuneResult_GLM_m1_logLink_gauss_to_raster = raster::predict(covs_m1, tuneResult_GLM_m1_logLink_gauss)
writeRaster(tuneResult_GLM_m1_logLink_gamma_to_raster, filename = "./results/model2_glm_loglink_gamma.tif", overwrite=TRUE)
writeRaster(tuneResult_GLM_m1_logLink_gauss_to_raster, filename = "./results/model2_glm_loglink_gauss.tif", overwrite=TRUE)
model2_glm_loglink_gamma = raster::raster("./results/model2_glm_loglink_gamma.tif")
model2_glm_loglink_gauss = raster::raster("./results/model2_glm_loglink_gauss.tif")
plot(model2_glm_loglink_gamma, main="model2_glm_loglink_gamma)", cex.main=0.9)
hist(model2_glm_loglink_gamma, main="model2_glm_loglink_gamma)", cex.main=0.8, maxpixels=22000000) 
plot(model2_glm_loglink_gauss, main="model2_glm_loglink_gauss", cex.main=0.8)
hist(model2_glm_loglink_gauss, main="model2_glm_loglink_gauss)", cex.main=0.8, maxpixels=22000000) 
tuneResult_GLM_m1_logLink_gamma_to_raster_gaspard = raster::predict(covs_m1_gaspard, tuneResult_GLM_m1_logLink_gamma)
tuneResult_GLM_m1_logLink_gamma_to_raster_quesnel = raster::predict(covs_m1_quesnel, tuneResult_GLM_m1_logLink_gamma)

# compare rasters with faib permanent sample plot data
library(MASS)
par(mfrow=c(4,4))
truehist(faib_vri_true_m1_df$wsvha_L, main="WSVHA (faib)", maxpixels=22000000)
hist(tuneResult_GLM_m1_logLink_gamma_to_raster, main="WSVHA (all sites)")
hist(tuneResult_GLM_m1_logLink_gamma_to_raster_gaspard, main="WSVHA (Gaspard)")
hist(tuneResult_GLM_m1_logLink_gamma_to_raster_quesnel, main="WSVHA (Quesnel)")

truehist(faib_vri_true_m1_df$elev, main="DEM (faib)", maxpixels=22000000)
hist(elev_raster, main="DEM (all sites)", maxpixels=22000000)
hist(elev_raster_gaspard, main="DEM (Gaspard)", maxpixels=22000000)
hist(elev_raster_quesnel, main="DEM (Quesnel)", maxpixels=22000000)

truehist(faib_vri_true_m1_df$slope, main="Slope (faib)", maxpixels=22000000)
hist(slope_raster, main="Slope (all sites)", maxpixels=22000000) 
hist(slope_raster_gaspard, main="Slope (Gaspard)", maxpixels=22000000) 
hist(slope_raster_quesnel, main="Slope (Quesnel)", maxpixels=22000000) 

truehist(faib_vri_true_m1_df$aspect, main="Aspect (faib)", maxpixels=22000000)
hist(aspect_raster, main="Aspect (all sites)", maxpixels=22000000)
hist(aspect_raster_gaspard, main="Aspect (Gaspard)", maxpixels=22000000)
hist(aspect_raster_quesnel, main="Aspect (Quesnel)", maxpixels=22000000)

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
