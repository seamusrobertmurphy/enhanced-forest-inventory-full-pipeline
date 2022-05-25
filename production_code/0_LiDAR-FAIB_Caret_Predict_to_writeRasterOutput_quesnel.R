library(devtools)
library(rgdal)
library(rgeos)
library(tibble)
library(conflicted)
library(sf)
library(terra)
library(raster)
library(rasterVis)
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

# Import LiDAR data
zip_file_vh = ("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/VegHt.zip")
zip_file_be = ("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/BareEarth.zip")
zip_dir_vh = ("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region")
zip_dir_be = ("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region")
unzip(zip_file_vh, exdir=zip_dir_vh, overwrite = TRUE)
unzip(zip_file_be, exdir=zip_dir_be, overwrite = TRUE)
unzip_dir_vh <- paste0("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/VegHt")
unzip_dir_be <- paste0("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/BareEarth")

filez_vh = list.files(
  unzip_dir_vh,
  full.names = T,
  all.files = FALSE,
  pattern = '.tif$') 

filez_be = list.files(
  unzip_dir_be,
  full.names = T,
  all.files = FALSE,
  pattern = '.tif$') 

lead_htop_raster_list <- lapply(filez_vh, raster)
elev_raster_list <- lapply(filez_be, raster)
lead_htop_raster = do.call(merge, c(lead_htop_raster_list, tolerance = 1))
elev_raster = do.call(merge, c(elev_raster_list, tolerance = 1))
writeRaster(lead_htop_raster, filename = "./data_quesnel/covariates/lead_htop_raster.tif", overwrite=TRUE)
writeRaster(elev_raster, filename = "./data_quesnel/covariates/elev_raster.tif", overwrite=TRUE)


# Derive CHM-covariates
kernel <- matrix(1,3,3)
wf_Quan<-function(x){ 
  a=0.179-0.1
  b=0.51+0.5 
  y<-a*x+b 
  return(y)}
wf_Plowright<-function(x){ 
  a=0.05
  b=0.6 
  y<-a*x+b 
  return(y)}
heights <- seq(0,40,0.5)
window_Quan <- wf_Quan(heights)
window_Plowright <- wf_Plowright(heights)
plot(heights, window_Quan, type = "l",  ylim = c(0,12), xlab="point elevation (m)", ylab="window diameter (m)", main='Quan')
plot(heights, window_Plowright, type = "l", ylim = c(0,12), xlab="point elevation (m)", ylab="window diameter (m)", main='Plowright')

#lead_htop_raster_1m = raster::raster("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/lead_htop_raster.tif")
#lead_htop_raster_1m_smoothed = focal(rast(lead_htop_raster_1m), w = kernel, fun = median, na.rm = TRUE) %>% raster()
#writeRaster(lead_htop_raster_1m_smoothed, filename = "/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/lead_htop_raster_1m_smoothed.tif", overwrite=TRUE)
lead_htop_raster_1m_smoothed = raster::raster("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/lead_htop_raster_1m_smoothed.tif")
ttops_2m_Quan = ForestTools::vwf(CHM = lead_htop_raster_1m_smoothed, winFun = wf_Quan, minHeight = 2)
#crownsPoly_2mTO1.5m_Quan <- ForestTools::mcws(treetops = ttops_2m_Quan, CHM = lead_htop_raster_1m_smoothed, format = "polygons", minHeight = 1.5, verbose = FALSE)
#crowns_2mTO1.5m_Quan <- ForestTools::mcws(treetops = ttops_2m_Quan, CHM = lead_htop_raster_1m_smoothed, minHeight = 1.5, verbose = FALSE)

plot(ttops_2m_Quan, cex = 0.0001, pch=19, col = 'blue')
#plot(crowns_2mTO1.5m_Quan, col = sample(rainbow(50), length(unique(crowns_2mTO1.5m_Quan[])), replace = TRUE), legend = FALSE)
#plot(crownsPoly_2mTO1.5m_Quan, border = "blue", lwd = 0.001)

quant95 <- function(x, ...) 
  quantile(x, c(0.95), na.rm = TRUE)
custFuns <- list(quant95, max)
names(custFuns) <- c("95thQuantile", "Max")

#ttops_2m_Quan_raster_2m1.5m_95th_1cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 1, variables = "height", statFuns = custFuns)
ttops_2m_Quan_raster_2m1.5m_95th_10cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 10, variables = "height", statFuns = custFuns)
#ttops_2m_Quan_raster_2m1.5m_95th_20cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 20, variables = "height", statFuns = custFuns)
#ttops_2m_Quan_raster_2m1.5m_95th_50cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 50, variables = "height", statFuns = custFuns)
ttops_2m_Quan_raster_2m1.5m_95th_100cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 100, variables = "height", statFuns = custFuns)

#lead_htop_ttops_1cell = ttops_2m_Quan_raster_2m1.5m_95th_1cell[["height95thQuantile"]]
lead_htop_ttops_10cell = ttops_2m_Quan_raster_2m1.5m_95th_10cell[["height95thQuantile"]]
#lead_htop_ttops_20cell = ttops_2m_Quan_raster_2m1.5m_95th_20cell[["height95thQuantile"]]
#lead_htop_ttops_50cell = ttops_2m_Quan_raster_2m1.5m_95th_50cell[["height95thQuantile"]]
lead_htop_ttops_100cell = ttops_2m_Quan_raster_2m1.5m_95th_100cell[["height95thQuantile"]]

#stemsha_L_ttops_1cell = ttops_2m_Quan_raster_2m1.5m_95th_1cell[["TreeCount"]]
stemsha_L_ttops_10cell = ttops_2m_Quan_raster_2m1.5m_95th_10cell[["TreeCount"]]
#stemsha_L_ttops_20cell = ttops_2m_Quan_raster_2m1.5m_95th_20cell[["TreeCount"]]
#stemsha_L_ttops_50cell = ttops_2m_Quan_raster_2m1.5m_95th_50cell[["TreeCount"]]
stemsha_L_ttops_100cell = ttops_2m_Quan_raster_2m1.5m_95th_100cell[["TreeCount"]]

#raster::writeRaster(lead_htop_ttops_1cell, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_ttops_1cell.tif", overwrite=TRUE)
raster::writeRaster(lead_htop_ttops_10cell, filename = "./data_quesnel/covariates/lead_htop_ttops_10cell.tif", overwrite=TRUE)
#raster::writeRaster(lead_htop_ttops_20cell, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_ttops_20cell.tif", overwrite=TRUE)
#raster::writeRaster(lead_htop_ttops_50cell, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_ttops_50cell.tif", overwrite=TRUE)
raster::writeRaster(lead_htop_ttops_100cell, filename = "./data_quesnel/covariates/lead_htop_ttops_100cell.tif", overwrite=TRUE)

#raster::writeRaster(stemsha_L_ttops_1cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_1cell.tif", overwrite=TRUE)
#raster::writeRaster(stemsha_L_ttops_10cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_10cell.tif", overwrite=TRUE)
#raster::writeRaster(stemsha_L_ttops_20cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_20cell.tif", overwrite=TRUE)
#raster::writeRaster(stemsha_L_ttops_50cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_50cell.tif", overwrite=TRUE)
raster::writeRaster(stemsha_L_ttops_100cell, filename = "./data_quesnel/covariates/stemsha_L_ttops_100cell.tif", overwrite=TRUE)

lead_htop = raster::raster("./data_quesnel/covariates/lead_htop_ttops_100cell.tif")
lead_htop_rast = rast(lead_htop)
crs(lead_htop_rast) = "epsg:3005"
lead_htop_sv = as.polygons(lead_htop_rast)
lead_htop_sf = sf::st_as_sf(lead_htop_sv)

# Import VRI & Mask layers
vri_sf = read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/vri/vri_bc_2020_rank1.shp")
#vri_sf = read_sf("/media/seamusrobertmurphy/128GB_WORKD/EFI-FIRE/Data/vri/vri-bc-2020/vri-2020-shp.shp")
#vri_sf = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/0_Caret_Predict_to_writeRasterOutput/Data/VEG_COMP_LYR_R1_POLY/VEG_R1_PLY_polygon.shp")
#vri_species = vri_sf[c("SPECIES__1", "SPECIES_CD", "SPECIES_PC")]
#vri_species_aoi = st_intersection(vri_species, st_make_valid(lead_htop_sf))
#summary.factor(vri_species_aoi$SPECIES__1)
vri_species_aoi =  dplyr::filter(vri_species_aoi, SPECIES__1=='PL' | SPECIES__1=='PLI' | SPECIES__1=='SB' | SPECIES__1=='SE' | SPECIES__1=='SW' | SPECIES__1=='SX' | SPECIES__1=='FD' | SPECIES__1=='FDI' | SPECIES__1=='CW' | SPECIES__1=='HW' | SPECIES__1=='BL' | SPECIES__1=='LW')
#vri_species_aoi_df = as_tibble(vri_species_aoi[!(vri_species_aoi$SPEC_CD_1 == 'FD' & vri_species_aoi$SPEC_PCT_1 >= 50 | vri_species_aoi$SPEC_CD_1 == 'FDI' & vri_species_aoi$SPEC_PCT_1 >=50),])
vri_species_aoi$SPECIES__1 = dplyr::recode(vri_species_aoi$SPECIES__1, PL = 0, PLI = 0, SB = 1, SE = 1, SW = 1, SX = 1, FD = 2, FDI = 2, CW = 3, HW = 4, BL = 5, LW = 6)
vri_species_aoi = dplyr::rename(vri_species_aoi, species_class = SPECIES__1)
vri_species_aoi = sf::st_as_sf(vri_species_aoi)
vri_species_aoi = vri_species_aoi["species_class"]

species_class_rast_quesnel = terra::rasterize(terra::vect(vri_species_aoi_quesnel), lead_htop_rast_quesnel, field = "species_class", touches = TRUE)
species_class_rast_gaspard = terra::rasterize(terra::vect(vri_species_aoi_gaspard), lead_htop_rast_gaspard, field = "species_class", touches = TRUE)
species_class_raster_quesnel = raster::raster(species_class_rast_quesnel)
species_class_raster_gaspard = raster::raster(species_class_rast_gaspard)
writeRaster(species_class_raster_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/species/species_class_raster_quesnel", overwrite=TRUE)
writeRaster(species_class_raster_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/species/species_class_raster_gaspard", overwrite=TRUE)


mask_burn2017 = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/tcc_mask_layers/TCC_Burn_Severity TCC_Burn_Severity_2017.shp")
mask_burn2017 = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/tcc_mask_layers/TCC_Burn_Severity TCC_Burn_Severity_2017.shp")

mask_burn2017 = mask_burn2017["BurnSev"]
mask_burn2017 = dplyr::filter(mask_burn2017, BurnSev == 'High')
mask_burn2017 = sf::st_intersection(sf::st_make_valid(mask_burn2017), lead_htop_sf)
mask_burn2018 = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/tcc_mask_layers/TCC_Burn_Severity TCC_Burn_Severity_2018.shp")
mask_burn2018 = mask_burn2018["BurnSev"]
mask_burn2018 = dplyr::filter(mask_burn2018, BurnSev == 'High')
mask_burn2018 = sf::st_intersection(sf::st_make_valid(mask_burn2018), lead_htop_sf)
mask_burn2021 = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/tcc_mask_layers/TCC_Burn_Severity TCC_Burn_Severity_2021.shp")
mask_burn2021 = mask_burn2021["BurnSev"]
mask_burn2021 = dplyr::filter(mask_burn2021, BurnSev == 'High')
mask_burn2021 = sf::st_intersection(sf::st_make_valid(mask_burn2021), lead_htop_sf)
masks_df = full_join(as_tibble(mask_burn2017), as_tibble(mask_burn2018), as_tibble(mask_burn2021), by = "geometry")
masks_sf = st_as_sf(masks_df) # easier to combine by 'geometry'

#mask_burns_bc = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/burns/burns_severity_bc/BURN_SVRTY_polygon.shp")
#mask_burns_2017_2021 = dplyr::filter(mask_burns_bc, FIRE_YEAR>=2017 & BURN_RATE == 'High')
#mask_burns_2017_2021 = mask_burns_2017_2021["FIRE_YEAR"]
#masks_sf = sf::st_intersection(sf::st_make_valid(mask_burns_2017_2021), lead_htop_sf)
#plot(masks_sf)

mask_clearcut = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/tcc_mask_layers/RSLT_CCRES_CLEAR.shp")
mask_clearcut = sf::st_intersection(mask_clearcut, st_make_valid(lead_htop_sf))
masks_df = full_join(as_tibble(masks_sf), as_tibble(mask_clearcut), by = 'geometry')
masks_sf = st_as_sf(masks_df)

mask_blocks = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/tcc_mask_layers/TCC_Blocks_Join.shp")
mask_blocks = sf::st_intersection(mask_blocks, st_make_valid(lead_htop_sf))
masks_df = full_join(as_tibble(masks_sf), as_tibble(mask_blocks), by = 'geometry')
masks_sf = st_as_sf(masks_df)
#ggplot(masks_sf) + geom_sf()

mask_roads_tcc = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/tcc_mask_layers/TCC_Roads.shp")
mask_roads_tcc = sf::st_zm(mask_roads_tcc)
mask_roads_tcc = sf::st_intersection(mask_roads_tcc, st_make_valid(lead_htop_sf))
mask_roads_tcc = sf::st_buffer(mask_roads_tcc, dist = 15, nQuadSegs = 5, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 2)
mask_roads_ften = sf::read_sf("/media/seamusrobertmurphy/128GB_WORKD/data/vector/tcc_mask_layers/FTEN_Roads_All.shp")
mask_roads_ften = sf::st_zm(mask_roads_ften)
mask_roads_ften = sf::st_intersection(mask_roads_ften, st_make_valid(lead_htop_sf))
mask_roads_ften = sf::st_buffer(mask_roads_ften, dist = 15, nQuadSegs = 5, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 2)
masks_df = full_join(as_tibble(masks_sf), as_tibble(mask_roads_tcc), as_tibble(mask_roads_ften), by = 'geometry')
masks_sf = st_as_sf(masks_df)
#ggplot(masks_sf) + geom_sf(aes(fill = 'red'), show.legend = FALSE)

masks_rast = rasterize(vect(masks_sf), lead_htop_rast, touches = TRUE)
masks_raster = raster::raster(masks_rast)
writeRaster(masks_raster, filename = "./data_quesnel/covariates/masks_raster.tif", overwrite=TRUE)
masks_raster = raster::raster("./data_quesnel/covariates/masks_raster.tif")
masks_rast = rast(masks_raster)
plot(masks_rast)

# Import LiDAR and derive DEM-based covariates 
elev_rast = terra::rast("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/elev_raster.tif")
elev_rast = terra::rast("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/elev_raster.tif")

lead_htop_rast = terra::rast("./data_quesnel/covariates/lead_htop_ttops_100cell.tif")
lead_htop_rast = terra::rast("./data_quesnel/covariates/lead_htop_ttops_100cell.tif")

stemsha_L_rast = terra::rast("./data_quesnel/covariates/stemsha_L_ttops_100cell.tif")
species_class_rast = terra::rast("./data_quesnel/covariates/species_class_raster.tif")
masks_rast = terra::rast("./data_quesnel/covariates/masks_raster.tif")

species_class_rast_quesnel = terra::rasterize(terra::vect(vri_species_aoi_quesnel), lead_htop_rast_quesnel, field = "species_class", touches = TRUE)
species_class_rast_gaspard = terra::rasterize(terra::vect(vri_species_aoi_gaspard), lead_htop_rast_gaspard, field = "species_class", touches = TRUE)
species_class_raster_quesnel = raster::raster(species_class_rast_quesnel)
species_class_raster_gaspard = raster::raster(species_class_rast_gaspard)
writeRaster(species_class_raster_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/species/species_class_raster_quesnel", overwrite=TRUE)
writeRaster(species_class_raster_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/species/species_class_raster_gaspard", overwrite=TRUE)

############

terra::crs(elev_rast_quesnel) = "epsg:3005"
terra::crs(elev_rast_gaspard) = "epsg:3005"
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

lead_htop_rast_20cell_quesnel = terra::resample(lead_htop_rast_20cell_quesnel, elev_rast_quesnel)
lead_htop_rast_20cell_gaspard = terra::resample(lead_htop_rast_20cell_gaspard, elev_rast_gaspard)
stemsha_L_rast_200cell_quesnel = terra::resample(stemsha_L_rast_200cell_quesnel, elev_rast_quesnel)
stemsha_L_rast_20cell_gaspard = terra::resample(stemsha_L_rast_20cell_gaspard, elev_rast_gaspard)
species_class_rast_quesnel = terra::resample(species_class_rast_quesnel, elev_rast_quesnel)
species_class_rast_gaspard = terra::resample(species_class_rast_gaspard, elev_rast_gaspard)
elev_rast_quesnel = terra::mask(elev_rast_quesnel, lead_htop_rast_20cell_quesnel)
elev_rast_gaspard = terra::mask(elev_rast_gaspard, lead_htop_rast_20cell_gaspard)
slope_rast_quesnel = terra::mask(slope_rast_quesnel, lead_htop_rast_20cell_quesnel)
slope_rast_gaspard = terra::mask(slope_rast_gaspard, lead_htop_rast_20cell_gaspard)
asp_cos_rast_quesnel = terra::mask(asp_cos_rast_quesnel, lead_htop_rast_20cell_quesnel)
asp_cos_rast_gaspard = terra::mask(asp_cos_rast_gaspard, lead_htop_rast_20cell_gaspard)
asp_sin_rast_quesnel = terra::mask(asp_sin_rast_quesnel, lead_htop_rast_20cell_quesnel)
asp_sin_rast_gaspard = terra::mask(asp_sin_rast_gaspard, lead_htop_rast_20cell_quesnel)
species_class_rast_quesnel = terra::mask(species_class_rast_quesnel, lead_htop_rast_20cell_quesnel)
species_class_rast_gaspard = terra::mask(species_class_rast_gaspard, lead_htop_rast_20cell_gaspard)

writeRaster(elev_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/elevation/elev_raster_quesnel", overwrite=TRUE)
writeRaster(elev_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/elevation/elev_raster_gaspard", overwrite=TRUE)
writeRaster(slope_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/slope/slope_raster_quesnel", overwrite=TRUE)
writeRaster(slope_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/slope/slope_raster_gaspard", overwrite=TRUE)
writeRaster(asp_cos_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/aspect/asp_cos_raster_quesnel", overwrite=TRUE)
writeRaster(asp_cos_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/aspect/asp_cos_raster_gaspard", overwrite=TRUE)
writeRaster(asp_sin_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/aspect/asp_sin_raster_quesnel", overwrite=TRUE)
writeRaster(asp_sin_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/aspect/asp_sin_raster_gaspard", overwrite=TRUE)
writeRaster(species_class_rast_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/species/species_class_raster_quesnel", overwrite=TRUE)
writeRaster(species_class_rast_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/species/species_class_raster_gaspard", overwrite=TRUE)



species_class = raster::raster("./data_quesnel/covariates/species_class_raster.tif")
elev = raster::raster("./data_quesnel/covariates/elev_raster.tif")
slope = raster::raster("./data_quesnel/covariates/slope_raster.tif")
asp_cos = raster::raster("./data_quesnel/covariates/asp_cos_raster.tif")
asp_sin = raster::raster("./data_quesnel/covariates/asp_sin_raster.tif")
elev_rast = terra::rast(elev)
slope_rast = terra::rast(slope)
asp_cos_rast = terra::rast(asp_cos)
asp_sin_rast = terra::rast(asp_sin)
species_class_rast = terra::rast(species_class)

# mask by >3m canopy 
lead_htop_rast[lead_htop_rast < 3] = NA
elev_rast = mask(elev_rast, lead_htop_rast, inverse=FALSE)
slope_rast = mask(slope_rast, lead_htop_rast, inverse=FALSE)
asp_cos_rast = mask(asp_cos_rast, lead_htop_rast, inverse=FALSE)
asp_sin_rast = mask(asp_sin_rast, lead_htop_rast, inverse=FALSE)
species_class_rast = mask(species_class_rast, lead_htop_rast, inverse=FALSE)
stemsha_L_rast = mask(stemsha_L_rast, lead_htop_rast, inverse=FALSE)

# mask by species layer
lead_htop_rast = mask(lead_htop_rast, species_class_rast, inverse=FALSE)
elev_rast = mask(elev_rast, species_class_rast, inverse=FALSE)
slope_rast = mask(slope_rast, species_class_rast, inverse=FALSE)
asp_cos_rast = mask(asp_cos_rast, species_class_rast, inverse=FALSE)
asp_sin_rast = mask(asp_sin_rast, species_class_rast, inverse=FALSE)
stemsha_L_rast = mask(stemsha_L_rast, species_class_rast, inverse=FALSE)

# mask by master mask file
masks_rast = terra::resample(masks_rast, lead_htop_rast, method="near")
lead_htop_rast = mask(lead_htop_rast, masks_rast, inverse=TRUE)
elev_rast = mask(elev_rast, masks_rast, inverse=TRUE)
slope_rast = mask(slope_rast, masks_rast, inverse=TRUE)
asp_cos_rast = mask(asp_cos_rast, masks_rast, inverse=TRUE)
asp_sin_rast = mask(asp_sin_rast, masks_rast, inverse=TRUE)
stemsha_L_rast = mask(stemsha_L_rast, masks_rast, inverse=TRUE)
species_class_rast = mask(species_class_rast, masks_rast, inverse=TRUE)

names(elev_rast) = "elev"
names(slope_rast) = "slope"
names(asp_cos_rast) = "asp_cos"
names(asp_sin_rast) = "asp_sin"
names(species_class_rast) = "species_class"
names(stemsha_L_rast) = "stemsha_L"
names(lead_htop_rast) = "lead_htop"

elev_raster = raster(elev_rast)
slope_raster = raster(slope_rast)
asp_cos_raster = raster(asp_cos_rast)
asp_sin_raster = raster(asp_sin_rast)
species_class_raster = raster(species_class_rast)
stemsha_L_raster = raster(stemsha_L_rast)
lead_htop_raster = raster(lead_htop_rast)

covs_m2 = stack(
  elev_raster,
  slope_raster, 
  asp_cos_raster, 
  asp_sin_raster, 
  lead_htop_raster, 
  species_class_raster)

covs_m1 = stack(
  elev_raster,
  slope_raster, 
  asp_cos_raster, 
  asp_sin_raster, 
  lead_htop_raster, 
  species_class_raster,
  stemsha_L_raster)

names(covs_m2)
names(covs_m1)

# Import ground plot data
#faib_psp <- utils::read.csv("./Data/FAIB_PSP_20211028.csv")
faib_psp <- read.csv("/media/seamusrobertmurphy/128GB_WORKD/EFI-TCC/0_Caret_Predict_to_writeRasterOutput/Data/FAIB_PSP_20211028.csv")
print(as_tibble(faib_psp), n = 10)

# Tidy ground plot data
faib_psp$spc_live1 = as.factor(faib_psp$spc_live1)
base::table(faib_psp$spc_live1, faib_psp$beclabel)
faib_psp =  subset(faib_psp, spc_live1=='PL' | spc_live1=='SB' | spc_live1=='SE' | spc_live1=='SX' | spc_live1=='FD')
faib_psp$species_class = dplyr::recode(faib_psp$spc_live1, PL = 0, SB = 1, SE = 1, SX = 1, FD = 2)
faib_psp = faib_psp[!(faib_psp$species_class==2 & faib_psp$bgc_zone == 'SBS' | faib_psp$species_class==2 & faib_psp$bgc_zone =='SBPS' | faib_psp$species_class==2 & faib_psp$bgc_zone =='ICH'),]
base::table(faib_psp$species_class, faib_psp$beclabel)

faib_psp$asp_cos = cos((faib_psp$aspect * pi) / 180)
faib_psp$asp_sin = sin((faib_psp$aspect * pi) / 180)
faib_vri_true_m1_df = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "stemsha_L", "wsvha_L")]
faib_vri_true_m2_df = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "wsvha_L")] 
faib_vri_true_m1_df$lead_htop[faib_vri_true_m1_df$lead_htop < 1.3] = NA
faib_vri_true_m2_df$lead_htop[faib_vri_true_m2_df$lead_htop < 1.3] = NA
faib_vri_true_m1_df = na.omit(faib_vri_true_m1_df)
faib_vri_true_m2_df = na.omit(faib_vri_true_m2_df)
sum(is.na(faib_vri_true_m1_df))
sum(is.na(faib_vri_true_m2_df))

faib_psp$elev = as.numeric(faib_psp$elev)
faib_psp$slope = as.numeric(faib_psp$slope)
faib_psp$aspect = as.numeric(faib_psp$aspect)
faib_psp$asp_cos = as.numeric(faib_psp$asp_cos)
faib_psp$asp_sin = as.numeric(faib_psp$asp_sin)
faib_psp$lead_htop = as.numeric(faib_psp$lead_htop)
faib_psp$species_class = as.numeric(faib_psp$species_class)
faib_psp$stemsha_L = as.numeric(faib_psp$stemsha_L)
faib_psp$wsvha_L = as.numeric(faib_psp$wsvha_L)
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


# fit models: model1_svmRadial (with BoxCox and YeoJohnson transformations)
fitControl_YeoJx1 = caret::trainControl(method="repeatedcv", number=10, repeats=1)
fitControl_YeoJx3 = caret::trainControl(method="repeatedcv", number=10, repeats=3)


# GLM gamma family  https://daviddalpiaz.github.io/r4sl/index.html & https://rpubs.com/kaz_yos/glm-Gamma 
m2_coefs_I = wsvha_L ~ elev + slope + asp_cos + asp_sin + lead_htop + species_class - 1
m1_coefs_I = wsvha_L ~ elev + slope + asp_cos + asp_sin + lead_htop + species_class + stemsha_L - 1
m2_coefs_II = lm(m2_coefs_I, data = faib_vri_true_m2_df)
m1_coefs_II = lm(m1_coefs_I, data = faib_vri_true_m1_df)
coefplot::coefplot(m2_coefs_II, sort='magnitude')
coefplot::coefplot(m1_coefs_II, sort='magnitude')

#plot the effects of gamma relaxer on RMSE
tuneResult_glmnet_gamma_m2_cv <- cv.glmnet(as.matrix(X_train_m2), y_train_m2, relax = TRUE)
plot(tuneResult_glmnet_gamma_m2_cv)
print(tuneResult_glmnet_gamma_m2_cv)

#plot the effects of gamma relaxer on coefficients
tuneResult_glmnet_gamma_m2 <- glmnet(X_m2, y_m2, relax = TRUE)
print(tuneResult_glmnet_gamma_m2)
par(mfrow = c(1, 3), mar=c(4,4,5.5,1))
plot(tuneResult_glmnet_gamma_m2, main = "gamma = 1")
plot(tuneResult_glmnet_gamma_m2, gamma = 0.5, main = "gamma = 0.5")
plot(tuneResult_glmnet_gamma_m2, gamma = 0, main = "gamma = 0")

#control <- trainControl(method = "LOOCV")
grid <- expand.grid(.alpha = seq(0, 1, by = 0.2), .lambda = seq(-1,3,0.0125))
gridII <- expand.grid(.alpha=seq(0,1,by=0.1), .lambda=c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6))
gridIII <- expand.grid(.gamma=c(0, 0.5, 1), .lambda=c(0, 0.5, 1))

tuneResult_GLM_m2_full <- train(
  X_m2, y_m2,
  method = 'glmnet',
  trControl = fitControl_YeoJx1,
  tuneGrid = grid,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  metric='RMSE')
tuneResult_GLM_m2_full$bestTune
tuneResult_GLM_m2_coerced = glmnet(
  as.matrix(X_m2), y_m2, alpha=1, lambda=0.425,
  relax = TRUE)

tuneResult_GLM_m1_full <- caret::train(
  X_m1, y_m1,
  method = 'glmnet',
  trControl = fitControl_YeoJx1,
  tuneGrid = grid,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  metric='RMSE')
tuneResult_GLM_m1_full$bestTune
tuneResult_GLM_m1_coerced = glmnet(
  as.matrix(X_m1), y_m1, alpha=1, lambda=0.2625,
  relax = TRUE)

save(tuneResult_GLM_m2_coerced, file = "./Models/tuneResult_GLM_m2_coerced.RData")
save(tuneResult_GLM_m1_coerced, file = "./Models/tuneResult_GLM_m1_coerced.RData")
tuneResult_GLM_m2_coerced_to_raster <- raster::predict(covs_m2, tuneResult_GLM_m2_full)
tuneResult_GLM_m1_coerced_to_raster <- raster::predict(covs_m1, tuneResult_GLM_m1_full)
writeRaster(tuneResult_GLM_m2_coerced_to_raster, filename = "./Results/model1_glm_gamma_combo3.tif", overwrite=TRUE)
writeRaster(tuneResult_GLM_m1_coerced_to_raster, filename = "./Results/model2_glm_gamma_combo3.tif", overwrite=TRUE)
print(tuneResult_GLM_m2_full)
print(tuneResult_GLM_m1_full)
tuneResult_GLM_m1_full$finalModel

par(mfrow = c(2,2))
model1_glmGamma_100cell = raster::raster("./Results/model1_glm_gamma_combo3.tif")
model2_glmGamma_100cell = raster::raster("./Results/model2_glm_gamma_combo3.tif")
plot(model1_glmGamma_100cell, main="Model1 NO-STEMS (Combo3 GLM gamma-tuned)", cex.main=0.9)
hist(model1_glmGamma_100cell, main="Model1 NO-STEMS (Combo3 GLM gamma-tuned)", cex.main=0.8, maxpixels=22000000) 
plot(model2_glmGamma_100cell, main="Model2 WITH-STEMS (Combo3 GLM gamma-tuned)", cex.main=0.8)
hist(model2_glmGamma_100cell, main="Model2 WITH-STEMS (Combo3 GLM gamma-tuned)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_glmGamma_100cell, main="Model1 NO-STEMS (Combo3 GLM gamma-tuned)")
rasterVis::densityplot(model2_glmGamma_100cell, main="Model2 WITH-STEMS (Combo3 GLM gamma-tuned)")

#train-fitted
tuneResult_GLM_m2_full_train <- train(
  X_train_m2, y_train_m2,
  method = 'glmnet',
  trControl = fitControl_YeoJx1,
  tuneGrid = grid,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  metric='RMSE')

tuneResult_GLM_m1_full_train <- train(
  X_train_m1, y_train_m1,
  method = 'glmnet',
  trControl = fitControl_YeoJx1,
  tuneGrid = grid,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  metric='RMSE')

tuneResult_GLM_m2_train = glmnet(
  as.matrix(X_train_m2), y_train_m2,
  family = "gaussian",
  alpha=1, lambda=0.2375,
  relax = T)

tuneResult_GLM_m1_train = glmnet(
  as.matrix(X_train_m1), y_train_m1,
  family = "gaussian",
  alpha=1, lambda=0.2625,
  relax = T)


#test-fitted # tuneResult_GLM_m1_full
tunedModel_GLM_m2_test = predict(tuneResult_GLM_m2_train, newx = as.matrix(X_test_m2))
tunedModel_GLM_m1_test = predict(tuneResult_GLM_m1_train, newx = as.matrix(X_test_m1))
tunedModel_GLM_m2_test_MAE = MAE(tunedModel_GLM_m2_test, y_test_m2)
tunedModel_GLM_m1_test_MAE = MAE(tunedModel_GLM_m1_test, y_test_m1)
tunedModel_GLM_m2_test_RMSE = RMSE(tunedModel_GLM_m2_test, y_test_m2)
tunedModel_GLM_m1_test_RMSE = RMSE(tunedModel_GLM_m1_test, y_test_m1)

tunedModel_GLM_m2 = predict(tuneResult_GLM_m2_full, newx = as.matrix(X_m2))
tunedModel_GLM_m1 = predict(tuneResult_GLM_m1_full, newx = as.matrix(X_m1))
tunedModel_GLM_m2_MAE = MAE(tunedModel_GLM_m2, y_m2)
tunedModel_GLM_m1_MAE = MAE(tunedModel_GLM_m1, y_m1)
tunedModel_GLM_m2_RMSE = RMSE(tunedModel_GLM_m2, y_m2)
tunedModel_GLM_m1_RMSE = RMSE(tunedModel_GLM_m1, y_m1)

tunedModel_GLM_m2_MAE
tunedModel_GLM_m2_RMSE 
tunedModel_GLM_m2_test_MAE
tunedModel_GLM_m2_test_RMSE
tunedModel_GLM_m2_RMSE/tunedModel_GLM_m2_test_RMSE

tunedModel_GLM_m1_MAE
tunedModel_GLM_m1_RMSE 
tunedModel_GLM_m1_test_MAE
tunedModel_GLM_m1_test_RMSE
tunedModel_GLM_m1_RMSE/tunedModel_GLM_m1_test_RMSE



#svmRadial with epsilon
tuneResult_svm_m2_full_eps <- train(wsvha_L~., data=faib_vri_true_m2_df,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  ranges = list(
    epsilon = seq(0.02,0.1,0.2), 
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m1_full_eps = train(wsvha_L~., data=faib_vri_true_m1_df,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  ranges = list(
    epsilon = seq(0.02,0.1,0.2), 
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full_eps <- tuneResult_svm_m2_full_eps$finalModel
tunedModel_svm_m1_full_eps <- tuneResult_svm_m1_full_eps$finalModel
tunedModel_svm_m2_to_raster_eps <- raster::predict(covs_m2, tuneResult_svm_m2_full_eps)
tunedModel_svm_m1_to_raster_eps <- raster::predict(covs_m1, tuneResult_svm_m1_full_eps)
save(tunedModel_svm_m2_full_eps, file = "./Models/tunedModel_svmRadial_m2_10k_eps.RData")
save(tunedModel_svm_m1_full_eps, file = "./Models/tunedModel_svmRadial_m1_10k_eps.RData")
writeRaster(tunedModel_svm_m2_to_raster_eps, filename = "./Results/model1_svmRadial_100m_combo3_eps,tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_eps, filename = "./Results/model2_svmRadial_100m_combo3_eps.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full_eps)
print(tunedModel_svm_m1_full_eps)

par(mfrow = c(2,2))
model1_svmRadial_100cell_eps = raster::raster("./Results/model1_svmRadial_100m_combo3_eps.tif")
model2_svmRadial_100cell_eps = raster::raster("./Results/model2_svmRadial_100m_combo3_eps.tif")
plot(model1_svmRadial_100cell_eps, main="Model1 SVM-radial-eps NO-STEMS (Combo3 & Filter2)", cex.main=0.9)
hist(model1_svmRadial_100cell_eps, main="Model1 SVM-radial-eps NO-STEMS (Combo3 & Filter2)", cex.main=0.8, maxpixels=22000000) 
plot(model2_svmRadial_100cell_eps, main="Model2 SVM-radial-eps WITH-STEMS (Combo3 & Filter2)", cex.main=0.8)
hist(model2_svmRadial_100cell_eps, main="Model2 SVM-radial-eps WITH-STEMS (Combo3 & Filter2)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell_eps, main="Model1 NO-STEMS (Combo3 & Filter2)")
rasterVis::densityplot(model2_svmRadial_100cell_eps, main="Model2 WITH-STEMS (Combo3 & Filter2)")

tuneResult_svmRadial_m2_10k_train_eps <- train(X_train_m2, y_train_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  ranges = list(
    epsilon = seq(0.02,0.1,0.2), 
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svmRadial_m1_10k_train_eps <- train(X_train_m1, y_train_m1,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  ranges = list(
    epsilon = seq(0.02,0.1,0.2), 
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
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
tuneResult_svm_m2_full <- train(wsvha_L~., data=faib_vri_true_m2_df,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  ranges = list(
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m1_full <- train(wsvha_L~., data=faib_vri_true_m1_df,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  ranges = list(
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full <- tuneResult_svm_m2_full$finalModel
tunedModel_svm_m1_full <- tuneResult_svm_m1_full$finalModel
tunedModel_svm_m2_to_raster <- raster::predict(covs_m2, tuneResult_svm_m2_full)
tunedModel_svm_m1_to_raster <- raster::predict(covs_m1, tuneResult_svm_m1_full)
save(tunedModel_svm_m2_full, file = "./Models/tunedModel_svmRadial_m2_10k.RData")
save(tunedModel_svm_m1_full, file = "./Models/tunedModel_svmRadial_m1_10k.RData")
writeRaster(tunedModel_svm_m2_to_raster, filename = "./Results/model1_svmRadial_100m_combo3.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster, filename = "./Results/model2_svmRadial_100m_combo3.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full)
print(tunedModel_svm_m1_full)


par(mfrow = c(2,2))
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_combo3.tif")
model2_svmRadial_100cell = raster::raster("./Results/model2_svmRadial_100m_combo3.tif")
plot(model1_svmRadial_100cell, main="Model1 NO-STEMS (Combo3 SVMRad Eps-Free)", cex.main=0.9)
hist(model1_svmRadial_100cell, main="Model1 NO-STEMS (Combo3 SVMRad Eps-Free)", cex.main=0.8, maxpixels=22000000) 
plot(model2_svmRadial_100cell, main="Model2 WITH-STEMS (Combo3 SVMRad Eps-Free)", cex.main=0.8)
hist(model2_svmRadial_100cell, main="Model2 WITH-STEMS (Combo3 SVMRad Eps-Free)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell, main="Model1 NO-STEMS (Combo3 SVMRad Eps-Free)")
rasterVis::densityplot(model2_svmRadial_100cell, main="Model2 WITH-STEMS (Combo3 SVMRad Eps-Free)")

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
  wsvha_L~., data=faib_vri_true_m2_df,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear',
  metric = 'RMSE', 
  ranges = list(
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m1_full_linear <- train(
  wsvha_L~., data=faib_vri_true_m1_df,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear',
  metric = 'RMSE',
  ranges = list(
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full_linear <- tuneResult_svm_m2_full_linear$finalModel
tunedModel_svm_m1_full_linear <- tuneResult_svm_m1_full_linear$finalModel
tunedModel_svm_m2_to_raster_linear <- raster::predict(covs_m2, tuneResult_svm_m2_full_linear)
tunedModel_svm_m1_to_raster_linear <- raster::predict(covs_m1, tuneResult_svm_m1_full_linear)
save(tunedModel_svm_m2_full_linear, file = "./Models/tunedModel_svmRadial_m2_10k_linear.RData")
save(tunedModel_svm_m1_full_linear, file = "./Models/tunedModel_svmRadial_m1_10k_linear.RData")
writeRaster(tunedModel_svm_m2_to_raster_linear, filename = "./Results/model1_svmRadial_100m_combo3_linear.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_linear, filename = "./Results/model2_svmRadial_100m_combo3_linear.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full_linear)
print(tunedModel_svm_m1_full_linear)

par(mfrow = c(2,2))
model1_svmLinear_100cell = raster::raster("./Results/model1_svmRadial_100m_combo3_linear.tif")
model2_svmLinear_100cell = raster::raster("./Results/model2_svmRadial_100m_combo3_linear.tif")
plot(model1_svmLinear_100cell, main="Model1 NO-STEMS (Combo3 SVMLin Eps-Free)", cex.main=0.9)
hist(model1_svmLinear_100cell, main="Model1 NO-STEMS (Combo3 SVMLin Eps-Free)", cex.main=0.8, maxpixels=22000000) 
plot(model2_svmLinear_100cell, main="Model2 WITH-STEMS (Combo3 SVMLin Eps-Free)", cex.main=0.8)
hist(model2_svmLinear_100cell, main="Model2 WITH-STEMS (Combo3 SVMLin Eps-Free)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmLinear_100cell, main="Model1 NO-STEMS (Combo3 SVMLin Eps-Free)")
rasterVis::densityplot(model2_svmLinear_100cell, main="Model2 WITH-STEMS (Combo3 SVMLin Eps-Free)")

tuneResult_svmLinear_m2_10k_train <- train(
  X_train_m2, y_train_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear',
  metric = 'RMSE',
  ranges = list(
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svmLinear_m1_10k_train <- train(
  X_train_m1, y_train_m1,
  trControl = fitControl_YeoJx1,
  method = 'svmLinear',
  metric = 'RMSE',
  ranges = list(
    cost = c(1,5,7,15,20), 
    gamma = 2^(-1:1)),
  tuneLength = 10,
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

tunedModel_svmLinear_m2_MAE
tunedModel_svmLinear_m2_RMSE 
tunedModel_svmLinear_m2_test_MAE
tunedModel_svmLinear_m2_test_RMSE
tunedModel_svmLinear_m2_RMSE/tunedModel_svmLinear_m2_test_RMSE

tunedModel_svmLinear_m1_MAE
tunedModel_svmLinear_m1_RMSE 
tunedModel_svmLinear_m1_test_MAE
tunedModel_svmLinear_m1_test_RMSE
tunedModel_svmLinear_m1_RMSE/tunedModel_svmLinear_m1_test_RMSE
