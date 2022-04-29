
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
library(ForestTools)
library(lidR)
library(e1071)
library(caret)
library(tibble)
library(MASS)
set.seed(123)

# Import ground plot data
faib_psp <- utils::read.csv("./Data/FAIB_PSP_20211028.csv")
print(as_tibble(faib_psp), n = 10)

# Import LiDAR and derive CHM-covariates
lead_htop_raster_1m = raster::raster("./Data/Raster_Covariates/lead_htop_raster.tif")
lead_htop_rast_1m = terra::rast(lead_htop_raster_1m)
terra::crs(lead_htop_rast_1m) = "epsg:3005"
plot(lead_htop_rast_1m)

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

lead_htop_rast_1m_smoothed = terra::focal(lead_htop_rast_1m, w = kernel, fun = median, na.rm = TRUE)
lead_htop_raster_1m_smoothed = raster::raster(lead_htop_rast_1m_smoothed)

ttops_2m_Quan = ForestTools::vwf(CHM = lead_htop_raster_1m_smoothed, winFun = wf_Quan, minHeight = 2)
#crowns_2mTO1.5m_Quan <- ForestTools::mcws(treetops = ttops_2m_Quan, CHM = lead_htop_raster_1m_smoothed, minHeight = 1.5, verbose = FALSE)
#crownsPoly_2mTO1.5m_Quan <- ForestTools::mcws(treetops = ttops_2m_Quan, CHM = lead_htop_raster_1m_smoothed, format = "polygons", minHeight = 1.5, verbose = FALSE)

#plot(ttops_2m_Quan, cex = 0.0001, pch=19, col = 'blue', alpha=0.4)
#plot(crowns_2mTO1.5m_Quan, col = sample(rainbow(50), length(unique(crowns_2mTO1.5m_Quan[])), replace = TRUE), legend = FALSE)
#plot(crownsPoly_2mTO1.5m_Quan, border = "blue", lwd = 0.001)

quant95 <- function(x, ...) 
  quantile(x, c(0.95), na.rm = TRUE)
custFuns <- list(quant95, max)
names(custFuns) <- c("95thQuantile", "Max")

ttops_2m_Quan_raster_2m1.5m_95th_1cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 1, variables = "height", statFuns = custFuns)
ttops_2m_Quan_raster_2m1.5m_95th_10cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 10, variables = "height", statFuns = custFuns)
ttops_2m_Quan_raster_2m1.5m_95th_20cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 20, variables = "height", statFuns = custFuns)
ttops_2m_Quan_raster_2m1.5m_95th_50cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 50, variables = "height", statFuns = custFuns)
ttops_2m_Quan_raster_2m1.5m_95th_100cell <- ForestTools::sp_summarise(ttops_2m_Quan, grid = 100, variables = "height", statFuns = custFuns)

lead_htop_ttops_1cell = ttops_2m_Quan_raster_2m1.5m_95th_1cell[["height95thQuantile"]]
lead_htop_ttops_10cell = ttops_2m_Quan_raster_2m1.5m_95th_10cell[["height95thQuantile"]]
lead_htop_ttops_20cell = ttops_2m_Quan_raster_2m1.5m_95th_20cell[["height95thQuantile"]]
lead_htop_ttops_50cell = ttops_2m_Quan_raster_2m1.5m_95th_50cell[["height95thQuantile"]]
lead_htop_ttops_100cell = ttops_2m_Quan_raster_2m1.5m_95th_100cell[["height95thQuantile"]]

stemsha_L_ttops_1cell = ttops_2m_Quan_raster_2m1.5m_95th_1cell[["TreeCount"]]
stemsha_L_ttops_10cell = ttops_2m_Quan_raster_2m1.5m_95th_10cell[["TreeCount"]]
stemsha_L_ttops_20cell = ttops_2m_Quan_raster_2m1.5m_95th_20cell[["TreeCount"]]
stemsha_L_ttops_50cell = ttops_2m_Quan_raster_2m1.5m_95th_50cell[["TreeCount"]]
stemsha_L_ttops_100cell = ttops_2m_Quan_raster_2m1.5m_95th_100cell[["TreeCount"]]

raster::writeRaster(lead_htop_ttops_1cell, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_ttops_1cell.tif", overwrite=TRUE)
raster::writeRaster(lead_htop_ttops_10cell, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_ttops_10cell.tif", overwrite=TRUE)
raster::writeRaster(lead_htop_ttops_20cell, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_ttops_20cell.tif", overwrite=TRUE)
raster::writeRaster(lead_htop_ttops_50cell, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_ttops_50cell.tif", overwrite=TRUE)
raster::writeRaster(lead_htop_ttops_100cell, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_ttops_100cell.tif", overwrite=TRUE)

raster::writeRaster(stemsha_L_ttops_1cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_1cell.tif", overwrite=TRUE)
raster::writeRaster(stemsha_L_ttops_10cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_10cell.tif", overwrite=TRUE)
raster::writeRaster(stemsha_L_ttops_20cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_20cell.tif", overwrite=TRUE)
raster::writeRaster(stemsha_L_ttops_50cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_50cell.tif", overwrite=TRUE)
raster::writeRaster(stemsha_L_ttops_100cell, filename = "./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_100cell.tif", overwrite=TRUE)

lead_htop = raster::raster("./Data/Raster_Covariates/UnMasked/lead_htop_ttops_100cell.tif")
stemsha_L = raster::raster("./Data/Raster_Covariates/UnMasked/stemsha_L_ttops_100cell.tif")
lead_htop_rast = rast(lead_htop)
stemsha_L_rast = rast(stemsha_L)
crs(lead_htop_rast) = "epsg:3005"
crs(stemsha_L_rast) = "epsg:3005"
# lead_htop_rast = terra::mask(lead_htop_rast, vect(aoi_sf))
lead_htop_sv = as.polygons(lead_htop_rast)
lead_htop_sf = sf::st_as_sf(lead_htop_sv)
plot(lead_htop_rast)

# Import VRI & Mask layers
vri_sf = sf::read_sf("./Data/VEG_COMP_LYR_R1_POLY/VEG_R1_PLY_polygon.shp")
vri_species = vri_sf[c("SPEC_CD_1", "SPEC_PCT_1")]
vri_species_aoi = st_intersection(vri_species, st_make_valid(lead_htop_sf))
vri_species_aoi = vri_species_aoi[c("SPEC_CD_1", "SPEC_PCT_1")]
vri_species_aoi =  dplyr::filter(vri_species_aoi, SPEC_CD_1=='PL' | SPEC_CD_1=='PLI' | SPEC_CD_1=='SE' | SPEC_CD_1=='SW' | SPEC_CD_1=='SX' | SPEC_CD_1=='FD' | SPEC_CD_1=='FDI')
vri_species_aoi_df = as_tibble(vri_species_aoi[!(vri_species_aoi$SPEC_CD_1 == 'FD' & vri_species_aoi$SPEC_PCT_1 >= 50 | vri_species_aoi$SPEC_CD_1 == 'FDI' & vri_species_aoi$SPEC_PCT_1 >=50),])
vri_species_aoi_df$SPEC_CD_1 = dplyr::recode(vri_species_aoi_df$SPEC_CD_1, PL = 0, PLI = 0, SE = 1, SW = 1, SX = 1, FD = 2, FDI = 2)
vri_species_aoi_df = dplyr::rename(vri_species_aoi_df, species_class = SPEC_CD_1)
vri_species_aoi = sf::st_as_sf(vri_species_aoi_df)
vri_species_aoi = vri_species_aoi["species_class"]

species_class_rast = terra::rasterize(vect(vri_species_aoi), lead_htop_rast, field = "species_class", touches = TRUE)
species_class_raster = raster::raster(species_class_rast)
writeRaster(species_class_raster, filename = "./Data/Raster_Covariates/UnMasked/species_class_raster.tif", overwrite=TRUE)
species_class_raster = raster::raster("./Data/Raster_Covariates/UnMasked/species_class_raster.tif")
plot(species_class_rast, main = "species_class_raster")

mask_burn2017 = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/TCC_Burn_Severity TCC_Burn_Severity_2017.shp")
mask_burn2017 = mask_burn2017["BurnSev"]
mask_burn2017 = dplyr::filter(mask_burn2017, BurnSev == 'High')
mask_burn2017 = sf::st_intersection(sf::st_make_valid(mask_burn2017), lead_htop_sf)
mask_burn2018 = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/TCC_Burn_Severity TCC_Burn_Severity_2018.shp")
mask_burn2018 = mask_burn2018["BurnSev"]
mask_burn2018 = dplyr::filter(mask_burn2018, BurnSev == 'High')
mask_burn2018 = sf::st_intersection(sf::st_make_valid(mask_burn2018), lead_htop_sf)
mask_burn2021 = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/TCC_Burn_Severity TCC_Burn_Severity_2021.shp")
mask_burn2021 = mask_burn2021["BurnSev"]
mask_burn2021 = dplyr::filter(mask_burn2021, BurnSev == 'High')
mask_burn2021 = sf::st_intersection(sf::st_make_valid(mask_burn2021), lead_htop_sf)
masks_df = full_join(as_tibble(mask_burn2017), as_tibble(mask_burn2018), as_tibble(mask_burn2021), by = "geometry")
masks_sf = st_as_sf(masks_df) # easier to combine by 'geometry'
ggplot(masks_sf) + geom_sf(size = 0.0005)

mask_clearcut = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/RSLT_CCRES_CLEAR.shp")
mask_clearcut = sf::st_intersection(mask_clearcut, st_make_valid(lead_htop_sf))
masks_df = full_join(as_tibble(masks_sf), as_tibble(mask_clearcut), by = 'geometry')
masks_sf = st_as_sf(masks_df)

mask_blocks = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/TCC_Blocks_Join.shp")
mask_blocks = sf::st_intersection(mask_blocks, st_make_valid(lead_htop_sf))
masks_df = full_join(as_tibble(masks_sf), as_tibble(mask_blocks), by = 'geometry')
masks_sf = st_as_sf(masks_df)
ggplot(masks_sf) + geom_sf()

mask_roads_tcc = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/TCC_Roads.shp")
mask_roads_tcc = sf::st_zm(mask_roads_tcc)
mask_roads_tcc = sf::st_intersection(mask_roads_tcc, st_make_valid(lead_htop_sf))
mask_roads_tcc = sf::st_buffer(mask_roads_tcc, dist = 15, nQuadSegs = 5, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 2)
mask_roads_ften = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/FTEN_Roads_All.shp")
mask_roads_ften = sf::st_zm(mask_roads_ften)
mask_roads_ften = sf::st_intersection(mask_roads_ften, st_make_valid(lead_htop_sf))
mask_roads_ften = sf::st_buffer(mask_roads_ften, dist = 15, nQuadSegs = 5, endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 2)
masks_df = full_join(as_tibble(masks_sf), as_tibble(mask_roads_tcc), as_tibble(mask_roads_ften), by = 'geometry')
masks_sf = st_as_sf(masks_df)
ggplot(masks_sf) + geom_sf(aes(fill = 'red'), show.legend = FALSE)

masks_rast = rasterize(vect(masks_sf), raster_template, touches = TRUE)
masks_raster = raster::raster(masks_rast)
writeRaster(masks_raster, filename = "./Data/Raster_Covariates/UnMasked/masks_raster.tif", overwrite=TRUE)
masks_raster = raster::raster("./Data/Raster_Covariates/UnMasked/masks_raster.tif")
masks_rast = rast(masks_raster)
plot(masks_rast)

# Import LiDAR and derive DEM-based covariates 
elev_raster = raster::raster("./Data/Raster_Covariates/elev_raster.tif")
elev_rast = terra::rast(elev_raster)
terra::crs(elev_rast) = "epsg:3005"
elev_rast = terra::aggregate(elev_rast, fact = 100, fun = mean)

slope_rast = terra::terrain(elev_rast, v="slope", unit="degrees", neighbors=8)
aspect_rast = terra::terrain(elev_rast, v="aspect", unit="degrees", neighbors=8)
asp_cos_rast = cos((aspect_rast*pi)/180)
asp_sin_rast = sin((aspect_rast*pi)/180)

lead_htop_rast = terra::resample(lead_htop_rast, elev_rast)
stemsha_L_rast = terra::resample(stemsha_L_rast, elev_rast)
elev_rast = terra::mask(elev_rast, lead_htop_rast)
slope_rast = terra::mask(slope_rast, lead_htop_rast)
asp_cos_rast = terra::mask(asp_cos_rast, lead_htop_rast)
asp_sin_rast = terra::mask(asp_sin_rast, lead_htop_rast)
species_class_rast = terra::mask(species_class_rast, lead_htop_rast)

plot(species_class_rast, main = "species_class")
plot(elev_rast, main = "elevation")
plot(slope_rast, main = "slope")
plot(asp_cos_rast, main = "asp_cos")
plot(asp_sin_rast, main = "asp_sin")

writeRaster(elev_rast, filename = "./Data/Raster_Covariates/UnMasked/elev_raster.tif", overwrite=TRUE)
writeRaster(slope_rast, filename = "./Data/Raster_Covariates/UnMasked/slope_raster.tif", overwrite=TRUE)
writeRaster(asp_cos_rast, filename = "./Data/Raster_Covariates/UnMasked/asp_cos_raster.tif", overwrite=TRUE)
writeRaster(asp_sin_rast, filename = "./Data/Raster_Covariates/UnMasked/asp_sin_raster.tif", overwrite=TRUE)
writeRaster(species_class_rast, filename = "./Data/Raster_Covariates/UnMasked/species_class_raster.tif", overwrite=TRUE)
species_class = raster::raster("./Data/Raster_Covariates/UnMasked/species_class_raster.tif")
elev = raster::raster("./Data/Raster_Covariates/UnMasked/elev_raster.tif")
slope = raster::raster("./Data/Raster_Covariates/UnMasked/slope_raster.tif")
asp_cos = raster::raster("./Data/Raster_Covariates/UnMasked/asp_cos_raster.tif")
asp_sin = raster::raster("./Data/Raster_Covariates/UnMasked/asp_sin_raster.tif")


# Derive CHM-based covariates: lidR Pipeline
#opt_output_files(lead_htop_rast_1m_smoothed) = paste0(tempdir(), "./Data/lead_htop_stemMapping")
#opt_output_files(lead_htop_raster_1m_smoothed) = paste0(tempdir(), "./Data/lead_htop_stemMapping")

#ttops_Quan_lidR_find_trees = lidR::find_trees(lead_htop_rast_1m_smoothed, lmf(wf_Quan)) #SpatialPointsDataFrame object
#ttops_Quan_lidR_locate_trees = lidR::locate_trees(lead_htop_rast_1m_smoothed, lmf(wf_Quan)) #SimpleFeatures object
#ttops_2m_Quan_sf = st_as_sf(ttops_2m_Quan)

#lead_htop_raster_1m_smoothed_memory = readAll(lead_htop_raster_1m_smoothed)
#crowns_Quan_lidR_locate_trees = lidR::li2012(lead_htop_raster_1m_smoothed_memory, ttops_2m_Quan_sf)()

#crowns_Quan_lidR_locate_trees_sv = terra::as.points(crowns_Quan_lidR_locate_trees)
#stemsha_L_rast_lidR_segmented_20cell = terra::rasterize(crowns_Quan_lidR_locate_trees_sv, lead_htop_20, fun = length, touches = TRUE)
#stemsha_L_rast_lidR_segmented_50cell = terra::rasterize(crowns_Quan_lidR_locate_trees_sv, lead_htop_50, fun = length, touches = TRUE)
#stemsha_L_rast_lidR_segmented_100cell = terra::rasterize(crowns_Quan_lidR_locate_trees_sv, lead_htop_100, fun = length, touches = TRUE)
#stemsha_L_raster_lidR_segmented_20cell = raster::raster(stemsha_L_rast_lidR_segmented_20cell)
#stemsha_L_raster_lidR_segmented_50cell = raster::raster(stemsha_L_rast_lidR_segmented_50cell)
#stemsha_L_raster_lidR_segmented_100cell = raster::raster(stemsha_L_rast_lidR_segmented_100cell)
#writeRaster(lead_htop_rast, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_rast.tif", overwrite=TRUE)

# Tidy raster covariates
# lead_htop_rast = terra::rast(lead_htop)
# stemsha_L_rast = terra::rast(stemsha_L)
# elev_rast = terra::rast(elev)
# slope_rast = terra::rast(slope)
# asp_cos_rast = terra::rast(asp_cos)
# asp_sin_rast = terra::rast(asp_sin)
# species_class_rast = terra::rast(species_class)
# masks_rast = terra::rast(masks_raster)

#elev_rast = terra::aggregate(elev_rast, fact = 5, fun = mean)
#slope_rast = terra::aggregate(slope_rast, fact = 5, fun = mean)
#asp_cos_rast = terra::aggregate(asp_cos_rast, fact = 5, fun = mean)
#asp_sin_rast = terra::aggregate(asp_sin_rast, fact = 5, fun = mean)
#species_class_rast = terra::aggregate(species_class_rast, fact = 5, fun = mean)

# lead_htop_rast = terra::mask(lead_htop_rast, vect(aoi_sf))
# stemsha_L_rast = terra::mask(stemsha_L_rast, vect(aoi_sf))

# crs(lead_htop_rast) = "epsg:3005"
# crs(stemsha_L_rast) = "epsg:3005"
# crs(elev_rast) = "epsg:3005"
# crs(slope_rast) = "epsg:3005"
# crs(asp_cos_rast) = "epsg:3005"
# crs(asp_sin_rast) = "epsg:3005"
# crs(species_class_rast) = "epsg:3005"

plot(elev_rast)
plot(lead_htop_rast)
plot(species_class)
plot(masks_rast)
plot(slope_rast)
plot(asp_cos_rast)
plot(asp_sin_rast)
plot(stemsha_L_rast)


# slope_rast = terra::resample(slope_rast, lead_htop_rast, method="bilinear")
# asp_cos_rast = terra::resample(asp_cos_rast, lead_htop_rast, method="bilinear")
# asp_sin_rast = terra::resample(asp_sin_rast, lead_htop_rast, method="bilinear")
# species_class_rast = terra::resample(species_class_rast, lead_htop_rast, method="near")

# mask by >3m canopy 
# lead_htop_rast[lead_htop_rast < 3] = NA
# elev_rast = mask(elev_rast, lead_htop_rast, inverse=FALSE)
# slope_rast = mask(slope_rast, lead_htop_rast, inverse=FALSE)
# asp_cos_rast = mask(asp_cos_rast, lead_htop_rast, inverse=FALSE)
# asp_sin_rast = mask(asp_sin_rast, lead_htop_rast, inverse=FALSE)
# species_class_rast = mask(species_class_rast, lead_htop_rast, inverse=FALSE)
# stemsha_L_rast = mask(stemsha_L_rast, lead_htop_rast, inverse=FALSE)

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
names(covs_m2)

#faib_psp = faib_psp[!(faib_psp$SPEC_PCT_1 == 'FD' & faib_psp$SPEC_PCT_1 >= 50 | faib_psp$SPEC_CD_1 == 'FDI' & faib_psp$SPEC_PCT_1 >=50),])

# Tidy ground plot data
faib_psp$spc_live1 = as.factor(faib_psp$spc_live1)
base::table(faib_psp$spc_live1, faib_psp$beclabel)
faib_psp =  subset(faib_psp, spc_live1=='PL' | spc_live1=='SB' | spc_live1=='SE' | spc_live1=='SX' | spc_live1=='FD')
faib_psp$species_class = dplyr::recode(faib_psp$spc_live1, PL = 0, SB = 1, SE = 1, SX = 1, FD = 2)
faib_psp = faib_psp[!(faib_psp$species_class==2 & faib_psp$bgc_zone == 'SBS' | faib_psp$species_class==2 & faib_psp$bgc_zone =='SBPS' | faib_psp$species_class==2 & faib_psp$bgc_zone =='ICH'),]
# | faib_psp$species_class==2 & faib_psp$beclabel =='IDFdw' | faib_psp$species_class==2 & faib_psp$beclabel =='IDFxm'),]
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

# fit models: model1_svmRadial (data transformations: boxcox, center, scale)
tuneResult_svm_m2_full <- tune(svm, X_m2, y_m2, ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
                               tunecontrol = tune.control(cross = 10),preProcess = c("BoxCox","center","scale"))
tunedModel_svm_m2_full <- tuneResult_svm_m2_full$best.model
save(tunedModel_svm_m2_full, file = "./Models/model1_svmRadial_100m_april21.tif")
tunedModel_svm_m2 = predict(
  tunedModel_svm_m2_full,
  data=faib_vri_true_m2_df)

# writeRaster and plot outputs
tunedModel_svm_m2_to_raster <- raster::predict(covs_m2, tunedModel_svm_m2_full)
writeRaster(tunedModel_svm_m2_to_raster, filename = "./Results/model1_svmRadial_100m_april21.tif", overwrite=TRUE)
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_april21.tif")

graphics.off()
par(mfrow = c(1,2))
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox, centre, scale)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox, centre, scale)", maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox, centre, scale)")

# plot faib vs raster values
graphics.off()
par(mfrow = c(4, 4)) 
truehist(faib_vri_true_m1_df$elev, main="Elevation (faib)", maxpixels=22000000)
hist(elev_raster, main="Elevation (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$slope, main="Slope (faib)", maxpixels=22000000)
hist(slope_raster, main="Slope (raster)", maxpixels=22000000) 
truehist(faib_vri_true_m1_df$asp_cos, main="Northness (faib)", maxpixels=22000000)
hist(asp_cos_raster, main="Northness (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$asp_sin, main="Eastness (faib)", maxpixels=22000000)
hist(asp_sin_raster, main="Eastness (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$stemsha_L, main="Stems/ha (faib)", maxpixels=22000000)
hist(stemsha_L_raster, main="Stems/ha (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$species_class, main="Lead Species (faib)", maxpixels=22000000)
hist(species_class_raster, main="Lead Species (raster)", maxpixels=22000000)
truehist(faib_vri_true_m1_df$lead_htop, main="Mean Tree Height (faib)", maxpixels=22000000)
hist(lead_htop_raster, main="Mean Tree Height (raster)", maxpixels=22000000) 
truehist(tunedModel_svm_m2, main="Whole Stem Vol (faib predicted)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster predicted)", maxpixels=22000000) 

graphics.off()
par(mfrow = c(1, 3)) 
hist(faib_vri_true_m1_df$wsvha_L, main="Whole Stem Vol (faib fitted)", maxpixels=22000000)
hist(tunedModel_svm_m2, main="Whole Stem Vol (faib predicted)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster predicted)", maxpixels=22000000) 


# Applying new caret pre-processing transformations:
# Near-zero-variance filter
preproc_nzv <- preProcess(faib_vri_true_m2_df[, -7], method = c("center", "scale", "BoxCox", "nzv"))
preproc_nzv

preproc_yj <- preProcess(faib_vri_true_m2_df[, -7], method = c("center", "scale", "YeoJohnson"))
preproc_yj
X_m2_transform_comp = predict(preproc_nzv_yj, newdata=faib_vri_true_m2_df[,-7])

# fit models: model1_svmRadial (with BoxCox and YeoJohnson transformations)
fitControl_YeoJx1 = caret::trainControl(method="repeatedcv", number=10, repeats=1)
fitControl_YeoJx3 = caret::trainControl(method="repeatedcv", number=10, repeats=3)

tuneResult_svm_m2_full <- train(wsvha_L~., data=faib_vri_true_m2_df,
                                trControl = fitControl_YeoJx1,
                                method = 'svmRadial',
                                metric = 'RMSE',
                                tuneLength = 10,
                                preProc = c('YeoJohnson', 'scale'),
                                verbose=F)

tuneResult_svm_m2_full <- tune(svm, X_m2, y_m2, 
                               ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
                               tunecontrol = tune.control(cross = 10, nrepeat = 3),
                               preProcess = c("center", "scale", "BoxCox"))

#tuneResult_svm_m2_full <- tune(svm, X_m2, y_m2, 
#                               ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
#                               tunecontrol = tune.control(cross = 10, nrepeat = 3),
#                               preProcess = c("center", "scale", "YeoJohnson"))


tunedModel_svm_m2_full <- tuneResult_svm_m2_full$best.model
save(tunedModel_svm_m2_full, file = "./Models/model1_svmRadial_100m_boxcox_april22.tif")
# writeRaster and plot outputs
tunedModel_svm_m2_to_raster <- raster::predict(covs_m2, tunedModel_svm_m2_full)
writeRaster(tunedModel_svm_m2_to_raster, filename = "./Results/model1_svmRadial_100m_boxcox_april22.tif", overwrite=TRUE)
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_boxcox_april22.tif")
par(mfrow = c(1,2))
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ centre, scale, YeoJohnson)", cex.main=0.8)
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ centre, scale, YeoJohnson)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox)")

# fit models: model1_svmRadial (data transformations: center)
tuneResult_svm_m2_full <- tune(svm, X_m2, y_m2, ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
                               tunecontrol = tune.control(cross = 10),preProcess = c("center"))
tunedModel_svm_m2_full <- tuneResult_svm_m2_full$best.model
save(tunedModel_svm_m2_full, file = "./Models/model1_svmRadial_100m_center_april21.tif")
# writeRaster and plot outputs
tunedModel_svm_m2_to_raster <- raster::predict(covs_m2, tunedModel_svm_m2_full)
writeRaster(tunedModel_svm_m2_to_raster, filename = "./Results/model1_svmRadial_100m_center_april21.tif", overwrite=TRUE)
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_center_april21.tif")
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ center)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ center)", maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ center)")

# fit models: model1_svmRadial (data transformations: scale)
tuneResult_svm_m2_full <- tune(svm, X_m2, y_m2, ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
                               tunecontrol = tune.control(cross = 10), preProcess = c("scale"))
tunedModel_svm_m2_full <- tuneResult_svm_m2_full$best.model
save(tunedModel_svm_m2_full, file = "./Models/model1_svmRadial_100m_scale_april21.tif")
# writeRaster and plot outputs
tunedModel_svm_m2_to_raster <- raster::predict(covs_m2, tunedModel_svm_m2_full)
writeRaster(tunedModel_svm_m2_to_raster, filename = "./Results/model1_svmRadial_100m_scale_april21.tif", overwrite=TRUE)
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_scale_april21.tif")
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ scale)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ scale)", maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ scale)")

par(mfrow = c(4, 2))
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_april04.tif")
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox, centre, scale)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox, centre, scale)", maxpixels=22000000) 
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_boxcox_april21.tif")
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox)", maxpixels=22000000) 
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_center_april21.tif")
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ center)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ center)", maxpixels=22000000) 
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_scale_april21.tif")
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ scale)")
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ scale)", maxpixels=22000000) 

par(mfrow = c(4,1))
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox, centre, scale)")
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox)")
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ center)")
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ scale)")



# generate mask to normalize negative estimates
model1_svmRadial_positives_100cell_mask = model1_svmRadial_100cell > 0
writeRaster(model1_svmRadial_positives_100cell_mask, filename = "./Results/model1_svmRadial_100m_positives_mask_april04.tif", overwrite=TRUE)
model1_svmRadial_positives_100cell_mask = raster::raster("./Results/model1_svmRadial_100m_positives_mask_april04.tif")
plot(model1_svmRadial_positives_100cell_mask)
# mask WSVHA raster with 'positives' mask above
#model1_svmRadial_100cell_rast = terra::rast(model1_svmRadial_100cell)
#model1_svmRadial_positives_100cell_rast = terra::rast(model1_svmRadial_positives_100cell)
#model1_svmRadial_positives_only_100cell = terra::mask(model1_svmRadial_positives_100cell_rast, model1_svmRadial_positives_100cell_rast, inverse=FALSE)
#writeRaster(model1_svmRadial_positives_only_100cell, filename = "./Results/model1_svmRadial_positives_only_100cell_april04.tif", overwrite=TRUE)
#model1_svmRadial_positives_only_100cell = raster::raster("./Results/model1_svmRadial_positives_only_100cell_april04.tif")
#plot(model1_svmRadial_positives_100cell)
#plot(model1_svmRadial_positives_only_100cell)
