
library(rgdal)
library(tibble)
library(conflicted)
library(sf)
library(terra)
library(raster)
library(ggplot2)
library(ForestTools)
library(lidR)
library(e1071)
library(caret)

# Import ground plot data
faib_psp <- utils::read.csv("./Data/FAIB_PSP_20211028.csv")
print(as_tibble(faib_psp), n = 10)

# Import AOi boundary
aoi_sf <- sf::read_sf("./Data/BCTS_OPERATING_AREAS_SP/BCTS_OP_AR_polygon.shp")
aoi_sf = dplyr::rename(aoi_sf, AOI_Boundary = SHAPE)
aoi_sf = aoi_sf[1, "AOI_Boundary"]
plot(aoi_sf)

# Import VRI layers
vri_sf = sf::read_sf("./Data/VEG_COMP_LYR_R1_POLY/VEG_R1_PLY_polygon.shp")
vri_species_aoi = vri_sf["SPEC_CD_1"]
vri_species_aoi = st_intersection(st_make_valid(vri_species_aoi), aoi_sf)
ggplot(vri_species_aoi) + geom_sf(aes(fill=SPEC_CD_1), size = 0.0005)
vri_species_aoi =  dplyr::filter(vri_species_aoi, SPEC_CD_1=='BL' | SPEC_CD_1=='FD' | SPEC_CD_1=='FDI' 
                                 | SPEC_CD_1=='PL' | SPEC_CD_1=='PLI' | SPEC_CD_1=='SE' | SPEC_CD_1=='SW' | SPEC_CD_1=='SX')
vri_species_aoi = dplyr::rename(vri_species_aoi, species_class = SPEC_CD_1)
vri_species_aoi$species_class = dplyr::recode(vri_species_aoi$species_class,
                                              PL = 0, PLI = 0, SE = 1, SW = 1, SX = 1, FD = 2, FDI = 2, BL = 5)
vri_species_aoi$species_class = as.factor(vri_species_aoi$species_class)
summary.factor(vri_species_aoi$species_class)
vri_species_aoi = vri_species_aoi["species_class"]
raster_template = rast(ext(aoi_sf), resolution = 20, crs = st_crs(aoi_sf)$wkt)
species_class_rast = rasterize(vect(vri_species_aoi), raster_template, field = "species_class", touches = TRUE)
plot(species_class_rast, main = "species_class_raster")
writeRaster(species_class_rast, filename = "./Data/Raster_Covariates/UnMasked/species_class_raster.tif", overwrite=TRUE)

# Import LiDAR and derive DEM-based covariates
lead_htop_raster = raster::raster("./Data/Raster_Covariates/lead_htop_raster.tif")
elev_raster = raster::raster("./Data/Raster_Covariates/elev_raster.tif")
lead_htop_rast = terra::rast(lead_htop_raster)
elev_rast = terra::rast(elev_raster)
terra::crs(lead_htop_rast) = "epsg:3005"
terra::crs(elev_rast) = "epsg:3005"
lead_htop_rast = terra::aggregate(lead_htop_rast, fact = 20, fun = mean)
elev_rast = terra::aggregate(elev_rast, fact = 20, fun = mean)
slope_rast = terra::terrain(elev_rast, v="slope", unit="degrees", neighbors=8)
aspect_rast = terra::terrain(elev_rast, v="aspect", unit="degrees", neighbors=8)
asp_cos_rast = cos((aspect_rast*pi)/180)
asp_sin_rast = sin((aspect_rast*pi)/180)

lead_htop_rast = terra::mask(lead_htop_rast, vect(aoi_sf))
elev_rast = terra::mask(elev_rast, vect(aoi_sf))
slope_rast = terra::mask(slope_rast, vect(aoi_sf))
asp_cos_rast = terra::mask(asp_cos_rast, vect(aoi_sf))
asp_sin_rast = terra::mask(asp_sin_rast, vect(aoi_sf))
species_class_rast = terra::resample(species_class_rast, elev_rast, method="near")
species_class_rast = terra::mask(species_class_rast, elev_rast)
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

# Derive CHM-based covariates
kernel <- matrix(1,3,3)
lead_htop_rast <- terra::focal(lead_htop_rast, w = kernel, fun = median, na.rm = TRUE)
lead_htop_raster = raster::raster(lead_htop_rast)
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

# Derive CHM-based covariates: ForestTools pipeline
ttops_3m_Quan = ForestTools::vwf(CHM = lead_htop_raster, winFun = wf_Quan, minHeight = 3)
ttops_2m_Quan = ForestTools::vwf(CHM = lead_htop_raster, winFun = wf_Quan, minHeight = 2)
ttops_3m_Plowright = ForestTools::vwf(CHM = lead_htop_raster, winFun = wf_Plowright, minHeight = 3)

plot(lead_htop_raster)
mypalette = RColorBrewer::brewer.pal(8,"Greens")
plot(lead_htop_raster, col = mypalette, alpha=0.8)
plot(ttops_3m_Quan, add=TRUE, cex = 0.0001, pch=19, col = 'blue', alpha=0.4)
plot(lead_htop_raster, col = mypalette, alpha=0.8)
plot(ttops_2m_Quan, add=TRUE, cex = 0.0001, pch=19, col = 'blue', alpha=0.4)

crowns_3mTO2m_Quan <- ForestTools::mcws(treetops = ttops_3m_Quan, CHM = lead_htop_raster, minHeight = 2, verbose = FALSE)
crowns_2mTO1.5m_Quan <- ForestTools::mcws(treetops = ttops_2m_Quan, CHM = lead_htop_raster, minHeight = 1.5, verbose = FALSE)
crownsPoly_3mTO2m_Quan <- ForestTools::mcws(treetops = ttops_3m_Quan, CHM = lead_htop_raster, format = "polygons", minHeight = 2, verbose = FALSE)
crownsPoly_2mTO1.5m_Quan <- ForestTools::mcws(treetops = ttops_2m_Quan, CHM = lead_htop_raster, format = "polygons", minHeight = 1.5, verbose = FALSE)

plot(lead_htop_raster)
plot(crowns_3mTO2m_Quan, col = sample(rainbow(50), length(unique(crowns_3mTO2m_Quan[])), replace = TRUE), legend = FALSE)
plot(lead_htop_raster)
plot(crowns_2mTO1.5m_Quan, col = sample(rainbow(50), length(unique(crowns_2mTO1.5m_Quan[])), replace = TRUE), legend = FALSE)

plot(lead_htop_raster)
plot(crownsPoly_3mTO2m_Quan, border = "blue", lwd = 0.001, add = TRUE)
plot(lead_htop_raster)
plot(crownsPoly_2mTO1.5m_Quan, border = "blue", lwd = 0.001, add = TRUE)

quant95 <- function(x, ...) quantile(x, c(.95), na.rm = TRUE)
custFuns <- list(quant95, max)
names(custFuns) <- c("95thQuantile", "Max")
crownsPoly_3mTO2m_Quan_gridStats_20cell <- ForestTools::sp_summarise(crownsPoly_3mTO2m_Quan, grid = 20, variables = "height", statFuns = custFuns)
crownsPoly_3mTO2m_Quan_gridStats_50cell <- ForestTools::sp_summarise(crownsPoly_3mTO2m_Quan, grid = 50, variables = "height", statFuns = custFuns)
crownsPoly_3mTO2m_Quan_gridStats_100cell <- ForestTools::sp_summarise(crownsPoly_3mTO2m_Quan, grid = 100, variables = "height", statFuns = custFuns)

crownsPoly_2mTO1.5m_Quan_gridStats_20cell <- ForestTools::sp_summarise(crownsPoly_2mTO1.5m_Quan, grid = 20, variables = "height", statFuns = custFuns)
crownsPoly_2mTO1.5m_Quan_gridStats_50cell <- ForestTools::sp_summarise(crownsPoly_2mTO1.5m_Quan, grid = 50, variables = "height", statFuns = custFuns)
crownsPoly_2mTO1.5m_Quan_gridStats_100cell <- ForestTools::sp_summarise(crownsPoly_2mTO1.5m_Quan, grid = 100, variables = "height", statFuns = custFuns)

names(crownsPoly_2mTO1.5m_Quan_gridStats_20cell)
plot(crownsPoly_3mTO2m_Quan_gridStats_20cell[["height95thQuantile"]], col = heat.colors(255))
plot(crownsPoly_3mTO2m_Quan_gridStats_50cell[["height95thQuantile"]], col = heat.colors(255))
plot(crownsPoly_3mTO2m_Quan_gridStats_100cell[["height95thQuantile"]], col = heat.colors(255))

plot(crownsPoly_2mTO1.5m_Quan_gridStats_20cell[["height95thQuantile"]], col = heat.colors(255))
plot(crownsPoly_2mTO1.5m_Quan_gridStats_50cell[["height95thQuantile"]], col = heat.colors(255))
plot(crownsPoly_2mTO1.5m_Quan_gridStats_100cell[["height95thQuantile"]], col = heat.colors(255))

lead_htop_raster_3m2m_95th_20cell = crownsPoly_3mTO2m_Quan_gridStats_20cell[["height95thQuantile"]]
lead_htop_raster_3m2m_95th_50cell = crownsPoly_3mTO2m_Quan_gridStats_50cell[["height95thQuantile"]]
lead_htop_raster_3m2m_95th_100cell = crownsPoly_3mTO2m_Quan_gridStats_100cell[["height95thQuantile"]]

lead_htop_raster_2m1.5m_95th_20cell = crownsPoly_2mTO1.5m_Quan_gridStats_20cell[["height95thQuantile"]]
lead_htop_raster_2m1.5m_95th_50cell = crownsPoly_2mTO1.5m_Quan_gridStats_50cell[["height95thQuantile"]]
lead_htop_raster_2m1.5m_95th_100cell = crownsPoly_2mTO1.5m_Quan_gridStats_100cell[["height95thQuantile"]]

stemsha_L_raster_2m_20cell <- sp_summarise(trees = ttops_2m_Quan, grid = 20)
stemsha_L_raster_2m_50cell <- sp_summarise(trees = ttops_2m_Quan, grid = 50)
stemsha_L_raster_2m_100cell <- sp_summarise(trees = ttops_2m_Quan, grid = 100)

stemsha_L_raster_3m_20cell <- sp_summarise(trees = ttops_3m_Quan, grid = 20)
stemsha_L_raster_3m_50cell <- sp_summarise(trees = ttops_3m_Quan, grid = 50)
stemsha_L_raster_3m_100cell <- sp_summarise(trees = ttops_3m_Quan, grid = 100)

lead_htop_20 = lead_htop_raster_3m2m_95th_20cell
lead_htop_50 = lead_htop_raster_3m2m_95th_50cell
lead_htop_100 = lead_htop_raster_3m2m_95th_100cell

stemsha_L_20 = stemsha_L_raster_3m_20cell[["TreeCount"]]
stemsha_L_50 = stemsha_L_raster_3m_50cell[["TreeCount"]]
stemsha_L_100 = stemsha_L_raster_3m_100cell[["TreeCount"]]

writeRaster(lead_htop_20, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_raster_20cell.tif", overwrite=TRUE)
writeRaster(lead_htop_50, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_raster_50cell.tif", overwrite=TRUE)
writeRaster(lead_htop_100, filename = "./Data/Raster_Covariates/UnMasked/lead_htop_raster_100cell.tif", overwrite=TRUE)

writeRaster(stemsha_L_20, filename = "./Data/Raster_Covariates/UnMasked/stems_L_ha_raster_20cell.tif", overwrite=TRUE)
writeRaster(stemsha_L_50, filename = "./Data/Raster_Covariates/UnMasked/stems_L_ha_raster_50cell.tif", overwrite=TRUE)
writeRaster(stemsha_L_100, filename = "./Data/Raster_Covariates/UnMasked/stems_L_ha_raster_100cell.tif", overwrite=TRUE)

lead_htop = raster::raster("./Data/Raster_Covariates/UnMasked/lead_htop_raster_100cell.tif")
stemsha_L = raster::raster("./Data/Raster_Covariates/UnMasked/stems_L_ha_raster_100cell.tif")

lead_htop_rast = terra::rast(lead_htop)
stemsha_L_rast = terra::rast(stemsha_L)
elev_rast = terra::rast(elev)
slope_rast = terra::rast(slope)
asp_cos_rast = terra::rast(asp_cos)
asp_sin_rast = terra::rast(asp_sin)
species_class_rast = terra::rast(species_class)

crs(lead_htop_rast) = "epsg:3005"
crs(stemsha_L_rast) = "epsg:3005"
crs(elev_rast) = "epsg:3005"
crs(slope_rast) = "epsg:3005"
crs(asp_cos_rast) = "epsg:3005"
crs(asp_sin_rast) = "epsg:3005"
crs(species_class_rast) = "epsg:3005"

elev_rast = terra::resample(elev_rast, lead_htop_rast, method="bilinear")
slope_rast = terra::resample(slope_rast, lead_htop_rast, method="bilinear")
asp_cos_rast = terra::resample(asp_cos_rast, lead_htop_rast, method="bilinear")
asp_sin_rast = terra::resample(asp_sin_rast, lead_htop_rast, method="bilinear")
species_class_rast = terra::resample(species_class_rast, lead_htop_rast, method="near")

elev_rast = mask(elev_rast, lead_htop_rast, inverse=FALSE)
slope_rast = mask(slope_rast, lead_htop_rast, inverse=FALSE)
asp_cos_rast = mask(asp_cos_rast, lead_htop_rast, inverse=FALSE)
asp_sin_rast = mask(asp_sin_rast, lead_htop_rast, inverse=FALSE)
species_class_rast = mask(species_class_rast, lead_htop_rast, inverse=FALSE)
stemsha_L_rast = mask(stemsha_L_rast, lead_htop_rast, inverse=FALSE)

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

# Tidy ground plot data
faib_psp$spc_live1 = as.factor(faib_psp$spc_live1)
faib_psp = subset(faib_psp, spc_live1 == "PL" | spc_live1 == "SB" | spc_live1 == "SE" | 
                    spc_live1 == "SX" | spc_live1 == "FD" | spc_live1 == "CW" | spc_live1 == "HW" | spc_live1 == "BL")
faib_psp$species_class = dplyr::recode(faib_psp$spc_live1, 
  PL = 0, SB = 1, SE = 1, SX = 1, FD = 2, CW = 3, HW = 4, BL = 5)
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
faib_psp$wsvha_L = as.numeric(faib_psp$wsvha_L)
faib_psp$stemsha_L = as.numeric(faib_psp$stemsha_L)
faib_psp$slope = as.numeric(faib_psp$slope)
faib_psp$aspect = as.numeric(faib_psp$aspect)
faib_psp$asp_cos = as.numeric(faib_psp$asp_cos)
faib_psp$asp_sin = as.numeric(faib_psp$asp_sin)
faib_psp$lead_htop = as.numeric(faib_psp$lead_htop)
faib_psp$species_class = as.numeric(faib_psp$species_class)
faib_psp$elev = as.numeric(faib_psp$elev)
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

# fit models: model1_svmRadial
tuneResult_svm_m2_full <- tune(svm, X_m2, y_m2, ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)),
  tunecontrol = tune.control(cross = 10),preProcess = c("BoxCox","center","scale"))

tunedModel_svm_m2_full <- tuneResult_svm_m2_full$best.model
save(tunedModel_svm_m2_full, file = "./Models/model1_svmRadial_mar24.RData")


names(faib_vri_true_m2_df)
names(covs_m2)

# writeRaster and plot outputs
tunedModel_svm_m2_to_raster <- raster::predict(covs_m2, tunedModel_svm_m2_full)
writeRaster(tunedModel_svm_m2_to_raster, filename = "./Results/model1_svmRadial_mar24.tif", overwrite=TRUE)




















# Derive CHM-based covariates: lidR to ForestTools pipeline
ttops_Quan_lidR_find_trees = lidR::find_trees(lead_htop_raster, lmf(wf_Quan)) #SpatialPointsDataFrame object
ttops_Quan_lidR_locate_trees = lidR::locate_trees(lead_htop_raster, lmf(wf_Quan)) #SimpleFeatures object

crowns_Quan_lidR_2m = ForestTools::mcws(treetops = ttops_Quan_lidR_find_trees, CHM = lead_htop_raster, minHeight = 2, verbose = FALSE)
crowns_Quan_lidR_3m = ForestTools::mcws(treetops = ttops_Quan_lidR_find_trees, CHM = lead_htop_raster, minHeight = 2, verbose = FALSE)
crownsPoly_Quan_lidR_2m = ForestTools::mcws(treetops = ttops_Quan_lidR_find_trees, CHM = lead_htop_raster, format = "polygons", minHeight = 2, verbose = FALSE)
crownsPoly_Quan_lidR_3m = ForestTools::mcws(treetops = ttops_Quan_lidR_find_trees, CHM = lead_htop_raster, format = "polygons", minHeight = 3, verbose = FALSE)

plot(ttops_Quan_lidR, col = sample(rainbow(50), length(unique(ttops_Quan_lidR_find_trees[])), replace = TRUE), legend = FALSE)
plot(crowns_Quan_lidR_2m, col = sample(rainbow(50), length(unique(crowns_Quan_lidR_2m[])), replace = TRUE), legend = FALSE)
plot(crownsPoly_Quan_lidR_2m, col = sample(rainbow(50), length(unique(crownsPoly_Quan_lidR_2m[])), replace = TRUE), legend = FALSE)

crowns_Quan_lidR_2m_gridStats_20cell = ForestTools::sp_summarise(crowns_Quan_lidR_2m, grid = 20, variables = "height", statFuns = custFuns)
crowns_Quan_lidR_2m_gridStats_50cell  = ForestTools::sp_summarise(crowns_Quan_lidR_2m, grid = 50, variables = "height", statFuns = custFuns)
crowns_Quan_lidR_2m_gridStats_100cell = ForestTools::sp_summarise(crowns_Quan_lidR_2m, grid = 100, variables = "height", statFuns = custFuns)

crowns_Quan_lidR_3m_gridStats_20cell = ForestTools::sp_summarise(crowns_Quan_lidR_3m, grid = 20, variables = "height", statFuns = custFuns)
crowns_Quan_lidR_3m_gridStats_50cell  = ForestTools::sp_summarise(crowns_Quan_lidR_3m, grid = 50, variables = "height", statFuns = custFuns)
crowns_Quan_lidR_3m_gridStats_100cell = ForestTools::sp_summarise(crowns_Quan_lidR_3m, grid = 100, variables = "height", statFuns = custFuns)

crownsPoly_Quan_lidR_2m_gridStats_20cell <- ForestTools::sp_summarise(crowns_Quan_lidR_2m, grid = 20, variables = "height", statFuns = custFuns)
crownsPoly_Quan_lidR_2m_gridStats_50cell <- ForestTools::sp_summarise(crowns_Quan_lidR_2m, grid = 50, variables = "height", statFuns = custFuns)
crownsPoly_Quan_lidR_2m_gridStats_100cell <- ForestTools::sp_summarise(crowns_Quan_lidR_2m, grid = 100, variables = "height", statFuns = custFuns)

crownsPoly_Quan_lidR_3m_gridStats_20cell <- ForestTools::sp_summarise(crowns_Quan_lidR_3m, grid = 20, variables = "height", statFuns = custFuns)
crownsPoly_Quan_lidR_3m_gridStats_50cell <- ForestTools::sp_summarise(crowns_Quan_lidR_3m, grid = 50, variables = "height", statFuns = custFuns)
crownsPoly_Quan_lidR_3m_gridStats_100cell <- ForestTools::sp_summarise(crowns_Quan_lidR_3m, grid = 100, variables = "height", statFuns = custFuns)
