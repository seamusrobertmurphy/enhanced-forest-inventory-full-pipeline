
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

# Import LiDAR CHM-covariates
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
#ggplot(masks_sf) + geom_sf(size = 0.0005)

mask_clearcut = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/RSLT_CCRES_CLEAR.shp")
mask_clearcut = sf::st_intersection(mask_clearcut, st_make_valid(lead_htop_sf))
masks_df = full_join(as_tibble(masks_sf), as_tibble(mask_clearcut), by = 'geometry')
masks_sf = st_as_sf(masks_df)

mask_blocks = sf::read_sf("./Data/Seamus_20220330/Seamus_20220330/TCC_Blocks_Join.shp")
mask_blocks = sf::st_intersection(mask_blocks, st_make_valid(lead_htop_sf))
masks_df = full_join(as_tibble(masks_sf), as_tibble(mask_blocks), by = 'geometry')
masks_sf = st_as_sf(masks_df)
#ggplot(masks_sf) + geom_sf()

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
#ggplot(masks_sf) + geom_sf(aes(fill = 'red'), show.legend = FALSE)

masks_rast = rasterize(vect(masks_sf), lead_htop_rast, touches = TRUE)
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
species_class_rast = terra::resample(species_class_rast, elev_rast)
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

tuneResult_svm_m2_full <- train(wsvha_L~., data=faib_vri_true_m2_df,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svm_m1_full <- train(wsvha_L~., data=faib_vri_true_m1_df,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tunedModel_svm_m2_full <- tuneResult_svm_m2_full$finalModel
tunedModel_svm_m1_full <- tuneResult_svm_m1_full$finalModel
tunedModel_svm_m2_to_raster <- raster::predict(covs_m2, tuneResult_svm_m2_full)
tunedModel_svm_m1_to_raster <- raster::predict(covs_m1, tuneResult_svm_m1_full)
save(tunedModel_svm_m2_full, file = "./Models/tunedModel_svmRadial_m2_10k.RData")
save(tunedModel_svm_m1_full, file = "./Models/tunedModel_svmRadial_m1_10k.RData")
writeRaster(tunedModel_svm_m2_to_raster, filename = "./Results/model1_svmRadial_100m_combo3_april28.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster, filename = "./Results/model2_svmRadial_100m_combo3_april28.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full)
print(tunedModel_svm_m1_full)

par(mfrow = c(2,2))
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_combo3_april28.tif")
model2_svmRadial_100cell = raster::raster("./Results/model2_svmRadial_100m_combo3_april28.tif")
plot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ centre, scale, corr, YeoJohnson)", cex.main=0.8)
plot(model2_svmRadial_100cell, main="Whole Stem Vol (raster w/ centre, scale, corr, YeoJohnson)", cex.main=0.8)
hist(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ centre, scale, corr, YeoJohnson)", cex.main=0.8, maxpixels=22000000) 
hist(model2_svmRadial_100cell, main="Whole Stem Vol (raster w/ centre, scale, corr, YeoJohnson)", cex.main=0.8, maxpixels=22000000) 
rasterVis::densityplot(model1_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox)")
rasterVis::densityplot(model2_svmRadial_100cell, main="Whole Stem Vol (raster w/ boxcox)")

tuneResult_svmRadial_m2_10k_train <- train(X_train_m2, y_train_m2,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
  tuneLength = 10,
  preProc = c('YeoJohnson', 'scale', 'center', 'corr'),
  verbose=F)

tuneResult_svmRadial_m1_10k_train <- train(X_train_m1, y_train_m1,
  trControl = fitControl_YeoJx1,
  method = 'svmRadial',
  metric = 'RMSE',
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

tunedModel_svmRadial_m2_test_MAE
tunedModel_svmRadial_m1_test_MAE
tunedModel_svmRadial_m2_test_RMSE
tunedModel_svmRadial_m1_test_RMSE

tunedModel_svmRadial_m2_MAE
tunedModel_svmRadial_m1_MAE
tunedModel_svmRadial_m2_RMSE 
tunedModel_svmRadial_m1_RMSE 
tunedModel_svmRadial_m2_RMSE/tunedModel_svmRadial_m2_test_RMSE
tunedModel_svmRadial_m1_RMSE/tunedModel_svmRadial_m1_test_RMSE


