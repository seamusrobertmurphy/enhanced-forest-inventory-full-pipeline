
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
species_class_rast = terra::rast(species_class_raster)
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
writeRaster(tunedModel_svm_m2_to_raster_eps, filename = "./Results/model1_svmRadial_100m_combo3_eps_april28.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_eps, filename = "./Results/model2_svmRadial_100m_combo3_eps_april28.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full_eps)
print(tunedModel_svm_m1_full_eps)

par(mfrow = c(2,2))
model1_svmRadial_100cell_eps = raster::raster("./Results/model1_svmRadial_100m_combo3_eps_april28.tif")
model2_svmRadial_100cell_eps = raster::raster("./Results/model2_svmRadial_100m_combo3_eps_april28.tif")
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
writeRaster(tunedModel_svm_m2_to_raster, filename = "./Results/model1_svmRadial_100m_combo3_april28.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster, filename = "./Results/model2_svmRadial_100m_combo3_april28.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full)
print(tunedModel_svm_m1_full)


par(mfrow = c(2,2))
model1_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_combo3_april28.tif")
model2_svmRadial_100cell = raster::raster("./Results/model1_svmRadial_100m_combo3_april28.tif")
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
writeRaster(tunedModel_svm_m2_to_raster_linear, filename = "./Results/model1_svmRadial_100m_combo3_linear_april28.tif", overwrite=TRUE)
writeRaster(tunedModel_svm_m1_to_raster_linear, filename = "./Results/model2_svmRadial_100m_combo3_linear_april28.tif", overwrite=TRUE)
print(tunedModel_svm_m2_full_linear)
print(tunedModel_svm_m1_full_linear)

par(mfrow = c(2,2))
model1_svmLinear_100cell = raster::raster("./Results/model1_svmRadial_100m_combo3_linear_april28.tif")
model2_svmLinear_100cell = raster::raster("./Results/model1_svmRadial_100m_combo3_linear_april28.tif")
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
