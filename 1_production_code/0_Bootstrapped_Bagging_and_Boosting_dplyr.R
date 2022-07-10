
library(dplyr)
library(psych)
library(gdalUtils)
library(rgdal)
library(raster)
library(terra)
library(rgeos)
library(sp)
library(sf)
library(spatial)
library(spatstat)
library(spatstat.geom)
library(spatstat.utils)
library(maptools)
library(bootstrap)
library(doParallel)  
library(foreach)     
library(caret)       
library(rpart)       
library(ipred)       
library(ggplot2)     
library(kernlab)
library(randomForest)
set.seed(8888)

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
faib_psp = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "stemsha_L", "wsvha_L")]
faib_psp$elev[faib_psp$elev <= 0] = NA
faib_psp$slope[faib_psp$slope <= 0] = NA
faib_psp$lead_htop[faib_psp$lead_htop < 2] = NA
faib_psp$stemsha_L[faib_psp$stemsha_L <= 0] = NA
faib_psp$wsvha_L[faib_psp$wsvha_L <= 1] = NA
faib_psp = subset(faib_psp, stemsha_L < 864)
faib_psp = na.omit(faib_psp)
psych::describe(faib_psp)

# Customized simulation for bootstrap resampling
stemsha_L_raster_df = as.data.frame(rasterToPoints(stemsha_L_raster))
dens.rast = density(stemsha_L_raster_df$stemsha_L, adjust=0.8)
dens.fun = approxfun(density(stemsha_L_raster_df$stemsha_L, adjust=0.8))
dens.rast$y
dens.fun
# number of simulations B, and exponentials n
B = 1000
n = 40

# generate resampling matrix with replacement
#resample.faib <- matrix(sample(
 # dens.faib$x, B * n, replace = TRUE, prob=dens.rast$y))
#simulation <- apply(resample.faib, 1, mean)
#summary(simulation)

bootstrap.faib.to.rast = sample_n(faib_psp, B * n, weight_by = dens.fun(faib_psp$stemsha_L), replace = T)
par(mfrow = c(3, 1)) 
hist(faib_psp$stemsha_L)
hist(bootstrap.faib.to.rast$stemsha_L)
hist(bootstrap.faib.to.rast.prob$stemsha_L)

bootstrap.faib.to.rast = na.omit(bootstrap.faib.to.rast)
bootstrap.faib.to.rast_m1 = bootstrap.faib.to.rast[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "wsvha_L")] 
bootstrap.faib.to.rast_m2 = bootstrap.faib.to.rast[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "stemsha_L", "wsvha_L")]
bootstrap.faib.to.rast_m1 = na.omit(bootstrap.faib.to.rast_m1)
bootstrap.faib.to.rast_m2 = na.omit(bootstrap.faib.to.rast_m2)

faib_vri_true_m1_df = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "wsvha_L")] 
faib_vri_true_m2_df = faib_psp[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "stemsha_L", "wsvha_L")]
faib_vri_true_m1_df = na.omit(faib_vri_true_m1_df)
faib_vri_true_m2_df = na.omit(faib_vri_true_m2_df)


bootstrap.faib.to.rast_m1_split = createDataPartition(bootstrap.faib.to.rast_m1$wsvha_L, p=0.80, list=F)
train_m1_bagbag = bootstrap.faib.to.rast_m1[bootstrap.faib.to.rast_m1_split, ]
test_m1_bagbag = bootstrap.faib.to.rast_m1[-bootstrap.faib.to.rast_m1_split, ]
X_train_m1_bagbag = train_m1_bagbag[,-7]
y_train_m1_bagbag = train_m1_bagbag[, 7]
X_test_m1_bagbag = test_m1_bagbag[,-7]
y_test_m1_bagbag = test_m1_bagbag[, 7]
X_m1_bagbag = bootstrap.faib.to.rast_m1[,-7]
y_m1_bagbag = bootstrap.faib.to.rast_m1[, 7]
str(X_train_m1_bagbag)
str(y_train_m1_bagbag)


faib_vri_true_m1_df_partition = createDataPartition(faib_vri_true_m1_df$wsvha_L, p=0.80, list=F)
train_m1_bag = faib_vri_true_m1_df[faib_vri_true_m1_df_partition, ]
test_m1_bag = faib_vri_true_m1_df[-faib_vri_true_m1_df_partition, ]
X_train_m1_bag = train_m1_bag[,-7]
y_train_m1_bag = train_m1_bag[, 7]
X_test_m1_bag = test_m1_bag[,-7]
y_test_m1_bag = test_m1_bag[, 7]
X_m1_bagbag = faib_vri_true_m1_df[,-7]
y_m1_bagbag = faib_vri_true_m1_df[, 7]
str(X_train_m1_bag)
str(y_train_m1_bag)


fitControl_YeoJx1 = caret::trainControl(method="cv", number=10)
fitControl_YeoJx3 = caret::trainControl(method="repeatedcv", number=10, repeats=3)
fitControl_YeoJx5 = caret::trainControl(method="repeatedcv", number=10, repeats=5)
fitControl_YeoJx10 = caret::trainControl(method="repeatedcv", number=10, repeats=10)

str(svmBag)
str(ldaBag)
str(plsBag)
str(ctreeBag)
str(nnetBag)
svmBag$fit
svmBag$pred
svmBag$aggregate

## svmBag Radial epsilon-untuned
predfunct<-function (object, x) {
  if (is.character(lev(object))) {
    out <- predict(object, as.matrix(x), type = "probabilities")
    colnames(out) <- lev(object)
    rownames(out) <- NULL }
  else out <- predict(object, as.matrix(x))[, 1]
  out }

svmbag_m1_tuned = bag(
  X_train_m1_bagbag, y_train_m1_bagbag, 
  method="svmbag", B = 10, downSample=T,
  trControl = fitControl_YeoJx1,  
  ranges = list(cost = c(1,5,7,15,20), gamma = 2^(-1:1)), tuneLength = 10,
  bagControl = bagControl(fit = svmBag$fit, predict = predfunct, aggregate = svmBag$aggregate))

svmbag_m1_tuned$control
svmbag_m1_tuned$fits
tunedModel_svmRadial_m1_tuned = predict(svmbag_m1_tuned, newdata = train_m1_bag)
tunedModel_svmRadial_m1_tuned_MAE = MAE(tunedModel_svmRadial_m1_tuned, train_m1_bag$wsvha_L)
tunedModel_svmRadial_m1_tuned_RMSE = RMSE(tunedModel_svmRadial_m1_tuned, train_m1_bag$wsvha_L)
tunedModel_svmRadial_m1_tuned_test = predict(svmbag_m1_tuned, newdata = test_m1_bag)
tunedModel_svmRadial_m1_tuned_test_MAE = MAE(tunedModel_svmRadial_m1_tuned_test, test_m1_bag$wsvha_L)
tunedModel_svmRadial_m1_tuned_test_RMSE = RMSE(tunedModel_svmRadial_m1_tuned_test, test_m1_bag$wsvha_L)

R2(tunedModel_svmRadial_m1_tuned, train_m1_bag$wsvha_L)
tunedModel_svmRadial_m1_tuned_MAE
tunedModel_svmRadial_m1_tuned_RMSE 
tunedModel_svmRadial_m1_tuned_test_MAE
tunedModel_svmRadial_m1_tuned_test_RMSE
tunedModel_svmRadial_m1_tuned_RMSE/tunedModel_svmRadial_m1_tuned_test_RMSE

wsvha_model1_svmRadial_bagged <- svmbag_m1_tuned
save(wsvha_model1_svmRadial_bagged, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_svmRadial_bagged.RData")
wsvha_model1_svmRadial_bagged_100m_allAreas <- raster::predict(covs_m1, wsvha_model1_svmRadial_bagged)
wsvha_model1_svmRadial_bagged_100m_gaspard <- raster::predict(covs_m1_gaspard, wsvha_model1_svmRadial_bagged)
wsvha_model1_svmRadial_bagged_100m_quesnel <- raster::predict(covs_m1_quesnel, wsvha_model1_svmRadial_bagged)
wsvha_model1_svmRadial_bagged_100m_allAreas$layer[wsvha_model1_svmRadial_bagged_100m_allAreas$layer <= 0] = 0
wsvha_model1_svmRadial_bagged_100m_gaspard$layer[wsvha_model1_svmRadial_bagged_100m_gaspard$layer <= 0] = 0
wsvha_model1_svmRadial_bagged_100m_quesnel$layer[wsvha_model1_svmRadial_bagged_100m_quesnel$layer <= 0] = 0
writeRaster(wsvha_model1_svmRadial_bagged_100m_allAreas, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_bagged_100m_allAreas.tif", overwrite=TRUE)
writeRaster(wsvha_model1_svmRadial_bagged_100m_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_bagged_100m_gaspard.tif", overwrite=TRUE)
writeRaster(wsvha_model1_svmRadial_bagged_100m_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_bagged_100m_quesnel.tif", overwrite=TRUE)
graphics.off()
par(mfrow = c(3, 1)) 
plot(wsvha_model1_svmRadial_bagged_100m_gaspard, main="Gaspard: Model1 SVM Radial", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_svmRadial_bagged_100m_quesnel, main="Quesnel: Model1 SVM Radial", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_svmRadial_bagged_100m_allAreas, main="All Areas: Model1 SVM Radial", cex.main=0.8, maxpixels=22000000) 


## svmBag Radial epsilon-tuned
svmbag_m1_tuned_eps = bag(
  X_train_m1_bagbag, y_train_m1_bagbag, 
  method="svmbag", B = 10, downSample=T,
  trControl = fitControl_YeoJx1,  
  ranges = list(epsilon = seq(0.02,0.1,0.2), cost = c(1,5,7,15,20), gamma = 2^(-1:1)), tuneLength = 10,
  bagControl = bagControl(fit = svmBag$fit, predict = predfunct, aggregate = svmBag$aggregate))

svmbag_m1_tuned_eps$fits
tunedModel_svmRadial_m1_tuned_eps = predict(svmbag_m1_tuned_eps, newdata = train_m1_bag)
tunedModel_svmRadial_m1_tuned_eps_MAE = MAE(tunedModel_svmRadial_m1_tuned_eps, train_m1_bag$wsvha_L)
tunedModel_svmRadial_m1_tuned_eps_RMSE = RMSE(tunedModel_svmRadial_m1_tuned_eps, train_m1_bag$wsvha_L)
tunedModel_svmRadial_m1_tuned_eps_test = predict(svmbag_m1_tuned_eps, newdata = test_m1_bag)
tunedModel_svmRadial_m1_tuned_eps_test_MAE = MAE(tunedModel_svmRadial_m1_tuned_eps_test, test_m1_bag$wsvha_L)
tunedModel_svmRadial_m1_tuned_eps_test_RMSE = RMSE(tunedModel_svmRadial_m1_tuned_eps_test, test_m1_bag$wsvha_L)

R2(tunedModel_svmRadial_m1_tuned_eps, train_m1_bag$wsvha_L)
tunedModel_svmRadial_m1_tuned_eps_MAE
tunedModel_svmRadial_m1_tuned_eps_RMSE 
tunedModel_svmRadial_m1_tuned_eps_test_MAE
tunedModel_svmRadial_m1_tuned_eps_test_RMSE
tunedModel_svmRadial_m1_tuned_eps_RMSE/tunedModel_svmRadial_m1_tuned_eps_test_RMSE

wsvha_model1_svmRadial_eps_bagged <- svmbag_m1_tuned_eps
save(wsvha_model1_svmRadial_eps_bagged, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_svmRadial_eps_bagged.RData")

wsvha_model1_svmRadial_eps_bagged_100m_allAreas <- raster::predict(covs_m1, wsvha_model1_svmRadial_eps_bagged)
wsvha_model1_svmRadial_eps_bagged_100m_gaspard <- raster::predict(covs_m1_gaspard, wsvha_model1_svmRadial_eps_bagged)
wsvha_model1_svmRadial_eps_bagged_100m_quesnel <- raster::predict(covs_m1_quesnel, wsvha_model1_svmRadial_eps_bagged)
wsvha_model1_svmRadial_eps_bagged_100m_allAreas$layer[wsvha_model1_svmRadial_eps_bagged_100m_allAreas$layer <= 0] = 0
wsvha_model1_svmRadial_eps_bagged_100m_gaspard$layer[wsvha_model1_svmRadial_eps_bagged_100m_gaspard$layer <= 0] = 0
wsvha_model1_svmRadial_eps_bagged_100m_quesnel$layer[wsvha_model1_svmRadial_eps_bagged_100m_quesnel$layer <= 0] = 0
writeRaster(wsvha_model1_svmRadial_eps_bagged_100m_allAreas, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_eps_bagged_100m_allAreas.tif", overwrite=TRUE)
writeRaster(wsvha_model1_svmRadial_eps_bagged_100m_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_eps_bagged_100m_gaspard.tif", overwrite=TRUE)
writeRaster(wsvha_model1_svmRadial_eps_bagged_100m_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmRadial_eps_bagged_100m_quesnel.tif", overwrite=TRUE)
graphics.off()
par(mfrow = c(3, 1)) 
plot(wsvha_model1_svmRadial_eps_bagged_100m_gaspard, main="Gaspard: Model1 SVM Radial-e", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_svmRadial_eps_bagged_100m_quesnel, main="Quesnel: Model1 SVM Radial-e", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_svmRadial_eps_bagged_100m_allAreas, main="All Areas: Model1 SVM Radial-e", cex.main=0.8, maxpixels=22000000) 

## Random Forest
grid_rf = expand.grid(.mtry=c(2,4))    
rfbag_m1_tuned = train(
  X_train_m1_bagbag, y_train_m1_bagbag,
  method="rf", metric="RMSE", ntree=50,
  trControl = fitControl_YeoJx1,
  tuneGrid = grid_rf)

rfbag_m1_tuned
rfbag_m1_tuned_train = predict(rfbag_m1_tuned, newdata=faib_vri_true_m1_df, type="raw")
rfbag_m1_tuned_test = predict(rfbag_m1_tuned, newdata=test_m1_bagbag, type="raw")
R2(rfbag_m1_tuned_train, faib_vri_true_m1_df$wsvha_L)
MAE(rfbag_m1_tuned_train, faib_vri_true_m1_df$wsvha_L)
RMSE(rfbag_m1_tuned_train, faib_vri_true_m1_df$wsvha_L)
MAE(rfbag_m1_tuned_test, test_m1_bagbag$wsvha_L)
RMSE(rfbag_m1_tuned_test, test_m1_bagbag$wsvha_L)

wsvha_model1_randomForest_bagged <- rfbag_m1_tuned
save(wsvha_model1_randomForest_bagged, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_randomForest_bagged.RData")

wsvha_model1_randomForest_bagged_100m_allAreas <- raster::predict(covs_m1, wsvha_model1_randomForest_bagged)
wsvha_model1_randomForest_bagged_100m_gaspard <- raster::predict(covs_m1_gaspard, wsvha_model1_randomForest_bagged)
wsvha_model1_randomForest_bagged_100m_quesnel <- raster::predict(covs_m1_quesnel, wsvha_model1_randomForest_bagged)
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

## Linear double bagging: ipred partitioning
survbag_m1 = bagging(wsvha_L~., 
  data=bootstrap.faib.to.rast_m1, 
  coob=T, nbagg=50, method="svmbag", metric="RMSE",
  trControl = fitControl_YeoJx1,
  control=rpart.control(minsplit = 2, cp = 0, xval=0),
  ns=length(y_m1_bagbag), keepX=T)

#comb.lda <- list(list(model=lda, 
 #   predict=function(obj, newdata)
  #    predict(obj, newdata)$x))

#survbag_m1_linearEnsemble = bagging(wsvha_L~., 
 # data=bootstrap.faib.to.rast_m1, comb=comb.lda, nbagg=50)

#survbag_m1_linearEnsemble$OOB
#survbag_m1_linearEnsemble_train = predict(survbag_m1_linearEnsemble, newdata=faib_vri_true_m1_df, type="raw")
#survbag_m1_linearEnsemble_test = predict(survbag_m1_linearEnsemble, newdata=test_m1_bagbag, type="raw")
#R2(survbag_m1_linearEnsemble_train, faib_vri_true_m1_df$wsvha_L)
#MAE(survbag_m1_linearEnsemble_train, faib_vri_true_m1_df$wsvha_L)
#RMSE(survbag_m1_linearEnsemble_train, faib_vri_true_m1_df$wsvha_L)
#MAE(survbag_m1_linearEnsemble_test, test_m1_bagbag$wsvha_L)
#RMSE(survbag_m1_linearEnsemble_test, test_m1_bagbag$wsvha_L)

summary(survbag_m1$mtrees)
survbag_m1_train = predict(survbag_m1, newdata=bootstrap.faib.to.rast_m1)
survbag_m1_test = predict(survbag_m1, newdata=test_m1_bagbag)
R2(survbag_m1_train, bootstrap.faib.to.rast_m1$wsvha_L)
MAE(survbag_m1_train, bootstrap.faib.to.rast_m1$wsvha_L)
RMSE(survbag_m1_train, bootstrap.faib.to.rast_m1$wsvha_L)
MAE(survbag_m1_test, test_m1_bagbag$wsvha_L)
RMSE(survbag_m1_test, test_m1_bagbag$wsvha_L)


wsvha_model1_svmLinear_bagged <- survbag_m1
save(wsvha_model1_svmLinear_bagged, file = "/media/seamus/128GB_WORKD/data/models/tcc-wsvha/wsvha_model1_svmLinear_bagged.RData")

wsvha_model1_svmLinear_bagged_100m_allAreas <- raster::predict(covs_m1, wsvha_model1_svmLinear_bagged)
wsvha_model1_svmLinear_bagged_100m_gaspard <- raster::predict(covs_m1_gaspard, wsvha_model1_svmLinear_bagged)
wsvha_model1_svmLinear_bagged_100m_quesnel <- raster::predict(covs_m1_quesnel, wsvha_model1_svmLinear_bagged)
wsvha_model1_svmLinear_bagged_100m_allAreas$layer[wsvha_model1_svmLinear_bagged_100m_allAreas$layer <= 0] = 0
wsvha_model1_svmLinear_bagged_100m_gaspard$layer[wsvha_model1_svmLinear_bagged_100m_gaspard$layer <= 0] = 0
wsvha_model1_svmLinear_bagged_100m_quesnel$layer[wsvha_model1_svmLinear_bagged_100m_quesnel$layer <= 0] = 0
writeRaster(wsvha_model1_svmLinear_bagged_100m_allAreas, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmLinear_bagged_100m_allAreas.tif", overwrite=TRUE)
writeRaster(wsvha_model1_svmLinear_bagged_100m_gaspard, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmLinear_bagged_100m_gaspard.tif", overwrite=TRUE)
writeRaster(wsvha_model1_svmLinear_bagged_100m_quesnel, filename = "/media/seamus/128GB_WORKD/data/raster/tcc/wsvha/bootstrapped/wsvha_model1_svmLinear_bagged_100m_quesnel.tif", overwrite=TRUE)
graphics.off()
par(mfrow = c(3, 1)) 
plot(wsvha_model1_svmLinear_bagged_100m_gaspard, main="Gaspard: Model1 SVM Linear-e", cex.main=0.8, maxpixels=22000000)
plot(wsvha_model1_svmLinear_bagged_100m_quesnel, main="Quesnel: Model1 SVM Radial-e", cex.main=0.8, maxpixels=22000000) 
hist(wsvha_model1_svmLinear_bagged_100m_allAreas, main="All Areas: Model1 SVM Radial-e", cex.main=0.8, maxpixels=22000000) 