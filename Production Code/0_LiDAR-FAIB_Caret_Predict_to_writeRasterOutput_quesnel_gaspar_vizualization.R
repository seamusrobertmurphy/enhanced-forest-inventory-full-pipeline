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

# Import raster stacks
lead_htop_raster_quesnel = raster::raster("./data_quesnel/covariates/lead_htop_ttops_100cell.tif")
stemsha_L_raster_quesnel = raster::raster("./data_quesnel/covariates/stemsha_L_ttops_100cell.tif")
elev_raster_quesnel = raster::raster("./data_quesnel/covariates/elev_raster.tif")
slope_raster_quesnel = raster::raster("./data_quesnel/covariates/slope_raster.tif")
asp_cos_raster_quesnel = raster::raster("./data_quesnel/covariates/asp_cos_raster.tif")
asp_sin_raster_quesnel = raster::raster("./data_quesnel/covariates/asp_sin_raster.tif")
species_class_raster_quesnel = raster::raster("./data_quesnel/covariates/species_class_raster.tif")

lead_htop_rast_quesnel = terra::rast(lead_htop_raster_quesnel)
stemsha_L_rast_quesnel = terra::rast(stemsha_L_raster_quesnel)
elev_rast_quesnel = terra::rast(elev_raster_quesnel)
slope_rast_quesnel = terra::rast(slope_raster_quesnel)
asp_cos_rast_quesnel = terra::rast(asp_cos_raster_quesnel)
asp_sin_rast_quesnel = terra::rast(asp_sin_raster_quesnel)
species_class_rast_quesnel = terra::rast(species_class_raster_quesnel)

terra::plot(lead_htop_rast_quesnel, main = "lead_htop")
terra::plot(stemsha_L_rast_quesnel, main = "stemsha_L")
terra::plot(elev_rast_quesnel, main = "elevation")
terra::plot(slope_rast_quesnel, main = "slope")
terra::plot(asp_cos_rast_quesnel, main = "asp_cos")
terra::plot(asp_sin_rast_quesnel, main = "asp_sin")
terra::plot(species_class_rast_quesnel, main = "species_class")




lead_htop_rast_quesnel = terra::resample(lead_htop_rast_quesnel, elev_rast_quesnel)
stemsha_L_rast_quesnel = terra::resample(stemsha_L_rast_quesnel, elev_rast_quesnel)
species_class_rast_quesnel = terra::resample(species_class_rast_quesnel, elev_rast_quesnel)
elev_rast_quesnel = terra::mask(elev_rast_quesnel, lead_htop_rast_quesnel)
slope_rast_quesnel = terra::mask(slope_rast_quesnel, lead_htop_rast_quesnel)
asp_cos_rast = terra::mask(asp_cos_rast_quesnel, lead_htop_rast_quesnel)
asp_sin_rast = terra::mask(asp_sin_rast_quesnel, lead_htop_rast_quesnel)
species_class_rast_quesnel = terra::mask(species_class_rast_quesnel, lead_htop_rast_quesnel)

lead_htop_rast_quesnel = terra::mask(lead_htop_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
elev_rast_quesnel = terra::mask(elev_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
slope_rast_quesnel = terra::mask(slope_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
asp_cos_rast_quesnel = terra::mask(asp_cos_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
asp_sin_rast_quesnel = terra::mask(asp_sin_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)
stemsha_L_rast_quesnel = terra::mask(stemsha_L_rast_quesnel, species_class_rast_quesnel, inverse=FALSE)

terra::plot(lead_htop_rast_quesnel, main = "lead_htop")
terra::plot(stemsha_L_rast_quesnel, main = "stemsha_L")
terra::plot(elev_rast_quesnel, main = "elevation")
terra::plot(slope_rast_quesnel, main = "slope")
terra::plot(asp_cos_rast_quesnel, main = "asp_cos")
terra::plot(asp_sin_rast_quesnel, main = "asp_sin")
terra::plot(species_class_rast_quesnel, main = "species_class")

names(elev_rast_quesnel) = "elev"
names(slope_rast_quesnel) = "slope"
names(asp_cos_rast_quesnel) = "asp_cos"
names(asp_sin_rast_quesnel) = "asp_sin"
names(species_class_rast_quesnel) = "species_class"
names(stemsha_L_rast_quesnel) = "stemsha_L"
names(lead_htop_rast_quesnel) = "lead_htop"

elev_raster_quesnel = raster::raster(elev_rast_quesnel)
slope_raster_quesnel = raster::raster(slope_rast_quesnel)
asp_cos_raster_quesnel = raster::raster(asp_cos_rast_quesnel)
asp_sin_raster_quesnel = raster::raster(asp_sin_rast_quesnel)
species_class_raster_quesnel = raster::raster(species_class_rast_quesnel)
stemsha_L_raster_quesnel = raster::raster(stemsha_L_rast_quesnel)
lead_htop_raster_quesnel = raster::raster(lead_htop_rast_quesnel)

covs_m1_quesnel = raster::stack(
  lead_htop_raster_quesnel,
  stemsha_L_raster_quesnel,
  elev_raster_quesnel, 
  slope_raster_quesnel,
  asp_cos_raster_quesnel, 
  asp_sin_raster_quesnel, 
  species_class_raster_quesnel)

p1 = levelplot(covs_m1_quesnel, layers=1, margin = list(FUN = median))
p2 = levelplot(covs_m1_quesnel)

# PLot log scale of rasters
# The zscaleLog argument controls whether the object will be log transformed before being passed to the panel function. Defaults to ‘NULL’, in which case the Raster* is not transformed. Other possible values are any number that works as a base for taking logarithm, ‘TRUE’ (which is equivalent to 10), and ‘“e”’ (for the natural logarithm). As a side effect, the colorkey is labeled differently. 
p3.1 = levelplot(lead_htop_raster_quesnel^2, zscaleLog=TRUE, contour=TRUE) 
p3.2 = levelplot(stemsha_L_raster_quesnel^2, zscaleLog=TRUE, contour=TRUE) 
p3.3 = levelplot(elev_raster_quesnel^2, zscaleLog=TRUE, contour=TRUE) 
p3.4 = levelplot(slope_raster_quesnel^2, zscaleLog=TRUE, contour=TRUE) 
p3.5 = levelplot(asp_cos_raster_quesnel^2, zscaleLog=TRUE, contour=TRUE) 
p3.6 = levelplot(asp_sin_raster_quesnel^2, zscaleLog=TRUE, contour=TRUE) 
p3.7 = levelplot(species_class_raster_quesnel^2, zscaleLog=TRUE, contour=TRUE) 


mean_covs_m1_quesnel = raster::cellStats(covs_m1_quesnel, mean)
p4 = levelplot(covs_m1_quesnel - mean_covs_m1_quesnel, par.settings = RdBuTheme())

# plot scatter matrix and distribution grids
p5.1 = splom(covs_m1_quesnel)
p5.2 = histogram(covs_m1_quesnel)
p5.3 = bwplot(covs_m1_quesnel)
p5.4 = vectorplot(elev_raster_quesnel, par,settings=RdBuTheme())
p5.5 = streamplot(elev_raster_quesnel)

# Import ground plot data
faib_psp <- read.csv("/media/seamus/128GB_WORKD/EFI-TCC/0_Caret_Predict_to_writeRasterOutput/Data/FAIB_PSP_20211028.csv")
print(as_tibble(faib_psp), n = 10)
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


# compare rasters with faib permanent sample plot data
library(MASS)
par(mfrow=c(4,4))
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
