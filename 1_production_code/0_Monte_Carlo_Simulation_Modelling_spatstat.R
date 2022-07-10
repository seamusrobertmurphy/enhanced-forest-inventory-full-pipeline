
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


faib_psp <- read.csv("/media/seamus/128GB_WORKD/EFI-TCC/0_Caret_Predict_to_writeRasterOutput/Data/FAIB_PSP_20211028.csv")
faib_psp = dplyr::rename(faib_psp, x = bcalb_x)
faib_psp = dplyr::rename(faib_psp, y = bcalb_y)
faib_psp_sf = st_as_sf(faib_psp, coords = c("x", "y"), crs = 3005)

faib_psp_sf = subset(faib_psp_sf, util == '12.5')
faib_psp_sf$spc_live1 = as.factor(faib_psp_sf$spc_live1)
faib_psp_sf =  subset(
  faib_psp_sf, spc_live1=='PL' | spc_live1=='PLI' | spc_live1=='FD'| spc_live1=='FDI' | 
    spc_live1=='SB' | spc_live1=='SE' | spc_live1=='SW' | spc_live1=='SX' | 
    spc_live1=='CW' | spc_live1=='HW' | spc_live1=='BL' | spc_live1=='LW')

faib_psp_sf$species_class = dplyr::recode(
  faib_psp_sf$spc_live1, PL = 1, PLI = 1, SB = 2, SE = 2, SX = 2, 
  FD = 3, FDI = 3, CW = 3, HW = 4, BL = 5, LW = 6)

faib_psp_sf$asp_cos = cos((faib_psp_sf$aspect * pi) / 180)
faib_psp_sf$asp_sin = sin((faib_psp_sf$aspect * pi) / 180)
faib_psp_sf = faib_psp_sf[c("elev", "slope", "asp_cos", "asp_sin", "lead_htop", "species_class", "stemsha_L", "wsvha_L")]
faib_psp_sf$elev[faib_psp_sf$elev <= 0] = NA
faib_psp_sf$slope[faib_psp_sf$slope <= 0] = NA
faib_psp_sf$lead_htop[faib_psp_sf$lead_htop < 2] = NA
faib_psp_sf$stemsha_L[faib_psp_sf$stemsha_L <= 0] = NA
faib_psp_sf$wsvha_L[faib_psp_sf$wsvha_L <= 1] = NA
faib_psp_sf = subset(faib_psp_sf, stemsha_L < 864)
faib_psp_sf = na.omit(faib_psp_sf)
faib_psp_df = as.data.frame(faib_psp_sf)

stemsha_L_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/stemsha_L_raster_100m_gaspard.tif")
names(stemsha_L_rast_gaspard) = "stemsha_L"
stemsha_L_sv_gaspard = as.polygons(stemsha_L_rast_gaspard)
stemsha_L_sf_gaspard = sf::st_as_sf(stemsha_L_sv_gaspard)

set.seed(8888)
# Customized simulation for bootstrap resampling
x = stemsha_L_sf_gaspard$stemsha_L
dens.rast = density(stemsha_L_sf_gaspard$stemsha_L, adjust=0.8)
dens.faib = density(faib_psp_df$stemsha_L, adjust=0.8)
dens.fun.prob = approxfun(dens.rast$y)
dens.fun = approxfun(dens.rast)

# number of simulations B, and exponentials n
B = 1000
n = 40

# generate resampling matrix with replacement
resample.faib <- matrix(sample(
  dens.faib$x, B * n, replace = TRUE, prob=dens.rast$y))
simulation <- apply(resample.faib, 1, mean)
summary(simulation)

bootstrap.faib.prob = (sample_n(
  faib_psp_df, B * n, prob = dens.fun.prob(faib_psp_df$stemsha_L), replace = T))
simulation.prob <- apply(bootstrap.faib.prob, 1, mean)

bootstrap.faib.approx = (sample_n(
  faib_psp_df, B * n, weight_by = dens.fun(faib_psp_df$stemsha_L), replace = T))
simulation.approx <- apply(bootstrap.faib.approx, 1, mean)

bootstrap.faib = (sample_n(
  faib_psp_df, 1000, weight_by = dens.fun(dens.faib), replace = T))
simulation.faib = apply(bootstrap.faib, 1, mean)

summary(faib_psp_df$stemsha_L)
summary(bootstrap.faib.prob$stemsha_L)
summary(bootstrap.faib.approx$stemsha_L)
summary(bootstrap.faib$stemsha_L)
summary(simulation.prob)
summary(simulation.approx)
summary(simulation.faib)

hist(faib_psp_df$stemsha_L)
hist(bootstrap.faib.prob$stemsha_L)
hist(bootstrap.faib.approx$stemsha_L)
hist(bootstrap.faib$stemsha_L)

str(bootstrap.faib.approx)
str(bootstrap.faib.prob)

class(bootstrap.faib.approx)
class(bootstrap.faib)


library(MASS)

# data points
x <- faib_psp_df$stemsha_L

# number of simulations B, and exponentials n
B = 1000
n = 40

# generate resampling (1000,40) matrix with replacement
set.seed(133742069)
resample <- matrix(sample(x, n * B, replace = TRUE),  B, n)
simulation <- apply(resample, 1, mean)
class(simulation)
summary(simulation)

# SPATSTAT workflow = df > sf > sp > sp-SpatialPoints > ppp < sp < df 
# convert the original faib sf object to sp-Spatial to sp-SpatialPoints to ppp object
# (ignore all the warning messages that follow, and 
# notice we need to force call units of measurement)
#faib_psp_spts = as(faib_psp_sp, "SpatialPoints") 
faib_psp_sp = as(faib_psp_sf, "Spatial") 
faib_psp_ppp = as(faib_psp_sp, "ppp") 
unitname(faib_psp_ppp)=c("meter", "meter")
terra::crs(faib_psp_sp)
sum(is.na(faib_psp_sf))
sum(is.na(faib_psp_sp))
sum(is.na(faib_psp_ppp))
class(faib_psp_ppp)

# in SPATSTAT we use the marks/unmarks function to call which variable to present/analyse 
#unmark data points OR... 
marks(faib_psp_ppp) <- NULL
marks(faib_psp_ppp) <- faib_psp_ppp$species_class
marks(faib_psp_ppp) <- NULL

## DERIVE OBSERVATIONAL WINDOW (fyi: SPATSTAT package is super pernickety, buggy, 
# and needs lots of attention and or consulting with source code. Awesome package but 
# generally just causes a weekly headache.
lead_htop_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/lead_htop_raster_100m_quesnel.tif")
lead_htop_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/lead_htop_raster_100m_gaspard.tif")
names(lead_htop_rast_gaspard) = "lead_htop"
names(lead_htop_rast_quesnel) = "lead_htop"
lead_htop_sv_quesnel = as.polygons(lead_htop_rast_quesnel)
lead_htop_sv_gaspard = as.polygons(lead_htop_rast_gaspard)
lead_htop_sf_quesnel = sf::st_as_sf(lead_htop_sv_quesnel)
lead_htop_sf_gaspard = sf::st_as_sf(lead_htop_sv_gaspard)

# merge data objects to expand our extent to widest possible window
faib_psp_mpt = st_cast(faib_psp_sf, "MULTIPOINT")
faib_psp_ply = faib_psp_mpt %>%
  dplyr::group_by(species_class) %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")

# polygonise sf object using terra funcs for purpose of deriving window shapefile 
lead_htop_sf_gaspard = st_cast(lead_htop_sf_gaspard, "POLYGON")
lead_htop_sf_quesnel = st_cast(lead_htop_sf_quesnel, "POLYGON")
lead_htop_sv_gaspard = terra::vect(lead_htop_sf_gaspard)
lead_htop_sv_quesnel = terra::vect(lead_htop_sf_quesnel)
faib_psp_sv = terra::vect(faib_psp_ply)

# merge all data objects as spatVectors and convert back
full_window_sv = rbind(lead_htop_sv_quesnel, lead_htop_sv_gaspard)
full_window_sv = rbind(lead_htop_sv_quesnel, faib_psp_sv)
full_window_sf = st_as_sf(full_window_sv)
plot(full_window_sv) # This looks like error but its only needed for window extent

# handy bypass here using centroids. However it fails to capture the full 
# window extent because the polygons are reduced down in the conversion process
#lead_htop_sf_gaspard_cnt = st_centroid(lead_htop_sf_gaspard)
#lead_htop_sf_quesnel_cnt = st_centroid(lead_htop_sf_quesnel)
#full_window <- st_as_sf(data.table::rbindlist(list(faib_psp_sf, lead_htop_sf_gaspard_cnt, lead_htop_sf_quesnel_cnt), fill = TRUE))

# derive bbox from new merged extent 
full_window_bbox = st_as_sfc(st_bbox(full_window_sf))
plot(st_geometry(faib_psp_sf), pch = 18, col="blue")
plot(lead_htop_sf_quesnel, add=T, col="red")
plot(lead_htop_sf_gaspard, add=T, col="red")
plot(st_geometry(full_window_bbox), add=T)
#st_write(full_window_bbox, dsn = "/media/seamus/128GB_WORKD/data/vector/", 
 #        layer="full_window_box-2", driver="ESRI Shapefile")

# define observational window (& units) and assign it to new faib ppp object
window = spatstat.geom::as.owin(full_window_bbox, unitname="meters")
spatstat.geom::Window(faib_psp_ppp)=window

## Import covariates and prepare as SPATSTAT objects
# SPATSTAT workflow: sf > sp-Spatial > sp-SpatialPoints > ppp > im.pp
# SPATSTAT shortcut: raster.tif > im.Raster > im.Raster.log
elev_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/elev_raster_100m_quesnel.tif")
elev_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/elev_raster_100m_gaspard.tif")
lead_htop_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/lead_htop_raster_100m_quesnel.tif")
lead_htop_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/lead_htop_raster_100m_gaspard.tif")
slope_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/slope_raster_100m_quesnel.tif")
slope_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/slope_raster_100m_gaspard.tif")
asp_cos_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/asp_cos_raster_100m_quesnel.tif")
asp_cos_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/asp_cos_raster_100m_gaspard.tif")
asp_sin_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/asp_sin_raster_100m_quesnel.tif")
asp_sin_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/asp_sin_raster_100m_gaspard.tif")
species_class_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/species_class_raster_100m_quesnel.tif")
species_class_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/species_class_raster_100m_gaspard.tif")
stemsha_L_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/stemsha_L_raster_100m_quesnel.tif")
stemsha_L_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/stemsha_L_raster_100m_gaspard.tif")
# spatRaster to Raster 
elev_raster_quesnel = raster::raster(elev_rast_quesnel)
elev_raster_gaspard = raster::raster(elev_rast_gaspard)
lead_htop_raster_quesnel = raster::raster(lead_htop_rast_quesnel)
lead_htop_raster_gaspard = raster::raster(lead_htop_rast_gaspard)
slope_raster_quesnel = raster::raster(slope_rast_quesnel)
slope_raster_gaspard = raster::raster(slope_rast_gaspard)
asp_cos_raster_quesnel = raster::raster(asp_cos_rast_quesnel)
asp_cos_raster_gaspard = raster::raster(asp_cos_rast_gaspard)
asp_sin_raster_quesnel = raster::raster(asp_sin_rast_quesnel)
asp_sin_raster_gaspard = raster::raster(asp_sin_rast_gaspard)
species_class_raster_quesnel = raster::raster(species_class_rast_quesnel)
species_class_raster_gaspard = raster::raster(species_class_rast_gaspard)
stemsha_L_raster_quesnel = raster::raster(stemsha_L_rast_quesnel)
stemsha_L_raster_gaspard = raster::raster(stemsha_L_rast_gaspard)
# Raster to Image.im 
elev_raster_quesnel_im = spatstat.geom::as.im(elev_raster_quesnel)
elev_raster_gaspard_im = spatstat.geom::as.im(elev_raster_gaspard)
lead_htop_raster_quesnel_im = spatstat.geom::as.im(lead_htop_raster_quesnel)
lead_htop_raster_gaspard_im = spatstat.geom::as.im(lead_htop_raster_gaspard)
slope_raster_quesnel_im = spatstat.geom::as.im(slope_raster_quesnel)
slope_raster_gaspard_im = spatstat.geom::as.im(slope_raster_gaspard)
asp_cos_raster_quesnel_im = spatstat.geom::as.im(asp_cos_raster_quesnel)
asp_cos_raster_gaspard_im = spatstat.geom::as.im(asp_cos_raster_gaspard)
asp_sin_raster_quesnel_im = spatstat.geom::as.im(asp_sin_raster_quesnel)
asp_sin_raster_gaspard_im = spatstat.geom::as.im(asp_sin_raster_gaspard)
species_class_raster_quesnel_im = spatstat.geom::as.im(species_class_raster_quesnel)
species_class_raster_gaspard_im = spatstat.geom::as.im(species_class_raster_gaspard)
stemsha_L_raster_quesnel_im = spatstat.geom::as.im(stemsha_L_raster_quesnel)
stemsha_L_raster_gaspard_im = spatstat.geom::as.im(stemsha_L_raster_gaspard)


# Pre-MonteCarlo EDA: Cross-tabulate faib variables (watch for NA's)
group_by(faib_psp_df, species_class) %>%
  summarise(count = n(),
            mean = mean(elev, na.rm = TRUE),
            sd = sd(elev, na.rm = TRUE))

group_by(faib_psp_df, species_class) %>%
  summarise(count = n(),
            mean = mean(slope, na.rm = TRUE),
            sd = sd(slope, na.rm = TRUE))

group_by(faib_psp_df, species_class) %>%
  summarise(count = n(),
            mean = mean(asp_cos, na.rm = TRUE),
            sd = sd(asp_cos, na.rm = TRUE))

group_by(faib_psp_df, species_class) %>%
  summarise(count = n(),
            mean = mean(asp_sin, na.rm = TRUE),
            sd = sd(asp_sin, na.rm = TRUE))

group_by(faib_psp_df, species_class) %>%
  summarise(count = n(),
            mean = mean(lead_htop, na.rm = TRUE),
            sd = sd(lead_htop, na.rm = TRUE))

group_by(faib_psp_df, species_class) %>%
  summarise(count = n(),
            mean = mean(stemsha_L, na.rm = TRUE),
            sd = sd(stemsha_L, na.rm = TRUE))

group_by(faib_psp_df, species_class) %>%
  summarise(count = n(),
            mean = mean(wsvha_L, na.rm = TRUE),
            sd = sd(wsvha_L, na.rm = TRUE))

faib_crossTab = table(faib_psp_sp$species_class, faib_psp_sp$asp_cos)
prop.table(faib_crossTab, 1)
prop.table(faib_crossTab, 2)
table(faib_psp_sp$species_class)/length((faib_psp_sp$species_class))
hist(faib_psp_df$elev)
hist(faib_psp_df$slope)
hist(faib_psp_df$asp_cos)
hist(faib_psp_df$asp_sin)
hist(faib_psp_df$lead_htop)
hist(faib_psp_df$species_class)
hist(faib_psp_df$wsvha_L)
hist(faib_psp_df$stemsha_L)

# Check ppp objects behave correctly
summary.factor(faib_psp_sp$species_class)
marks(faib_psp_ppp) = faib_psp_ppp$species_class
pinus_ctt = subset(faib_psp_sp,faib_psp_sp$species_class==1)
picea_spp = subset(faib_psp_sp,faib_psp_sp$species_class==2)
pseudo_m = subset(faib_psp_sp,faib_psp_sp$species_class==3)
abies_a = subset(faib_psp_sp,faib_psp_sp$species_class==5)
faib_psp_sp$species_class = factor(faib_psp_sp$species_class, 
  levels = c(1,2,3,5), labels = c(
    "Pi,PLi", "Sb,Sw,Sx,Se","Fd,Fdi","Ba")) # watch out for placement of commas here
p = plot(faib_psp_ppp, type="n", main="FAIB Sample Plots in PPP format")
points(pinus_ctt,pch=20,cex=0.8,col="blue")
points(picea_spp,pch=20,cex=0.8,col="green3")
points(pseudo_m,pch=20,cex=0.8,col="brown")
points(abies_a,pch=20,cex=0.8,col="purple")
species_palette = c("blue", "green3", "brown", "purple")
legend("topright",legend=levels(faib_psp_sp$species_class),
       fill=species_palette)

# prepare leave-one-out bandwith kernel for input into MonteCarlo simulation  
# compute optimal bandwidth, remember we did a lot of this in npreg report 
# back in 2021 that report is stored in the Udrive 'reports' folder'. 
# Might be useful for quick catch-up on bandwidth optomization
bw_likelihood_kernppp = bw.ppl(faib_psp_ppp)
plot(bw_likelihood_kernppp, main="FAIB-derived LCV-sampled Bootstrapping Kernel Estimator", cex.main=0.8)
marks(faib_psp_ppp) = NULL


# Its useful to quick fit different kernel estimators here. 
# No MonteCarlo runtime yet (next section), but its worth 
# playing around with the kernel parameter below to quickly 
# visualize and compare the different reshaping effects of 
# the kernels available. Its worth doing this before the 
# simulation begins. Also not that heat maps & persp plots are 
# more suited to spatially coherent data, not dispersed faib plots. 
# just some examples

epanechnikov_estimator = density(faib_psp_ppp, 
  sigma=10, kernel="epanechnikov", diggle=TRUE)
plot(epanech_dens_estimate, las=1)
contour(epanech_dens_estimate, add=TRUE)
persp(epanech_dens_estimate)


# run MonteCarlo resampling simulations (99999...) 
# I grabbed a few screenshots here for later references 
# and uploaded in new MonteCarlo subfolder. Theres quite a 
# bit to run through in the SPATSTAT functions. Tricky bit here 
# is figuring out which tools we need for model sharpening and 
# how to combine them. Seems majority of documentation is about 
# Bayesian tasks like data mining or validation.
set.seed(8888)

# Inhomogenous kernel - useful for forcing a fit 
mcarlo_kinhom999_optomizedEdge = envelope(
  faib_psp_ppp, Kinhom, nsim=999, correction="best",global = TRUE)

# Ripley's isotropic K-function - heavy spatial weighting
mcarlo_Kest999_isoTropicEdge = envelope(
  faib_psp_ppp, Kest, nsim=999, correction="Kest",global = TRUE)

# G-function enveloping - conservative constraints
mcarlo_Gest999_gestEdge = envelope(
  faib_psp_ppp, Gest, nsim=999, correction="Gest",global = TRUE)

# L-function enveloping - used for forcing one-side critical bias 
mcarlo_Lest999_erosionEdge = envelope(
  faib_psp_ppp, Lest, nsim=999, alternative="erode")


# custom fitting - using Kinhom for reshaping power  
stemRaster_density = density(stemsha_L_raster_gaspard, sigma=bw.diggle, positive=T)
stemRaster_density_int = density(stemsha_L_raster_gaspard)
plot(stemRaster_density)
plot(stemRaster_density_int)

faib_kppm_proc = kppm(faib_psp_ppp, clusters="MatClust")
faib_kppm_proc_int = kppm(faib_psp_ppp ~ 1, clusters = "MatClust")

trendFitted_Kinhom99 = envelope(
  faib_psp_ppp, Kinhom, 
  lambda=faib_kppm_proc, nsim=99)

lambdaFitted_Kinhom = envelope(
  faib_psp_ppp, Kinhom, sigma=bw.diggle,
  lambda=faib_kppm_proc_int, nsim=99)

outOFbox_Fitted = envelope(
  faib_psp_ppp, Kinhom, sigma=bw.diggle,
  lambda=rpoispp(stemsha_L_raster_gaspard_im))

# internally-based default 
plot(envelope(faib_kppm_proc, Kinhom, lambda=fit, nsim=99))
plot(envelope(faib_kppm_proc_int, Kinhom, lambda=fit, nsim=99))

# Thomas-Cox modelling functions SPATSTAT raster predictions
faib_psp_ppp_CoxT_isotropEpanech = kppm(
  faib_psp_ppp ~ elev_raster_gaspard_im + 
    slope_raster_gaspard_im + 
    asp_cos_raster_gaspard_im + 
    asp_sin_raster_gaspard_im +
    lead_htop_raster_gaspard_im + 
    species_class_raster_gaspard_im, 
  kernel="epanechnikov", clusters="MatClust",
  correction="isotropic", "Thomas", 
  sigma=bw_likelihood_kernppp)






