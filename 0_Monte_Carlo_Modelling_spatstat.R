
library(rgdal)
library(raster)
library(terra)
library(spatstat)
library(rgeos)
library(sp)
library(sf)
library(spatial)
library(dplyr)
library(psych)

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
faib_psp_sf

elev_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/elev_raster_100m_quesnel.tif")
elev_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/elev_raster_100m_gaspard.tif")
slope_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/slope_raster_100m_quesnel.tif")
slope_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/slope_raster_100m_gaspard.tif")
asp_cos_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/asp_cos_raster_100m_quesnel.tif")
asp_cos_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/asp_cos_raster_100m_gaspard.tif")
asp_sin_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/asp_sin_raster_100m_quesnel.tif")
asp_sin_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/asp_sin_raster_100m_gaspard.tif")
lead_htop_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/lead_htop_raster_100m_quesnel.tif")
lead_htop_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/lead_htop_raster_100m_gaspard.tif")
stemsha_L_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/stemsha_L_raster_100m_quesnel.tif")
stemsha_L_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/stemsha_L_raster_100m_gaspard.tif")
species_class_rast_quesnel = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/species_class_raster_100m_quesnel.tif")
species_class_rast_gaspard = terra::rast("/media/seamus/128GB_WORKD/data/raster/tcc/masked-covariates/species_class_raster_100m_gaspard.tif")
names(lead_htop_rast_gaspard) = "lead_htop"
names(lead_htop_rast_quesnel) = "lead_htop"
lead_htop_sv_quesnel = as.polygons(lead_htop_rast_quesnel)
lead_htop_sv_gaspard = as.polygons(lead_htop_rast_gaspard)
lead_htop_sf_quesnel = sf::st_as_sf(lead_htop_sv_quesnel)
lead_htop_sf_gaspard = sf::st_as_sf(lead_htop_sv_gaspard)

#merge faib with rasters to widen area before building a window object
#lead_htop_sf_gaspard_tibble = as_tibble(lead_htop_sf_gaspard)
#lead_htop_sf_quesnel_tibble = as_tibble(lead_htop_sf_quesnel)
#faib_psp_sf_tibble = as_tibble(faib_psp_sf)
#lead_htop_sf_gaspard_df = as.data.frame(lead_htop_sf_gaspard)
#lead_htop_sf_quesnel_df = as.data.frame(lead_htop_sf_quesnel)
#faib_psp_sf_df = as.data.frame(faib_psp_sf)
#psych::describe(lead_htop_sf_gaspard_df)
#psych::describe(lead_htop_sf_quesnel_df)
#psych::describe(faib_psp_sf_df)
faib_psp_mpt = st_cast(faib_psp_sf, "MULTIPOINT")
faib_psp_ply = faib_psp_mpt %>%
  dplyr::group_by(species_class) %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")

lead_htop_sf_gaspard = st_cast(lead_htop_sf_gaspard, "POLYGON")
lead_htop_sf_quesnel = st_cast(lead_htop_sf_quesnel, "POLYGON")
lead_htop_sv_gaspard = terra::vect(lead_htop_sf_gaspard)
lead_htop_sv_quesnel = terra::vect(lead_htop_sf_quesnel)
faib_psp_sv = terra::vect(faib_psp_ply)

full_window_sv = rbind(lead_htop_sv_quesnel, lead_htop_sv_gaspard)
full_window_sv = rbind(lead_htop_sv_quesnel, faib_psp_sv)
full_window_sf = st_as_sf(full_window_sv)
plot(full_window_sv)

#lead_htop_sf_gaspard_cnt = st_centroid(lead_htop_sf_gaspard)
#lead_htop_sf_quesnel_cnt = st_centroid(lead_htop_sf_quesnel)
#full_window <- st_as_sf(data.table::rbindlist(list(faib_psp_sf, lead_htop_sf_gaspard_cnt, lead_htop_sf_quesnel_cnt), fill = TRUE))

# derive bbox for faib to use as observational window 
full_window_bbox = st_as_sfc(st_bbox(full_window_sf))
plot(st_geometry(faib_psp_sf), pch = 18, col="blue")
plot(lead_htop_sf_quesnel, add=T, col="red")
plot(lead_htop_sf_gaspard, add=T, col="red")
plot(st_geometry(full_window_bbox), add=T)
st_write(full_window_bbox, dsn = "/media/seamus/128GB_WORKD/data/vector/", 
         layer="full_window_box", driver="ESRI Shapefile")

