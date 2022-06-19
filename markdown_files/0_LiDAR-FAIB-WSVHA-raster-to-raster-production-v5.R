
#filez_be_gaspard = list.files("/media/seamus/128GB_WORKD/EFI-TCC/LiDAR_Data/gaspard_region/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
#elev_raster_list_gaspard <- lapply(filez_be_gaspard, raster)
#elev_raster_gaspard = do.call(merge, c(elev_raster_list_gaspard, tolerance = 1))
#writeRaster(elev_raster_gaspard, filename = "/media/seamus/128GB_WORKD/EFI-TCC/LiDAR_Data/gaspard_region/elev_raster_1m_gaspard.tif", overwrite=TRUE)

#filez_be_quesnel = list.files("/media/seamus128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
#elev_raster_list_quesnel <- lapply(filez_be_quesnel, raster)
#elev_raster_quesnel = do.call(merge, c(elev_raster_list_quesnel, tolerance = 1))
#writeRaster(elev_raster_quesnel, filename = "/media/seamus/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/elev_raster_1m_quesnel.tif", overwrite=TRUE)

filez_be_ahbau = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/ahbau/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_ahbau <- lapply(filez_be_ahbau, raster)
elev_raster_ahbau = do.call(merge, c(elev_raster_list_ahbau, tolerance = 1))
writeRaster(elev_raster_ahbau, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/ahbau/elev_raster_1m_ahbau.tif", overwrite=TRUE)

filez_be_bells = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/bells/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_bells <- lapply(filez_be_bells, raster)
elev_raster_bells = do.call(merge, c(elev_raster_list_bells, tolerance = 1))
writeRaster(elev_raster_bells, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/bells/elev_raster_1m_bells.tif", overwrite=TRUE)

filez_be_big_valley = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/big_valley/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_big_valley <- lapply(filez_be_big_valley, raster)
elev_raster_big_valley = do.call(merge, c(elev_raster_list_big_valley, tolerance = 1))
writeRaster(elev_raster_big_valley, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/big_valley/elev_raster_1m_big_valley.tif", overwrite=TRUE)

filez_be_cariboo_lake = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/cariboo_lake/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_cariboo_lake <- lapply(filez_be_cariboo_lake, raster)
elev_raster_cariboo_lake = do.call(merge, c(elev_raster_list_cariboo_lake, tolerance = 1))
writeRaster(elev_raster_cariboo_lake, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/cariboo_lake/elev_raster_1m_cariboo_lake.tif", overwrite=TRUE)

filez_be_charleson_marvincreek = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/charleson_marvincreek/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_charleson_marvincreek <- lapply(filez_be_charleson_marvincreek, raster)
elev_raster_charleson_marvincreek = do.call(merge, c(elev_raster_list_charleson_marvincreek, tolerance = 1))
writeRaster(elev_raster_charleson_marvincreek, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/charleson_marvincreek/elev_raster_1m_charleson_marvincreek.tif", overwrite=TRUE)

filez_be_dash = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/dash/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_dash <- lapply(filez_be_dash, raster)
elev_raster_dash = do.call(merge, c(elev_raster_list_dash, tolerance = 1))
writeRaster(elev_raster_dash, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/dash/elev_raster_1m_dash.tif", overwrite=TRUE)

filez_be_hawks_creek = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/hawks_creek/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_hawks_creek <- lapply(filez_be_hawks_creek, raster)
elev_raster_hawks_creek = do.call(merge, c(elev_raster_list_hawks_creek, tolerance = 1))
writeRaster(elev_raster_hawks_creek, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/hawks_creek/elev_raster_1m_hawks_creek.tif", overwrite=TRUE)

filez_be_little_river = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/little_river/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_little_river <- lapply(filez_be_little_river, raster)
elev_raster_little_river = do.call(merge, c(elev_raster_list_little_river, tolerance = 1))
writeRaster(elev_raster_little_river, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/little_river/elev_raster_1m_little_river.tif", overwrite=TRUE)

filez_be_little_swift = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/little_swift/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_little_swift <- lapply(filez_be_little_swift, raster)
elev_raster_little_swift = do.call(merge, c(elev_raster_list_little_swift, tolerance = 1))
writeRaster(elev_raster_little_swift, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/little_swift/elev_raster_1m_little_swift.tif", overwrite=TRUE)

filez_be_mcintosh = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/mcintosh/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_mcintosh <- lapply(filez_be_mcintosh, raster)
elev_raster_mcintosh = do.call(merge, c(elev_raster_list_mcintosh, tolerance = 1))
writeRaster(elev_raster_mcintosh, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/mcintosh/elev_raster_1m_mcintosh.tif", overwrite=TRUE)

filez_be_meldrum = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/meldrum/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_meldrum <- lapply(filez_be_meldrum, raster)
elev_raster_meldrum = do.call(merge, c(elev_raster_list_meldrum, tolerance = 1))
writeRaster(elev_raster_meldrum, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/meldrum/elev_raster_1m_meldrum.tif", overwrite=TRUE)

filez_be_phillips_anahim_lake = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/phillips_anahim_lake/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_phillips_anahim_lake <- lapply(filez_be_phillips_anahim_lake, raster)
elev_raster_phillips_anahim_lake = do.call(merge, c(elev_raster_list_phillips_anahim_lake, tolerance = 1))
writeRaster(elev_raster_phillips_anahim_lake, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/phillips_anahim_lake/elev_raster_1m_phillips_anahim_lake.tif", overwrite=TRUE)

filez_be_piltz = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/piltz/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_piltz <- lapply(filez_be_piltz, raster)
elev_raster_piltz = do.call(merge, c(elev_raster_list_piltz, tolerance = 1))
writeRaster(elev_raster_piltz, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/piltz/elev_raster_1m_piltz.tif", overwrite=TRUE)

filez_be_punky_clisbako = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/punky_clisbako/BareEarth", full.names = T, all.files = FALSE, pattern = '.tif$') 
elev_raster_list_punky_clisbako <- lapply(filez_be_punky_clisbako, raster)
elev_raster_punky_clisbako = do.call(merge, c(elev_raster_list_punky_clisbako, tolerance = 1))
writeRaster(elev_raster_punky_clisbako, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/punky_clisbako/elev_raster_1m_punky_clisbako.tif", overwrite=TRUE)




#filez_vh_gaspard = list.files("/media/seamus/128GB_WORKD/EFI-TCC/LiDAR_Data/gaspard_region/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
#lead_htop_raster_list_gaspard <- lapply(filez_vh_gaspard, raster)
#lead_htop_raster_gaspard = do.call(merge, c(lead_htop_raster_list_gaspard, tolerance = 1))
#writeRaster(lead_htop_raster_gaspard, filename = "/media/seamus/128GB_WORKD/EFI-TCC/LiDAR_Data/gaspard_region/lead_htop_raster_1m_gaspard.tif", overwrite=TRUE)

#filez_vh_quesnel = list.files("/media/seamus/128GB_WORKD/EFI-TCC/LiDAR_Data/quesnel_region/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
#lead_htop_raster_list_quesnel <- lapply(filez_vh_quesnel, raster)
#lead_htop_raster_quesnel = do.call(merge, c(lead_htop_raster_list_quesnel, tolerance = 1))
#writeRaster(lead_htop_raster_quesnel, filename = "/media/seamus/128GB_WORKD/EFI-TCC/LiDAR_Data/gaspard_region/lead_htop_raster_1m_quesnel.tif", overwrite=TRUE)

filez_vh_ahbau = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/ahbau/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_ahbau <- lapply(filez_vh_ahbau, raster)
lead_htop_raster_ahbau = do.call(merge, c(lead_htop_raster_list_ahbau, tolerance = 1))
writeRaster(lead_htop_raster_ahbau, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/ahbau/lead_htop_raster_1m_ahbau.tif", overwrite=TRUE)

filez_vh_bells = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/bells/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_bells <- lapply(filez_vh_bells, raster)
lead_htop_raster_bells = do.call(merge, c(lead_htop_raster_list_bells, tolerance = 1))
writeRaster(lead_htop_raster_bells, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/bells/lead_htop_raster_1m_bells.tif", overwrite=TRUE)

filez_vh_big_valley = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/big_valley/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_big_valley <- lapply(filez_vh_big_valley, raster)
lead_htop_raster_big_valley = do.call(merge, c(lead_htop_raster_list_big_valley, tolerance = 1))
writeRaster(lead_htop_raster_big_valley, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/big_valley/lead_htop_raster_1m_big_valley.tif", overwrite=TRUE)

filez_vh_cariboo_lake = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/cariboo_lake/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_cariboo_lake <- lapply(filez_vh_cariboo_lake, raster)
lead_htop_raster_cariboo_lake = do.call(merge, c(lead_htop_raster_list_cariboo_lake, tolerance = 1))
writeRaster(lead_htop_raster_cariboo_lake, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/cariboo_lake/lead_htop_raster_1m_cariboo_lake.tif", overwrite=TRUE)

filez_vh_charleson_marvincreek = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/charleson_marvincreek/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_charleson_marvincreek <- lapply(filez_vh_charleson_marvincreek, raster)
lead_htop_raster_charleson_marvincreek = do.call(merge, c(lead_htop_raster_list_charleson_marvincreek, tolerance = 1))
writeRaster(lead_htop_raster_charleson_marvincreek, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/chaleson_marvincreek/lead_htop_raster_1m_charleson_marvincreek.tif", overwrite=TRUE)

filez_vh_dash = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/dash/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_dash <- lapply(filez_vh_dash, raster)
lead_htop_raster_dash = do.call(merge, c(lead_htop_raster_list_dash, tolerance = 1))

filez_vh_hawks_creek = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/hawks_creek/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_hawks_creek <- lapply(filez_vh_hawks_creek, raster)
lead_htop_raster_hawks_creek = do.call(merge, c(lead_htop_raster_list_hawks_creek, tolerance = 1))
writeRaster(lead_htop_raster_hawks_creek, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/hawks_creek/lead_htop_raster_1m_hawks_creek.tif", overwrite=TRUE)

filez_vh_little_river = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/little_river/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_little_river <- lapply(filez_vh_little_river, raster)
lead_htop_raster_little_river = do.call(merge, c(lead_htop_raster_list_little_river, tolerance = 1))
writeRaster(lead_htop_raster_little_river, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/little_river/lead_htop_raster_1m_little_river.tif", overwrite=TRUE)

filez_vh_little_swift = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/little_swift/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_little_swift <- lapply(filez_vh_little_swift, raster)
lead_htop_raster_little_swift = do.call(merge, c(lead_htop_raster_list_little_swift, tolerance = 1))
writeRaster(lead_htop_raster_little_swift, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/little_swift/lead_htop_raster_1m_little_swift.tif", overwrite=TRUE)

filez_vh_mcintosh = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/mcintosh/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_mcintosh <- lapply(filez_vh_mcintosh, raster)
lead_htop_raster_mcintosh = do.call(merge, c(lead_htop_raster_list_mcintosh, tolerance = 1))
writeRaster(lead_htop_raster_mcintosh, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/mcintosh/lead_htop_raster_1m_mcintosh.tif", overwrite=TRUE)

filez_vh_meldrum = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/meldrum/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_meldrum <- lapply(filez_vh_meldrum, raster)
lead_htop_raster_meldrum = do.call(merge, c(lead_htop_raster_list_meldrum, tolerance = 1))
writeRaster(lead_htop_raster_meldrum, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/meldrum/lead_htop_raster_1m_meldrum.tif", overwrite=TRUE)

filez_vh_phillips_anahim_lake = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/phillips_anahim_lake/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_phillips_anahim_lake <- lapply(filez_vh_phillips_anahim_lake, raster)
lead_htop_raster_phillips_anahim_lake = do.call(merge, c(lead_htop_raster_list_phillips_anahim_lake, tolerance = 1))
writeRaster(lead_htop_raster_phillips_anahim_lake, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/phillips_anahim_lake/lead_htop_raster_1m_phillips_anahim_lake.tif", overwrite=TRUE)

filez_vh_piltz = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/piltz/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_piltz <- lapply(filez_vh_piltz, raster)
lead_htop_raster_piltz = do.call(merge, c(lead_htop_raster_list_piltz, tolerance = 1))
writeRaster(lead_htop_raster_piltz, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/piltz/lead_htop_raster_1m_piltz.tif", overwrite=TRUE)

filez_vh_punky_clisbako = list.files("/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/punky_clisbako/VegHt", full.names = T, all.files = FALSE, pattern = '.tif$') 
lead_htop_raster_list_punky_clisbako <- lapply(filez_vh_punky_clisbako, raster)
lead_htop_raster_punky_clisbako = do.call(merge, c(lead_htop_raster_list_punky_clisbako, tolerance = 1))
writeRaster(lead_htop_raster_punky_clisbako, filename = "/media/seamus/Ubuntu 22_04 LTS amd64/mosaics/punky_clisbako/lead_htop_raster_1m_punky_clisbako.tif", overwrite=TRUE)
```