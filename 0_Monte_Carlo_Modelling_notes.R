plot(faib_psp$stemsha_L, faib_psp$wsvha_L, pch = 19, frame = FALSE)
abline(lm(faib_psp$wsvha_L ~ faib_psp$stemsha_L), add=T, col="blue")




stemsha_L_raster_df = as.data.frame(rasterToPoints(stemsha_L_raster))
dist.fun = approxfun(density(stemsha_L_raster_df$stemsha_L))
#faib_psp = dplyr::sample_n(faib_psp, 500, weight_by = dist.fun(faib_psp$stemsha_L), replace = TRUE)
#faib_psp = dplyr::sample_n(faib_psp, 600, weight_by = dist.fun, replace = TRUE)
#truehist(faib_psp$stemsha_L, main="Stems/ha (Bootstrapped FAIB)")

## Import sampling and Monte Carlo together used as variance reduction technique


set.seed(123)
dist.fun = approxfun(density(stemsha_L_raster_df$stemsha_L))
hist(dist.fun)
