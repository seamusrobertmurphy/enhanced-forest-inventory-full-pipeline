Script Review: “0_EFI_CaretEnsemble\_…R”
================
SRM-LQ
24/01/2022

*See '0_Caret_Predict_to_writeRasterOutput.md' file for full report and outputs:* 
(https://github.com/seamusrobertmurphy/0_Caret_Predict_to_writeRasterOutput/blob/0426c60811d9abd433a93abb7c51403d4a5cd3a6/0_Caret_Predict_to_writeRasterOutput.md)*
 

-   [Action](#action)
-   [1 Import](#import)
    -   [1.1 Import permanent sample plot
        data](#import-permanent-sample-plot-data)
    -   [1.2 Import AOI and VRI layers to derive bbox and species
        raster](#import-aoi-and-vri-layers-to-derive-bbox-and-species-raster)
    -   [1.3 Import LiDAR data and derive terrain
        rasters](#import-lidar-data-and-derive-terrain-rasters)
-   [2 Tidy](#tidy)
    -   [2.1 Tidy permanent sample plot
        data](#tidy-permanent-sample-plot-data)
    -   [2.2 Tidy raster covariates](#tidy-raster-covariates)
-   [3 Transform](#transform)
    -   [3.1 Explore data
        transformations](#explore-data-transformations)
    -   [3.2 Apply data transformations](#apply-data-transformations)
-   [4 Model](#model)
    -   [4.1 Model 1: ‘M1.svm.radial’](#model-1-m1svmradial)
-   [5 Visualize](#visualize)

## Action

This is an R Markdown document showing the script run-through and code
edits made during the last meeting. The exported table of contents below 
presents a tentative pipeline of the discussed workflow though still room
for improved parsimony. For peer-reviewing, we can comment on the
Rmarkdown reports directly or go ahead and make code edits to the 
github repository by downloading locally and forking remotely. This
gives us a kind of double-layered privacy so that no html.docs or data
sources are available beyond the repo access. After cloning the github
repo you will find the proposed .gitignore rules that include
“Data/” folder otherwise shared via the project drive. Worth noting that
for reducing word limit not all backend code was made visible in the
report.



