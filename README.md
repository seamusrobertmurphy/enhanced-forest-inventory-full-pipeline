Script Review: “0_EFI_CaretEnsemble\_…R”
================
SRM-LQ
24/01/2022

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
edits made during the last meeting. There was also mention of the need
for better ways to edit and peer-review future script development. Some
R-users have recommended using trackdown with github functions and
google docs. These can allow some forms of collaboration with code
editing or at least exchanges of iterative coding done locally and
remotely. This document written as a quick trial run to get that up and
running.

The exported table of contents below presents a tentative pipeline of
our workflow, which we also discussed editing and rearranging in places
for improved parsimony. For peer-reviewing, we can comment on these
Rmarkdown reports directly to the pdf attachment using the usual callout
boxes and we can also edit the backend code and push these commits to
the github repository for downloading locally or forking remotely This
gives us a kind of double-layered privacy so that no html.docs or data
sources are available beyond the repo access. When cloning the github
repo \[@hester\] you will find .gitignore rules that include that
“Data/” folder otherwise shared via the project drive. Worth noting that
for reducing word limit not all backend code was made visible in the
report.

* See '0_Caret_Predict_to_writeRasterOutput.md' file for full report and outputs... * 
* (https://github.com/seamusrobertmurphy/0_Caret_Predict_to_writeRasterOutput/blob/0426c60811d9abd433a93abb7c51403d4a5cd3a6/0_Caret_Predict_to_writeRasterOutput.md)*
 

