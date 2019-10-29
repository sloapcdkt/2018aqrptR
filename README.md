# 2018aqrptR
Data and R code for graphs and analysis in 2018 Annual Air Quality Report for San Luis Obispo County.

## Details
The 2018 AQ Report is a product of the [San Luis Obispo County Air Pollution Control District](http://www.slocleanair.org/) and will be available on the District website [here](http://www.slocleanair.org/library/air-quality-reports.php) once finalized.

## Data Sources
Almost all data used in the report were downloaded from EPA's [Air Quality System (AQS)](https://www.epa.gov/aqs). These raw data downloads are provided as `.txt` files. Data from California Department of Parks and Recreation's "S1" meteorology tower were obtained from their data management systems, and is provided in the `.csv` files. The data in Tables 3 and 4 are from AQS AMP440 and AMP450 reports; these are provided as `.pdf` files.

## Analyses and Figures
Scripts for reproducing the analyses and figures in the report are provided as `.R` files. `00_AQSloader.R` contains a single function for loading certain types files spit out by AQS. Most of the other scripts make use of this function. Otherwise, the different scripts are independent, i.e. `00_ozone.R` depends on having sourced `AQSloader.R` but not on having run any of the other scripts. 

## Dependencies
The following packages will be needed: `knitr`, `nlme`, `openair`, `RColorBrewer`, `reshape2`, `svglite`, and `VGAM`. All are available on [CRAN](https://cran.r-project.org/).

### Session Info:
```
R version 3.6.1 (2019-07-05)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

Random number generation:
 RNG:     Mersenne-Twister 
 Normal:  Inversion 
 Sample:  Rounding 
 
locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] splines   stats4    stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] VGAM_1.1-1     reshape2_1.4.3

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.2          pillar_1.4.2        compiler_3.6.1      RColorBrewer_1.1-2  plyr_1.8.4          tools_3.6.1        
 [7] digest_0.6.20       packrat_0.5.0       evaluate_0.14       lubridate_1.7.4     tibble_2.1.3        nlme_3.1-140       
[13] lattice_0.20-38     mgcv_1.8-28         pkgconfig_2.0.2     rlang_0.4.0         Matrix_1.2-17       rstudioapi_0.10    
[19] mapproj_1.2.6       yaml_2.2.0          xfun_0.9            hexbin_1.27.3       dplyr_0.8.3         stringr_1.4.0      
[25] openair_2.6-5       cluster_2.1.0       knitr_1.24          gdtools_0.2.0       systemfonts_0.1.1   maps_3.3.0         
[31] grid_3.6.1          tidyselect_0.2.5    svglite_1.2.2       glue_1.3.1          R6_2.4.0            rmarkdown_1.15     
[37] foreign_0.8-71      sp_1.3-1            latticeExtra_0.6-28 purrr_0.3.2         tidyr_0.8.3         magrittr_1.5       
[43] htmltools_0.3.6     maptools_0.9-5      MASS_7.3-51.4       rsconnect_0.8.15    assertthat_0.2.1    stringi_1.4.3      
[49] crayon_1.3.4  
```