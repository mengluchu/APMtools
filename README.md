# APMtools

R package providing tools to streamline and facilitate air pollution mapping using statistical methods. The package also includes a global dataset. 


### Installing
``` r
install_github("mengluchu/APMtools")
```

### Data
The APMtools include a dataset for global annual NO2 concentration, and the predictors we calculated at each station location.
The variables are: 

Measurements:

* value_mean: annual mean $NO_2$ ($mg/m^3$). 

Predictors (all together 90):
* road_class_XX_size: road lenght within a buffer with radius "size" of type XX. ROAD_1: highway, ROAD_2: primary, ROAD_3: secondary, ROAD_4: tertiary, ROAD_5: unpaved   
* industry_size: Industrial area within a buffer with radius "size".   
* trop_mean: TROPOMI averaged over Feb 2018 - Jan 2019.    
* temperature_2m_m: monthly mean temperature at 2m height of month "m".  
* wind_speed_10m_m:monthly mean wind speed at 2m height of month "m".  
* poppulation_1000/ 3000 /5000: population 1, 3, 5 km resolution.  
* Rsp: Surface remote sensing and chemical transport model product (only for 2012).  
* OMI_mean_filt: OMI column density, 2017 annual average.    
* nightlight_size: nightlight VIIRS data in original resolution (500 m) and various buffer sizes. 

Other:

* country: 2-digit country code

``` r
library(APMtools) 
data("global_annual")
names(global_annual)
#>  [1] "long"              "lat"               "nightlight_800"   
#>  [4] "nightlight_500"    "nightlight_5000"   "nightlight_3000"  
#>  [7] "nightlight_1000"   "elevation"         "industry_1000"    
#> [10] "industry_100"      "industry_25"       "industry_3000"    
#> [13] "industry_300"      "industry_5000"     "industry_500"     
#> [16] "industry_50"       "industry_800"      "OMI_mean_filt"    
#> [19] "population_1000"   "population_3000"   "population_5000"  
#> [22] "road_class_1_1000" "road_class_1_100"  "road_class_1_25"  
#> [25] "road_class_1_3000" "road_class_1_300"  "road_class_1_5000"
#> [28] "road_class_1_500"  "road_class_1_50"   "road_class_1_800" 
#> [31] "road_class_2_1000" "road_class_2_100"  "road_class_2_25"  
#> [34] "road_class_2_3000" "road_class_2_300"  "road_class_2_5000"
#> [37] "road_class_2_500"  "road_class_2_50"   "road_class_2_800" 
#> [40] "road_class_3_1000" "road_class_3_100"  "road_class_3_25"  
#> [43] "road_class_3_3000" "road_class_3_300"  "road_class_3_5000"
#> [46] "road_class_3_500"  "road_class_3_50"   "road_class_3_800" 
#> [49] "road_class_4_1000" "road_class_4_100"  "road_class_4_25"  
#> [52] "road_class_4_3000" "road_class_4_300"  "road_class_4_5000"
#> [55] "road_class_4_500"  "road_class_4_50"   "road_class_4_800" 
#> [58] "road_class_5_1000" "road_class_5_100"  "road_class_5_25"  
#> [61] "road_class_5_3000" "road_class_5_300"  "road_class_5_5000"
#> [64] "road_class_5_500"  "road_class_5_50"   "road_class_5_800" 
#> [67] "Rsp"               "temperature_2m_10" "temperature_2m_11"
#> [70] "temperature_2m_12" "temperature_2m_1"  "temperature_2m_2" 
#> [73] "temperature_2m_3"  "temperature_2m_4"  "temperature_2m_5" 
#> [76] "temperature_2m_6"  "temperature_2m_7"  "temperature_2m_8" 
#> [79] "temperature_2m_9"  "trop_mean_filt"    "wind_speed_10m_10"
#> [82] "wind_speed_10m_11" "wind_speed_10m_12" "wind_speed_10m_1" 
#> [85] "wind_speed_10m_2"  "wind_speed_10m_3"  "wind_speed_10m_4" 
#> [88] "wind_speed_10m_5"  "wind_speed_10m_6"  "wind_speed_10m_7" 
#> [91] "wind_speed_10m_8"  "wind_speed_10m_9"  "value_mean"       
#> [94] "country"
```
 
### Functions

The objects included in this package are:
``` r
library(APMtools)
ls("package:APMtools")
#>  [1] "Brt_imp"              "Brt_LUR"              "Brt_pre"             
#>  [4] "cforest_LUR"          "countrywithppm"       "create_ring"         
#>  [7] "ctree_LUR"            "DENL_new"             "error_matrix"        
#> [10] "join_by_id"           "Lasso"                "Lasso_pre"           
#> [13] "Lassoselected"        "mechanical"           "merge_roads"         
#> [16] "merged"               "merged_nightlight"    "mergeraster2file"    
#> [19] "plot_error"           "plot_rsq"             "ppm2ug"              
#> [22] "predicLA_RF_XGBtiles" "RDring_coef"          "removedips"          
#> [25] "retrieve_predictor"   "rf_imp"               "rf_LUR"              
#> [28] "rf_pre"               "sampledf"             "scatterplot"         
#> [31] "subset_grep"          "univar_rsq"           "xgb_pre"             
#> [34] "xgboost_imp"          "xgboost_LUR"
```
 
Brt_xx, xgboost_xx, ctree_xx, Lasso_xx, and rf_xx are gives important variables (method_imp), predictions(method_pre), and error matrices (method_LUR). 
For example, boostrapping variable importance calculation using the xgboost_imp.

``` r 
Impor_val =  function(n,df, method , y_var, varstring  ) {
  set.seed(n) # for reproducing results.
  smp_size <- floor(0.8 * nrow(df))
  training<- sample(seq_len(nrow(df)), size = smp_size) 
  test = seq_len(nrow(df))[-training] # hold-out dataset. It is called "test" to be conistent with the validation process.
  
  methodID = switch(method,  "xboost"=1,"rf" =2, "gb"=3) 
  df = switch(methodID,  
              xgboost_imp (variabledf= df, y_varname= y_var, max_depth =3, gamma=1, eta =0.05, nthread = 4, nrounds = 636, training=training, test=test, grepstring =varstring ),  
              rf_imp(df, y_varname= y_var, training=training, test=test, grepstring =varstring,mtry = 24, numtrees = 1000),
              Brt_imp (df, opti = F, ntree= 1000, y_varname= y_var, training=training, test=test,  grepstring =varstring)) 
  return(df)  
} 
varstring = "road|nightlight|population|temp|wind|trop|indu|elev"
Vxb = data.frame(lapply(1:3, Impor_val, df=global_annual, "xboost" , y_var ="value_mean", varstring = varstring))
apply(Vxb, 1, median) # you can of course sort and visualize.

#>         elevation      industry_100     industry_1000       industry_25 
#>      5.076441e-03      1.354538e-04      3.265738e-03      0.000000e+00 
#>      industry_300     industry_3000       industry_50      industry_500 
#>      5.045334e-04      3.116757e-03      3.988246e-05      4.358613e-04 
#>     industry_5000      industry_800   nightlight_1000   nightlight_3000 
#>      4.536942e-03      1.062431e-03      1.240915e-03      2.638790e-03 
#>    nightlight_500   nightlight_5000    nightlight_800   population_1000 
#>      7.053277e-03      2.410676e-03      2.925039e-03      6.213645e-02 
#>   population_3000   population_5000  road_class_1_100 road_class_1_1000 
#>      7.423998e-02      1.453697e-01      7.090040e-03      2.201510e-03 
#>   road_class_1_25  road_class_1_300 road_class_1_3000   road_class_1_50 
#>      9.638529e-03      8.506897e-03      7.489224e-03      9.708558e-03 
#>  road_class_1_500 road_class_1_5000  road_class_1_800  road_class_2_100 
#>      7.895989e-03      5.317113e-03      1.902116e-03      1.316581e-02 
#> road_class_2_1000   road_class_2_25  road_class_2_300 road_class_2_3000 
#>      2.353575e-03      1.609025e-02      3.871651e-03      8.524231e-03 
#>   road_class_2_50  road_class_2_500 road_class_2_5000  road_class_2_800 
#>      1.887654e-02      2.640841e-03      6.262479e-03      2.230385e-03 
#>  road_class_3_100 road_class_3_1000   road_class_3_25  road_class_3_300 
#>      4.396174e-03      7.165572e-03      1.000826e-02      4.530140e-03 
#> road_class_3_3000   road_class_3_50  road_class_3_500 road_class_3_5000 
#>      5.725236e-03      3.860313e-03      1.354494e-03      3.684797e-03 
#>  road_class_3_800  road_class_4_100 road_class_4_1000   road_class_4_25 
#>      1.506445e-03      2.638703e-03      1.414082e-03      4.179137e-03 
#>  road_class_4_300 road_class_4_3000   road_class_4_50  road_class_4_500 
#>      2.552942e-03      2.537398e-03      1.944496e-03      2.188836e-03 
#> road_class_4_5000  road_class_4_800  road_class_5_100 road_class_5_1000 
#>      1.936854e-03      3.315679e-03      5.628222e-03      6.044179e-03 
#>   road_class_5_25  road_class_5_300 road_class_5_3000   road_class_5_50 
#>      2.430375e-03      5.681138e-03      7.620955e-03      3.185166e-03 
#>  road_class_5_500 road_class_5_5000  road_class_5_800  temperature_2m_1 
#>      5.337202e-03      8.310121e-03      2.518276e-03      6.770100e-03 
#> temperature_2m_10 temperature_2m_11 temperature_2m_12  temperature_2m_2 
#>      8.985068e-03      2.278889e-03      3.022410e-03      3.891842e-03 
#>  temperature_2m_3  temperature_2m_4  temperature_2m_5  temperature_2m_6 
#>      3.105518e-03      3.476173e-03      8.294943e-03      3.464240e-03 
#>  temperature_2m_7  temperature_2m_8  temperature_2m_9    trop_mean_filt 
#>      8.457528e-03      3.125743e-03      3.382316e-03      2.941242e-01 
#>  wind_speed_10m_1 wind_speed_10m_10 wind_speed_10m_11 wind_speed_10m_12 
#>      4.889684e-03      1.748170e-03      2.876713e-03      3.312144e-03 
#>  wind_speed_10m_2  wind_speed_10m_3  wind_speed_10m_4  wind_speed_10m_5 
#>      7.482134e-03      1.565142e-02      4.059565e-03      4.412742e-03 
#>  wind_speed_10m_6  wind_speed_10m_7  wind_speed_10m_8  wind_speed_10m_9 
#>      3.280105e-03      1.591655e-03      1.634610e-03      1.584987e-03
```

And cross-validation using rf_LUR, xgboost_LUR, Lasso_LUR

```r
crossvali =  function(n,df, y_var) {
  smp_size <- floor(0.8 * nrow(df)) 
  set.seed(n)
  training<- sample(seq_len(nrow(df)), size = smp_size)
  test = seq_len(nrow(df))[-training] 
  
  P_rf = rf_LUR(df, numtrees =  1000, mtry = 14, vis1 = F,y_varname= y_var, training=training, test=test, grepstring =varstring)
  P_xgb= xgboost_LUR(df, max_depth =3, gamma=1, eta =0.05, nthread = 4, nrounds = 200, y_varname= y_var,training=training, test=test, grepstring =varstring)
  P_Lasso =  Lasso(df,alpha =1 , vis1  = F,y_varname = y_var,training=training, test=test,grepstring =prestring )  
  V = cbind(P_rf, P_xgb, P_Lasso)
} 
lapply(1:20, df = merged, y_var = y_var,crossvali) # 20-times bootstrapped cross-validation 
```
 
