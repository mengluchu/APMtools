if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_cran(c("devtools", "roxygen2", "testthat", "spelling", "usethis", "pkgdown"))

usethis::create_package("APMtools")

library(devtools)
document()
devtools::load_all()
devtools::build()
devtools::check()
library(APMtools)
retrieve_predictor
#library(globalLUR)
#data("merged")
#data("countrywithppm")
devtools::use_data(countrywithppm )

install_github("mengluchu/APMtools")
