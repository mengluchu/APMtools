% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve_predictor.R
\name{retrieve_predictor}
\alias{retrieve_predictor}
\title{read predictor values at the point locations, returns a dataframe or save into a csv}
\usage{
retrieve_predictor(
  predictorstack,
  pointdatafile,
  coordslonlat,
  proj,
  csvname = "predictors"
)
}
\arguments{
\item{predictorstack}{raster stack to read point values from}

\item{pointdatafile}{the file contains point locations to be read}

\item{coordslonlat}{a vector specify the names of the longitude and latitude variable names.}

\item{proj}{the projection of the point file}

\item{csvname}{the name to save the csv file}
}
\description{
read predictor values at the point locations, returns a dataframe or save into a csv
}
\examples{
\donttest{
lus = raster("/data/lu01/NWA/predictor/NLstack.grd")
lf_lo = list.files("/data/lu01/NWA/Bakfietsdata", pattern = "^.*morning.*.csv$", full.names = T)
bakfile1 = read.csv(lf_lo[1])
proj = "+proj=longlat +datum=WGS84"
retrieve_predictor(lus, bakfile1, c("Lon", "Lat"), proj)}
}
