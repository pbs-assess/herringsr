library(tidyverse)
library(csasdown)
library(gfutilities)
library(gfiscamutils)
library(coda)
here <- here::here

source(here("R/models.R"))

models_dir <- here("models")
retros_dir <- here("retrospectives")
confidence_vals <- c(0.05, 0.95)
## Fixed cutoffs for decision tables, corresponsing to the stock order
## HG, PRD, CC, SOG, WCVI
fixed_cutoffs <- c(10.7, 12.1, 17.6, 21.2, 18.8)
assess_yr <- as.numeric(substr(Sys.Date(), 1, 4))
last_assess_yr <- assess_yr - 1
forecast_yr <- assess_yr + 1
start_yr <- 1951
start_yr_age_comps <- 1951
end_yr <- 2016
last_data_yr <- 2016
this_season <- paste(assess_yr - 1, assess_yr, sep = "/")

## Directories and names of stocks
stock_dir <- list("HG", "PRD", "CC", "SOG", "WCVI")
base_model_dir_name <- lapply(1:length(stock_dir),
                              function(x){
                                file.path(models_dir, stock_dir[[x]], "AM2")})
stock_name <- list("Haida Gwaii", "Pr. Rupert", "Central Coast", "SOG", "WCVI")
base_model_name <- lapply(1:length(stock_name),
                          function(x){
                            paste("Reference model", stock_name[[x]])})

#' Create RData files for models found in the directories given by `dirs`
#'
#' @param dirs List of directories to nuild Rdata fies for
#' @param ovwrt Overwrite Rdata file if it exists? Logical
#' @param burnin integer burnin value for MCMC calculations
#' @param thin integer thinning interval value for MCMC calculations
#' @param conf_vals Confidence interval values. Vector of length 2.
#' @param ld_proj Load projections? Logical
#' @param fixed_cutoffs. vector of fixed cutoff values for Herring areas
#' @importFrom gfiscamutils create.rdata.file
build <- function(dirs = base_model_dir_name,
                  ovwrt = FALSE,
                  burnin = 1000,
                  thin = 1,
                  conf_vals = confidence_vals,
                  fixed_cutoffs. = fixed_cutoffs,
                  ld_proj = TRUE){

  invisible(lapply(1:length(dirs),
                   function(x){
                     bm <- dirs[[x]]
                     if(length(grep("HG", bm))){
                       which_stock <- 1
                     }else if(length(grep("PRD", bm))){
                       which_stock <- 2
                     }else if(length(grep("CC", bm))){
                       which_stock <- 3
                     }else if(length(grep("SOG", bm))){
                       which_stock <- 4
                     }else if(length(grep("WCVI", bm))){
                       which_stock <- 5
                     }else{
                       which_stock <- 0
                     }
                     create.rdata.file(bm,
                                       mcmc.subdir = "mcmc",
                                       ovwrt.rdata = ovwrt,
                                       load.proj = ld_proj,
                                       lower = conf_vals[1],
                                       upper = conf_vals[2],
                                       burnin = burnin,
                                       thin = thin,
                                       which.stock = which_stock,
                                       which.model = 2,
                                       fixed.cutoffs = fixed_cutoffs.)
                     }
                   )
            )

}

#' Must be called from within the first knitr code chunk
#' in the document. It is defined here so that it is in the same place
#' as the other model setup.
#'
#' @param dirs List of directories shich hold Rdata files build using [build()] function
#' @importFrom gfiscamutils load.models
load_models_into_parent_env <- function(dirs = base_model_dir_name){
  base_models <<- lapply(dirs,
                         function(x){
                           load.models(x)})
}
