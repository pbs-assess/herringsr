library(csasdown)
library(gfutilities)
library(gfiscamutils)
library(coda)
library(knitr)
library(dplyr)
library(lubridate)
library(PBSmodelling)
library(xtable)
library(tidyverse)
library(RColorBrewer)
library(here)
library(zoo)
library(Hmisc)
library(scales)
library(sp)
library(cowplot)
library(maptools)
library(rgdal)
library(rgeos)
library(raster)
library(grid)
library(colorRamps)
library(stringr)
library(data.table)
library(ggrepel)
library(viridis)
here <- here::here

source(here("R/models.R"))

models_dir <- here("../Herring/Assessments/2019")
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
stock_dir <- list()
stock_name <- list()
stock_dir[[1]] <- "HG"
stock_name[[1]] <- "Haida Gwaii"
stock_dir[[2]] <- "PRD"
stock_name[[2]] <- "Pr_ Rupert"
stock_dir[[3]] <- "CC"
stock_name[[3]] <- "Central Coast"
stock_dir[[4]] <- "SOG"
stock_name[[4]] <- "SOG"
stock_dir[[5]] <- "WCVI"
stock_name[[5]] <- "WCVI"
base_model_name <- lapply(1:length(stock_name),
                          function(x){
                            paste("Reference model", stock_name[[x]])})

base_model_dir_name <- lapply(1:length(stock_dir),
                              function(x){
                                file.path(models_dir, stock_dir[[x]], "AM2")})


## Sensitivity models group 1
## This is a list of the AM1 models, one for each stock
## -----------------------------------------------------------------------------
# sens_model_dir_name_1 <- lapply(1:length(stock_dir),
#                                 function(x){
#                                   file.path(stock_dir[[x]], "AM1")})
#
# sens_model_name_1 <- "AM1"
#
# lapply(1:length(sens_model_dir_name_1),
#        function(x){
#          verify_models(model_dir,
#                        sens_model_dir_name_1[[x]],
#                        sens_model_name_1)})

## -----------------------------------------------------------------------------
## Sensitivity models group 2
## This is a list of the constant natural mortality sensitivities for AM1
## -----------------------------------------------------------------------------
## Redefine stock_dir so that sensitivities can be loaded
# stock_dir <- list()
# stock_dir[[1]] <- "HG-natural-mortality"
# stock_dir[[2]] <- "PRD-natural-mortality"
# stock_dir[[3]] <- "CC-natural-mortality"
# stock_dir[[4]] <- "SOG-natural-mortality"
# stock_dir[[5]] <- "WCVI-natural-mortality"
# sens_model_dir_name_2 <- lapply(1:length(stock_dir),
#                                 function(x){
#                                   file.path(stock_dir[[x]], "AM1_constantM")})
#
# sens_model_name_2 <- "AM1 constant M"
#
# lapply(1:length(sens_model_dir_name_2),
#        function(x){
#          verify_models(model_dir,
#                        sens_model_dir_name_2[[x]],
#                        sens_model_name_2)})
#
# if(verbose){
#   lapply(1:length(sens_model_dir_name_2),
#          function(x){
#            print_model_message(sens_model_dir_name_2[[x]],
#                                sens_model_name_2,
#                                2,
#                                model_type = "Sensitivity")})
# }

## -----------------------------------------------------------------------------
## Sensitivity models group 3
## This is a list of the time varying natural mortality sensitivities for AM1
## -----------------------------------------------------------------------------
# sens_model_dir_name_3 <- lapply(1:length(stock_dir),
#                                 function(x){
#                                   file.path(stock_dir[[x]], "AM1_timevaryingM")})
#
# sens_model_name_3 <- "AM1 time-varying M"
#
# lapply(1:length(sens_model_dir_name_3),
#        function(x){
#          verify_models(model_dir,
#                        sens_model_dir_name_3[[x]],
#                        sens_model_name_3)})
#
# if(verbose){
#   lapply(1:length(sens_model_dir_name_3),
#          function(x){
#            print_model_message(sens_model_dir_name_3[[x]],
#                                sens_model_name_3,
#                                3,
#                                model_type = "Sensitivity")})
# }

## -----------------------------------------------------------------------------
## Sensitivity models group 4
## This is a list of the constant natural mortality sensitivities for AM2
## -----------------------------------------------------------------------------
# sens_model_dir_name_4 <- lapply(1:length(stock_dir),
#                                 function(x){
#                                   file.path(stock_dir[[x]], "AM2_constantM")})
#
# sens_model_name_4 <- "AM2 constant M"
#
# lapply(1:length(sens_model_dir_name_4),
#        function(x){
#          verify_models(model_dir,
#                        sens_model_dir_name_4[[x]],
#                        sens_model_name_4)})
#
# if(verbose){
#   lapply(1:length(sens_model_dir_name_4),
#          function(x){
#            print_model_message(sens_model_dir_name_4[[x]],
#                                sens_model_name_4,
#                                4,
#                                model_type = "Sensitivity")})
# }

## -----------------------------------------------------------------------------
## Sensitivity models group 5
## This is a list of the time varying natural mortality sensitivities for AM2
## -----------------------------------------------------------------------------
# sens_model_dir_name_5 <- lapply(1:length(stock_dir),
#                                 function(x){
#                                   file.path(stock_dir[[x]], "AM2_timevaryingM")})
#
# sens_model_name_5 <- "AM2 time-varying M"
#
# lapply(1:length(sens_model_dir_name_5),
#        function(x){
#          verify_models(model_dir,
#                        sens_model_dir_name_5[[x]],
#                        sens_model_name_5)})
#
# if(verbose){
#   lapply(1:length(sens_model_dir_name_5),
#          function(x){
#            print_model_message(sens_model_dir_name_5[[x]],
#                                sens_model_name_5,
#                                5,
#                                model_type = "Sensitivity")})
# }

## -----------------------------------------------------------------------------
## Sensitivity models group 6
## This is a list of the q priors "a" sensitivities for AM1
## -----------------------------------------------------------------------------
## Redefine stock_dir so that sensitivities can be loaded
# stock_dir <- list()
# stock_dir[[1]] <- "HG-q-priors"
# stock_dir[[2]] <- "PRD-q-priors"
# stock_dir[[3]] <- "CC-q-priors"
# stock_dir[[4]] <- "SOG-q-priors"
# stock_dir[[5]] <- "WCVI-q-priors"
# sens_model_dir_name_6 <- lapply(1:length(stock_dir),
#                                 function(x){
#                                   file.path(stock_dir[[x]], "AM1qa")})
#
# sens_model_name_6 <- "AM1 q-a"
#
# lapply(1:length(sens_model_dir_name_6),
#        function(x){
#          verify_models(model_dir,
#                        sens_model_dir_name_6[[x]],
#                        sens_model_name_6)})
#
# if(verbose){
#   lapply(1:length(sens_model_dir_name_6),
#          function(x){
#            print_model_message(sens_model_dir_name_6[[x]],
#                                sens_model_name_6,
#                                6,
#                                model_type = "Sensitivity")})
# }

## -----------------------------------------------------------------------------
## Sensitivity models group 7
## This is a list of the q priors "b" sensitivities for AM1
## -----------------------------------------------------------------------------
# sens_model_dir_name_7 <- lapply(1:length(stock_dir),
#                                 function(x){
#                                   file.path(stock_dir[[x]], "AM1qb")})
#
# sens_model_name_7 <- "AM1 q-b"
#
# lapply(1:length(sens_model_dir_name_7),
#        function(x){
#          verify_models(model_dir,
#                        sens_model_dir_name_7[[x]],
#                        sens_model_name_7)})
#
# if(verbose){
#   lapply(1:length(sens_model_dir_name_7),
#          function(x){
#            print_model_message(sens_model_dir_name_7[[x]],
#                                sens_model_name_7,
#                                7,
#                                model_type = "Sensitivity")})
# }

## -----------------------------------------------------------------------------
## Sensitivity models group 8
## This is a list of the q priors "c" sensitivities for AM1
## -----------------------------------------------------------------------------
# sens_model_dir_name_8 <- lapply(1:length(stock_dir),
#                                 function(x){
#                                   file.path(stock_dir[[x]], "AM1qc")})
#
# sens_model_name_8 <- "AM1 q-c"
#
# lapply(1:length(sens_model_dir_name_8),
#        function(x){
#          verify_models(model_dir,
#                        sens_model_dir_name_8[[x]],
#                        sens_model_name_8)})
#
# if(verbose){
#   lapply(1:length(sens_model_dir_name_8),
#          function(x){
#            print_model_message(sens_model_dir_name_8[[x]],
#                                sens_model_name_8,
#                                8,
#                                model_type = "Sensitivity")})
# }

## -----------------------------------------------------------------------------
## Retrospectives
## -----------------------------------------------------------------------------
# RETRO_STRING <- c(paste0("0", 1:9), 10:15)
# stock_dir[[1]] <- "HG"
# stock_dir[[2]] <- "PRD"
# stock_dir[[3]] <- "CC"
# stock_dir[[4]] <- "SOG"
# stock_dir[[5]] <- "WCVI"
# retro_names_am1 <- list()
# retro_names_am2 <- list()
# retro_yr <- list()
# for(i in 1:length(stock_dir)){
#   retro_names_am1[[i]] <- list()
#   retro_names_am2[[i]] <- list()
#   for(j in 1:length(RETRO_STRING)){
#     if(stock_dir[i] == "HG"){
#       which_stock <- 1
#     }else if(stock_dir[i] == "PRD"){
#       which_stock <- 2
#     }else if(stock_dir[i] == "CC"){
#       which_stock <- 3
#     }else if(stock_dir[i] == "SOG"){
#       which_stock <- 4
#     }else if(stock_dir[i] == "WCVI"){
#       which_stock <- 5
#     }else{
#       stop("The retrospectives directory does not contain the known stock names_")
#     }
#     ## Create lists of the names of the retrospectives for AM1 and AM2
#     retro_names_am1[[i]][[j]] <- file.path(stock_dir[i],
#                                            RETRO_STRING[j],
#                                            stock_dir[i],
#                                            "AM1")
#     retro_names_am2[[i]][[j]] <- file.path(stock_dir[i],
#                                            RETRO_STRING[j],
#                                            stock_dir[i],
#                                            "AM2")
#   }
# }

## -----------------------------------------------------------------------------
## Vector of directory names for all models referenced above
## -----------------------------------------------------------------------------
## ALL models must be in this list!
## Each model directory listed here will have an RData file in it,
##  or one will be created depending on what is found in the directory_
##  i_e_ mcmc, retrospective, or forecast directories_
model_dir_names <- c(base_model_dir_name)#,
# unlist(sens_model_dir_name_1),
# unlist(sens_model_dir_name_2),
# unlist(sens_model_dir_name_3),
# unlist(sens_model_dir_name_4),
# unlist(sens_model_dir_name_5),
# unlist(sens_model_dir_name_6),
# unlist(sens_model_dir_name_7),
# unlist(sens_model_dir_name_8))

## This function must be called from within the first knitr code chunk
## in the document_ It is defined here so that it is in the same place
## as the other model setup and should be changed if bridge models
## and sensitivity models change in the model_dir_names above__
load_models_into_parent_env <- function(){
  base_models <<- lapply(base_model_dir_name,
                         function(x){
                           load_models(x)})
  # sens_models_1 <<- lapply(sens_model_dir_name_1,
  #                          function(x){
  #                            load_models(model_dir, x)})
  # sens_models_2 <<- lapply(sens_model_dir_name_2,
  #                          function(x){
  #                            load_models(model_dir, x)})
  # sens_models_3 <<- lapply(sens_model_dir_name_3,
  #                          function(x){
  #                            load_models(model_dir, x)})
  # sens_models_4 <<- lapply(sens_model_dir_name_4,
  #                          function(x){
  #                            load_models(model_dir, x)})
  # sens_models_5 <<- lapply(sens_model_dir_name_5,
  #                          function(x){
  #                            load_models(model_dir, x)})
  # sens_models_6 <<- lapply(sens_model_dir_name_6,
  #                          function(x){
  #                            load_models(model_dir, x)})
  # sens_models_7 <<- lapply(sens_model_dir_name_7,
  #                          function(x){
  #                            load_models(model_dir, x)})
  # sens_models_8 <<- lapply(sens_model_dir_name_8,
  #                          function(x){
  #                            load_models(model_dir, x)})

  # base_retro_models <<- lapply(retro_names_am2,
  #                              function(x){
  #                                lapply(x,
  #                                       function(y){
  #                                         tmp <- load_models(retro_dir, y)
  #                                       })})

  # am1_retro_models <<- lapply(retro_names_am1,
  #                             function(x){
  #                               lapply(x,
  #                                      function(y){
  #                                        tmp <- load_models(retro_dir, y)
  #                                      })})

  ## Remove NULL entries in retrospective lists_ This allows for
  ##  each stock to have a different length retrospective period
  ## A helper function that tests whether an object is either NULL
  ##  or a list of NULLs
  is_null_elem <- function(x){
    is_null(x) | all(sapply(x, is_null))
  }

  ## Recursively step down into list, removing all such objects
  remove_null_elems <- function(x){
    x <- Filter(Negate(is_null_elem), x)
    lapply(x,
           function(x){
             if(is_list(x)){
               remove_null_elems(x)
             }else{
               x
             }})
  }

  # base_retro_models <<- remove_null_elems(base_retro_models)
  # am1_retro_models <<- remove_null_elems(am1_retro_models)

}

build <- function(ovwrt_base = FALSE,
                  ovwrt_sens = FALSE,
                  ovwrt_retro = FALSE,
                  burnin = 1000,
                  thin = 1){
  ## Once the model setup has been verified, this function will create the
  ##  corresponding RData files_ Each model defined in the models-setup_r
  ##  file will have its own RData file holding the model object as defined
  ##  in the Readme_md file_
  ##
  ## ovwrt_base - overwrite the RData file for the base model?
  ## ovwrt_sens - overwrite the RData files for the sensitivity models?
  ## ovwrt_retro - overwrite the RData files for the retrospective runs?

  ## Base models
  invisible(lapply(1:length(base_model_dir_name),
                   function(x){
                     ## Determine which stock is being loaded
                     bm <- base_model_dir_name[[x]]
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
                     create_rdata_file(bm,
                                       ovwrt_rdata = ovwrt_base,
                                       load_proj = TRUE,
                                       low = confidence_vals[1],
                                       high = confidence_vals[2],
                                       burnin = burnin,
                                       thin = thin,
                                       which_stock = which_stock,
                                       which_model = 2,
                                       fixed_cutoffs = fixed_cutoffs,
                                       verbose = ss_verbose)}))

  ## Sensitivity models need to be unlisted from their groups
  ##  and placed into a single list for the lapply below to work right
  # sens_model_names_list <- c(unlist(sens_model_dir_name_1),
  #                            unlist(sens_model_dir_name_2),
  #                            unlist(sens_model_dir_name_3),
  #                            unlist(sens_model_dir_name_4),
  #                            unlist(sens_model_dir_name_5),
  #                            unlist(sens_model_dir_name_6),
  #                            unlist(sens_model_dir_name_7),
  #                            unlist(sens_model_dir_name_8))

  ## Sensitivity models
  # invisible(lapply(1:length(sens_model_names_list),
  #                  function(x){
  #                    ## Determine which stock is being loaded
  #                    sm <- sens_model_names_list[[x]]
  #                    if(length(grep("HG", sm))){
  #                      which_stock <- 1
  #                    }else if(length(grep("PRD", sm))){
  #                      which_stock <- 2
  #                    }else if(length(grep("CC", sm))){
  #                      which_stock <- 3
  #                    }else if(length(grep("SOG", sm))){
  #                      which_stock <- 4
  #                    }else if(length(grep("WCVI", sm))){
  #                      which_stock <- 5
  #                    }else{
  #                      stop("The directory ", sm, " does not contain the known stock names_")
  #                    }
  #                    if(length(grep("AM1", sm))){
  #                      which_model = 1
  #                    }else if(length(grep("AM2", sm))){
  #                      which_model = 2
  #                    }else{
  #                      stop("The directory ", sm, " does not contain the known model names AM1 or AM2_")
  #                    }
  #                    create_rdata_file(
  #                      model_name = sm,
  #                      ovwrt_rdata = ovwrt_sens,
  #                      load_proj = TRUE,
  #                      low = confidence_vals[1],
  #                      high = confidence_vals[2],
  #                      burnin = burnin,
  #                      thin = thin,
  #                      which_stock = which_stock,
  #                      which_model = which_model,
  #                      fixed_cutoffs = fixed_cutoffs,
  #                      verbose = ss_verbose)}))

  ## Retrospective models
  # stock_dir[[1]] <- "HG"
  # stock_dir[[2]] <- "PRD"
  # stock_dir[[3]] <- "CC"
  # stock_dir[[4]] <- "SOG"
  # stock_dir[[5]] <- "WCVI"
  # retro_dirs_am1 <- list()
  # retro_dirs_am2 <- list()
  # retro_yr <- list()
  # for(i in 1:length(stock_dir)){
  #   retro_dirs_am1[[i]] <- list()
  #   retro_dirs_am2[[i]] <- list()
  #   for(j in 1:length(RETRO_STRING)){
  #     if(stock_dir[i] == "HG"){
  #       which_stock <- 1
  #     }else if(stock_dir[i] == "PRD"){
  #       which_stock <- 2
  #     }else if(stock_dir[i] == "CC"){
  #       which_stock <- 3
  #     }else if(stock_dir[i] == "SOG"){
  #       which_stock <- 4
  #     }else if(stock_dir[i] == "WCVI"){
  #       which_stock <- 5
  #     }else{
  #       stop("The retrospectives directory does not contain the known stock names_")
  #     }
  #     create_rdata_file_retro(
  #       model_dir = file.path(retro_dir,
  #                             stock_dir[i],
  #                             RETRO_STRING[j],
  #                             stock_dir[i],
  #                             "AM1"),
  #       ovwrt_rdata = ovwrt_retro,
  #       load_proj = TRUE,
  #       low = confidence_vals[1],
  #       high = confidence_vals[2],
  #       burnin = burnin,
  #       thin = thin,
  #       which_stock = which_stock,
  #       which_model = 1)
  #     create_rdata_file_retro(
  #       model_dir = file.path(retro_dir,
  #                             stock_dir[i],
  #                             RETRO_STRING[j],
  #                             stock_dir[i],
  #                             "AM2"),
  #       ovwrt_rdata = ovwrt_retro,
  #       load_proj = TRUE,
  #       low = confidence_vals[1],
  #       high = confidence_vals[2],
  #       burnin = burnin,
  #       thin = thin,
  #       which_stock = which_stock,
  #       which_model = 2)
  #   }
  # }
}
