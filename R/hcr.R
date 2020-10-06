#' Get the posteriors for the sbt and sbo for the given region
#'
#' @param name the area (SAR region) to return values for
#'
#' @return the posteriors for the sbt and sbo for the given region
get_sbt_sbo <- function(name){
  model_ind <- match(en2fr(name, french), major_regions_short)
  b <- major_models[[model_ind]]
  sbt <- as.list(as.data.frame(t(b$mcmccalcs$sbt.dat)))
  sbo <- b$mcmccalcs$p.dat$sbo
  list(sbt, sbo)
}

#' Retrieve MP file with decision table data and perform HCR calculations on the posteriors
#'
#' @param name name of the SAR Region
#' @param fn file which contains the MP data for the given region
#'
#' @return a list of two elements:
#' 1. A list with the same number of elements as there are rows in the table read in from `fn`. Each of these is a list of
#'  posteriors of length 2, tac and hr (harvest rate)
#' 2. [tibble::tibble()] holding the data from the file plus two columns, tac and hr (harvest rate)
#'  which are the median tac and harverst rate calculated for the region by MP.
get_hcr <- function(sbt, sbo, fn){
  mp <- read_csv(fn, col_types=cols()) %>%
    as_tibble() %>%
    arrange(factor(om, levels = c("DDM", "DIM", "conM")),
            desc(obj1),
            desc(obj2)) %>%
    mutate(tac = NA,
           targ.hr = NA)
  mp.lst <- NULL
  for(rw in seq_len(nrow(mp))){
    mp.lst[[rw]] <- hcr(sbt, sbo, row = mp[rw,])
    hcr_meds <- get_hcr_tac_hr(mp.lst[[rw]])
    mp[rw,]$tac <- hcr_meds[1]
    mp[rw,]$targ.hr <- hcr_meds[2]
  }
  list(mp.lst,
  mp %>% select(om,
                label,
                obj1,
                obj2,
                obj3,
                obj4,
                catch,
                tac,
                targ.hr))
}
#' Retrieve MP file with decision table data and perform HCR calculations on
#' the posteriors, with 3 additional Biomass columns
#'
#' @param name name of the SAR Region
#' @param fn file which contains the MP data for the given region
#'
#' @return a list of two elements:
#' 1. A list with the same number of elements as there are rows in the table read in from `fn`. Each of these is a list of
#'  posteriors of length 2, tac and hr (harvest rate)
#' 2. [tibble::tibble()] holding the data from the file plus two columns, tac and hr (harvest rate)
#'  which are the median tac and harverst rate calculated for the region by MP.
get_hcr_add <- function(sbt, sbo, fn){
  mp <- read_csv(fn, col_types=cols()) %>%
    as_tibble() %>%
    arrange(factor(om, levels = c("DDM", "DIM", "conM")),
            desc(obj1),
            desc(obj2)) %>%
    mutate(tac = NA,
           targ.hr = NA)
  mp.lst <- NULL
  for(rw in seq_len(nrow(mp))){
    mp.lst[[rw]] <- hcr(sbt, sbo, row = mp[rw,])
    hcr_meds <- get_hcr_tac_hr(mp.lst[[rw]])
    mp[rw,]$tac <- hcr_meds[1]
    mp[rw,]$targ.hr <- hcr_meds[2]
  }
  list(mp.lst,
       mp %>% select(om,
                     label,
                     obj1,
                     obj2,
                     obj2b,
                     obj2c,
                     obj2d,
                     obj3,
                     obj4,
                     catch,
                     tac,
                     targ.hr))
}

sbt_sbo.hg <- get_sbt_sbo("HG")
sbt.hg <- sbt_sbo.hg[[1]]
sbo.hg <- sbt_sbo.hg[[2]]
rel.sbo.hg <- rep(1, length(sbo.hg))

sbt_sbo.cc <- get_sbt_sbo("CC")
sbt.cc <- sbt_sbo.cc[[1]]
sbo.cc <- sbt_sbo.cc[[2]]
rel.sbo.cc <- rep(1, length(sbo.cc))
mp.lst.cc <- get_hcr(sbt.cc,
                     sbo.cc,
                     fn = here("data/mp-cc.csv"))
mp.vals.cc <- mp.lst.cc[[1]]
mp.cc <- mp.lst.cc[[2]]

sbt_sbo.prd <- get_sbt_sbo("PRD")
sbt.prd <- sbt_sbo.prd[[1]]
sbo.prd <- sbt_sbo.prd[[2]]
rel.sbo.prd <- rep(1, length(sbo.prd))
mp.lst.prd <- get_hcr(sbt.prd,
                      sbo.prd,
                      fn = here("data/mp-prd.csv"))
mp.vals.prd <- mp.lst.prd[[1]]
mp.prd <- mp.lst.prd[[2]]

sbt_sbo.sog <- get_sbt_sbo("SoG")
sbt.sog <- sbt_sbo.sog[[1]]
sbo.sog <- sbt_sbo.sog[[2]]
rel.sbo.sog <- rep(1, length(sbo.sog))
mp.lst.sog <- get_hcr(sbt.sog,
                      sbo.sog,
                      fn = here("data/mp-sog.csv"))
mp.vals.sog <- mp.lst.sog[[1]]
mp.sog <- mp.lst.sog[[2]]

sbt_sbo.wcvi <- get_sbt_sbo("WCVI")
sbt.wcvi <- sbt_sbo.wcvi[[1]]
sbo.wcvi <- sbt_sbo.wcvi[[2]]
rel.sbo.wcvi <- rep(1, length(sbo.wcvi))
mp.lst.wcvi <- get_hcr_add(sbt.wcvi,
                      sbo.wcvi,
                      fn = here("data/mp-wcvi.csv"))
mp.vals.wcvi <- mp.lst.wcvi[[1]]
mp.wcvi <- mp.lst.wcvi[[2]]


sog.min.esc.rel.50.hr.10.cap.2 <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     rel.sbo.sog,
                     tibble(esc = 0.5,
                            abs_esc = 0,
                            cap = 2.0,
                            hr = 0.1,
                            lrp = NA,
                            usr = NA,
                            num_end_yrs = NA)))

wcvi.min.esc.rel.50.hr.10.cap.2 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     rel.sbo.wcvi,
                     tibble(esc = 0.5,
                            abs_esc = 0,
                            cap = 2.0,
                            hr = 0.1,
                            lrp = NA,
                            usr = NA,
                            num_end_yrs = NA)))

sog.min.hs.3060.hr.20 <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     sbo.sog,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.2,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = NA)))

wcvi.min.hs.3060.hr.20 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = 0.0,
                            hr = 0.2,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = NA)))

sog.min.hs.3060.hr.20.cap.30 <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     sbo.sog,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = 30.0,
                            hr = 0.2,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = NA)))

wcvi.min.hs.5060.hr.10.cap.2 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = 2.0,
                            hr = 0.1,
                            lrp = 0.5,
                            usr = 0.6,
                            num_end_yrs = NA)))

sog.min.esc.rel.50.hr.10.cap.2.slow <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     sbo.sog,
                     tibble(esc = 0.5,
                            abs_esc = 0,
                            cap = 2.0,
                            hr = 0.1,
                            lrp = NA,
                            usr = NA,
                            num_end_yrs = 3)))

wcvi.min.esc.rel.50.hr.10.cap.2.slow <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = 0.5,
                            abs_esc = 0,
                            cap = 2.0,
                            hr = 0.1,
                            lrp = NA,
                            usr = NA,
                            num_end_yrs = 3)))

sog.min.hs.3060.hr.20.slow <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     sbo.sog,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.2,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = 3)))

wcvi.min.hs.3060.hr.20.slow <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.2,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = 3)))

sog.min.hs.3060.hr.10.cap.2.slow <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     sbo.sog,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = 0.2,
                            hr = 0.1,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = 3)))

wcvi.min.hs.3060.hr.10.cap.2.slow <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = 0.2,
                            hr = 0.1,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = 3)))

sog.min.esc.rel.30.hr.10 <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     rel.sbo.sog,
                     tibble(esc = 0.3,
                            abs_esc = 0,
                            cap = NA,
                            hr = 0.1,
                            lrp = NA,
                            usr = NA,
                            num_end_yrs = NA)))

sog.min.esc.rel.30.hr.20 <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     rel.sbo.sog,
                     tibble(esc = 0.3,
                            abs_esc = 0,
                            cap = NA,
                            hr = 0.2,
                            lrp = NA,
                            usr = NA,
                            num_end_yrs = NA)))

sog.min.hs.3040.hr.30 <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     sbo.sog,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.3,
                            lrp = 0.3,
                            usr = 0.4,
                            num_end_yrs = NA)))

wcvi.min.hs.3060.hr.10.cap.2 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = 2.0,
                            hr = 0.1,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = NA)))

wcvi.min.hs.3060.hr.15.cap.2 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = 2.0,
                            hr = 0.15,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = NA)))
wcvi.min.hs.5060.hr.10 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.1,
                            lrp = 0.5,
                            usr = 0.6,
                            num_end_yrs = NA)))
wcvi.min.hs.5060.hr.15 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.15,
                            lrp = 0.5,
                            usr = 0.6,
                            num_end_yrs = NA)))

wcvi.min.esc.rel.30.hr.05 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     rel.sbo.wcvi,
                     tibble(esc = 0.3,
                            abs_esc = 0,
                            cap = NA,
                            hr = 0.05,
                            lrp = NA,
                            usr = NA,
                            num_end_yrs = NA)))

#NOTE this was set incorrectly just to get the 0
wcvi.consTAC.cap.1 <-
  get_hcr_tac_hr(hcr(sbt.wcvi,
                     sbo.wcvi,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.15,
                            lrp = 0.5,
                            usr = 0.6,
                            num_end_yrs = NA)))

sog.min.hs.3060.hr.10 <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     sbo.sog,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.1,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = NA)))

sog.min.hs.3060.hr.15 <-
  get_hcr_tac_hr(hcr(sbt.sog,
                     sbo.sog,
                     tibble(esc = NA,
                            abs_esc = NA,
                            cap = NA,
                            hr = 0.15,
                            lrp = 0.3,
                            usr = 0.6,
                            num_end_yrs = NA)))

hcr.sog <- sog.min.hs.3060.hr.20.cap.30

hcr.wcvi <- wcvi.min.hs.5060.hr.10.cap.2
