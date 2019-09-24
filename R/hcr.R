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

#' Retrieve MP file with decision table data, perform HCR calculations,
#' and return the table
#'
#' @param name name of the SAR Region
#' @param fn file which contains the MP data for the given region
#'
#' @return a `tibble::tibble()` holding the data from the file plus two columns, catch, and hr (harvest rate)
#' which are the catch limits and harverst rate calculated for the region by MP. Each value is a median of the
#' values calculated for each point of the posterior.
get_hcr <- function(sbt, sbo, fn){
  mp <- read_csv(fn) %>%
    as_tibble() %>%
    arrange(desc(obj1), desc(obj2)) %>%
    mutate(tac = NA,
           targ.hr = NA)
  for(rw in seq_len(nrow(mp))){
    mp[rw,] <- hcr(sbt, sbo, row = mp[rw,])
  }
  mp %>% select(mp,
                label,
                obj1,
                obj2,
                obj3,
                obj4,
                catch,
                tac,
                targ.hr)
}

sbt_sbo.hg <- get_sbt_sbo("HG")
sbt.hg <- sbt_sbo.hg[[1]]
sbo.hg <- sbt_sbo.hg[[2]]
rel.sbo.hg <- rep(1, length(sbo.hg))

sbt_sbo.cc <- get_sbt_sbo("CC")
sbt.cc <- sbt_sbo.cc[[1]]
sbo.cc <- sbt_sbo.cc[[2]]
rel.sbo.cc <- rep(1, length(sbo.cc))
mp.cc <- get_hcr(sbt.cc,
                 sbo.cc,
                 fn = here("data/mp-cc.csv"))

sbt_sbo.prd <- get_sbt_sbo("PRD")
sbt.prd <- sbt_sbo.prd[[1]]
sbo.prd <- sbt_sbo.prd[[2]]
rel.sbo.prd <- rep(1, length(sbo.prd))
mp.prd <- get_hcr(sbt.prd,
                  sbo.prd,
                  fn = here("data/mp-prd.csv"))

sbt_sbo.sog <- get_sbt_sbo("SoG")
sbt.sog <- sbt_sbo.sog[[1]]
sbo.sog <- sbt_sbo.sog[[2]]
rel.sbo.sog <- rep(1, length(sbo.sog))
mp.sog <- get_hcr(sbt.sog,
                  sbo.sog,
                  fn = here("data/mp-sog.csv"))

sbt_sbo.wcvi <- get_sbt_sbo("WCVI")
sbt.wcvi <- sbt_sbo.wcvi[[1]]
sbo.wcvi <- sbt_sbo.wcvi[[2]]
rel.sbo.wcvi <- rep(1, length(sbo.wcvi))

sog.min.esc.rel.50.hr.10.cap.2 <- hcr(sbt.sog,
                                      rel.sbo.sog,
                                      tibble(esc = 0.5,
                                             abs_esc = 0,
                                             cap = 2.0,
                                             hr = 0.1,
                                             lrp = NA,
                                             usr = NA,
                                             slowyrs = NA))

wcvi.min.esc.rel.50.hr.10.cap.2 <- hcr(sbt.wcvi,
                                       rel.sbo.wcvi,
                                       tibble(esc = 0.5,
                                              abs_esc = 0,
                                              cap = 2.0,
                                              hr = 0.1,
                                              lrp = NA,
                                              usr = NA,
                                              slowyrs = NA))

sog.min.hs.3060.hr.20 <- hcr(sbt.sog,
                             sbo.sog,
                             tibble(esc = NA,
                                    abs_esc = NA,
                                    cap = NA,
                                    hr = 0.2,
                                    lrp = 0.3,
                                    usr = 0.6,
                                    slowyrs = NA))

wcvi.min.hs.3060.hr.20 <- hcr(sbt.wcvi,
                              sbo.wcvi,
                              tibble(esc = NA,
                                     abs_esc = NA,
                                     cap = 0.0,
                                     hr = 0.2,
                                     lrp = 0.3,
                                     usr = 0.6,
                                     slowyrs = NA))

sog.min.hs.3060.hr.20.cap.30 <- hcr(sbt.sog,
                                    sbo.sog,
                                    tibble(esc = NA,
                                           abs_esc = NA,
                                           cap = 30.0,
                                           hr = 0.2,
                                           lrp = 0.3,
                                           usr = 0.6,
                                           slowyrs = NA))

wcvi.min.hs.5060.hr.10.cap.2 <- hcr(sbt.wcvi,
                                    sbo.wcvi,
                                    tibble(esc = NA,
                                           abs_esc = NA,
                                           cap = 2.0,
                                           hr = 0.1,
                                           lrp = 0.5,
                                           usr = 0.6,
                                           slowyrs = NA))

sog.min.esc.rel.50.hr.10.cap.2.slow <- hcr(sbt.sog,
                                           sbo.sog,
                                           tibble(esc = 0.5,
                                                  abs_esc = 0,
                                                  cap = 2.0,
                                                  hr = 0.1,
                                                  lrp = NA,
                                                  usr = NA,
                                                  slowyrs = 3))

wcvi.min.esc.rel.50.hr.10.cap.2.slow <- hcr(sbt.wcvi,
                                            sbo.wcvi,
                                            tibble(esc = 0.5,
                                                   abs_esc = 0,
                                                   cap = 2.0,
                                                   hr = 0.1,
                                                   lrp = NA,
                                                   usr = NA,
                                                   slowyrs = 3))

sog.min.hs.3060.hr.20.slow <- hcr(sbt.sog,
                                  sbo.sog,
                                  tibble(esc = NA,
                                         abs_esc = NA,
                                         cap = NA,
                                         hr = 0.2,
                                         lrp = 0.3,
                                         usr = 0.6,
                                         slowyrs = 3))

wcvi.min.hs.3060.hr.20.slow <- hcr(sbt.wcvi,
                                   sbo.wcvi,
                                   tibble(esc = NA,
                                          abs_esc = NA,
                                          cap = NA,
                                          hr = 0.2,
                                          lrp = 0.3,
                                          usr = 0.6,
                                          slowyrs = 3))

sog.min.hs.3060.hr.10.cap.2.slow <- hcr(sbt.sog,
                                        sbo.sog,
                                        tibble(esc = NA,
                                               abs_esc = NA,
                                               cap = 0.2,
                                               hr = 0.1,
                                               lrp = 0.3,
                                               usr = 0.6,
                                               slowyrs = 3))

wcvi.min.hs.3060.hr.10.cap.2.slow <- hcr(sbt.wcvi,
                                         sbo.wcvi,
                                         tibble(esc = NA,
                                                abs_esc = NA,
                                                cap = 0.2,
                                                hr = 0.1,
                                                lrp = 0.3,
                                                usr = 0.6,
                                                slowyrs = 3))

hcr.sog <- sog.min.hs.3060.hr.20.cap.30

hcr.wcvi <- wcvi.min.hs.5060.hr.10.cap.2
