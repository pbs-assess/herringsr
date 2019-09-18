model_ind <- match(en2fr("SoG", french), major_regions_short)
sog.b <- major_models[[model_ind]]

model_ind <- match(en2fr("WCVI", french), major_regions_short)
wcvi.b <- major_models[[model_ind]]

bt.sog <- sog.b$mcmccalcs$sbt.quants[2,]
## Replace last year (assess_yr + 1 spawning biomass with the projected one)
bt.sog[length(bt.sog)]<- sog.b$mcmccalcs$proj.quants[2,2]
bo.sog <- as.data.frame(sog.b$mcmccalcs$p.quants)$sbo[2]

bt.wcvi <- wcvi.b$mcmccalcs$sbt.quants[2,]
## Replace last year (assess_yr + 1 spawning biomass with the projected one)
bt.wcvi[length(bt.wcvi)]<- wcvi.b$mcmccalcs$proj.quants[2,2]
bo.wcvi <- as.data.frame(wcvi.b$mcmccalcs$p.quants)$sbo[2]

sog.min.esc.rel.50.hr.10.cap.2 <- hcr.min.esc(bt.sog,
                                              ref.hr = 0.1,
                                              min.esc = 0.5,
                                              catch.cap = 2.0)
wcvi.min.esc.rel.50.hr.10.cap.2 <- hcr.min.esc(bt.wcvi,
                                               ref.hr = 0.1,
                                               min.esc = 0.5,
                                               catch.cap = 2.0)

sog.min.hs.3060.hr.20 <- hcr.hs(bt.sog,
                                ref.hr = 0.2,
                                lrp = 0.3,
                                usr = 0.6,
                                catch.cap = 0.0,
                                bo = bo.sog)
wcvi.min.hs.3060.hr.20 <- hcr.hs(bt.wcvi,
                                 ref.hr = 0.2,
                                 lrp = 0.3,
                                 usr = 0.6,
                                 catch.cap = 0.0,
                                 bo = bo.wcvi)

##### This one for SoG #####
sog.min.hs.3060.hr.20.cap.30 <-hcr.hs(bt.sog,
                                       ref.hr = 0.2,
                                       lrp = 0.3,
                                       usr = 0.6,
                                       catch.cap = 30.0,
                                       bo = bo.sog)

##### This one for WCVI #####
wcvi.min.hs.5060.hr.10.cap.2 <- hcr.hs(bt.wcvi,
                                       ref.hr = 0.1,
                                       lrp = 0.5,
                                       usr = 0.6,
                                       catch.cap = 2.0,
                                       bo = bo.wcvi)

sog.min.esc.rel.50.hr.10.cap.2.slow <- hcr.min.esc.slow(bt.sog,
                                                        num.end.yrs = 3,
                                                        ref.hr = 0.1,
                                                        min.esc = 0.5,
                                                        catch.cap = 2.0,
                                                        bo = bo.sog)
wcvi.min.esc.rel.50.hr.10.cap.2.slow <- hcr.min.esc.slow(bt.wcvi,
                                                         num.end.yrs = 3,
                                                         ref.hr = 0.1,
                                                         min.esc = 0.5,
                                                         catch.cap = 2.0,
                                                         bo = bo.wcvi)

sog.min.hs.3060.hr.20.slow <- hcr.hs.slow(bt.sog,
                                          num.end.yrs = 3,
                                          ref.hr = 0.2,
                                          lrp = 0.3,
                                          usr = 0.6,
                                          catch.cap = 0.0,
                                          bo = bo.sog)
wcvi.min.hs.3060.hr.20.slow <- hcr.hs.slow(bt.wcvi,
                                           num.end.yrs = 3,
                                           ref.hr = 0.2,
                                           lrp = 0.3,
                                           usr = 0.6,
                                           catch.cap = 0.0,
                                           bo = bo.wcvi)

sog.min.hs.3060.hr.10.cap.2.slow <- hcr.hs.slow(bt.sog,
                                                num.end.yrs = 3,
                                                ref.hr = 0.1,
                                                lrp = 0.3,
                                                usr = 0.6,
                                                catch.cap = 2.0,
                                                bo = bo.sog)
wcvi.min.hs.3060.hr.10.cap.2.slow <- hcr.hs.slow(bt.wcvi,
                                                 num.end.yrs = 3,
                                                 ref.hr = 0.1,
                                                 lrp = 0.3,
                                                 usr = 0.6,
                                                 catch.cap = 2.0,
                                                 bo = bo.wcvi)

# hcr.sog <- c(sog.min.esc.rel.50.hr.10.cap.2,
#              sog.min.hs.3060.hr.20,
#              sog.min.hs.3060.hr.10.cap.2,
#              sog.min.esc.rel.50.hr.10.cap.2.slow,
#              sog.min.hs.3060.hr.20.slow,
#              sog.min.hs.3060.hr.10.cap.2.slow)
hcr.sog <- sog.min.hs.3060.hr.20.cap.30

# hcr.wcvi <- c(wcvi.min.esc.rel.50.hr.10.cap.2,
#               wcvi.min.hs.3060.hr.20,
#               wcvi.min.hs.3060.hr.10.cap.2,
#               wcvi.min.esc.rel.50.hr.10.cap.2.slow,
#               wcvi.min.hs.3060.hr.20.slow,
#               wcvi.min.hs.3060.hr.10.cap.2.slow)

hcr.wcvi <- wcvi.min.hs.5060.hr.10.cap.2

# names(hcr.wcvi) <- names(hcr.sog) <-
#   c("minE_rel50_hr10_cap2",
#     "minHS3060_hr20",
#     "minHS3060_hr10_cap2",
#     "minE_rel50_hr10_cap2_slow",
#     "minHS3060_hr20_slow",
#     "minHS3060_hr10_cap2_slow")
