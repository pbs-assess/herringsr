require(xtable)
require(dplyr)

decision_tables_mp_add <- function(df,
                                   xcaption = "Default",
                                   xlabel = "tab:default",
                                   font.size = 9,
                                   space.size = 10,
                                   placement = "ht",
                                   last_col_header = "TAC",
                                   perc_dec_pts = 0,
                                   dec_pts = 2,
                                   col_align = "cc|c|c|c|c|c|c|c|c|c|c",
                                   inc_mps = NA,
                                   translate = FALSE){

  df$label <- gsub("_", "\\\\_", df$label)

  # If the conservation target is less than 75%, show -- for TAC and hr
  df <- df %>%
    mutate(tac = ifelse(obj1 < 0.75, NA, tac),
           targ.hr = ifelse(obj1 < 0.75, NA, targ.hr))

  df <- df %>%
    mutate(om,
           obj1 = paste0(f(obj1 * 100, dec.points = perc_dec_pts), "\\%"),
           obj2 = paste0(f(obj2 * 100, dec.points = perc_dec_pts), "\\%"),
           obj2b = paste0(f(obj2b * 100, dec.points = perc_dec_pts), "\\%"),
           obj2c = paste0(f(obj2c * 100, dec.points = perc_dec_pts), "\\%"),
           obj2d = paste0(f(obj2d * 100, dec.points = perc_dec_pts), "\\%"),
           obj3 = f(obj3, dec.points = dec_pts),
           obj4 = f(obj4, dec.points = dec_pts),
           catch = paste0(f(catch * 100, dec.points = perc_dec_pts), "\\%"),
           tac = ifelse(tac == "--", "--", f(tac, dec.points = dec_pts)),
           targ.hr = ifelse(targ.hr == "--", "--", f(targ.hr, dec.points = dec_pts)))
  df[is.na(df)] <- "--"

  if(!is.na(inc_mps[1])){
    df <- df %>%
      filter(mp %in% inc_mps)
  }

  new_rows <- list()
  new_rows$pos <- list()
  new_rows$pos[[1]] <- -1
  new_rows$pos[[2]] <- -1
  new_rows$pos[[3]] <- -1
  new_rows$pos[[4]] <- -1
  new_rows$command <- c(paste0(latex.cline("1-12"),
                               latex.amp(2),
                               latex.bold(en2fr("Conservation", translate = translate)),
                               latex.amp(),
                               latex.mcol(4,
                                          "c|",
                                          latex.bold(en2fr("Biomass", translate = translate))),
                               latex.amp(1),
                               latex.mcol(3,
                                          "c|",
                                          latex.bold(en2fr("Yield", translate = translate))),
                               latex.amp(2),
                               latex.nline),
                        paste0(latex.mcol(2,
                                          "c|",
                                          latex.bold(en2fr("Scenario", translate = translate))),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 1 (",
                                                 en2fr("LRP", translate = translate),
                                                 ")")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 2")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 2b")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 2c")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 2d")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 3")),
                               latex.amp(),
                               latex.bold(paste0(en2fr("Obj", translate = translate),
                                                 " 4")),
                               latex.amp(),
                               latex.bold(en2fr("Catch", translate = translate)),
                               latex.amp(),
                               latex.bold(last_col_header),
                               latex.amp(),
                               latex.bold(en2fr("HR", translate = translate)),
                               latex.nline),
                        paste0(latex.amp(2),
                               "$\\geq 75\\%$",
                               latex.amp(),
                               "$\\geq 50\\%$",
                               latex.amp(),
                               "new",
                               latex.amp(),
                               "new",
                               latex.amp(),
                               "new",
                               latex.amp(),
                               "$< 25\\%$",
                               latex.amp(),
                               "max",
                               latex.amp(),
                               "min",
                               latex.amp(),
                               latex.bold(paste0(en2fr("by", translate = translate),
                                                 " ",
                                                 en2fr("MP", translate = translate))),
                               latex.amp(),
                               latex.nline),
                        paste0(latex.cline("3-10"),
                               latex.bold(en2fr("OM", translate = translate)),
                               latex.amp(),
                               latex.bold(en2fr("MP", translate = translate)),
                               latex.amp(),
                               ifelse(translate,
                                      "$P(\\mli{BR}_t > 0.3\\mli{BR}_0)$",
                                      "$P(\\mli{SB}_t > 0.3\\mli{SB}_0)$"),
                               latex.amp(),
                               ifelse( translate,
                                       "$P(\\mli{BR}_t \\geq 0.6\\mli{BR}_0)$",
                                       "$P(\\mli{SB}_t \\geq 0.6\\mli{SB}_0)$"),
                               latex.amp(),
                               "new",
                               latex.amp(),
                               "new",
                               latex.amp(),
                               "new",
                               latex.amp(),
                               en2fr("AAV", translate = translate),
                               latex.amp(),
                               "$\\overline{C}$",
                               latex.amp(),
                               "$P(C_t < 650~\\text{t})$",
                               latex.amp(),
                               latex.bold("(1000~t)"),
                               latex.amp(),
                               latex.nline,
                               latex.cline("1-12")))
  # Horizontal line locations for separating groups of OMs
  last_ddm <- which(df$om == "DDM")
  last_dim <- which(df$om == "DIM")
  last_conm <- which(df$om == "conM")
  last_ddm <- tail(last_ddm, 1)
  last_dim <- tail(last_dim, 1)
  last_conm <- tail(last_conm, 1)
  new_rows$pos[[5]] <- last_ddm
  new_rows$pos[[6]] <- last_dim
  new_rows$pos[[7]] <- last_conm
  new_rows$command <- c(new_rows$command,
                        latex.cline("1-12"),
                        latex.cline("1-12"),
                        latex.cline("1-12"))
  size.string <- latex.size.str(font.size, space.size)
  df$om <- en2fr(df$om, translate, allow_missing = TRUE)
  print(xtable(df,
               caption = xcaption,
               label = xlabel),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string ,
        add.to.row = new_rows,
        table.placement = placement,
        tabular.environment = "tabular",
        hline.after = NULL)
}

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