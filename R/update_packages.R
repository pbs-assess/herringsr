git_dir <- "C:/github"
if( Sys.getenv("USERNAME") == "GrinnellM" )  git_dir <- "C:/Grinnell/Git"
curr_dir <- getwd()
shell(paste0("cd ", file.path(git_dir, "gfutilities"), " && git pull --rebase"))
shell(paste0("cd ", file.path(git_dir, "gfiscamutils"), " && git pull --rebase"))
shell(paste0("cd ", file.path(git_dir, "rosettafish"), " && git pull --rebase"))
shell(paste0("cd ", file.path(git_dir, "herringutils"), " && git pull --rebase"))
shell(paste0("cd ", file.path(git_dir, "csasdown"), " && git pull --rebase"))

setwd(git_dir)
devtools::install("rosettafish", quick = TRUE, dependencies = FALSE)
devtools::install("gfutilities", quick = TRUE, dependencies = FALSE)
devtools::install("gfiscamutils", quick = TRUE, dependencies = FALSE)
devtools::install("herringutils", quick = TRUE, dependencies = FALSE)
devtools::install("csasdown", quick = TRUE, dependencies = FALSE)
setwd(curr_dir)
