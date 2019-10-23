git_dir <- "C:/github"
if( tolower(Sys.getenv("USERNAME")) == "grinnellm" )  git_dir <- "C:/Grinnell/Git"
if( tolower(Sys.getenv("USERNAME")) == "grandin" )  git_dir <- "C:/github/main"
curr_dir <- getwd()

cat(crayon::green("\nRebasing new commits from rosettafish...\n"))
shell(paste0("cd ", file.path(git_dir, "rosettafish"), " && git pull --rebase"))
cat(crayon::green("\nRebasing new commits from gfutilities...\n"))
shell(paste0("cd ", file.path(git_dir, "gfutilities"), " && git pull --rebase"))
cat(crayon::green("\nRebasing new commits from gfiscamutils...\n"))
shell(paste0("cd ", file.path(git_dir, "gfiscamutils"), " && git pull --rebase"))
cat(crayon::green("\nRebasing new commits from herringutils...\n"))
shell(paste0("cd ", file.path(git_dir, "herringutils"), " && git pull --rebase"))
cat(crayon::green("\nRebasing new commits from csasdown...\n"))
shell(paste0("cd ", file.path(git_dir, "csasdown"), " && git pull --rebase"))
cat(crayon::green("\nRebasing new commits from herringsr...\n"))
shell(paste0("cd ", file.path(git_dir, "herringsr"), " && git pull --rebase"))

setwd(git_dir)
cat(crayon::green("\nBuilding and installing rosettafish package...\n"))
devtools::install("rosettafish", quick = TRUE, dependencies = FALSE)
cat(crayon::green("\nBuilding and installing gfutilities package...\n"))
devtools::install("gfutilities", quick = TRUE, dependencies = FALSE)
cat(crayon::green("\nBuilding and installing gfiscamutils package...\n"))
devtools::install("gfiscamutils", quick = TRUE, dependencies = FALSE)
cat(crayon::green("\nBuilding and installing herringutils package...\n"))
devtools::install("herringutils", quick = TRUE, dependencies = FALSE)
cat(crayon::green("\nBuilding and installing csasdown package...\n"))
devtools::install("csasdown", quick = TRUE, dependencies = FALSE)
setwd(curr_dir)

