# TODO: Question.. Do you want to run the SCA models (Y/N)?

# Variables
year <- "2022"
num_mcmc <- 20000
save_nth <- 10
model_dir <- "models"
sars <- c("HG", "PRD", "CC", "SoG", "WCVI")
files_keep <- c(
  "iscam.exe", "xyz.ctl", "xyz.dat", "iscam.dat", "xyz.pfc"
)

# Loop over SARs
for (i in seq(sars)) {
  # Change directory
  setwd(dir = here(model_dir, sars[i]))
  # Message
  cat("Running SCA model in ", getwd(), "...", sep = "")
  # Get directories
  dirs <- list.dirs(full.names = FALSE, recursive = FALSE)
  # Remove directories
  if ("mcmc" %in% dirs) unlink("mcmc", recursive = TRUE)
  if ("retrospectives" %in% dirs) unlink("retrospectives", recursive = TRUE)
  # Stop if folders still exist
  if (length(list.dirs(full.names = FALSE, recursive = FALSE)) >= 1) {
    stop("Directories still exist in ", getwd(), call. = FALSE)
  }
  # Update file names to keep
  keepers <- gsub(
    pattern = "xyz",
    replacement = paste("Herring", sars[i], year, sep = ""),
    x = files_keep
  )
  # Get files
  files <- list.files()
  # Files to remove
  files_remove <- files[!files %in% keepers]
  # Remove files
  if (length(files_remove) >= 1) unlink(x = files_remove)
  # Stop if there are more than keepers
  if (length(list.files()) > length(files_keep)) {
    stop("Extra files still exist in ", getwd(), call. = FALSE)
  }
  # Ensure required files still exist
  if(!all(keepers %in% list.files()))
    stop("Missing required files in ", getwd(), call. = FALSE)
  # Run iscam
  system("iscam")
  # Run iscam: MCMCs
  system(paste("iscam -mcmc", num_mcmc, "-mcsave", save_nth))
  # Run iscam: eval
  system("iscam -mceval")
  # Make folder to hold MCMC output
  dir.create("mcmc")
  # Get CSVs
  csvs <- list.files(pattern = "*.csv")
  # Copy CSVs to folder
  file.copy(from = csvs, to = "mcmc")
  # Remove CSVs
  file.remove(csvs)
  # Re-set the working directory
  setwd(dir = here())
  # Update message
  cat(" done.\n")
} # End i loop over SARs
