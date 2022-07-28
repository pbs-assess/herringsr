require(here)
models_dir <- here("models")
major_stock_dir <- list("HG", "PRD", "CC", "SoG", "WCVI")
minor_stock_dir <- list("A27", "A2W")
special_stock_dir <- list("A10")
assess_yr <- 2022
mcmc_length <- 200
mcmc_samp_freq <- 10
mcmc_burnin <- 10
mcmc_num_samples <- (mcmc_length / mcmc_samp_freq) - mcmc_burnin

# Function to run SCA
run_sca <- function(sars,
                    models_loc = models_dir,
                    files_keep = c(
                      "iscam.exe", "xyz.ctl", "xyz.dat", "iscam.dat", "xyz.pfc"
                    ),
                    mcmc_loc = "mcmc",
                    retro_loc = "retrospectives",
                    mcmc_len = mcmc_length,
                    mcmc_freq = mcmc_samp_freq) {
  # Unlist SARs
  sars <- unlist(sars)
  # Loop over SARs
  for (i in seq(sars)) {
    # Get ith SAR
    sar <- sars[i]
    # Change directory
    setwd(dir = here(models_loc, sar))
    # Message
    cat("Running SCA model in ", getwd(), "...", sep = "")
    # Get directories
    dirs <- list.dirs(full.names = FALSE, recursive = FALSE)
    # Remove directories
    if (mcmc_loc %in% dirs) unlink(mcmc_loc, recursive = TRUE)
    if (retro_loc %in% dirs) unlink(retro_loc, recursive = TRUE)
    # Stop if folders still exist
    if (length(list.dirs(full.names = FALSE, recursive = FALSE)) >= 1) {
      stop("Directories still exist in ", getwd(), call. = FALSE)
    }
    # Update file names to keep
    keepers <- gsub(
      pattern = "xyz",
      replacement = paste("Herring", sar, assess_yr, sep = ""),
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
    system(paste("iscam -mcmc", mcmc_len, "-mcsave", mcmc_freq))
    # Run iscam: eval
    system("iscam -mceval")
    # Make folder to hold MCMC output
    dir.create(mcmc_loc)
    # Get CSVs
    csvs <- list.files(pattern = "*.csv")
    # Copy CSVs to folder
    file.copy(from = csvs, to = mcmc_loc)
    # Remove CSVs
    file.remove(csvs)
    # Re-set the working directory
    setwd(dir = here())
    # Update message
    cat(" done.\n")
  } # End i loop over SARs
} # End run_sca function

# Run SCAs
run_sca(sars = major_stock_dir)