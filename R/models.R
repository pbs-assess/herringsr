#### Sources #####

# # Location and name of the location database and tables
# areaLoc <- list(
#     loc=file.path(dirShare, "Stock_Assess_Database"),
#     db="HSA_BE_Locations_Main_v6.1_2015.mdb",
#     fns=list(sections="Sections", locations="Location") )
#
# # Location(s) and names of the Sections and land shapefiles
# shapesLoc <- list(
#     locSec=file.path(dirShare, "Kristen", "Herring_Shapefiles"),
#     locLand=file.path("..", "..", "Data", "Polygons"),
#     fns=list(sections="SectionsIntegrated", land="GSHHS_h_L1_Alb") )

##### Parameters #####

allRegions <- list(
  major = c("HG",
            "PRD",
            "CC",
            "SoG",
            "WCVI"),
  minor = c("A27",
            "A2W"))

allRegionNames <- list(
  major = c("Haida Gwaii (HG)",
            "Prince Rupert District (PRD)",
            "Central Coast (CC)",
            "Strait of Georgia (SoG)",
            "West Coast of Vancouver Island (WCVI)"),
  minor = c("Area 27 (A27)",
            "Area 2 West (A2W)"))

# Cross-walk table for SAR to region and region name (and french)
regions <- read_csv(file =
    "SAR, Region, RegionName, Major, RegionFR, RegionNameFR
        1, HG, Haida Gwaii, TRUE, HG, Haida Gwaii
        2, PRD, Prince Rupert District, TRUE, DPR, District de Prince Rupert
        3, CC, Central Coast, TRUE, CC, C\u{00F4}te centrale
        4, SoG, Strait of Georgia, TRUE, DG, D\u{00E9}troit de Georgie
        5, WCVI, West Coast of Vancouver Island, TRUE, COIV, C\u{00F4}te Ouest de l'\u{00EE}le de Vancouver
        6, A27, Area 27, FALSE, Z27, Zone 27
        7, A2W, Area 2 West, FALSE, Z2O, Zone 2 ouest",
  col_types = cols())

# Cross-walk table for Period to Gear type
gear <- read_csv(file =
    "Period, Gear, Engin
      Gear1, Other, Autre
      Gear2, RoeSN, RogueFM
      Gear3, RoeGN, RogueSN",
  col_types = cols())

# Age to highlight in figure
ageShow <- 3

# Age class of plus group for proportion-at-age
agePlusProp <- 10

# Age of recruitment
ageRec <- 2

# Number of years to calculate running/rolling mean
nRoll <- 5

# Number of years to calculate running/rolling mean of recruitment deviations
nRollDev <- 3

# Spawn survey method changed from surface (1951--1987) to dive (1988--present)
newSurvYr <- 1988

# Intended harvest rate
intendU <- 0.2

# First year of intended harvest rate
intendUYrs <- 1983

# Figure width
figWidth <- 6.5

# Type of smoothing line
smLine <- "loess"

# Level of confidence interval
ciLevel <- 0.9

# Get ylimits (e.g., weight in kg) for the weight-at-age plot
wtMax <- 150 / 1000

# 1996 fixed cutoff values (t*10^3)
fixedCutoffs <- list( HG=10.7, PRD=12.1, CC=17.6, SoG=21.2, WCVI=18.8 )

# High-productivity years
hiProdYrs <- list( HG=1994:1997, PRD=1994:2002, CC=1990:1999, SoG=1988:2016,
  WCVI=1988:1996 )

# Proportion of B_0 for LRP
propB0 <- 0.3

# Multiplyer of LRP for USR
multLRP <- 2

# Years for calculating mean catch (i.e., recent)
recentCatchYrs <- 35

# Years for calculating mean catch (i.e., early)
earlyCatchYrs <- 1951:1965

# First year to show for minor stock area timseries plots
firstYrMinor <- 1978

# Burnin (1:n; rows to drop)
# TODO: Make sure burn is applied in all the correct places!
burn <- 1000
#
# # Load ADMB data from the dat file
# LoadADMB <- function( SARs ) {
#   # Progress message
#   cat( "Loading input data... " )
#   # Start a loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Get the file location: only need the first model because all input files
#     # should be the same
#     datLoc <- file.path( rootd.models, SAR, mNames[1] )
#     # Look for *.dat files in the directory
#     fName <- list.files( path=datLoc, pattern="Herring[[:alnum:]]+.dat" )
#     # Read the entire file
#     dat <- readLines( con=file.path(datLoc, fName) )
#     # Get model dimensions index
#     mIndex <- which( dat == "##### Model dimensions #####" )
#     # Get number of areas
#     nAreas <- scan( file=file.path(datLoc, fName), skip=mIndex, n=1,
#       quiet=TRUE )
#     # Get number of groups
#     nGroups <- scan( file=file.path(datLoc, fName), skip=mIndex+1, n=1,
#       quiet=TRUE )
#     # Get number of sexes
#     nSexes <- scan( file=file.path(datLoc, fName), skip=mIndex+2, n=1,
#       quiet=TRUE )
#     # Get the first year of data
#     firstYr <- scan( file=file.path(datLoc, fName), skip=mIndex+3, n=1,
#       quiet=TRUE )
#     # Get the last year of data
#     lastYr <- scan( file=file.path(datLoc, fName), skip=mIndex+4, n=1,
#       quiet=TRUE )
#     # Get the vector of years
#     yrRange <<- firstYr:lastYr
#     # Get the youngest age
#     youngAge <- scan( file=file.path(datLoc, fName), skip=mIndex+5, n=1,
#       quiet=TRUE )
#     # Get the oldest age (i.e., plus group)
#     oldAge <- scan( file=file.path(datLoc, fName), skip=mIndex+6, n=1,
#       quiet=TRUE )
#     # Age range: omit below, plus group above
#     ageRange <<- youngAge:oldAge
#     # Get the number of gear types
#     nGears <- scan( file=file.path(datLoc, fName), skip=mIndex+7, n=1,
#       quiet=TRUE )
#
#     # Get catch index
#     cIndex <- which( dat == "##### Catch (t*10^3) #####" )
#     # Get number of observations
#     nObsCatch <- scan( file=file.path(datLoc, fName), skip=cIndex, n=1,
#       quiet=TRUE )
#     # Get catch data
#     catchRaw <- read_delim( file=file.path(datLoc, fName), col_names=TRUE,
#       delim="\t", skip=cIndex+1, n_max=nObsCatch,  trim_ws=TRUE,
#       col_types=paste(c("i", "i", "i", "i", "i", "i", "d"), collapse="") )
#     # Update catch
#     catch <- catchRaw %>%
#       mutate( Region=SAR ) %>%
#       rename( Year=`# Year`, Catch=Value, Period=Gear ) %>%
#       dplyr::select( Region, Year, Period, Catch )
#
#     # Get spawn index
#     sIndex <- which( dat == "##### Spawn (t*10^3) #####" )
#     # Get number of survey types
#     nSurvSpawn <- scan( file=file.path(datLoc, fName), skip=sIndex, n=1,
#       quiet=TRUE )
#     # Get number of years per survey
#     nSurvSpawnYrs <- scan( file=file.path(datLoc, fName), skip=sIndex+1,
#       n=nSurvSpawn, quiet=TRUE )
#     # Get number of observations
#     nObsSpawn <- sum( nSurvSpawnYrs )
#     # Get spawn data
#     spawnRaw <- read_delim( file=file.path(datLoc, fName), col_names=TRUE,
#       delim="\t", skip=sIndex+3, n_max=nObsSpawn,
#       trim_ws=TRUE,
#       col_types=cols("i", "d", "i", "i", "i", "i", "d",
#         "i") )
#     # Translate survey type
#     survType <- read_csv( file=
#         "Gear, Survey
#             4, Surface
#             5, Dive",
#       col_types=cols("i", "c") )
#     # Update spawn
#     spawn <- spawnRaw %>%
#       mutate( Region=SAR ) %>%
#       rename( Year=`# Year` ) %>%
#       left_join( y=survType, by="Gear" ) %>%
#       dplyr::select( Region, Year, Spawn, Survey )
#
#     # Get numAged index
#     nIndex <- which( dat == "##### Number-at-age #####" )
#     # Get number of survey types
#     nSurvNum <- scan( file=file.path(datLoc, fName), skip=nIndex, n=1,
#       quiet=TRUE )
#     # Get number of years per survey
#     nSurvNumYrs <- scan( file=file.path(datLoc, fName), skip=nIndex+1,
#       n=nSurvNum, quiet=TRUE )
#     # Get number of observations
#     nObsNum <- sum( nSurvNumYrs )
#     # Get numAged data
#     numAgedRaw <- read_delim( file=file.path(datLoc, fName), col_names=TRUE,
#       delim="\t", skip=nIndex+6, n_max=nObsNum,
#       trim_ws=TRUE,
#       col_types=paste(rep("i", times=5+oldAge-youngAge+1),
#         collapse="") )
#     # Update numAged
#     numAged <- numAgedRaw %>%
#       mutate( Region=SAR ) %>%
#       rename( Year=`# Year` ) %>%
#       dplyr::select( Region, Year, Gear, as.character(ageRange) )
#
#     # Get weight index
#     wIndex <- which( dat == "##### Weight-at-age (kg) #####" )
#     # Get number of observations
#     nObsWt <- scan( file=file.path(datLoc, fName), skip=wIndex+1, n=1,
#       quiet=TRUE )
#     # Get weight data
#     weightRaw <- read_delim( file=file.path(datLoc, fName), col_names=TRUE,
#       delim="\t", skip=wIndex+2, n_max=nObsWt,
#       trim_ws=TRUE,
#       col_types=paste(c("i", "i", "i", "i", "i",
#         rep("d", times=oldAge-youngAge+1)),
#         collapse="") )
#     # Update weight
#     weight=weightRaw %>%
#       mutate( Region=SAR ) %>%
#       rename( Year=`# Year` ) %>%
#       dplyr::select( Region, Year, as.character(ageRange) )
#
#     # If it's the first region
#     if( k == 1 ) {
#       # Start data frames
#       resCatch <- catch
#       resSpawn <- spawn
#       resNumAged <- numAged
#       resWeight <- weight
#     } else {  # End if it's the first region, otherwise
#       # Append to the data frames
#       resCatch <- bind_rows( resCatch, catch )
#       resSpawn <- bind_rows( resSpawn, spawn )
#       resNumAged <- bind_rows( resNumAged, numAged )
#       resWeight <- bind_rows( resWeight, weight )
#     }  # End if it's not the first region
#   }  # End k loop over regions
#   res <- list( catch=resCatch, spawn=resSpawn, numAged=resNumAged,
#     weight=resWeight )
#   # Update the progress message
#   cat( "done\n" )
#   # Return the list
#   return( res )
# }  # End LoadADMB function
#
# # Load ADMB data (major and minor SARs)
# inputData <- LoadADMB( SARs=unlist(allRegions, use.names=FALSE) )
#
# # Load the number of biosamples
# LoadNBio <- function( SARs ) {
#   # Progress message
#   cat( "Loading biosamples... " )
#   # Start a loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Get the number of biosamples
#     nBio <- fread( input=file.path(rootd.data, paste0("NumBiosamples", SAR, ".csv")),
#       verbose=FALSE )
#     # If it's the first region
#     if( k == 1 ) {
#       # Start data frame
#       dat <- nBio
#     } else {  # End if it's the first region, otherwise
#       # Append to the data frame
#       dat <- bind_rows( dat, nBio )
#     }  # End if it's not the first region
#   }  # End k loop over regions
#   # Wrangle
#   res <- dat %>%
#     as_tibble( ) %>%
#     filter( Year %in% yrRange ) %>%
#     spread( key=Region, value=Total, fill=0 )
#   # Update the progress message
#   cat( "done\n" )
#   # Return the list
#   return( res )
# }  # End LoadNBio function
#
# # Load the number of biosamples
# nBio <- LoadNBio( SARs=unlist(allRegions, use.names=FALSE) )
#
# # Load the spawn distribution (proportion by area)
# LoadPropSpawn <- function( SARs ) {
#   # Progress message
#   cat( "Loading spawn distribution... " )
#   # Start a list to hold output (SARs have different columns)
#   res <- list( )
#   # Start a loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Get the spawn proportions
#     nBio <- fread( input=file.path(rootd.data, paste0("PropSpawn", SAR, ".csv")), header=TRUE,
#       verbose=FALSE )
#     # Add data to the list
#     res[[k]] <- as_tibble( nBio )
#     # Name the list element
#     names( res )[k] <- SAR
#   }  # End k loop over regions
#   # Update the progress message
#   cat( "done\n" )
#   # Return the list
#   return( res )
# }  # End LoadPropSpawn function
#
# # Load the number of biosamples
# pSpawn <- LoadPropSpawn( SARs=unlist(allRegions, use.names=FALSE) )
#
# # Load the SOK harvest
# LoadSOK <- function( SARs ) {
#   # Progress message
#   cat( "Loading SOK... " )
#   # Start a loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Get the SOK harvest
#     nBio <- fread( input=file.path(rootd.data, paste0("HarvSOK", SAR, ".csv")),
#       verbose=FALSE )
#     # If it's the first region
#     if( k == 1 ) {
#       # Start data frame
#       dat <- nBio
#     } else {  # End if it's the first region, otherwise
#       # Append to the data frame
#       dat <- bind_rows( dat, nBio )
#     }  # End if it's not the first region
#   }  # End k loop over regions
#   # Wrangle
#   res <- dat %>%
#     as_tibble( ) %>%
#     filter( Year %in% yrRange ) #%>%
#   # spread( key=Region, value=Total, fill=0 )
#   # Update the progress message
#   cat( "done\n" )
#   # Return the list
#   return( res )
# }  # End LoadSOK function
#
# # Load the number of biosamples
# harvSOK <- LoadSOK( SARs=unlist(allRegions, use.names=FALSE) )
#
# # Get model parameters
# GetModelPars <- function( fn, SARs, models=mNames, probs=ciLevel ) {
#   # Message
#   cat( "Loading model parameters... " )
#   # Get lower CI level
#   lo <- (1 - probs) / 2
#   # Get upper CI level
#   up <- 1 - lo
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Loop over models
#     for( i in 1:length(models) ) {
#       # Get the model
#       model <- models[i]
#       # Load the file and wrangle
#       raw <- fread( input=file.path(rootd.models, SAR, models[i], "mcmc", fn),
#         verbose=FALSE ) %>%
#         as_tibble( ) %>%
#         slice( -1:-burn ) %>%
#         mutate( Model=model, Region=SAR ) %>%
#         dplyr::select( Region, Model, sbo, q1, q2 ) %>%
#         rename( SB0=sbo )
#       # Reshare to long
#       rLong <- raw %>%
#         gather( -Region, -Model, key="Parameter", value="Value" )
#       # Perform some stats
#       mp <- rLong %>%
#         group_by( Region, Model, Parameter ) %>%
#         summarise(
#           Lower=quantile(Value, probs=lo),
#           Median=quantile(Value, probs=0.5),
#           Upper=quantile(Value, probs=up) ) %>%
#         ungroup( )
#       # If it's the first region and model
#       if( k == 1 & i == 1 ) {
#         # Start data frame (raw values)
#         resRaw <- rLong
#         # Start data frame (summarised values)
#         resSummary <- mp
#       } else {  # End if it's the first region and model, otherwise
#         # Append to the data frame (raw values)
#         resRaw <- bind_rows( resRaw, rLong )
#         # Append to the data frame (summarised values)
#         resSummary <- bind_rows( resSummary, mp )
#       }  # End if it's not the first region and model
#     }  # End i loop over models
#   }  # End k loop over regions
#   # Message
#   cat( "done\n" )
#   # Return the data
#   return( res=list( resRaw=resRaw, resSummary=resSummary) )
# }  # End GetModelPars function
#
# # Get model parameters (major SARs only)
# modelPars <- GetModelPars( fn="iscam_mcmc.csv", SARs=allRegions$major )
#
# # Extract raw data
# mRaw <- modelPars$resRaw
#
# # Extract summary data
# mPars <- modelPars$resSummary
#
# # Assemble model parameters
# GetPars <- function( fn, SARs, models=mNames, varName, probs=ciLevel ) {
#   # Progress message
#   cat( "Loading",  varName, "data... " )
#   # Get lower CI level
#   lo <- (1 - probs) / 2
#   # Get upper CI level
#   up <- 1 - lo
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Loop over models
#     for( i in 1:length(models) ) {
#       # Get the model
#       model <- models[i]
#       # Grab the data (transposed)
#       raw <- fread( input=file.path(rootd.models, SAR, model, "mcmc", fn), verbose=FALSE ) %>%
#         as_tibble( ) %>%
#         slice( -1:-burn )
#       # TODO: Perform a check to make sure recruitment is for the requested age
#       # (i.e., 'ageRec').
#       # Grab the years from the header names
#       yrNames <- str_sub( string=names(raw), start=-4, end=-1 )
#       # Calculate the median of model runs for each year
#       out <- tibble( Region=SAR, Model=model, Year=as.numeric(yrNames),
#         Parameter=varName,
#         Lower=apply(X=raw, MARGIN=2, FUN=function(x)
#           quantile(x, probs=lo)),
#         Median=apply(X=raw, MARGIN=2, FUN=function(x)
#           quantile(x, probs=0.5)),
#         Upper=apply(X=raw, MARGIN=2, FUN=function(x)
#           quantile(x, probs=up)) ) %>%
#         filter( Year %in% yrRange )
#       # If it's the first region and model
#       if( k == 1 & i == 1 ) {
#         # Start a data frame
#         res <- out
#       } else {  # End if it's the first region and model, otherwise
#         # Append to the data frame
#         res <- bind_rows( res, out )
#       }  # End if it's not the first region and model
#     }  # End i loop over models
#   }  # End k loop over regions
#   # Update progress message
#   cat( "done\n" )
#   # Return the model output as a data frame
#   return( res )
# }  # End GetPars function
#
# # Get instantaneous natural mortality (major SARs only)
# natMort <- GetPars( fn="iscam_m_mcmc.csv", SARs=allRegions$major,
#   varName="Mortality" )
#
# # Get recruitment (number in millions; major SARs only)
# recruits <- GetPars( fn="iscam_rt_mcmc.csv",
#   SARs=allRegions$major, varName="Recruitment" )
#
# # Get spawning biomass (thousands of tonnes; major SARs only)
# spBio <- GetPars( fn="iscam_sbt_mcmc.csv", SARs=allRegions$major,
#   varName="Abundance" ) %>%
#   mutate( Survey=ifelse(Year < newSurvYr, "Surface", "Dive") )
#
# # Get recruitment deviations (major SARs only)
# recruitDev <- GetPars( fn="iscam_rdev_mcmc.csv",
#   SARs=allRegions$major, varName="RecruitDevs" ) %>%
#   # TODO: omit this filter once we re-run the mcmc eval on the output
#   filter( Year>=min(yrRange)+2 )
#
# # Assemble model values (raw)
# GetVals <- function( fn, SARs, models=mNames, varName, yr ) {
#   # Progress message
#   cat( "Loading raw", varName, yr, "data... " )
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Loop over models
#     for( i in 1:length(models) ) {
#       # Get the model
#       model <- models[i]
#       # Grab the data (transposed)
#       raw <- fread( input=file.path(rootd.models, SAR, model, "mcmc", fn), verbose=FALSE ) %>%
#         as_tibble( ) %>%
#         slice( -1:-burn )
#       # TODO: Perform a check to make sure recruitment is for the requested age
#       # (i.e., 'ageRec').
#       # Grab the years from the header names
#       yrNames <- str_sub( string=names(raw), start=-4, end=-1 )
#       # Add the year names to columns
#       colnames( raw ) <- yrNames
#       # Grab the recent year data
#       raw <- raw %>%
#         dplyr::select( which(colnames(raw)==yr) )
#       # Calculate the median of model runs for each year
#       out <- tibble( Region=SAR, Model=model, Parameter=varName,
#         Value=raw[[1]], Year=yr )
#       # If it's the first region and model
#       if( k == 1 & i == 1 ) {
#         # Start a data frame
#         res <- out
#       } else {  # End if it's the first region and model, otherwise
#         # Append to the data frame
#         res <- bind_rows( res, out )
#       }  # End if it's not the first region and model
#     }  # End i loop over models
#   }  # End k loop over regions
#   # Update progress message
#   cat( "done\n" )
#   # Return the model output as a data frame
#   return( res )
# }  # End GetVals function
#
# # Get current year raw spawning biomass (thousands of tonnes, major SARs only)
# spBioValsCurrent <- GetVals( fn="iscam_sbt_mcmc.csv", SARs=allRegions$major,
#   varName="Abundance", yr=max(yrRange) )
#
# # Assemble model projections
# GetProjected <- function( fn, SARs, models=mNames, probs=ciLevel ) {
#   # Progress message
#   cat( "Loading Projection data... " )
#   # Get lower CI level
#   lo <- (1 - probs) / 2
#   # Get upper CI level
#   up <- 1 - lo
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Loop over models
#     for( i in 1:length(models) ) {
#       # Get the model
#       model <- models[i]
#       # Generate a name for next year's projected biomass
#       sbNextYr <- paste("B", max(yrRange)+1, sep="")
#       # Grab the data
#       raw <- fread( input=file.path(rootd.models, SAR, model, "mcmc", fn), verbose=FALSE ) %>%
#         as_tibble( ) %>%
#         rename_( "SBProj"=sbNextYr ) %>%
#         mutate( Region=SAR, Model=model ) %>%
#         dplyr::select( Region, Model, TAC, SBProj ) %>%  # PropAge3, PropAge4to10
#         slice( -1:(-burn*n_distinct(TAC)) )
#       # Reshare to long
#       rLong <- raw %>%
#         gather( -Region, -Model, -TAC, key="Parameter", value="Value" )
#       # Perform some stats
#       out <- rLong %>%
#         group_by( Region, Model, TAC, Parameter ) %>%
#         summarise(
#           Lower=quantile(Value, probs=lo),
#           Median=quantile(Value, probs=0.5),
#           Upper=quantile(Value, probs=up) ) %>%
#         ungroup( )
#       # If it's the first region and model
#       if( k==1 & i == 1 ) {
#         # Start a data frame
#         res <- out
#       } else {  # End if it's the first region and model, otherwise
#         # Append to the data frame
#         res <- bind_rows( res, out )
#       }  # End if it's not the first region and model
#     }  # End i loop over models
#   }  # End k loop ove regions
#   # Update progress message
#   cat( "done\n" )
#   # Return the model output as a data frame
#   return( res )
# }  # End GetProjected function
#
# # Get projected spawning biomass (major SARs only)
# pPars <- GetProjected( fn="iscammcmc_proj_Gear1.csv", SARs=allRegions$major )
#
# # Get projected raw values
# GetProjVals <- function( fn, SARs, models=mNames, varName, cName, tac ) {
#   # Progress message
#   cat( "Loading raw projected", varName, "data... " )
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Loop over models
#     for( i in 1:length(models) ) {
#       # Get the model
#       model <- models[i]
#       # Grab the data
#       raw <- fread( input=file.path(rootd.models, SAR, model, "mcmc", fn), verbose=FALSE ) %>%
#         as_tibble( ) %>%
#         mutate( Region=SAR, Model=model, Parameter=varName, Year=max(yrRange)+1 ) %>%
#         filter( TAC==tac ) %>%
#         rename_( "Value"=cName ) %>%
#         dplyr::select( Region, Model, Parameter, Value, Year ) %>%
#         slice( -1:(-burn*length(unique(tac))) )
#       # If it's the first region and model
#       if( k==1 & i == 1 ) {
#         # Start a data frame
#         res <- raw
#       } else {  # End if it's the first region and model, otherwise
#         # Append to the data frame
#         res <- bind_rows( res, raw )
#       }  # End if it's not the first region and model
#     }  # End i loop over models
#   }  # End k loop ove regions
#   # Update progress message
#   cat( "done\n" )
#   # Return the model output as a data frame
#   return( res )
# }  # End GetProjected function
#
# # Get projected year raw spawning biomass (thousands of tonnes, major SARs only)
# spBioValsForecast <- GetProjVals( fn="iscammcmc_proj_Gear1.csv",
#   SARs=allRegions$major, varName="Abundance",
#   cName=paste("B", max(yrRange)+1, sep=""), tac=0 )
#
# # Get MPD from rep file
# GetMPD <- function( fn, SARs, models=mNames[1], flag, varName ) {
#   # Progress message
#   cat( "Loading MPD",  varName, "data... " )
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Loop over models
#     for( i in 1:length(models) ) {
#       # Get the model
#       model <- models[i]
#       # Read the file (big blob)
#       obj <- read.rep( file.path(rootd.models, SAR, model, fn) )
#       # Grab the data
#       raw <- obj[names(obj) == flag]
#       # If Abundance
#       if( varName == "Abundance" ) {
#         # Update the year range
#         yrs <- yrRange
#         # Grab the object
#         dat <- tibble( Abundance=raw[[1]][1:length(yrRange)] )
#       }  # End if Abundance
#       # If Recruitment
#       if( varName == "Recruitment" ) {
#         # Update the year range
#         yrs <- yrRange[-c(1:min(ageRange))]
#         # Grab the object
#         dat <- tibble( Recruitment=raw[[1]] )
#       }  # End if Recruitment
#       # Calculate the median of model runs for each year
#       out <- tibble( Region=SAR, Model=model, Year=yrs ) %>%
#         cbind( dat ) %>%
#         as_tibble( )
#       # If it's the first region and model
#       if( k == 1 & i == 1 ) {
#         # Start a data frame
#         res <- out
#       } else {  # End if it's the first region and model, otherwise
#         # Append to the data frame
#         res <- bind_rows( res, out )
#       }  # End if it's not the first region and model
#     }  # End i loop over models
#   }  # End k loop over regions
#   # Update progress message
#   cat( "done\n" )
#   # Return the model output as a data frame
#   return( res )
# }  # End GetMPD function
#
# # Get MPD (abundance)
# abundMPD <- GetMPD( fn="iscam.rep", SARs=allRegions$major, flag="sbt",
#   varName="Abundance" )
#
# # Get MPD (recruitment)
# recMPD <- GetMPD( fn="iscam.rep", SARs=allRegions$major, flag="rt",
#   varName="Recruitment" )
#
# # Get Beverton-Holt parameters
# GetBHPars <- function( fn, SARs, models=mNames[1] ) {
#   # Progress message
#   cat( "Loading BH parameters... " )
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Loop over models
#     for( i in 1:length(models) ) {
#       # Get the model
#       model <- models[i]
#       # Read the file (big blob)
#       obj <- readLines( con=file.path(rootd.models, SAR, model, fn) )
#       # Get kappa
#       kappa <- scan( file=file.path(rootd.models, SAR, model, fn), skip=which(obj == "kappa"),
#         n=1, quiet=TRUE )
#       # Get tau
#       tau <- scan( file=file.path(rootd.models, SAR, model, fn), skip=which(obj == "tau"),
#         n=1, quiet=TRUE )
#       # Get ro
#       ro <- scan( file=file.path(rootd.models, SAR, model, fn), skip=which(obj == "ro"),
#         n=1, quiet=TRUE )
#       # Get sbo
#       sbo <- scan( file=file.path(rootd.models, SAR, model, fn), skip=which(obj == "sbo"),
#         n=1, quiet=TRUE )
#       # Calculate the median of model runs for each year
#       out <- tibble( Region=SAR, Model=model, kappa=kappa, tau=tau, ro=ro,
#         sbo=sbo )
#       # If it's the first region and model
#       if( k == 1 & i == 1 ) {
#         # Start a data frame
#         res <- out
#       } else {  # End if it's the first region and model, otherwise
#         # Append to the data frame
#         res <- bind_rows( res, out )
#       }  # End if it's not the first region and model
#     }  # End i loop over models
#   }  # End k loop over regions
#   # Update progress message
#   cat( "done\n" )
#   # Return the model output as a data frame
#   return( res )
# }  # End GetBHPars function
#
# # Get BH parameters
# bhPars <- GetBHPars( fn="iscam.rep", SARs=allRegions$major )
#
# ##### Main #####
#
# # Format catch data for plotting
# catch <- inputData$catch %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#     RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR),
#     Period=paste("Gear", Period, sep="") ) %>%
#   left_join( y=gear, by="Period" ) %>%
#   mutate( Gear=factor(Gear, levels=gear$Gear),
#     Engin=factor(Engin, levels=gear$Engin) ) %>%
#   dplyr::select( Region, RegionName, Year, Gear, Catch, RegionFR, RegionNameFR, Engin )
#
# # Format SOK harvest data for plotting
# harvSOK <- harvSOK %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName) )
#
# # Annual catch
# annualCatch <- catch %>%
#   group_by( Year ) %>%
#   summarise( Catch=SumNA(Catch) ) %>%
#   ungroup( )
#
# #spBioAM2 <- spBio %>%
# #    filter( Model=="AM2" ) %>% mutate( Year=as.integer(Year) ) %>%
# #    arrange( Region, Year )
# #write_csv( x=spBioAM2, path="spBioAM2.csv" )
# #catch <- inputData$catch %>%
# #    filter( Region %in% allRegions$major ) %>%
# #    mutate( Gear=paste("Gear", Gear, sep="") ) %>%
# #    spread( key=Gear, value=Catch, fill=0 ) %>%
# #    complete( Region, Year, fill=list(Gear1=0, Gear2=0, Gear3=0) ) %>%
# #    arrange( Region, Year )
# #write_csv( x=catch, path="catch.csv" )
# #catch <- inputData$catch %>%
# #    filter( Region %in% allRegions$major ) %>%
# #    group_by( Region, Year ) %>%
# #    summarise( Catch=SumNA(Catch) ) %>%
# #    ungroup( ) %>%
# #    complete( Region, Year, fill=list(Catch=0) ) %>%
# #    mutate( Parameter="Catch" ) %>%
# #    rename( Value=Catch ) %>%
# #    arrange( Region, Year ) %>%
# #    dplyr::select( Region, Year, Parameter, Value )
# #write_csv( x=catch, path="Catch.csv" )
# #recruitment <- recruits %>%
# #    filter( Model=="AM2", Region %in% c("PRD", "SoG") ) %>%
# #    dplyr::select( -Model ) %>%
# #    mutate( Year=as.integer(Year) ) %>%
# #    arrange( Region, Year )
# #write_csv( x=recruitment, path="RecruitsAge2.csv" )
# #spawnBio <- spBio %>%
# #    filter( Model=="AM2", Region %in% c("PRD", "SoG") ) %>%
# #    mutate( Year=as.integer(Year) ) %>%
# #    dplyr::select( Region, Year, Parameter, Survey, Lower, Median, Upper ) %>%
# #    arrange( Region, Year )
# #write_csv( x=spawnBio, path="SpawningBiomass.csv" )
# #recruitmentMPD <- recMPD %>%
# #    filter( Region %in% c("PRD", "SoG") ) %>%
# #    rename( MPD=Recruitment ) %>%
# #    mutate( Parameter="Recruitment" ) %>%
# #    arrange( Region, Year ) %>%
# #    dplyr::select( Region, Year, Parameter, MPD )
# #recruitment <- full_join( x=recruitment, y=recruitmentMPD,
# #    by=c("Region", "Year", "Parameter") )
# #write_csv( x=recruitment, path="RecruitsAge2.csv" )
# #spBioMPD <- abundMPD %>%
# #    filter( Region %in% c("PRD", "SoG") ) %>%
# #    rename( MPD=Abundance ) %>%
# #    mutate( Parameter="Abundance" ) %>%
# #    arrange( Region, Year ) %>%
# #    dplyr::select( Region, Year, Parameter, MPD )
# #spawnBio <- full_join( x=spawnBio, y=spBioMPD,
# #    by=c("Region", "Year", "Parameter") )
# #write_csv( x=spawnBio, path="SpawningBiomass.csv" )
#
# # Format spawn index data for plotting
# spawn <- inputData$spawn %>%
#   mutate( Survey=factor(Survey, levels=c("Surface", "Dive")),
#     Releve=ifelse(Year < newSurvYr, "Surface", "Plong\u{00E9}e"),
#     Releve=factor(Releve, levels=c("Surface", "Plong\u{00E9}e")) ) %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#     RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR) )
#
# # Format number-at-age data for plotting
# numAgedYear <- inputData$numAged %>%
#   dplyr::select( -Gear ) %>%
#   gather( key=Age, value=Number, -Year, -Region ) %>%
#   mutate( Age=as.numeric(Age),
#     Age=ifelse(Age >= agePlusProp, agePlusProp, Age),
#     Age=as.character(Age) ) %>%
#   group_by( Region, Year, Age ) %>%
#   summarise( Number=sum(Number) ) %>%
#   mutate( Proportion=Number/SumNA(Number) ) %>%
#   ungroup( ) %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#     RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR) )
#
# # Determine weighted mean and approximate CI age by year
# qAgedYear <- numAgedYear %>%
#   dplyr::select( Region, Year, Age, Proportion ) %>%
#   mutate( Age=as.numeric(Age) ) %>%
#   group_by( Region, Year ) %>%
#   summarise(
#     MeanAge=weighted.mean(x=Age, w=Proportion),
#     # CI is based on R code by Steve Martel
#     sBar=qnorm(1 - (1 - ciLevel) / 2) *
#       sum(sqrt(Proportion * (1 - Proportion)) / sqrt(Age)),
#     Lower=exp(log(MeanAge) - log(sBar)),
#     Upper=exp(log(MeanAge) + log(sBar)) ) %>%
#   ungroup( ) %>%
#   mutate( GroupID=ConsecutiveGroup(Year) ) %>%
#   arrange( Region, Year ) %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#     RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR) )
#
# # Format weight-at-age data for plotting
# weightLong <- inputData$weight %>%
#   gather( key=Age, value=Weight, -Year, -Region )
#
# # Calculate running mean weight-at-age by year
# muWeightAge <- weightLong %>%
#   group_by( Region, Age ) %>%
#   mutate( muWeight=rollmean(x=Weight, k=5, align="right", na.pad=TRUE) ) %>%
#   ungroup( ) %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( Age=factor(Age),
#     RegionName=factor(RegionName, levels=regions$RegionName),
#     RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR) )
#
# # Subset q parameters
# qPars <- mPars %>%
#   filter( Parameter %in% c("q1", "q2") ) %>%
#   mutate( Survey=ifelse(Parameter=="q1", "Surface", "Dive") )
#
# # Subset biomass parameters
# bPars <- mPars %>%
#   bind_rows( pPars ) %>%
#   filter( TAC==0 | is.na(TAC) & Parameter %in% c("SB0", "SBProj") ) %>%
#   mutate( Year=ifelse(Parameter=="SB0", min(yrRange)-1, max(yrRange)+1) ) %>%
#   dplyr::select( Region, Model, Parameter, Lower, Median, Upper, Year )
#
# # Format current spawning biomass for plotting
# spBioValsCurrent <- spBioValsCurrent %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( Region=factor(Region, levels=regions$Region),
#     RegionName=factor(RegionName, levels=regions$RegionName),
#     Model=factor(Model, levels=mNames) )
#
# # Format forecast spawning biomass for plotting
# spBioValsForecast <- spBioValsForecast %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#     RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR) )
#
# # Get data for the Beverton-Holt
# BevHolt <- abundMPD %>%
#   full_join( recMPD, by=c("Region", "Model", "Year") ) %>%
#   na.omit( ) %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#     Model=factor(Model, levels=mNames),
#     Region=factor(Region, levels=regions$Region) )
#
# # Get data for the effective harvest rate
# harvRate <- catch %>%
#   group_by( Region, Year ) %>%
#   summarise( Catch=SumNA(Catch) ) %>%
#   ungroup( ) %>%
#   full_join( y=spBio, by=c("Region", "Year") ) %>%
#   mutate( LowerHR=Catch/(Lower+Catch), MedianHR=Catch/(Median+Catch),
#     UpperHR=Catch/(Upper+Catch) ) %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#     RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR) )
#
# # Get maximum Abundance by Region and Model
# maxAbund <- BevHolt %>%
#   group_by( Region, Model ) %>%
#   summarise( MaxAbund=MaxNA(Abundance) ) %>%
#   ungroup( )
#
# # Generate predicted line for Beverton-Holt relationship
# predBH <- bhPars %>%
#   full_join( y=maxAbund, by=c("Region", "Model") ) %>%
#   group_by( Region, Model, kappa, tau, ro, sbo ) %>%
#   expand( Abundance=seq(from=0, to=max(MaxAbund, sbo), length.out=100) ) %>%
#   mutate( Recruitment=kappa * ro * Abundance /
#       (sbo + (kappa-1) * Abundance) * exp(-0.5 * tau^2) ) %>%
#   ungroup( ) %>%
#   left_join( y=regions, by="Region" ) %>%
#   mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#     Model=factor(Model, levels=mNames),
#     Region=factor(Region, levels=regions$Region) )
#
# ## Calculate the proportion of spawning biomass
# #CalcPropSSB <- function( dat ) {
# #  # Calculate the proportions
# #  props <- spBio %>%
# #      filter( Region%in%c("SoG", "WCVI"), Model=="AM2") %>%
# #      dplyr::select( Region, Year, Median, Survey ) %>%
# #      rename( SSB=Median ) %>%
# #      spread( key=Region, value=SSB ) %>%
# #      mutate( pSoG=SoG/(SoG+WCVI), pWCVI=WCVI/(SoG+WCVI) )
# #  # Extract the biomass
# #  resBio <- props %>%
# #      dplyr::select( Year, Survey, SoG, WCVI ) %>%
# #      gather( 'SoG', 'WCVI', key=Region, value="SSB" )
# #  # Extract the proportions
# #  resProp <- props %>%
# #      dplyr::select( Year, Survey, pSoG, pWCVI ) %>%
# #      rename( SoG=pSoG, WCVI=pWCVI ) %>%
# #      gather( 'SoG', 'WCVI', key=Region, value="Proportion" )
# #  # Merge the values
# #  res <- full_join( x=resBio, y=resProp, by=c("Year", "Region", "Survey") ) %>%
# #      mutate( Region=factor(Region, levels=c("SoG", "WCVI")),
# #          Survey=factor(Survey, levels=c("Surface", "Dive")) )
# #  # Return the data
# #  return( res )
# #}  # End CalcPropSSB function
# #
# ## Proportion of spawning biomass: SoG and WCVI (AM2)
# #propSSB <- CalcPropSSB( dat=spBio )
#
# ##### Figures #####
#
# # Change default ggplot theme to 'black and white'
# theme_set( theme_bw() )
#
# # Modify default theme
# myTheme <- theme(
#   legend.box.background=element_rect(fill=alpha("white", 0.7)),
#   legend.box.margin=margin(1, 1, 1, 1, "mm"),
#   legend.key=element_blank(), legend.margin=margin(), legend.text.align=1,
#   panel.grid.major=element_line(colour="darkgrey", size=0.2),
#   panel.grid.minor=element_line(colour="darkgrey", size=0.1),
#   legend.background=element_rect(fill="transparent"),
#   #panel.spacing.x=unit(3, "lines"),
#   plot.margin=unit(c(0.1, 0.6, 0.1, 0.1), "lines") )
#
# # Plot catch by year and gear type (i.e., period)
# PlotCatch <- function( SARs, dat, fn, minYr=NA ){
#   # Range for x-axes
#   rangeX <- yrRange
#   # Get stocks of interest
#   df <- dat %>%
#     filter( Region%in%SARs )
#   # If trimming years
#   if( !is.na(minYr) ) {
#     # Filter data
#     df <- filter( df, Year >= minYr )
#     # Trim x axes
#     rangeX <- rangeX[rangeX >= minYr]
#   }  # End if trimming years
#   # Base plot
#   basePlot <- ggplot( data=df, aes(x=Year, y=Catch) ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     # scale_y_continuous( labels=comma ) +
#     expand_limits( x=rangeX ) +
#     scale_fill_grey( start=0, end=0.8 ) +
#     myTheme +
#     theme( legend.position="top" )
#   # If english
#   if( plotEng ) {
#     # Plot catch: english
#     plotCatch <- basePlot +
#       geom_bar( stat="identity", position="stack", aes(fill=Gear), width=1 ) +
#       labs( y=expression(paste("Catch (t"%*%10^3, ")", sep="")) )  +
#       facet_wrap( ~ RegionName, ncol=2, dir="v", scales="free_y" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25+0.75 )
#   } else {  # End if english, otherwise
#     # Plot catch: french
#     plotCatchFR <- basePlot +
#       geom_bar( stat="identity", position="stack", aes(fill=Engin), width=1 ) +
#       labs( x="Ann\u{00E9}e", y=expression(paste("Prises (t"%*%10^3, ")", sep="")) )  +
#       facet_wrap( ~ RegionNameFR, ncol=2, dir="v", scales="free_y" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25+0.75 )
#   }  # End if french
# }  # End PlotCatch
#
# # Catch for major regions
# PlotCatch( SARs=allRegions$major, dat=catch, fn="CatchMajor" )
#
# # Catch for minor regions
# PlotCatch( SARs=allRegions$minor, dat=catch, fn="CatchMinor",
#   minYr=firstYrMinor )
#
# # Plot SOK harvest by year
# PlotSOK <- function( SARs, dat, fn, minYr=NA ){
#   # Range for x-axes
#   rangeX <- yrRange
#   # Get stocks of interest
#   df <- dat %>%
#     filter( Region%in%SARs )
#   # If trimming years
#   if( !is.na(minYr) ) {
#     # Filter data
#     df <- filter( df, Year >= minYr )
#     # Trim x axes
#     rangeX <- rangeX[rangeX >= minYr]
#   }  # End if trimming years
#   # Plot all the regions together
#   SOKPlotAll <- ggplot( data=df, aes(x=Year, y=Biomass) ) +
#     geom_bar( stat="identity", position="stack", width=1 ) +
#     labs( y="Biomass (t)" )  +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     scale_y_continuous( labels=comma ) +
#     expand_limits( x=rangeX ) +
#     facet_wrap( ~ RegionName, ncol=2, dir="v", scales="free_y" ) +
#     myTheme +
#     theme( legend.position="top" ) +
#     ggsave( filename=paste0(fn, ".png"), dpi=pDPI, width=figWidth,
#       height=ceiling(length(SARs)/2)*2.25+0.75 )
# }  # End PlotSOK function
#
# # # Plot SOK (major SARs)
# # PlotSOK( SARs=allRegions$major, dat=harvSOK, fn=file.path(rootd.figs, "SOKMajor"))
# #
# # # Plot SOK (minor SARs)
# # PlotSOK( SARs=allRegions$minor, dat=harvSOK, fn=file.path(rootd.figs, "SOKMinor"), minYr=firstYrMinor )
#
# # Plot weight-at-age by year
# PlotWeightAge <- function( SARs, dat, fn, minYr=NA ){
#   # Range for x-axes
#   rangeX <- yrRange
#   # Get stocks of interest
#   df <- dat %>%
#     filter( Region%in%SARs )
#   # If trimming years
#   if( !is.na(minYr) ) {
#     # Filter data
#     df <- filter( df, Year >= minYr )
#     # Trim x axes
#     rangeX <- rangeX[rangeX >= minYr]
#   }  # End if trimming years
#   # Base plot
#   basePlot <- ggplot( data=df ) +
#     geom_line( aes(x=Year, y=muWeight, group=Age) ) +
#     geom_point( data=filter(.data=df, Age == ageShow), aes(x=Year, y=Weight),
#       shape=1, size=1 ) +
#     geom_line( data=filter(.data=df, Age == ageShow),
#       aes(x=Year, y=muWeight), size=1 ) +
#     expand_limits( x=rangeX ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     coord_cartesian( ylim=c(min(df$muWeight, na.rm=TRUE), wtMax) ) +
#     myTheme
#   # If english
#   if( plotEng ) {
#     # Plot weight-at-age: english
#     plotWeight <- basePlot +
#       labs( y="Weight-at-age (kg)" ) +
#       facet_wrap( ~ RegionName, ncol=2, dir="v" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25 )
#   } else {  # End if english, otherwise
#     # Plot weight-at-age: french
#     plotWeightFR <- basePlot +
#       labs( x="Ann\u{00E9}e", y="Poids \u{00E0} l'\u{00E2}ge (kg)" ) +
#       facet_wrap( ~ RegionNameFR, ncol=2, dir="v" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25 )
#   }  # End if french
# }  # End PlotWeightAge function
#
# # Weight-at-age for major regions
# PlotWeightAge( SARs=allRegions$major, dat=muWeightAge, fn="WeightAgeMajor" )
#
# # Weight-at-age for minor regions
# PlotWeightAge( SARs=allRegions$minor, dat=muWeightAge, fn="WeightAgeMinor",
#   minYr=firstYrMinor )
#
# # Plot proportion-at-age
# PlotPropAge <- function( SARs, dat1, dat2, fn, minYr=NA ) {
#   # Range for x-axes
#   rangeX <- yrRange
#   # Get stocks of interest
#   df1 <- dat1 %>%
#     filter( Region%in%SARs ) %>%
#     mutate( Age=as.numeric(Age) )
#   # Get stocks of interest
#   df2 <- dat2 %>%
#     filter( Region%in%SARs )
#   # If trimming years
#   if( !is.na(minYr) ) {
#     # Filter data
#     df1 <- filter( df1, Year >= minYr )
#     # Filter data
#     df2 <- filter( df2, Year >= minYr )
#     # Trim x axes
#     rangeX <- rangeX[rangeX >= minYr]
#   }  # End if trimming years
#   # Base plot
#   basePlot <- ggplot( data=df1, aes(x=Year)  ) +
#     geom_point( aes(y=Age, size=ifelse(Proportion==0, NA, Proportion)) ) +
#     geom_path( data=df2, aes(y=MeanAge, group=GroupID) ) +
#     geom_ribbon( data=df2, aes(ymin=Lower, ymax=Upper, group=GroupID),
#       alpha=0.25 ) +
#     labs( size="Proportion" ) +
#     scale_size( range=c(0, 2) ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     scale_y_continuous( breaks=pretty_breaks() ) +
#     expand_limits( rangeX ) +
#     myTheme +
#     theme( legend.position="top" )
#   # If english
#   if( plotEng ) {
#     # Plot proportion-at-age: english
#     plotProp <- basePlot +
#       facet_wrap( ~ RegionName, ncol=2, dir="v" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25+0.75 )
#   } else {  # End if english, otherwise
#     # Plot proportion-at-age: french
#     plotPropFR <- basePlot +
#       labs( x="Ann\u{00E9}e", y="\u{00C2}ge" ) +
#       facet_wrap( ~ RegionNameFR, ncol=2, dir="v" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25+0.75 )
#   }  # End if french
# }  # End PlotPropAge function
#
# # Proportion-at-age for major regions
# PlotPropAge( SARs=allRegions$major, dat1=numAgedYear, dat2=qAgedYear,
#   fn="PropAgeMajor" )
#
# # Proportion-at-age for minor regions
# PlotPropAge( SARs=allRegions$minor, dat1=numAgedYear, dat2=qAgedYear,
#   fn="PropAgeMinor", minYr=firstYrMinor )
#
# # Plot total spawn index by year
# PlotSpawn <- function( SARs, dat, fn, minYr=NA ){
#   # Range for x-axes
#   rangeX <- yrRange
#   # Get stocks of interest
#   df <- dat %>%
#     filter( Region%in%SARs )
#   # If trimming years
#   if( !is.na(minYr) ) {
#     # Filter data
#     df <- filter( df, Year >= minYr )
#     # Trim x axes
#     rangeX <- rangeX[rangeX >= minYr]
#   }  # End if trimming years
#   # Base plot
#   basePlot <- ggplot( data=df, aes(x=Year, y=Spawn) ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     scale_y_continuous( labels=comma ) +
#     scale_shape_manual( values=c(1, 2) ) +
#     geom_vline( xintercept=newSurvYr-0.5, linetype="dashed", size=0.25 ) +
#     expand_limits( y=0, x=rangeX ) +
#     myTheme +
#     theme( legend.position="top" )
#   # If english
#   if( plotEng ) {
#     # Plot spawn index: english
#     plotSpawn <- basePlot +
#       geom_point( aes(shape=Survey) ) +
#       geom_line( aes(group=Survey) ) +
#       labs( shape="Survey period",
#         y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) )  +
#       facet_wrap( ~ RegionName, ncol=2, dir="v", scales="free_y" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25+0.75 )
#   } else {  # End if english, otherwise
#     # Plot spawn index: french
#     plotSpawnFR <- basePlot +
#       geom_point( aes(shape=Releve) ) +
#       geom_line( aes(group=Releve) ) +
#       labs( shape="P\u{00E9}riode du relev\u{00E9}", x="Ann\u{00E9}e",
#         y=expression(paste("Indice du frai (t"%*%10^3, ")", sep="")) )  +
#       facet_wrap( ~ RegionNameFR, ncol=2, dir="v", scales="free_y" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25+0.75 )
#   }  # End if french
# }  # End PlotSpawn function
#
# # Spawn index for major regions
# PlotSpawn( SARs=allRegions$major, dat=spawn, fn="SpawnIndexMajor" )
#
# # Spawn index for minor regions
# PlotSpawn( SARs=allRegions$minor, dat=spawn, fn="SpawnIndexMinor",
#   minYr=firstYrMinor )
#
# # Plot number aged by year
# PlotNumber <- function( SARs, dat, fn, minYr=NA ){
#   # Range for x-axes
#   rangeX <- yrRange
#   # Get stocks of interest
#   df <- dat %>%
#     filter( Region%in%SARs )
#   # If trimming years
#   if( !is.na(minYr) ) {
#     # Filter data
#     df <- filter( df, Year >= minYr )
#     # Trim x axes
#     rangeX <- rangeX[rangeX >= minYr]
#   }  # End if trimming years
#   # Plot all the regions together
#   numberAgePlotAll <- ggplot( data=df, aes(x=Year, y=Number, group=Year) ) +
#     geom_bar( stat="identity", width=1 ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     scale_y_continuous( labels=comma ) +
#     expand_limits( x=rangeX ) +
#     facet_wrap( ~ RegionName, ncol=2, dir="v" ) +
#     myTheme +
#     ggsave( filename=fn, dpi=pDPI, width=figWidth,
#       height=ceiling(length(SARs)/2)*2.25 )
# }  # End PlotNumber function
#
# # # Plot number aged (major SARs)
# # PlotNumber( SARs=allRegions$major, dat=numAgedYear, fn=file.path(rootd.figs, "NumberAgedMajor.png"))
# #
# # # Plot number aged (minor SARs)
# # PlotNumber( SARs=allRegions$minor, dat=numAgedYear, fn=file.path(rootd.figs, "NumberAgedMinor.png"),
# #   minYr=firstYrMinor )
#
# # Storyboard plot: 6 panels (abundance, recruitment, M, SSB with catch,
# # recruitment deviations, and production)
# PlotStoryboard <- function( SARs, si, qp, rec, recDev, M, SSB, C, bp ) {
#   # List to hold biomas plots
#   lBiomass <- list()
#   lBiomass2 <- list()
#   # Counter for biomass plots
#   iCount <- 0
#   # Fixed cut-offs (1996; only apply to AM2)
#   cutOffs <- tibble( Model="AM2", Region=names(fixedCutoffs),
#     Cutoff=unlist(fixedCutoffs) ) %>%
#     complete( Region=names(fixedCutoffs) )
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Update the counter
#     iCount <- iCount + 1
#     # Get region name
#     SAR <- SARs[k]
#     # Default point size
#     pSize <- 0.75
#     # Defauly line thickness
#     lSize <- 0.5
#     # Need to extend the year range by 1
#     rangeX <- c( min(yrRange-1), max(yrRange)+1 )
#     # Data wrangling: abundance
#     siSub <- si %>%
#       mutate( Survey=as.character(Survey) ) %>%
#       filter( Region==SARs[k] ) %>%
#       full_join( y=filter(qp, Region==SARs[k]),
#         by=c("Region", "Survey") ) %>%
#       mutate( Abundance=Spawn/Median )
#     # Data wrangling: recruitment
#     recSub <- rec %>%
#       filter( Region==SARs[k] )
#     # Data wrangling: recruitment deviations
#     recDevSub <- recDev %>%
#       filter( Region==SARs[k] ) %>%
#       mutate( RunMean=rollmean(x=Median, k=nRollDev, align="right",
#         na.pad=TRUE ) )
#     # Data wrangling: natural mortality
#     MSub <- M %>%
#       filter( Region==SARs[k] )
#     # Data wrangling: spawning biomass
#     SSBSub <- SSB %>%
#       filter( Region==SARs[k] )
#     # Data wrangling: catch
#     CSub <- C %>%
#       filter( Catch > 0, Region==SARs[k] )
#     # Wrangle catch for production
#     pCatch <- CSub %>%
#       group_by( Year ) %>%
#       summarise( Catch=SumNA(Catch) ) %>%
#       ungroup( ) %>%
#       complete( Year=yrRange, fill=list(Catch=0) )
#     # Wrangle spawning biomass for production
#     pSSB <- SSBSub %>%
#       filter( Year %in% yrRange ) %>%
#       dplyr::select( Region, Year, Median, Survey ) %>%
#       rename( SSB=Median )
#     # Combine spawning biomass and catch, and calculate production
#     pDat <- full_join( x=pSSB, y=pCatch, by="Year" ) %>%
#       mutate(
#         # Next year's SSB
#         SSBNextYr=lead(SSB, n=1),
#         # Next year's Catch
#         CatchNextYr=lead(Catch, n=1),
#         # Surplus production
#         Production=SSBNextYr-SSB+CatchNextYr,
#         # Surplus production rate
#         ProdRate=Production/SSB ) %>%
#       na.omit( ) %>%
#       filter( Survey == "Dive" )
#     # Data wrangling: LRP = SB_0 * propB0
#     LRP <- bp %>%
#       filter( Parameter=="SB0", Region==SARs[k] ) %>%
#       mutate( Estimate=Median, Lower=Lower*propB0, Median=Median*propB0,
#         Upper=Upper*propB0 )
#     # Get 'high productivity' years -- kinda clunky
#     hpYrs <- hiProdYrs[which(names(hiProdYrs)==SAR)][[1]]
#     # Calcuate USR: average SB
#     USRa <- SSBSub %>%
#       summarise( Lower=mean(Lower), Median=mean(Median),
#         Upper=mean(Upper) ) %>%
#       mutate( Value="bar(italic('SB'))" )
#     # Calcuate USR: average SB in high productivity years
#     USRb <- SSBSub %>%
#       filter( Year %in% hpYrs ) %>%
#       summarise( Lower=mean(Lower), Median=mean(Median),
#         Upper=mean(Upper) ) %>%
#       mutate( Value="bar(italic('SB'))[prod]" )
#     # Calcuate USR: multiplyer of LRP
#     USRc <- LRP %>%
#       transmute( Lower=Lower*multLRP, Median=Median*multLRP,
#         Upper=Upper*multLRP ) %>%
#       mutate( Value=paste(multLRP, "*LRP") )
#     # Calcuate USR: B0
#     USRd <- bp %>%
#       filter( Parameter=="SB0", Region==SARs[k] ) %>%
#       dplyr::select( Lower, Median, Upper ) %>%
#       mutate( Value="italic('SB')[0]" )
#     # Combine the USRs
#     USRs <- bind_rows( USRa, USRb, USRc, USRd )
#     # Data wrangling: SB_projected
#     SBProjSub <- bp %>%
#       filter( Parameter=="SBProj", Region==SARs[k] )
#     # Data wranging: cutoffs
#     coSub <- cutOffs %>%
#       filter( Region==SARs[k] )
#     # Make a depletion table (with other things) - remove na.omit from
#     # pDat above
#     MakeDepletionTable <- function( si, p, st=newSurvYr ) {
#       # Production
#       pOut <- p %>%
#         dplyr::select( Year, SSB, Catch, Production, ProdRate ) %>%
#         rename( Biomass=SSB, ProductionRate=ProdRate ) %>%
#         mutate( Depletion=Biomass/LRP$Estimate,
#           HarvestRate=Catch/(Biomass+Catch)) %>%
#         filter( Year >= st )
#       # Spawn index
#       siOut <- si %>%
#         dplyr::select( Year, Survey, Spawn, Median ) %>%
#         rename( SpawnIndex=Spawn, q=Median ) %>%
#         mutate( SpawnIndexQ=SpawnIndex/q ) %>%
#         filter( Year >= st )
#       # Combine tables
#       res <- pOut %>%
#         full_join( y=siOut, by="Year" ) %>%
#         dplyr::select( Year, Survey, Biomass, Depletion, SpawnIndex,
#           SpawnIndexQ, Catch, HarvestRate, Production, ProductionRate ) %>%
#         mutate_if( is.numeric, round, 2 )
#       # Return results
#       return( res )
#     }  # End MakeDepletionTable
#     # Create the table
#     # dTable <- MakeDepletionTable( si=siSub, p=pDat ) %>%
#     #   write_csv( path=paste("Depletion", SAR, ".csv", sep="") )
#     # Plot a: abundance
#     plotA <- ggplot( data=siSub, aes(x=Year, y=Abundance) ) +
#       # geom_hline( yintercept=LRP$Median, colour="red" ) +
#       # annotate( geom="rect", xmin=-Inf, xmax=Inf, ymin=LRP$Lower,
#       #   ymax=LRP$Upper, colour="transparent", fill="red", alpha=0.3 ) +
#       # geom_hline( yintercept=coSub$Cutoff, colour="blue" ) +
#       geom_point( aes(shape=Survey), size=pSize ) +
#       geom_line( data=SSBSub, aes(x=Year, y=Median, group=Survey),
#         size=lSize ) +
#       expand_limits( x=rangeX, y=0 ) +
#       labs( x=NULL,
#         y=expression(paste("Scaled abundance (t"%*%10^3,")", sep="")) ) +
#       scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#       scale_shape_manual( values=c(2, 1) ) +
#       guides( shape=FALSE, linetype=FALSE ) +
#       annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3,
#         hjust=-0.1, size=2.5 ) +
#       myTheme +
#       theme( axis.text.x=element_blank(), text=element_text(size=8),
#         axis.text=element_text(size=8) )
#     # French version
#     if( !plotEng )
#       plotA <- plotA +
#       labs( y=expression(
#         paste("Abondance mise \u{00E0} l'\u{00E9}chelle (t"%*%10^3,")",
#           sep="")) )
#     # Plot b: natural mortality
#     plotB <- ggplot( data=MSub, aes(x=Year, y=Median) ) +
#       geom_ribbon( aes(ymin=Lower, ymax=Upper), alpha=0.5 ) +
#       geom_line( size=lSize ) +
#       expand_limits( x=rangeX, y=0 ) +
#       # TODO: Add units (yr^-1)
#       labs( x=NULL,
#         y=expression(paste("Instantaneous natural mortality (yr"^-1,")",
#           sep="")) ) +
#       scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#       annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3,
#         hjust=-0.1, size=2.5 ) +
#       myTheme +
#       theme( axis.text.x=element_blank(), text=element_text(size=8),
#         axis.text=element_text(size=8) )
#     # French version
#     if( !plotEng )
#       plotB <- plotB +
#       labs( y=expression(
#         paste("Mortalit\u{00E9} naturelle instantan\u{00E9}e (yr"^-1,")",
#           sep="")) )
#     # Plot c: recruitment
#     plotC <- ggplot( data=recSub, aes(x=Year, y=Median) ) +
#       geom_point( size=pSize ) +
#       geom_errorbar( aes(ymin=Lower, ymax=Upper), size=lSize/2, width=0 ) +
#       expand_limits( x=rangeX, y=0 ) +
#       labs( x=NULL, y=paste("Number of age-", ageRec,
#         " recruits (1,000 millions)",  sep="") ) +
#       scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#       scale_y_continuous( labels=function(x) x/1000 ) +
#       annotate( geom="text", x=-Inf, y=Inf, label="(c)", vjust=1.3,
#         hjust=-0.1, size=2.5 ) +
#       myTheme +
#       theme( axis.text.x=element_blank(), text=element_text(size=8),
#         axis.text=element_text(size=8) )
#     # French version
#     if( !plotEng )
#       plotC <- plotC +
#       labs( y="Nombre de recrues\nd'\u{00E2}ge 2 (en 1,000 millions)" )
#     # Temprary plot: spawning biomass and catch
#     plotTemp <- ggplot( data=USRs ) +
#       geom_hline( yintercept=LRP$Median, colour="red" ) +
#       annotate( geom="rect", xmin=-Inf, xmax=Inf, ymin=LRP$Lower,
#         ymax=LRP$Upper, colour="transparent", fill="red", alpha=0.3 ) +
#       # geom_hline( yintercept=coSub$Cutoff, colour="blue" ) +
#       geom_bar( data=CSub, aes(x=Year, y=Catch), stat="identity",
#         width=lSize, fill="black" ) +
#       geom_ribbon( data=SSBSub, aes(x=Year, ymin=Lower, ymax=Upper), alpha=0.5 ) +
#       geom_line( data=SSBSub, aes(x=Year, y=Median), size=lSize ) +
#       geom_point( data=SBProjSub, aes(x=Year, y=Median), size=pSize ) +
#       geom_errorbar( data=SBProjSub, aes(x=Year, ymin=Lower, ymax=Upper),
#         size=lSize/2, width=0 ) +
#       expand_limits( x=rangeX, y=0 ) +
#       labs( x=NULL, y=expression(paste("Spawning biomass (t"%*%10^3,")",
#         sep="")) ) +
#       scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#       myTheme +
#       theme( axis.text.x=element_blank(), text=element_text(size=8),
#         axis.text=element_text(size=8) )
#     # Put the plot in the list
#     lBiomass[[iCount]] <- plotTemp +
#       annotate( geom="text", x=-Inf, y=Inf,
#         label=paste(SAR, sep=" "), vjust=1.3, hjust=-0.1,
#         size=2.5 )
#     # Temprary plot (2): spawning biomass and catch
#     plotTemp2 <- ggplot( data=USRs ) +
#       geom_hline( yintercept=LRP$Median, colour="red" ) +
#       annotate( geom="rect", xmin=-Inf, xmax=Inf, ymin=LRP$Lower,
#         ymax=LRP$Upper, colour="transparent", fill="red", alpha=0.3 ) +
#       geom_hline( yintercept=USRa$Median, colour="blue", linetype="dashed",
#         size=lSize ) +
#       geom_hline( yintercept=USRb$Median, colour="blue", linetype="dotted",
#         size=lSize ) +
#       geom_hline( yintercept=USRc$Median, colour="blue", linetype="dotdash",
#         size=lSize ) +
#       annotate( geom="pointrange", x=min(yrRange)-1, y=USRd$Median,
#         ymin=USRd$Lower, ymax=USRd$Upper, size=lSize/2 ) +
#       geom_bar( data=CSub, aes(x=Year, y=Catch), stat="identity",
#         width=lSize, fill="black" ) +
#       geom_ribbon( data=SSBSub, aes(x=Year, ymin=Lower, ymax=Upper), alpha=0.5 ) +
#       geom_line( data=SSBSub, aes(x=Year, y=Median), size=lSize ) +
#       expand_limits( x=rangeX, y=0 ) +
#       labs( x="Year",
#         y=expression(paste("Estimated spawning biomass (t"%*%10^3,")",
#           sep="")) ) +
#       scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#       facet_grid( Region ~ . ) +
#       myTheme +
#       theme( text=element_text(size=8), axis.text=element_text(size=8) )
#     # Put the plot in the list
#     lBiomass2[[iCount]] <- plotTemp2
#     # Plot d: spawning biomass and catch
#     plotD <- plotTemp +
#       annotate( geom="text", x=-Inf, y=Inf, label="(d)", vjust=1.3,
#         hjust=-0.1, size=2.5 )
#     # French version
#     if( !plotEng )
#       plotD <- plotD +
#       labs( y=expression(paste("Biomass du stock reproducteur (t"%*%10^3,")",
#         sep="")) )
#     # Plot e: stock-recruitment deviations
#     plotE <- ggplot( data=recDevSub, aes(x=Year, y=Median) ) +
#       geom_point( size=pSize ) +
#       geom_line( aes(y=RunMean), colour="red", size=lSize ) +
#       geom_errorbar( aes(ymin=Lower, ymax=Upper), size=lSize/2, width=0 ) +
#       expand_limits( x=rangeX, y=0 ) +
#       geom_hline( yintercept=0, linetype="dashed" ) +
#       labs( y="Log recruitment deviations" ) +
#       scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#       scale_y_continuous( labels=comma ) +
#       annotate( geom="text", x=-Inf, y=Inf, label="(e)", vjust=1.3,
#         hjust=-0.1, size=2.5 ) +
#       myTheme +
#       theme( text=element_text(size=8), axis.text=element_text(size=8) )
#     # French version
#     if( !plotEng )
#       plotE <- plotE +
#       labs( x="Ann\u{00E9}e", y="Log des \u{00E9}carts du recruitment" )
#     # Plot f: surplus production
#     plotF <- ggplot( data=pDat, aes(x=SSB, y=Production) ) +
#       geom_vline( xintercept=LRP$Median, colour="red" ) +
#       annotate( geom="rect", xmin=LRP$Lower, xmax=LRP$Upper, ymin=-Inf,
#         ymax=Inf, colour="transparent", fill="red", alpha=0.3 ) +
#       # geom_vline( xintercept=coSub$Cutoff, colour="blue" ) +
#       geom_point( aes(shape=Year==max(yrRange)-1, colour=Year), size=2 ) +
#       geom_point( data=filter(pDat, Year==max(yrRange)-1), shape=24,
#         colour="black", fill="white") +
#       geom_text_repel( aes(label=Year), segment.colour="grey", size=2 ) +
#       geom_path( size=0.4 ) +
#       geom_hline( yintercept=0, linetype="dashed" ) +
#       expand_limits( x=0 ) +
#       scale_colour_gradient( low="lightgrey", high="black" ) +
#       labs( x=expression(paste("Spawning biomass (t"%*%10^3, ")", sep="")),
#         y=expression(paste("Spawning biomass production (t"%*%10^3, ")",
#           sep="")) ) +
#       guides( colour=FALSE, shape=FALSE ) +
#       scale_y_continuous( labels=comma ) +
#       # scale_x_continuous( labels=comma,
#       #   sec.axis=sec_axis(~./LRP$Estimate) ) +
#       annotate( geom="text", x=-Inf, y=Inf, label="(f)", vjust=1.3,
#         hjust=-0.1, size=2.5 ) +
#       myTheme +
#       theme( text=element_text(size=8), axis.text=element_text(size=8) )
#     # French version
#     if( !plotEng )
#       plotF <- plotF +
#       labs( x=expression(paste("Biomass du stock reproducteur (t"%*%10^3, ")",
#         sep="")),
#         y=expression(
#           paste("Production de la biomass\ndu stock reproducteur (t"%*%10^3, ")",
#             sep="")) )
#     # # Plot f: surplus production rate
#     # plotF <- ggplot( data=pDat, aes(x=SSB, y=ProdRate) ) +
#     #   geom_vline( xintercept=LRP$Median, colour="red" ) +
#     #   annotate( geom="rect", xmin=LRP$Lower, xmax=LRP$Upper, ymin=-Inf,
#     #     ymax=Inf, colour="transparent", fill="red", alpha=0.3 ) +
#     #   # geom_vline( xintercept=coSub$Cutoff, colour="blue" ) +
#     #   geom_point( aes(shape=Year==max(yrRange)-1, colour=Year), size=3 ) +
#     #   geom_point( data=filter(pDat, Year==max(yrRange)-1), shape=24,
#     #     colour="black", fill="white") +
#     #   geom_text_repel( aes(label=Year), segment.colour="grey", size=2 ) +
#     #   geom_path( size=0.4 ) +
#     #   geom_hline( yintercept=0, linetype="dashed" ) +
#     #   expand_limits( x=0 ) +
#     #   scale_colour_gradient( low="lightgrey", high="black" ) +
#     #   labs( x=expression(paste("Spawning biomass (t"%*%10^3, ")", sep="")),
#     #     y="Spawning biomass production rate" ) +
#     #   guides( colour=FALSE, shape=FALSE ) +
#     #   scale_y_continuous( labels=comma ) +
#     #   scale_x_continuous( labels=comma,
#     #     sec.axis=sec_axis(~./LRP$Estimate) ) +
#     #   annotate( geom="text", x=-Inf, y=Inf, label="(f)", vjust=1.3,
#     #     hjust=-0.1, size=2.5 ) +
#     #   myTheme +
#     #   theme( text=element_text(size=8) )
#     # Combine the plots
#     storyboard <- plot_grid( plotA, plotB, plotC, plotD, plotE, plotF,
#       align="v", ncol=2, nrow=3, rel_heights=c(1.0, 1.0, 1.1) ) +
#       ggsave( filename=file.path(rootd.figs,
#         paste("Storyboard", SAR, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=figWidth )
#     # Make a second set of plots for USRs
#     # plotUSR <- plotTemp +
#     #   geom_hline( aes(yintercept=Median), colour="green" ) +
#     #   #          geom_rect( aes(xmin=-Inf, xmax=Inf, ymin=Lower,
#     #   #                  ymax=Upper), colour="transparent", fill="green", alpha=0.3 ) +
#     #   facet_wrap( ~ Value, labeller=label_parsed ) +
#     #   theme( axis.text.x=element_text(size=8) ) +
#     #   ggsave(filename=file.path(rootd.figs, paste0("USRs", SAR, ".png")),
#     #     dpi=pDPI, width=figWidth, height=figWidth*0.67 )
#   }  # End k loop over regions
#   # # Combine the plots
#   # biomassPlots <- plot_grid( lBiomass[[1]], lBiomass[[2]], lBiomass[[3]],
#   #   lBiomass[[4]], lBiomass[[5]], align="hv", ncol=2 ) +
#   #   ggsave( filename=file.path(rootd.figs, "Biomass.png"), dpi=pDPI, width=figWidth,
#   #     height=figWidth )
#   # biomassPlots2 <- plot_grid( lBiomass2[[1]], lBiomass2[[2]], lBiomass2[[3]],
#   #   lBiomass2[[4]], lBiomass2[[5]], align="hv", ncol=2 ) +
#   #   ggsave( filename=file.path(rootd.figs, "Biomasssm.png"), dpi=pDPI, width=figWidth,
#   #     height=figWidth )
# }  # End PlotStoryboard function
#
# # spBio %>%
# #   filter( Region%in%allRegions$major ) %>%
# #   write_csv( path="SpawningBiomass.csv" )
#
# # catch %>%
# #   filter( Region%in%allRegions$major, Catch>0 ) %>%
# #   group_by(Region, Year) %>%
# #   summarise( Catch=sum(Catch) ) %>%
# #   ungroup() %>%
# #   write_csv( path="Catch.csv" )
#
# # Make the storyboard (major SARs only)
# PlotStoryboard( SARs=allRegions$major, si=spawn, qp=qPars, rec=recruits,
#   recDev=recruitDev, M=natMort, SSB=spBio, C=catch, bp=bPars )
#
# # Plot effective harvest rate
# PlotHarvRate <- function( SARs, dat, fn ) {
#   # Filter for desired regions and areas
#   datSub <- dat %>%
#     filter( Region %in% SARs )
#   # Base plot
#   basePlot <- ggplot( data=datSub, aes(x=Year) ) +
#     geom_ribbon( aes(ymin=LowerHR, ymax=UpperHR), fill="grey" ) +
#     geom_line( aes(y=MedianHR) ) +
#     annotate( geom="segment", x=intendUYrs, y=intendU, xend=max(yrRange),
#       yend=intendU, linetype="dashed" ) +
#     expand_limits( y=c(0, 1) ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     myTheme
#   # If english
#   if( plotEng ) {
#     # Plot harvest rate: english
#     plotHarv <- basePlot +
#       facet_wrap( ~ RegionName, ncol=2, dir="v" ) +
#       labs( y="Effective harvest rate" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25 )
#   } else {  # End if english, otherwise
#     # Plot harvest rate: french
#     plotHarvFR <- basePlot +
#       facet_wrap( ~ RegionNameFR, ncol=2, dir="v" ) +
#       labs( x="Ann\u{00E9}e", y="Taux de r\u{00E9}colte effectif" ) +
#       ggsave( filename=file.path(rootd.figs, paste(fn, ".png", sep="")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25 )
#   }  # End if french
# }  # End PlotHarvRate function
#
# # Plot harvest rate: major SARs
# PlotHarvRate( SARs=allRegions$major, dat=harvRate, fn="HarvestRate")
#
# # Plot distribution of spawning biomass, and the LRP
# PlotSSB <- function( SARs, models, SSB, SB0, probs=ciLevel ) {
#   # Get the year
#   yr <- unique( SSB$Year )
#   # Get lower CI level
#   lo <- (1 - probs) / 2
#   # Get upper CI level
#   up <- 1 - lo
#   # # Fixed cut-offs (1996; only apply to AM2)
#   # cutOffs <- tibble( Region=names(fixedCutoffs),
#   #   Cutoff=unlist(fixedCutoffs) ) %>%
#   #   complete( Region=names(fixedCutoffs) ) %>%
#   #   mutate( Region=factor(Region, levels=regions$Region) ) %>%
#   #   filter( Region %in% SARs )
#   # Update SSB
#   SSB <- SSB %>%
#     mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#       RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR) ) %>%
#     filter( Region %in% SARs )
#   # Calculate LRP
#   LRP <- SB0 %>%
#     mutate( Lower=Lower*propB0, Median=Median*propB0, Upper=Upper*propB0 ) %>%
#     left_join( y=regions, by="Region" ) %>%
#     mutate( RegionName=factor(RegionName, levels=regions$RegionName),
#       RegionNameFR=factor(RegionNameFR, levels=regions$RegionNameFR) ) %>%
#     filter( Region %in% SARs )
#   # SSB quantiles
#   quantSSB <- SSB %>%
#     group_by( Region, RegionName, RegionNameFR, Model ) %>%
#     summarise( Lower=quantile(Value, probs=lo),
#       Median=quantile(Value, probs=0.5),
#       Upper=quantile(Value, probs=up) ) %>%
#     ungroup( ) %>%
#     filter( Region %in% SARs )
#   # Base plot
#   basePlot <- ggplot( data=SSB ) +
#     geom_density( aes(x=Value), fill="grey" ) +
#     #      geom_vline( data=LRP, aes(xintercept=Lower), colour="red",
#     #          linetype="dashed" ) +
#     geom_vline( data=LRP, aes(xintercept=Median), colour="red" ) +
#     geom_rect( data=LRP, aes(xmin=Lower, xmax=Upper, ymin=-Inf, ymax=Inf),
#       colour="transparent", fill="red", alpha=0.3 ) +
#     #      geom_vline( data=LRP, aes(xintercept=Upper), colour="red",
#     #          linetype="dashed" ) +
#     # geom_vline( data=cutOffs, aes(xintercept=Cutoff), colour="blue" ) +
#     geom_vline( data=quantSSB, aes(xintercept=Lower), linetype="dashed" ) +
#     geom_vline( data=quantSSB, aes(xintercept=Median) ) +
#     geom_vline( data=quantSSB, aes(xintercept=Upper), linetype="dashed" ) +
#     myTheme
#   # If english
#   if( plotEng ) {
#     # Plot SSB: english
#     plotSSB <- basePlot +
#       facet_wrap( ~ RegionName, scales="free", ncol=2, dir="v" ) +
#       labs( x=bquote(italic("SB")[.(yr)]~" (t"%*%10^3*")"), y="Density" ) +
#       ggsave( filename=file.path(rootd.figs, paste0("SSB", yr, ".png")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25 )
#   } else {  # End if english, otherwise
#     # Plot SSB: french
#     plotSSB <- basePlot +
#       facet_wrap( ~ RegionNameFR, scales="free", ncol=2, dir="v" ) +
#       labs( x=bquote(italic("BSR")[.(yr)]~" (t"%*%10^3*")"), y="Densit\u{00E9}" ) +
#       ggsave( filename=file.path(rootd.figs, paste0("SSB", yr, ".png")),
#         dpi=pDPI, width=figWidth, height=ceiling(length(SARs)/2)*2.25 )
#   }  # End if french
# }  # End PlotSSB function
#
# # Show forecast SSB
# PlotSSB( SARs=allRegions$major, SSB=spBioValsForecast,
#   SB0=filter(bPars, Parameter=="SB0") )
#
# # Plot Beverton-Holt stock-recruitment relationship
# PlotBevertonHolt <- function( bh, bhPred, SARs, models ) {
#   # Filter for desired regions and areas
#   bhSub <- bh %>%
#     filter( Region %in% SARs )
#   # Filter for desired regions and areas
#   bhPredSub <- bhPred %>%
#     filter( Region %in% SARs ) %>%
#     # TODO: Confirm if we should use this, or the "uncorrected" ro for points
#     mutate( ro2=ro*exp(-0.5*tau^2) )
#   # The plot
#   plotBH <- ggplot( data=bhSub, aes(x=Abundance, y=Recruitment) ) +
#     geom_point( aes(colour=Year, shape=Year==max(yrRange)) ) +
#     geom_point( data=filter(bhSub, Year==max(yrRange)), shape=24,
#       colour="black", fill="white") +
#     geom_point( data=bhPredSub, aes(x=sbo, y=ro), shape=8 ) +
#     geom_line( data=bhPredSub ) +
#     #      geom_text_repel( aes(label=Year), segment.colour="grey", size=2 ) +
#     #      geom_path( size=0.4 ) +
#     scale_colour_gradient( low="lightgrey", high="black" ) +
#     facet_wrap( ~ RegionName, ncol=2, scales="free", dir="v" ) +
#     labs( x=expression(paste("Spawning biomass (t"%*%10^3, ")")),
#       y=paste("Number of age-", ageRec, " recruits (millions)", sep="") ) +
#     scale_y_continuous( labels=comma ) +
#     expand_limits( x=0, y=0 ) +
#     guides( colour=FALSE, shape=FALSE ) +
#     myTheme +
#     ggsave( filename=file.path(rootd.figs, "BevertonHolt.png"), dpi=pDPI, width=figWidth,
#       height=figWidth )
# }  # End PlotBevertonHolt function
#
# # Plot Beverton-Holt
# PlotBevertonHolt( bh=filter(BevHolt, Model==mNames[1]), SARs=allRegions$major,
#   bhPred=filter(predBH, Model==mNames[1]) )
#
# # Plot unfished spawning biomass
# PlotSB0 <- function( dat, SARs, models, probs=ciLevel ) {
#   # Get lower CI level
#   lo <- (1 - probs) / 2
#   # Get upper CI level
#   up <- 1 - lo
#   # Filter the data
#   SB0 <- dat %>%
#     filter( Parameter=="SB0" ) %>%
#     left_join( y=regions, by="Region" ) %>%
#     mutate( Region=factor(Region, levels=regions$Region),
#       RegionName=factor(RegionName, levels=regions$RegionName),
#       Model=factor(Model, levels=mNames) ) %>%
#     filter( Region %in% SARs, Model %in% models )
#   # SB0 quantiles
#   quantSB0 <- SB0 %>%
#     group_by( RegionName, Region, Model ) %>%
#     summarise( Lower=quantile(Value, probs=lo),
#       Median=quantile(Value, probs=0.5),
#       Upper=quantile(Value, probs=up) ) %>%
#     ungroup( ) %>%
#     filter( Region %in% SARs, Model %in% models )
#   # The plot
#   plotSSB <- ggplot( data=SB0 ) +
#     geom_density( aes(x=Value), fill="grey", trim=TRUE ) +
#     coord_cartesian( xlim=c(10, 200) ) +
#     geom_vline( data=quantSB0, aes(xintercept=Lower), linetype="dashed" ) +
#     geom_vline( data=quantSB0, aes(xintercept=Median) ) +
#     geom_vline( data=quantSB0, aes(xintercept=Upper), linetype="dashed" ) +
#     facet_wrap( Model ~ Region, scales="free_y", ncol=2, dir="v",
#       labeller=label_wrap_gen(multi_line=FALSE) ) +
#     labs( x=bquote("SB"[0]~" (t"%*%10^3*")"), y="Density" ) +
#     myTheme +
#     ggsave( filename=file.path(rootd.figs, paste0("SB0", ".png")), dpi=pDPI,
#       width=figWidth, height=figWidth )
# }  # End PlotSB0 function
#
# # Plot SB0
# # PlotSB0( dat=mRaw, SARs=allRegions$major, models=mNames[1] )
#
# # Plot coastwide biomass, and catch (and proportions by region), and spawn index
# PlotCoastwideBiomass <- function( dat1, dat2, dat3, model, SARs ) {
#   # Filter for the requested model (biomass)
#   df1 <- dat1 %>%
#     filter( Model==model, Region%in%SARs ) %>%
#     dplyr::select( Region, Year, Median ) %>%
#     mutate( Region=factor(Region, levels=regions$Region) ) %>%
#     rename( SSB=Median ) %>%
#     group_by( Year ) %>%
#     mutate( PropSSB=SSB/sum(SSB) ) %>%
#     ungroup( )
#   # Filter for the major areas (catch)
#   df2 <- dat2 %>%
#     filter( Region%in%SARs ) %>%
#     mutate( Region=factor(Region, levels=regions$Region) ) %>%
#     group_by( Region, Year ) %>%
#     summarise( Catch=SumNA(Catch) ) %>%
#     group_by( Year ) %>%
#     mutate( PropCat=Catch/sum(Catch) ) %>%
#     ungroup( )
#   # Filter for major areas (spawn)
#   df3 <- dat3 %>%
#     filter( Region%in%SARs ) %>%
#     mutate( Region=factor(Region, levels=regions$Region) ) %>%
#     dplyr::select( Region, Year, Spawn, Survey )
#   # Total spawn index, biomass, and catch
#   df4 <- full_join( x=df1, y=df2, by=c("Region", "Year") ) %>%
#     full_join( y=df3, by=c("Region", "Year") ) %>%
#     group_by( Year ) %>%
#     summarise( Spawn=SumNA(Spawn), SSB=SumNA(SSB), Catch=SumNA(Catch),
#       Survey=unique(Survey) ) %>%
#     ungroup( ) %>%
#     mutate( Survey=factor(Survey, levels=c("Surface", "Dive")) ) %>%
#     complete( Year=yrRange ) %>%
#     arrange( Year )
#   # Plot 1: stacked biomass
#   pBio <- ggplot( data=df1, aes(x=Year, y=SSB) ) +
#     geom_col( aes(fill=Region), width=1 ) +
#     labs( x=NULL,
#       y=expression(paste("Estimated spawning biomass (t"%*%10^3, ")",
#         sep="")) ) +
#     annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3, hjust=-0.1,
#       size=2.5 ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     scale_fill_viridis( discrete=TRUE ) +
#     expand_limits( x=yrRange ) +
#     myTheme +
#     theme( legend.position="top", axis.text.x=element_blank() )
#   # Plot 2: biomass proportion
#   pPropBio <- ggplot( data=df1, aes(x=Year, y=PropSSB) ) +
#     geom_line( aes(color=Region), size=0.75 ) +
#     labs( y="Proportion of biomass" ) +
#     annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, hjust=-0.1,
#       size=2.5 ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     scale_colour_viridis( discrete=TRUE ) +
#     expand_limits( x=c(min(yrRange)-0.5, max(yrRange)+0.5) ) +
#     guides( color=FALSE ) +
#     myTheme
#   # Plot 3: stacked catch
#   pCat <- ggplot( data=df2, aes(x=Year, y=Catch) ) +
#     geom_col( aes(fill=Region), width=1 ) +
#     labs( x=NULL, y=expression(paste("Catch (t"%*%10^3, ")", sep="")) ) +
#     annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3, hjust=-0.1,
#       size=2.5 ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     expand_limits( x=yrRange ) +
#     scale_fill_viridis( discrete=TRUE ) +
#     myTheme +
#     theme( legend.position="top", axis.text.x=element_blank() )
#   # Plot 4: catch proportion
#   pPropCat <- ggplot( data=df2, aes(x=Year, y=PropCat) ) +
#     geom_line( aes(color=Region), size=0.75 ) +
#     labs( y="Proportion of catch" ) +
#     annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, hjust=-0.1,
#       size=2.5 ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     scale_colour_viridis( discrete=TRUE ) +
#     expand_limits( x=c(min(yrRange)-0.5, max(yrRange)+0.5) ) +
#     guides( color=FALSE ) +
#     myTheme
#   # Plot 5: stacked spawn index, biomass, and catch
#   pAll <- ggplot( data=df4, mapping=aes(x=Year) ) +
#     geom_bar( mapping=aes(y=Catch), stat="identity", fill="darkgrey" ) +
#     geom_point( mapping=aes(y=Spawn, shape=Survey) ) +
#     geom_line( mapping=aes(y=SSB) ) +
#     labs( y=expression(paste("Catch, spawn index, and SSB",
#       " (t"%*%10^3, ")", sep="")) ) +
#     scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
#     myTheme +
#     theme( legend.position="top" ) +
#     ggsave( filename=file.path(rootd.figs,
#       paste0("CoastwideSSC", model, ".png")),
#       dpi=pDPI, height=figWidth/2, width=figWidth )
#   # Combine the plots: V1
#   pGrid1 <- plot_grid( pBio, pPropBio, align="v", ncol=1,
#     rel_heights=c(1.05, 1) ) +
#     ggsave( filename=file.path(rootd.figs, paste0("CoastwideBiomass", model, ".png")),
#       dpi=pDPI, height=figWidth, width=figWidth )
#   # Combine the plots: V2
#   pGrid2 <- plot_grid( pCat, pPropCat, align="v", ncol=1,
#     rel_heights=c(1.05, 1) ) +
#     ggsave( filename=file.path(rootd.figs, paste0("CoastwideCatch", model, ".png")),
#       dpi=pDPI, height=figWidth, width=figWidth )
# }  # End PlotCoastwideBiomass function
#
# # Coastwide biomass
# PlotCoastwideBiomass( dat1=spBio, dat2=catch, dat3=spawn, model="AM2",
#   SARs=allRegions$major )  # c("SoG", "WCVI")
#
# ## Plot proportion of spawning biomass
# #PlotProportionSSB <- function( dat ) {
# #  browser()
# #  # Plot 1: biomass
# #  plotPropSSB <- ggplot( data=dat, aes(x=Year, y=SSB) ) +
# #      geom_col( aes(fill=Region) ) +
# #      labs( x=NULL,
# #          y=expression(paste("Spawning biomass (t"%*%10^3, ")", sep="")) ) +
# #      scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
# #      scale_fill_viridis( discrete=TRUE ) +
# #      expand_limits( x=yrRange ) +
# #      myTheme +
# #      theme( legend.position="top", axis.text.x=element_blank() )
# #  # Plot 2: proportion
# #
# #}  # End PlotProportionSSB function
# #
# ##PlotProportionSSB( dat=propSSB )
# #
# ## Proportion SSB
# #plotProportionSSB <- ggplot( data=propSSB, aes(x=Year, y=Proportion) ) +
# #    geom_line( aes(color=Region), size=0.75 ) +
# #    labs( y="Proportion of spawning biomass" ) +
# #    scale_x_continuous( breaks=seq(from=1000, to=3000, by=10) ) +
# #    scale_colour_viridis( discrete=TRUE ) +
# #    expand_limits( x=yrRange ) +
# #    myTheme +
# #    theme( legend.position="top" ) +
# #    ggsave( filename="ProportionSSB.png", width=figWidth, height=figWidth*0.75 )
#
# ##### xTables #####
#
# # Format and print fixed cutoffs
# xCutOffs <- tibble( SAR=names(fixedCutoffs), Cutoff=unlist(fixedCutoffs) ) %>%
#   mutate( Cutoff=format(Cutoff*1000, big.mark=",", digits=0,
#     scientific=FALSE) ) %>%
#   rename( `Cut-off (t)`=Cutoff ) %>%
#   xtable( align="llr" ) %>%
#   print( file=file.path(rootd.tex, "Cutoffs.tex"), include.rownames=FALSE, booktabs=TRUE,
#     NA.string=NA, floating=FALSE )
#
# # Format and print 'high productivity' years
# xHiProd <- tibble( SAR=names(hiProdYrs), Start=lapply( X=hiProdYrs, FUN=min),
#   End=lapply( X=hiProdYrs, FUN=max) ) %>%
#   mutate( Start=as.integer(Start), End=as.integer(End) ) %>%
#   xtable( align="llrr" ) %>%
#   print( file=file.path(rootd.tex, "HiProd.tex"), include.rownames=FALSE, booktabs=TRUE,
#     NA.string=NA, floating=FALSE )
#
# # Print catch
# PrintCatch <- function( SARs, dat ) {
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Format catch
#     xCatch <- dat %>%
#       filter( Region==SAR ) %>%
#       dplyr::select( -Region ) %>%
#       mutate( Period=paste("Gear", Period, sep="") ) %>%
#       complete( Year=yrRange, Period=c("Gear1", "Gear2", "Gear3"),
#         fill=list(Catch=0) ) %>%
#       left_join( y=gear, by="Period" ) %>%
#       dplyr::select( -Period, -Engin ) %>%
#       spread( Gear, Catch ) %>%
#       xtable( digits=c(0, 0, 3, 3, 3) )
#     # Write catch to longtable
#     dir.create(file.path(rootd.tex, SAR), showWarnings = FALSE)
#     WriteLongTable( dat=xCatch, fn=file.path(rootd.tex, SAR, "Catch.tex"))
#     # Column names for catch
#     myNames <- paste( names(xCatch), collapse=" & " )
#   }  # End k loop over regions
#   # Return column names
#   return( myNames )
# }  # End PrintCatch function
#
# # Print catch and get column names
# namesCatch <- PrintCatch( SARs=unlist(allRegions, use.names=FALSE),
#   dat=inputData$catch )
#
# # Print spawn index
# PrintSpawn <- function( SARs, dat ) {
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Format spawn
#     xSpawn <- dat %>%
#       filter( Region==SAR ) %>%
#       dplyr::select( -Region ) %>%
#       rename( `Spawn index (t$\\times 10^{3}$)`=Spawn ) %>%
#       xtable( digits=c(0, 0, 3, 0) )
#     # Write spawn to longtable
#     dir.create(file.path(rootd.tex, SAR), showWarnings = FALSE)
#     WriteLongTable( dat=xSpawn, fn=file.path(rootd.tex, SAR, "Spawn.tex"))
#     # Column names for spawn
#     myNames <- paste( names(xSpawn), collapse=" & " )
#   }  # End k loop over regions
#   # Return column names
#   return( myNames )
# }  # End PrintSpawn function
#
# # Print spawn and get column names
# namesSpawn <- PrintSpawn( SARs=unlist(allRegions, use.names=FALSE),
#   dat=inputData$spawn )
#
# # Print number-at-age
# PrintAge <- function( SARs, dat ) {
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Format number-at-age
#     xNumAged <- dat %>%
#       filter( Region==SAR ) %>%
#       dplyr::select( -Region ) %>%
#       xtable( )
#     # Write number-at-age to longtable
#     dir.create(file.path(rootd.tex, SAR), showWarnings = FALSE)
#     WriteLongTable( dat=xNumAged, fn=file.path(rootd.tex, SAR, "NumAged.tex"))
#     # Column names for number-at-age
#     myNames <- paste( names(xNumAged), collapse=" & " )
#   }  # End k loop over regions
#   # Return column names
#   return( myNames )
# }  # End PrintAge function
#
# # Print number-at-age and get column names
# namesNumAged <- PrintAge( SARs=unlist(allRegions, use.names=FALSE),
#   dat=inputData$numAged )
#
# # Print weight-at-age
# PrintWeight <- function( SARs, dat ) {
#   # Loop over regions
#   for( k in 1:length(SARs) ) {
#     # Get the region
#     SAR <- SARs[k]
#     # Format weight-at-age
#     xWeight <- dat %>%
#       filter( Region==SAR ) %>%
#       dplyr::select( -Region ) %>%
#       xtable( digits=c(0, 0, rep(3, times=max(ageRange)-min(ageRange)+1)) )
#     # Write weight-at-age to longtable
#     dir.create(file.path(rootd.tex, SAR), showWarnings = FALSE)
#     WriteLongTable( dat=xWeight, fn=file.path(rootd.tex, SAR, "Weight.tex"))
#     # Column names for weight-at-age
#     myNames <- paste( names(xWeight), collapse=" & " )
#   }  # End k loop over regions
#   # Return column names
#   return( myNames )
# }  # End PrintWeight function
#
# # Print weight-at-age and get column names
# namesWeight <- PrintWeight( SARs=unlist(allRegions, use.names=FALSE),
#   dat=inputData$weight )
#
# # Print the number of biosamples
# PrintNBio <- function( dat ) {
#   # Format number of biosamples
#   xNBio <- dat %>%
#     xtable( digits=c(0, 0, rep(0, times=length(unlist(allRegions)))) )
#   # Write number of biosamples to longtable
#   WriteLongTable( dat=xNBio, fn=file.path(rootd.tex, "NBio.tex"))
#   # Column names for number of biosamples
#   myNames <- paste( names(xNBio), collapse=" & " )
#   # Return column names
#   return( myNames )
# }  # End PrintNBio function
#
# # Print number of biosamples and get column names
# namesNBio <- PrintNBio( dat=nBio )
#
# # Format and print spawn distribution (proportion)
# PrintPSpawn <- function( dat ) {
#   # Loop over the SARs
#   for( k in 1:length(dat) ) {
#     # Get the kth SAR
#     kSAR <- names( dat )[k]
#     # Get the table
#     kTab <- dat[[k]]
#     # Generate the multicolumn row
#     addMulti <- list(
#       pos=list(-1),
#       command="\\" )
#     # Format and print the table
#     dir.create(file.path(rootd.tex, kSAR), showWarnings = FALSE)
#     xHiProd <- kTab %>%
#       xtable( align=c("l", "l", rep("r", times=ncol(kTab)-1)),
#         digits=c(0, 0, rep(3, times=ncol(kTab)-1)) ) %>%
#       print( file=file.path(rootd.tex, kSAR, "PSpawn.tex"),
#         include.rownames=FALSE, booktabs=TRUE, NA.string=NA, floating=FALSE,
#         add.to.row=NULL, sanitize.colnames.function=Bold2 )
#   }  # End k loop over SARs
# }  # End PrintPSpawn function
#
# # Print spawn distribution
# PrintPSpawn( dat=pSpawn )
#
# # Print annual catch to a table
# catchYr <- catch %>%
#   filter( Year >= max(yrRange)-9, Region%in%allRegions$major ) %>%
#   group_by( Region, Year ) %>%
#   summarise( Catch=SumNA(Catch) ) %>%
#   ungroup( ) %>%
#   mutate( Catch=format(Catch*1000, big.mark=",", digits=0,
#     scientific=FALSE) ) %>%
#   complete( Year=(max(yrRange)-9):max(yrRange),
#     Region=allRegions$major, fill=list(Catch=0) ) %>%
#   mutate( Region=factor(Region, allRegions$major) ) %>%
#   spread( key=Region, value=Catch, drop=FALSE ) %>%
#   arrange( Year ) %>%
#   xtable( align=c("r", "r", rep("r", times=length(allRegions$major))) ) %>%
#   print( file=file.path(rootd.tex, "AnnualCatch.tex"), include.rownames=FALSE,
#     booktabs=TRUE, sanitize.colnames.function=Bold2, floating=FALSE )
#
# # Print annual catch to a table
# harvYr <- harvSOK %>%
#   filter( Year >= max(yrRange)-9, Region%in%allRegions$major ) %>%
#   mutate( Harvest=format(Harvest*2.20462, big.mark=",", digits=0,
#     scientific=FALSE),
#     # Deal with privacy issues
#     Harvest=ifelse(Year%in%c(2016, 2019) & Region=="PRD", "WP", Harvest) ) %>%
#   complete( Year=(max(yrRange)-9):max(yrRange),
#     Region=allRegions$major, fill=list(Catch=0) ) %>%
#   mutate( Region=factor(Region, allRegions$major) ) %>%
#   dplyr::select( Region, Year, Harvest ) %>%
#   spread( key=Region, value=Harvest, drop=FALSE ) %>%
#   arrange( Year ) %>%
#   xtable( align=c("r", "r", rep("r", times=length(allRegions$major))) ) %>%
#   print( file=file.path(rootd.tex, "AnnualHarvest.tex"), include.rownames=FALSE,
#     booktabs=TRUE, sanitize.colnames.function=Bold2, floating=FALSE )
#
# ##### LaTeX #####
#
# # Year for model predictions (i.e., next year)
# nextYr <- max( yrRange ) + 1
#
# # Number of years in the time series
# nYrs <- length( yrRange )
#
# # Current season code
# thisSeason <- paste( yrRange[nYrs-1], yrRange[nYrs], sep="/" )
#
# # Formatted year ranges for q1 (surface) and q2 (dive)
# qYrs <- list(
#   q1=paste(range(yrRange[yrRange<newSurvYr]), collapse=" to "),
#   q2=paste(range(yrRange[yrRange>=newSurvYr]), collapse=" to ") )
#
# # Catch in current year
# finalYrCatch <- catch %>%
#   complete( Year=yrRange, Region=unlist(allRegions, use.names=FALSE),
#     fill=list(Catch=0) ) %>%
#   group_by( Region, Year ) %>%
#   summarise( Catch=SumNA(Catch) ) %>%
#   ungroup() %>%
#   filter( Year == max(yrRange) ) %>%
#   dplyr::select( Region, Catch ) %>%
#   mutate( Catch=format(Catch*1000, big.mark=",", digits=0, scientific=FALSE) )
#
# # Spawn in current year
# finalYrSpawn <- inputData$spawn %>%
#   filter( Year == max(yrRange) ) %>%
#   mutate( Spawn=format(Spawn*1000, big.mark=",", digits=0, scientific=FALSE) )
#
# # Spawn in previous year
# prevYrSpawn <- inputData$spawn %>%
#   # Get spawn in final year of timeseries
#   filter( Year == max(yrRange)-1 ) %>%
#   # Convert to tonnes, and format nicely
#   mutate( Spawn=format(Spawn*1000, big.mark=",", digits=0, scientific=FALSE) )
#
# # Spawn direction (increased or decreased from last year)
# directionSpawn <- inputData$spawn %>%
#   filter( Year %in% (max(yrRange)-1):max(yrRange) ) %>%
#   group_by( Region ) %>%
#   mutate( Direction=ifelse(Spawn>lag(Spawn, n=1), "increased",
#     "decreased") ) %>%
#   ungroup( ) %>%
#   filter( Year == max(yrRange) )
#
# # Proportion at age in current year
# finalYrPropAge <- numAgedYear %>%
#   filter( Year == max(yrRange) ) %>%
#   mutate( Percent=format(Proportion*100, digits=0, scientific=FALSE) )
#
# # Number of biosamples in current year
# finalYrNBio <- nBio %>%
#   filter( Year == max(yrRange) )
#
# # Early mean catch
# earlyMeanCatch <- annualCatch %>%
#   filter( Year %in% earlyCatchYrs ) %>%
#   dplyr::select( Catch ) %>%
#   summarise( Mean=mean(Catch) )
#
# # Recent mean catch
# recentMeanCatch <- annualCatch %>%
#   filter( Year %in% (max(yrRange)-recentCatchYrs+1):max(yrRange) ) %>%
#   dplyr::select( Catch ) %>%
#   summarise( Mean=mean(Catch) )
#
# # Current year total catch: roe
# finalYrCatchRoe <- catch %>%
#   filter( Year==max(yrRange), Gear%in%c("RoeGN", "RoeSN") ) %>%
#   dplyr::select( Catch ) %>%
#   mutate( Catch=Catch*1000 ) %>%
#   sum( ) %>%
#   format( big.mark=",", digits=0, scientific=FALSE )
#
# # Current year total catch: roe
# finalYrCatchOther <- catch %>%
#   filter( Year==max(yrRange), Gear=="Other" ) %>%
#   dplyr::select( Catch ) %>%
#   mutate( Catch=Catch*1000 ) %>%
#   sum( ) %>%
#   format( big.mark=",", digits=0, scientific=FALSE )
#
#
# # Save the workspace image
# save.image(file=file.path(rootd.tex, "Image.RData"))
