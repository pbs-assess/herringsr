## Location and name of the location database and tables
# areaLoc <- list(
#     loc=file.path(dirShare, "Stock_Assess_Database"),
#     db="HSA_BE_Locations_Main_v6.1_2015.mdb",
#     fns=list(sections="Sections", locations="Location") )
#
## Location(s) and names of the Sections and land shapefiles
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
    "SAR, Region, RegionName, Major
        1, HG, Haida Gwaii, TRUE
        2, PRD, Prince Rupert District, TRUE
        3, CC, Central Coast, TRUE
        4, SoG, Strait of Georgia, TRUE
        5, WCVI, West Coast of Vancouver Island, TRUE
        6, A27, Area 27, FALSE
        7, A2W, Area 2 West, FALSE",
  col_types = cols())

# Cross-walk table for Period to Gear type
gear <- read_csv(file =
    "Period, Gear
      Gear1, Other
      Gear2, RoeSN
      Gear3, RoeGN",
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
#figWidth <- 6.5

# Type of smoothing line
smLine <- "loess"

# Level of confidence interval
ciLevel <- 0.9

# Get ylimits (e.g., weight in kg) for the weight-at-age plot
wtMax <- 150 / 1000

# 1996 fixed cutoff values (t*10^3)
#fixedCutoffs <- list( HG=10.7, PRD=12.1, CC=17.6, SoG=21.2, WCVI=18.8 )

# High-productivity years
hiProdYrs <- list(HG = 1994:1997,
                  PRD = 1994:2002,
                  CC = 1990:1999,
                  SoG = 1988:2016,
                  WCVI = 1988:1996 )

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



