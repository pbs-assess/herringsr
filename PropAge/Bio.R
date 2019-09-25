# Housekeeping
rm( list=ls() )
graphics.off( )

# Packages
require( tidyverse )

# Gear look up table
gear <- tribble(
  ~Period,   ~Gear,
  1,     "Other",
  2,     "RoeSN",
  3,     "RoeGN" )

# Load the biosample data
bio <- read_csv( file=file.path("PropAge", "bio.csv"), col_types=cols() ) %>%
  left_join( y=gear, by="Period" ) %>%
  select( Year, Month, Region, StatArea, Section,
          LocationName, Eastings, Northings, Sample, Fish, Length, Weight,
          Sex, MaturityCode, Age, GonadLength, GonadWeight, Gear )
