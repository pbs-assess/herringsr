# Housekeeping
rm( list=ls() )
graphics.off( )

# Packages
require( tidyverse )
require( reshape2 )

# Gear look up table
gear <- tribble(
  ~Period,   ~Gear,
  1,     "Other",
  2,     "RoeSN",
  3,     "RoeGN" )

# Maximum age (plus group)
agePlus <- 10

# Plot width
pWidth <- 6.5

# Load the biosample data
raw <- read_csv( file=file.path("PropAge", "BioSoG2019.csv"),
                 col_types=cols() )

# Light wrangling
bio <- raw %>%
  left_join( y=gear, by="Period" ) %>%
  select( Year, Month, Region, StatArea, Section,
          LocationName, Sample, Fish, Length, Weight,
          Sex, MaturityCode, Age, Gear ) %>%
  mutate( Age=ifelse(Age >= agePlus, agePlus, Age),
          Gear=factor(Gear, levels=gear$Gear) ) %>%
  arrange( Month, Sample, Fish, Age )

# Summarise by sample
bioSamp <- bio %>%
  group_by( Gear, Month, StatArea, Section, LocationName, Sample, Age ) %>%
  summarise( Number=n()) %>%
  mutate(  Proportion=Number/sum(Number) ) %>%
  ungroup( )

# Summaries by sample, wide version
bioSampW <- bioSamp %>%
  dcast( Gear + Month + StatArea + Section + LocationName + Sample ~ Age,
         value.var="Number" ) %>%
  write_csv( path=file.path("PropAge", "Summary.csv") )

# Summarise by gear
bioGear <- bioSamp %>%
  group_by( Gear, Age ) %>%
  summarise( Number=sum(Number) ) %>%
  mutate(  Proportion=Number/sum(Number) ) %>%
  ungroup( )

# Plot proportion-at-age by gear
pPropAgeGear <- ggplot( data=bioGear, mapping=aes(x=Age, y=Proportion) ) +
  geom_bar( stat="identity", aes(fill=Age!=2) )  +
  facet_grid( ~ Gear ) +
  scale_x_continuous( breaks=pretty(2:agePlus) ) +
  scale_fill_viridis_d( ) +
  guides( fill=FALSE ) +
  theme_bw( ) +
  ggsave( filename=file.path("PropAge", "PropAgeGear.png"), width=pWidth,
          height=pWidth/3 )

# Plot proportion-at-age by sample: Other
pPropAgeSampOther <- ggplot( data=bioSamp %>% filter( Gear == "Other"),
                             mapping=aes(x=Age, y=Proportion) ) +
  geom_bar( stat="identity", aes(fill=Age!=2) )  +
  facet_wrap( ~ Sample ) +
  scale_x_continuous( breaks=pretty(2:agePlus) ) +
  scale_fill_viridis_d( ) +
  guides( fill=FALSE ) +
  theme_bw( ) +
  ggsave( filename=file.path("PropAge", "PropAgeSampOther.png"), width=pWidth,
          height=pWidth )

# Plot proportion-at-age by sample: RoeSN
pPropAgeSampRoeSN <- ggplot( data=bioSamp %>% filter( Gear == "RoeSN"),
                             mapping=aes(x=Age, y=Proportion) ) +
  geom_bar( stat="identity", aes(fill=Age!=2) )  +
  facet_wrap( ~ Sample ) +
  scale_x_continuous( breaks=pretty(2:agePlus) ) +
  scale_fill_viridis_d( ) +
  guides( fill=FALSE ) +
  ggsave( filename=file.path("PropAge", "PropAgeSampRoeSN.png"), width=pWidth,
          height=pWidth )

# Plot proportion-at-age by sample: RoeGN
pPropAgeSampRoeGN <- ggplot( data=bioSamp %>% filter( Gear == "RoeGN"),
                             mapping=aes(x=Age, y=Proportion) ) +
  geom_bar( stat="identity", aes(fill=Age!=2) )  +
  facet_wrap( ~ Sample ) +
  scale_x_continuous( breaks=pretty(2:agePlus) ) +
  scale_fill_viridis_d( ) +
  guides( fill=FALSE ) +
  theme_bw( ) +
  ggsave( filename=file.path("PropAge", "PropAgeSampRoeGN.png"), width=pWidth,
          height=pWidth )
