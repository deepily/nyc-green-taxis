library( dplyr )
library( lubridate )
library( ggplot2 )
library( data.tabl )

# only do this ONCE
# # ~13 seconds to fetch...
# system.time( taxis_raw <- fread( "https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv" ) )
# system.time( fwrite( taxis_raw, "data/green-tripdata-2015-09-raw.csv", verbose=TRUE ) )

# s l o o o o o o w, ~19s to read
# system.time( taxis_raw <- read.csv( "data/green-tripdata-2015-09-raw.csv" ) )

# fast! ~.6 seconds :-)
system.time( taxis_raw <- fread( "data/green-tripdata-2015-09-raw.csv" ) )

# Question 1
# 
# Programmatically download and load into your favorite analytical tool the trip data for September 2015.
# Report how many rows and columns of data you have loaded.

dim( taxis_raw )
str( taxis_raw )

# colSums( is.na( taxis_raw ) )
# # drop Ehail_fee, all NAs
# taxis_raw$Ehail_fee <- NULL
# dim( taxis_raw )

