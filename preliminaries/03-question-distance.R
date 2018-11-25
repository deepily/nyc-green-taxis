# Question 3
#
# 1) Report mean and median trip distance grouped by hour of day.
# 2) We’d like to get a rough sense of identifying trips that originate or terminate at one of the NYC area airports. 
# Can you provide a count of how many transactions fit this criteria, the average fare, and any other interesting 
# characteristics of these trips.
str( taxis_filtered )

# add pickup hour
taxis_filtered$Pickup_hour <- hour( taxis_filtered$lpep_pickup_datetime )
summary( taxis_filtered$Pickup_hour )

# what's the distance look like?
#summary( taxis_filtered$Trip_distance ) #0.000   1.080   1.920   2.705   3.600  12.190 
#hist( taxis_filtered$taxis_filtered )

# WE'VE ALREADY DONE THIS IN Q2!
# # let's nuke by 3rd std
# # nuke outliers: what's reasonable to keep? w/in 3 stds of mean?
# dist_sd <- sd( taxis_filtered$trip_distance )
# dist_sd
# dist_mean <- mean( taxis_filtered$trip_distance )
# dist_mean
# 
# # do we want to discard outliers?
# dim( taxis_filtered )[ 1 ] 
# taxis_filtered <- dplyr::filter( taxis_filtered, trip_distance < dist_mean + 3 * dist_sd )
# dim( taxis_filtered )[ 1 ]
# summary( taxis_filtered$trip_distance )
# hist( taxis_filtered$trip_distance )

# group by hour
taxis_filtered_grp <- dplyr::group_by( taxis_filtered, Pickup_hour )
taxis_filtered_sum <- dplyr::summarise( 
  taxis_filtered_grp, 
  #count = n(),
  Mean = mean( Trip_distance ),
  Median = median( Trip_distance )
)
taxis_filtered_sum

# reshape for plotting
# http://markhneedham.com/blog/2014/09/16/r-ggplot-plotting-multiple-variables-on-a-line-chart/
library( "reshape2" )

melted_sums = melt( taxis_filtered_sum, id='Pickup_hour' )
melted_sums

ggplot( melted_sums, aes( x=Pickup_hour, y=value, colour=variable ) ) + 
  geom_line() + 
  ylab( label="Distance (miles)" ) + 
  xlab( "Hour of Day" ) #+ 
#scale_colour_manual( values=c("grey", "blue"))

# starting/stopping at nearby airports
# define polygons for newark and jfk, and then run point_in functions (time and cpu intensive) --or--
# use ratecodeid in( 2, 3 ) http://www.nyc.gov/html/tlc/downloads/pdf/data_dictionary_trip_records_green.pdf

summary( taxis_filtered$RateCodeID )
table( taxis_filtered$RateCodeID )
# # what's '99' doing in there? Nuke 'em
# dim( taxis_filtered )
taxis_filtered <- dplyr::filter( taxis_filtered, RateCodeID < 7 )
#dim( taxis_filtered )
table( taxis_filtered$RateCodeID )

# TODO: This could be naive(?), other rate code 5 = group rides?
# 2=JFK
# 3=Newark
airport_rides = dplyr::filter( taxis_filtered, RateCodeID %in% c( 2, 3 ) )
dim( airport_rides )
# what do the fare values look like?
summary( airport_rides$Fare_amount ) 
# # fares need to be > $0
airport_rides = dplyr::filter( airport_rides, Fare_amount > 0 )
dim( airport_rides )
summary( airport_rides$Fare_amount ) 

# histograms for fare and distance, BOTH airports
ggplot( airport_rides, aes( x=Fare_amount ) ) + geom_histogram()
ggplot( airport_rides, aes( x=Trip_distance ) ) + geom_histogram()

# plot separately
# 2=JFK
# 3=Newark
newark <- dplyr::filter( airport_rides, RateCodeID == 2 )
ggplot( newark, aes( x=Fare_amount ) ) + geom_histogram()
# fares are fixed for newark!

jfk <- dplyr::filter( airport_rides, RateCodeID == 3 )
ggplot( jfk, aes( x=Fare_amount ) ) + geom_histogram()
# fares for jfk are not

# look at duration too, lubridate
airport_rides$Trip_duration_mins <- round( as.duration( interval( airport_rides$lpep_pickup_datetime, airport_rides$Lpep_dropoff_datetime ) ) / dminutes( 1 ), 0 )
hist( airport_rides$Trip_duration_mins )
ggplot( airport_rides, aes( x=Trip_duration_mins ) ) + geom_histogram()
# let's knock off outliers
dur_sd <- sd( airport_rides$Trip_duration_mins )
dur_sd
dur_mean <- mean( airport_rides$Trip_duration_mins )
dur_mean

# how many get discarded?
rows_before <- dim( airport_rides )[ 1 ]
airport_rides <- dplyr::filter( airport_rides, Trip_duration_mins < dur_mean + 3 * dur_sd )
rows_after <- dim( airport_rides )[ 1 ] 

ggplot( airport_rides, aes( x=Trip_duration_mins ) ) + geom_histogram()
#hist( airport_rides$trip_duration_mins )

# 2=JFK
# 3=Newark
table( airport_rides$RateCodeID )
# TODO: Why are rides so short in time and distance, yet so expensive? 
# TODO: Maybe there are many hotels nearby? Or the codes are inaccurate, 
# TODO: Look into polygon defs for airports to see what those fares look like.


# what are the mean fares, per airport?  This omits tips, tolls, tax, surcharge
airport_rides_grp <- dplyr::group_by( airport_rides, RateCodeID )
airport_rides_sum <- dplyr::summarise( airport_rides_grp, Fare=mean( Fare_amount ), Tip=mean( Tip_amount ), Total=mean( Total_amount ), Distance=mean( Trip_distance ), Duration=mean( Trip_duration_mins ) )
# add labels for x axis
airport_rides_sum$Airport <- c( "JFK", "Newark" )
airport_rides_sum

# what do the fares look like?
#boxplot( Fare_amount~RateCodeID, data=airport_rides, main="Airport Fares", xlab="RateCodeID", ylab="Fare value") 
#ggplot( airport_rides, aes( y=Fare_amount, x=RateCodeID, color=RateCodeID ) ) + geom_boxplot()

# Fare
ggplot( airport_rides_sum, aes( Airport, Fare ) ) + 
  geom_bar( stat="identity" ) +
  ylab( "Fare (USD)" ) +
  ggtitle( "Mean Fare by Airport" )

# ggplot( airport_rides_sum, aes( Airport, Fare ) ) + 
#   geom_bar( stat="identity" ) +
#   ylab( "Fare (USD)" ) +
#   ggtitle( "Mean Fare by Airport" )

# leaflet experiment
library( leaflet )
library( ggmap )

nyc_map <- qmap( location="New York City" ) 
nyc_map

meters_in_mile <- 1609.344
newark_sum <- dplyr::filter( airport_rides_sum, Airport == "Newark" )
jfk_sum <- dplyr::filter( airport_rides_sum, Airport == "JFK" )

leaflet( nyc_map ) %>%
  addTiles() %>%
  addMarkers( lng=-73.7781, lat=40.6413, popup=paste( "JFK Mean distance ==", jfk_sum$Distance, "?!?", sep=" " ) ) %>%
  addCircles( lng=-73.7781, lat=40.6413, radius=( meters_in_mile * jfk_sum$Distance  ) ) %>%
  
  addMarkers( lng=-74.1745, lat=40.6895, popup=paste( "Newark Mean distance ==", newark_sum$Distance, "?!?", sep=" " ) ) %>%
  addCircles( lng=-74.1745, lat=40.6895, radius=( meters_in_mile * newark_sum$Distance  ) )

# this seems to confirms that we should look into pickup/dropoff lat/lons and the calculate distances using 
# https://en.wikipedia.org/wiki/Haversine_formula
# TODO: Later!


# # see what the start/stop lat/lons look like
# # just nuking 0 values cleans up both lat/lons
summary( airport_rides$Pickup_latitude )
summary( airport_rides$Pickup_longitude )
summary( airport_rides$Dropoff_latitude )
summary( airport_rides$Dropoff_longitude )

airport_rides <- dplyr::filter( airport_rides, Pickup_latitude > 0 )
airport_rides <- dplyr::filter( airport_rides, Dropoff_latitude > 0 )

summary( airport_rides$Pickup_latitude )
summary( airport_rides$Pickup_longitude )
summary( airport_rides$Dropoff_latitude )
summary( airport_rides$Dropoff_longitude )


