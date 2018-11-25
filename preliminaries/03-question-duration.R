# Question 3
#
# * Report mean and median trip distance grouped by hour of day.
# * We’d like to get a rough sense of identifying trips that originate or terminate at one of the NYC area airports. 
# Can you provide a count of how many transactions fit this criteria, the average fare, and any other interesting 
# characteristics of these trips.
str( taxis_filtered )

# convert date & time into separate fields
# Day_of_month, Day_of_week, Dour_of_day (0-24), Trip_duration (0.00hrs)

Sys.setenv( TZ='EST' )

# $ lubridate functions
# this_moment <- now()
# this_moment
# y <- year(this_moment)
# m <- month(this_moment)
# d <-day(this_moment)
# y
# m
# d
# 
# wday( this_moment, label=TRUE )
# mday( this_moment )
# 
# hr <- hour(this_moment)
# mins <- minute(this_moment)
# sec <- second(this_moment)
# 
# rbind( hr, mins, sec )
# summary( taxis_filtered$lpep_pickup_datetime )
# foo <- ymd_hms( "2015-09-09 15:29:43" )
# month( foo, label=TRUE )
# hour( foo )
# 
# class( taxis_filtered$lpep_pickup_datetime )
taxis_filtered$pickup_hour <- hour( taxis_filtered$lpep_pickup_datetime )
summary( taxis_filtered$pickup_hour )

# get trip durations as minutes
taxis_filtered$Trip_duration_mins <- round( as.duration( interval( taxis_filtered$lpep_pickup_datetime, taxis_filtered$Lpep_dropoff_datetime ) ) / dminutes( 1 ), 0 )

# what's the duration look like?
summary( taxis_filtered$Trip_duration_mins ) #0.00    6.00   10.00   19.67   17.00 1440.00
# 1440 / 60 = 24 hrs.  What's our cutoff for outliers?
hist( taxis_filtered$Trip_duration_mins )

# let's nuke by 3rd std
# nuke outliers: what's reasonable to keep? w/in 3 stds of mean?
dur_sd <- sd( taxis_filtered$Trip_duration_mins )
dur_sd
dur_mean <- mean( taxis_filtered$Trip_duration_mins )
dur_mean

# how many get discarded?
dim( taxis_filtered )[ 1 ] #1465734
taxis_filtered <- dplyr::filter( taxis_filtered, Trip_duration_mins < dur_mean + 3 * dur_sd )
dim( taxis_filtered )[ 1 ] #1457555
summary( taxis_filtered$Trip_duration_mins )
hist( taxis_filtered$Trip_duration_mins, breaks=100 )

# group by hour
taxis_filtered_grp <- dplyr::group_by( taxis_filtered, pickup_hour )
taxis_filtered_sum <- dplyr::summarise( 
  taxis_filtered_grp, 
  #count = n(),
  distance_mean = mean( Trip_duration_mins ),
  distance_median = median( Trip_duration_mins )
)
taxis_filtered_sum

# 
# ggplot( taxis_filtered_sum, aes( x=hour ) ) + 
#   geom_line( aes( y=dist_mean, color="blue" ) ) +
#   geom_line( aes( y=distance_median, color="green" ) )
# 
# ggplot( taxis_filtered_sum, aes(x = hour)) + 
#   geom_line(aes(y = dist_mean), colour="blue") + 
#   geom_line(aes(y = distance_median), colour = "grey") + 
#   ylab(label="Number of new members") + 
#   xlab("Week")

# reshape for plotting
# http://markhneedham.com/blog/2014/09/16/r-ggplot-plotting-multiple-variables-on-a-line-chart/
library( "reshape2" )

melted_sums = melt( taxis_filtered_sum, id='pickup_hour' )
melted_sums

ggplot( melted_sums, aes( x=pickup_hour, y=value, colour=variable ) ) + 
  geom_line() + 
  ylab( label="Duration (minutes)" ) + 
  xlab( "Hour of Day" ) #+ 
#scale_colour_manual( values=c("grey", "blue"))

# starting/stopping at nearby airports
# define polygons for newark and jfk, and then run point_in functions (time and cpu intensive) --or--
# use RateCodeID in( 2, 3 ) http://www.nyc.gov/html/tlc/downloads/pdf/data_dictionary_trip_records_green.pdf

summary( taxis_filtered$RateCodeID )
table( taxis_filtered$RateCodeID )
# what's '99' doing in there? Nuke 'em
dim( taxis_filtered )
taxis_filtered <- dplyr::filter( taxis_filtered, RateCodeID < 7 )
dim( taxis_filtered )
table( taxis_filtered$RateCodeID )

airport_rides = dplyr::filter( taxis_filtered, RateCodeID == 2 | RateCodeID == 3 )
dim( airport_rides )
# what do the fare values look like?
summary( airport_rides$Fare_amount ) 
# fares need to be > $0
airport_rides = dplyr::filter( airport_rides, Fare_amount > 0 )
dim( airport_rides )
summary( airport_rides$Fare_amount ) 
hist( airport_rides$Fare_amount )
hist( airport_rides$Trip_distance )
hist( airport_rides$Trip_duration_mins )

# 2=JFK
# 3=Newark
table( airport_rides$RateCodeID )
# TODO: Why are rides so short in time and distance, yet so expensive? 
# TODO: Maybe there are many hotels nearby? Or the codes are inaccurate, 
# TODO: Look into polygon defs for airports to see what those fares look like.




# what are the mean fares, per airport?  This omits tips, tolls, tax, surcharge
airport_rides_grp <- dplyr::group_by( airport_rides, RateCodeID )
airport_rides_sum <- dplyr::summarise( airport_rides_grp, mean( Fare_amount ), mean( Trip_distance ), mean( Trip_duration_mins ) )
airport_rides_sum

# what do the fares look like?
boxplot( Fare_amount~RateCodeID, data=airport_rides, main="Airport Fares", xlab="RateCodeID", ylab="Fare value") 






# see what the start/stop lat/lons look like
summary( airport_rides$Pickup_longitude )
summary( airport_rides$Pickup_latitude )
summary( airport_rides$Dropoff_longitude )
summary( airport_rides$Dropoff_latitude )

