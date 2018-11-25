taxis_cleaned <- fread( "data/green-tripdata-2015-09-cleaned.csv" )

#taxis_filtered$Tip_percent <- taxis_cleaned$Tip_amount / taxis_filtered$Total_amount * 100
summary( taxis_cleaned$tip_percent )
fare_type_grp <- dplyr::group_by( taxis_cleaned, payment_type )
fare_type_sum <- dplyr::summarise( fare_type_grp, 
                                   count = n(),
                                   tip_mean = mean( tip_amount ),
                                   tip_percent_mean = mean( tip_percent ) )
# 1= Credit card
# 2= Cash
# 3= No charge
# 4= Dispute
# 5= Unknown
# 6= Voided trip
fare_type_sum$payment <- c( "Credit card", "Cash", "No charge", "Dispute", "Unknown" )
fare_type_sum$percent <- fare_type_sum$count / sum( fare_type_sum$count ) * 100
fare_type_sum
# conclusion: if they're paying w/ cc, then they'll tip.  If cash, it won't get *recorded*
# Fare
ggplot( dplyr::filter( fare_type_sum, payment_type < 3 ), aes( payment, percent ) ) + 
  geom_bar( stat="identity" ) +
  ylab( "Percent" ) +
  ggtitle( "Fare Payment Types" )



# take a look at tip distributions
trips_with_tips = dplyr::filter( taxis_filtered, Tip_amount > 0 )

ggplot( trips_with_tips, aes( x=Tip_percent ) ) + geom_histogram()
#hist( trips_with_tips$tip_percent )

ggplot( taxis_filtered, aes( x=Tip_percent ) ) + geom_histogram()
#hist( taxis_filtered$tip_percent )

##################################################################################################
# do heat maps of tips by day/hour and mph by day/hour, using cleaned data from tips prediction
##################################################################################################

# what do tips look like by hour of day/speed/trip length
# load all clean data
taxis_cleaned <- fread( "data/green-tripdata-2015-09-cleaned.csv" )
#colnames( taxis_cleaned )


# 1st: group by day/hour
#trips_with_tips           <- dplyr::filter( taxis_cleaned, payment_type == 1 )
trips_by_day_and_hour_grp <- dplyr::group_by( taxis_cleaned, pickup_day_of_week, pickup_hour )
trips_by_day_and_hour_sum <- dplyr::summarise( trips_by_day_and_hour_grp, Tip=mean( tip_percent ), Mph=mean( speed_mph ) )

#View( trips_by_day_and_hour_sum )
# heatmap of tips by hour
ggplot( trips_by_day_and_hour_sum, aes( x=as.factor( pickup_day_of_week ), y=as.factor( pickup_hour ), fill=Tip ) ) + 
  geom_tile() +
  # days of week start w/ 0=mon in Python's datetime.weekday(): https://docs.python.org/3/library/datetime.html
  scale_x_discrete( name='Day of Week', labels=c( '0'='Mon','1'='Tue','2'='Wed','3'='Thu','4'='Fri','5'='Sat','6'='Sun' ) ) +
  ylab( "Hour of Day" ) +
  ggtitle( "Tips by Day and Hour" ) +
  scale_fill_gradientn( colors=c( 'black','dark green','green', 'yellow','white' ) ) 


# mph by hour
ggplot( trips_by_day_and_hour_sum, aes( x=as.factor( pickup_day_of_week ), y=as.factor( pickup_hour ), fill=Mph ) ) + 
  geom_tile() +
  # days of week start w/ 0=mon in Python's datetime.weekday(): https://docs.python.org/3/library/datetime.html
  scale_x_discrete( name='Day of Week', labels=c( '0'='Mon','1'='Tue','2'='Wed','3'='Thu','4'='Fri','5'='Sat','6'='Sun' ) ) +
  ylab( "Hour of Day" ) +
  ggtitle( "Mph by Day and Hour" ) +
  scale_fill_gradientn( colors=c( 'black','dark blue','blue', 'light blue', 'white' ) ) 


# tips by dropoff lat/lon
# bin lat/lons, using 1 degree of precision ~7x7mile bins, 2 ~.7miles, 3 ~.07 miles (364ft)
taxis_cleaned$dropoff_longitude_bin <- round( taxis_cleaned$dropoff_longitude, 2 )
taxis_cleaned$dropoff_latitude_bin <- round( taxis_cleaned$dropoff_latitude, 2 )
taxis_cleaned$pickup_longitude_bin <- round( taxis_cleaned$pickup_longitude, 2 )
taxis_cleaned$pickup_latitude_bin <- round( taxis_cleaned$pickup_latitude, 2 )

trips_by_dropoff_grp <- dplyr::group_by( taxis_cleaned, dropoff_longitude_bin, dropoff_latitude_bin )
trips_by_dropoff_sum <- dplyr::summarise( trips_by_dropoff_grp, Count=n(), Tip=mean( tip_percent ) )
dim( trips_by_dropoff_sum )
sum( trips_by_dropoff_sum$Count )
trips_by_dropoff_sum

ggplot( trips_by_dropoff_sum, aes( x=dropoff_longitude_bin, y=dropoff_latitude_bin, fill=Tip ) ) + 
  geom_tile() + 
  #geom_point( aes( colour=trips_by_dropoff_sum$Tip ) )
  ylab( "Latitude" ) +
  xlab( "Longitude" ) +
  ggtitle( "Tips by Dropoff Lat/Lon" ) +
  scale_fill_gradientn( colors=c( 'yellow', 'orange', 'red', 'dark red', 'black' ) ) +
  #scale_fill_gradientn( colors=c( 'black','dark red','red', 'orange','yellow' ) ) +
  xlim( -75, -72.5 ) +
  ylim( 40.5, 41 )
  #scale_fill_gradientn( colors=c( 'black','dark green','green', 'yellow','white' ) ) 

# Plotting tip hot spots, Take II
ggmap( nyc_map_ggmap ) + 
  #geom_point( data=misses, aes( x=lon, y=lat ), alpha=.125 ) +
  geom_point( data=hits, aes( x=my_lon, y=my_lat, color=taxis_nearby ), alpha=.25, size=0.05 ) +
  scale_color_gradientn( colors=c( 'yellow', 'orange', 'red', 'dark red', 'black' ) )


trips_by_pickup_grp <- dplyr::group_by( taxis_cleaned, pickup_longitude_bin, pickup_latitude_bin )
trips_by_pickup_sum <- dplyr::summarise( trips_by_pickup_grp, Count=n(), Tip=mean( tip_percent ) )
dim( trips_by_pickup_sum )
sum( trips_by_pickup_sum$Count )
trips_by_pickup_sum

ggplot( trips_by_pickup_sum, aes( x=pickup_longitude_bin, y=pickup_latitude_bin, fill=Tip ) ) + 
  geom_tile() + 
  #geom_point( aes( colour=trips_by_dropoff_sum$Tip ) )
  ylab( "Latitude" ) +
  xlab( "Longitude" ) +
  ggtitle( "Tips by Pickup Lat/Lon" ) +
  scale_fill_gradientn( colors=c( 'black','dark red','red', 'orange','yellow' ) )
  #scale_fill_gradientn( colors=c( 'black','dark green','green', 'yellow','white' ) ) 


# tips by speed?
# bin the speeds by rounding
taxis_cleaned$speed_mph_bin <- round( taxis_cleaned$speed_mph, 0 )
taxis_cleaned$tip_percent_bin <- round( taxis_cleaned$tip_percent, 0 )

taxis_tips_and_mph_grp <- dplyr::group_by( taxis_cleaned, speed_mph_bin )
taxis_tips_and_mph_sum <- dplyr::summarise( taxis_tips_and_mph_grp, Tip=mean( tip_percent ) )
taxis_tips_and_mph_sum

ggplot( taxis_tips_and_mph_sum, aes( x=speed_mph_bin, y=Tip ) ) +
  geom_line() +
  ylab( "Tip %" ) +
  xlab( "Speed (MPH)" ) +
  ggtitle( "Mean Tips Per MPH Bin" )


