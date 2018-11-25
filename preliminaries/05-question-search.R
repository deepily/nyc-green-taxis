





# means of mph by day/hour
# build lookup table for mph by day and hour
taxis_cleaned <- fread( "data/green-tripdata-2015-09-cleaned.csv" )
str( taxis_cleaned )
trips_mph_day_n_hour_grp <- dplyr::group_by( taxis_cleaned, pickup_day_of_week, pickup_hour )
trips_mph_day_n_hour_grp
trips_mph_day_n_hour_sum <- dplyr::summarise( trips_mph_day_n_hour_grp, mph=mean( speed_mph ) )
trips_mph_day_n_hour_sum


misses <- fread( "data/taxis-nearby-misses-2.csv" )
hits   <- fread( "data/taxis-nearby-hits-2.csv" )
miss_count <- dim( misses )[ 1 ]
 hit_count <- dim( hits )[ 1 ]
hit_count / ( hit_count + miss_count ) * 100

 
nyc_map <- qmap( location="New York City" ) 
nyc_map

# original bounding box from: http://minimaxir.com/2015/11/nyc-ggplot2-howto/
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

# # calculate my own bounding box
# min_lat <- min( airport_rides$Pickup_latitude )
# min_lat
# max_lat <- max( airport_rides$Pickup_latitude )
# max_lat
# min_long <- min( airport_rides$Pickup_longitude )
# min_long
# max_long <- max( airport_rides$Pickup_longitude )
# max_long
summary( hits$taxis_nearby )

nyc_map_ggmap <- get_map( location="New York City" ) 
ggmap( nyc_map_ggmap )

plot <- ggplot( data=misses, aes( x=lon, y=lat ) ) +
  #ggmap( nyc_map_ggmap ) +
  #geom_point( data=misses, aes( x=lon, y=lon ), alpha=1.0, size=1 ) +
  geom_point( size=0.06 ) +
  #geom_point( data=hits, aes( x=my_lon, y=my_lat, color=taxis_nearby ), alpha=.125, size=0.05 ) +
  
  #scale_color_gradientn( colors=c( 'dark red','red', 'orange','yellow' ) ) #+
  # scale_x_continuous( limits=c( min_long, max_long ) ) +
  # scale_y_continuous( limits=c( min_lat, max_lat ) )
plot


#ggplot( data=misses, aes( x=lon, y=lat ) ) +
ggmap( nyc_map_ggmap ) +
  geom_point( data=misses, aes( x=lon, y=lon ), alpha=1.0, size=1 ) +
  #geom_point( size=0.06 ) +
  #geom_point( data=hits, aes( x=my_lon, y=my_lat, color=taxis_nearby ), alpha=.125, size=0.05 ) +
  
  #scale_color_gradientn( colors=c( 'dark red','red', 'orange','yellow' ) ) #+
  scale_x_continuous( limits=c( min_long, max_long ) ) +
  scale_y_continuous( limits=c( min_lat, max_lat ) )
  
# hits and misses
ggmap( nyc_map_ggmap ) + 
  geom_point( data=misses, aes( x=lon, y=lat ), alpha=.125 ) +
  geom_point( data=hits, aes( x=my_lon, y=my_lat, color=taxis_nearby ), alpha=.125, size=0.05 ) +
  scale_color_gradientn( colors=c( 'dark red','red', 'orange','yellow' ) )

# just misses
ggmap( nyc_map_ggmap ) + 
  geom_point( data=misses, aes( x=lon, y=lat ), alpha=.125 ) +
  ggtitle( "Ride Sharing MISSes: No Taxis w/in 10 mins and 90° of Bearing Δ" )

# hits, all colors, sizes and alphas equal
ggmap( nyc_map_ggmap ) + 
  #geom_point( data=misses, aes( x=lon, y=lat ), alpha=.125 ) +
  geom_point( data=hits, aes( x=my_lon, y=my_lat, color=taxis_nearby ), alpha=.25, size=0.05 ) +
  scale_color_gradientn( colors=c( 'yellow', 'orange', 'red', 'dark red', 'black' ) ) +
  ggtitle( "Ride Sharing HITs: Taxis w/in 10 mins and 90° of Bearing Δ" )

ggplot( hits, aes( x=taxis_nearby ) ) + geom_histogram()

# what's the distribution of taxis nearby?
ggplot( hits, aes( x=taxis_nearby ) ) + 
    geom_histogram( binwidth=2 )

summary( hits$taxis_nearby )




taxis_nearby_stats <- summary( hits$taxis_nearby )
taxis_nearby_stats
names( taxis_nearby_stats )

str( taxis_nearby_stats[ 1 ] )
taxis_nearby_stats[ 1 ]

min( hits$taxis_nearby )
max( hits$taxis_nearby )
median( hits$taxis_nearby )
# first_q <- quantile( hits$taxis_nearby, 0.25, names=FALSE )
# names( quantile( first_q, 0.25 ) )
# class( first_q[ "25%" ] )
# first_q[ "25%" ]
# first_q[ 1 ]

# this allows numeric value output only!
#summary( hits$taxis_nearby )
q_min <- 1
#q_min
q_1st <- quantile( hits$taxis_nearby, 0.25, names=FALSE )[ 1 ]
#q_1st
q_2nd <- quantile( hits$taxis_nearby, 0.50, names=FALSE )[ 1 ]
#q_2nd
q_3rd <- quantile( hits$taxis_nearby, 0.75, names=FALSE )[ 1 ]
#q_3rd
q_max <- max( hits$taxis_nearby )
#q_max

min_hits <- dplyr::filter( hits, taxis_nearby == 1)
#dim( min_hits )

q_1st_hits <- dplyr::filter( hits, taxis_nearby > 1 & taxis_nearby <= q_1st )
#dim( q_1st_hits )

q_2nd_hits <- dplyr::filter( hits, taxis_nearby > q_1st & taxis_nearby <= q_2nd )
#dim( q_2nd_hits )

q_3rd_hits <- dplyr::filter( hits, taxis_nearby > q_2nd & taxis_nearby <= q_3rd )
#dim( q_3rd_hits )

q_4th_hits <- dplyr::filter( hits, taxis_nearby > q_3rd & taxis_nearby <= q_4th )
#dim( q_4th_hits )

ggmap( nyc_map_ggmap ) + 
  geom_point( data=misses, aes( x=lon, y=lat ), alpha=.075, size=0.01, colour="black" ) +
  geom_point( data=min_hits, aes( x=my_lon, y=my_lat ), alpha=.5, size=0.02, colour="dark red" ) +
  geom_point( data=q_1st_hits, aes( x=my_lon, y=my_lat ), alpha=.25, size=0.02, colour="red" ) +
  geom_point( data=q_2nd_hits, aes( x=my_lon, y=my_lat ), alpha=.125, size=0.02, colour="orange" ) + 
  geom_point( data=q_3rd_hits, aes( x=my_lon, y=my_lat ), alpha=.075, size=0.02, colour="yellow" ) +
  geom_point( data=q_4th_hits, aes( x=my_lon, y=my_lat ), alpha=.050, size=0.02, colour="green" ) +
  #scale_color_gradientn( colors=c( 'yellow', 'orange', 'red', 'dark red', 'black' ) ) +
  ggtitle( "Ride Sharing: Taxis w/in 10 mins and 90° of Bearing Δ" )
