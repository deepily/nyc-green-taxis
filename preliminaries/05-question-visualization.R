library( plotly )
#packageVersion( 'plotly' )
library( ggmap )

head( airport_rides )

# original bounding box from: http://minimaxir.com/2015/11/nyc-ggplot2-howto/
# min_lat <- 40.5774
# max_lat <- 40.9176
# min_long <- -74.15
# max_long <- -73.7004

# calculate my own bounding box
min_lat <- min( airport_rides$Pickup_latitude )
min_lat
max_lat <- max( airport_rides$Pickup_latitude )
max_lat
min_long <- min( airport_rides$Pickup_longitude )
min_long
max_long <- max( airport_rides$Pickup_longitude )
max_long

plot <- ggplot( airport_rides, aes( x=Pickup_longitude, y=Pickup_latitude ) ) +
  geom_point( size=0.06 ) +
  scale_x_continuous( limits=c( min_long, max_long ) ) +
  scale_y_continuous( limits=c( min_lat, max_lat ) )

#png("nyc-taxi-2.png", w=600, h=600)
plot

nyc_map <- qmap( location="New York City" ) 
nyc_map

airport_rides$Tip_percent <- airport_rides$Tip_amount / airport_rides$Total_amount * 100

row_count_all <- dim( airport_rides )[ 1 ]
colnames( airport_rides )
str( airport_rides$Tip_percent )
ggplot( airport_rides, aes( x=Tip_percent ) ) + geom_histogram()
row_count_tips <- dim( dplyr::filter( airport_rides, Tip_amount > 0 ) )[ 1 ]
# how many airport fares tip?
row_count_tips / row_count_all * 100


airport_rides_with_tips <- dplyr::filter( airport_rides, Tip_amount > 0 )
ggplot( airport_rides_with_tips, aes( x=Tip_percent ) ) + geom_histogram()

airport_tip_mean <- mean( airport_rides_with_tips$Tip_percent )
airport_tip_sd <- sd( airport_rides_with_tips$Tip_percent )

airport_rides_with_tips_clipped <- dplyr::filter( airport_rides_with_tips, Tip_percent < airport_tip_mean + 3 * airport_tip_sd )
ggplot( airport_rides_with_tips_clipped, aes( x=Tip_percent ) ) + geom_histogram()


nyc_map + 
  geom_point( aes( x=Pickup_longitude, y=Pickup_latitude, colour=Tip_percent ) , data=airport_rides_with_tips_clipped, alpha=0.4 ) +
  scale_colour_gradient2() #scale_colour_gradient( low="red", high="green" )

# # leaflet experiment: Works, moved into Q3 to highlight invalid distance values in airport fares
# library( leaflet )
# 
# meters_in_mile <- 1609.344
# newark_sum <- dplyr::filter( airport_rides_sum, Airport == "Newark" )
# jfk_sum <- dplyr::filter( airport_rides_sum, Airport == "JFK" )
# 
# leaflet( nyc_map ) %>%
#   addTiles() %>%
#   addMarkers( lng=-73.7781, lat=40.6413, popup=paste( "JFK Mean distance ==", jfk_sum$Distance, "?!?", sep=" " ) ) %>%
#   addCircles( lng=-73.7781, lat=40.6413, radius=( meters_in_mile * jfk_sum$Distance  ) ) %>%
#   
#   addMarkers( lng=-74.1745, lat=40.6895, popup=paste( "Newark Mean distance ==", newark_sum$Distance, "?!?", sep=" " ) ) %>%
#   addCircles( lng=-74.1745, lat=40.6895, radius=( meters_in_mile * newark_sum$Distance  ) )





# BLAH!
# # Takes forever to render because it's a million+ records being calc'd on *ONE* core
# dim( taxis_cleaned )
# ggplot( taxis_cleaned, aes( x=speed_mph, y=tip_percent ) ) +
#   geom_point() +
#   ylab( "Tip %" ) +
#   xlab( "Speed (MPH)" ) +
#   ggtitle( "Mean Tips Per MPH" )

# # BLAH!
# # bin the tips too
# taxis_tips_and_mph_grp <- dplyr::group_by( taxis_cleaned, speed_mph_bin, tip_percent_bin )
# taxis_tips_and_mph_sum <- dplyr::summarise( taxis_tips_and_mph_grp, Count=n() )
# taxis_tips_and_mph_sum
# 
# ggplot( taxis_tips_and_mph_sum, aes( x=speed_mph_bin, y=Count ) ) +
#   geom_point() +
#   ylab( "Tip %" ) +
#   xlab( "Speed (MPH)" ) +
#   ggtitle( "Tips Per MPH Bin" )


