library( ggplot2 )

taxis_raw <- read.csv( "data/green-tripdata-2015-09-raw.csv" )

# Question 2
# 
# Plot a histogram of the number of the trip distance (“Trip Distance”).
# Report any structure you find and any hypotheses you have about that structure.
# 1) Trips of 600 miles are not interborough 

#hist( taxis_raw$Trip_distance ) # 600 mile trips? Nuke'em
ggplot( taxis_raw, aes( x=Trip_distance ) ) + geom_histogram()
summary( taxis_raw$Trip_distance )

# nuke outliers: what's reasonable to keep? w/in n stds of mean?
dist_sd <- sd( taxis_raw$Trip_distance )
dist_sd
dist_mean <- mean( taxis_raw$Trip_distance )
dist_mean

# how many get discarded?
rows_before <- dim( taxis_raw )[ 1 ]
#summary( taxis_raw$Trip_distance )
taxis_filtered <- dplyr::filter( taxis_raw, Trip_distance < dist_mean + 3 * dist_sd )
rows_after <- dim( taxis_filtered )[ 1 ]
summary( taxis_filtered$Trip_distance )
rows_before - rows_after # rows removed > 3 sd

#taxis_filtered <- dplyr::arrange( taxis_filtered, desc( Trip_distance ) ) 
#head( taxis_filtered$Trip_distance )
#summary( taxis_filtered$Trip_distance )
#hist( taxis_filtered$Trip_distance )
ggplot( taxis_filtered, aes( x=Trip_distance ) ) + geom_histogram()

# 1st things 
# TODO: What are my obs aboutt this structure, and hypothesis as to why it's like that...
# TODO: How does price and distance correlate?  Does higher price discourage long trips (D'uh!)



