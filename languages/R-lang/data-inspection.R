library(dslabs)
library(ggplot2)
library(dplyr)
library(ggthemes)
data(heights)
View(heights)

dframe <- heights %>% filter( sex == "Female" )  # data frame for inspection

### comparing quantiles ###

# generate quantiles for the data
p <- seq( 0, 1, 0.1 )
q_data <- quantile( dframe$height, probs=p )

# generate quantiles for a normal distribution with the same mean and sd
mu <- mean( dframe$height )
sigma <- sd( dframe$height )
q_norm <- qnorm( p, mean=mu, sd=sigma )

# compare data quantiles to those for the normal distribution
plot( q_norm, q_data, xlim=c(60,70), ylim=c(60,70) )
abline( 0, 1 )
grid()


### comparing densities ###
#q <- seq( -2.5*sigma+mu, 2.5*sigma+mu, length.out=100 )
q <- seq( min(dframe$height), max(dframe$height), length.out=100 )
d_norm <- dnorm( q, mean=mu, sd=sigma )
dframe_norm <- data.frame( height=q, density=d_norm )

## using ggplot2
p <- ggplot( dframe, aes(x=height) ) + 
    geom_line( data=dframe_norm, aes(x=height,y=density), size=1, color="#222222", linetype="dotted" ) +
    geom_density( color="#000000", fill="#00c0ff", alpha=0.3, size=1.2 ) +
    xlab( "self-reported female heights (in inches)" ) +
    theme_economist()

## native R
# generate smoothed density plot for the data
d_data <- density( dframe$height )
plot( d_data )

# generate density plot for the normal distribution
lines( q, d_norm, type="l", col="#00c0ff" )
grid()


print( p )  # print ggplot2 graph