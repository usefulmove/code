library(ggplot2)
library(ggthemes)

homicides <- read.csv("/Volumes/persnl/repos/dedmonds/code/oakland-homicides/OaklandHomicides2017.csv")
    View(homicides)
  
names(homicides) <- c("demographics","homicides","population","rate","year")
  
# R language native
#barplot(homicides$homicides, names.arg=homicides$demographics,
#    col="#00c0ff",main="Homicides in Oakland (2017)",xlab="demographics of victims")

# ggplot2
p <- ggplot( data=homicides, aes( x=demographics ) ) +
    geom_bar( aes( y=population/1e3 ), stat="identity", fill="grey", alpha=0.6 ) +
    geom_bar( aes( y=homicides ), stat="identity", fill="#00c0ff" ) +
    xlab( "demographics of victims" ) +
    ylab( "homicides (blue) and population in thousands (grey)" ) +
    ggtitle( "Homicides in Oakland, California (2017)" ) +
    theme_economist()
print(p)
