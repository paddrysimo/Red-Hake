load("/Volumes/Hybrid/Documents/School/Rutgers/R/neus.RData")
#This is only red hake and scallops in the Northeast US Spring trawls.

#Separate out hake from scallops:
reha <- neus$SCINAME == 'UROPHYCIS CHUSS'
sesc <- neus$SCINAME == 'PLACOPECTEN MAGELLANICUS'

#Call out neus to RH and SC, respectively
rhtot <- neus$SVSPP=='77'
sctot <- neus$SVSPP=='401'

#Create a table of total abundance by year
sc <- aggregate(list(total = neus$ABUNDANCE[sctot]), by=list(year = neus$YEAR[sctot]), FUN=sum)
#Plot it
plot(sc$year, sc$total, type="l", col="red", log="y")

rh <- aggregate(list(total = neus$ABUNDANCE[rhtot]), by=list(year = neus$YEAR[rhtot]), FUN=sum)
lines(rh$year, rh$total, type="l", col="blue", log="y")

#Call out neus to RH and SC in 1968
rh68 <- neus$SVSPP=='77' & neus$YEAR=='1968'
sc68 <- neus$SVSPP=='401' & neus$YEAR=='1968'

#Create a table of abundance by latitude for 1968
sc68lat <- aggregate(list(total = neus$ABUNDANCE[sc68]), by=list(lat = neus$LAT[sc68]), FUN=sum)
sc68lat

#Plot it
plot(sc68lat$total, sc68lat$lat, type="l", col="red", main="Scallop abundance by latitude in 1968", ylab="Latitude", xlab="Abundance")

#RH abundance by lat, 1968
rh68lat <- aggregate(list(total = neus$ABUNDANCE[rh68]), by=list(lat = neus$LAT[rh68]), FUN=sum)

#Plot it
lines(rh68lat$total, rh68lat$lat, type="l", col="blue")

# Whoops. Way more abundant hake. Flip the order:
plot(rh68lat$total, rh68lat$lat, type="l", col="blue", main="Abundance by latitude in 1968", ylab="Latitude", xlab="Abundance")
lines(sc68lat$total, sc68lat$lat, type="l", col="red")

#1969?
rh69 <- neus$SVSPP=='77' & neus$YEAR=='1969'
sc69 <- neus$SVSPP=='401' & neus$YEAR=='1969'
sc69lat <- aggregate(list(total = neus$ABUNDANCE[sc69]), by=list(lat = neus$LAT[sc69]), FUN=sum)
rh69lat <- aggregate(list(total = neus$ABUNDANCE[rh69]), by=list(lat = neus$LAT[rh69]), FUN=sum)
plot(rh69lat$total, rh69lat$lat, type="l", col="blue", main="Abundance by latitude in 1969", ylab="Latitude", xlab="Abundance")
lines(sc69lat$total, sc69lat$lat, type="l", col="red")

# # # # # # # # # # # # # # # # 
#What's the relationship between biomass and abundance?
rhmasabu <- aggregate(list(abundance = neus$ABUNDANCE[rhtot]), 
                      by=list(biomass = neus$BIOMASS[rhtot]), FUN=sum, NA.rm=T)
scmasabu <- aggregate(list(abundance = neus$ABUNDANCE[sctot]), 
                      by=list(biomass = neus$BIOMASS[sctot]), FUN=sum, NA.rm=T)

#Plot biomass against abundance
plot(scmasabu$biomass, scmasabu$abundance, 
     main='Sea Scallop', ylab='Abundance', xlab='Biomass', pch=20, col='red')
abline(lm(scmasabu$abundance~scmasabu$biomass), col="red")

#Subset data from scmasabu with values greater than 0 to get logs without taking log(0)
scmasabu.pos <- scmasabu[scmasabu[,"abundance"]>0 & scmasabu[,"biomass"]>0,]
attach(scmasabu.pos)
plot(log10(biomass), log10(abundance), 
     main='Sea Scallops', ylab='Log Abundance', xlab='Log Biomass', pch=20, col='red')
#lm from subset:
abline(lm(log10(biomass)~log10(abundance), data=scmasabu.pos))

rhmasabu.pos <- rhmasabu[rhmasabu[,"abundance"]>0 & rhmasabu[,"biomass"]>0,]
attach(rhmasabu.pos)
plot(log10(biomass), log10(abundance), 
     main='Red Hake', ylab='Log Abundance', xlab='Log Biomass', pch=20, col='blue')
#lm from subset:
abline(lm(log10(biomass)~log10(abundance), data=scmasabu.pos))


# # # # # # # MAPS # # # # # # # # #
#Generate a map of the Northeast US shelf
library(ggplot2)
library(ggmap)
NE <- get_map(location = c(lon = -71, lat = 40), zoom = 6, maptype = 'satellite')

#Map bottom temperatures from all trawls in all years
bottemp <- ggmap(NE)+ geom_point(data=neus, aes(x=LON, y=LAT, col=BOTTEMP.x))+ 
  scale_colour_gradient2(mid="#0000FF", high="#FF9933")+ ggtitle("Bottom Temperature")
bottemp

# # # # # # Scatterplots # # # # # # # 
library(scatterplot3d)
y1968 <- neus[neus[,"YEAR"]==1968,]

# # # # # # Troubleshooting# # # # # # 
dput(rhmasabu.pos)
head(rhmasabu.pos)
sample(scmasabu.pos)
head(neus)
bioabu <-sample(mini[1:500,])
attach(bioabu)
plot(log10(bioabu$biomass), log10(bioabu$abundance), 
     ylab='Log Abundance', xlab='Log Biomass', pch=20, col='darkred')
#lm from subset:
abline(lm(log10(biomass)~log10(abundance), data=bioabu))
plot(bioabu$biomass, bioabu$abundance)
