load("~/Documents/R/trawl_allregionsforprojections_2014-07-01.RData")

# Call out and separate REd HAke spring and fall trawls from the NEUS
reha.s <- dat$region == 'NEFSC_NEUSSpring' & dat$spp == 'UROPHYCIS CHUSS'
reha.f <- dat$region == 'NEFSC_NEUSFall' & dat$spp == 'UROPHYCIS CHUSS'

# Make a table of total wtcpue by year for both spring and fall for reha
reha.sum.s <- aggregate(list(total = dat$wtcpue[reha.s]), by=list(year = dat$year[reha.s]), FUN=sum)
reha.sum.f <- aggregate(list(total = dat$wtcpue[reha.f]), by=list(year = dat$year[reha.f]), FUN=sum)

# Same for SEa SCallop
sesc.s <- dat$region == 'NEFSC_NEUSSpring' & dat$spp == 'PLACOPECTEN MAGELLANICUS'
sesc.f <- dat$region == 'NEFSC_NEUSFall' & dat$spp == 'PLACOPECTEN MAGELLANICUS'
sesc.sum.s <- aggregate(list(total = dat$wtcpue[sesc.s]), by=list(year = dat$year[sesc.s]), FUN=sum)
sesc.sum.f <- aggregate(list(total = dat$wtcpue[sesc.f]), by=list(year = dat$year[sesc.f]), FUN=sum)

# Plot rehasumf and sescsums against each other
plot(sesc.sum.s, type="l", col="darkgreen", ylim=c(0, 1600))
points(reha.sum.f, type="l", col="darkorange")
# Looks like there might be some correlation, but this is too broad of a scale, and I don't know
# how to fit a model to this yet. Let's go to a smaller scale.
#- - - - - - - - - - - - - - - 

# Call out reha and sesc in 1968
sesc68s <- dat$region == 'NEFSC_NEUSSpring' & dat$spp == 'PLACOPECTEN MAGELLANICUS' & dat$year == '1968'
sesc68f <- dat$region == 'NEFSC_NEUSFall' & dat$spp == 'PLACOPECTEN MAGELLANICUS' & dat$year == '1968'
reha68s <- dat$region == 'NEFSC_NEUSSpring' & dat$spp == 'UROPHYCIS CHUSS' & dat$year == '1968'
reha68f <- dat$region == 'NEFSC_NEUSFall' & dat$spp == 'UROPHYCIS CHUSS' & dat$year == '1968'

library("maps", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("mapdata", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
map("worldHires","USA", xlim=c(-80, -60), ylim=c(30, 50), col="darkgreen",fill=TRUE, bg="lightblue")
map.axes()

points(dat$lat[sesc68f], dat$lon[sesc68f], col="red")

#---------------------------------------

# Change "total" column in sesc.sum.s and reha.sum.f to their respective species totals
colnames(sesc.sum.s) <- c("year", "sesc.wtcpue")
colnames(reha.sum.f)  <- c("year", "reha.wtcpue")

# Merge reha.sum.f and sesc.sum.s, setting empty values to NA
hasc.sum  <- merge(reha.sum.f, sesc.sum.s, all=TRUE)

# The cross correlation function (ccf) of two time series is the product moment correlation as a function of lag, or time
# offset, between the series. Where the histogram peaks, either positively or negatively, is the magnitude of the offset.
#ccf(hasc.sum$reha.wtcpue, hasc.sum$sesc.wtcpue, na.action = na.exclude)

#require(zoo)
#x <- zoo(hasc.sum$sesc.wtcpue, order.by=hasc.sum$year)
#y <- lag(hasc.sum$reha.wtcpue, -2, na.pad = TRUE)

# - - - - - - - - - - - - - - - - - - -
#shrink dat to just these elements:
yr.lat.lon.wt <- dat[,c("year", "lat", "lon", "spp", "wtcpue")]

# Make a table of each species
sca <- yr.lat.lon.wt[grep("PLACOPECTEN MAGELLANICUS", yr.lat.lon.wt$spp),]
red  <- yr.lat.lon.wt[grep("UROPHYCIS CHUSS", yr.lat.lon.wt$spp),]

#Merge the two tables
sca.red <- merge(sca, red, all=TRUE)

scwt68  <- sca[grep("1968", sca$year),]
sc.wt68 <- scwt68[,c("lat", "lon", "wtcpue")]

rhwt68  <- red[grep("1968", red$year),]
rh.wt68 <- rhwt68[,c("lat", "lon", "wtcpue")]

#- - - - - - - - - - - - - - - - - -
map("worldHires","USA", xlim=c(-80, -60), ylim=c(30, 48), 
    col="#003300",fill=TRUE, mar=c(0,0,0,0), bg="#FFFFFF")
points(x=sc.wt68$lon, y=sc.wt68$lat, pch=21, bg="sc.wt68$wtcpue")
library(ggplot2)
library(ggmap)


SCMAP68 <- ggmap(NE)+ geom_point(data=sc.wt68, aes(x=lon, y=lat, col=wtcpue, size=wtcpue))+ 
  scale_colour_gradient2(mid="#ccFF00", high="#00FF00")+ ggtitle("Sea Scallops, 1968")
SCMAP68
RHMAP68 <- ggmap(NE)+ geom_point(data=rh.wt68, aes(x=lon, y=lat, col=wtcpue, size=wtcpue))+ 
  scale_colour_gradient2(mid="#CC6600", high="#FFFF00") + ggtitle("Red Hake, 1968")
RHMAP68

#- - - - - - - - - - - - - 

NE <- get_map(location = c(lon = -71, lat = 40), zoom = 6, maptype = 'satellite')
bottemp <- ggmap(NE)+ geom_point(data=neus, aes(x=LON, y=LAT, col=BOTTEMP.x))+ 
  scale_colour_gradient2(mid="#0000FF", high="#FF9933")+ ggtitle("Bottom Temperature")
bottemp
# - - - - - - - - - - 
hist(neus$LENGTH[hak & neus$YEAR==1975], breaks=40)
hist(neus$LENGTH[hak], breaks=40)
sums = aggregate(list(total = dat$wtcpue[sumfl]), by=list(year = dat$year[sumfl]), FUN=sum)