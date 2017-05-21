# stuff that you need to install/load
library(sp)
library(maptools)
library(spdep)
library(RANN)
library(maps)
library(mapdata)
library(dplyr)

## some functions that are needed for non-us maps
loadGADM <- function (fileName, level = 0, ...) {
	print(paste("./", fileName, "_adm", level, ".RData", sep = ""))
 	load(paste("./", fileName, "_adm", level, ".RData", sep = ""))
	gadm
}

changeGADMPrefix <- function (GADM, prefix) {
	GADM <- spChFIDs(GADM, paste(prefix, row.names(GADM), sep = "_"))
	GADM
}

loadChangePrefix <- function (fileName, level = 0, ...) {
	theFile <- loadGADM(fileName, level)
	theFile <- changeGADMPrefix(theFile, fileName)
	theFile
}

getCountries <- function (fileNames, level = 0, ...) {
	polygon <- sapply(fileNames, loadChangePrefix, level)
	polyMap <- do.call("rbind", polygon)
	polyMap
}

# load the maps
spdf <- getCountries(c("NLD", "BEL"), level=1)

# read in the data
citetaal = read.csv(url("https://raw.githubusercontent.com/ruettet/citetaal/master/cite.csv"), sep=";")

# fix the lat lon as string
citetaal$lat =  as.numeric(gsub(",", ".", as.vector(citetaal$lat)))
citetaal$lon =  as.numeric(gsub(",", ".", as.vector(citetaal$lon)))

############################################################################################
# Niet variationele kaarten van palatalisatie in het algemeen
############################################################################################

# fetch the palatalization observations
citetaal.pal = citetaal[ which(citetaal$tweet_observation_variable == "palatalisation" & 
			       citetaal$tweet_observation_variant == "cite" &
			       citetaal$loc != ""), ]

# calculate per location the amount of palatalization observations
pallocgroup = group_by(citetaal.pal, loc)
pallocs = summarise(pallocgroup, count=n(), latitude=mean(lat), longitude=mean(lon))

# initialize the map
plot(spdf, border="darkgrey")
map("rivers", add=TRUE, col="cornflowerblue")

# plot the palatalization observations on map
points(pallocs$longitude, pallocs$latitude, pch=20, cex=(pallocs$count / max(pallocs$count)) + 1, col="red")

############################################################################################
# Niet variationele kaarten van palatalisatie "shtijl"
############################################################################################

# fetch the palatalization observations
citetaal.stijlpal = citetaal[ which(citetaal$tweet_observation_variable == "palatalisation" & 
			       citetaal$tweet_observation_variant == "cite" &
			       grepl("ijl", citetaal$tweet_observation) &
			       citetaal$loc != ""), ]

# calculate per location the amount of palatalization observations
stijlpallocgroup = group_by(citetaal.stijlpal, loc)
stijlpallocs = summarise(stijlpallocgroup, count=n(), latitude=mean(lat), longitude=mean(lon))

# initialize the map
plot(spdf, border="darkgrey")
map("rivers", add=TRUE, col="cornflowerblue")

# plot the palatalization observations on map
points(stijlpallocs$longitude, stijlpallocs$latitude, pch=20, 
       cex=(stijlpallocs$count / max(stijlpallocs$count)) + 1, col="red")

############################################################################################
# Niet variationele kaarten van palatalisatie "shtijl", jaar per jaar
############################################################################################

par(mfrow=c(2,3))
for (year in c(2012, 2013, 2014, 2015, 2016)) 
{
	# fetch the palatalization observations
	citetaal.stijlpal = citetaal[ which(citetaal$tweet_observation_variable == "palatalisation" & 
				       citetaal$tweet_observation_variant == "cite" &
				       grepl("ijl", citetaal$tweet_observation) &
				       citetaal$loc != "" & 
				       citetaal$year == year
				      ), ]

	# calculate per location the amount of palatalization observations
	stijlpallocgroup = group_by(citetaal.stijlpal, loc)
	stijlpallocs = summarise(stijlpallocgroup, count=n(), latitude=mean(lat), longitude=mean(lon))

	# initialize the map
	plot(spdf, border="darkgrey", main=year)
	map("rivers", add=TRUE, col="cornflowerblue")

	# plot the palatalization observations on map
	points(stijlpallocs$longitude, stijlpallocs$latitude, pch=20, 
	       cex=(stijlpallocs$count / max(stijlpallocs$count)) + 1, col="red")
}

############################################################################################
# Niet variationele kaarten van intensificatie met "vies"
############################################################################################

# fetch the palatalization observations
citetaal.viesint = citetaal[ which(citetaal$tweet_observation_variable == "intensification" & 
			       citetaal$tweet_observation_variant == "cite" &
			       grepl("vies", citetaal$tweet_observation) &
			       citetaal$loc != ""), ]

# calculate per location the amount of vies intensification observations
viesintlocgroup = group_by(citetaal.viesint, loc)
viesintlocs = summarise(viesintlocgroup, count=n(), latitude=mean(lat), longitude=mean(lon))

# initialize the map
par(mfrow=c(1,1))
plot(spdf, border="darkgrey")
map("rivers", add=TRUE, col="cornflowerblue")

# plot the vies observations on map
points(viesintlocs$longitude, viesintlocs$latitude, pch=20, 
       cex=(viesintlocs$count / max(viesintlocs$count)) + 1, col="red")
