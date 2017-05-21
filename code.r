# stuff that you need to install/load
library(sp)
library(maptools)
library(spdep)
library(RANN)
library(maps)
library(mapdata)
library(xtable)

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
			       citetaal$tweet_observation_variant == "cite"), ]

# initialize the map
plot(spdf, border="darkgrey", main="Observations of palatalization are mainly in Flanders")
map("rivers", add=TRUE, col="cornflowerblue")

# plot the palatalization observations on map
points(citetaal.pal$lon, citetaal.pal$lat, pch=20, col="red")

############################################################################################
# Niet variationele kaarten van palatalisatie "shtijl"
############################################################################################

# fetch the stijl-palatalization observations
citetaal.pal.stijl = citetaal[ which(citetaal$tweet_observation_variable == "palatalisation" && citetaal$tweet_observation_), ]

# initialize the map
plot(spdf, border="darkgrey", main="Observations of palatalization are mainly in Flanders")
map("rivers", add=TRUE, col="cornflowerblue")

# plot the palatalization observations on map
points(citetaal.pal$lon, citetaal.pal$lat, pch=20, col="red")

############################################################################################
# Niet variationele kaarten van palatalisatie "shtijl", jaar per jaar
############################################################################################

############################################################################################
# Niet variationele kaarten van intensificatie met "vies"
############################################################################################
