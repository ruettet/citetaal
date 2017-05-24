# stuff that you need to install/load
library(sp)
library(maptools)
library(spdep)
library(RANN)
library(maps)
library(mapdata)
library(dplyr)
library(lazyeval)
library(tidyr)

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

pivotAndPlot <- function(selected_data) {
	# calculate per location the amount of palatalization observations
	grouping = group_by(selected_data, loc)
	summarised = summarise(grouping, count=n(), latitude=mean(lat), longitude=mean(lon))

	# initialize the map
	plot(spdf, border="darkgrey")
	map("rivers", add=TRUE, col="cornflowerblue")

	# plot the palatalization observations on map
	points(summarised$longitude, summarised$latitude, pch=20, cex=(summarised$count / max(summarised$count)) + 1, col="red")
}

pivotAndProportionFactorAndPlot <- function(selected_data, grouper, a, b) {
	# build a lat/lon source
	latLon = as.data.frame(summarise(group_by(selected_data, loc), latitude=mean(lat), longitude=mean(lon)))
	
	# count the cite and not cite observations
	grouping = group_by_(selected_data, 'loc', grouper)
	summarised = summarise(grouping, count=n())
	
	# turn into wide format as to be able to take proportion of cite observations
	summarised.wide = spread_(summarised, grouper, 'count')
	summarised.wide[is.na(summarised.wide)] <- 0
	ratios = summarised.wide[a] / (summarised.wide[a] + summarised.wide[b])
	
	# add to lat/lon source
	latLon$ratios = ratios
	
	# initialize the map
	plot(spdf, border="darkgrey")
	map("rivers", add=TRUE, col="cornflowerblue")

	# plot the palatalization observations on map
	points(latLon$longitude, latLon$latitude, pch=20, cex=1, col=rgb(latLon$ratios[[a]], 0, 0))
}

# load the maps
spdf <- getCountries(c("NLD", "BEL"), level=1)

# read in the data
google_docs_url = "https://docs.google.com/spreadsheets/d/1JtJ6SSaQxI6Ayx2mAyM3rUy3u4wq0tkwnANRbfLo3nw/pub?gid=1587530388&single=true&output=csv"
citetaal = read.csv(url(google_docs_url), sep=",")

# fix the lat lon as string
# TODO: fix this in the datasource
citetaal$lat =  as.numeric(gsub(",", ".", as.vector(citetaal$lat)))
citetaal$lon =  as.numeric(gsub(",", ".", as.vector(citetaal$lon)))

############################################################################################
# KAART 1. Niet variationele kaarten van palatalisatie, jaar per jaar
############################################################################################

par(mfrow=c(2,3))
for (year in c(2012, 2013, 2014, 2015, 2016)) 
{
	# fetch the palatalization observations
	citetaal.pal = citetaal[which(citetaal$tweet_observation_variable == "palatalisation" & 
				      citetaal$tweet_observation_variant == "cite" &
				      citetaal$loc != "" & 
				      citetaal$year == year), ]
	pivotAndPlot(citetaal.pal)
}

############################################################################################
# KAART 3. Niet variationele kaart van palatalisatie "shtijl", jaar per jaar
############################################################################################

par(mfrow=c(2,3))
for (year in c(2012, 2013, 2014, 2015, 2016)) 
{
	# fetch the palatalization observations
	citetaal.stijlpal = citetaal[which(citetaal$tweet_observation_variable == "palatalisation" & 
				           citetaal$tweet_observation_variant == "cite" &
				           citetaal$shibboleth == "stijl" &
				           citetaal$loc != "" & 
				           citetaal$year == year), ]
	pivotAndPlot(citetaal.stijlpal)
}

############################################################################################
# KAART 4. Variationele kaart (cite/not cite) van lemma "stijl" in "style" betekenis
############################################################################################

# fetch the palatalization observations
citetaal.stijl.style = citetaal[which(citetaal$shibboleth == "stijl" & 
			              citetaal$meaning == "style" &
			              citetaal$loc != ""), ]
par(mfrow=c(1,1))
pivotAndProportionAndPlot(citetaal.stijl.style)

############################################################################################
# KAART 5. Variationele kaart (cite/not cite) van lemma "stijl" in "cool" betekenis
############################################################################################

# fetch the palatalization observations
citetaal.stijl.cool = citetaal[which(citetaal$shibboleth == "stijl" & 
			             citetaal$meaning == "cool" &
			             citetaal$loc != ""), ]
par(mfrow=c(1,1))
pivotAndProportionFactorAndPlot(citetaal.stijl.cool, 'tweet_observation_variant', 'cite', 'not cite')

############################################################################################
# KAART 7. Variationele kaart (hashtag/no hashtag) van "stijl" in "cool" betekenis cite vorm
############################################################################################

citetaal.stijl.cool.cite = citetaal[which(citetaal$shibboleth == "stijl" &
					  citetaal$meaning == "cool" &
					  citetaal$loc != "" &
					  citetaal$tweet_observation_variant == "cite"),]
par(mfrow=c(1,1))
pivotAndProportionFactorAndPlot(citetaal.stijl.cool.cite, 'stylisation', 'hashtag', 'no hashtag')


############################################################################################
# KAART 8. Variationele kaart (hashtag/no hashtag) van "stijl" in "cool" betekenis niet cite
############################################################################################

citetaal.stijl.cool.notcite = citetaal[which(citetaal$shibboleth == "stijl" &
			                     citetaal$meaning == "cool" &
					     citetaal$loc != "" &
					     citetaal$tweet_observation_variant == "not cite"),]
par(mfrow=c(1,1))
pivotAndProportionFactorAndPlot(citetaal.stijl.cool.notcite, 'stylisation', 'hashtag', 'no hashtag')

############################################################################################
# KAART 9. Variationele kaart intensification (yes/no) van "stijl" in "cool" betekenis cite
############################################################################################

par(mfrow=c(1,1))
pivotAndProportionFactorAndPlot(citetaal.stijl.cool.cite, 'intensification', 'yes', 'no')

############################################################################################
# KAART 10. Variationele kaart intensification (yes/no) van "stijl" in "cool" betekenis niet cite
############################################################################################

par(mfrow=c(1,1))
pivotAndProportionFactorAndPlot(citetaal.stijl.cool.notcite, 'intensification', 'yes', 'no')

