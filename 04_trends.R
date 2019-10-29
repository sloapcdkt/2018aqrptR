
################################################
## Generate Ozone and PM trend graphs         ##
################################################

## define consistent color palatte
colors <- RColorBrewer::brewer.pal(8, "Set1")[-6]
colors <- c(colors, colors[c(2,6,7)]) # reuse morro, red, carrizo colors for cdf, mesa, oso
sites <- c( "San Luis Obispo", "Morro Bay", "NRP",
            "Paso Robles", "Atascadero",  "Red Hills", "Carrizo Plains",
            "CDF", "Mesa2", "Oso Flaco")
colors <- data.frame(sites=sites, colors=colors, stringsAsFactors = FALSE)
rownames(colors) <- c("slo", "morro", "nrp", "paso", "atas", "red", "carrizo", "cdf", "mesa", "oso")
rm(sites)

################################################
### ozone trends graph (Fig 7)               ###
################################################

## load 2009 to 2018 data from AQS AMP501 Raw Data extract
source('00_AQSloader.R')
ozone<-load.aqs("AMP501_1772053-0.txt")
names(ozone)<-c("date", "slo", "morro", "nrp", "paso", "atas_old",
                "atas", "red", "carrizo")

## merge old and new atascadero locations
ozone$atas[which(is.na(ozone$"atas"))]<-ozone$"atas_old"[which(is.na(ozone$"atas"))]
ozone$"atas_old"<-NULL

## summarize data: number of hours each year >= 65
d <- apply(ozone[, -1], 2, 
           function(a) tapply(a, format(ozone$date, "%Y"), function(x) sum(x>=65, na.rm=T)))
d <- as.data.frame(d)
d$year <- row.names(d)

## create plot
svglite::svglite("fig7.svg", width = 8, height = 6, 
    pointsize = 10)

## set up empty plot
par(mar = c(5, 2, 4, 4), mfrow = c(1, 1))
plot(d$year, d$red, "n",
     xlim = as.numeric(range(d$year)) + c(-2, 0),
     ylim = c(-50, 800),
     xlab = "", 
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")

axis(1, d$year)
at <- range(d[, -8], na.rm=TRUE)
ticks <- seq(0, 1400, by = 200)
at <- c(at, ticks[which(ticks > min(at) & ticks < max(at))])
axis(4, at = at, las = 2)

## add trend lines
for (i in names(d)[1:7]) {
  lines(d$year, d[, i], col=colors[i, "colors"], lwd = 2)
}

## labels
text(x = min(d$year), y = d[1, 6:7], 
     pos = 2,
     labels = colors$sites[6:7],
     font = 2,
     col = colors$colors[6:7],
     cex = 1)

text(x = min(d$year), y = c(0, -40, 40, 120, 80), 
     pos = 2,
     labels = colors$sites[1:5],
     font = 2,
     col = colors$colors[1:5],
     cex = 1)

dev.off()


##########################################
### Ozone Design value Graph, Figure 8  ##
##########################################

## load design values from AQS rpt
dv <- read.csv("AMP480_1772133_DV_Ozone.csv", comment.char="#", header=F, as.is=T)
dv <- dv[, c(4, 7, 52, 53)]
names(dv) <- c("site", "year", "value", "valid")
dv$value[dv$valid == "N"] <- NA   ## remove design values that don't meet completeness requirements
dv$value <- dv$value*1000       ## convert ppm to ppb
dv <- reshape2::dcast(dv[, -4], year ~ site)
names(dv) <- c("year", "paso", "slo", "morro", "nrp", "atas", "red", "carrizo")

## can't get AQS to include old Atascadero site, so grab data from last year's report
atas <- read.csv("AMP480_1673066-0.txt", comment.char="#", header=F, as.is=T)
atas <- atas[c(3,6,35,36)]
names(atas) <- c("site", "year", "value", "valid")
atas <- atas[atas$site == 8001 & atas$year > 2008, ]
atas$value[atas$valid == "N"] <- NA   ## remove design values that don't meet completeness requirements
dv$atas[dv$year >= 2009 & dv$year <= 2015] <- 1000*atas$value[atas$year >= 2009 & atas$year <= 2015]

## reorder colums to match "ozone" dataframe
dv <- dv[, c("year", names(ozone)[-1])]

## create plot
svglite::svglite("fig8.svg", width = 8, height = 6, 
    pointsize = 10)

## set up empty plot
par(mar = c(5, 3, 4, 3))
plot(dv$year, dv$red, "n",
     xlim = range(dv$year) + c(0,2),
     ylim = c(50, 85),
     xlab = "", 
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")

## add ozone std
std <- data.frame(year = c(min(dv$year), 2015, 2015, max(dv$year)), value = c(75, 75, 70, 70))
lines(std$year, std$value, col = "red", lty = 2)

## add axes
axis(1, dv$year)

at <- round(range(dv[, -1], na.rm=TRUE), 1)
ticks <- seq(0, 100, by = 10)
at <- c(at, ticks[which(ticks > min(at) & ticks < max(at))])
axis(2, at = at, las = 2)

## add trendlines
for (i in names(dv)[-1]) {
  lines(dv$year, dv[, i], col=colors[i, "colors"], lwd = 2)
}

## add labels
text(x = max(dv$year), 
     y = dv[dv$year==max(dv$year), c(2:8)], 
     pos = 4,
     labels = colors$sites[c(1:7)],
     font = 2,
     col = colors$colors[c(1:7)])

text(x = min(dv$year)-0.45, y = min(at) - 1, pos = 2, "ppb", xpd = TRUE)
text(x = max(dv$year), y = 70, pos = 4, "Fed. Standard", col = "red")

dev.off()

## clean up
rm(d, dv, ozone, std, at, i, ticks, atas)


##########################################################################
### Figure 9: PM10 exceedences by year                                 ##
##########################################################################


##################
## Load 10 year of hourly PM10 data
## ARB only reported PM10LC for a while, so AQS export includes both 81102 and 85101
hourly <- load.aqs("AMP501_1741027-0.txt", tz = "Etc/GMT+8")
hourly <- hourly[, c(1, 2, 4, 5, 6, 7, 10, 12, 13, 14, 16, 18)] # get rid of unneed monitors
names(hourly) <- c("date", "mesa", "slo", "slo.lc", "cdf.teom", "cdf", 
                   "nrp", "paso", "paso.lc","atas.old", "atas", "oso")
hourly$year <- format(hourly$date, "%Y")

## need to figure out which years have complete data, b/c of FRM/FEM transition
apply(hourly[, -1], 2, function(b) tapply(b, hourly$year, function(a) sum(!is.na(a))))

hourly$cdf[hourly$year == 2010] <- NA   # 2010 is incomplete for CDF, even w/ teom
hourly$cdf.teom <- NULL
hourly$slo[is.na(hourly$slo)] <- hourly$slo.lc[is.na(hourly$slo)] # fill in SLO STP gaps with LC
hourly$slo.lc <- NULL
hourly$paso[is.na(hourly$paso)] <- hourly$paso.lc[is.na(hourly$paso)] # fill in paso STP gaps with LC
hourly$paso.lc <- NULL
hourly$atas[is.na(hourly$atas)] <- hourly$atas.old[is.na(hourly$atas)] # merge old and new atas
hourly$atas.old <- NULL
hourly$atas[hourly$year == 2010] <- NA # 2010 is incomplete
#############
# calc days/year > state standard
#############

daily <- data.frame(openair::timeAverage(hourly, data.thresh = 75))
daily$year <- format(daily$date, "%Y")
ex <- apply(daily[, 2:8], 2, 
            function(b) tapply(b, daily$year, function(a) sum(a >= 51, na.rm = TRUE)))
inval <- apply(daily[, 2:8], 2, 
               function(b) tapply(b, daily$year, function(a) sum(!is.na(a))))

ex[which(inval < 329)] <- NA
ex <- data.frame(ex)
ex$year <- row.names(ex)
#############
## plot it
#############

# set up empty plot
svglite::svglite("fig9.svg", width = 8, height = 6, 
    pointsize = 10)

par(mar = c(5, 4, 4, 3), mfrow = c(1,1))
plot(ex$year, ex$cdf, "n",
     xlim = c(2010, 2019),
     ylim = c(0, 100),
     xlab = "", 
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")

# axes
axis(1, ex$year)
at <- max(ex[, -8], na.rm=TRUE)
ticks <- seq(0, 100, by = 10)
at <- c(at, ticks[ticks < max(at)])
axis(2, at = at, las = 2)

# trendlines
for (i in names(ex)[1:6]) {
  lines(ex$year, ex[, i], col=colors[i, "colors"], lwd = 2)
}

# add segments for SLO, PASO
for (i in c("slo", "paso")){
  lines(c(2017.5, 2018), c(ex["2018", i], ex["2018", i]),
        col=colors[i, "colors"], lwd = 2)
}

# labels
text(x = 2018, y = ex[9, 1:6], 
     pos = 4,
     labels = colors[names(ex)[1:6], "sites"],
     font = 2,
     col = colors[names(ex)[1:6], "colors"],
     xpd = TRUE)

dev.off()

##########################################################################
### Figure 10: PM10 hours > threshold                                   ##
##########################################################################

##################
## Load 10 year of hourly PM10 data
## ARB only reported PM10LC for a while, so AQS export includes both 81102 and 85101
hourly <- load.aqs("AMP501_1772158-0.txt", tz = "Etc/GMT+8")

hourly <- hourly[, c(1, 4, 7, 9, 11:15, 19, 21, 23, 25, 27, 29:33)] # get rid of 24-hr/FRM data
hourly <- hourly[, c(1, 2, 4, 5, 6, 7, 10, 12, 13, 14, 16, 18)] # get rid of unneed monitors

names(hourly) <- c("date", "mesa", "slo", "slo.lc", "cdf.teom", "cdf", 
                   "nrp", "paso", "paso.lc","atas.old", "atas", "oso")
hourly$year <- format(hourly$date, "%Y")

## need to figure out which years have complete data, b/c of FRM/FEM transition
apply(hourly[, -1], 2, function(b) tapply(b, hourly$year, function(a) sum(!is.na(a))))

hourly$cdf[hourly$year == 2010] <- NA   # 2010 is incomplete for CDF, even w/ teom
hourly$cdf.teom <- NULL
hourly$slo[is.na(hourly$slo)] <- hourly$slo.lc[is.na(hourly$slo)] # fill in SLO STP gaps with LC
hourly$slo.lc <- NULL
hourly$paso[is.na(hourly$paso)] <- hourly$paso.lc[is.na(hourly$paso)] # fill in paso STP gaps with LC
hourly$paso.lc <- NULL
hourly$atas[is.na(hourly$atas)] <- hourly$atas.old[is.na(hourly$atas)] # merge old and new atas
hourly$atas.old <- NULL
hourly$atas[hourly$year == 2010] <- NA #incomplete year
hourly$slo[hourly$year == 2011 | hourly$year == 2015 | hourly$year == 2016 | hourly$year == 2017] <- NA #incomplete years
hourly$paso[hourly$year == 2017] <- NA # incomplete year (<90%)
hourly$oso <- NULL # only 2018 has >90% completeness
hourly <- hourly[hourly$year > 2009, ] # No sites have complete data for 2009

########################
## Plot number of hours between 10 am and 4 pm (DST) > threshold each year

## Need PDT hour!!
hourly$hour <- as.numeric(format(hourly$date, format = "%H", tz = "America/Los_Angeles"))
hourly <- hourly[hourly$hour >= 10 & hourly$hour <= 16, ]

## calculate hours
x <- apply(hourly[, 2:7], 2, function(b) tapply(b, hourly$year, function(a) sum(a >= 50, na.rm = T)))
x <- apply(x, 2, function(a) ifelse(a == 0, NA, a)) # change zeros to NA for pretty plotting
x <- as.data.frame(x)
x$year <- as.numeric(row.names(x))

#############
## plot it
#############

## set up empty plot
svglite::svglite("fig10.svg", width = 8, height = 6, 
    pointsize = 10)

par(mar = c(5, 4, 4, 3))
plot(x$year, x$cdf, "n",
     xlim = c(min(x$year), max(x$year)+1),
     ylim = c(0, 700),
     xlab = "", 
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")

## axes
axis(1, x$year)
at <- max(x[, -which(names(x)=="year")], na.rm=TRUE)
ticks <- seq(0, 600, by = 100)
at <- c(at, ticks[ticks < max(at)])
axis(2, at = at, las = 2)

## trendlines
for (i in names(x)[1:6]) {
  lines(x$year, x[, i], col=colors[i, "colors"], lwd = 2)
}

## segments for SLO and PASO 2018
lines(c(2017.5, 2018), c(mean(c(14, 9)), 14), col = colors["slo", "colors"], lwd = 2)
lines(c(2017.5, 2018), c(mean(c(109, 555, 555, 555)), 555), col = colors["paso", "colors"], lwd = 2)

# labels
text(x = max(x$year), y = c(0, 0, -4, 0, 4, 0) + x[which.max(x$year), ], 
     pos = 4,
     labels = colors[names(x)[-7], "sites"],
     font = 2,
     col = colors[names(x)[-7], "colors"],
     xpd = TRUE)

dev.off()

## clean up
rm(x, hourly, at, i, ticks, inval, ex, daily)


##########################################################################
### PM10 trends bar graph, figure 11                                    ##
##########################################################################

## take last year's data, and add 2018
## read in fixed width file from AMP450 report
pm10annual <- read.fwf("AMP450_1673098-0.txt", widths=c(5,4,9,4, 159-22,6, 179-159+6), skip=5)
pm10annual <- pm10annual[,c(2,4,6)]
names(pm10annual) <- c("site", "year", "ave")
pm10annual <- reshape2::dcast(pm10annual, year ~ site, value.var="ave", function(x) x[1]) #take first of duplicate entries
pm10annual <- pm10annual[,-c(6,8, 11)] # rm morro, ofs, cp, and whatever 4003 is
pm10annual$"8001"[8] <- 18.9 # merge atas sites...
pm10annual$"8001"[9] <- pm10annual$"8002"[9] # merge atas sites...
pm10annual$"8001"[10] <- pm10annual$"8002"[10] # merge atas sites...
pm10annual$"8002" <- NULL
names(pm10annual) <- c("year", "paso", "mesa", "slo", "cdf", "nrp", "atas")
pm10annual <- pm10annual[,c(1, 2, 7, 4, 6, 3, 5)] #reorder for plotting

## fill in proper averages b/c of FRM/FEM stuff...
pm10annual$cdf[3] <- NA # partial year
pm10annual$paso[3:5] <- c(19.5, 20.8, 17.5) #from AQRs. AMP450 doesn't include BAM data.
pm10annual$slo[4] <- 16.0 #from 2011 AQR. AMP450 doesn't include BAM data.
pm10annual$slo[5] <- 14.8 #from 2012 AQR. AMP450 doesn't include BAM data.
pm10annual$cdf[1:2] <- NA
pm10annual$paso[6] <- 21.5 #from AQRs. AMP450 doesn't handle BAM data well.
pm10annual$atas[3] <- 19.2 #from AQRs. AMP450 doesn't handle BAM data well.
pm10annual$slo[c(8, 9)] <- NA #partial years
pm10annual$paso[c(10)] <- NA #partial years

## Add 2018:
pm10annual <- rbind(pm10annual, c(2018, 26.0, 19.0, 14.0, 24.3, 27.3, 28.7))

## Remove 2008
pm10annual <- pm10annual[pm10annual$year > 2008, ]

#######
## Draw it
svglite::svglite("fig11.svg", width = 8, height = 6, 
    pointsize = 10)

## set up empty plot
par(mar = c(5, 4, 4, 2))
plot(pm10annual$year, pm10annual$paso, "n",
     xlim = c(2009, 2020),
     ylim = c(13, 41),
     xlab = "", 
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")

## add axes
axis(1, pm10annual$year)
ticks <- seq(15, 40, by = 5)
axis(2, at = ticks, las = 2)

## state standard
segments(min(pm10annual$year), 20, max(pm10annual$year), 20, col = "red", lty = 2)

## loop thru data, add trendlines
for (i in names(pm10annual)[-1]) {
  lines(pm10annual$year, pm10annual[, i], col=colors[i, "colors"], lwd = 2)
}
segments(2017.5, 24, 2018, 26.0, col = colors["paso", "colors"], lwd = 2)

## add labels
text(x = max(pm10annual$year), 
     y = pm10annual[which.max(pm10annual$year), 2:7], 
     pos = 4,
     labels = colors[names(pm10annual)[-1], "sites"],
     font = 2,
     col =  colors[names(pm10annual)[-1], "colors"])

text(x = max(pm10annual$year), y = 20, labels = "State Standard", pos = 4, col = "red")
text(x = 2007.78, y = 14, pos = 4, "ug/m3", xpd = TRUE)

dev.off()

## clean up
rm(i, ticks, pm10annual)


##########################################################################
### PM25 trends bar graph, figure 12                                    ##
##########################################################################

## read data, clean up
## take previous year's file and manually add 2017 & 2018

pm25annual <- read.csv("AnnualPM25.txt", comment.char = "#", header=F) ## AQS Design Value Rpt from 2017
pm25annual <- pm25annual[, c(3,5,20,23)]                                ## these are weighted averages.
names(pm25annual) <- c("site", "year", "ann.ave", "complete")
pm25annual <- pm25annual[which(pm25annual$complete=="Y"), c(1:3)]       ## Get rid on incomplete years
pm25annual$ann.ave <- as.numeric(as.character(pm25annual$ann.ave))
pm25annual <- reshape2::dcast(pm25annual, year~site, value.var="ann.ave")     ## Reshape data
names(pm25annual) <- c("year",  "mesa", "slo","cdf", "atas", "atas2")

pm25annual$atas[10] <- 6.0  ## merge atascadero sites from 2016 AAQR
pm25annual$atas[11] <- pm25annual$atas2[11]
pm25annual$atas2 <- NULL
pm25annual <- pm25annual[, c(1,5,3,2,4)]

# get rid of earlier years
pm25annual <- pm25annual[pm25annual$year > 2007, ]

# add 2017 & 2018 data
pm25annual <- rbind(pm25annual, c(2017, 5.7, 6.8, 9.1, 9.6), c(2018, 6.46, 5.86, 7.56, 8.77))

# roll off 2008
pm25annual <- pm25annual[pm25annual$year > 2008, ]

######
## plot it
svglite::svglite("fig12.svg", width = 8, height = 6, 
    pointsize = 10)

## set up empty plot
par(mar = c(5, 4, 4, 2))
plot(pm25annual$year, pm25annual$slo, "n",
     xlim = c(2009, 2020),
     ylim = c(5, 13),
     xlab = "", 
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")

## axes
axis(1, pm25annual$year)
at <- round(range(pm25annual[, -1], na.rm=TRUE), 1)
ticks <- seq(6, 12, by = 2)
at <- c(at, ticks[which(ticks > min(at) & ticks < max(at))])
axis(2, at = at, las = 2)

## annual standard
segments(min(pm25annual$year), 12, max(pm25annual$year), 12, col = "red", lty = 2)

## loop thru data, add trendlines
for (i in names(pm25annual[-1])){
  lines(pm25annual$year, pm25annual[, i],  col=colors[i, "colors"], lwd = 2)
}

## labels
text(x = max(pm25annual$year), 
     y = pm25annual[which.max(pm25annual$year), 2:5], 
     pos = 4,
     labels = colors[names(pm25annual)[-1], "sites"],
     font = 2,
     col = colors[names(pm25annual)[-1], "colors"])


text(x = max(pm25annual$year), y = 12, labels = "Annual Standard", pos = 4, col = "red")
text(x = 2007.7, y = 5.2, pos = 4, "ug/m3", xpd = TRUE)

dev.off()

## clean up
rm(pm25annual, at, i, ticks)




####################################
## final clean up                 ##
####################################
rm(colors,  load.aqs)
