##################################
# calculate AQI days by site     #
# for AQ inforgraphic            #  
# 2018 2018 2018 2018 2018 2018  #
##################################


##---------------------------------##
## "Annulus Plots" of AQI days     ##
##---------------------------------##


## use data from AQS AQI Rpts (AMP410)

read.aqi <- function(file){
  d <- read.fwf(file, widths = c(23, 8, 3, 8, 30, 14), as.is = TRUE)
  d <- d[, c(2,5)]
  names(d) <- c("date", "category")
  return(d)
}

rh <- read.aqi("infographic/red.txt")
slo <- read.aqi("infographic/slo-aqi.txt")
paso <- read.aqi("infographic/paso-aqi.txt")
nrp <- read.aqi("infographic/nrp-aqi.txt")
cdf <- read.aqi("infographic/cdf-aqi.txt")


## plot function
## creates SVGs AND generates plot in Rstudio
annulus2 <- function(site, thick = 0.5, basefilename = NULL){
  
  op <- par(no.readonly = TRUE)
  on.exit(par <- op)
  
  if(is.null(basefilename)) {
    filename = paste0(deparse(substitute(site)), ".svg")
  } else {
    filename = paste0(basefilename, ".svg")
  }
  
  if(class(site) == "data.frame") site <- site$category
  
  res <- 1
  tt <-seq(pi/2, -3*pi/2, length = res*length(site))
  dat <- table(site)
  good <- 1:(res*dat[1])
  mod <- (res*dat[1]):(res*dat[2]+res*dat[1])
  ufsg <- (res*dat[1] + res*dat[2]):(res*length(site))
  col <- c("#00e400", "#ffff00", "#ff7e00")
  
  svg(filename = filename, bg = "transparent")
  par(bg = "transparent")
  plot(0,0, xlim = c(-1,1), ylim = c(-1,1), asp = 1,
       xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       bty = "n", type = "n")
  polygon(c(cos(tt[good]), thick*cos(tt[rev(good)])),c(sin(tt[good]), thick*sin(tt[rev(good)])),
          col = col[1], lty = 0)
  polygon(c(cos(tt[mod]), thick*cos(tt[rev(mod)])),c(sin(tt[mod]), thick*sin(tt[rev(mod)])),
          col = col[2], lty = 0)
  polygon(c(cos(tt[ufsg]), thick*cos(tt[rev(ufsg)])),c(sin(tt[ufsg]), thick*sin(tt[rev(ufsg)])),
          col = col[3], lty = 0)
  dev.off()
  
  
  plot(0,0, xlim = c(-1,1), ylim = c(-1,1), asp = 1,
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
       bty = "n", type = "n")
  polygon(c(cos(tt[good]), thick*cos(tt[rev(good)])),c(sin(tt[good]), thick*sin(tt[rev(good)])),
          col = col[1], lty = 0)
  polygon(c(cos(tt[mod]), thick*cos(tt[rev(mod)])),c(sin(tt[mod]), thick*sin(tt[rev(mod)])),
          col = col[2], lty = 0)
  polygon(c(cos(tt[ufsg]), thick*cos(tt[rev(ufsg)])),c(sin(tt[ufsg]), thick*sin(tt[rev(ufsg)])),
          col = col[3], lty = 0)
  
  
}

## generate plots.
invisible(mapply(annulus2, list(slo, paso, cdf, nrp, rh), 
                 basefilename = c("infographic/slo", "infographic/paso", 
                                  "infographic/cdf", "infographic/nrp", 
                                  "infographic/rh")))

##--------------------------------------##
## Ozone trends plot                    ##
## adapted from 2018 AQ RPT Fig 8       ##
##--------------------------------------##

#####################
## define consistent color palatte
#####################
colors <- RColorBrewer::brewer.pal(8, "Set1")[-6]
colors <- c(colors, colors[c(2,6,7)]) # reuse morro, red, carrizo colors for cdf, mesa, oso
sites <- c( "San Luis Obispo", "Morro Bay", "NRP",
            "Paso Robles", "Atascadero",  "Red Hills", "Carrizo Plains",
            "CDF", "Mesa2", "Oso Flaco")
colors <- data.frame(sites=sites, colors=colors, stringsAsFactors = FALSE)
rownames(colors) <- c("slo", "morro", "nrp", "paso", "atas", "red", "carrizo", "cdf", "mesa", "oso")
rm(sites)

######################
## load Data
######################

## load design values from AQS rpt
dv <- read.csv("AMP480_1772133_DV_Ozone.csv", comment.char="#", header=F, as.is=T)
dv <- dv[, c(4, 7, 52, 53)]
names(dv) <- c("site", "year", "value", "valid")
dv$value[dv$valid=="N"] <- NA   ## remove design values that don't meet completeness requirements
dv$value <- dv$value*1000       ##convert ppm to ppb
dv <- reshape2::dcast(dv[,-4], year ~ site)
names(dv) <- c("year", "paso", "slo", "morro", "nrp", "atas", "red", "carrizo")

## can't get AQS to include old Atascadero site, so grab data from last year's report
atas <- read.csv("AMP480_1673066-0.txt", comment.char="#", header=F, as.is=T)
atas <- atas[c(3,6,35,36)]
names(atas) <- c("site", "year", "value", "valid")
atas <- atas[atas$site == 8001 & atas$year > 2008, ]
atas$value[atas$valid == "N"] <- NA   ## remove design values that don't meet completeness requirements
dv$atas[dv$year >= 2009 & dv$year <= 2015] <- 1000*atas$value[atas$year >= 2009 & atas$year <= 2015]

#####################
## create plot
#####################
op <- par(no.readonly = TRUE)

svg("infographic/ozonetrend.svg", width = 8, height = 6, 
    pointsize = 12, bg = "transparent")

## set up empty plot
par(mar = c(5, 3, 4, 3), bg = "transparent")
plot(dv$year, dv$red, "n",
     xlim = range(dv$year) + c(0,2),
     ylim = c(50, 85),
     xlab = "", 
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")

## add axes
axis(1, dv$year, lwd = 2)

at <- round(range(dv[, -1], na.rm=TRUE), 1)
ticks <- seq(0, 100, by = 10)
at <- c(at, ticks[which(ticks > min(at) & ticks < max(at))])
axis(2, at = at, las = 2, lwd = 2)

## add trendlines
for (i in names(dv)[-1]) {
  lines(dv$year, dv[, i], col=colors[i, "colors"], lwd = 3)
}


## add labels
text(x = max(dv$year), 
     y = dv[dv$year==max(dv$year), c(2:8)], 
     pos = 4,
     labels = colors[names(dv)[-1], "sites"],
     font = 2,
     col = colors[names(dv)[-1], "colors"])

text(x = 2007.78, y = 50, pos = 4, "ppb", xpd = TRUE)


dev.off()
par(op)


## clean up
rm(d, dv, ozone, std, at, i, ticks)

##--------------------------------------##
## PM10 trends plot                    ##
## adapted from 2018 AQ RPT Fig 9       ##
##--------------------------------------##

##################
## Load 10 year of hourly PM10 data
## ARB only reported PM10LC for a while, so AQS export includes both 81102 and 85101

source('00_AQSloader.R')
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
            function(b) tapply(b, daily$year, function(a) sum(a > 50.9, na.rm = TRUE)))
inval <- apply(daily[, 2:8], 2, 
               function(b) tapply(b, daily$year, function(a) sum(!is.na(a))))

ex[which(inval < 329)] <- NA
ex <- data.frame(ex)
ex$year <- row.names(ex)
#############
## plot it
#############

## set up empty plot
svg("infographic/pm10trend.svg", width = 8, height = 6, 
    pointsize = 12, bg = "transparent")

par(mar = c(5, 4, 4, 3), bg = "transparent")
plot(ex$year, ex$cdf, "n",
     xlim = c(2010, 2019),
     ylim = c(0, 100),
     xlab = "", 
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     bty = "n")

## axes
axis(1, ex$year, lwd = 2)
at <- max(ex[, -8], na.rm=TRUE)
ticks <- seq(0, 100, by = 10)
at <- c(at, ticks[ticks < max(at)])
axis(2, at = at, las = 2, lwd =2)

## trendlines
for (i in names(ex)[1:6]) {
  lines(ex$year, ex[, i], col=colors[i, "colors"], lwd = 3)
}

## add segments for SLO, PASO
for (i in c("slo", "paso")){
  lines(c(2017.5, 2018), c(ex["2018", i], ex["2018", i]),
        col=colors[i, "colors"], lwd = 3)
}

## labels
text(x = 2018, y = ex[9, 1:6], 
     pos = 4,
     labels = colors[names(ex)[1:6], "sites"],
     font = 2,
     col = colors[names(ex)[1:6], "colors"],
     xpd = TRUE)

dev.off()
