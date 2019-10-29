##########################################################################
### Calculate total and by site 24-hr PM10 exceedences (state and fed) ###
### Generate monthly summary plots                                     ###
##########################################################################

#################################
## LOAD 24hr PM10 data from AQS##
#################################

## in AQS, exact 24-hr average PM10 by running
## AMP350MX selecting 24 hour PM10 (Standard Conditions; AQS code 81102) for the whole county.
## This ensures 40 CFR rules for data completeness and averaging and 
## rounding are applied.

source('00_AQSloader.R')
pm10<-load.aqs("AMP350MX_1741022-0.txt", format="AMP350MX") 
names(pm10)<-c("date", "mesa", "slo", "cdf", "nrp", "paso", "atas", "ofs")


###############################
## Count up PM10 exceedences ##
###############################

## federal 24-hr standard
apply(pm10[,-1], 2, function(x) sum(x>155, na.rm=T)) # none

## state 24-hr standard, by site
apply(pm10[,-1], 2, function(x) sum(x>50, na.rm=T))
# mesa  slo  cdf  nrp paso atas  ofs 
#   39    0   47   17   27    3    2 

## state 24-hr, countywide
sum(apply(pm10[,-1], 1, function(x) sum(x>50, na.rm=T)>0))
 # 74

## number of valid sample days per site:
apply(pm10[,-1], 2, function(a) sum(!is.na(a)))
# mesa  slo  cdf  nrp paso atas  ofs 
# 360  349  361  360  358  359  348 

##############################
## Rule 1001 Violations     ##
##############################

sum(pm10$cdf > 55 & pm10$cdf > 1.2*pm10$ofs, na.rm = TRUE) # rule vios: 40
sum(pm10$cdf > 55 & is.na(pm10$ofs), na.rm = TRUE) # CDF > 55 but no Oso data: 1

###################################################################
### figure 6                                                    ###
###################################################################

f2 <- function(site, main){
  
  ## padding between months
  p <- 0.25
  
  ## set-up empty plot window
  plot(as.numeric(format(pm10$date, "%m")), pm10[,site], type="n",
       xlim=c(0, 12+p), ylim=c(0, 125),
       ylab="", xlab="",
       xaxt="n", yaxt="n", bty="n")
  
  ## site label
  text(12.70, -10, main, font=2, pos=2, xpd = TRUE, cex = 1.2)
  
  ## black lines for <50, red for exceedences (>50)
  segments(as.numeric(format(pm10$date, "%m"))-p,
           pm10[,site],
           as.numeric(format(pm10$date, "%m"))+p,
           col = ifelse(pm10[,site] <= 50, 1, 2))
  
  ## median line
  lines(c(1:12 - 2*p, 12+2*p),
        tapply(pm10[,site], format(pm10$date, "%m"), function(a) median(a, na.rm=T))[c(1:12,12)],
        type = "s",
        lwd = 2, lend = 1)
  
  ## month labels on x-axis
  axis(1, at=1:12, labels=unique(format(pm10$date, "%b")), 
       las=1, tick=FALSE, line = -2, cex.axis = 0.7)
  
  ## y-axis labels: 0, max and state standard, if max > 50
  at <- max(pm10[,site], na.rm=TRUE)
  ticks <- seq(0, 150, by = 25)
  # if(max(at) > 50) at <- c(at, 50)
  at <- c(at, ticks[which(ticks < max(at))])
  axis(2, at = at, las = 1, line = -1, cex.axis = 0.8)
  
  ## day-of-month labels for exceedences
  ## to prevent overprinting of exceedances with the same value in same month
  d <- data.frame(month = as.numeric(format(pm10$date, "%m")),
                  day = as.numeric(format(pm10$date, "%d")),
                  pm10 = pm10[, site])
  d <- d[which(d$pm10 > 50), ]
  d$tag <- paste(d$month, d$pm10)
  
  tags <- unique(d$tag)
  labs <- c()
  
  for(i in tags){
    labs <- c(labs, paste(d$day[d$tag == i], collapse = ","))
  }
  
  d <- d[!duplicated(d[, c("month", "pm10")]), ]
  d$lab <- labs
  
  if(nrow(d) > 0){
    text(d$month,
         d$pm10,
         d$lab,
         pos = 4,
         col = 2,
         cex = 0.6)
  }
}

svglite::svglite("fig6.svg", width = 10.5, height = 7.5, 
    pointsize = 10)

par(mfrow = c(2, 4),
    mar = c(2, 2, 1, 0))

plot(0, type="n",
     xlim=c(0, 12), ylim=c(0, 150),
     ylab="", xlab="",
     xaxt="n", yaxt="n", bty="n")

f2("atas", "Atascadero")
f2("paso", "Paso Robles")
f2("slo", "San Luis Obispo")

f2("cdf", "CDF")
f2("mesa", "Mesa2")
f2("nrp", "Nipomo Regional Park")
f2("ofs", "Oso Flaco")

dev.off()

###################################################################
### Clean up                                                    ###
###################################################################

rm(pm10, f2, load.aqs)

