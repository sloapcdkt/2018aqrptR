
####################################################################
### Calculate total and by site 8-hr exceedences (state and fed) ###
### Generate monthly ozone summary plots                         ###
####################################################################


#########################
## LOAD 8hr ozone data ##
#########################

# in AQS, extract daily max 8hr ozone by runing
# AMP350MX selecting 8 hour ozone and the whole county.
# AMP350MX BE SURE TO SELECT 2015 8-hr STD!

# load AQS data download:
source('00_AQSloader.R')
o38hr<-load.aqs("AMP350MX_1773800-0.txt", format="AMP350MX") 
names(o38hr)<-c("date", "slo", "morro", "nrp", "paso", "atas", "red", "carrizo")


################################
## Count up ozone exceedences ##
################################

# count exceedences of old fed 8-hr standard (75 ppb):
apply(o38hr[,2:8], 2, function(x) sum(x > 0.075, na.rm=T))
o38hr$date[which(o38hr$carrizo > 0.075)] #  "2018-08-07 UTC"

# count exceedences of new fed 8-hr standard (70 ppb) (by site):
apply(o38hr[,2:8], 2, function(x) sum(x > 0.070, na.rm=T))
# slo   morro     nrp    paso    atas     red carrizo 
#   0       0       0       2       0       5       3

# count countywide exceedences of new fed 8-hr standard (70 ppb):
sum(apply(o38hr[,2:8], 1, function(x) sum(x>0.070, na.rm=T)>0)) 
# 6

# double check county wide total against by-site totals
o38hr[which(o38hr$red > 0.070), ]
o38hr[which(o38hr$carrizo > 0.070), ]
o38hr[which(o38hr$paso > 0.070), ]



####################################################################
### Calculate total and by site 1-hr exceedences                 ###
####################################################################

## load 2009 to 2018 data from AQS AMP501 Raw Data extract
source('00_AQSloader.R')
ozone <- load.aqs("AMP501_1772053-0.txt")
names(ozone) <- c("date", "slo", "morro", "nrp", "paso", "atas_old",
                "atas", "red", "carrizo")

## merge old and new atascadero locations
ozone$atas[which(is.na(ozone$"atas"))] <- ozone$"atas_old"[which(is.na(ozone$"atas"))]
ozone$"atas_old" <- NULL

## filter to just 2018; get daily maxes
ozone <- ozone[ozone$date >= as.POSIXct("2018-01-01", tz = "UTC"), ]
ozone <- data.frame(openair::timeAverage(ozone, avg.time = "day", data.thresh = 75, statistic = "max"))

## count exceedences of new 1-hr standard (90 ppb) (by site):
apply(ozone[,2:8], 2, function(x) sum(x > 90, na.rm=T))
#    slo   morro     nrp    paso    atas     red carrizo 
#      0       0       0       0       0       0       1 

## count countywide exceedences of 1-hr standard (90 ppb):
sum(apply(ozone[,2:8], 1, function(x) sum(x > 90, na.rm=T)>0)) 
ozone[which.max(ozone$carrizo), ] # 2018-08-07

## clean up
rm(ozone)


###################################################################
### figures 3 and 4  NEW STYLE                                  ###
###################################################################

## plot function
f2 <- function(site, main){
  
  ## padding between months
  p <- 0.25
  
  ## set-up empty plot window
  plot(as.numeric(format(o38hr$date, "%m")), o38hr[,site], type="n",
       xlim=c(0, 12+p), ylim=c(10, 90),
       ylab="", xlab="",
       xaxt="n", yaxt="n", bty="n")
  
  ## site label
  text(12.70, 4.5, main, font=2, pos=2, xpd = TRUE, cex = 1.2) 
  
  ## black lines for <=70 red for exceedences (>70)
  segments(as.numeric(format(o38hr$date, "%m"))-p,
           1000*o38hr[,site],
           as.numeric(format(o38hr$date, "%m"))+p,
           col = ifelse(o38hr[,site] <= 0.07, 1, 2))
  
  ## median line
  lines(c(1:12 - 2*p, 12+2*p),
        tapply(o38hr[,site], format(o38hr$date, "%m"), function(a) median(1000*a, na.rm=T))[c(1:12,12)],
        type = "s",
        lwd = 2, lend = 1)
  
  ## month labels on x-axis
  axis(1, at=1:12, labels=unique(format(o38hr$date, "%b")), 
       las=1, tick=FALSE, line = -2, cex.axis = 0.7)
  
  ## y-axis labels: 10 to max and state standard, if max > 50
  mx <- 1000*max(o38hr[,site], na.rm=TRUE)
  ticks <- seq(10, 100, by = 10)
  at <- c(mx, ticks[which(ticks < max(mx))])
  if(mx %% 10 != 1){  # if max is, e.g., 71, then there's not space for both 70 and 71
    axis(2, at = at, las = 1, line = -1, cex.axis = 0.8)
  } else {
    axis(2, at = at[1:(length(at)-1)], las = 1, line = -1, cex.axis = 0.8)
  }
  
  ## day-of-month labels for exceedences
  ## to prevent overprinting of exceedances with the same value in same month
  d <- data.frame(month = as.numeric(format(o38hr$date, "%m")),
                  day = as.numeric(format(o38hr$date, "%d")),
                  ozone = 1000 * o38hr[, site])
  d <- d[which(d$ozone > 70), ]
  d$tag <- paste(d$month, d$ozone)
  
  tags <- unique(d$tag)
  labs <- c()
  
  for(i in tags){
    labs <- c(labs, paste(d$day[d$tag == i], collapse = ","))
  }
  
  d <- d[!duplicated(d[, c("month", "ozone")]), ]
  d$lab <- labs
  
  if(nrow(d) > 0){
    text(d$month,
         d$ozone,
         d$lab,
         pos = 4,
         col = 2,
         cex = 0.7)
  }
}


#################
## plot figure3 #
#################

svglite::svglite("fig3.svg", width = 8, height = 8, 
    pointsize = 11)

par(mfrow=c(2,2),
    mar=c(3, 4, 1, 1))

f2("paso", "Paso Robles")
f2("atas", "Atascadero")
f2("red", "Red Hills")
f2("carrizo", "Carrizo Plains")

dev.off()

#################
## plot figure3 #
#################

svglite::svglite("fig4.svg", width = 8, height = 8, 
    pointsize = 11)

par(mfrow=c(2,2),
    mar=c(2, 4, 0, 1))

f2("morro", "Morro Bay")
f2("slo", "San Luis Obispo")
f2("nrp", "Nipomo Regional Park")

dev.off()

par(mfrow=c(1,1),
    mar=c(5, 4, 4, 2)+0.1) #reset plot window

###################################################################
### Clean up                                                    ###
###################################################################

rm(o38hr, f2, load.aqs)

