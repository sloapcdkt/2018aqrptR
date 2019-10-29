
################################################
## Appendix B: Respirable Crystalline Silica  ##
################################################

###########################
#  Silica Data
###########################

silica <- data.frame(date = as.Date(c("2019-04-09", "2019-04-11", "2019-04-13", "2019-04-20",
                                      "2019-05-14", "2019-05-16", "2019-05-19", "2019-05-22",
                                      "2019-05-28", "2019-05-30", "2019-06-06", "2019-06-14",
                                      "2019-06-27", "2019-07-01", "2019-07-03", "2019-07-11", 
                                      "2019-07-15", "2019-07-29", "2019-08-02", "2019-08-10",
                                      "2019-08-20", "2019-08-22", "2019-09-08", "2019-09-10",
                                      "2019-09-18", "2019-09-28")),
                     si =  c(11, 8.1, 1.3, 3,
                             0.49, 0, 0, 3.9,
                             11, 0.47, 0, 0.63,
                             6.3, 9.5, 5.8, 0,
                             8.9, 2.2, 1.2, 3.5,
                             4.0, 0, 12, 4.8,
                             2.4, 2.1),
                     pm10 = c(104, 87, 26, 32,  # PM10 LC from BAM
                              12, 14, 12, 53,
                              99, 15, 21, 7,
                              60, 88, 62, 6,
                              85, 40, 15, 29,
                              39, 16, 106, 51,
                              42, 42),
                     pm10std = c(100, 83, 25, 31,
                                 11, 13, 12, 52,
                                 96, 15, 20, 7,
                                 58, 86, 60, 6,
                                 84, 39, 15, 28,
                                 38, 15, 104, 50,
                                 41, 41),
                     grav = c(110, 91, 35, 39,  # PM10 mass from SiO2 filters
                              16, 20, 17, 59,
                              54, 19, 25, 14,
                              62, 89, 65, 11,
                              93, 40, 15, 28,
                              40, 18, 100, 52,
                              41, 45))

## set reporting limit
limit <- 0.42 #ug/m3 based on 10 ug per filter


#####################################
## Figure B1: Total Dust vs PM10LC
#####################################

svglite::svglite("figb1.svg", width = 8, height = 6, 
    pointsize = 11)

#set up plot area
par(mar = c(4, 4, 4, 4) + 0.1)
plot(silica$pm10, silica$grav,
     pch = 16,
     xlim = c(0, 110), ylim = c(0, 110),
     xaxt = "n", yaxt = "n",  
     xlab = "", ylab = "",
     bty = "n", asp = 1)

axis(1, at = 10*0:11, gap.axis = 0.25, line = 0)
axis(2, at = 10*(0:11), las = 2, line = -5)

title(xlab = "PM10 Local Conditions, ug/m3", line = 2.5)
title(ylab = "Total Dust, ug/m3", line = -2.5)

lines(c(0, 110), c(0, 110), lty = 2)

dev.off()

#####################################
## Sats for Gravimetric Analysis
#####################################

summary(lm(grav ~ pm10, data = silica))
cor(silica$grav, silica$pm10)

cvub <- function(x, y, thresh = 10) { # checked vs EPA DASC example.
  y <- ifelse(y <= 0, 0.1, y)
  d <- 200*(x - y)/(x + y)
  d <- d[y > thresh]
  n <- length(d)
  num <- n*sum(d^2) - sum(d)^2
  sqrt(num/(2*n*(n-1))) * sqrt((n-1)/(qchisq(0.1, n-1)))
}

cvub(silica$grav, silica$pm10, thresh = 3) # 20.37

#####################################
## Figure B2: Silica vs PM10
#####################################

svglite::svglite("figb2.svg", width = 8, height = 6, 
                 pointsize = 11)

## set up plot area
par(mar = c(5, 4, 4, 2) + 0.1)
plot(silica$pm10std, silica$si,
     pch = 16,
     xlim = c(0, 120), ylim = c(0, 13),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = "",
     bty = "n"
     )

axis(1, at = 10*0:11, line = 1)
axis(2, at = 2*(0:6), las = 2)

title(xlab = "PM10 Standard Conditions, ug/m3", line = 3.5)
title(ylab = "PM10-Silica, ug/m3", line = 2)

## Run Censored (Tobit) Regression
library(VGAM)
vglm0 <- vglm(si ~ pm10std, family = tobit(Lower = limit), data = silica)

## add censored regression line
newx = seq(5, 104, by = 0.1)
vglm_interval <- predictvglm(vglm0, newdata = data.frame(pm10std=newx), type = "link", se.fit = TRUE)
vglm_pred <- ifelse(vglm_interval$fitted.values[, 1] >= 0, vglm_interval$fitted.values[, 1], 0)
lines(newx, vglm_pred)

dev.off()
#####

########################################
## Discussion of silica - PM10 relationship
########################################

## model summary
summary(vglm0) # pm10std: 0.1250; intercept: -1.567

## residual SE
exp(coef(vglm0)[2]) # 0.82

## McFadden's pseudo r2
vglm00 <- update(vglm0, . ~ 1) # null model
1 - logLik(vglm0)/logLik(vglm00) # 0.57

## alternative r2: squared pearson r
preds <- ifelse(predict(vglm0)[, 1] >= 0, predict(vglm0)[, 1], 0)
cor(silica$si, preds)^2 # 0.96

## PM10 threshold 
-coef(vglm0)[1]/coef(vglm0)[3] # 12.5

## for comparison's sake, a simple linear regression:
lm0 <- lm(si ~ pm10std, data = silica)
summary(lm0) # pm10sts: 0.1221; intercept: -1.362
             # resid s.e.: 0.801; r2 = 0.96
             # threshold: 11.2

#########################################
## Calc of annual means
#########################################

## load, format data: CDF PM10, 2011 - 2018
source('00_AQSloader.R')
pm10 <- load.aqs("AMP350MX_1780617-0.txt", format="AMP350MX")
names(pm10) <- c("date", "cdf")
pm10$year <- format(pm10$date, "%Y")

## function to calc annual si averages
annual.ave <- function(data, year, mod){
  stopifnot(class(mod)[1] == "vglm")
  
  slope <- coef(mod)[3]
  intercept <- coef(mod)[1]
  dat <- data$cdf[data$year == year]
  
  p <- dat*slope + intercept
  p <-ifelse(p < 0, 0, p)
  
  return(mean(p, na.rm = TRUE))
}

## calc annual averages
annual.ave(pm10, 2018, vglm0) # 2.17
round(sapply(2018:2011, annual.ave, data = pm10, mod = vglm0), 2)
# 2.17 3.39 2.74 2.81 3.19 3.47 2.73 2.78 
 
## plot si vs pm10 annual means (not in report):
plot(tapply(pm10$cdf, pm10$year, mean, na.rm = TRUE), sapply(2011:2018, annual.ave, data = pm10, mod = vglm0) )

## calc multi-year average
pm10$year <- 1000
annual.ave(pm10, 1000, vglm0) # 2.91 
pm10$year <- format(pm10$date, "%Y") 

############################################
# Bootstrap conf intervals for annual silica means
############################################

## bootstrap a bunch of models, save coefs
boot_mod <- function(dat){
  n <- nrow(dat)
  mb <- vglm(si ~ pm10std, family = tobit(Lower = limit), data = dat[sample(1:n, n, replace = TRUE), ])
  coef(mb)
}

set.seed(3433)
mods <- replicate(5000, boot_mod(silica)) # this takes a while
mods <- data.frame(t(mods))
names(mods) <- c("int", "logse", "slope")

## calc daily silica using these mods
daily.silica <- function(pm10, int, slope){
  si <- pm10*slope + int
  si <- ifelse(si < 0, 0, si)
}

res <- matrix(nrow = nrow(pm10), ncol = nrow(mods))

for(i in 1:nrow(mods)){
  res[, i] <- sapply(pm10$cdf, function(a) daily.silica(a, mods$int[i], mods$slope[i]))
}

## calc annual silica aves
res.annual <- apply(res, 2, function(a) tapply(a, pm10$year, mean))

## take quantiles
round(apply(res.annual, 1, function(a) quantile(a, c(0.025, 0.975))), 2)
#       2011 2012 2013 2014 2015 2016 2017 2018
# 2.5%  2.43 2.44 3.14 2.82 2.44 2.41 3.06 1.87
# 97.5% 3.13 3.05 3.80 3.55 3.17 3.08 3.72 2.48

## cross check means vs above
apply(res.annual, 1, mean) # looks good.

## calc multiyear mean, conf int
multi.year <- apply(res, 2, mean)
mean(multi.year)
quantile(multi.year, c(0.025, 0.5, 0.975))

