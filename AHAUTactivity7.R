
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity7/Deemer_GHG_Data.csv")


# log transform methane fluxes
ghg$log.ch4 <- log(ghg$ch4+1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)
ghg$log.sa <- log(ghg$surface.area)
ghg$log.runoff <- log(ghg$runoff+1)

#look at names
unique(ghg$Region)

# binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
# binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)
# binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)
# binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)


# multiple regression
# creates a model object
#ACTIVITY SEVEN PT 1 QUESTION 3
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe

summary(mod.full)

mod.edit <- lm(log.ch4 ~ airTemp+
                 log.age+
                 mean.depth+
                 log.runoff+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe

summary(mod.edit)

res.edit <- rstandard(mod.edit)
fit.edit <- fitted.values(mod.edit)

# qq plot
qqnorm(res.edit, pch=19, col="grey50")
qqline(res.edit)

# shapiro-wilks test
shapiro.test(res.edit)


plot(fit.edit,res.edit, pch=19, col="grey50")
abline(h=0)

# isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

summary(mod.edit)$coefficients
table_out <- summary(mod.edit)$coefficients 

# run stepwise
full.step <- ols_step_forward_aic(mod.edit)
# view table
full.step 
# check full model
full.step$model
# plot AIC over time
plot(full.step )


# IN CLASS PROMPT 2
ETdat <- read.csv("/cloud/project/activity7/ETdata.csv")
unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

almondTrend <- almond_dec$trend
almondSeason <- almond_dec$seasonal

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

pacf.plot <- pacf(na.omit(almond_ts))

almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4

# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))

# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#HOMEWORK 7
#Question 1 

#tranform co2 using suggested transformation
ghg$co2.tranform <- 1/(ghg$co2 + 1000)

#design a multiple regression
mod.co2 <- lm(co2.tranform ~ 
                 log.age+
                 mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe
summary(mod.co2)

res.co2 <- rstandard(mod.co2)
fit.co2 <- fitted.values(mod.co2)

# qq plot
qqnorm(res.co2, pch=19, col="grey50")
qqline(res.co2)

# shapiro-wilks test
shapiro.test(res.co2)


plot(fit.co2,res.co2, pch=19, col="grey50")
abline(h=0)

# isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$log.age,
                       ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

summary(mod.co2)$coefficients

# run stepwise
full.step <- ols_step_forward_aic(mod.co2)
# view table
full.step 
# check full model
full.step$model
# plot AIC over time
plot(full.step )

summary(mod.co2)$coefficients

#QUESTION 3

ETdat <- read.csv("/cloud/project/activity7/ETdata.csv")
unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>% 
  filter(crop == "Almonds") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) 


# almond ET time series
almond_ts <- ts(almond$ET.in,
                start = c(2016,1), 
                frequency= 12) 

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

# average fields for each month for pistachios
pistachio <- ETdat %>% 
  filter(crop == "Pistachios") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))


# pistachio ET time series
pistachio_ts <- ts(pistachio$ET.in, 
                start = c(2016,1), 
                frequency= 12) 

# decompose pistachio ET time series
pistachio_dec <- decompose(pistachio_ts)
# plot decomposition
plot(pistachio_dec)


# average fields for each month for fallow/idle
fallow_idle <- ETdat %>% 
  filter(crop == "Fallow/Idle Cropland") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) 


# fallow/idle ET time series
fallow_idle_ts <- ts(fallow_idle$ET.in, 
                   start = c(2016,1), 
                   frequency= 12) 

# decompose fallow/idle ET time series
fallow_idle_dec <- decompose(fallow_idle_ts)
# plot decomposition
plot(fallow_idle_dec)

# average fields for each month for corn
corn <- ETdat %>% 
  filter(crop == "Corn") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) 


# corn ET time series
corn_ts <- ts(corn$ET.in, 
                     start = c(2016,1),
                     frequency= 12) 

# decompose corn ET time series
corn_dec <- decompose(corn_ts)
# plot decomposition
plot(corn_dec)

# average fields for each month for grapes
grape <- ETdat %>% 
  filter(crop == "Grapes (Table/Raisin)") %>% 
  group_by(date) %>% 
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))


# grapes ET time series
grape_ts <- ts(grape$ET.in, 
              start = c(2016,1), 
              frequency= 12) 

# decompose grape ET time series
grape_dec <- decompose(grape_ts)
# plot decomposition
plot(grape_dec)

#QUESTION 4
pistachio_y <- na.omit(pistachio_ts)
model1_pistachio <- arima(pistachio_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4_pistachio <- arima(pistachio_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4_pistachio
# calculate fit
AR_fit1_pistachio <- pistachio_y - residuals(model1_pistachio) 
AR_fit4_pistachio <- pistachio_y - residuals(model4_pistachio)
#plot data
plot(pistachio_y)
# plot fit
points(AR_fit1_pistachio, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4_pistachio, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("pistachio data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")
newPistachio <- forecast(model4_pistachio)
newPistachio
#make dataframe for plotting
newPistachioF <- data.frame(newPistachio)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistachioF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = pistachio, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachio$date[1]),newPistachioF$dateF[24])+  # Plotting original data
  geom_line(data = newPistachioF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newPistachioF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")


fallow_y <- na.omit(fallow_idle_ts)
model1_fallow <- arima(fallow_y , # data 
                          order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4_fallow <- arima(fallow_y , # data 
                          order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4_fallow
# calculate fit
AR_fit1_fallow <- fallow_y - residuals(model1_fallow) 
AR_fit4_fallow <- fallow_y - residuals(model4_fallow)
#plot data
plot(fallow_y)
# plot fit
points(AR_fit1_fallow, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4_fallow, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("fallow data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")
newFallow <- forecast(model4_fallow)
newFallow
#make dataframe for plotting
newFallowF <- data.frame(newFallow)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallowF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = fallow_idle, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallow_idle$date[1]),newFallowF$dateF[24])+  # Plotting original data
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newFallowF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")