
load("predictors.Rdata")
library(fields)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tibble)



load("/Users/haosheng/Desktop/Data_Science_Class/Data_Science_Project/predictors.Rdata") 

lon.new <- lon-360

latt <- which(24<lat&lat<37)
stations

texasStations <- which(stations == "KHOU    ")
texasStations


plot(lon.new,lat,pch=19)
map("world",add=T)
map("state",add=T)

View(dates)




rowsofTexas



data_temp <- tibble(temp = c(Twb.prof),
                    depth = rep(seq(0, 3000, by = 100), length.out = length(temp)),
                    prec = rep(ptype, each = 31),
                    station = rep(stations[station.ind], each = 31),
                    lat = rep(lat[station.ind], each = 31),
                    lon = rep(lon[station.ind] - 360, each = 31),
                    date = rep(dates[date.ind], each = 31))
data_notemp <- tibble(prec = ptype, # get a noice tibble
                      station = stations[station.ind],
                      lat = lat[station.ind],
                      lon = lon[station.ind] - 360,
                      date = dates[date.ind])


data_of_texas <- filter(data_temp, station == stations[rowsofTexas])

View(data_of_texas)
data_of_texas$date<- as.numeric(substr(data_of_texas$date,1,10))

data_of_texas$date<- as.numeric(substr(data_of_texas$date,5,6))

#### year
data_of_texas$date<- as.numeric(substr(data_of_texas$date,1,4))

View(data_of_texas)




data_of_texas %>% ggplot(aes(prec,fill = prec)) +
  geom_bar() +
  xlab("Precipitation Type") +
  ylab("Frequency") + ggtitle("Number of Precipetation of Each year Through Out 18 years") +
  facet_wrap(.~date)
  


### map of station of Texas
data_of_texas %>% 
  ggplot(aes(lon,lat)) + 
  geom_point(color = "darkblue") + borders(
    database = "state",
  )
  
################ 2013 map
data_of_texas1998 <- filter(data_temp, station == stations[rowsofTexas])
                           
data_of_texas1998$date <-  as.numeric(substr(data_of_texas1998$date,1,4))

data_of_texas1998<- data_of_texas1998 %>% filter(as.numeric(substr(data_of_texas1998$date,5,6))==9)
length(data_of_texas1998$prec == "RN")

View(data_of_texas1998)




data_of_texas1998 %>% filter(as.numeric(substr(date,1,4)) == 2008) %>% 
  ggplot(aes(prec,fill = prec)) +
  geom_bar() +
  xlab("Precipitation Type") +
  ylab("Frequency") + ggtitle("Precipetation of 2008")
 
leng_of_1998<- length(data_of_texas1998$prec == "RN")

leng_of_1998

leng_rain_1998<- length(data_of_texas1998$prec == "RN")
lm(leng_of_1998~leng_of_1998,data_of_texas1998)
data_of_texas1998 %>% predict(lm(leng_of_1998~leng_of_1998,data_of_texas1998),.)








rain<-which(ptype=="IP")
xlims<-range(Twb.prof[septemberKABI,])
plot(Twb.prof[septemberKABI,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Rain Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(rain)){
  lines(Twb.prof[rain[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=2,lwd=2)






####################################################################



####################################################################


View(dates)

year<- as.numeric(substr(dates,1,4))


month<- as.numeric(substr(dates,5,6))


september <- which(month == 9)
september

dateOfSep<- dates[september]

date.ind

View(date.ind)

NROW(date.ind)
NROW(station.ind)




cor(data_of_texas$temp,data_of_texas$depth)










step.size <- seq(0,3000,by=100)
xlims <- range(Twb.prof[1:100,])


pellet<-which(ptype=="SN")
xlims<-range(Twb.prof[pellet,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Ice Pellet Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(pellet)){
  lines(Twb.prof[pellet[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=5,lwd=3)



#Is there a Snow type occurring in Florida in September?

length(station.ind)
which(station.ind==21)

View(Twb.prof)
######################
#Plotting some of the temperature vertical profiles
######################

dim(Twb.prof)
head(Twb.prof)


step.size <- seq(0,3000,by=100)
xlims <- range(Twb.prof[1:100,])


#First vertical profile
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="l")

#First 100 vertical profiles
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="l",xlim=xlims)
for(i in 1:100){
  lines(Twb.prof[i,],step.size)
}

abline(v=273.15,col=2,lwd=2)


#Ice Pellet Profiles
pellet<-which(ptype=="IP")
xlims<-range(Twb.prof[pellet,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Ice Pellet Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(pellet)){
  lines(Twb.prof[pellet[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=5,lwd=3)
#lines(Twb.prof[pellet[694],],step.size,col=4,lwd=2)
#lines(Twb.prof[pellet[574],],step.size,col=2,lwd=2)
#lines(Twb.prof[pellet[696],],step.size,col=2,lwd=2)


#Rain Profiles
rain<-which(ptype=="RA")
xlims<-range(Twb.prof[rain,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Rain Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(rain)){
  lines(Twb.prof[rain[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=2,lwd=2)


#Snow Profiles
snow<-which(ptype=="SN")
xlims<-range(Twb.prof[snow,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Snow Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(snow)){
  lines(Twb.prof[snow[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=2,lwd=2)



#Freezing Rain Profiles
frza<-which(ptype=="FZRA")
xlims<-range(Twb.prof[frza,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters AGL",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Freezing Rain Profiles",cex.main=2)
abline(v=273.15,col=2,lwd=2)
for(i in 1:length(frza)){
  lines(Twb.prof[frza[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=2,lwd=2)
















