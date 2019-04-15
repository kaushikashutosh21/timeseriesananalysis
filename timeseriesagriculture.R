setwd("F:\\timeseriesagri")
data <- read.csv("Monthly_data_cmo.csv")
data <- na.omit(data)

data
class(data)
summary(data)

dim(data)

n<-rnorm(data$arrivals_in_qtl)
qqnorm(n)
qqline(n)

d<-as.data.frame(unique(data$Commodity))


#Number of APMCS

length(unique(data$APMC))

#Number of commodities

length(unique(data$Commodity))
data1<-data
data<-data1

#Spliting into commodities

commodity<-split(data,data$Commodity)

#bp<-boxplot(commodity$`Ambat Chuka`[,5],plot = TRUE)
#commodity$`Ambat Chuka`[which(commodity$`Ambat Chuka`[,5]%n% bp$out),5]<-NA


for (i in 5:8) {
  bp<-boxplot(commodity$`Ambat Chuka`[,i])
  boxplot(commodity$`Ambat Chuka`[which(commodity$Maize[,i]%in% bp$out),i])
  commodity$`Ambat Chuka`[which(commodity$Maize[,i]%in% bp$out),i]<-NA
}


boxplot(commodity$`Amba Koy`)
boxplot(commodity$Maize)
boxplot(commodity$Cotton)
boxplot(commodity$Apple)
boxplot(commodity$`Baru Seed`)
boxplot(commodity$Sunflower)
boxplot(commodity$Sugarcane)
boxplot(commodity$Bajri)
boxplot(commodity$Banana)
boxplot(commodity$`Wheat(Husked)`)




###outliers filteration

for(j in 2:62430){
  for(i in 5:8){
    bp <- boxplot(commodity[[j]][,i], plot = FALSE)
    commodity[[j]][which(commodity[[j]][,i] %in% bp$out),i] <- NA   # to remove the outliers
  }
}



##number of outliers in each commodity And tremoving outliers

for (i  in 2:62430) {
  print(names(commodity[[i]]))
  print(sum(is.na(commodity[[i]])))
  n<-sum(is.na(commodity[[i]]))
  if(n!=0)
    commodity[[i]]<-na.omit(commodity[[i]])
  
}

sum(is.na(commodity[[24]]))


###detecting sesonility type for different commodities

##1.Amba koy

da<-as.data.frame(commodity$`Amba Koy`$date)
da$price<-commodity$`Amba Koy`$modal_price

amba.ts<-ts(da,frequency = 12 , start = c(2014,1), end = c(2016,12))
plot(amba.ts[,2])


#trend
library(forecast)
trend_amba<-ma(amba.ts,order=12,centre=TRUE)
plot(amba.ts[,2])
line(trend_amba[,2])
plot(trend_amba[,2])


##detrend amba

deterend_amba=amba.ts - trend_amba
plot(as.ts(deterend_amba[,2]))

decomp<-stl(log(amba.ts[,2]),s.window ="periodic")
ap.sa<-exp(seasadj(decomp))

library(ggplot2)
autoplot(cbind(amba.ts[,2],SeasonallyAdjusted=ap.sa))+xlab("Year")+ylab("Number of passengers(thousands)")



##2. Maize
rm(da)

da <- as.data.frame(commodity$Maize$date)
da$price <- commodity$Maize$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

# trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

decomp <- stl(ts(comm.ts[,2],freq=12), t.window=15, s.window="per", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")+ scale_y_continuous(breaks = NULL)


ts_comm = ts(comm.ts[,2], frequency = 4)
decompose_amba = decompose(ts_comm, "additive")

plot(as.ts(decompose_amba$seasonal))
plot(as.ts(decompose_amba$trend))
plot(as.ts(decompose_amba$random))
plot(decompose_amba)

##3. Cotton
rm(da)

da <- as.data.frame(commodity$Cotton$date)
da$price <- commodity$Cotton$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")


ts_comm = ts(comm.ts[,2], frequency = 4)
decompose_amba = decompose(ts_comm, "additive")

plot(as.ts(decompose_amba$seasonal))
plot(as.ts(decompose_amba$trend))
plot(as.ts(decompose_amba$random))
plot(decompose_amba)


##4. apple
rm(da)

da <- as.data.frame(commodity$Apple$date)
da$price <- commodity$Apple$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

# trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")


##4.Baru seed
rm(da)

da <- as.data.frame(commodity$`Baru Seed`$date)
da$price <- commodity$`Baru Seed`$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")



##6. Sunflower
rm(da)

da <- as.data.frame(commodity$Sunflower$date)
da$price <- commodity$Sunflower$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend 

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")



##7. Sugar cane
rm(da)

da <- as.data.frame(commodity$Sugarcane$date)
da$price <- commodity$Sugarcane$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")




##8. Bajri
rm(da)

da <- as.data.frame(commodity$Bajri$date)
da$price <- commodity$Bajri$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")



##9. Banana
rm(da)

da <- as.data.frame(commodity$Banana$date)
da$price <- commodity$Banana$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")

##10. Banana(raw)
rm(da)

da <- as.data.frame(commodity$`Banana(Raw)`$date)
da$price <- commodity$`Banana(Raw)`$modal_price

comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2]) #additive trend for apple 

#trend

trend_comm = ma(comm.ts[], order = 12, centre = T)
plot(comm.ts[,2])
lines(trend_comm[,2])
plot(trend_comm[,2])

##Detrend the Time Series

detrend_comm = comm.ts - trend_comm
plot(as.ts(detrend_comm[,2]))

## Deseasonlize
decomp <- stl(log(comm.ts[,2]), t.window=15, s.window="periodic", robust=TRUE)
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(comm.ts[,2], SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Average Price")





data2<-read.csv("CMO_MSP_Mandi.csv")
length(unique(data2$commodity))
data4<-data2
data2<-data4
commodity1<-split(data2,data2$commodity)
da1<-as.data.frame(commodity1$COTTON$year)
ts_comm = ts(comm.ts[,2], frequency = 4)
decompose_amba = decompose(ts_comm, "additive")
 
plot(as.ts(decompose_amba$seasonal))
plot(as.ts(decompose_amba$trend))
plot(as.ts(decompose_amba$random))
plot(decompose_amba)

comm.ts.qtr <- aggregate(comm.ts, nfrequency=4)
comm.ts.yr <- aggregate(comm.ts, nfrequency=1)
plot.ts(comm.ts[,2], main = "Monthly Apple Production", xlab = "Year", ylab = "ML")
plot.ts(comm.ts.qtr[,2], main = "Quarterly Beer Production in Australia", xlab = "Year", ylab = "ML")
plot.ts(comm.ts.yr[,2], main = "Yearly arrival of Apple", xlab = "Year", ylab = "ML")
rm(da1)
da1 <- as.data.frame(commodity1$COCONUT$year)
da1$Price <- commodity$arvi$modal_price
comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
plot(comm.ts[,2])
# 
comm.ts <- ts(da, frequency = 12, start = c(2014,1), end = c(2016,12))
comm.ts.qtr <- aggregate(comm.ts, nfrequency=4)

library(Rserve)
Rserve()
