library(lubridate)
library(zoo)
library(xts)
library(dplyr)
library(ggplot2)
library(forecast)
library(quantmod)
library(sophisthse)

#unreal stationary processes

y <- arima.sim(n=100, list(ar=0.7)) #ar process
plot(y)
Acf(y)
Pacf(y)
tsdisplay(y) #all ARMA graphs in one screen

y <- arima.sim(n=100, list(ma=-0.8)) #ma process
tsdisplay(y)

y <- arima.sim(n=100, list(ma=-0.8, ar=-0.5)) #arma process
tsdisplay(y)

y <- arima.sim(n=500, list(order = c(0,1,0))) #random process
tsdisplay(y)

y <- seq(0, 10, length=100) + arima.sim(n=100, list(ar=0.7)) #trend process
tsdisplay(y)


y <- LakeHuron
tsdisplay(y)

mod1 <- Arima(y, order=c(2,0,0)) #2 lags on AR
mod2 <- Arima(y, order=c(1,0,1)) #ARMA 1,1

summary(mod1) #если мы делим коэффициент на стандартную ошибку и число больше 0, то коэффициент значим
summary(mod2)

AIC(mod1)
AIC(mod2) #less is better

mod3 <- Arima(y, order=c(2,0,1))
summary(mod3)
AIC(mod3)

predicti <- forecast(mod2, h=5) #modelling process for 5 years
predicti
mod4 <- Arima(y, order=c(1,1,0)) #non stationary process -- 1 in the middle
AIC(mod4)

predicti2 <- forecast(mod4,h=5)
plot(predicti2)

#choose better model automatically
mod_a <- auto.arima(y)
x <- summary(mod_a)
summary(mod_a)

prognoz_a <- forecast(mod_a, h=5)
prognoz_a
plot(prognoz_a)



#google stocks analysis
Sys.setlocale("LC_TIME", "C")
getSymbols(Symbols = "GOOG", from="2014-01-01", to="2014-12-01" )

head(GOOG)
y <- GOOG$GOOG.Close

tsdisplay(y) #this is random walk
dy <- diff(y) #difference between today and yesterday stock price
tsdisplay(dy)


mod1 <- Arima(y, order=c(0,1,0))
summary(mod1)

predict1 <- forecast(mod1, h=20)
predict1
plot(predict1)

mod_a <- auto.arima(y)
summary(mod_a)

y <- sophisthse("POPNUM_Y")
tsdisplay(y)

mod1 <- Arima(y, order=c(1,1,0), include.drift = T)
summary(mod1)
predict1 <- forecast(mod1, h=5)
plot(predict1)


y <- sophisthse("CPI_M_CHI")
ym <- y[97:nrow(y),]
tsdisplay(ym) #this is seasoned data

mod1 <- Arima(ym, order=c(1,0,0), seasonal = list(order=c(1,0,0),period=12))  #1 in seasonal like a 12 month ago, this is year
summary(mod1)

AIC(mod1)
predict1 <- forecast(mod1, h=12)
plot(predict1) #forecast with seasons, not just linear

moda <- auto.arima(ym)
predicta <- forecast(mod1, h=12)
plot(predicta) #forecast with seasons, not just linear



#homework
y <- arima.sim(n=500, list(order = c(1,1,1)))
tsdisplay(y)


set.seed(30)
y <- arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

set.seed(2)
y <- arima.sim(n=100, list(ar=0.99))
tsdisplay(y)


trend <- c(1:100)
tsdisplay(trend)
plot(forecast(Arima(y, seasonal = c(1,0,0)), h=100))

mod1 <- lm(y ~ trend + I(trend^2) + I(trend^3))
summary(mod1)

set.seed(1)
y <- arima.sim(n=100, list(ar=0.5))

d <- Arima(y, order=c(0,0,4))
(1-pnorm(abs(d$coef)/sqrt(diag(d$var.coef))))*2 #check p-value 


ym <- hhi_q_i[1:89,1]
tsdisplay(ym)
Arima(ym, order = c(1,1,0))
Arima(ym, order = c(2,1,0))
Arima(ym, order = c(3,1,0))
Arima(ym, order = c(1,1,1))

ym <- hhi_q_i[30:61,1]
ym
ar <- auto.arima(ym)
AIC(ar)

ym <- hhi_q_i[1:89,1]
model <- Arima(ym, order=c(2,1,0))
forecast(model, h=3)


library(hydroGOF)
ym <- hhi_q_i[1:86,1]

model <- Arima(ym, order=c(2,1,2))
model <- Arima(ym, order=c(0,1,0))
model <- Arima(ym, order=c(1,1,2))
model <- Arima(ym, order=c(1,1,3))
fc <- forecast(model, h=3)

mse(sim = as.numeric(fc$mean), obs = as.numeric(hhi_q_i[87:89,1]))



ym <- hhi_q_i[1:89,1]
model <- Arima(ym, order=c(1,1,0), seasonal = c(0,0,1))
model


ym <- hhi_q_i[1:89,]
ym$crysis <- rep(0, 89)
ym[grep("2008...|2009...", time(ym)),4] <- 1
Arima(ym[,1], order=c(2,1,1), xreg = ym[,4])


set.seed(70)

y1 <- arima.sim(n=100, list(ar=0.7))

plot(y1,type="l",axes=T, ylab = "variable Y")

rect(20,-1000,25,1000,col="#FFCCEE",border="#FFCCEE")

rect(70,-1000,80,1000,col="#FFCCEE",border="#FFCCEE")

par(new=TRUE)

plot(y1,type="l",ylab="")
