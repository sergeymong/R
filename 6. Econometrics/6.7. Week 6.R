# library(devtools)
# install_github("dgrtwo/broom")
# install_github("cran/bstats")
# install_github("bdemeshev/rusquant")
# install_github("bdemeshev/sophisthse")

library(lubridate)
library(sandwich)
library(lmtest)
library(car)
library(bstats)
library(zoo)
library(xts)
library(dplyr)
library(broom)
library(ggplot2)
library(Ecdat)

library(quantmod) #загрузка с finance google
library(rusquant) #загрузка с finam
library(sophisthse)
library(Quandl)


# work with dates
x <- c("2012-04-15", "2011-08-17")
y <- ymd(x)

y + days(20)

y - years(10)
day(y)
month(y)
year(y)

# create time series
x <- rnorm(5)
y <- ymd("2014-01-01") + days(0:4)

ts <- zoo(x, order.by = y)
ts

stats::lag(ts, -1) #showing value for yesterday,compare it with ts and will understand
stats::lag(ts, 1) #showing value for tomorrow,compare it with ts and will understand

diff(ts) #showing difference between dates, beginning since tomorrow

z <- (stats::lag(ts, -1)+stats::lag(ts, -2)+stats::lag(ts, -3))/3 #showing mean value for past 3 days
z

ts2 <- zooreg(x, start=as.yearqtr("2014-01"), freq=4)
ts2
ts3 <- zooreg(x, start=as.yearmon("2014-01"), freq=12)
ts3

#
data("Investment")
start(Investment)
end(Investment)
time(Investment)

coredata(Investment) # data without time

# делаем пропущенные значения. Их можно заполнить или с помощью линейной аппроксимации -- среднее между двумя значениями выше и ниже, или взять просто предыдущее значение
dna <- Investment
dna[1, 2] <- NA
dna[5, 3] <- NA

na.approx(dna)
na.locf(dna)


#download from outside sources
a <- sophisthse("POPNUM_Y")

b <- Quandl("FRED/GNP")
b

# finance google
Sys.setlocale("LC_TIME", "C") #set english locale, because russian and english date formats are different
getSymbols(Symbols = "AAPL", from="2010-01-01",
           to="2014-03-04", src="yahoo")

head(AAPL)
tail(AAPL)

# finam
getSymbols(Symbols = "GAZP", from="2010-01-01",
           to="2014-03-04", src="Finam")

head(GAZP)
tail(GAZP)

plot(GAZP)
autoplot(GAZP[,1:4])
autoplot(GAZP[,1:4], facets = NULL)

chartSeries(GAZP)


#robust intervals
d <- as.zoo(Investment)
autoplot(d[,1:2], facets = NULL)

model <- lm(data=d, RealInv~RealInt+RealGNP)
summary(model)

coeftest(model)

confint(model)

d_aug <- augment(model, as.data.frame(d))

qplot(data=d_aug, lag(.resid), .resid)

vcov(model)
vcovHAC(model)

coeftest(model, vcov. = vcovHAC(model))
conftable <- coeftest(model, vcov. = vcovHAC(model))
ci <- data.frame(estimate=conftable[,1], se_ac=conftable[,2])
ci <- mutate(ci, left_95=estimate-1.96*se_ac, right_95=estimate+1.96*se_ac)
ci

#durbin-watson
dwt(model)
res <- dwt(model)
res$p

#BG-test
bgtest(model, order = 6)

# если данных одного теста не хватило, чтобы отвергнуть гипотезу, а другому тесту хватило, то гипотеза отвергается

# дз
d <- Griliches
d
model <- lm(lw80 ~ age80 + iq + school80 + expr80, d)
vcov(model)
vcov(model) - vcovHC(model,type = "HC3")

vcovHC(model,type = "HC4m")[2,2]
vcovHC(model,type = "HC3")[2,2]
vcovHC(model,type = "HC5")[2,2]
vcovHC(model,type = "HC1")[2,2]

d <- augment(model, d)
bptest(model, varformula = ~ iq, data = d)

gqtest(model, order.by = ~expr80, data = d, fraction = 0.2)


d <- Solow
model <- lm(q ~ k + A, d)
summary(model)
vcov(model) - vcovHAC(model)

model2 <- lm(q ~ k, d)
dw <- dwt(model2)
dw$dw

bgtest(model, order = 3)


Sys.setlocale("LC_TIME","C")

getSymbols(Symbols = "AAPL",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(AAPL$AAPL.Close, main = "")

getSymbols(Symbols = "GOOG",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(GOOG$GOOG.Close, main = "")

getSymbols(Symbols = "INTC",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(INTC$INTC.Close, main = "")

getSymbols(Symbols = "MSFT",from="2010-01-01", to="2014-02-03",src="yahoo")
plot(MSFT$MSFT.Close, main = "")

p <- MSFT$MSFT.Close

summary(lm(p ~ stats::lag(p, -1) + stats::lag(p, -2)))
