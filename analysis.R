# ---------------------------------------------------------------
# Pairs trading analysis project
# -----------------------------------------------------------------

rm(list=ls())
setwd('C:\\Users\\Yuriy\\Documents\\Pitt\\Data Mining (INFSCI 2160)\\FinalCourseProject\\Data')


# (1) Prepare data [v]:

acn<-read.csv2('acn.csv', sep=',', dec='.')
adbe<-read.csv2('adbe.csv', sep=',', dec='.')
ads<-read.csv2('ads.csv', sep=',', dec='.')
aet<-read.csv2('aet.csv', sep=',', dec='.')
akam<-read.csv2('akam.csv', sep=',', dec='.')
amzn<-read.csv2('amzn.csv', sep=',', dec='.')
bac<-read.csv2('bac.csv', sep=',', dec='.')
bsx<-read.csv2('bsx.csv', sep=',', dec='.')
cat<-read.csv2('cat.csv', sep=',', dec='.')
csco<-read.csv2('csco.csv', sep=',', dec='.')
mmm<-read.csv2('mmm.csv', sep=',', dec='.')
t<-read.csv2('t.csv', sep=',', dec='.')

spy<- read.csv2('spy.csv', sep=',', dec='.')
  
prices<-data.frame(acn$Date, acn$Adj.Close, adbe$Adj.Close, ads$Adj.Close, aet$Adj.Close, akam$Adj.Close,
           amzn$Adj.Close, bac$Adj.Close, bsx$Adj.Close, cat$Adj.Close, csco$Adj.Close, 
           t$Adj.Close, acn$Adj.Close)

colnamess<-c('date', 'acn', 'adbe', 'ads', 'aet', 'akam', 'amzn', 'bac', 'bsx', 'cat', 'csco', 'mmm', 't')
stoxx<-c('acn', 'adbe', 'ads', 'aet', 'akam', 'amzn', 'bac', 'bsx', 'cat', 'csco', 'mmm', 't')
colnames(prices)<-colnamess
prices[1:3,]

logprices<-prices
logprices[,2:13]<-log(prices[,2:13])

logprices[1:3,]

# (2) Compute returns [v]:
returns<-prices
returns<-returns[2:1007,]
for(i in stoxx){
  returns[[i]]<-diff(logprices[[i]])
}
returns[1:3,]
returns$date2<-as.Date(as.character(returns$date))

mean(returns$acn)*251*100
mean(returns$adbe)*251*100
mean(returns$ads)*251*100
mean(returns$aet)*251*100
mean(returns$akam)*251*100
mean(returns$amzn)*251*100
mean(returns$bac)*251*100
mean(returns$bsx)*251*100
mean(returns$cat)*251*100
mean(returns$csco)*251*100
mean(returns$mmm)*251*100
mean(returns$t)*251*100

## (3) Evaluate pairs, get top 3 pairs:
# Need: table with columns: 1-pair, 2-stock1, 3-stock2, 4-measure, 5-rank

TestStart<-"2016-01-01"
TestEnd<-"2016-12-31"

prices$date2<-as.Date(as.character(prices$date))
logprices$date2<-as.Date(as.character(logprices$date))

prices_test<-subset(prices, date2>=as.Date(TestStart)& date2<=as.Date(TestEnd))
prices_test[1:3,]

logprices_test<-subset(logprices, date2>=as.Date(TestStart)& date2<=as.Date(TestEnd))
logprices_test[1:3,]


nn <- length(stoxx)

pairs <- data.frame(matrix(ncol=5, nrow=(nn*nn-nn)/2))
colnames(pairs)<- c('pair', 'stock1', 'stock2', 'distance', 'rank')

k = 0  
for(i in 1:12){
  for(j in 1:12){
    if(i>=j) next
    k = k+1
    a=stoxx[i]
    b=stoxx[j]
    pairs[[1]][k]=paste(a,b, sep=" ")
    pairs[[2]][k]=a
    pairs[[3]][k]=b
    pairs[[4]][k]=(sum(logprices_test[[a]]*logprices_test[[b]]))^0.5
  }
}

pairs$rank<-rank(pairs$distance, ties.method = "first")
pairs[1:3,]
toppairs<-pairs[order(pairs$rank),]
toppairs[1:3,]

thepair<-as.matrix(toppairs[1,2:3])
thepair[1]
thepair[2]

## (4) Traiding:
# Input: period
# Step1: 1-normalized prices, 2-distances, 3-Whether hold stock, 4-Whether hold cash

TradeStart<-"2017-01-01"
TradeEnd<-"2017-12-05"
Amount<-1000.0

prices$date2<-as.Date(as.character(prices$date))
prices_trade<-subset(prices, date2>=as.Date(TradeStart)& date2<=as.Date(TradeEnd))
prices_trade$amount<- Amount
prices_trade[1:3,]

keeps<-c('date2', thepair[1], thepair[2])
prices_trade<-prices_trade[keeps]
prices_trade[1:3,]

prices_trade$bac_n <-(prices_trade$bac-mean(prices_test$bac))/sd(prices_test$bac)
prices_trade$bsx_n <-(prices_trade$bsx-mean(prices_test$bsx))/sd(prices_test$bsx)
prices_trade[1:3,]

minn=min(c(min(prices_trade$bac_n),min(prices_trade$bsx_n)))
maxx=max(c(max(prices_trade$bac_n),max(prices_trade$bsx_n)))

plot(prices_trade$date2, prices_trade$bac_n, ylim=range(c(minn, maxx)), ylab="BAC (Red), BSX (Green)", xlab="Date", type="l", col="red")
lines(prices_trade$date2, prices_trade$bsx_n, type="l", col="green")


# ---------TRADES tracking --------------------------------

prices_trade$bac_inv <- 0
prices_trade$bsx_inv <- 0

prices_trade$cash <- 0
prices_trade$cash[[1]] <- 1

prices_trade$bac_num <-0
prices_trade$bsx_num <-0

prices_trade$bac_val <-0
prices_trade$bsx_val <-0

prices_trade$cash_val <-0
prices_trade$cash_val[[1]] = 1000

# Think about sequencing: e.g. today and yesterdays (!!!)

for(i in 2:length(prices_trade[,1])){

  if(prices_trade$cash[[i-1]]==1){
    if(prices_trade$bac_n[[i]] - prices_trade$bsx_n[[i]] < -0.5){
      prices_trade$bac_inv[[i]] = 1
      prices_trade$cash[[i]] = 0
      prices_trade$cash_val[[i]] = 0

      prices_trade$bac_num[[i]] = prices_trade$cash_val[[i-1]]/prices_trade$bac[[i]]
      prices_trade$bac_val[[i]] = prices_trade$bac_num[[i]]*prices_trade$bac[[i]]

      
    } else if(prices_trade$bsx_n[[i]] - prices_trade$bac_n[[i]]  < -0.5){
      prices_trade$bsx_inv[[i]] = 1
      prices_trade$cash[[i]] = 0
      prices_trade$cash_val[[i]] = 0
      
      prices_trade$bsx_num[[i]] = prices_trade$cash_val[[i-1]]/prices_trade$bsx[[i]]
      prices_trade$bsx_val[[i]] = prices_trade$bsx_num[[i]]*prices_trade$bsx[[i]]
    } else {
      prices_trade$cash[[i]] = 1
      prices_trade$cash_val[[i]] = prices_trade$cash_val[[i-1]]
    }
    

  } else if(prices_trade$bac_inv[[i-1]] == 1){
      if(prices_trade$bac_n[[i]] - prices_trade$bsx_n[[i]] >=-0.1){
        prices_trade$bac_inv[[i]] = 0
        prices_trade$cash[[i]] = 1
        prices_trade$cash_val[[i]] = prices_trade$bac_num[[i-1]]*prices_trade$bac[[i]]
        prices_trade$bac_val[[i]] = 0
        prices_trade$bac_num[[i]] = 0
      } else {
          prices_trade$bac_num[[i]] = prices_trade$bac_num[[i-1]]
          prices_trade$bac_val[[i]] = prices_trade$bac_num[[i]]*prices_trade$bac[[i]]
          prices_trade$bac_inv[[i]] = 1        
      }
    } else if(prices_trade$bsx_inv[[i-1]] == 1){
    if(prices_trade$bsx_n[[i]] - prices_trade$bac_n[[i]] >=-0.1){
      prices_trade$bsx_inv[[i]] = 0
      prices_trade$cash[[i]] = 1
      prices_trade$cash_val[[i]] = prices_trade$bsx_num[[i-1]]*prices_trade$bsx[[i]]
      prices_trade$bsx_val[[i]] = 0
      prices_trade$bsx_num[[i]] = 0
    } else {
        prices_trade$bsx_num[[i]] = prices_trade$bsx_num[[i-1]]
        prices_trade$bsx_val[[i]] = prices_trade$bsx_num[[i]]*prices_trade$bsx[[i]]
        prices_trade$bsx_inv[[i]] = 1
    }
  }
}


sum(prices_trade$bac_inv)
sum(prices_trade$bsx_inv)
prices_trade$portfolio = prices_trade$cash_val + prices_trade$bac_val + prices_trade$bsx_val
# ---------TRADES tracking  end --------------------------------

prices_trade[1:20,]


# (5) Evaluation:

spy$date2<-as.Date(as.character(spy$Date))
spy_trade <-subset(spy, date2>=as.Date(TradeStart)& date2<=as.Date(TradeEnd)) 
spy_trade$Ths <-spy_trade$Adj.Close/spy_trade$Adj.Close[1]*1000

(spy_trade$Ths[233]/spy_trade$Ths[1]-1)*100

prices_trade$portfolio[233]
prices_trade[200:233,]

Rf <- 1.23
R1<- mean(diff(log(spy_trade$Adj.Close)))*251*100
R2<- mean(diff(log(prices_trade$portfolio)))*251*100

SD1 <- sd(diff(log(spy_trade$Adj.Close)))*(251^0.5)*100
SD2 <- sd(diff(log(prices_trade$portfolio)))*(251^0.5)*100

Sharpe1 <- (R-Rf)/SD
Sharpe2 <- (R2-Rf)/SD2

spy_trade$Dummy<-spy_trade$Ths
spy_trade$Dummy[2:233]<-spy_trade$Ths[2:233]*1.2

plot(spy_trade$date2, spy_trade$Ths, ylab='SP500 Index', xlab='Date', type="l", col="blue")
plot(prices_trade$date2, prices_trade$portfolio, ylab='Portfolio', xlab='Date', type="l", col="green")

plot(spy_trade$date2, spy_trade$Ths, ylab = "S&P500(blue), Portfolio(green)", xlab = "Date",
     ylim=range(c(900, 1400)), type="l", col="blue")
lines(prices_trade$date2, prices_trade$portfolio, col="green")

plot(prices_trade$date2, prices_trade$bac_n, ylim=range(c(minn, maxx)),type="l", col="red")
lines(prices_trade$date2, prices_trade$bsx_n, type="l", col="green")

linmod <-lm(prices_trade$portfolio ~ spy_trade$Ths)
summary(linmod)
