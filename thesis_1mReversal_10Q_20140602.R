###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
# http://systematicinvestor.wordpress.com/?s=residual
###############################################################################
#setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
#=======================================================================
# http://systematicinvestor.wordpress.com/?s=minimum+variance+portfolio
#
#

#*****************************************************************
# Load historical data
#****************************************************************** 
packages <- c("quantmod", "PerformanceAnalytics", "reshape2", "fBasics", "Hmisc", 
              "xtsExtra", "RColorBrewer", "fPortfolio", "BurStFin", "lubridate", 
              "ggplot2", "quadprog", "corpcor",
              "htmlTable", "lpSolve", "scales", "Rsolnp", "rbenchmark")

packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

load.packages('quantmod') 
load.packages('PerformanceAnalytics')
load.packages('reshape2')
load.packages('fBasics')
load.packages('Hmisc')
load.packages('xtsExtra') 
load.packages('RColorBrewer')
load.packages('fPortfolio')
load.packages('BurStFin')
load.packages('lubridate')
load.packages('ggplot2')
load.packages('quadprog,corpcor,lpSolve')
load.packages('scales')
load.packages('Rsolnp')
load.packages('rbenchmark')
#install.packages("Rdonlp2", repos="http://R-Forge.R-project.org")

require("PerformanceAnalytics")
library(reshape2)
#library(xtsExtra)
library(zoo)
library(fPortfolio)
#na.locf(data.frame(rep("a",4), 1:4,1:4, c(1,NA,NA,NA)))
library(reshape2)
library(Hmisc)
library(RColorBrewer)
library(PerformanceAnalytics)
library(BurStFin)
library(lubridate)
library(ggplot2)
#library(Rdonlp2)
library(scales)
library(Rsolnp)
library(rbenchmark)
library(dplyr)
library(timetk)
library(tidyverse)

#=========================================================================
# The following part is about importing raw data and restructuring data
#=========================================================================
# import monthly closing price;
#=====================================

#===============================
# Download data from TEJ, choose "common stock without ETF and ADR and F stocks" category, 
# and filled missing data with NA
#===============================
m.price = read_tsv("../thesis_2014/data/m_close_1991_2013.txt")
m.price <- m.price %>% 
           rename(id = 證券代碼, name = 簡稱, date = 年月, price = `收盤價(元)_月`) %>% 
           mutate(id = as.character(id)) %>% 
           select(id, date, price) %>% 
           spread(key = id, value = price)
          
dim(m.price)
# use dcast to reorder dataframe by date;
mprice.reorder = dcast(m.price,date~id)
head(mprice.reorder)
tail(mprice.reorder[,1])
class(mprice.reorder)
dim(mprice.reorder)
n_month = dim(mprice.reorder)[1]
mon.seq1 = seq(as.Date("1991-02-01"), length=n_month, by="1 month")-1 
mprice.reorder["date"] = mon.seq1
#=====================================================
# write out data
write.csv(mprice.reorder, "../thesis_2014/data/m_close_1991_2013_reorder.csv")
#=====================================================
#====================================================
# import Fama French 4 factors monthly return series;
# code in TEJ: Y9999:
#====================================================
ff4f.tw = read.table("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/FF3F_1990_2013_month_Y9999.txt")
ff4f.tw = ff4f.tw[,c(-1,-2)]
ff4f.tw = ff4f.tw[-1,]
colnames(ff4f.tw)=c("date","market","size","hml","momentum","RF") 
#hml:high-minus-low book value
n_month = dim(ff4f.tw)[1]
mon.seq = seq(as.Date("1990-02-01"), length=n_month, by="1 month") - 1
head(ff4f.tw)
ff4f.tw["date"] = mon.seq
#=====================================================
# write out data 
write.csv(ff4f.tw, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/ff4f.csv")
#=====================================================

#=======================================
# import monthly close TWSE index 1990-2013
#========================================
twse = read.table("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/monthly data/m_close_twse_1990_2013.txt")
twse = twse[-1, c(-1,-2)]
twse[,1] = mon.seq
#===========================================
#write out data
#===========================================
write.csv(twse, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/monthly data/m_close_twse_1990_2013.csv")

#================================
# import daily log returns;
#================================
daily.ret = read.table("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/daily data/daily_lnret_19900101_20131231.txt")
daily.ret = daily.ret[,-2]
daily.ret = daily.ret[-1,]
head(daily.ret,10)
colnames(daily.ret) = c("id","date","ret")
dim(daily.ret)
# use dcast to reorder dataframe by date;
ret.reorder = dcast(daily.ret,date~id)
head(ret.reorder)
class(ret.reorder)
dim(ret.reorder)
# ret.reorder[is.na(ret.reorder)] =0
#=====================================================
#write out data
write.csv(ret.reorder, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/daily data/ret_day.csv")
#=====================================================

#============================
# import daily close price
#=============================
daily.price = read.table("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/daily data/daily_close_price_19900101_20131231.txt")
daily.price = daily.price[,-2]
daily.price = daily.price[-1,]
head(daily.price,10)
colnames(daily.price) = c("id","date","price")
dim(daily.price)
# use dcast to reorder dataframe by date;
price.reorder = dcast(daily.price,date~id)
head(price.reorder)
tail(price.reorder)

#=====================================================
#write out data
write.csv(price.reorder, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/daily data/daily_price.csv")
#=====================================================

#====================================
# Import weekly close price
#====================================
wk.price = read.table("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/weekly/weekly_close_1991_2013.txt")
wk.price = wk.price[,-2]
wk.price = wk.price[-1,]
head(wk.price,10)
colnames(wk.price) = c("id","date","price")
dim(wk.price)
# use dcast to reorder dataframe by date;
wk.reorder = dcast(wk.price,date~id)
head(wk.reorder)
tail(wk.reorder)

#=====================================================
#write out data
write.csv(wk.reorder, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/weekly/wk_reorder.csv")
#=====================================================

#****************************************************************************
# The following section begins downloading the previously reordered data 
#****************************************************************************

#==============================================
# import file into data
#===============================================
# Import daily log returns from data 
#===============================================
# For this part of data, when changing data type into xts, we found there exists problem of 
# converting numbers to strings don't know how to solve
#================================================
#day.ret = read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/daily data/ret_day.csv")

#===============================================
# Import daily close price from data 
#===============================================
#price.day = read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/daily data/daily_price.csv", header=TRUE)

#===============================================
# Import weekly close price from data 
#===============================================
wk.price = read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/weekly/wk_reorder.csv", header=TRUE)
wk.price = wk.price[,-1]
date.wk = (levels(factor(wk.price[,1])))
length(date.wk)
wk.seq = as.Date(date.wk, "%Y%m%d")
rownames(wk.price) = wk.seq
wk.price = wk.price[,-1]
wk.price.xts = as.xts(wk.price)
head(wk.price.xts)

#==================================================
# Import monthly data
#==================================================
#ret.tw=read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/monthly data/m_ln_ret_1990_2013_reorder.csv", header=TRUE)
price.tw = read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/monthly data/m_close_1991_2013_reorder.csv", header=TRUE)
#price.tw1 = na.omit(price.tw)
# Fama-French 3 factor returns
ff4f.tw = read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/ff4f.csv", header=TRUE)
twse = read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/monthly data/m_close_twse_1990_2013.csv", header=TRUE)
#==============================
# convert data to xts, zoo 
#==============================
# For this part of data, when changing data type into xts, we found there exists problem of 
# converting numbers to strings don't know how to solve
#=================================
#?????????????????????????????
day.ret = day.ret[,-1]
# dtest = day.ret[, 1:10]
# head(dtest)
# dtest.z = read.zoo(dtest)
# dtest.z
date.d = (levels(factor(day.ret[,1])))
length(date.d)
day.seq = as.Date(date.d, "%Y%m%d")
rownames(day.ret) = day.seq
day.ret = day.ret[,-1]
tail(day.ret)
day.ret.xts = as.xts(day.ret)
head(day.ret.xts)
tail(day.ret.xts)
#ret.tw.xts = as.xts(ret.tw)
#tail(ret.tw.xts)

#==================
# daily closing price
#==================
#????????????????????????????
price.day = price.day[,-1]
date.d = (levels(factor(price.day[,1])))
length(date.d)
day.seq = as.Date(date.d, "%Y%m%d")
rownames(price.day) = day.seq
price.day = price.day[,-1]
price.day.xts = as.xts(price.day)
tail(price.day.xts)
head(price.day.xts)
#===============================
# monthly closing price
#===============================
price.tw = price.tw[,-1]
rownames(price.tw)=price.tw[,1]
price.tw = price.tw[,-1]
price.tw.xts = as.xts(price.tw)
head(price.tw.xts)
tail(price.tw.xts)
#==============================
ff4f.tw = ff4f.tw[,-1]
rownames(ff4f.tw) = ff4f.tw[,1]
ff4f.tw = ff4f.tw[,-1]
ff4f.tw.xts = as.xts(ff4f.tw)
tail(ff4f.tw.xts)
#==============================
twse = twse[,c(-1,-2)]
#rownames(twse) = twse[,1]
n_month = length(twse)
mon.seq = seq(as.Date("1990-02-01"), length=n_month, by="1 month") - 1
twse.xts = as.xts(twse, order.by = mon.seq)
colnames(twse.xts)= "twse"
head(twse.xts)
#================================================
#
# 3 factors model and data :market, size and book ratio;

tickers.tw<-colnames(price.tw.xts)
tickers.tw
tickers.n<-length(tickers.tw)
tickers.n
# sample data range: starting date = 199201; end date=20111231
price.tw.sample <- price.tw.xts["199201/201312"]
wk.price.sample <- wk.price.xts["199201/201312"]
ff4f.tw.sample <- ff4f.tw.xts["199201/201312"]
twse.sample<- twse.xts["199201/201312"]
twse.close<-twse.sample[,1]
#head(twse.close)
#head(price.tw.sample)
#head(ret.tw.xts)
#head(ff.tw.sample)

#################################
#equal weight for stocks in TWSE 
################################
models.tw<-list()
data.tw <- new.env()
data.tw$prices<-price.tw.sample
data.tw$weight<-price.tw.sample
data.tw$execution.price<-price.tw.sample
data.tw$execution.price[]<-NA
#head(price.tw.sample,5)
#weight setting = equal weight;
#head(ntop(data.tw.sample,tickers.n))
data.tw$weight = ntop(price.tw.sample, tickers.n)
#head(model.tw$weight,5)
#sum(model.tw$weight[1,])
data.tw$weight[1:36,] = NA
#ret.tw<-data.tw$prices / mlag(data.tw$prices) - 1
#write.csv(data.tw$weight, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/equal_weights.csv")
#write.csv(ret.tw, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/ret_tw.csv")
#head(model.tw$weight[,1],40)
models.tw$equal.weight = bt.run(data.tw)
#sum(model.tw$weight[100,],na.rm=TRUE)
names(models.tw$equal.weight)
# ret = return of portfolio each period 
models.tw$equal.weight[3]
# equity = accumulative return of the portfolio;
models.tw$equal.weight[6]
################################
# TWSE (Taiwan stock index) as a benchmark;
################################
data.twse<-new.env()
data.twse$prices<-twse.close
data.twse$weight <- twse.close
data.twse$execution.price<-twse.close
data.twse$execution.price[,1]<-NA
data.twse$weight[,1] = 1
data.twse$weight[1:36,] = NA
#head(model.twse$prices)
#head(model.twse$weight)
#nrow(model.twse$prices)
models.tw$twse = bt.run(data.twse)
# ret = return of portfolio each period 
models.tw$twse[3]
# equity = accumulative return of the portfolio;
models.tw$twse[6]

####################################################################################
#Next let??s group stocks into Quantiles based on 1-Month returns and create back-test 
#for each Quantile. I will rely on the code in the Volatility Quantiles post to create 
#Quantiles.
#********************************************************************************
# Create Reversal Quantiles
#******************************************************************************** 
#Step 1: group returns into quintiles, the lowest returns in previous month as group 1 
#        the highest return as group 10?Fwe call this sorting as retmom (return momentum)
#Step 2: assign equal weight to stocks within each group
#########################################################################
n.quantiles = 10
start.t = 1 + 36
quantiles.tw = weights.tw = coredata(price.tw.sample) * NA    	
head(quantiles.tw)
one.month.tw = coredata(price.tw.sample / mlag(price.tw.sample))
head(one.month.tw[1:36,],20)

# t=37
for( t in start.t:nrow(weights.tw) ) {
  factor.tw = as.vector(one.month.tw[t,])
  # write.csv(factor.tw, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/output test/factor_tw.csv")
  ranking.tw = ceiling(n.quantiles * rank(factor.tw, na.last = 'keep','first') / count(factor.tw))
  # write.csv(ranking.tw, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/output test/ranking_tw.csv")  
  quantiles.tw[t,] = ranking.tw
  weights.tw[t,] = 1/tapply(rep(1,tickers.n), ranking.tw, sum)[ranking.tw]
  # tapply.test = tapply(rep(1,tickers.n), ranking.tw, sum)[ranking.tw]
  # write.csv(tapply.test, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/output test/tapply_test.csv")  
}

quantiles.tw = ifna(quantiles.tw,0)
#head(quantiles.tw[,1],140)
head(weights.tw)
#weights.tw[37:40,]
#write.csv(quantiles.tw, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/quantiles_stocks_10Q.csv")
#write.csv(weights.tw, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/weights_stocks_10Q.csv")

#*****************************************************************
# Create backtest for each Quintile
#****************************************************************** 
temp.tw = weights.tw * NA

#i=5
for( i in 1:n.quantiles) {
  temp.tw[] = 0
  temp.tw[quantiles.tw == i] = weights.tw[quantiles.tw == i]
  
  data.tw$weight[] = NA
  data.tw$weight = temp.tw
  #?N?U???v???g?X?t?s???ɮ?
  #name1<-paste("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/weights",i,sep='')
  #name2<-paste(name1,'.csv',sep='')
  #write.csv(temp.tw, file=name2)
  models.tw[[ paste('M1_Q',i,sep='') ]] = bt.run(data.tw, silent = T)
}

models.tw
names(models.tw)
#[1] "equal.weight" "twse"         "M1_Q1"        "M1_Q2"        "M1_Q3"        "M1_Q4"       
#[7] "M1_Q5"        "M1_Q6"        "M1_Q7"        "M1_Q8"        "M1_Q9"        "M1_Q10"  
names(models.tw[[1]]) #?Ĥ@?Ӽҫ?:equal.weight????????;
#[1] "weight"      "type"        "ret"         "best"        "worst"       "equity"     
#[7] "cagr"        "dates.index"

#?Ĥ@?Ӽҫ?:equal.weight?????ĤT?Ӧ???:ret
models.tw[[1]][3] 
#?ĤC?Ӽҫ?:the fifth quantile (M1_Q5) ?????ĤT?Ӧ???:ret
models.tw[[7]][3]

#??names(models.tw)?????G?A?̧ǱNreturn?X??
all.ret<-merge.xts(models.tw[[1]][3]$ret,models.tw[[2]][3]$ret,
                   models.tw[[3]][3]$ret,models.tw[[4]][3]$ret,
                   models.tw[[5]][3]$ret,models.tw[[6]][3]$ret,
                   models.tw[[7]][3]$ret,models.tw[[8]][3]$ret,
                   models.tw[[9]][3]$ret,models.tw[[10]][3]$ret,
                   models.tw[[11]][3]$ret,models.tw[[12]][3]$ret)
#names(all.ret)<-names(models.tw)
#head(all.ret)
#??names(models.tw)?????G?A?̧ǱNequity?X??
all.equity<-merge.xts(models.tw[[1]][6]$equity,models.tw[[2]][6]$equity,
                      models.tw[[3]][6]$equity,models.tw[[4]][6]$equity,
                      models.tw[[5]][6]$equity,models.tw[[6]][6]$equity,
                      models.tw[[7]][6]$equity,models.tw[[8]][6]$equity,
                      models.tw[[9]][6]$equity,models.tw[[10]][6]$equity,
                      models.tw[[11]][6]$equity,models.tw[[12]][6]$equity)
names(all.equity)<-names(models.tw)
names(all.ret)<-names(models.tw)
# These are return momentum (Q1-Q10) backtesting results compared with equal-weighting Taiwan stock portfolios 
# and TWSE index
#write.csv(as.data.frame(all.ret), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/all.ret_10Q.csv")
#write.csv(as.data.frame(all.equity), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/all.equity_10Q.csv")
#*****************************************************************
# Create Report
#******************************************************************
plotbt.custom.report.part1(models.tw)
#*****************************************************************
# Create Report
#******************************************************************   
setEPS()
postscript("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_quantile_1993_201312.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") 
plotbt.custom.report.part1(models.tw)
dev.off()
####################################################################
####################################################################
# head(quantiles.tw==5)
# head(weights.tw)
# head(temp.tw)
# test<-weights.tw[quantiles.tw == 5]
# test
#*****************************************************************
# Create Q1-Q10 spread based on return quantile
#****************************************************************** 
temp.tw[] = 0
temp.tw[quantiles.tw == 1] = weights.tw[quantiles.tw == 1]
temp.tw[quantiles.tw == n.quantiles] = -weights.tw[quantiles.tw == n.quantiles]

data.tw$weight[] = NA
data.tw$weight = temp.tw
models.tw$spread = bt.run(data.tw, silent = T)
models.tw$spread
names(models.tw$spread)
models.tw$spread[[6]]
ret.equity.spread<-merge.xts(models.tw$spread[[3]],models.tw$spread[[6]])
head(ret.equity.spread)
#write.csv(ret.equity.spread, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/ret_equity_spread.csv")
all.ret<-merge.xts(all.ret, models.tw$spread[[3]])
names(all.ret)<-names(models.tw)
head(all.ret)
all.equity<-merge.xts(all.equity, models.tw$spread[[6]])
names(all.equity)<-names(models.tw)
head(all.equity)
#write.csv(as.data.frame(all.ret), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/all.ret_all.csv")
#write.csv(as.data.frame(all.equity), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/all.equity_all.csv")
#*****************************************************************
# Create Report
#******************************************************************   
plotbt.custom.report.part1(models.tw[spl('twse,equal.weight,spread,M1_Q1,M1_Q2,M1_Q3')])
#*****************************************************************
# Create Report
#******************************************************************
setEPS()
postscript("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_equal_quantile_1993_201312.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") 
plotbt.custom.report.part1(models.tw[spl('twse,equal.weight,spread,M1_Q1,M1_Q2,M1_Q3')])
dev.off()


###################################################################
#?H?W???̳??S?v?��Q?թұo???Z?ĵ??G
###################################################################

##########################
# add factors and align
##########################
data.fa.tw <- new.env()
# ???Ƥ??e?????G
# data.fa.tw[[i]]:??i???Ѳ?????
# data.fa.tw$factors: ?T?]?l?ҫ?????
# data.fa.tw$prices: ?Ҧ??ɼƪѲ?????
#i="X1101.?x?d"
#head(data.tw$prices[,i])
#?N?S???ѻ????ƪ??Ѳ??R??(?Y?????ONA???ƪ?)=>fprices:filtered prices;
dim(data.tw$prices)
data.tw.fprices<-data.tw$prices[, sapply(data.tw$prices, function(x) 
                                  sum(is.na(x)))!=nrow(data.tw$prices)]
#?N???????Ʀh??36?????Ѳ??d?U
data.tw.ffprices<-data.tw.fprices[, sapply(data.tw.fprices, function(x) 
                                  sum(!is.na(x)))>36]                                  

head(data.tw.ffprices)
dim(data.tw.ffprices) 
#decrease # of stocks to 753 stocks
tickers.tw.f<-colnames(data.tw.ffprices)
tickers.tw.f
tickers.n.f<-length(tickers.tw.f)
tickers.n.f


# assign individual stock prices into data.fa.tw
for(i in tickers.tw.f) data.fa.tw[[i]] = data.tw.ffprices[,i]
head(data.fa.tw[['X1101']])
# assign factors data to data.fa.tw$factors
data.fa.tw$factors = ff4f.tw.sample / 100
head(data.fa.tw$factors)
head(data.fa.tw$factors$"RF")
#bt.prep(data.fa.tw, align='remove.na')
head(data.fa.tw[['X1101']])
data.fa.tw$prices<-data.tw.ffprices
data.fa.tw$weight<-data.tw.ffprices
head(data.fa.tw$prices)
ncol(data.fa.tw$prices)
#test<-data.fa.tw$symbolnames[-grep('factor', data.fa.tw$symbolnames)]
#test[1]

#*****************************************************************
# Compute Factor Attribution for each ticker
#****************************************************************** 
#===========================================================================
# Please run specific factor.rolling.regression() function first
# because we have modified part of the function code
#==================================================================
temp = NA * data.tw.ffprices
head(temp)
factors.tw  = list()
factors.tw$last.e = temp
factors.tw$last.e_s = temp
#FF?]?l?��R?j?k???p?Y?Ƽg?X
coeff.tw = list()
coeff.tw$b0 = temp #alpha
coeff.tw$b1 = temp #market
coeff.tw$b2 = temp #size
coeff.tw$b3 = temp #book
coeff.tw$r2 = temp #R-squared

i="X1101"
j=1
for(i in tickers.tw.f) {
  cat(i, '\n')
  
  # Factor Loadings Regression
  obj.tw = factor.rolling.regression(data.fa.tw, i, 36, silent=T,
                                     factor.rolling.regression.custom.stats)
  
  for(j in 1:len(factors.tw))  	
    factors.tw[[j]][,i] = obj.tw$fl$custom[,j]
  
  for (k in 1:len(coeff.tw))
    coeff.tw[[k]][,i] = obj.tw$fl$estimate[,k]
}

# factors.tw ???C?��C?ɪѲ????ݮt??(?]?t???зǤƤμзǤƪ??ݮt)
#*******************************
names(coeff.tw)
# [1] "b0" "b1" "b2" "b3" "r2"
names(coeff.tw[[1]])
head(coeff.tw[[1]])
#write.csv(coeff.tw[[1]], file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/coeff_b0_10Q.csv")
# data = data.fa.tw
names(factors.tw)
#[1] "last.e"   "last.e_s" "one.month"
names(factors.tw[[1]])
last(factors.tw[[1]])
#?N?̫??@?��U?ӪѲ????ݮt?μзǤƫ᪺?ݮt?ȼg?X???ɮ?
#write.csv(factors.tw[[1]], file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/last_e_10Q.csv")
#write.csv(factors.tw[[2]], file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/last_es_10Q.csv")
#write.csv(factors.tw[[3]], file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/last_1m.csv")
# add base strategy
factors.tw$one.month = coredata(data.tw.ffprices / mlag(data.tw.ffprices))

#############################################################################
# function to compute additional custom stats for factor.rolling.regression
#############################################################################
factor.rolling.regression.custom.stats <- function(x,y,fit) {
  n = len(y)
  e = y - x %*% fit$coefficients
  se = sd(e)
  return(c(e[n], e[n]/se))
}

#======================================================================================
# 3 factor model: just test running factor.rolling.regression() function start from here 
# no changed made to the original function
#======================================================================================
factor.rolling.regression <-function(
                            data,
                            ticker = data$symbolnames[-grep('factor', data$symbolnames)],
                            window.len = 36,
                            silent = F,
                            custom.stats.fn = NULL
)
{
  #ticker = "X1101"
  data = data.fa.tw
  prices = data$prices
  nperiods = nrow(prices)
  dates = index(data$prices)
  hist.returns = ROC(prices[,ticker], type = 'discrete')
  hist.returns = hist.returns - data$factors$RF
  yout = hist.returns
  y = coredata(yout)
  xout = data$factors[, -which(names(data$factors) == 'RF')]
  x = coredata(xout)
  ok.index = !(is.na(y) | (rowSums(is.na(x)) > 0))
  fit = ols(cbind(1,x[ok.index,]),y[ok.index], T)
  est = fit$coefficients
  std.err = fit$seb
  r2 = fit$r.squared
  fl.all = list()
  fl.all$estimate = c(est, r2)
  fl.all$std.error = c(std.err, NA)
  colnames = c('alpha', colnames(x), 'R2')
  estimate = make.xts(matrix(NA, nr = nperiods, len(colnames)), dates)
  colnames(estimate) = colnames
  fl = list()
  fl$estimate = estimate
  fl$std.error = estimate
  if( !is.null(custom.stats.fn) ) {
    temp = match.fun(custom.stats.fn)(cbind(1,x), y, fit)
    fl$custom = make.xts(matrix(NA, nr = nperiods, len(temp)), dates)
  }
  for( i in window.len:nperiods ) {
    window.index = (i - window.len + 1) : i
    if(all(!is.na(y[window.index]))) {
      xtemp = cbind(1,x[window.index,])
      ytemp = y[window.index]
      fit = ols(xtemp, ytemp, T)
      est = fit$coefficients
      std.err = fit$seb
      r2 = fit$r.squared
      fl$estimate[i,] = c(est, r2)
      fl$std.error[i,] = c(std.err, NA)
      if( !is.null(custom.stats.fn) )
        fl$custom[i,] = match.fun(custom.stats.fn)(xtemp, ytemp, fit)
    }
    if( i %% 10 == 0) if(!silent) cat(i, '\n')
  }
  return(list(fl.all = fl.all, fl = fl, window.len=window.len,
              y=yout, x=xout, RF=data$factors$RF))
}
#==== modifying factor.rolling.regression() function end of here ==================

#======================================================================================
# 1 factor model: modify factor.rolling.regression() function to become 1 factor model
# ... start from here: factor1.rolling.regression()
#======================================================================================

factor1.rolling.regression <-function(
  data,
  ticker = data$symbolnames[-grep('factor', data$symbolnames)],
  window.len = 36,
  silent = F,
  custom.stats.fn = NULL
)
{
  #ticker = "X1101"
  data = data.fa.tw
  prices = data$prices
  nperiods = nrow(prices)
  dates = index(data$prices)
  hist.returns = ROC(prices[,ticker], type = 'discrete')
  hist.returns = hist.returns - data$factors$RF
  yout = hist.returns
  y = coredata(yout)
  xout = data$factors[, 'market']
  x = coredata(xout)
  ok.index = !(is.na(y) | (rowSums(is.na(x)) > 0))
  fit = ols(cbind(1,x[ok.index,]),y[ok.index], T)
  est = fit$coefficients
  std.err = fit$seb
  r2 = fit$r.squared
  fl.all = list()
  fl.all$estimate = c(est, r2)
  fl.all$std.error = c(std.err, NA)
  colnames = c('alpha', colnames(x), 'R2')
  estimate = make.xts(matrix(NA, nr = nperiods, len(colnames)), dates)
  colnames(estimate) = colnames
  fl = list()
  fl$estimate = estimate
  fl$std.error = estimate
  if( !is.null(custom.stats.fn) ) {
    temp = match.fun(custom.stats.fn)(cbind(1,x), y, fit)
    fl$custom = make.xts(matrix(NA, nr = nperiods, len(temp)), dates)
  }
  for( i in window.len:nperiods ) {
    window.index = (i - window.len + 1) : i
    if(all(!is.na(y[window.index]))) {
      xtemp = cbind(1,x[window.index,])
      ytemp = y[window.index]
      fit = ols(xtemp, ytemp, T)
      est = fit$coefficients
      std.err = fit$seb
      r2 = fit$r.squared
      fl$estimate[i,] = c(est, r2)
      fl$std.error[i,] = c(std.err, NA)
      if( !is.null(custom.stats.fn) )
        fl$custom[i,] = match.fun(custom.stats.fn)(xtemp, ytemp, fit)
    }
    if( i %% 10 == 0) if(!silent) cat(i, '\n')
  }
  return(list(fl.all = fl.all, fl = fl, window.len=window.len,
              y=yout, x=xout, RF=data$factors$RF))
}

#======================================================================================
# One-factor model: modify factor.rolling.regression() function to become 1 factor model
# ... end in here: factor1.rolling.regression()
#======================================================================================



















#*******************************************************************************************
#Next let??s group stocks into Quantiles based on 1-Month Reversal factors and create reports.
#*******************************************************************************************

#*****************************************************************
# Create Quantiles
#****************************************************************** 
quantiles.tw = list()
index.tw = match( index(data.tw.ffprices), index(data.tw$prices) )
head(index.tw)
head(coredata(data.tw.ffprices))
data.fa.tw$execution.price<-data.tw.ffprices
data.fa.tw$execution.price[]<-NA
#name_10q = list() #?N?C?ժѲ??W???��O?C?X;
#**********************************************************
#???Ƶ{?????I?G
#???Ƨǩұo???G,???Jranking?H??quantiles
#?A?p???C?դ??Ѳ??ӼơA?íp?⥭?????v???A???Jweights??;
#**********************************************************
# position.score = coredata(factors.tw$last.e)
# data = data.fa.tw
# period.ends = index.tw
# run this modified function first because I have changed grouping from 5 to 10
#===============================================================================
bt.make.quintiles<-function(
  position.score,
  data,
  period.ends,
  n.quantiles = 10,
  start.t = 2,
  prefix = ''
)
{
  n = ncol(position.score)
  position.score = coredata(position.score)
  quantiles = weights = position.score * NA
  # t=37
  for( t in start.t:nrow(weights) ) {
    factor = as.vector(position.score[t,])
    ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
    quantiles[t,] = ranking
    weights[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]
  }
  quantiles = ifna(quantiles,0)
  temp = weights * NA
  models = list()
  stock_names = list()
  #i=1
  for( i in 1:n.quantiles) {
    temp[] = 0
    #weights[quantiles == 1]:?Q??"quantiles == 1"?N?Ĥ@?ժ??Ѳ???index???X?A?i?ӿz???X?ӹ?��?Ѳ??v???????ơA
    #?é??Jtemp[]???F
    temp[quantiles == i] = weights[quantiles == i]
    data$weight[] = NA
    data$weight[period.ends,] = temp
    models[[ paste(prefix,'Q',i,sep='') ]] = bt.run(data, silent = T)
  }
  temp[] = 0
  temp[quantiles == 1] = weights[quantiles == 1]
  temp[quantiles == n.quantiles] = -weights[quantiles == n.quantiles]
  data$weight[] = NA
  data$weight[period.ends,] = temp
  models$spread = bt.run(data, silent = T)
  models$quantiles = quantiles
  # write.csv(quintiles, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/last_e_10Q.csv")
  return(models)
}


name = "last.e_s"
for(name in names(factors.tw)) {
  cat(name, '\n')
  quantiles.tw[[name]] = bt.make.quintiles(factors.tw[[name]], data.fa.tw, index.tw, start.t =  1+36, prefix=paste(name,'_',sep=''))
  #?N?C???Q?դ��ո??Ƽg?X
  filename1=paste(name,"_10Q_ranking.csv",sep="")
  filename2=paste("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/", filename1,sep="")
  #write.csv(quantiles.tw[[name]]$quantiles, file=filename2)
} 

#write out residual values for each stock
write.zoo(factors.tw[["last.e_s"]], "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/position_score.csv", sep=',')

names(quantiles.tw)
#[1] "last.e"    "last.e_s"  "one.month"
names(quantiles.tw$last.e_s)
#[1] "last.e_s_Q1"  "last.e_s_Q2"  "last.e_s_Q3"  "last.e_s_Q4"  "last.e_s_Q5"  "last.e_s_Q6" 
#[7] "last.e_s_Q7"  "last.e_s_Q8"  "last.e_s_Q9"  "last.e_s_Q10" "spread" "quantiles"        
# quantiles.tw$last.e_s ???Ĥ@?Ӧ???(Q1)???e?p?U?G
names(quantiles.tw$last.e_s[[1]])
#[1] "weight"      "type"        "ret"         "best"        "worst"       "equity"     
#[7] "cagr"        "dates.index"
#quantiles.tw$last.e_s[[12]]?G?N???��Q?ժ??էO(quantiles);
dim(quantiles.tw$last.e_s[[12]])
head(quantiles.tw$last.e_s[[1]]["weight"])
names(quantiles.tw$one.month)
#[1] "one.month_Q1"  "one.month_Q2"  "one.month_Q3"  "one.month_Q4"  "one.month_Q5" 
#[6] "one.month_Q6"  "one.month_Q7"  "one.month_Q8"  "one.month_Q9"  "one.month_Q10"
#[11] "spread"        "quantiles"    
names(quantiles.tw$one.month[[1]])
names(quantiles.tw$one.month$one.month_Q1)
#[1] "weight"      "type"        "ret"         "best"        "worst"       "equity"     
#[7] "cagr"        "dates.index"

#es:?N??standardized residuals; e:?N??residuals;
all.ret.es.10Q<-merge.xts(quantiles.tw$last.e_s[[1]][3]$ret,quantiles.tw$last.e_s[[2]][3]$ret,
                          quantiles.tw$last.e_s[[3]][3]$ret,quantiles.tw$last.e_s[[4]][3]$ret,
                          quantiles.tw$last.e_s[[5]][3]$ret,quantiles.tw$last.e_s[[6]][3]$ret,
                          quantiles.tw$last.e_s[[7]][3]$ret,quantiles.tw$last.e_s[[8]][3]$ret,
                          quantiles.tw$last.e_s[[9]][3]$ret,quantiles.tw$last.e_s[[10]][3]$ret,
                          quantiles.tw$last.e_s[[11]][3]$ret)
all.equity.es.10Q<-merge.xts(quantiles.tw$last.e_s[[1]][6]$equity,quantiles.tw$last.e_s[[2]][6]$equity,
                             quantiles.tw$last.e_s[[3]][6]$equity,quantiles.tw$last.e_s[[4]][6]$equity,
                             quantiles.tw$last.e_s[[5]][6]$equity,quantiles.tw$last.e_s[[6]][6]$equity,
                             quantiles.tw$last.e_s[[7]][6]$equity,quantiles.tw$last.e_s[[8]][6]$equity,
                             quantiles.tw$last.e_s[[9]][6]$equity,quantiles.tw$last.e_s[[10]][6]$equity,
                             quantiles.tw$last.e_s[[11]][6]$equity)
names(all.equity.es.10Q)<-names(quantiles.tw$last.e_s)[1:11]
names(all.ret.es.10Q)<-names(quantiles.tw$last.e_s)[1:11]
#write.csv(as.data.frame(all.equity.es.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/all.equity_es_10Q.csv")
#write.csv(as.data.frame(all.ret.es.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/all.ret_es_10Q.csv")
#========================================================================================================

#******************************************************
# Replicate Table 4 in Blitz paper
#?p?????pFF model?Y?ƨC?թҦ??p?X???Y?ơA???䤤???Ƥ???????
# calculate the average of medians by 5 parameters of FF model 
# create two tables: one by es; one by return (momentum)
#*****************************************************
# 1. ?w???зǤƫ᪺?ݮt?Ȥ��ո??ƶi???p??
#10-Q ranking by es and return
quantiles = list()
quantiles[[1]] = coredata(quantiles.tw$last.e_s[[12]])
quantiles[[2]] = coredata(quantiles.tw$one.month[[12]])
# create a matrix to place average of medians of estimated coefficients: b0, b1, b2, b3, r2
coeff.meds = list()
coeff.meds[[1]] = matrix(data = NA, nrow=length(coeff.tw), ncol = 10)
coeff.meds[[2]] = matrix(data = NA, nrow=length(coeff.tw), ncol = 10)
k=1
i=1
j=1
for (j in 1:2){
  for (k in 1:length(coeff.tw)){
     coeff = coredata(coeff.tw[[k]])
     for( i in 1:n.quantiles) {
     #temp.coeff[] = 0
     temp.coeff = quantiles[[j]] * NA
     temp.coeff[quantiles[[j]] == i] = coeff[quantiles[[j]] == i]
     rmeds = apply(temp.coeff, 1, median, na.rm = TRUE)     ## get row medians
     rmeds.avg = mean(rmeds, na.rm = TRUE)
     coeff.meds[[j]][k,i] = rmeds.avg
    }
  }
}

coeff.meds.es.df = as.data.frame(coeff.meds[[1]], row.names = c("b0","b1","b2","b3","r2"))
colnames(coeff.meds.es.df) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
coeff.meds.es.df
#write.csv(coeff.meds.es.df, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/coeff_meds_es_10Q.csv")

coeff.meds.ret.df = as.data.frame(coeff.meds[[2]], row.names = c("b0","b1","b2","b3","r2"))
colnames(coeff.meds.ret.df) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
coeff.meds.ret.df
#write.csv(coeff.meds.ret.df, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/coeff_meds_ret_10Q.csv")
#======================================================================================================

#==================================================
#?H?U??MVP???v???p?⤧?????{??
#=======================================================================================================
# http://stackoverflow.com/questions/21525736/portfolio-optimization-with-r-with-known-mu-and-cov-matrix
# Portfolio optimization with R with known mu and cov matrix as inputs
#=======================================================================================================
# 
myPortfolioData <- function(mu, sigma, data, spec){
  if (is(data, "fPFOLIODATA")) 
    return(data)
  stopifnot(class(data) == "timeSeries")
  data = sort(data)
  nAssets = NCOL(data)
  names = colnames(data)
  if (is.null(names)) 
    names = paste("A", 1:nAssets, sep = "")
  #Cov = factor.model.stat(data) # use the function from library 'BurStFin'
  Cov = cov(data)
  rownames(Cov) <- colnames(Cov) <- names
  .data = list(series = data, nAssets = nAssets, names = names)
  .statistics <- list(mean = colMeans(data), Cov = Cov, estimator = 'other', mu = mu, Sigma = sigma);
  .tailRisk = spec@model$tailRisk
  new("fPFOLIODATA", data = .data, statistics =.statistics, tailRisk = .tailRisk)
}

#================================================
# test running the function...
#================================================
lppAssets <- 100*LPP2005.RET[, c("SBI", "SPI", "LMI", "MPI")]
mu <- c(SBI=0.05, SPI=0.1, LMI=0.075, MPI=0.06)
sigma <- matrix(c(0.02657429, 0.01805751, 0.02048764, 0.02110555, 0.01805751, 0.03781108, 0.03859943, 0.02959261, 
                  0.02048764, 0.03859943, 0.04606304, 0.03043146, 0.02110555, 0.02959261, 0.03043146, 0.03880064), 
                  4, 4, dimnames=list(names(mu), names(mu))) 
globminSpec <- portfolioSpec()
myLppData <- myPortfolioData(mu, sigma, lppAssets, globminSpec)
myLppData
globminPortfolio = minvariancePortfolio(data = myLppData, spec = globminSpec, constraints = "LongOnly")
globminPortfolio
#===============================
# By original defaults 
#===============================
globminPortfolio.def = minvariancePortfolio(data = lppAssets, spec = globminSpec, constraints = "LongOnly")
globminPortfolio.def
#======================================
#sigma
temp.ret = hist.returns.ts[1:35,]
mu = colMeans(temp.ret)
sigma = factor.model.stat(temp.ret)
globminSpec = portfolioSpec()
myLppData <- myPortfolioData(mu, sigma, temp.ret, globminSpec)
#===========================================================================
globminSpec = portfolioSpec()
#===========================================================================
# modifying bt.make.qunitiles() function code to be bt.make.quintiles.mvp()
#===========================================================================
# import previously saved residual values from file
position.score.r = read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/position_score.csv")
d.seq = as.Date(position.score.r[,1], "%Y/%m/%d")
position.score.xts = as.xts(position.score.r[,-1], order.by = d.seq)
#=====================================================================

########## ERC function code ############################
ob.fn <- function(x,CovMat){
  XSigmaX <-  x * (CovMat %*% x)
  return(sum(outer(XSigmaX, XSigmaX, "-")^2))
}

eq.fun <- function(x,CovMat){
  sum(x[1:ncol(CovMat)]) 
}
#########################################################

# position.score: ?C?ɪѲ??b?C?��??ݮt??
position.score = factors.tw[["last.e_s"]]
#factors.tw[["last.e_s"]][,8]
# fill missing price data with the last available one
#price.tw.xts = na.locf(price.tw.xts)
#price.tw.xts = na.locf(price.tw.xts, fromLast = TRUE)
#data.fa.tw$prices = price.tw.xts["199112/201312"]
data.fa.tw$prices.wk = wk.price.xts["199201/201312"]
data.fa.tw$prices = price.tw.xts["199201/201312"]
data.fa.tw$execution.price = price.tw.xts["199201/201312"]
data.fa.tw$execution.price[] = NA
index.tw = match(index(data.fa.tw$prices), index(data.fa.tw$prices))
period.ends = index.tw
data = data.fa.tw
#period.ends = 36
n.quantiles = 10
t=37
prefix = ''
weights = list()

#===================================================================
# Below is the modified code for function of bt.make.quintiles.mvp()
#===================================================================
bt.make.quintiles.mv<-function(
  position.score,
  data,
  period.ends,
  n.quantiles = 10,
  start.t = 37,
  prefix = ''
)
{
  n = ncol(position.score)
  data$prices = data$prices[,names(position.score)]
  data$execution.price = data$execution.price[,names(position.score)]
  data$prices.wk = data$prices.wk[,names(position.score)]
  position.score.c = coredata(position.score)
  quantiles = weight = position.score.c * NA
  weights$equal = position.score.c * NA
  weights$min.var = position.score.c * NA
  weights$min.maxloss = weight
  weights$min.mad = weight
  weights$min.cvar = weight
  weights$min.cdar = weight
  weights$min.cor.insteadof.cov = weight
  weights$min.mad.downside = weight
  weights$min.risk.downside = weight
  weights$max.sharpe = weight
  weights$max.sharpe.ls = weight  # long-short 
  weights$max.sharpe.mkneutral = weight # market neutral
  weights$erc = weight # equal risk contribution
#============================================================
# following optimizations use a non-linear solver
# reference webpage
# http://systematicinvestor.wordpress.com/2011/10/14/maximum-loss-and-mean-absolute-deviation-risk-measures/
# http://systematicinvestor.wordpress.com/2011/10/25/expected-shortfall-cvar-and-conditional-drawdown-at-risk-cdar-risk-measures/
# http://systematicinvestor.wordpress.com/2011/10/27/the-most-diversified-or-the-least-correlated-efficient-frontier/
# http://systematicinvestor.wordpress.com/2011/11/01/minimizing-downside-risk/
# http://systematicinvestor.wordpress.com/2012/09/26/minimum-correlation-algorithm-speed-comparison/
# weights$erc = weight        
# weights$min.avgcor = weight  
#============================================================  
  #calculate monthly returns
#  ret.month = data$prices / mlag(data$prices) - 1
  # temp.10Q : a place to put quantile returns;
#  temp.10Q = data$prices * NA
#  temp.10Q = coredata(temp.10Q[,1:10])
#  colnames(temp.10Q) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
#  ret.10Q = list()
#   ret.10Q$equal = temp.10Q
#   ret.10Q$min.var = temp.10Q
#   ret.10Q$min.maxloss = temp.10Q             # min maximum loss
#   ret.10Q$min.mad = temp.10Q                 # min absolute deviation
#   ret.10Q$min.cvar = temp.10Q                # conditional VaR
#   ret.10Q$min.cdar = temp.10Q                # conditional drawdown at risk
#   ret.10Q$min.cor.insteadof.cov = temp.10Q   # The Most Diversified or The Least Correlated Efficient Frontier
#   ret.10Q$min.mad.downside = temp.10Q        # min Lower Semi-Absolute Deviation
#   ret.10Q$min.risk.downside = temp.10Q       # minimizing downside risk
#   ret.10Q$max.sharpe = temp.10Q
#   ret.10Q$max.sharpe.ls = temp.10Q
#   ret.10Q$max.sharpe.mkneutral = temp.10Q
#   ret.10Q$min.cor = temp.10Q                # min correlation portfolio
  
  
  for( t in start.t:nrow(weight) ) {
    #t=37
    #i=4
    #for( t in 251:263) {
    n = ncol(position.score.c)
    factor = as.vector(position.score.c[t,])
    ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
    quantiles[t,] = ranking
    weights$equal[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]
    #i=1    
    for (i in 1:10){ 
    tickeri = which(quantiles[t,]==i)
    tickeri.name = names(tickeri)
    indexi = index(position.score[t,])
    yeari = year(indexi)
    monthi = month(indexi)
    monthi = ifelse(monthi<10,paste("0",monthi, sep=""), monthi)
    year.month = paste(yeari,monthi,sep="")
    # find the monthly return in the next month
#    ret.index = which(index(ret.month) ==indexi)
#    ret.index = ret.index + 1
#    ret.i = ret.month[ret.index, names(tickeri)]
    #delete the next-monthly returns with NA values
#    ret.i = ret.i[,!is.na(ret.i)]
#    tickeri = names(ret.i)
    # calculate covariance and mean value by weekly returns;
    temp = data$prices.wk[year.month]
    last.i = which(index(data$prices.wk) == index(tail(temp,1)))
    # use past 140 weeks return to calculate covariance matrix
    hist.returns = ROC(data$prices.wk[(last.i-140):last.i,tickeri.name], type = 'discrete')
    hist.returns = na.omit(hist.returns)
    ia = create.historical.ia(hist.returns,52)
    s0 = apply(coredata(hist.returns),2,sd)
    ia$correlation = cor(coredata(hist.returns), use='complete.obs',method='pearson')
    ia$cov = ia$correlation * (s0 %*% t(s0))
    ia$parameters.alpha = 0.95 # parameter for CVaR and Cdar
    #=====================================================================
    # create constraints
    #=====================================================================
    n = length(tickeri)
    constraints = new.constraints(n, lb = 0, ub = 1)
    # SUM x.i = 1
    constraints = add.constraints(rep(1, n), 1, type = '=', constraints) 
    #===================================================================
    weights$min.var[t,ia$symbols]=min.risk.portfolio(ia, constraints)
    weights$min.maxloss[t,ia$symbols] = min.maxloss.portfolio(ia, constraints)
    weights$min.mad[t,ia$symbols] = min.mad.portfolio(ia, constraints)
    weights$min.cvar[t,ia$symbols] = min.cvar.portfolio(ia, constraints)
    weights$min.cdar[t,ia$symbols] = min.cdar.portfolio(ia, constraints)
    weights$min.cor.insteadof.cov[t,ia$symbols] = min.cor.insteadof.cov.portfolio(ia, constraints)
    weights$min.mad.downside[t,ia$symbols] = min.mad.downside.portfolio(ia, constraints)
    weights$min.risk.downside[t,ia$symbols] = min.risk.downside.portfolio(ia, constraints)
    weights$max.sharpe[t,ia$symbols] = max.sharpe.portfolio('long-only')(ia,constraints)
    weights$max.sharpe.ls[t,ia$symbols] = max.sharpe.portfolio('long-short')(ia,constraints)
    weights$max.sharpe.mkneutral[t,ia$symbols] = max.sharpe.portfolio('market-neutral')(ia,constraints)
    # calculate ERC weights
    p<-rep(0.01,n)
    erc <- solnp(pars = p,fun = ob.fn, eqfun = eq.fun, eqB = 1,
                 ineqfun = NULL, ineqLB = NULL, ineqUB = NULL,
                 LB = rep(0,n), UB = rep(1,n), CovMat =ia$cov)
    weights$erc[t,ia$symbols]<-erc$par
 
    }
  }

    #========================================================================================
    # following optimizations use a non-linear solver       
    #constraints$x0 = weights$erc[(t-1),]
    #weights$erc[t,ia$symbols] = find.erc.portfolio(ia, constraints)       
    #===
    #constraints$x0 = weights$min.avgcor[(t-1),]
    #weights$min.avgcor[t,ia$symbols] = min.avgcor.portfolio(ia, constraints)
    names(weights)
    quantiles = ifna(quantiles,0)
    temp = weights$min.var * NA
    models = list()
    stock_names = list()

    for (k in names(weights)){
    # k="equal"
      for( i in 1:n.quantiles) {
        temp[] = 0
        #weights[quantiles == 1]:?Q??"quantiles == 1"?N?Ĥ@?ժ??Ѳ???index???X?A?i?ӿz???X?ӹ?��?Ѳ??v???????ơA
        #?é??Jtemp[]???F
        temp[quantiles == i] = weights[[k]][quantiles == i]
        data$weight[] = NA
        data$weight[period.ends,] = temp
        models[[k]][[ paste(prefix,'Q',i,sep='') ]] = bt.run(data, type = 'weight', silent = T)
        #models[[ paste(prefix,'Q',i,sep='') ]] = bt.run.share(data,clean.signal = F)
      }
    }

  # k ="erc"
  for (k in names(weights)) {
    m = which(names(weights)==k)
    merge.temp <-merge.xts(models[[m]]$Q1$equity, models[[m]]$Q2$equity,
                           models[[m]]$Q3$equity, models[[m]]$Q4$equity,
                           models[[m]]$Q5$equity, models[[m]]$Q6$equity,
                           models[[m]]$Q7$equity, models[[m]]$Q8$equity,
                           models[[m]]$Q9$equity, models[[m]]$Q10$equity)
    names(merge.temp) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")                                 
    data.pg = fortify(merge.temp["1995/2013"], melt = TRUE)
    label.dat = fortify(tail(merge.temp,1), melt=TRUE)
    title = paste("Cumulative returns of model", m)
    title = paste(title, 'based on quintiles of residual returns')
    gp=  ggplot(data.pg, aes(x=Index, y=Value, group = Series)) +
         geom_line(aes(linetype=Series)) +
         geom_point(aes(shape=Series))+
         xlab("Index") + ylab("cumulative returns")+
         scale_x_datetime(breaks = date_breaks("1 year"),labels = date_format("%Y"))+
         geom_text(data = label.dat, aes(x=Index,y=Value, label = Series), hjust = -0.5)+
         ggtitle(title) +
         theme(plot.title=element_text(face="bold", size=14))+
         theme(legend.justification=c(0,0), legend.position=c(0,0.5))+
         theme(legend.text = element_text(colour="blue", size = 12, face = "bold")) +
         geom_hline(yintercept=c(1,10),colour="#990000", linetype="dashed")
    path_gp = paste(paste("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/", k, sep=""), '_10Q_equity',sep="")
    ExportPlot(gp, path_gp) # run ExportPlot() function first!
    # export statistics table
    models.m = rev(models[[m]])
    #  plotbt(models, plotX = T, log = 'y', LeftMargin = 3)            
    #  mtext('Cumulative Performance', side = 2, line = 1)
    pbt = plotbt.strategy.sidebyside(models.m, return.table = TRUE)
    path_risk = paste(paste("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/", k, sep=""), '_risk_table.csv', sep="")
    write.csv(pbt, path_risk)
  }

# draw boxplot
# k="erc"
for (k in names(weights)) {
  m = which(names(weights)==k)
  temp.ret <-merge.xts(models[[m]]$Q1$ret, models[[m]]$Q2$ret,
                         models[[m]]$Q3$ret, models[[m]]$Q4$ret,
                         models[[m]]$Q5$ret, models[[m]]$Q6$ret,
                         models[[m]]$Q7$ret, models[[m]]$Q8$ret,
                         models[[m]]$Q9$ret, models[[m]]$Q10$ret)
  names(temp.ret) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10") 
  boxplot.dat = fortify(temp.ret["1995/2013"], melt = TRUE)
  title = paste("Boxplot of monthly returns by model", m)
  title = paste(title, 'based on quintiles of residual returns')
  gp = ggplot(boxplot.dat, aes(x=Series, y=Value)) +
       geom_boxplot()+
       geom_hline(yintercept=c(0),colour="#990000", linetype="dashed")+
       xlab("Quintiles") + ylab("monthly returns")+
       ggtitle(title) 
        
  path_gp = paste(paste("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/", k, sep=""), '_10Q_boxplot',sep="")
  ExportPlot(gp, path_gp) # run ExportPlot() function first!
}

#=================================
# melt data first to use ggplot  
#=================================
#  data.pg = fortify(minvar.10Q.equity["1995/2013"], melt = TRUE)
#  head(data.pg)

#  gp = qplot(x = Index, y = Value, group = Series, colour = Series,
#      linetype = Series, facets = NULL, data = data.pg) 
#  gp
#  gp+geom_line() + xlab("Index") + ylab("cumulative returns")
#  last_plot() + scale_x_datetime(breaks = date_breaks("1 year"))
# label data point
# label.dat = fortify(tail(minvar.10Q.equity,1), melt=TRUE)

# another way to plot by ggplot

# gp=  ggplot(data.pg, aes(x=Index, y=Value, group = Series)) +
#      geom_line(aes(linetype=Series)) +
#      geom_point(aes(shape=Series))+
#      xlab("Index") + ylab("cumulative returns")+
#      scale_x_datetime(breaks = date_breaks("1 year"),labels = date_format("%Y"))+
#      geom_text(data = label.dat, aes(x=Index,y=Value, label = Series), hjust = -0.5)+
#      ggtitle("Cumulative returns of minimum variance portfolios based on quintiles of residual returns") +
#      theme(plot.title=element_text(face="bold", size=14))+
#      theme(legend.justification=c(0,0), legend.position=c(0,0.5))+
#      theme(legend.text = element_text(colour="blue", size = 12, face = "bold"))
# 
# 
# ggsave(paste(path_gp, '.pdf', sep=""), gp, width = 11.69, height = 8.27)
# #
  #============================
  # output graph in pdf
  #============================
ExportPlot <- function(gplot, filename, width=11.69, height=8.27) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}
# path_gp = "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/minvar_10Q_equity"
# ExportPlot(gp, path_gp)




  
  #=================================
  temp[] = 0
    temp[quantiles == 1] = weights[quantiles == 1]
    temp[quantiles == n.quantiles] = -weights[quantiles == n.quantiles]
    data$weight[] = NA
    data$weight[period.ends,] = temp
    models$spread = bt.run(data, silent = T)
    models$quantiles = quantiles
    # write.csv(quintiles, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/last_e_10Q.csv")
    return(models)
}
  
  write.csv(temp.10Q,"D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/tang_ret_10Q.csv" )
  write.csv(ret.10Q$equal, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_equal.csv" )
  write.csv(ret.10Q$min.var, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_minvar.csv" )
  write.csv(ret.10Q$min.max, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_minmax.csv" )
  write.csv(ret.10Q$min.mad, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_minmad.csv" )
  write.csv(ret.10Q$min.cvar, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_mincvar.csv" )
  write.csv(ret.10Q$min.cdar, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_mincdar.csv" )
  write.csv(ret.10Q$min.cor.insteadof.cov, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_mincor.csv" )
  write.csv(ret.10Q$min.mad.downside, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_minmaddownside.csv" )
  write.csv(ret.10Q$min.r.downside, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_minmaddownside.csv" )
  write.csv(ret.10Q$max.sharpe, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_maxsharpe.csv" )
  write.csv(ret.10Q$max.sharpe.ls, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_maxsharpels.csv" )
  write.csv(ret.10Q$max.sharpe.mkneutral, "D:/?Ȭw?j?ǺӤh?Z???ɽפ?/?f??/data_output/ret_10Q_maxsharpemkneutral.csv" )

names(weights)
#[1] "#min.var"               "min.maxloss"           "min.mad"               "min.cvar"              "min.cdar"             
#[6] "min.cor.insteadof.cov" "min.mad.downside"      "min.risk.downside"     "erc"                   "min.avgcor"           


?@temp.10Q.all = temp.10Q[38:264,]
  temp.10Q.all.1 = mfadsadfasdfasdfelt(temp.10Q.all)
  temp.10Q.all.1 = temp.10Q.all.1[,-1]
  colnames(temp.10Q.all.1) = c('quintiles','ret')
  p <- ggplot(temp.10Q.all.1, aes(quintiles, ret, fill=quintiles)) + geom_boxplot()
  print(p)
  qplot(Q1, data = as.data.frame(temp.10Q.all*100), geom = "histogram", binwidth = 0.3, xlim = c(-14,14))
  #qplot(Q1, data = temp.10Q.all.1*100, geom = "histogram", binwidth = 0.3, xlim = c(-14,14))
  }
  #quantiles = ifna(quantiles,0)
  #temp = weights * NA
  #models = list()
  #stock_names = list()
  # i=1
  # t=37
  
#=========================================================
# end of modifying function 
#=========================================================

#============================================
# test running bt.run() function
#============================================
b= data
trade.summary = F
do.lag = 1
do.CarryLastObservationForwardIfNA = TRUE
type = c('weight', 'share')
silent = F
capital = 100000
commission = 0
weight = b$weight
dates = 1:nrow(b$prices)


#=== test running bt.run() function =================
bt.run  <- function(
  b,
  trade.summary = F,
  do.lag = 1,
  do.CarryLastObservationForwardIfNA = TRUE,
  type = c('weight', 'share'),
  silent = F,
  capital = 100000,
  commission = 0,
  weight = b$weight,
  dates = 1:nrow(b$prices)
)
{
  dates.index = dates2index(b$prices, dates)
  type = type[1]
  if( !silent ) {
    cat('Latest weights :\n')
    print( last(weight) )
    cat('\n')
  }
  weight[] = ifna(weight, NA)
  if(do.lag > 0) {
    weight = mlag(weight, do.lag)
  }
  if(do.CarryLastObservationForwardIfNA) {
    weight[] = apply(coredata(weight), 2, ifna.prev)
  }
  weight[is.na(weight)] = 0
  weight1 = mlag(weight, -1)
  tstart = weight != weight1 & weight1 != 0
  tend = weight != 0 & weight != weight1
  trade = ifna(tstart | tend, FALSE)
  prices = b$prices
  if( sum(trade) > 0 ) {
    execution.price = coredata(b$execution.price)
    prices1 = coredata(b$prices)
    prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
    prices[] = prices1
  }
  if( type == 'weight') {
    ret = prices / mlag(prices) - 1
    ret[] = ifna(ret, NA)
    ret[is.na(ret)] = 0
  } else {
    ret = prices
  }
  temp = b$weight
  temp[] = weight
  weight = temp
  bt = bt.summary(weight, ret, type, b$prices, capital, commission, dates.index)
  bt$dates.index = dates.index
  if( trade.summary ) bt$trade.summary = bt.trade.summary(b, bt)
  if( !silent ) {
    cat('Performance summary :\n')
    cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')
    cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')
    cat('\n')
  }
  return(bt)
}

#========================
# bt.summary() test running
#=========================

bt.summary = function(
  weight,
  ret,
  type = c('weight', 'share'),
  close.prices,
  capital = 100000,
  commission = 0,
  dates.index = 1:nrow(weight)
)
{
  if( !is.list(commission) ) {
    if( type == 'weight')
      commission = list(cps = 0.0, fixed = 0.0, percentage = commission)
    else
      commission = list(cps = commission, fixed = 0.0, percentage = 0.0)
  }
  if(len(dates.index) != nrow(weight)) {
    weight = weight[dates.index,,drop=F]
    ret = ret[dates.index,,drop=F]
    close.prices = close.prices[dates.index,,drop=F]
  }
  type = type[1]
  n = nrow(ret)
  bt = list()
  bt$weight = weight
  bt$type = type
  com.weight = mlag(weight,-1)
  if( type == 'weight') {
    temp = ret[,1]
    temp[] = rowSums(ret * weight) -
      rowSums(abs(com.weight - mlag(com.weight)) * commission$percentage, na.rm=T)
    - rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, na.rm=T)
    bt$ret = temp
  } else {
    bt$share = weight
    bt$capital = capital
    prices = ret
    prices[] = bt.apply.matrix(coredata(prices), ifna.prev)
    close.prices[] = bt.apply.matrix(coredata(close.prices), ifna.prev)
    cash = capital - rowSums(bt$share * mlag(close.prices), na.rm=T)
    share.nextday = mlag(bt$share, -1)
    tstart = bt$share != share.nextday & share.nextday != 0
    tend = bt$share != 0 & bt$share != share.nextday
    trade = ifna(tstart | tend, FALSE)
    tstart = trade
    index = mlag(apply(tstart, 1, any))
    index = ifna(index, FALSE)
    index[1] = T
    totalcash = NA * cash
    totalcash[index] = cash[index]
    totalcash = ifna.prev(totalcash)
    totalcash = ifna(totalcash,0)
    portfolio.ret = (totalcash  + rowSums(bt$share * prices, na.rm=T)
                     - rowSums(abs(com.weight - mlag(com.weight)) * commission$cps, na.rm=T)
                     - rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, na.rm=T)
                     - rowSums(prices * abs(com.weight - mlag(com.weight)) * commission$percentage, na.rm=T)
    ) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) ) - 1
    bt$weight = bt$share * mlag(prices) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) )
    bt$weight[is.na(bt$weight)] = 0
    temp = ret[,1]
    temp[] = ifna(portfolio.ret,0)
    temp[1] = 0
    bt$ret = temp
  }
  bt$best = max(bt$ret)
  bt$worst = min(bt$ret)
  bankrupt = which(bt$ret <= -1)
  if(len(bankrupt) > 0) bt$ret[bankrupt[1]:n] = -1
  bt$equity = cumprod(1 + bt$ret)
  bt$cagr = compute.cagr(bt$equity)
  return(bt)
}









#==========================================
# end of function bt.make.quintiles()
#==========================================

# position.score = factors.tw$last.e
# period.ends = index.tw
# data = data.fa.tw
# t= 37
# name = "last.e_s"
# name = "one.month"
quantiles.mvp.tw = quantiles.tw
for(name in names(factors.tw)) {
  cat(name, '\n')
  quantiles.mvp.tw[[name]] = bt.make.quintiles.mvp(factors.tw[[name]], data.fa.tw, index.tw, start.t =  1+36, prefix=paste(name,'_',sep=''))
  #?N?ݮt?��?(?]?t???зǤƤμзǤƪ??ݮt)?H?Τ????S?v?????Ƽg?X?ɮ?
  #filename1=paste(name,"_10Q_ranking.csv",sep="")
  #filename2=paste("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/output test/", filename1,sep="")
  #write.csv(quantiles.tw[[name]]$quantiles, file=filename2)
}  

#i=1
#j=37
#?N?Ѳ??W?٨̤Q?դ��O???X, ???qlast.e???��ն}?l
quantiles = quantiles.tw$last.e[[12]]
temp = NA*quantiles
name_stock<-list()
for (i in 1:n.quantiles){
     name_stock[[i]] = temp
     for (j in 37:length(index.tw)){
          n_list<-names(which(quantiles.tw$last.e[[12]][j,]==i))
          name_stock[[i]][j,1:length(n_list)]<-n_list  
      }
     colnames(name_stock[[i]])<-NULL
     filename1=paste(i,"q_stocknames.csv",sep="")
     filename2=paste("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/", filename1,sep="")
     #write.csv(name_stock[[i]], file=filename2)
}

write.table(name_stock[[1]],"D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/1q_stock.names.txt",
             col.names=F,sep="\t", quote = FALSE)
# use complete.case to remove NA in stock names
id<-complete.cases(name_stock[[1]][37,]) 
name = name_stock[[1]][37,id]


#========================================================================================================
all.equity.es.mvp.10Q<-merge.xts(quantiles.tw$last.e_s[[1]][6]$equity,quantiles.tw$last.e_s[[2]][6]$equity,
                             quantiles.tw$last.e_s[[3]][6]$equity,quantiles.tw$last.e_s[[4]][6]$equity,
                             quantiles.tw$last.e_s[[5]][6]$equity,quantiles.tw$last.e_s[[6]][6]$equity,
                             quantiles.tw$last.e_s[[7]][6]$equity,quantiles.tw$last.e_s[[8]][6]$equity,
                             quantiles.tw$last.e_s[[9]][6]$equity,quantiles.tw$last.e_s[[10]][6]$equity,
                             quantiles.tw$last.e_s[[11]][6]$equity)

names(all.equity.es.mvp.10Q)<-names(quantiles.tw$last.e_s)[1:11]
write.csv(as.data.frame(all.equity.es.mvp.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/data/output test/all.equity_es_mvp_10Q.csv")
#========================================
#?H?U?????зǤƪ??ݮt???Ƥ��յ??G;
all.ret.e.10Q<-merge.xts(quantiles.tw$last.e[[1]][3]$ret,quantiles.tw$last.e[[2]][3]$ret,
                          quantiles.tw$last.e[[3]][3]$ret,quantiles.tw$last.e[[4]][3]$ret,
                          quantiles.tw$last.e[[5]][3]$ret,quantiles.tw$last.e[[6]][3]$ret,
                          quantiles.tw$last.e[[7]][3]$ret,quantiles.tw$last.e[[8]][3]$ret,
                          quantiles.tw$last.e[[9]][3]$ret,quantiles.tw$last.e[[10]][3]$ret,
                          quantiles.tw$last.e[[11]][3]$ret)
all.equity.e.10Q<-merge.xts(quantiles.tw$last.e[[1]][6]$equity,quantiles.tw$last.e[[2]][6]$equity,
                             quantiles.tw$last.e[[3]][6]$equity,quantiles.tw$last.e[[4]][6]$equity,
                             quantiles.tw$last.e[[5]][6]$equity,quantiles.tw$last.e[[6]][6]$equity,
                             quantiles.tw$last.e[[7]][6]$equity,quantiles.tw$last.e[[8]][6]$equity,
                             quantiles.tw$last.e[[9]][6]$equity,quantiles.tw$last.e[[10]][6]$equity,
                             quantiles.tw$last.e[[11]][6]$equity)
names(all.equity.e.10Q)<-names(quantiles.tw$last.e)[1:11]
names(all.ret.e.10Q)<-names(quantiles.tw$last.e)[1:11]
#write.csv(as.data.frame(all.ret.es.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/all.ret_es_10Q.csv")
#write.csv(as.data.frame(all.equity.es.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/all.equity_es_10Q.csv")
#write.csv(as.data.frame(all.ret.e.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/all.ret_e_10Q.csv")
#write.csv(as.data.frame(all.equity.e.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/all.equity_e_10Q.csv")

#?H?U???̪Ѳ????S?v?ƧǤ��դ????G
all.equity.ret.10Q<-merge.xts(quantiles.tw$one.month[[1]][6]$equity,quantiles.tw$one.month[[2]][6]$equity,
                         quantiles.tw$one.month[[3]][6]$equity,quantiles.tw$one.month[[4]][6]$equity,
                         quantiles.tw$one.month[[5]][6]$equity,quantiles.tw$one.month[[6]][6]$equity,
                         quantiles.tw$one.month[[7]][6]$equity,quantiles.tw$one.month[[8]][6]$equity,
                         quantiles.tw$one.month[[9]][6]$equity,quantiles.tw$one.month[[10]][6]$equity,
                         quantiles.tw$one.month[[11]][6]$equity)
all.return.ret.10Q<-merge.xts(quantiles.tw$one.month[[1]][3]$ret,quantiles.tw$one.month[[2]][3]$ret,
                              quantiles.tw$one.month[[3]][3]$ret,quantiles.tw$one.month[[4]][3]$ret,
                              quantiles.tw$one.month[[5]][3]$ret,quantiles.tw$one.month[[6]][3]$ret,
                              quantiles.tw$one.month[[7]][3]$ret,quantiles.tw$one.month[[8]][3]$ret,
                              quantiles.tw$one.month[[9]][3]$ret,quantiles.tw$one.month[[10]][3]$ret,
                              quantiles.tw$one.month[[11]][3]$ret)

names(all.return.ret.10Q)<-names(quantiles.tw$one.month)[1:11]
names(all.equity.ret.10Q)<-names(quantiles.tw$one.month)[1:11]
#?N?̪Ѳ????S?v?��ո??Ƽg?X
#write.csv(as.data.frame(all.equity.ret.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/all.equity_ret_10Q.csv")
#write.csv(as.data.frame(all.return.ret.10Q), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/all.return_ret_10Q.csv")
#maxDD<-compute.max.drawdown(quantiles.tw$one.month[[1]][6]$equity)
#cagr<-compute.cagr(quantiles.tw$one.month[[1]][6]$equity)

#*****************************************************************
# Create Report
#******************************************************************   				
plotbt.custom.report.part1(quantiles.tw$one.month$spread,quantiles.tw$last.e$spread,quantiles.tw$last.e_s$spread)

plotbt.strategy.sidebyside(quantiles.tw$one.month$spread,quantiles.tw$last.e$spread,quantiles.tw$last.e_s$spread)

plotbt.custom.report.part1(quantiles.tw$last.e_s)

#******************************************************************
# Performance analysis
#******************************************************************
names(quantiles.tw$one.month$spread)
names(quantiles.tw$last.e$spread)
names(quantiles.tw$last.e_s$spread)
#?N?n?��R?????ƦX??;
ret.risk<-merge.xts(models.tw[[1]][3]$ret,
                    models.tw[[2]][3]$ret,
                    quantiles.tw$one.month$spread[3]$"ret",
                    quantiles.tw$last.e$spread[3]$"ret",
                    quantiles.tw$last.e_s$spread[3]$"ret",
                    quantiles.tw$one.month[[1]][3]$ret,
                    quantiles.tw$last.e[[1]][3]$ret,
                    quantiles.tw$last.e_s[[1]][3]$ret,
                    quantiles.tw$one.month[[10]][3]$ret,
                    quantiles.tw$last.e[[10]][3]$ret,
                    quantiles.tw$last.e_s[[10]][3]$ret)
names(ret.risk)<-c("equal.weight","TWSE","spread.ret","e.spread","es.spread",
                   "Q1.ret","Q1.e","Q1.es","Q10.ret","Q10.e","Q10.es")

equity.risk<-merge.xts(models.tw[[1]][6]$equity,
                    models.tw[[2]][6]$equity,
                    quantiles.tw$one.month$spread[6]$"equity",
                    quantiles.tw$last.e$spread[6]$"equity",
                    quantiles.tw$last.e_s$spread[6]$"equity",
                    quantiles.tw$one.month[[1]][6]$equity,
                    quantiles.tw$last.e[[1]][6]$equity,
                    quantiles.tw$last.e_s[[1]][6]$equity,
                    quantiles.tw$one.month[[10]][6]$equity,
                    quantiles.tw$last.e[[10]][6]$equity,
                    quantiles.tw$last.e_s[[10]][6]$equity)
names(equity.risk)<-c("equal.weight","TWSE","spread.ret","e.spread","es.spread",
                   "Q1.ret","Q1.e","Q1.es","Q10.ret","Q10.e","Q10.es")
ret.risk<-ret.risk["199501/201112"]
equity.risk<-equity.risk["199501/201112"]
head(ret.risk)
head(equity.risk)

#***************************
# VaR
#***************************
performance.df<-data.frame()
var.df<-VaR(ret.risk, p=.95, method="modified")
#performance.df

#************************
# Expected shortfall
#************************
eshortfall<-ES(ret.risk, p=.95, method="modified")
performance.df<-as.data.frame(rbind(var.df, eshortfall))

#as.vector(test)
#*********************
# Sharpe Ratio
#*********************
RF.tw<-data.fa.tw$factors$"RF"
SR<-SharpeRatio.annualized(ret.risk, Rf = 0, scale=12, geometric=FALSE)
SR
SR1<-SharpeRatio(ret.risk, Rf = 0, p = 0.95, FUN = "StdDev")
SR1
performance.df<-rbind(performance.df, SR)
performance.df
#******************************************
# Benchmark return: Information ratio
#******************************************
#equal.weight.bn<-models.tw[[1]][3]$ret["199501/201112"]
#twse.bn<-models.tw[[2]][3]$ret["199501/201112"]
equal.weight.bn<-ret.risk[,1]
twse.bn<-ret.risk[,2]
InformationRatio(ret.risk[,-c(1,2)], twse.bn, scale=12)
IR<-InformationRatio(ret.risk[,-c(1,2)], equal.weight.bn, scale=12)
performance.df[4,c(-1,-2)]<-IR
#******************************************
# maxDrawdown
#*****************************************
mdd<-maxDrawdown(ret.risk, invert=FALSE)
chart.Drawdown(ret.risk,legend.loc="bottomleft")
drawdown.series<-Drawdowns(ret.risk)
write.csv(as.data.frame(drawdown.series), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/drawdown.csv")
#write.csv(as.data.frame(performance.df), file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/perfromance.csv")
#****************************************
# Sortino ratio
#****************************************
sortino<-SortinoRatio(ret.risk,MR=twse.bn)
sortino



position.score=factors.tw[["one.month"]]
# write.csv(position.score, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/last.e.csv")
n = ncol(position.score)
n
position.score = coredata(position.score)
head(position.score[,1],37)
quantiles.tm = weights.tm = position.score * NA
start.t=37
prefix = ''
for( t in start.t:nrow(weights.tm) ) {
  factor.tm = as.vector(position.score[t,])
  ranking.tm = ceiling(n.quantiles * rank(factor.tm, na.last = 'keep','first') / count(factor.tm))
  quantiles.tm[t,] = ranking.tm
  weights.tm[t,] = 1/tapply(rep(1,n), ranking.tm, sum)[ranking.tm]
}
quantiles.tm = ifna(quantiles.tm,0)
#write.csv(quantiles.tm, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/1m_quantiles.csv")
#write.csv(weights.tm, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/1m_weights.csv")

#************************************************************************************
temp = weights.tm * NA
models_tw = list()

i=1
for( i in 1:n.quantiles) {
  temp[] = 0
  temp[quantiles.tm == i] = weights.tm[quantiles.tm == i]
  data.fa.tw$weight[] = NA
  data.fa.tw$weight[index.tw,] = temp
  models_tw[[ paste(prefix,'Q',i,sep='') ]] = bt.run(data.fa.tw, silent = T)

#*****************************************************************
# Create Report
#****************************************************************** 					
plotbt.custom.report.part1(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)

plotbt.strategy.sidebyside(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)

plotbt.custom.report.part1(quantiles$last.e_s)




#################################
#?D?????v???ƪ??צ?36?Ӥ몺?Ѳ??F
#################################
subset.36m<-index(data.tw.sample)[1:36]
subset.36m
data.tw.sample[subset.36m,1:10]
rm.index = which(sapply(tickers.tw, function(x) sum(!is.na(data.tw.sample[subset.36m,x])) ==36 ))
names(rm.index)
insample.y<-data.tw.sample[subset.36m, rm.index]
insample.x<-ff.tw.sample[subset.36m,]

#to.monthly(data.tw.sample)
#head(data.tw.sample)
#head(data.tw.sample,5)
#myxts<-xts(rnorm(31),as.Date("2008-12-31")+(0:30)*31)
#date.str<-as.Date(as.character(data.tw[,1]))
#format(date.str, "%Y-%m")
#test<-aggregate(data.tw.xts, format(index(data.tw.xts), "%Y-%m"))
#test<-data.tw.sample[,1]
#head(test)
#subset.36m<-index(test)[1:36]
#test[subset.36m]
#test[.indexmon(test)==0] # January for all years (note zero-based indexing!)
#format(as.Date("2000-01-31")+1:36)
#as.POSIXct(format(as.Date("2000-01-01")+1:10))

#########################
# ?p?ⶮ??Sharpe ratio
########################
# bnh = buy and hold portfolio
#portfolio.performance.df<-data.frame()
bnh.tw=read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/buy_and_hold.csv", header=TRUE)
rownames(bnh.tw)=bnh.tw[,1]
bnh.xts<-as.xts(bnh.tw[,-1])
bnh.sr<-SharpeRatio(bnh.xts, Rf = 0, p = 0.95, FUN = "StdDev")
bnh.sr
# Annualized Sharpe ratio
bnh.sr1<-SharpeRatio.annualized(bnh.xts, Rf = 0, scale=360, geometric=FALSE)
bnh.sr1  
# 10 day moving average  
d10.ma=read.csv("D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/10d_ma.csv", header=TRUE)
rownames(d10.ma)=d10.ma[,1]
d10.ma.xts<-as.xts(d10.ma[,-1])
d10.ma.sr<-SharpeRatio(d10.ma.xts, Rf = 0, p = 0.95, FUN = "StdDev")
d10.ma.sr  
  
# Annualized Sharpe ratio
d10.ma.sr1<-SharpeRatio.annualized(d10.ma.xts, Rf = 0, scale=360, geometric=FALSE)
d10.ma.sr1  
performance.port<-rbind(bnh.sr, bnh.sr1,d10.ma.sr, d10.ma.sr1) 
write.csv(performance.port, file="D:/?Ȭw?j?ǺӤh?Z???ɽפ?/???ӭ???/????/sharpe.csv")