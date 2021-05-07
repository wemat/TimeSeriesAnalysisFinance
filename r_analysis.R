library(ggplot2)
library(tidyr)
library(quantmod)
library(PerformanceAnalytics)
library(gridExtra)
library(lmtest)
library(tseries)

setwd("~/Documents/GitHub/TimeSeriesAnalysisFinance")

df_smi <- NULL
tickers_index <- c("^SSMI")

for (Ticker in tickers_index){
  df_smi <- cbind(df_smi,
                getSymbols.yahoo(Ticker, from="2000-01-01", periodicity = "monthly",
                                 auto.assign=FALSE)[,6])
}

df_smi <- data.frame(df_smi)
df_smi$Value <- df_smi$SSMI.Adjusted
df_smi$SSMI.Adjusted <- NULL
df_smi$INDICATOR <-'SMI'
df_smi$LOCATION <- 'CHE'
df_smi <- cbind(year_month = rownames(df_smi), df_smi)
rownames(df_smi) <- 1:nrow(df_smi)
df_smi$year_month <-as.Date(paste(df_smi$year_month,"-01",sep=""))


df_m2 <- read.csv('m2.csv', sep = ';')
df_int <- read.csv('interest_saron.csv', sep = ';')

df_m2$year_month <-as.Date(paste(df_m2$year_month,"-01",sep=""))
df_m2$LOCATION <- 'CHE'

df_int$year_month <-as.Date(paste(df_int$year_month,"-01",sep=""))
df_int$LOCATION <- 'CHE'


df <-rbind(df_m2, df_int, df_smi)


p1<-ggplot(df_m2[df_m2$INDICATOR=='M2_CHANGES',], aes(x = year_month, y = Value)) + 
    geom_line()+
    ggtitle('Money Supply (M2)', subtitle ='%-change from the corresponding month of the previous year')+
    labs(y='%')

p2<-ggplot(df_m2[df_m2$INDICATOR=='M2_ABSOLUTE',], aes(x = year_month, y = Value)) + 
    geom_line()+
    ggtitle('Money Supply (M2)', subtitle ='Time-series of M2 Money Supply in CHF milions')+
    labs(y='CHF milions')

p3<-ggplot(df_int, aes(x = year_month, y = Value)) + 
    geom_line()+
    ggtitle('Interest Rate (SARON)')+
    labs(y='%')

p4<-ggplot(df_smi, aes(x = year_month, y = Value)) + 
    geom_line()+
    ggtitle('SMI')+
    labs(y='points')

grid.arrange(p1, p2, p3, p4, nrow = 2)


df_wide <- spread(df,INDICATOR, Value)
df_wide <- subset( df_wide, select = -c(LOCATION, year_month))
chart.Correlation(df_wide, histogram=TRUE, pch=19)


