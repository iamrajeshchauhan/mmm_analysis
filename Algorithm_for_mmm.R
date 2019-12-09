#import the library

library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)

#Import the data 

raw_data<-read.csv("algo_case.csv",skip = 1)
View(raw_data)

#clean the data

summary(raw_data)
raw<- subset( raw_data, select = -c(Sales, FB, Google.Search, 
                                    Video.Display.1,Net.Sales) ) 
View(raw)
raw[is.na(raw)] <- 0
View(raw)

#understand the model by Spend of the platform

Spend_model <- lm(ALL ~ FB.Amazon +FB.Website+ FB.Nykaa + FB.Post.Engagement
            +Google.Amazon+ Google.Website+Video.Display+Amazon_Marketing.Services, data = raw)
summary(Spend_model)

#understand the model by Impression of the platform

impresson_model<- lm(ALL ~ FB.1 +Google.Search.1+Google.Website.1+
                       Video.Display.2+FB.Nykaa.1+FB.Website.1+
                       FB.Post.Engagement.1+Amazon_Marketing.Services.1
                     , data=raw)

summary(impresson_model)



#understand the model by Clicks of the platform

click_model<- lm(ALL ~ FB.2+Google.Search.2+Google.Website.2+Video.Display.3
                 +FB.Nykaa.2+FB.Website.2+Amazon_Marketing.Services.2 , data=raw)

summary(click_model)

#ROI Calculation model

Ufun<-function(x, Spend, Return) {
  predictedReturn = x[2] + (x[1] - x[2])*((Spend^x[3])/(x[4] + (Spend^x[3])))
  errorSq = (predictedReturn - Return)^2
  sumSqError = sum(errorSq)
  return(sumSqError)
}

#Add all the market in the single column to find the total spending 
all_market_spend<- (raw$FB.Amazon +raw$FB.Website+raw$FB.Nykaa+ raw$FB.Post.Engagement
              +raw$Google.Amazon+ raw$Google.Website+raw$Video.Display)


View(all_market_spend)

startValVec = c(25000,100,1.5,100000)
minValVec = c(0,0,1.01,0)
maxValVec = c(500000, 500000, 2, 10000000)

return<- raw$Net.Sales.1


optim.parms<-nlminb(objective=Ufun,start=startValVec,
                    lower=minValVec,
                    upper=maxValVec,
                    control=list(iter.max=100000,eval.max=2000),
                    Spend = all_market_spend,
                    Return = return)

View(optim.parms)


# optimisation model building 
a = optim.parms$par[1]
b = optim.parms$par[2]
c = optim.parms$par[3]
d = optim.parms$par[4]

curveDFx = seq(from=0, to=max(all_market_spend)*2, length.out=10000)
curveDFy = b+(a-b)*((curveDFx^c)/(d+(curveDFx^c)))
curveDF = data.frame(Spend = all_market_spend, Return = return)

maxX = 1.05*max(curveDFx, max(all_market_spend))
maxY = 1.05*max(curveDFy, max(return))

myPlotDataDF = data.frame(Return = return, Spend = all_market_spend)
optimLineDF = data.frame(Spend = all_market_spend, Return = return)

scatterPlotPlusFit <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black", shape = 16) +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  geom_line(data = optimLineDF, aes(x = Spend, y = Return, color = "darkgreen"))  +
  scale_color_manual(labels = "Optimized model for Digital Marketing ",values=c('darkgreen')) +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
  scale_x_continuous(labels = dollar_format( prefix=" ",suffix = " INR")) +
  scale_y_continuous(labels = comma) 
  #ggtitle(paste(channelName, "Data & Model Fit", sep = " "))


scatterPlotPlusFit



# Amazon model building

sales_amazon<-raw$Amazon_Marketplace_Sales
amazon_spend<-raw$Amazon_Marketing.Services

optim.parms<-nlminb(objective=Ufun,start=startValVec,
                    lower=minValVec,
                    upper=maxValVec,
                    control=list(iter.max=100000,eval.max=2000),
                    Spend = amazon_spend,
                    Return = sales_amazon)

View(optim.parms)


# optimisation model building 
a = optim.parms$par[1]
b = optim.parms$par[2]
c = optim.parms$par[3]
d = optim.parms$par[4]

curveDFx = seq(from=0, to=max(amazon_spend)*2, length.out=10000)
curveDFy = b+(a-b)*((curveDFx^c)/(d+(curveDFx^c)))
curveDF = data.frame(Spend = amazon_spend, Return = sales_amazon)

maxX = 1.05*max(curveDFx, max(amazon_spend))
maxY = 1.05*max(curveDFy, max(sales_amazon))

myPlotDataDF = data.frame(Return = sales_amazon, Spend = amazon_spend)
optimLineDF = data.frame(Spend = amazon_spend, Return = sales_amazon)

scatterPlotPlusFit <- ggplot(myPlotDataDF, aes(x = Spend, y = Return)) +
  geom_point(color="black", shape = 16) +
  theme(panel.background = element_rect(fill = 'grey85'),
        panel.grid.major = element_line(colour = "white")) +
  geom_line(data = optimLineDF, aes(x = Spend, y = Return, color = "darkgreen"))  +
  scale_color_manual(labels = "Optimized model for Amazon",values=c('darkgreen')) +
  theme(legend.title=element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
  scale_x_continuous(labels = dollar_format( prefix=" ",suffix = " INR")) +
  scale_y_continuous(labels = comma) 
#ggtitle(paste(channelName, "Data & Model Fit", sep = " "))


scatterPlotPlusFit




