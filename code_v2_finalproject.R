library(psych)
library(plyr)
library(dplyr)
library(car)
library(lubridate)
library(zoo)
library(xts) 
library(forecast)
library(tseries)

load("hl_data.RData")
str(ga.data)
dim(ga.data)
summary(ga.data)
describe(ga.data)
ga.data$deviceCategory <- as.factor(ga.data$deviceCategory)
levels(ga.data$deviceCategory)
ga.data$medium  <- as.factor(ga.data$medium)
ga.data$week    <- as.numeric(ga.data$week)
gpSumCompletion <- ga.data %>% group_by(week,deviceCategory) %>% summarize(totalCompletions = sum(goal1Completions))
attach(ga.data)
ga.data$medium1<- ifelse (ga.data$medium %in% c("none","cpc","social","email","organic","referral","affiliate"),"1","0")
View(ga.data)

#library(car)
ga.data$medium<- ifelse(ga.data$medium == "none", 1, ifelse(ga.data$medium == "cpc", 2, ifelse(ga.data$medium == "social", 3,ifelse(ga.data$medium == "email", 4, ifelse(ga.data$medium == "organic", 5,
                                                                                                                                                                         ifelse(ga.data$medium == "referral", 6, ifelse(ga.data$medium == "affiliate", 7, 0)))))))
#7 factors for mediums, and 0 is treated as other in total of 8.

ga.data$medium<- ifelse(ga.data$medium == 1, "none", ifelse(ga.data$medium == 2,"cpc", ifelse(ga.data$medium == 3,"social",ifelse(ga.data$medium == 4,"email", ifelse(ga.data$medium == 5,"organic",
                                                                                                                                                                      ifelse(ga.data$medium == 6, "referral", ifelse(ga.data$medium == 7, "affiliate", "other")))))))
ga.data <- ga.data[-which(ga.data$medium=='other'),]
ga.data[which(ga.data$medium=="other"),]
gpSumCompletion <- ga.data %>% group_by(week,deviceCategory) %>% summarize(totalCompletions = sum(goal1Completions))

library(psych) 
t_devicecategory<-table(deviceCategory) 
t_devicecategorypl<-(prop.table(t_devicecategory))
barplot(t_devicecategorypl, col=c("red", "orange", "blue", "steelblue"), main="Distribution of Device Category", ylim=c(0,0.6))

boxplot(log(sessions))
quantile(log(sessions))
plot(density(log(sessions)))

boxplot(sqrt(searchSessions))
quantile(sqrt(searchSessions))
plot(density(sqrt(searchSessions)))

boxplot(sqrt(avgTimeOnPage))
quantile(sqrt(avgTimeOnPage))
plot(density(avgTimeOnPage))

boxplot((bounceRate))
quantile((bounceRate))
plot(density(sqrt(bounceRate)))

boxplot(sqrt(pageviewsPerSession))
quantile(sqrt(pageviewsPerSession))
plot(density(sqrt(pageviewsPerSession)))

boxplot(sqrt(goal1Completions))
quantile(sqrt(goal1Completions))
plot(density(sqrt(goal1Completions)))

boxplot(sqrt(avgPageLoadTime))
quantile(sqrt(avgPageLoadTime))
plot(density(sqrt(avgPageLoadTime)))

# y variable
qqnorm(goal1Completions)
#its not very linear because we have alot of 0s labeled
# for goals completions, how should we treat these?

#lets observe the proportion of the distribution exactly
plot(density(log(ga.data$goal1Completions)))
table(ga.data$goal1Completions)
#2435 are 0s. 

#bivariate analysis
#correlation matrix
cormat <- cor(ga.data[,c(4,5,6,7,8,9,10)])
cormat
#sessions searchSessions avgTimeOnPage bounceRate pageviewsPerSession goal1Completions avgPageLoadTime
#sessions             1.00000000     0.90648490    0.05052642 -0.1236713           0.1209018       0.65989760      0.45259157
#searchSessions       0.90648490     1.00000000    0.04930519 -0.2037067           0.1996231       0.85669476      0.41156715
#avgTimeOnPage        0.05052642     0.04930519    1.00000000 -0.1657569           0.1143627       0.04948028      0.05431036
#bounceRate          -0.12367128    -0.20370672   -0.16575694  1.0000000          -0.5342496      -0.23964640     -0.11621487
#pageviewsPerSession  0.12090183     0.19962307    0.11436267 -0.5342496           1.0000000       0.25878585      0.12369789
#goal1Completions     0.65989760     0.85669476    0.04948028 -0.2396464           0.2587859       1.00000000      0.32414161
#avgPageLoadTime      0.45259157     0.41156715    0.05431036 -0.1162149           0.1236979       0.32414161      1.00000000


#factor analysis
#discriminant analysis

plot((goal1Completions~sessions), col="steelblue", pch=20, cex=0.75) 
qqplot(x= sessions,y=goal1Completions, data=ga.data, ylab="goal1Completions", xlab="Sessions", main="Relationship between Sessions and Goal1Completions, by Device", facets=~deviceCategory)
abline(lm(goal1Completions~sessions), col="red", lwd=2)

#Time series analysis
desktop.data <- gpSumCompletion[gpSumCompletion$deviceCategory=="desktop",]
desktop.ts <- ts(desktop.data$totalCompletions, start=c(2016,03),end = c(2017,03), frequency=52)
plot(desktop.ts)
mobile.data <- gpSumCompletion[gpSumCompletion$deviceCategory=="mobile",]
mobile.ts <- ts(mobile.data$totalCompletions, start=c(2016,03),end = c(2017,03), frequency=52)
plot(mobile.ts)
tablet.data <- gpSumCompletion[gpSumCompletion$deviceCategory=="tablet",]
tablet.ts <- ts(tablet.data$totalCompletions, start=c(2016,03),end = c(2017,03), frequency=52)
plot(tablet.ts)
adf.test(desktop.ts)
adf.test(mobile.ts)
adf.test(tablet.ts)


temp <- cbind(desktop.ts,mobile.ts)
combined.ts <- cbind(temp,tablet.ts)
plot(combined.ts)

ts.plot(combined.ts, xlab = "Weeks", ylab = "Index Value", main = "Goal Completion by different Devices", col = c("orange", "darkgray", "blue"), lwd=2)
legend("topleft", c("Desktop","Mobile", "Tablet"), lty = 1, col = c("orange", "darkgray","blue"), cex=0.8)


#plot using overlays
library(dplyr)
plot(week, goal1Completions)

par(mfrow=c(2,2))
ga_week<-ga.data%>%select(week,goal1Completions)%>% group_by(week)%>%summarize(totalCompletions = sum(goal1Completions))
plot(ga_week)
lo<-loess(ga_week$totalCompletions~ga_week$week)
lines(predict(lo), col='red',lwd=2)

Ga_weektotal <- ts(ga_week$totalCompletions, start=c(2016,03),end = c(2017,03), frequency=53)
plot(Ga_weektotal, main="GA- weekly completions total")
lo<-loess(ga_week$totalCompletions~ga_week$week)
lines(predict(lo), col='red',lwd=2)
dev.off()
#ga_week<-group_by(ga.data, ga.data$week)
#summaryga_week<-summarise(ga_week, num.types=n(),counts=sum(count()))
#important ones, none,social, affliate, CPC, referral, organic,email
#put all other ones into a seperate category




# For Weeks total completions
snaive_total <- snaive(Ga_weektotal, h=12) 
head(snaive_bp$fitted) 
snaive_bp$fitted 
snaive_bp$residuals
plot(snaive_total, main = "Predictions for Total goal completions via Desktop", col = "orange")  


axis(side=1,at=seq(from=2010, by = .25,length.out = length(time)))

# For Desktop completions
snaive_desktop <- snaive(desktop.ts, h=12) 
head(snaive_desktop$fitted) 
snaive_desktop$fitted 
snaive_desktop$residuals
plot(snaive_desktop, main = "Predictions for goal completions via Desktop", col = "orange") 


# For Mobile completions
snaive_mobile <- snaive(mobile.ts, h=12) 
head(snaive_mobile$fitted) 
snaive_mobile$fitted 
snaive_mobile$residuals
plot(snaive_mobile , main = "Predictions for goal completions via Mobile", col = "Red") 


# For Weeks total completions
snaive_tablet <- snaive(tablet.ts, h=12) 
head(snaive_tablet$fitted) 
snaive_tablet$fitted 
snaive_tablet$residuals
plot(snaive_tablet, main = "Predictions for goal completions via Tablet", col = "orange")  



ga.data$medium  <- as.factor(ga.data$medium)
levels(ga.data$medium)

#rid of the () in the none here 
#use the 
str(ga.data$medium)
ga.data$medium <- (gsub("\\(", "", ga.data$medium))
ga.data$medium <- (gsub("\\)", "", ga.data$medium))
ga.data$medium  <- as.factor(ga.data$medium)
levels(ga.data$medium)
table(ga.data$medium)


#factor analysis#
library(car)
library(cluster) # cluster functions 
library(fpc) # for PAM algorithm and automatic k detection 
library(NbClust) # determine number of clusters 
library(factoextra) # cluster plot 
library(psych)

#only use the numerical
myvars<-names(ga.data)%in% c("deviceCategory","medium","week","medium1","goal1Completions")
ga.data_s<-ga.data[!myvars]

#now we can rescale this data here
ga.data_s <- data.frame(scale(ga.data_s))

fa.parallel(ga.data_s, fa="both", n.iter=100)
fa.parallel(ga.data_s, fa="both", n.iter=100, show.legend=F)

#we can try both factors of 2 and factors of 3.
ga.data_sfa1 <- factanal(ga.data_s, 2) 
ga.data_sfa1$loadings

#anything higher than 0.33 correlation coefficient.
#from factor1, this group consists of sessions, searchsessions and avgpageloadtime
#factor 2 is a group just bouncerate.

#if using factors of 3.
ga.data_sfa1 <- factanal(ga.data_s, 3) 
ga.data_sfa1$loadings

#from factor1, this group consists of sessions, searchsessions and avgpageloadtime
#factor two is bouncerate
#not much here.

fit<-factanal(x=ga.data_s, factors=2,scores=c("regression"), rotation="none")
head(fit$scores)
load<-fit$loadings[,1:2]
plot(load)
text(load, labels=names(ga.data_s))
fit$scores

#using our factors for the OLS model.
ga.data_sfa1 <- factanal(ga.data_s, 3, scores="regression") 
ga.data_sfa1

ga.data_combine<-cbind(ga.data, ga.data_sfa1$scores)
attach(ga.data_combine)

#now lets use the factors to just to do regression modeling here #

mod1<-lm(goal1Completions~Factor1+Factor2+Factor3)
summary(mod1)
#r squareadjusted of 0.7928


#searchsessions and goalcompletions - 0.86
#sessions and goalcompletions - 0.66
#probably cant have both sessions and searchsessions - multicollinearity
#averageloadtime and sessions also have high orrelation
#perform multivariable regression 
# checking assumptions 
#cleaning dataset for the medium variable #
# Performing Time Series #
#PCA#
#FACTOR ANALYSIS#
#perform three seperate line chart where mobile, device, desktop 
#from week 1 to week 53, with goal1completions 

###############################################################################################################






