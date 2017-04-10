install.packages("RGoogleAnalytics")
library("RGoogleAnalytics")
require(RGoogleAnalytics)
client.id <- "941727436824-o2pml6tv8djlh7uv2dotevlo58btek3c.apps.googleusercontent.com"
client.secret <- "NItvBGu--EIeaL9X76-1H7CV"

#Once created, it will ask if web-cached version is the one you'd like to use, select 2 (No)
token <- Auth(client.id,client.secret)

# Save the token object for future sessions
# If this works without requiring additional authentication 
save(token,file="D:/UIC Study/IDS 575 Business Analytic Statistics")

# Get the Sessions & Transactions for each Source/Medium sorted in 
# descending order by the Transactions

query.list <- Init(start.date = "2017-03-15",
                   end.date = "2017-03-16",
                   dimensions = "ga:week,ga:deviceCategory,ga:medium",
                   metrics = "ga:sessions,ga:avgTimeOnPage,ga:bounceRate,ga:pageviewsPerSessions, ga:goalCompletionsAll, ga:avgPageLoadTime, ga:socialInteractions",
                   max.results = 15000,
                   sort = "-ga:date",
                   table.id = "ga:9097522")
?query.list
# Create the Query Builder object so that the query parameters are validated
ga.query <- QueryBuilder(query.list)

# Extract the data & store it in a data-frame
ga.data <- GetReportData(ga.query, token)

dimnames(ga.data)
dim(ga.data)
library(dplyr)
glimpse(ga.data)
head(ga.data)
View(ga.data)
hist(ga.data$hour)
write.csv(ga.data, "mydata.csv")


#Error in ParseDataFeedJSON(GA.Data) : 
#  code : 403 Reason : User does not have sufficient permissions for this profile.
