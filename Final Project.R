#Assignment3 - research proposal data pull script
#Please do not run this script as it pulls from a limited quota of api requests.

install.packages("RGoogleAnalytics")
library("RGoogleAnalytics")

require(RGoogleAnalytics)
client.id <- "941727436824-erco3n81jtiie2q3its107sel9crpkgt.apps.googleusercontent.com"
client.secret <- "3BQXJPZVI3fTgMcSBFz5YFOK"

#Once created, it will ask if web-cached version is the one you'd like to use, select 2 (No)
token <- Auth(client.id,client.secret)

# Save the token object for future sessions
# If this works without requiring additional authentication 
save(token,file="C:/Users/pmaitra/Documents/IDS575/token_file")

# Get the Sessions & Transactions for each Source/Medium sorted in 
# descending order by the Transactions

query.list <- Init(start.date = "2016-03-22",
                   end.date = "2017-03-22",
                   dimensions = "ga:week,ga:deviceCategory,ga:medium",
                   metrics = "ga:sessions,ga:searchSessions,ga:avgTimeOnPage,ga:bounceRate,ga:pageviewsPerSession,ga:goal1Completions,ga:avgPageLoadTime", 
                   max.results = 15000,
                   sort = "-ga:week",
                   table.id = "ga:10633480")
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
hist(ga.data$sessions)
