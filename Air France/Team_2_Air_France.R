#################################################################################
## HULT MSBA | Data Science: R
##
## Created by:
## Arthur Bedel
## Carolina Lemos
## Joslyn Li
## Stefania Rocha
## Valentin Voelckel
##
## Date: 12/09/2020
#################################################################################
## Importing packages & dataset
#################################################################################

# Installing packages
# install.packages("plotly")
# install.packages("mlbench")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("ngram")
# install.packages("dyplyr")
# install.packages("ggalt")
# install.packages("reshape2")


# Calling packages
library(readxl)
library(plotly)
library(ggplot2)
library(mlbench)
library(rpart)
library(rpart.plot)
library(ngram)
library(dplyr)
library(ggalt)
library(reshape2)
library(tidyr)


# Importing database
airfrance <- read_excel("Desktop/R/Air France Case Spreadsheet Supplement.xls", 
                        sheet = 2)

# Importing kayak database 
kayak_airfrance <- read_excel("Desktop/R/Air France Case Spreadsheet Supplement.xls",
                              sheet = "Kayak")

#################################################################################
## Data Cleaning & Massaging
#################################################################################

# Checking structure of our dataframe
str(airfrance)

# Update NAs
airfrance[airfrance==""] <- NA
airfrance[airfrance=="N/A"] <- NA

# Check for NAs
sapply(airfrance, function(x) sum(is.na(x)))

# Dropping observations with NA
clean_airfrance <- na.omit(airfrance)

# Publisher name to factor (US vs Global)
clean_airfrance$`Publisher Location` <- clean_airfrance$`Publisher Name`
clean_airfrance$`Publisher Location` <- gsub("..*US", 1, clean_airfrance$`Publisher Location`)

# Keyboard ID to num
clean_airfrance$`Keyword ID` <- as.numeric(clean_airfrance$`Keyword ID`)

# Match Type to factor
clean_airfrance$`Match Type` <- factor(clean_airfrance$`Match Type`, 
                                          ordered = T,
                                          levels = c("Broad", "Standard", 
                                                     "Exact", "Advanced"))

# Status to factor
clean_airfrance$`Status` <- factor(clean_airfrance$`Status`, 
                                       ordered = T,
                                       levels = c("Unavailable", "Deactivated", 
                                                  "Paused", "Sent", "Live"))

# Keyword type to short-tail (1 space or less), mid-tail (2 to 3 spaces), long-tail (4+ spaces)

word_count <- c()
i = 1
while(i <= nrow(clean_airfrance)){
  word_count <- sapply(strsplit(clean_airfrance$`Keyword`[i], " "), length)
  if (word_count <= 2){
    clean_airfrance$`Keyword Type`[i] <- "short-tail"
  } else if (word_count == 3 | word_count == 4) {
    clean_airfrance$`Keyword Type`[i] <- "mid-tail"
  } else if (word_count >= 5){
    clean_airfrance$`Keyword Type`[i] <- "long-tail"
  } #--> closing if statement
  i <- i + 1
} #--> closing while loop

clean_airfrance$`Keyword Type` <- factor(clean_airfrance$`Keyword Type`, ordered = T, 
                                          levels = c("short-tail", "mid-tail", "long-tail"))

# Creating Net Revenue column 
clean_airfrance$net_revenue <- as.numeric(clean_airfrance$Amount - 
                                            clean_airfrance$`Total Cost`)
# Creating ROA column
clean_airfrance$ROA <- as.numeric(clean_airfrance$net_revenue / 
                                    clean_airfrance$`Total Cost`)

# Creating Conversion rate column 
clean_airfrance$conversion_rate <- as.numeric(clean_airfrance$`Trans. Conv. %`/ 
                                                clean_airfrance$`Avg. Cost per Click`)

# Removing values from variable Amount including zero values
dfchannel <- clean_airfrance[!clean_airfrance$Amount == 0, ]
dfchannel$Average_Revenue_Booking <- dfchannel$Amount / dfchannel$`Total Volume of Bookings`

# Updating kayak info NAs
kayak_airfrance[kayak_airfrance==""] <- NA
kayak_airfrance[kayak_airfrance=="N/A"] <- NA

# Check kayak info for NAs
sapply(kayak_airfrance, function(x) sum(is.na(x)))

# Dropping kayak info observations with NA
clean_kayak_airfrance <- na.omit(kayak_airfrance)

# Renaming each variable with correct name
colnames(clean_kayak_airfrance) <- c("Search_Engine","Clicks","Media_Cost", 
                                     "Total_Bookings", "Avg_Ticket", 
                                     "Total_Revenue","Net_Revenue")

# Subseting to have only the rows with the values 
clean_kayak_airfrance <- clean_kayak_airfrance[ 2,]

# Clicks to numeric
clean_kayak_airfrance$Clicks <- as.numeric(clean_kayak_airfrance$Clicks)

# Media_Cost to numeric
clean_kayak_airfrance$Media_Cost <- as.numeric(clean_kayak_airfrance$Media_Cost)

# Total_Bookings to numeric
clean_kayak_airfrance$Total_Bookings <- as.numeric(clean_kayak_airfrance$Total_Bookings)

# Avg_Ticket to numeric
clean_kayak_airfrance$Avg_Ticket <- as.numeric(clean_kayak_airfrance$Avg_Ticket)

# Total_Revenue to numeric
clean_kayak_airfrance$Total_Revenue <- as.numeric(clean_kayak_airfrance$Total_Revenue)

# Net_Revenue to numeric
clean_kayak_airfrance$Net_Revenue <- as.numeric(clean_kayak_airfrance$Net_Revenue)

######Making calculations for kayak ################# 

# Calculating the average cpc for Kayak 
avg_cpc_kayak  <- clean_kayak_airfrance$Media_Cost/clean_kayak_airfrance$Clicks
# Calculating the average ROA for Kayak 
avg_ROA_kayak  <- clean_kayak_airfrance$Net_Revenue/clean_kayak_airfrance$Media_Cost
# Calculating the conversion rate price ratio for Kayak 
avg_conversion_rate_kayak  <- ((clean_kayak_airfrance$Total_Bookings/clean_kayak_airfrance$Clicks)/avg_cpc_kayak)
# Calculating the average Revenue boooking for Kayak 
# transformed result into percentage 
avg_Revenue_booking_kayak <- clean_kayak_airfrance$Net_Revenue/clean_kayak_airfrance$Total_Bookings 

#################################################################################
## KPIs
#################################################################################
### Looking to see if our KPIs are the same for each measure of success

##############################     Net Revenue     ##############################
# Highest in net_revenue
highest_net_revenue <- head(sort(clean_airfrance$net_revenue, 
                                 decreasing=TRUE), n=10)

# Sorting the Data by Net Revenue in the clean Data frame
Sorted_net_revenue <- clean_airfrance[order(- clean_airfrance$net_revenue),]

# Selecting the top 10 values 
Top_10_net_revenue <- head(Sorted_net_revenue,10) 

# Displaying the value in addition to the campaign and Publisher Name to make the results simpler to interpret
Top_10_net_revenue[ , c(2, 6, 25)]

##############################         ROA         ##############################

# Highest ROA
highest_roa <- head(sort(clean_airfrance$ROA, 
                         decreasing=TRUE), n=10)

# Sorting the Data by ROA in the clean Data frame
Sorted_roa <- clean_airfrance[order(- clean_airfrance$ROA),]

# Selecting the top 10 values 
Top_10_roa <- head(Sorted_roa,10) 

# Displaying the value in addition to the campaign and Publisher Name to make the results simpler to interpret
Top_10_roa[ , c(2, 6, 26)]


###########################      Conversion Rate      ###########################

# Highest Conversion Rate
highest_conversion <- head(sort(clean_airfrance$conversion_rate, 
                                decreasing=TRUE), n = 10)


# Sorting the Data by Conversion Rate in the clean Data frame
Sorted_conversion_rate_to_cost <- clean_airfrance[order(- clean_airfrance$conversion_rate),]

# Selecting the top 10 values 
Top_10_conversion_rate_to_cost <- head(Sorted_conversion_rate_to_cost,10) 

# Displaying the value in addition to the campaign and Publisher Name to make the results simpler to interpret
Top_10_conversion_rate_to_cost [ , c(2, 6, 27)]

#################################################################################
## PUBLISHER ANALYSIS
#################################################################################

# Creating Pivot table for averages
dfchannel_pivot <- dfchannel %>% group_by(`Publisher Name`) %>% summarize(
  avg_cpc = mean(`Avg. Cost per Click`),
  avg_ROA = mean(ROA),
  avg_conversion_rate = mean(conversion_rate),
  avg_Revenue_booking = mean(Average_Revenue_Booking ),
)
dfchannel_pivot

# adding kayak    
kayak.data <- c("Kayak", "1.26","64.513","5.83","1106.38")
dfchannelNew <- rbind(dfchannel_pivot, kayak.data)
dfchannelNew

# creating object for unique channels
channelUnique <- unique(dfchannelNew$`Publisher Name`)

#vector for US based or not
dfchannelNew$US <- c()
US <- factor(dfchannelNew$`Publisher Name`,
                levels = c("Google - Global",
                           "Google - US",
                           "MSN - Global",
                           "Overture - Global",
                           "Overture - US",
                           "Yahoo - US",
                           "Kayak"),
                labels = c(0,1,0,0,1,1,0))

dfchannelNew$US <- US
dfchannelNew$US


dfchannelNewUS <- rbind(dfchannelNew, dfchannelNew$US)
View(dfchannelNewUS)

# Pivot table for KPI overview complete 

# Visualizing and insight creation

# 1 average ROA
pROA <- ggplot() + geom_bar(aes(y= avg_ROA, x=`Publisher Name`, fill= channelUnique), data =dfchannelNew,
                            stat="identity")
pROA
# Overture-Global

# 2 average cost per click
pCPC <- ggplot() + geom_bar(aes(y= avg_cpc , x=`Publisher Name`, fill= channelUnique), data =dfchannelNew,
                            stat="identity")
pCPC 
# Google - US 
# Lowest overture - Global

# 3 Conversion rate price ratio
pConversion_rate<- ggplot() + geom_bar(aes(y= avg_conversion_rate , x=`Publisher Name`, fill= channelUnique), data =dfchannelNew,
                                                   stat="identity")
pConversion_rate
# Overture - Global

# 4 booking ratio
pRevBook <- ggplot() + geom_bar(aes(y= avg_Revenue_booking , x=`Publisher Name`, fill= channelUnique), data =dfchannelNew,
                                stat="identity")
pRevBook  
# Google - US 

# creating ranking for best channels ######how ranking was calculated ???????
x <- c('Kayak', 'MSN-Global', 'MSN - US','Yahoo-US', 'Google-Global', 'Overture-Global', 'Google-US', 'Overture-US')
y <- c(4,1,0,5,1,9,4,2) #points for first 3 places given to respective channel from comparison per category

# bar chart for comparison for success
data <- data.frame(x,y)
data$x <- factor(data$x, levels = data[["x"]])

pRanking <- plot_ly(data, x= ~x, y= ~y, type = "bar", name = "Comparison Channels", color = I("blue"), alpha = 0.5) %>%
  layout(title = "Ranking Channels",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
pRanking

# conclusion: Overture-Global is overall best channel in respective to 
# defined KPI's, winning in 3 categories (highest avg ROA, lowest cpc & Conversion_rate)

################################################################     

## Regression USpub
UsPub <- factor(dfchannel$`Publisher Name`,
                          levels = c("Google - Global",
                                     "Google - US",
                                     "MSN - Global",
                                     "Overture - Global",
                                     "Overture - US",
                                     "Yahoo - US"),
                          labels = c(0,1,0,0,1,1))

dfchannel$UsPub <- UsPub
dfchannel$UsPub

# logistic regression

# Build sample
index_dfchannel <- sample(1:nrow(dfchannel),
                          size = 0.8*nrow(dfchannel))

# Build training dataframe 
train_dfchannel <- dfchannel[index_dfchannel,]
# Build testing dataframe
test_dfchannel <- dfchannel[-index_dfchannel,]

# Test if there is a difference of result in SEM between US and non US
logitdfchannel <- glm(UsPub ~ ROA+conversion_rate+Average_Revenue_Booking,
                      data = train_dfchannel, family = "binomial")

summary(logitdfchannel)

##There is no significant different between the USA and Non-USA

#################################################################################
## IMPROVING CAMPAIGNS & KEYWORD ANALYSIS
#################################################################################

#####################   Match Type Analysis per Publisher   #####################

# See frequencies and Observation categories for Publishers
table(clean_airfrance$`Publisher Name`)
table(clean_airfrance$`Match Type`)
match_types <- c("Broad", "Standard", "Exact", "Advanced") #--> Create vector with match types names
publishers <- c("Google - Global", "Google - US", "MSN - Global", 
                "Overture - Global", "Overture - US", "Yahoo - US ") #--> Create vector with match types names

#### BASED ON ROA
#Create empty vectors per Publisher
google_GL_ROA_avg <- c()
google_US_ROA_avg <- c()
msn_GL_ROA_avg <- c()
overture_GL_ROA_avg <- c()
overture_US_ROA_avg <- c()
yahoo_US_ROA_avg <- c()
i <- 1 #--> setting i to start at 1

#While loop to get average ROA per Publisher, per match type
while(i <= length(match_types)){
  google_GL_ROA_avg <- c(google_GL_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Google - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  google_US_ROA_avg <- c(google_US_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Google - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  msn_GL_ROA_avg <- c(msn_GL_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "MSN - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  overture_GL_ROA_avg <- c(overture_GL_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Overture - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  overture_US_ROA_avg <- c(overture_US_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Overture - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  yahoo_US_ROA_avg <- c(yahoo_US_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Yahoo - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  i <- i + 1
  
} #--> closing ROA while loop

# Combine average ROA per publisher in a matrix
ROA_per_publisher_per_match_type <- rbind(google_GL_ROA_avg, google_US_ROA_avg, msn_GL_ROA_avg, 
                           overture_GL_ROA_avg, overture_US_ROA_avg, 
                           yahoo_US_ROA_avg)

colnames(ROA_per_publisher_per_match_type) <- match_types #--> assign names to the rows

print(ROA_per_publisher_per_match_type)

#### BASED ON TOTAL REVENUE
#Create empty vectors per Publisher
google_GL_net_revenue_avg <- c()
google_US_net_revenue_avg <- c()
msn_GL_net_revenue_avg <- c()
overture_GL_net_revenue_avg <- c()
overture_US_net_revenue_avg <- c()
yahoo_US_net_revenue_avg <- c()
i <- 1 #--> setting i to start at 1

#While loop to get average net revenue per Publisher, per match type
while(i <= length(match_types)){
  google_GL_net_revenue_avg <- c(google_GL_net_revenue_avg, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Google - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  google_US_net_revenue_avg <- c(google_US_net_revenue_avg, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Google - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  msn_GL_net_revenue_avg <- c(msn_GL_net_revenue_avg, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "MSN - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  overture_GL_net_revenue_avg <- c(overture_GL_net_revenue_avg, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Overture - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  overture_US_net_revenue_avg <- c(overture_US_net_revenue_avg, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Overture - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  yahoo_US_net_revenue_avg <- c(yahoo_US_net_revenue_avg, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Yahoo - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  i <- i + 1
  
} #--> closing net revenue while loop

# Combine average ROA per publisher in a matrix
net_revenue_per_publisher_per_match_type <- rbind(google_GL_net_revenue_avg, google_US_net_revenue_avg, msn_GL_net_revenue_avg, 
                           overture_GL_net_revenue_avg, overture_US_net_revenue_avg, 
                           yahoo_US_net_revenue_avg)

colnames(net_revenue_per_publisher_per_match_type) <- match_types  #--> assign names to the rows

print(net_revenue_per_publisher_per_match_type)

#### BASED ON CONVERSION
#Create empty vectors per Publisher
google_GL_conversion_rate_avg <- c()
google_US_conversion_rate_avg <- c()
msn_GL_conversion_rate_avg <- c()
overture_GL_conversion_rate_avg <- c()
overture_US_conversion_rate_avg <- c()
yahoo_US_conversion_rate_avg <- c()
i <- 1 #--> setting i to start at 1

#While loop to get average conversion rate per Publisher, per match type
while(i <= length(match_types)){
  google_GL_conversion_rate_avg <- c(google_GL_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Google - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  google_US_conversion_rate_avg <- c(google_US_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Google - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  msn_GL_conversion_rate_avg <- c(msn_GL_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "MSN - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  overture_GL_conversion_rate_avg <- c(overture_GL_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Overture - Global" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  overture_US_conversion_rate_avg <- c(overture_US_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Overture - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  yahoo_US_conversion_rate_avg <- c(yahoo_US_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Yahoo - US" & 
            clean_airfrance$`Match Type` == match_types[i])]))
  
  i <- i + 1
  
} #--> closing conversion rate while loop

# Combine average ROA per publisher in a matrix
conversion_rate_per_publisher_per_match_type <- rbind(google_GL_conversion_rate_avg, google_US_conversion_rate_avg, msn_GL_conversion_rate_avg, 
                                   overture_GL_conversion_rate_avg, overture_US_conversion_rate_avg, 
                                   yahoo_US_conversion_rate_avg)

colnames(conversion_rate_per_publisher_per_match_type) <- match_types  #--> assign names to the rows

print(conversion_rate_per_publisher_per_match_type)

#####################   Match Type Analysis per Campaign    #####################

# See frequencies and Observation categories for Campaigns
table(clean_airfrance$Campaign)
campaigns <- c("Air France Brand & French Destinations",
               "Air France Branded",
               "Air France Global Campaign",
               "Business Class",
               "French Destinations",
               "General Terms",
               "Geo Targeted Boston", 
               "Geo Targeted Chicago",
               "Geo Targeted DC",
               "Geo Targeted Detroit",
               "Geo Targeted Houston",
               "Geo Targeted Los Angeles",
               "Geo Targeted Miami",
               "Geo Targeted New York",
               "Geo Targeted Philadelphia",
               "Geo Targeted San Francisco",
               "Geo Targeted Seattle",
               "Google_Yearlong",
               "Outside Western Europe",
               "Paris & France Terms",
               "Unassigned",
               "Western Europe Destinations") #--> creating campaigns vector

#### BASED ON ROA
#Create empty vectors per Publisher
Air_France_Brand_French_Destinations_ROA_avg <- c()                   
Air_France_Branded_ROA_avg <- c()             
Air_France_Global_Campaign_ROA_avg <- c() 
Business_Class_ROA_avg <- c()                    
French_Destinations_ROA_avg <- c()                          
General_Terms_ROA_avg <- c() 
Geo_Targeted_Boston_ROA_avg <- c()                   
Geo_Targeted_Chicago_ROA_avg <- c()                        
Geo_Targeted_DC_ROA_avg <- c() 
Geo_Targeted_Detroit_ROA_avg <- c()                   
Geo_Targeted_Houston_ROA_avg <- c()               
Geo_Targeted_Los_Angeles_ROA_avg <- c() 
Geo_Targeted_Miami_ROA_avg <- c()                  
Geo_Targeted_New_York_ROA_avg <- c()              
Geo_Targeted_Philadelphia_ROA_avg <- c() 
Geo_Targeted_San_Francisco_ROA_avg <- c()                   
Geo_Targeted_Seattle_ROA_avg <- c()                   
Google_Yearlong_ROA_avg <- c()
Outside_Western_Europe_ROA_avg <- c()                   
Paris_France_Terms_ROA_avg <- c()                             
Unassigned_ROA_avg <- c() 
Western_Europe_Destinations_ROA_avg <- c() 
i <- 1

#While loop to get average ROA per Campaign, per match type
while(i <= length(match_types)){
  Air_France_Brand_French_Destinations_ROA_avg <- 
    c(Air_France_Brand_French_Destinations_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Air France Brand & French Destinations" 
          & clean_airfrance$`Match Type` == match_types[i])]))              
  Air_France_Branded_ROA_avg <- 
    c( Air_France_Branded_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Air France Branded" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Air_France_Global_Campaign_ROA_avg <- 
    c( Air_France_Global_Campaign_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Air France Global Campaign" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Business_Class_ROA_avg <- 
    c( Business_Class_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Business Class" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  French_Destinations_ROA_avg <- 
    c( French_Destinations_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "French Destinations" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  General_Terms_ROA_avg <- 
    c( General_Terms_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "General Terms" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Boston_ROA_avg <- 
    c( Geo_Targeted_Boston_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted Boston" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Chicago_ROA_avg <- 
    c( Geo_Targeted_Chicago_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted Chicago" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_DC_ROA_avg <- 
    c( Geo_Targeted_DC_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted DC" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Detroit_ROA_avg <- 
    c( Geo_Targeted_Detroit_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted Detroit" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Houston_ROA_avg <- 
    c( Geo_Targeted_Houston_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted Houston" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Los_Angeles_ROA_avg <- 
    c( Geo_Targeted_Los_Angeles_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted Los Angeles" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Miami_ROA_avg <- 
    c( Geo_Targeted_Miami_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted Miami" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_New_York_ROA_avg <- 
    c( Geo_Targeted_New_York_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted New York" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Philadelphia_ROA_avg <- 
    c( Geo_Targeted_Philadelphia_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted Philadelphia" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_San_Francisco_ROA_avg <- 
    c( Geo_Targeted_San_Francisco_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted San Francisco" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Seattle_ROA_avg <- 
    c( Geo_Targeted_Seattle_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Geo Targeted Seattle" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Google_Yearlong_ROA_avg <- 
    c( Google_Yearlong_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Google_Yearlong" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Outside_Western_Europe_ROA_avg <- 
    c( Outside_Western_Europe_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Outside Western Europe" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Paris_France_Terms_ROA_avg <- 
    c( Paris_France_Terms_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Paris & France Terms" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Unassigned_ROA_avg <- 
    c( Unassigned_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Unassigned" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  Western_Europe_Destinations_ROA_avg <- 
    c( Western_Europe_Destinations_ROA_avg, mean(clean_airfrance$ROA[
    which(clean_airfrance$Campaign == "Western Europe Destinations" 
          & clean_airfrance$`Match Type` == match_types[i])]))
  i <- i + 1
} #--> closing ROA while loop

# Combine average ROA per campaign in a matrix
campaign_ROA_per_match_type <- rbind(Air_France_Brand_French_Destinations_ROA_avg, 
                                     Air_France_Branded_ROA_avg, 
                                     Air_France_Global_Campaign_ROA_avg, 
                                     Business_Class_ROA_avg, 
                                     French_Destinations_ROA_avg, 
                                     General_Terms_ROA_avg, 
                                     Geo_Targeted_Boston_ROA_avg, 
                                     Geo_Targeted_Chicago_ROA_avg, 
                                     Geo_Targeted_DC_ROA_avg, 
                                     Geo_Targeted_Detroit_ROA_avg, 
                                     Geo_Targeted_Houston_ROA_avg, 
                                     Geo_Targeted_Los_Angeles_ROA_avg, 
                                     Geo_Targeted_Miami_ROA_avg, 
                                     Geo_Targeted_New_York_ROA_avg, 
                                     Geo_Targeted_Philadelphia_ROA_avg, 
                                     Geo_Targeted_San_Francisco_ROA_avg, 
                                     Geo_Targeted_Seattle_ROA_avg, 
                                     Google_Yearlong_ROA_avg, 
                                     Outside_Western_Europe_ROA_avg, 
                                     Paris_France_Terms_ROA_avg, 
                                     Unassigned_ROA_avg, 
                                     Western_Europe_Destinations_ROA_avg)

colnames(campaign_ROA_per_match_type) <- match_types  #--> assign names to the rows
print(campaign_ROA_per_match_type)

#### BASED ON Net Revenue
#Create empty vectors per Publisher
Air_France_Brand_French_Destinations_net_revenue_avg <- c()                   
Air_France_Branded_net_revenue_avg <- c()             
Air_France_Global_Campaign_net_revenue_avg <- c() 
Business_Class_net_revenue_avg <- c()                    
French_Destinations_net_revenue_avg <- c()                          
General_Terms_net_revenue_avg <- c() 
Geo_Targeted_Boston_net_revenue_avg <- c()                   
Geo_Targeted_Chicago_net_revenue_avg <- c()                        
Geo_Targeted_DC_net_revenue_avg <- c() 
Geo_Targeted_Detroit_net_revenue_avg <- c()                   
Geo_Targeted_Houston_net_revenue_avg <- c()               
Geo_Targeted_Los_Angeles_net_revenue_avg <- c() 
Geo_Targeted_Miami_net_revenue_avg <- c()                  
Geo_Targeted_New_York_net_revenue_avg <- c()              
Geo_Targeted_Philadelphia_net_revenue_avg <- c() 
Geo_Targeted_San_Francisco_net_revenue_avg <- c()                   
Geo_Targeted_Seattle_net_revenue_avg <- c()                   
Google_Yearlong_net_revenue_avg <- c()
Outside_Western_Europe_net_revenue_avg <- c()                   
Paris_France_Terms_net_revenue_avg <- c()                             
Unassigned_net_revenue_avg <- c() 
Western_Europe_Destinations_net_revenue_avg <- c() 
i <- 1

#While loop to get average Net Revenue per Campaign, per match type
while(i <= length(match_types)){
  Air_France_Brand_French_Destinations_net_revenue_avg <- 
    c(Air_France_Brand_French_Destinations_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Air France Brand & French Destinations" 
            & clean_airfrance$`Match Type` == match_types[i])]))              
  Air_France_Branded_net_revenue_avg <- 
    c( Air_France_Branded_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Air France Branded" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Air_France_Global_Campaign_net_revenue_avg <- 
    c( Air_France_Global_Campaign_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Air France Global Campaign" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Business_Class_net_revenue_avg <- 
    c( Business_Class_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Business Class" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  French_Destinations_net_revenue_avg <- 
    c( French_Destinations_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "French Destinations" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  General_Terms_net_revenue_avg <- 
    c( General_Terms_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "General Terms" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Boston_net_revenue_avg <- 
    c( Geo_Targeted_Boston_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Boston" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Chicago_net_revenue_avg <- 
    c( Geo_Targeted_Chicago_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Chicago" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_DC_net_revenue_avg <- 
    c( Geo_Targeted_DC_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted DC" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Detroit_net_revenue_avg <- 
    c( Geo_Targeted_Detroit_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Detroit" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Houston_net_revenue_avg <- 
    c( Geo_Targeted_Houston_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Houston" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Los_Angeles_net_revenue_avg <- 
    c( Geo_Targeted_Los_Angeles_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Los Angeles" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Miami_net_revenue_avg <- 
    c( Geo_Targeted_Miami_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Miami" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_New_York_net_revenue_avg <- 
    c( Geo_Targeted_New_York_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted New York" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Philadelphia_net_revenue_avg <- 
    c( Geo_Targeted_Philadelphia_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Philadelphia" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_San_Francisco_net_revenue_avg <- 
    c( Geo_Targeted_San_Francisco_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted San Francisco" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Seattle_net_revenue_avg <- 
    c( Geo_Targeted_Seattle_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Seattle" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Google_Yearlong_net_revenue_avg <- 
    c( Google_Yearlong_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Google_Yearlong" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Outside_Western_Europe_net_revenue_avg <- 
    c( Outside_Western_Europe_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Outside Western Europe" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Paris_France_Terms_net_revenue_avg <- 
    c( Paris_France_Terms_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Paris & France Terms" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Unassigned_net_revenue_avg <- 
    c( Unassigned_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Unassigned" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Western_Europe_Destinations_net_revenue_avg <- 
    c( Western_Europe_Destinations_net_revenue_avg, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Western Europe Destinations" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  i <- i + 1
} #--> closing net_revenue while loop

# Combine average net_revenue per campaign in a matrix
campaign_net_revenue_per_match_type <- rbind(Air_France_Brand_French_Destinations_net_revenue_avg, 
                                     Air_France_Branded_net_revenue_avg, 
                                     Air_France_Global_Campaign_net_revenue_avg, 
                                     Business_Class_net_revenue_avg, 
                                     French_Destinations_net_revenue_avg, 
                                     General_Terms_net_revenue_avg, 
                                     Geo_Targeted_Boston_net_revenue_avg, 
                                     Geo_Targeted_Chicago_net_revenue_avg, 
                                     Geo_Targeted_DC_net_revenue_avg, 
                                     Geo_Targeted_Detroit_net_revenue_avg, 
                                     Geo_Targeted_Houston_net_revenue_avg, 
                                     Geo_Targeted_Los_Angeles_net_revenue_avg, 
                                     Geo_Targeted_Miami_net_revenue_avg, 
                                     Geo_Targeted_New_York_net_revenue_avg, 
                                     Geo_Targeted_Philadelphia_net_revenue_avg, 
                                     Geo_Targeted_San_Francisco_net_revenue_avg, 
                                     Geo_Targeted_Seattle_net_revenue_avg, 
                                     Google_Yearlong_net_revenue_avg, 
                                     Outside_Western_Europe_net_revenue_avg, 
                                     Paris_France_Terms_net_revenue_avg, 
                                     Unassigned_net_revenue_avg, 
                                     Western_Europe_Destinations_net_revenue_avg)

colnames(campaign_net_revenue_per_match_type) <- match_types  #--> assign names to the rows
print(campaign_net_revenue_per_match_type)

#### BASED ON conversion_rate
#Create empty vectors per Publisher
Air_France_Brand_French_Destinations_conversion_rate_avg <- c()                   
Air_France_Branded_conversion_rate_avg <- c()             
Air_France_Global_Campaign_conversion_rate_avg <- c() 
Business_Class_conversion_rate_avg <- c()                    
French_Destinations_conversion_rate_avg <- c()                          
General_Terms_conversion_rate_avg <- c() 
Geo_Targeted_Boston_conversion_rate_avg <- c()                   
Geo_Targeted_Chicago_conversion_rate_avg <- c()                        
Geo_Targeted_DC_conversion_rate_avg <- c() 
Geo_Targeted_Detroit_conversion_rate_avg <- c()                   
Geo_Targeted_Houston_conversion_rate_avg <- c()               
Geo_Targeted_Los_Angeles_conversion_rate_avg <- c() 
Geo_Targeted_Miami_conversion_rate_avg <- c()                  
Geo_Targeted_New_York_conversion_rate_avg <- c()              
Geo_Targeted_Philadelphia_conversion_rate_avg <- c() 
Geo_Targeted_San_Francisco_conversion_rate_avg <- c()                   
Geo_Targeted_Seattle_conversion_rate_avg <- c()                   
Google_Yearlong_conversion_rate_avg <- c()
Outside_Western_Europe_conversion_rate_avg <- c()                   
Paris_France_Terms_conversion_rate_avg <- c()                             
Unassigned_conversion_rate_avg <- c() 
Western_Europe_Destinations_conversion_rate_avg <- c() 
i <- 1

#While loop to get average Conversion rate per Campaign, per match type
while(i <= length(match_types)){
  Air_France_Brand_French_Destinations_conversion_rate_avg <- 
    c(Air_France_Brand_French_Destinations_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Air France Brand & French Destinations" 
            & clean_airfrance$`Match Type` == match_types[i])]))              
  Air_France_Branded_conversion_rate_avg <- 
    c( Air_France_Branded_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Air France Branded" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Air_France_Global_Campaign_conversion_rate_avg <- 
    c( Air_France_Global_Campaign_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Air France Global Campaign" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Business_Class_conversion_rate_avg <- 
    c( Business_Class_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Business Class" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  French_Destinations_conversion_rate_avg <- 
    c( French_Destinations_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "French Destinations" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  General_Terms_conversion_rate_avg <- 
    c( General_Terms_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "General Terms" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Boston_conversion_rate_avg <- 
    c( Geo_Targeted_Boston_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Boston" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Chicago_conversion_rate_avg <- 
    c( Geo_Targeted_Chicago_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Chicago" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_DC_conversion_rate_avg <- 
    c( Geo_Targeted_DC_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted DC" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Detroit_conversion_rate_avg <- 
    c( Geo_Targeted_Detroit_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Detroit" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Houston_conversion_rate_avg <- 
    c( Geo_Targeted_Houston_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Houston" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Los_Angeles_conversion_rate_avg <- 
    c( Geo_Targeted_Los_Angeles_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Los Angeles" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Miami_conversion_rate_avg <- 
    c( Geo_Targeted_Miami_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Miami" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_New_York_conversion_rate_avg <- 
    c( Geo_Targeted_New_York_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted New York" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Philadelphia_conversion_rate_avg <- 
    c( Geo_Targeted_Philadelphia_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Philadelphia" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_San_Francisco_conversion_rate_avg <- 
    c( Geo_Targeted_San_Francisco_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted San Francisco" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Geo_Targeted_Seattle_conversion_rate_avg <- 
    c( Geo_Targeted_Seattle_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Seattle" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Google_Yearlong_conversion_rate_avg <- 
    c( Google_Yearlong_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Google_Yearlong" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Outside_Western_Europe_conversion_rate_avg <- 
    c( Outside_Western_Europe_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Outside Western Europe" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Paris_France_Terms_conversion_rate_avg <- 
    c( Paris_France_Terms_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Paris & France Terms" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Unassigned_conversion_rate_avg <- 
    c( Unassigned_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Unassigned" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  Western_Europe_Destinations_conversion_rate_avg <- 
    c( Western_Europe_Destinations_conversion_rate_avg, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Western Europe Destinations" 
            & clean_airfrance$`Match Type` == match_types[i])]))
  i <- i + 1
} #--> closing conversion_rate while loop

# Combine average conversion_rate per campaign in a matrix
campaign_conversion_rate_per_match_type <- rbind(Air_France_Brand_French_Destinations_conversion_rate_avg, 
                                             Air_France_Branded_conversion_rate_avg, 
                                             Air_France_Global_Campaign_conversion_rate_avg, 
                                             Business_Class_conversion_rate_avg, 
                                             French_Destinations_conversion_rate_avg, 
                                             General_Terms_conversion_rate_avg, 
                                             Geo_Targeted_Boston_conversion_rate_avg, 
                                             Geo_Targeted_Chicago_conversion_rate_avg, 
                                             Geo_Targeted_DC_conversion_rate_avg, 
                                             Geo_Targeted_Detroit_conversion_rate_avg, 
                                             Geo_Targeted_Houston_conversion_rate_avg, 
                                             Geo_Targeted_Los_Angeles_conversion_rate_avg, 
                                             Geo_Targeted_Miami_conversion_rate_avg, 
                                             Geo_Targeted_New_York_conversion_rate_avg, 
                                             Geo_Targeted_Philadelphia_conversion_rate_avg, 
                                             Geo_Targeted_San_Francisco_conversion_rate_avg, 
                                             Geo_Targeted_Seattle_conversion_rate_avg, 
                                             Google_Yearlong_conversion_rate_avg, 
                                             Outside_Western_Europe_conversion_rate_avg, 
                                             Paris_France_Terms_conversion_rate_avg, 
                                             Unassigned_conversion_rate_avg, 
                                             Western_Europe_Destinations_conversion_rate_avg)

colnames(campaign_conversion_rate_per_match_type) <- match_types  #--> assign names to the rows
print(campaign_conversion_rate_per_match_type)

#################################################################################

# See frequencies and Observation categories for keyword type
table(clean_airfrance$`Keyword Type`)
keyword_types <- c("short-tail", "mid-tail", "long-tail") #--> Create vector with keyword types names

#############           Keyword Type analysis per Campaign         #############

#### BASED ON ROA
#Create empty vectors per Campaign
Air_France_Brand_French_Destinations_ROA_avg_kt <- c()                   
Air_France_Branded_ROA_avg_kt <- c()             
Air_France_Global_Campaign_ROA_avg_kt <- c() 
Business_Class_ROA_avg_kt <- c()                    
French_Destinations_ROA_avg_kt <- c()                          
General_Terms_ROA_avg_kt <- c() 
Geo_Targeted_Boston_ROA_avg_kt <- c()                   
Geo_Targeted_Chicago_ROA_avg_kt <- c()                        
Geo_Targeted_DC_ROA_avg_kt <- c() 
Geo_Targeted_Detroit_ROA_avg_kt <- c()                   
Geo_Targeted_Houston_ROA_avg_kt <- c()               
Geo_Targeted_Los_Angeles_ROA_avg_kt <- c() 
Geo_Targeted_Miami_ROA_avg_kt <- c()                  
Geo_Targeted_New_York_ROA_avg_kt <- c()              
Geo_Targeted_Philadelphia_ROA_avg_kt <- c() 
Geo_Targeted_San_Francisco_ROA_avg_kt <- c()                   
Geo_Targeted_Seattle_ROA_avg_kt <- c()                   
Google_Yearlong_ROA_avg_kt <- c()
Outside_Western_Europe_ROA_avg_kt <- c()                   
Paris_France_Terms_ROA_avg_kt <- c()                             
Unassigned_ROA_avg_kt <- c() 
Western_Europe_Destinations_ROA_avg_kt <- c() 
i <- 1

#While loop to get average ROA per Campaign, per keyword type
while(i <= length(keyword_types)){
  Air_France_Brand_French_Destinations_ROA_avg_kt <- 
    c(Air_France_Brand_French_Destinations_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Air France Brand & French Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))              
  Air_France_Branded_ROA_avg_kt <- 
    c( Air_France_Branded_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Air France Branded" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Air_France_Global_Campaign_ROA_avg_kt <- 
    c( Air_France_Global_Campaign_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Air France Global Campaign" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Business_Class_ROA_avg_kt <- 
    c( Business_Class_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Business Class" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  French_Destinations_ROA_avg_kt <- 
    c( French_Destinations_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "French Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  General_Terms_ROA_avg_kt <- 
    c( General_Terms_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "General Terms" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Boston_ROA_avg_kt <- 
    c( Geo_Targeted_Boston_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted Boston" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Chicago_ROA_avg_kt <- 
    c( Geo_Targeted_Chicago_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted Chicago" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_DC_ROA_avg_kt <- 
    c( Geo_Targeted_DC_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted DC" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Detroit_ROA_avg_kt <- 
    c( Geo_Targeted_Detroit_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted Detroit" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Houston_ROA_avg_kt <- 
    c( Geo_Targeted_Houston_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted Houston" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Los_Angeles_ROA_avg_kt <- 
    c( Geo_Targeted_Los_Angeles_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted Los Angeles" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Miami_ROA_avg_kt <- 
    c( Geo_Targeted_Miami_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted Miami" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_New_York_ROA_avg_kt <- 
    c( Geo_Targeted_New_York_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted New York" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Philadelphia_ROA_avg_kt <- 
    c( Geo_Targeted_Philadelphia_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted Philadelphia" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_San_Francisco_ROA_avg_kt <- 
    c( Geo_Targeted_San_Francisco_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted San Francisco" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Seattle_ROA_avg_kt <- 
    c( Geo_Targeted_Seattle_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Geo Targeted Seattle" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Google_Yearlong_ROA_avg_kt <- 
    c( Google_Yearlong_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Google_Yearlong" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Outside_Western_Europe_ROA_avg_kt <- 
    c( Outside_Western_Europe_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Outside Western Europe" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Paris_France_Terms_ROA_avg_kt <- 
    c( Paris_France_Terms_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Paris & France Terms" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Unassigned_ROA_avg_kt <- 
    c( Unassigned_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Unassigned" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Western_Europe_Destinations_ROA_avg_kt <- 
    c( Western_Europe_Destinations_ROA_avg_kt, mean(clean_airfrance$ROA[
      which(clean_airfrance$Campaign == "Western Europe Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  i <- i + 1
} #--> closing ROA while loop

# Combine average ROA per campaign in a matrix
campaign_ROA_per_keyword_type <- rbind(Air_France_Brand_French_Destinations_ROA_avg_kt, 
                                     Air_France_Branded_ROA_avg_kt, 
                                     Air_France_Global_Campaign_ROA_avg_kt, 
                                     Business_Class_ROA_avg_kt, 
                                     French_Destinations_ROA_avg_kt, 
                                     General_Terms_ROA_avg_kt, 
                                     Geo_Targeted_Boston_ROA_avg_kt, 
                                     Geo_Targeted_Chicago_ROA_avg_kt, 
                                     Geo_Targeted_DC_ROA_avg_kt, 
                                     Geo_Targeted_Detroit_ROA_avg_kt, 
                                     Geo_Targeted_Houston_ROA_avg_kt, 
                                     Geo_Targeted_Los_Angeles_ROA_avg_kt, 
                                     Geo_Targeted_Miami_ROA_avg_kt, 
                                     Geo_Targeted_New_York_ROA_avg_kt, 
                                     Geo_Targeted_Philadelphia_ROA_avg_kt, 
                                     Geo_Targeted_San_Francisco_ROA_avg_kt, 
                                     Geo_Targeted_Seattle_ROA_avg_kt, 
                                     Google_Yearlong_ROA_avg_kt, 
                                     Outside_Western_Europe_ROA_avg_kt, 
                                     Paris_France_Terms_ROA_avg_kt, 
                                     Unassigned_ROA_avg_kt, 
                                     Western_Europe_Destinations_ROA_avg_kt)

colnames(campaign_ROA_per_keyword_type) <- keyword_types  #--> assign names to the rows
print(campaign_ROA_per_keyword_type)

#### BASED ON NET REVENUE
#Create empty vectors per Campaign
Air_France_Brand_French_Destinations_net_revenue_avg_kt <- c()                   
Air_France_Branded_net_revenue_avg_kt <- c()             
Air_France_Global_Campaign_net_revenue_avg_kt <- c() 
Business_Class_net_revenue_avg_kt <- c()                    
French_Destinations_net_revenue_avg_kt <- c()                          
General_Terms_net_revenue_avg_kt <- c() 
Geo_Targeted_Boston_net_revenue_avg_kt <- c()                   
Geo_Targeted_Chicago_net_revenue_avg_kt <- c()                        
Geo_Targeted_DC_net_revenue_avg_kt <- c() 
Geo_Targeted_Detroit_net_revenue_avg_kt <- c()                   
Geo_Targeted_Houston_net_revenue_avg_kt <- c()               
Geo_Targeted_Los_Angeles_net_revenue_avg_kt <- c() 
Geo_Targeted_Miami_net_revenue_avg_kt <- c()                  
Geo_Targeted_New_York_net_revenue_avg_kt <- c()              
Geo_Targeted_Philadelphia_net_revenue_avg_kt <- c() 
Geo_Targeted_San_Francisco_net_revenue_avg_kt <- c()                   
Geo_Targeted_Seattle_net_revenue_avg_kt <- c()                   
Google_Yearlong_net_revenue_avg_kt <- c()
Outside_Western_Europe_net_revenue_avg_kt <- c()                   
Paris_France_Terms_net_revenue_avg_kt <- c()                             
Unassigned_net_revenue_avg_kt <- c() 
Western_Europe_Destinations_net_revenue_avg_kt <- c() 
i <- 1

#While loop to get average Net Revenue per Campaign, per keyword type
while(i <= length(keyword_types)){
  Air_France_Brand_French_Destinations_net_revenue_avg_kt <- 
    c(Air_France_Brand_French_Destinations_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Air France Brand & French Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))              
  Air_France_Branded_net_revenue_avg_kt <- 
    c( Air_France_Branded_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Air France Branded" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Air_France_Global_Campaign_net_revenue_avg_kt <- 
    c( Air_France_Global_Campaign_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Air France Global Campaign" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Business_Class_net_revenue_avg_kt <- 
    c( Business_Class_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Business Class" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  French_Destinations_net_revenue_avg_kt <- 
    c( French_Destinations_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "French Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  General_Terms_net_revenue_avg_kt <- 
    c( General_Terms_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "General Terms" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Boston_net_revenue_avg_kt <- 
    c( Geo_Targeted_Boston_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Boston" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Chicago_net_revenue_avg_kt <- 
    c( Geo_Targeted_Chicago_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Chicago" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_DC_net_revenue_avg_kt <- 
    c( Geo_Targeted_DC_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted DC" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Detroit_net_revenue_avg_kt <- 
    c( Geo_Targeted_Detroit_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Detroit" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Houston_net_revenue_avg_kt <- 
    c( Geo_Targeted_Houston_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Houston" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Los_Angeles_net_revenue_avg_kt <- 
    c( Geo_Targeted_Los_Angeles_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Los Angeles" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Miami_net_revenue_avg_kt <- 
    c( Geo_Targeted_Miami_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Miami" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_New_York_net_revenue_avg_kt <- 
    c( Geo_Targeted_New_York_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted New York" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Philadelphia_net_revenue_avg_kt <- 
    c( Geo_Targeted_Philadelphia_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Philadelphia" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_San_Francisco_net_revenue_avg_kt <- 
    c( Geo_Targeted_San_Francisco_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted San Francisco" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Seattle_net_revenue_avg_kt <- 
    c( Geo_Targeted_Seattle_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Geo Targeted Seattle" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Google_Yearlong_net_revenue_avg_kt <- 
    c( Google_Yearlong_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Google_Yearlong" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Outside_Western_Europe_net_revenue_avg_kt <- 
    c( Outside_Western_Europe_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Outside Western Europe" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Paris_France_Terms_net_revenue_avg_kt <- 
    c( Paris_France_Terms_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Paris & France Terms" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Unassigned_net_revenue_avg_kt <- 
    c( Unassigned_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Unassigned" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Western_Europe_Destinations_net_revenue_avg_kt <- 
    c( Western_Europe_Destinations_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
      which(clean_airfrance$Campaign == "Western Europe Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  i <- i + 1
} #--> closing Net Revenue while loop

# Combine average Net Revenue per campaign in a matrix
campaign_net_revenue_per_keyword_type <- rbind(Air_France_Brand_French_Destinations_net_revenue_avg_kt, 
                                       Air_France_Branded_net_revenue_avg_kt, 
                                       Air_France_Global_Campaign_net_revenue_avg_kt, 
                                       Business_Class_net_revenue_avg_kt, 
                                       French_Destinations_net_revenue_avg_kt, 
                                       General_Terms_net_revenue_avg_kt, 
                                       Geo_Targeted_Boston_net_revenue_avg_kt, 
                                       Geo_Targeted_Chicago_net_revenue_avg_kt, 
                                       Geo_Targeted_DC_net_revenue_avg_kt, 
                                       Geo_Targeted_Detroit_net_revenue_avg_kt, 
                                       Geo_Targeted_Houston_net_revenue_avg_kt, 
                                       Geo_Targeted_Los_Angeles_net_revenue_avg_kt, 
                                       Geo_Targeted_Miami_net_revenue_avg_kt, 
                                       Geo_Targeted_New_York_net_revenue_avg_kt, 
                                       Geo_Targeted_Philadelphia_net_revenue_avg_kt, 
                                       Geo_Targeted_San_Francisco_net_revenue_avg_kt, 
                                       Geo_Targeted_Seattle_net_revenue_avg_kt, 
                                       Google_Yearlong_net_revenue_avg_kt, 
                                       Outside_Western_Europe_net_revenue_avg_kt, 
                                       Paris_France_Terms_net_revenue_avg_kt, 
                                       Unassigned_net_revenue_avg_kt, 
                                       Western_Europe_Destinations_net_revenue_avg_kt)

colnames(campaign_net_revenue_per_keyword_type) <- keyword_types  #--> assign names to the rows
print(campaign_net_revenue_per_keyword_type)

#### BASED ON CONVERSION RATE
#Create empty vectors per Campaign
Air_France_Brand_French_Destinations_conversion_rate_avg_kt <- c()                   
Air_France_Branded_conversion_rate_avg_kt <- c()             
Air_France_Global_Campaign_conversion_rate_avg_kt <- c() 
Business_Class_conversion_rate_avg_kt <- c()                    
French_Destinations_conversion_rate_avg_kt <- c()                          
General_Terms_conversion_rate_avg_kt <- c() 
Geo_Targeted_Boston_conversion_rate_avg_kt <- c()                   
Geo_Targeted_Chicago_conversion_rate_avg_kt <- c()                        
Geo_Targeted_DC_conversion_rate_avg_kt <- c() 
Geo_Targeted_Detroit_conversion_rate_avg_kt <- c()                   
Geo_Targeted_Houston_conversion_rate_avg_kt <- c()               
Geo_Targeted_Los_Angeles_conversion_rate_avg_kt <- c() 
Geo_Targeted_Miami_conversion_rate_avg_kt <- c()                  
Geo_Targeted_New_York_conversion_rate_avg_kt <- c()              
Geo_Targeted_Philadelphia_conversion_rate_avg_kt <- c() 
Geo_Targeted_San_Francisco_conversion_rate_avg_kt <- c()                   
Geo_Targeted_Seattle_conversion_rate_avg_kt <- c()                   
Google_Yearlong_conversion_rate_avg_kt <- c()
Outside_Western_Europe_conversion_rate_avg_kt <- c()                   
Paris_France_Terms_conversion_rate_avg_kt <- c()                             
Unassigned_conversion_rate_avg_kt <- c() 
Western_Europe_Destinations_conversion_rate_avg_kt <- c() 
i <- 1

#While loop to get average Conversion Rate per Campaign, per keyword type
while(i <= length(keyword_types)){
  Air_France_Brand_French_Destinations_conversion_rate_avg_kt <- 
    c(Air_France_Brand_French_Destinations_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Air France Brand & French Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))              
  Air_France_Branded_conversion_rate_avg_kt <- 
    c( Air_France_Branded_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Air France Branded" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Air_France_Global_Campaign_conversion_rate_avg_kt <- 
    c( Air_France_Global_Campaign_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Air France Global Campaign" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Business_Class_conversion_rate_avg_kt <- 
    c( Business_Class_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Business Class" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  French_Destinations_conversion_rate_avg_kt <- 
    c( French_Destinations_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "French Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  General_Terms_conversion_rate_avg_kt <- 
    c( General_Terms_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "General Terms" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Boston_conversion_rate_avg_kt <- 
    c( Geo_Targeted_Boston_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Boston" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Chicago_conversion_rate_avg_kt <- 
    c( Geo_Targeted_Chicago_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Chicago" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_DC_conversion_rate_avg_kt <- 
    c( Geo_Targeted_DC_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted DC" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Detroit_conversion_rate_avg_kt <- 
    c( Geo_Targeted_Detroit_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Detroit" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Houston_conversion_rate_avg_kt <- 
    c( Geo_Targeted_Houston_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Houston" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Los_Angeles_conversion_rate_avg_kt <- 
    c( Geo_Targeted_Los_Angeles_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Los Angeles" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Miami_conversion_rate_avg_kt <- 
    c( Geo_Targeted_Miami_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Miami" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_New_York_conversion_rate_avg_kt <- 
    c( Geo_Targeted_New_York_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted New York" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Philadelphia_conversion_rate_avg_kt <- 
    c( Geo_Targeted_Philadelphia_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Philadelphia" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_San_Francisco_conversion_rate_avg_kt <- 
    c( Geo_Targeted_San_Francisco_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted San Francisco" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Geo_Targeted_Seattle_conversion_rate_avg_kt <- 
    c( Geo_Targeted_Seattle_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Geo Targeted Seattle" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Google_Yearlong_conversion_rate_avg_kt <- 
    c( Google_Yearlong_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Google_Yearlong" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Outside_Western_Europe_conversion_rate_avg_kt <- 
    c( Outside_Western_Europe_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Outside Western Europe" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Paris_France_Terms_conversion_rate_avg_kt <- 
    c( Paris_France_Terms_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Paris & France Terms" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Unassigned_conversion_rate_avg_kt <- 
    c( Unassigned_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Unassigned" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  Western_Europe_Destinations_conversion_rate_avg_kt <- 
    c( Western_Europe_Destinations_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
      which(clean_airfrance$Campaign == "Western Europe Destinations" 
            & clean_airfrance$`Keyword Type` == keyword_types[i])]))
  i <- i + 1
} #--> closing Conversion Rate while loop

# Combine average Conversion Rate per campaign in a matrix
campaign_conversion_rate_per_keyword_type <- rbind(Air_France_Brand_French_Destinations_conversion_rate_avg_kt, 
                                               Air_France_Branded_conversion_rate_avg_kt, 
                                               Air_France_Global_Campaign_conversion_rate_avg_kt, 
                                               Business_Class_conversion_rate_avg_kt, 
                                               French_Destinations_conversion_rate_avg_kt, 
                                               General_Terms_conversion_rate_avg_kt, 
                                               Geo_Targeted_Boston_conversion_rate_avg_kt, 
                                               Geo_Targeted_Chicago_conversion_rate_avg_kt, 
                                               Geo_Targeted_DC_conversion_rate_avg_kt, 
                                               Geo_Targeted_Detroit_conversion_rate_avg_kt, 
                                               Geo_Targeted_Houston_conversion_rate_avg_kt, 
                                               Geo_Targeted_Los_Angeles_conversion_rate_avg_kt, 
                                               Geo_Targeted_Miami_conversion_rate_avg_kt, 
                                               Geo_Targeted_New_York_conversion_rate_avg_kt, 
                                               Geo_Targeted_Philadelphia_conversion_rate_avg_kt, 
                                               Geo_Targeted_San_Francisco_conversion_rate_avg_kt, 
                                               Geo_Targeted_Seattle_conversion_rate_avg_kt, 
                                               Google_Yearlong_conversion_rate_avg_kt, 
                                               Outside_Western_Europe_conversion_rate_avg_kt, 
                                               Paris_France_Terms_conversion_rate_avg_kt, 
                                               Unassigned_conversion_rate_avg_kt, 
                                               Western_Europe_Destinations_conversion_rate_avg_kt)

colnames(campaign_conversion_rate_per_keyword_type) <- keyword_types  #--> assign names to the rows
print(campaign_conversion_rate_per_keyword_type)


#####################   Keyword Type Analysis per Publisher  #####################

#### BASED ON ROA
#Create empty vectors per Publisher
google_GL_ROA_avg_kt <- c()
google_US_ROA_avg_kt <- c()
msn_GL_ROA_avg_kt <- c()
overture_GL_ROA_avg_kt <- c()
overture_US_ROA_avg_kt <- c()
yahoo_US_ROA_avg_kt <- c()
i <- 1 #--> setting i to start at 1

#While loop to get average ROA per Publisher, per match type
while(i <= length(keyword_types)){
  google_GL_ROA_avg_kt <- c(google_GL_ROA_avg_kt, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Google - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  google_US_ROA_avg_kt <- c(google_US_ROA_avg_kt, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Google - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  msn_GL_ROA_avg_kt <- c(msn_GL_ROA_avg_kt, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "MSN - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  overture_GL_ROA_avg_kt <- c(overture_GL_ROA_avg_kt, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Overture - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  overture_US_ROA_avg_kt <- c(overture_US_ROA_avg_kt, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Overture - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  yahoo_US_ROA_avg_kt <- c(yahoo_US_ROA_avg_kt, mean(clean_airfrance$ROA[
    which(clean_airfrance$`Publisher Name` == "Yahoo - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  i <- i + 1
  
} #--> closing ROA while loop

# Combine average ROA per publisher in a matrix
ROA_per_publisher_per_keyword_type <- rbind(google_GL_ROA_avg_kt, google_US_ROA_avg_kt, msn_GL_ROA_avg_kt, 
                                          overture_GL_ROA_avg_kt, overture_US_ROA_avg_kt, 
                                          yahoo_US_ROA_avg_kt)

colnames(ROA_per_publisher_per_keyword_type) <- keyword_types #--> assign names to the rows

print(ROA_per_publisher_per_keyword_type)

#### BASED ON NET REVENUE
#Create empty vectors per Publisher
google_GL_net_revenue_avg_kt <- c()
google_US_net_revenue_avg_kt <- c()
msn_GL_net_revenue_avg_kt <- c()
overture_GL_net_revenue_avg_kt <- c()
overture_US_net_revenue_avg_kt <- c()
yahoo_US_net_revenue_avg_kt <- c()
i <- 1 #--> setting i to start at 1

#While loop to get average net revenue per Publisher, per keyword type
while(i <= length(keyword_types)){
  google_GL_net_revenue_avg_kt <- c(google_GL_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Google - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  google_US_net_revenue_avg_kt <- c(google_US_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Google - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  msn_GL_net_revenue_avg_kt <- c(msn_GL_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "MSN - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  overture_GL_net_revenue_avg_kt <- c(overture_GL_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Overture - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  overture_US_net_revenue_avg_kt <- c(overture_US_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Overture - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  yahoo_US_net_revenue_avg_kt <- c(yahoo_US_net_revenue_avg_kt, mean(clean_airfrance$net_revenue[
    which(clean_airfrance$`Publisher Name` == "Yahoo - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  i <- i + 1
  
} #--> closing net_revenue while loop

# Combine average net_revenue per publisher in a matrix
net_revenue_per_publisher_per_keyword_type <- rbind(google_GL_net_revenue_avg_kt, google_US_net_revenue_avg_kt, msn_GL_net_revenue_avg_kt, 
                                            overture_GL_net_revenue_avg_kt, overture_US_net_revenue_avg_kt, 
                                            yahoo_US_net_revenue_avg_kt)

colnames(net_revenue_per_publisher_per_keyword_type) <- keyword_types #--> assign names to the rows

print(net_revenue_per_publisher_per_keyword_type)

#### BASED ON CONVERSION RATE
#Create empty vectors per Publisher
google_GL_conversion_rate_avg_kt <- c()
google_US_conversion_rate_avg_kt <- c()
msn_GL_conversion_rate_avg_kt <- c()
overture_GL_conversion_rate_avg_kt <- c()
overture_US_conversion_rate_avg_kt <- c()
yahoo_US_conversion_rate_avg_kt <- c()
i <- 1 #--> setting i to start at 1

#While loop to get average conversion rate per Publisher, per match type
while(i <= length(keyword_types)){
  google_GL_conversion_rate_avg_kt <- c(google_GL_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Google - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  google_US_conversion_rate_avg_kt <- c(google_US_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Google - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  msn_GL_conversion_rate_avg_kt <- c(msn_GL_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "MSN - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  overture_GL_conversion_rate_avg_kt <- c(overture_GL_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Overture - Global" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  overture_US_conversion_rate_avg_kt <- c(overture_US_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Overture - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  yahoo_US_conversion_rate_avg_kt <- c(yahoo_US_conversion_rate_avg_kt, mean(clean_airfrance$conversion_rate[
    which(clean_airfrance$`Publisher Name` == "Yahoo - US" & 
            clean_airfrance$`Keyword Type` == keyword_types[i])]))
  
  i <- i + 1
  
} #--> closing conversion_rate while loop

# Combine average conversion_rate per publisher in a matrix
conversion_rate_per_publisher_per_keyword_type <- rbind(google_GL_conversion_rate_avg_kt, google_US_conversion_rate_avg_kt, msn_GL_conversion_rate_avg_kt, 
                                            overture_GL_conversion_rate_avg_kt, overture_US_conversion_rate_avg_kt, 
                                            yahoo_US_conversion_rate_avg_kt)

colnames(conversion_rate_per_publisher_per_keyword_type) <- keyword_types #--> assign names to the rows

print(conversion_rate_per_publisher_per_keyword_type)


################      Updating DataSets for Keyword Analysis     ################

## Publishers <> Match Type
ROA_per_publisher_per_match_type <- as.data.frame(ROA_per_publisher_per_match_type)
net_revenue_per_publisher_per_match_type <- as.data.frame(net_revenue_per_publisher_per_match_type)
conversion_rate_per_publisher_per_match_type <- as.data.frame(conversion_rate_per_publisher_per_match_type)

ROA_per_publisher_per_match_type$Publishers <- publishers
net_revenue_per_publisher_per_match_type$Publishers <- publishers
conversion_rate_per_publisher_per_match_type$Publishers <- publishers

## Publishers <> Keyword Type
ROA_per_publisher_per_keyword_type <- as.data.frame(ROA_per_publisher_per_keyword_type)
net_revenue_per_publisher_per_keyword_type <- as.data.frame(net_revenue_per_publisher_per_keyword_type)
conversion_rate_per_publisher_per_keyword_type <- as.data.frame(conversion_rate_per_publisher_per_keyword_type)

ROA_per_publisher_per_keyword_type$Publishers <- publishers
net_revenue_per_publisher_per_keyword_type$Publishers <- publishers
conversion_rate_per_publisher_per_keyword_type$Publishers <- publishers

## Campaigns <> Match Type

campaign_ROA_per_match_type <- as.data.frame(campaign_ROA_per_match_type)
campaign_net_revenue_per_match_type <- as.data.frame(campaign_net_revenue_per_match_type)
campaign_conversion_rate_per_match_type <- as.data.frame(campaign_conversion_rate_per_match_type)

campaign_ROA_per_match_type$Campaigns <- campaigns
campaign_net_revenue_per_match_type$Campaigns <- campaigns
campaign_conversion_rate_per_match_type$Campaigns <- campaigns

## Campaigns <> Keyword Type
campaign_ROA_per_keyword_type <- as.data.frame(campaign_ROA_per_keyword_type)
campaign_net_revenue_per_keyword_type <- as.data.frame(campaign_net_revenue_per_keyword_type)
campaign_conversion_rate_per_keyword_type <- as.data.frame(campaign_conversion_rate_per_keyword_type)

campaign_ROA_per_keyword_type$Campaigns <- campaigns
campaign_net_revenue_per_keyword_type$Campaigns <- campaigns
campaign_conversion_rate_per_keyword_type$Campaigns <- campaigns


################     Graphs for Keyword and campaign analysis    ################
### PUBLISHERS
## ROA per match type
ROA_per_publisher_per_match_type %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("ROA per Publisher based on Match Type")

# Remove Outlier
ROA_per_publisher_per_match_type_outlier_free <- ROA_per_publisher_per_match_type
ROA_per_publisher_per_match_type_outlier_free[1, 3] <- 0
ROA_per_publisher_per_match_type_outlier_free %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("ROA per Publisher based on Match Type w/o Google Global Exact")

## Net Revenue per match type
net_revenue_per_publisher_per_match_type %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("Net revenue per Publisher based on Match Type")

# Remove Outlier
net_revenue_per_publisher_per_match_type_outlier_free <- net_revenue_per_publisher_per_match_type
net_revenue_per_publisher_per_match_type_outlier_free[1, 3] <- 0
net_revenue_per_publisher_per_match_type_outlier_free %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("Net Revenue per Publisher based on Match Type w/o Google Global Exact")

## Conversion rate per match type
conversion_rate_per_publisher_per_match_type %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("Conversion rate per Publisher based on Match Type")

# Remove Outlier
conversion_rate_per_publisher_per_match_type_outlier_free <- conversion_rate_per_publisher_per_match_type
conversion_rate_per_publisher_per_match_type_outlier_free[1, 3] <- 0
conversion_rate_per_publisher_per_match_type_outlier_free %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("Conversion rate per Publisher based on Match Type w/o Google Global Exact")

## ROA per keyword type
ROA_per_publisher_per_keyword_type %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("ROA per Publisher based on keyword Type")

## Net Revenue per keyword type
net_revenue_per_publisher_per_keyword_type %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("Net revenue per Publisher based on Match Type")

## Conversion rate per keyword type
conversion_rate_per_publisher_per_keyword_type %>%
  gather("Type", "Value",-Publishers) %>%
  ggplot(aes(Publishers, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  ggtitle("Conversion rate per Publisher based on keyword Type")

### CAMPAIGNS
## ROA per match type
campaign_ROA_per_match_type %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("ROA per Campaign based on Match Type")

# Remove Outlier
campaign_ROA_per_match_type_outlier_free <- campaign_ROA_per_match_type
campaign_ROA_per_match_type_outlier_free[3, 3] <- 0
campaign_ROA_per_match_type_outlier_free %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("ROA per Campaign based on Match Type w/o Air France Global exact match")

## Net Revenue per match type
campaign_net_revenue_per_match_type %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Net Revenue per Campaign based on Match Type")

# Remove Outlier
campaign_net_revenue_per_match_type_outlier_free <- campaign_net_revenue_per_match_type
campaign_net_revenue_per_match_type_outlier_free[2:3, c(1,3)] <- 0
campaign_net_revenue_per_match_type_outlier_free %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Net Revenue per Campaign based on Match Type w/o  
          Air France Global exact match & Air France branded broad match")

## Conversion rate per match type
campaign_conversion_rate_per_match_type %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Conversion rate per Campaign based on Match Type")

# Remove Outlier
campaign_conversion_rate_per_match_type_outlier_free <- campaign_conversion_rate_per_match_type
campaign_conversion_rate_per_match_type_outlier_free[3, 3] <- 0
campaign_conversion_rate_per_match_type_outlier_free %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Conversion rate per Campaign based on Match Type w/o Air France Global exact match")

## ROA per keyword type
campaign_ROA_per_keyword_type %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("ROA per Campaign based on keyword Type")

## Conversion rate per keyword type
campaign_conversion_rate_per_keyword_type %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Conversion rate per Campaign based on keyword Type")

## Net Revenue per keyword type
campaign_net_revenue_per_keyword_type %>%
  gather("Type", "Value",-Campaigns) %>%
  ggplot(aes(Campaigns, Value, fill = Type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Net Revenue per Campaign based on keyword Type")

#################################################################################
## KAYAK ANALYSIS
#################################################################################

########  Analyzing if they should invest in Kayak as well Q4 ###################

dfchannel_clean <- dfchannelNewUS[ 1:3, ]
dfchannel_clean2 <- dfchannelNewUS[ 6:7, ]
dfchannel_cleanfinal <- rbind(dfchannel_clean, dfchannel_clean2)
View(dfchannel_cleanfinal)
#changing from character to numeric and rounding to 2 decimals 
dfchannel_cleanfinal$avg_cpc <- round(as.numeric(dfchannel_cleanfinal$avg_cpc), digits = 2)

dfchannel_cleanfinal$avg_ROA <- round(as.numeric(dfchannel_cleanfinal$avg_ROA),digits =2)

dfchannel_cleanfinal$avg_conversion_rate <- round(as.numeric(dfchannel_cleanfinal$avg_conversion_rate), digits = 2)

dfchannel_cleanfinal$avg_Revenue_booking <- round(as.numeric(dfchannel_cleanfinal$avg_Revenue_booking), digits = 2)

View(dfchannel_cleanfinal)

##########   Creating plots for two most important variables    #################

#scatter plot for channel ROA 
pChannelROA <- plot_ly(dfchannel_cleanfinal, x= ~ `Publisher Name`,  y= ~avg_ROA, 
                       mode = "markers", type = "scatter", name = "Comparison Channels", 
                       color = I("red"), alpha = 0.8)%>%
  layout(title = "Average Return on Advertisement per Channel",
         xaxis = list(title = "Channel"),
         yaxis = list(title = "Avg ROA"))
pChannelROA

#scatter plot for channel revenue booking
pChannelrevb <- plot_ly(dfchannel_cleanfinal, x= ~ `Publisher Name`,  
                        y= ~avg_Revenue_booking, mode = "markers", type = "scatter", 
                        name = "Comparison Channels", color = I("blue"), alpha = 0.8)%>%
  layout(title = "Booking Revenue per Channel",
         xaxis = list(title = "Channel"),
         yaxis = list(title = "Avg Rev Booking"))
print(pChannelrevb)


