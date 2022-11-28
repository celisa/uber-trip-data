
# Importing the data

library(dplyr)
library(tidyverse)
library(knitr)
library(caret)
library(boot)
#library(tidyverse)
library(table1)
library(pander)
library(leaps)
library(tinytex)
library(car)
#library(dplyr)
library(lattice) 
library(RColorBrewer)
panderOptions('digits', 6)
panderOptions('round', 2)
panderOptions('keep.trailing.zeros', TRUE)
# Importing packages
library(lubridate)
library(ggplot2)
library(arm)

uber <- read_csv("https://raw.githubusercontent.com/celisa/uber-trip-data/main/uber-data-cleaned.csv")



uber15 <- read_csv("https://raw.githubusercontent.com/celisa/uber-trip-data/main/uber-data-2015.csv")
uber15$weekday <- wday(uber15$Pickup_date)
uber15$weekday <- 
  factor(uber15$weekday, 
         levels=c(1:7),
         labels=c("Sunday", "Monday","Tuesday",
                  "Wednesday","Thursday","Friday",
                  "Saturday"))
uber15_agg <- uber15 %>% group_by(Pickup_date, Borough) %>% 
  summarise(trips = sum(num_uber_trips))
#ggplot(data = uber15_agg, aes(x = Borough, y = trips)) + 
#  geom_bar(position = "dodge",  stat = "summary", fun = "mean") +
#  labs(x = "Borough", y = "Number of trips", title = "Average number of trips by borough")
uber_weather <- uber %>% select(!num_uber_trips)
uber15_weather <- left_join(uber15_agg, uber_weather, by = c("Pickup_date" = "DATE"))


uber15_weather$rain <- ifelse(uber15_weather$PRCP > 0, "rain", "no_rain")


new_weather <- subset(uber15_weather, uber15_weather$Borough != "EWR" & uber15_weather$Borough != "Unknown")

new_weather$Borough <- toupper(new_weather$Borough)





#uber15_weather$Borough != EWR & uber15_weather$Borough != Unknown


uber_merged <- read_csv("https://raw.githubusercontent.com/celisa/uber-trip-data/main/uber_merged.csv")


uber_weather_merged <- full_join(uber_merged, new_weather, by = c("DATE" = "Pickup_date", 'Borough' = 'Borough'))


#`AWND`(wind),`rain`(boolean flag to determine whether it rained), `SNOW` (boolean for whether it snowed during the day) and `TAVG`(avg. temp. of the day)

keeps <- c("DATE","Borough","No. of uber trips", "PERSONS KILLED", "trafficcol","AWND","rain", "SNOW", "TAVG")


#final dataset including both weather and uber trip values
final_df = uber_weather_merged[keeps]


uber15_weather$rain <- ifelse(uber15_weather$PRCP > 0, "rain", "no_rain")


final_df$killed_YN <- ifelse(final_df$"PERSONS KILLED">0, "Yes", "No")

final_df$killed_YN_binary <- ifelse(final_df$"PERSONS KILLED">0, 1, 0)

final_df$Borough2 <- relevel(final_df$Borough, ref = "MANHATTAN") 
colnames(final_df)[3] = "ubertrips"

final_df$rain <- factor(final_df$rain)
final_df$Borough2 <- factor(final_df$Borough2)
final_df$Borough <- factor(final_df$Borough)
final_df$killed_YN <- factor(final_df$killed_YN)
final_df$killed_YN_binary <- factor(final_df$killed_YN_binary)

#colnames(final_df)

#splitted data for train and test
#change the way you split the dataset

# final_df_train <- final_df[final_df$DATE < "2015-05-20",]
# 
# final_df_test <- final_df[final_df$DATE >= "2015-05-20",]

library(caTools)

#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample.split(final_df$killed_YN, SplitRatio = 0.7)
train  <- subset(final_df, sample == TRUE)
test   <- subset(final_df, sample == FALSE)


#running logistic 
# train_logistic <- glm(killed_YN ~ Borough + ubertrips + trafficcol + AWND + rain + SNOW + TAVG, data = final_df_train, family = "binomial"  )
# 
# summary(train_logistic)


##using set.seed(1) method - running logistic regression on the splitted train dataset

train_logist <- glm(killed_YN ~ Borough + ubertrips + trafficcol + AWND + rain + SNOW + TAVG, data = train, family = "binomial"  )
summary(train_logist)

#train_logistic2 <- glm(killed_YN ~ Borough +  trafficcol + AWND + rain + SNOW + TAVG, data = final_df_train, family = "binomial"  )


#don't run a logistic regression on the test data
#test_logistic <- glm(killed_YN ~ Borough + ubertrips + trafficcol + AWND + rain + SNOW + TAVG, data = final_df_test, family = "binomial"  )

summary(test_logistic)

# logist2 <- glm(Win ~ Home + TeamPoints + 
#                  FieldGoals. + Assists + Steals + Blocks + TotalRebounds + Turnovers + Opp.FieldGoals. +  Opp.TotalRebounds + Opp.TotalFouls + Opp.Turnovers, data = nba_reduced_train, family = "binomial") 

##binned plots - train dataset

binnedplot(fitted(train_logistic), residuals(train_logistic, "response"), xlab = "predicted probabilities")
#binnedplot(final_df_train$Borough, residuals(train_logistic, "response"), xlab = "Perimeter")

binnedplot(final_df_train$ubertrips, residuals(train_logistic, "response"), xlab = "ubertrips")

binnedplot(final_df_train$trafficcol, residuals(train_logistic, "response"), xlab = "traffic collision")
binnedplot(final_df_train$AWND, residuals(train_logistic, "response"), xlab = "average wind")

#binnedplot(final_df_train$rain, residuals(train_logistic, "response"), xlab = "average wind")

binnedplot(final_df_train$SNOW, residuals(train_logistic, "response"), xlab = "SNOW")
binnedplot(final_df_train$TAVG, residuals(train_logistic, "response"), xlab = "average temperature")



##binned plots - test dataset

binnedplot(fitted(test_logistic), residuals(test_logistic, "response"), xlab = "predicted probabilities")
#binnedplot(final_df_train$Borough, residuals(train_logistic, "response"), xlab = "Perimeter")

binnedplot(final_df_test$ubertrips, residuals(test_logistic, "response"), xlab = "ubertrips")

binnedplot(final_df_test$trafficcol, residuals(test_logistic, "response"), xlab = "traffic collision")
binnedplot(final_df_test$AWND, residuals(test_logistic, "response"), xlab = "average wind")

#binnedplot(final_df_train$rain, residuals(train_logistic, "response"), xlab = "average wind")

binnedplot(final_df_test$SNOW, residuals(test_logistic, "response"), xlab = "SNOW")
binnedplot(final_df_test$TAVG, residuals(test_logistic, "response"), xlab = "average temperature")


#Deviance test to check for the overall model fit

logistic_null <- glm(killed_YN ~ 1, data = final_df_train, family = "binomial")
summary(logistic_null)

anova(logistic_null, train_logist, test = "Chisq")


##roc curves - train and test data

#train dataset
roc(train$killed_YN, fitted(train_logist), print.thres=0.5,print.auc = T, plot = T, legacy.axes = T)



#Vif test

library(DAAG)

vif(train_logist)

#vif(test_logistic)


#confidence interval

confint(train_logist)

exp(confint(train_logist))

# confint(test_logist)
# 
# exp(confint(test_logist))


#confidence interval and exponentiating the coefficients for odds ratio

train_logistic_summary <- data.frame(exp(cbind(OR = coef(train_logist), confint(train_logist))))

#test_logistic_summary <- data.frame(exp(cbind(OR = coef(test_logistic), confint(test_logistic))))


library(caret)
#in sample prediction
training_prediction <- predict(train_logistic, final_df_train, type="response")
training_prediction <- as.factor(ifelse(training_prediction > 0.5, 1, 0))
confusion1 <- confusionMatrix(training_prediction, as.factor(final_df_train$killed_YN_binary))

##prediction using seed

training_prediction2 <- predict(train_logist, train, type="response")
training_prediction2 <- as.factor(ifelse(training_prediction2 > 0.5, 1, 0))
confusion_new <- confusionMatrix(training_prediction2, as.factor(train$killed_YN_binary))


#out of sample prediction
test_prediction <- predict(train_logistic, final_df_test, type="response")
test_prediction <- as.factor(ifelse(test_prediction > 0.5, 1, 0))
confusion2 <- confusionMatrix(test_prediction, final_df_test$killed_YN_binary)


#out of sample prediction seed
test_prediction2 <- predict(train_logistic, test, type="response")
test_prediction2 <- as.factor(ifelse(test_prediction2 > 0.5, 1, 0))
confusion_test <- confusionMatrix(test_prediction2, test$killed_YN_binary)


# test_prediction <- predict(test_logistic, final_df_test, type="response")
# test_prediction <- as.factor(ifelse(test_prediction > 0.1, 1, 0))
# confusion2 <- confusionMatrix(test_prediction, final_df_test$killed_YN_binary)