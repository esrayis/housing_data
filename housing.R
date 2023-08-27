library(dplyr)
library(tidyverse)



#STEP1 - Access the Data Set


housing <- read_csv("housing.csv")
names(housing)

#Cast the ocean_proximity character variable to the factor class
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

#display the resulting levels
levels(housing$ocean_proximity)



#STEP2 - EDA and Data Visualization

head(housing)
tail(housing)

summary(housing)

#find numeric variable from the housing dataset
housing_numeric <- housing[, sapply(housing, is.numeric)]
cor_housing_numeric <- cor(housing_numeric)

## Plot the correlation matrix using a heatmap
library(corrplot)
corrplot(cor_housing_numeric, type = "upper", method = "number")

#histogram
par(mfrow = c(3,3))
for (i in 1:length(housing_numeric)) {
  hist(housing_numeric[[i]], 
       main = paste(names(housing_numeric)[i], "histogram"), xlab = names(housing_numeric)[i] , col = "darkblue")
}

#boxplot
# Produce boxplots for each numeric variable
par(mfrow = c(3, 3))  # Set the layout of the subplots

for (i in 1:length(housing_numeric)) {
  boxplot(housing_numeric[[i]] , 
          main = paste(names(housing_numeric)[i] ,"boxplot"),
          ylab = names(housing_numeric)[i],
          col = "grey")
}

#boxplot2
# Produce boxplots for specific variables with respect to ocean_proximity
variables <- c("housing_median_age", "median_income", "median_house_value")

par(mfrow = c(length(variables), 1))  # Set the layout of the subplots

for (variable in variables) {
  boxplot(housing[[variable]] ~ housing$ocean_proximity, 
          data = housing, 
          xlab = "ocean proximity",
          ylab = "",
          main = variable, col = "darkblue")
}

#STEP3 - Data Transformation

#a-)
summary(housing) #207 missing values in total_bedrooms colomns.

# Find the median of the `total_bedrooms` variable
median_bedrooms <- median(housing$total_bedrooms, na.rm = TRUE)


#library(e1071)
#housing$total_bedrooms <- impute(housing[5], what = "median")

# Impute the missing values in the `total_bedrooms` variable with the median
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median_bedrooms

summary(housing$total_bedrooms) # Check that the missing values have been replaced


#b-)

 housing$NEAR_BAY <- ifelse(housing$ocean_proximity == "NEAR BAY", 1, 0)
 housing$`<1H OCEAN` <- ifelse(housing$ocean_proximity == "`<1H OCEAN`", 1, 0)
 housing$INLAND <- ifelse(housing$ocean_proximity == "INLAND", 1, 0)
 housing$NEAR_OCEAN <- ifelse(housing$ocean_proximity == "NEAR OCEAN", 1, 0)
 housing$ISLAND <- ifelse(housing$ocean_proximity == "ISLAND", 1, 0)
 
 # Remove the original ocean_proximity column
 housing$ocean_proximity <- NULL
 
 

#c-) 
#>Use the total_bedrooms and total_rooms variables along with
#households to create two new variables: mean_bedrooms and
#mean_rooms as these are likely to be more accurate depictions of the
#houses in a given group. You can then remove the total_bedrooms and
#total_rooms variables once you’ve accomplished this requirement

# Calculate the mean number of bedrooms and mean number of rooms per household
mean_bedrooms <- housing$total_bedrooms / housing$households
mean_rooms <- housing$total_rooms / housing$households

# Create the new variables mean_bedrooms and mean_rooms
housing$mean_bedrooms <- mean_bedrooms
housing$mean_rooms <- mean_rooms

# Remove the total_bedrooms and total_rooms variables
housing <- subset(housing, select = -c(total_bedrooms, total_rooms))


#d-)Perform feature scaling. Scale each numerical variable except for
#median_house_value (as this is our response variable), and the binary
#categorical variables

housing <- housing %>% 
  relocate(median_house_value, .after=ISLAND)

housing <- housing %>%
  relocate(mean_rooms, mean_bedrooms, .after = median_income)


dfNormZ <- as.data.frame(scale(housing[1:8]))
dfNormZ

#e-) relocation of columns as a given row

cleaned_housing <- housing %>%
  relocate("NEAR_BAY","<1H OCEAN" , "INLAND",
           "NEAR_OCEAN", "ISLAND", "longitude",
           "latitude" , "housing_median_age" , "population",
           "households" , "median_income" , "mean_bedrooms",
           "mean_rooms" , "median_house_value")

##STEP 4 - Create Training and Test Sets

##
# Split data set into training set and test set
n <- nrow(cleaned_housing)  # Number of observations = 20640
ntrain <- round(n*0.7)    # 70% for training set
set.seed(314)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index

train <- cleaned_housing[tindex,]  # Create training set
test <- cleaned_housing[-tindex,]  # Create test set

##STEP 5 - Supervised Machine Learning - Regression


install.packages("randomForest")
library(randomForest)
?randomForest

# Split the training set into train_x and train_y
train_x <- train[, -1]
train_y <- as.numeric(train$median_house_value)

# Train a random forest model
rf <- randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE)

# Print the names of the metrics computed by the algorithm
names(rf)

#STEP 6 - Evaluating Model Performance

#a-)
last_mse <- rf$mse[length(rf$mse)]


rmse <- sqrt(last_mse)


#b-)
test_x <- test[,-1]
test_y <- as.numeric(test$median_house_value)

rf_test <- predict(rf, newdata = test_x)

plot(y=sqrt(rf$mse) , x= seq(1, 500, 1))

#c-)
rmse_test <- sqrt(mean(as.matrix(rf_test - test$median_house_value)^2))

#d-)
print(rmse)
#training set RMSE is 5539.117
print(rmse_test)
#test set RMSE is 5171.901
#they are close each other

#e-)
varImpPlot(rf)

rf_test_last <- randomForest(x = test_x, y = test_y, ntree = 500, importance = TRUE)
varImpPlot(rf_test_last)

#it shows us which variable is important. In this tree, we can say that median house value is
#significant one.%IncMSE is simply the average increase in squared residuals of the test set 
#when variables are randomly permuted (little importance = little change in model when 
#variable is removed or added) and IncNodePurity is the increase in homogeneity in the data partitions.