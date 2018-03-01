library(caret)
# Read in the data from the CSV
houseData<-read.csv(file.choose())

## Introducing new column
houseData$howOld<-0
for(i in 1:nrow(houseData)){
  if(houseData[i,]$yr_renovated==0){
    houseData[i,]$howOld<-2015-houseData[i,]$yr_built
  }
  else{
    houseData[i,]$howOld<-2015-houseData[i,]$yr_renovated
  }
}

# Based on the correlation
# Filter out unnecessary columns
requiredCols<-c('price', 'sqft_living', 'grade', 'sqft_above', 'bathrooms',
                'sqft_basement', 'bedrooms', 'floors', 'waterfront', 'howOld',
                'view', 'sqft_living15', 'zipcode', 'yr_renovated')

houseDataClean<-houseData[,requiredCols]

# Some column types doesn't make sense
# For example: Year is not numeric, it must be categorical or factor
# So, change the corresponding column types
houseDataClean$bathrooms<-as.factor(houseDataClean$bathrooms)
houseDataClean$bedrooms<-as.factor(houseDataClean$bedrooms)
houseDataClean$floors<-as.factor(houseDataClean$floors)
houseDataClean$yr_renovated<-as.numeric(houseDataClean$yr_renovated)
houseDataClean$zipcode<-as.factor(houseDataClean$zipcode)
houseDataClean$waterfront<-as.factor(houseDataClean$waterfront)
houseDataClean$view<-as.numeric(houseDataClean$view)
houseDataClean$grade<-as.numeric(houseDataClean$grade)
houseDataClean$sqft_living<-as.numeric(houseDataClean$sqft_living)
houseDataClean$sqft_above<-as.numeric(houseDataClean$sqft_above)
houseDataClean$sqft_basement<-as.numeric(houseDataClean$sqft_basement)
houseDataClean$sqft_living15<-as.numeric(houseDataClean$sqft_living15)


# Removing 7, 8, 9, 10, 11 and 33 bedrooms since their individual
# count is less than 40 obs
index<-houseDataClean$bedrooms %in% c(0,7,8,9,10,11,33)
houseDataClean<-houseDataClean[!index,]

# Done with the data cleaning
# Now divide them for training and testing
# Set the random generator seed for reproducibility
set.seed(192)
# Slicing them in 80-20%
library(magrittr)
library(caret)
slice<-createDataPartition(houseDataClean$price, p=0.8, list = FALSE)

train<-houseDataClean[slice,]
test<-houseDataClean[-slice,]


# Fit the linear model
fit<-lm(price~., train)

# Predict testing data on our linear model
pred<-predict(fit, newdata =test)


# Compare the solutions and predictions

sol<-test$price

diff<-sol-pred



#############ui


