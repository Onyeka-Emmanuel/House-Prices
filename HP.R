#Call in Libraries 
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggthemes)
library(naniar)
library(ggplot2)
library(class)
library(caret)
library(e1071)
library(plotly)
library(Hmisc)
library(regclass)
library(reshape2)
library(imputeTS)
library(corrplot)
library(olsrr)
library(MASS)


#Import the training and test data.
HPtrain = read.csv(file.choose(), header = T)
HPtest = read.csv(file.choose(), header = T)

#Verify both datasets imported correctly.
head(HPtrain) #View first few rows.
head(HPtest) #View first few rows. 

#Get some info on datasets.
str(HPtrain)
str(HPtest)

# We see that SalePrice variable is missing from test dataset.


# FIRST: Merge the two datasets.
# SECOND: Address the Missing Values.
# THIRD: Separate the dataset back to train and test
# FOURTH: Deal with outliers in training dataset




#FIRST: MERGE
#Create variable to identify each dataset for later when we separate them.
HPtrain$Dataset = "train"
HPtest$Dataset = "test"

#Create SalePrice variable for the test dataset. We will remove this later.
HPtest$SalePrice = 0

#Merge the two datasets for cleaning
HP = merge(HPtrain, HPtest, all = T)
str(HP) #See info on merged dataset
dim(HP)[1] == dim(HPtrain)[1] + dim(HPtest)[1] #Check if the number of rows is correct





#SECOND: MISSING VALUES
#View missing values
gg_miss_var(HP) #Visual plot of missing values per variable.
colSums(is.na(HP)) #See number of missing values per variable.

# Firstly We will take care of the NAs character variables.
# ONE: Convert characters to factors.
# TWO: Since NAs in Electrical and MasVnrType are actually missing and few, we will replace with the mode.
# THREE: For the rest of the categorical variables, NAs represent "None", so we will replace with "None".

#ONE
HP = HP %>% mutate_if(is.character, as.factor) #Convert the character variables to factors
str(HP) #Verify the change

#TWO
#For Electrical and MasVnrType, replace the NAs with the mode.
HP$Electrical <- replace_na(HP$Electrical, mode_factor(HP$Electrical))
HP$MasVnrType <- replace_na(HP$MasVnrType, mode_factor(HP$MasVnrType))

#THREE
#The codes below will replace the NAs with None for the remaining factor variables
for(i in 1:dim(HP)[2]){
  if(is.factor(HP[,i])==T){
    HP[,i] <- replace_na(as.character(HP[,i]), "None")
  }
  HP = HP %>% mutate_if(is.character, as.factor)
}

gg_miss_var(HP) #Visual plot of missing values per variable


# Next we will take care of the NAs in the integer variables.
# We will tackle the missing values analytically. 
# We will see the effect of replacing NAs vs omitting the variable on our Adjusted R-squared for the model. Higher is better. 

# ONE: Using the training dataset we will subset our integer variables and remove Id variable
# TWO: Create a regression model for our test.
# THREE: Replace missing values and run through our model.
# FOUR: Remove variable with missing values all together and run through our model.
# FIVE: Address correlation between predictor variables
# SIX: Analyze table of Adjusted R-squared then address the missing values.
# SEVEN: Run the model through Machine Learning.
# PREDICT

#ONE:
HP.int = HPtrain[sapply(HPtrain, is.integer)] #Store integer variables in object. 
str(HP.int) #Verify that variables in HP are only integer or numerical.
gg_miss_var(HP.int) #See variables with NAs. We have 3, LotFrontage, GarageYrBlt, and MasVnrArea.

#  TWO:
#Create regression model
Fit = lm(SalePrice~., data = HP.int) #Our regression model
summary(Fit) # See the summary statistics for the model. 

# We see that LotFrontage and GrageYrBlt are not making significant contributions to the model. 
# MasVnrArea is making a significant contribution to the model (p-value 7.15e-06).
con = summary(Fit)$adj.r.squared #Store the Adjusted R-squared. It is 0.8036.
con #Call con


#  THREE:
# Let's replace the missing values and see if the model performs better
#The codes below will replace NAs with the mean of the variable, run our model and store the Adjusted R-squared into an object
HP.rpl = HP.int
HP.rpl$LotFrontage = replace_na(HP.rpl$LotFrontage, summary(HP.rpl$LotFrontage)[4]) #Replace NAs.
gg_miss_var(HP.rpl) #Visually verify NAs are gone.
Fit.NA1 = lm(SalePrice~., data = HP.rpl) #Run regression model.
summary(Fit.NA1) #Call the summary statistics for the model. 
# The model performed better (Adjusted R-squared 0.8055). 
# Of the 3 variables, only MasVnrArea is making a significant impact (p-value 2.33e-06)
rpll = summary(Fit.NA1)$adj.r.squared #Store Adjusted R-squared.

HP.rpl = HP.int
HP.rpl$GarageYrBlt = replace_na(HP.rpl$GarageYrBlt, round(summary(HP.rpl$GarageYrBlt)[4],0))
gg_miss_var(HP.rpl)
Fit.NA2 = lm(SalePrice~., data = HP.rpl)
summary(Fit.NA2) #Call the summary statistics for the model. 
# We see that the model did better (Adjusted R-squared 0.8070). 
#Only MasVnrArea is making a significant impact (p-value 3.14e-07).
rplg = summary(Fit.NA2)$adj.r.squared

HP.rpl = HP.int
HP.rpl$MasVnrArea = replace_na(HP.rpl$MasVnrArea, round(summary(HP.rpl$MasVnrArea)[4],0))
gg_miss_var(HP.rpl)
Fit.NA3 = lm(SalePrice~., data = HP.rpl)
summary(Fit.NA3) #Call the summary statistics for the model. 
# We see that the model did worse (Adjusted R-squared 0.8039). 
# Only MasVnrArea is making a significant impact (p-value 1.20e-05).
rplm = summary(Fit.NA3)$adj.r.squared


# Let's omit the variables and see if the model performs better.
HP.omit = HP.int %>% dplyr::select(-LotFrontage) #Remove the LotFrontage variable.
gg_miss_var(HP.omit) #Visually check that it is removed.
Fit.NA4 = lm(SalePrice~., data = HP.omit) #Run our model.
summary(Fit.NA4)  
# We see that the model performed worse without LotFrontage (Adjusted R-squared 0.8052).  
# Only MasVnrArea is making a significant impact (p-value 2.34e-06)
omtl = summary(Fit.NA4)$adj.r.squared

HP.omit = HP.int %>% dplyr::select(-GarageYrBlt)
gg_miss_var(HP.omit)
Fit.NA5 = lm(SalePrice~., data = HP.omit)
summary(Fit.NA5) 
# We see that the model performed better without GarageYrBlt (Adjusted R-squared 0.8067).
# Only MasVnrArea is making a significant impact (p-value 3.06e-07)
omtg = summary(Fit.NA5)$adj.r.squared

HP.omit = HP.int %>% dplyr::select(-MasVnrArea)
gg_miss_var(HP.omit)
Fit.NA6 = lm(SalePrice~., data = HP.int.omit)
summary(Fit.NA6) 
# We see that the model performed worse without MasVnrArea (Adjusted R-squared 0.8052). 
omtm = summary(Fit.NA6)$adj.r.squared


#FIVE:
# Let's look at the stored Adjusted R-squared values
var = c("Control","LotFrontage","GarageYrBlt","MasVnrArea") #Create a list with the variable names.
rplna = c(con, rpll, rplg, rplm) #Create vector of adj.r.squared for replaced variable.
omtna = c(con, omtl, omtg, omtm) #Create a vector of adj.r.squared for omitted variable.

Adjr = data.frame(Variable = var, Replace = rplna, Omit = omtna) #Create dataframe for adj.r.squared.
Adjr$Diff = Adjr$Replace - Adjr$Omit #Create variable to show the difference in adj.r.squared between replace and omit.
Adjr #Call to see the dataframe.

# We see that the difference in adj.r.squared between replacing and omitting are insignificant.
# Due to this insignificance, we will just replace the missing values with the mean.
gg_miss_var(HP)

HP$GarageYrBlt = replace_na(HP$GarageYrBlt, round(summary(HP$GarageYrBlt)[4],0)) #Replace NAs in GarageYrBlt. with the mean.
# For the codes below, the rest of the NAs in the integer variables will be replaced with the variable mean.
for(i in 1:dim(HP)[2]){
  if(is.integer(HP[,i])==T){
    HP[,i] <- replace_na(HP[,i], summary(HP[,i])[4])
  }
}

gg_miss_var(HP) #Check that there are no missing values.
str(HP) #See info on dataset. Everything looks good.



#  THIRD: SEPARATE TO TRAIN AND TEST
HPtrain.clean = HP[HP$Dataset=="train",] %>% dplyr::select(-Dataset, Id) #Store rows from the original HPtrain dataset in object.
head(HPtrain.clean) #View the first few rows.

HPtest.clean = HP[HP$Dataset=="test",] %>% dplyr::select(-Dataset) #Store rows from the original HPtest dataset in object.
head(HPtest.clean) #View the first few rows.

dim(HPtrain)[1] == dim(HPtrain.clean)[1] #Check that the number of rows matches the original training dataset.
dim(HPtest)[1] == dim(HPtest.clean)[1] #Check that the number of rows matches the original test dataset.

str(HP.int)



#  FOURTH: NORMALITY
hist(HPtrain.clean) #Looking at the histograms, there is evidence of skewness.  
#To address this we will use the log of SalePrice in our model

model = lm(SalePrice ~ ., data = HPtrain.clean)
summary(model)$adj.r.squared
mse <- function(x) mean(x$residuals^2)
mse(model)
#We see that the adj.r.squared is 0.9193 and the MSE is quite high.

model = lm(log(SalePrice) ~ ., data = HPtrain.clean)
summary(model)$adj.r.squared
mse(model)
#We see that with the log, the adj.r.squared improves to 0.9323 and MSE drastically lowers.




#  FIFTH: COLINEARITY
# we will start by check for any perfect correlations between the predictor variables.
# We will be looking only at the numerical variables.
HP.num = HPtrain.clean[sapply(HPtrain.clean, is.numeric)] #Subset numerical variables into an object.
model.num = lm(log(SalePrice) ~ ., data = HP.num) #Run the numerical subset through the model.
summary(model.num)$adj.r.squared #View summary statistics for the model. 
# We have an adj.r.squared of 0.8651  

alias(model.num) #This checks for perfect linear coefficients.
# We see that all the variables that measure square footage of some part of the house are perfectly correlated. 
# Thus we will drop some of these variables.

HP.num = HP.num %>% dplyr::select(-c(BsmtFinSF1, BsmtFinSF2, X1stFlrSF)) #Remove the variables with perfect correlation.
model.num = lm(log(SalePrice) ~ ., data = HP.num) #Fit all the integer variables into a model.
summary(model.num)$adj.r.squared #Looking at the summary statistics of the model.
# We see that the adj.r.squared value increased from 0.8651 but still rounds to 8651.

alias(model.num) #Verify that we do not have any perfect correlations between predictor variables.


# Let's look at a correlation table for all the variables
cormat = round(cor_matrix(HP.num),3) #Get the correlation between each variable.
cormatdf = data.frame(melt(cormat)) #store the correlation matrix as a dataframe.
cormatdfDesc = arrange(cormatdf, desc(value)) #Arrange the correlation values in descending order so we can see which variables are most strongly correlated.
cormatdfDesc %>% filter(value != 1.000) #Call the arranged correlation data frame with perfect correlations removed.

# Map the correlation data frame for further analysis
cormap = cormatdfDesc %>% ggplot(aes(Var2, Var1, fill = value)) + geom_tile(color = "white") +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson Correlation") + theme_economist() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  guides(fill = guide_colorbar(barwidth = 12, barheight = 1, 
                               title.position = "top", title.hjust = 0.5))
cormap #Call the map to view it.

# We see that there is a strong correlation between GarageArea and GarageCars. 
# There is also a strong correlation between GrLivArea and TotRmsAbvGrd.

# Let's test to see the effects of dropping any of those variables.
HP.num = HP.num %>% dplyr::select(-c(GarageArea)) #Drop the GarageArea variable.
model.num = lm(log(SalePrice) ~ ., data = HP.num) #Fit all the integer variables into a model.
summary(model.num)$adj.r.squared #See adj.r.squared. 
# We see that adj.r.squared Increased from 0.8652 but still rounds to 0.8652.

HP.num = HP.num %>% dplyr::select(-c(GarageCars)) #Drop the GarageCars variable.
model.num = lm(log(SalePrice) ~ ., data = HP.num) 
summary(model.num)$adj.r.squared 
# We see that adj.r.squared dropped from 0.8651 to 0.8623.

HP.num = HP.num %>% dplyr::select(-c(TotRmsAbvGrd)) #Drop the TotRmsAbvGrd variable.
model.num = lm(log(SalePrice) ~ ., data = HP.num) 
summary(model.num)$adj.r.squared 
# We see that adj.r.squared dropped from 0.8651 to 0.8642.

HP.num = HP.num %>% dplyr::select(-c(GrLivArea)) #Drop the GrLivArea variable.
model.num = lm(log(SalePrice) ~ ., data = HP.num) 
summary(model.num)$adj.r.squared 
# We see that adj.r.squared dropped from 0.8650 to 0.8568.

# We see that dropping the variables generally reduces adj.r.squared, so we will keep them.
#To be sure there is minimal co-linearity between the predictor variables.
ols_vif_tol(model.num)  
# The Variance Inflection Factors seem to be low (< 10) so we can proceed with making the changes to our training dataset.


HPtrain_clean = HPtrain.clean %>% 
  dplyr::select(-c(BsmtFinSF1, BsmtFinSF2, X1stFlrSF)) #Remove the variables with perfect correlation.
model.num = lm(log(SalePrice) ~ ., data = HPtrain_clean)
summary(model.num)$adj.r.squared




#  SIXTH: OUTLIERS
# Just like in step six, we will be dealing with just the numerical variables.
# First let's look at a plot of our 10 most influential variables.

par(mfrow=c(2,2)) #Put all plots in 2 rows and 3 columns
plot(model.num, bg = 'red', pch=23, id.n = 10) #Plot residuals and Standardized residuals vs fitted values, QQ plot while identifying the 10 most influential points. 

Outliers.cooksd <- data.frame(influence.measures(model.num)$infmat) %>% arrange(desc(cook.d)) #Store the influential measures for potential outliers as a data frame and arrange based on cook's distance in descending order. 
# We are primarily concerned with high cook's distances.
Outliers.cooksd #See the values with the 10 highest cooks distance that we identified in our residual plot.
head(Outliers.cooksd,100) #See the first 100 rows.

ols_test_outlier(model.num, cut_off = 0.05, 20) #See the top influential points using the ols library.


# Now that we have an idea which points are influential, let's begin deleting them.
# We will monitor the VIF after each deletion to make sure they stay under 10.

HP.out = HPtrain_clean

dim(HP.out)

# Remove the top 20 influential points
HP.out = HP.out %>% 
  filter(!(rownames(HP.out) %in% 
             rownames(ols_test_outlier(model.num, cut_off = 0.05, 20))))
Model95 = lm(log(SalePrice) ~ ., data = HP.out)
summary(Model95)
# We see that we are able to increase the adj.r.squared to 0.95.


# Remove the top 550 influential points.
HP.out = HP.out %>%
  filter(!(rownames(HP.out) %in% head(rownames(Outliers.cooksd),450)))
Model99 = lm(log(SalePrice)~., data = HP.out)
summary(Model99)
# We see that we were able to increase the adj.r.squared to 0.99.
# We will try this model along with model95 on our test dataset.

dim(HP.out) #See number of rows.
VIF(Model95)  #View the Variance Inflection.
mse(Model)

# Plot Cook's Distance for the model.
dev.off()
par(mfrow=c(3,1))
plot(cooks.distance(Model99), pch=23, bg='orange', ylab="Cook's distance", 
     main = "Cook's Distance", id.n = 10) #Plot cook's distance to detect outliers
plot(dffits(Model99), pch=23, bg='orange', ylab = 'DFFITS', main = 'DFFITS', id.n = 6) #Plot DFFITS to detect outliers
plot(resid(Model99), pch=23, bg='orange', ylab = 'Residuals', main = 'Residuals', id.n = 6)

#The plots look good. We can proceed to building our final model.
dev.off() #Turn off plot.










HousePrice = merge(HPtrain, HPtest, all = T)
str(HousePrice) #See info on merged dataset

HousePrice = HousePrice %>% mutate_if(is.character, as.factor) #Convert the character variables to factors
str(HousePrice)


HousePrice$Electrical <- replace_na(HousePrice$Electrical, mode_factor(HousePrice$Electrical)) #For Electrical, replace the NAs with the mode.
HousePrice$MasVnrType <- replace_na(HousePrice$MasVnrType, mode_factor(HousePrice$MasVnrType))  #For MasVnrType, replace the NAs with the mode.
HousePrice$GarageYrBlt = replace_na(HousePrice$GarageYrBlt, round(summary(HousePrice$GarageYrBlt)[4],0)) 
#The codes below will replace the NAs with None for the remaining factor variables

for(i in 1:dim(HousePrice)[2]){
  if(is.integer(HousePrice[,i])==T){
    HousePrice[,i] <- replace_na(HousePrice[,i], summary(HousePrice[,i])[4])
  }
}

for(i in 1:dim(HousePrice)[2]){
  if(is.character(HousePrice[,i])==T){
    HousePrice[,i] <- replace_na(as.character(HousePrice[,i]), "None")
  }
  HousePrice = HousePrice %>% mutate_if(is.character, as.factor)
}

gg_miss_var(HousePrice) #Visual plot of missing values per variable

HousePrice = HousePrice %>% dplyr::select(-c(BsmtFinSF1, BsmtFinSF2, X1stFlrSF))

HousePriceTrain = HP[HousePrice$Dataset=="train",] %>% dplyr::select(-Dataset) #Store rows from the original HPtrain dataset in object.
gg_miss_var(HousePriceTrain)

HousePriceTest = HP[HousePrice$Dataset=="test",] %>% dplyr::select(-Dataset)
gg_miss_var(HousePriceTest)

str(HousePriceTrain)
str(HousePrice)











# SEVENTH: FIT MODEL
#RUN THE MODELS THROUGH MACHINE LEARNING
#FORWARD SELECTION MODEL
FWD95.aic = ols_step_forward_aic(Model95, details = T)
FWD95.aic
formula(FWD95.aic$model)

FWD99.aic = ols_step_forward_aic(Model99, details = T)
FWD99.aic
formula(FWD99.aic$model)


dev.off()
plot(FWD95.aic)
plot(FWD99.aic)




#BACKWARD SELECTION MODEL
BWD95.aic = ols_step_backward_aic(Model95, details = T)
BWD95.aic
formula(BWD95.aic$model)

BWD99.aic = ols_step_backward_aic(Model99, details = T)
BWD99.aic
formula(BWD99.aic$model)

dev.off()
plot(BWD95.aic)




#STEPWISE SELECTION MODEL
STP95.aic = ols_step_both_aic(Model95, details = T)
STP95.aic
formula(STP95.aic$model)

STP99.aic = ols_step_both_aic(Model99, details = T)
STP99.aic
formula(STP99.aic$model)

dev.off()
plot(STP95.aic)



#Final Model 
Final = lm(log(SalePrice) ~ MSZoning + LotFrontage + LotArea + Street + 
             LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
             BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
             RoofStyle + RoofMatl + Exterior1st + MasVnrType + Foundation + 
             BsmtQual + BsmtCond + BsmtExposure + BsmtUnfSF + TotalBsmtSF + 
             Heating + HeatingQC + CentralAir + LowQualFinSF + GrLivArea + 
             BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
             Functional + FireplaceQu + GarageCars + GarageArea + GarageQual + 
             GarageCond + PavedDrive + WoodDeckSF + EnclosedPorch + X3SsnPorch + 
             ScreenPorch + PoolArea + PoolQC + SaleCondition, data = HP.out)

MODEL = lm(log(SalePrice) ~ MSZoning + LotFrontage + LotArea + Street + 
             LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
             BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
             RoofStyle + RoofMatl + Exterior1st + MasVnrType + Foundation + 
             BsmtQual + BsmtCond + BsmtExposure + BsmtUnfSF + TotalBsmtSF + 
             Heating + HeatingQC + CentralAir + LowQualFinSF + GrLivArea + 
             BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
             Functional + FireplaceQu + GarageCars + GarageArea + GarageQual + 
             GarageCond + PavedDrive + WoodDeckSF + EnclosedPorch + X3SsnPorch + 
             ScreenPorch + PoolArea + PoolQC + SaleCondition, data = HousePriceTrain)





#PREDICT
predict(MODEL, HousePriceTest)

levels(HPtest.clean$MSZoning)
levels(HP.out$MSZoning)

levels(as.factor(HPtest.clean$MSZoning))
levels(as.factor(HP.out$MSZoning))
