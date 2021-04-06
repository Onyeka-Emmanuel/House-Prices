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


#Import the training and test data
HP_train = read.csv(file.choose(), header = T)
HP_test = read.csv(file.choose(), header = T)

head(HP_train) #Check the first few rows of HP_train to verify data imported correctly
head(HP_test) #Check the first few rows of HP_test to verify data imported correctly

str(HP_train) #See the variable names and their class


#ANALYSIS 1
#Filter the three variables need for this analysis and store as object
HP_train_mod = HP_train %>% 
  filter(Neighborhood %in% c("NAmes", "Edwards", "BrkSide")) %>%
  select(SalePrice, GrLivArea, Neighborhood)

HP_train_mod$Neighborhood = factor(HP_train_mod$Neighborhood) #Convert Neighborhood variable to factor with 3 levels 

head(HP_train_mod) #Check the first few rows to verify data is filtered correctly
str(HP_train_mod) #Check that Neighborhood is now a factor with 3 levels

gg_miss_var(HP_train_mod) #Check for missing values. There appears to be none.

# Scatter plot of Living Area vs SalePrice to check for linearity.
HP_train_mod %>% ggplot(aes(x = GrLivArea, y = SalePrice)) + 
  geom_point() + geom_smooth(method = lm) + 
  labs(title = "Sale Price Vs Living Area", x = "Living Area", y = "Sale Price")

ggplotly(HP_train_out %>% ggplot(aes(x = GrLivArea, y = SalePrice, color = Neighborhood)) + 
  geom_point() + geom_smooth(method = lm) + 
  labs(title = "Sale Price Vs Living Area and Neighborhood", x = "Living Area", y = "Sale Price"))



# Diagnostic plots for basic model
par(mfrow=c(2,3)) #Put all plots in 2 rows and 3 columns
plot(model, bg = 'red', pch=23) #Plot residuas and Standardized residuals vs fitted values, QQ plot 
plot(cooks.distance(model), pch=23, bg='orange', ylab="Cook's distance", 
     main = "Cook's Distance") #Plot cook's distance to detect outliers
plot(dffits(model), pch=23, bg='orange', ylab = 'DFFITS', main = 'DFFITS') #Plot DFFITS to detect outliers

# We see that there are some outliers in the data set. We will now identify thoses outliers and see how influencial they are



#Check for outliers with Cook's Distance
Outliers <- HP_train_mod[which(cooks.distance(model) > .05),] #View values for rows with a high cook's distance. This shows rows that could be outliers.
Outliers #Call the rows

Outliers.cooksd <- data.frame(influence.measures(model)$infmat) %>% arrange(desc(cook.d)) #Store the influential measures for potential outliers as a dataframe and arrange based on cook's distance in descending order. We are primarily concerned with high cook's distances.
head(Outliers.cooksd) #Call the measues dataframe


#check how much of Sales Price is explained by Living Area. We are looking at the Adjusted R-squared
model = lm(SalePrice~GrLivArea, data = HP_train_mod) # Our basic model with no rows removed so all outliers are in data set.
summary(model) #See summary statistics of the model. We will be comparing the adjusted R-squared of the models with removed outliers. Higher is better.

HP_train_out = HP_train_mod[-339,] #Removing row 339 only from our data set.
modelout = lm(SalePrice~GrLivArea, data = HP_train_out) #Our regression model
summary(modelout) #See summary statistics of the model. Adjusted R-squared increases so more is explained

HP_train_out2 = HP_train_mod[-c(339,131),] #Removing row 339 only from our data set.
modelout2 = lm(SalePrice~GrLivArea, data = HP_train_out2)
summary(modelout2)

HP_train_out3 = HP_train_mod[-c(339,131,169),] #Removing row 339, 131,and 169 only from our data
modelout3 = lm(SalePrice~GrLivArea, data = HP_train_out3) #Our regression model
summary(modelout3) #See summary statistics of the model. Adjusted R-squared reduces so less is explained. Thus, we will keep row 169 in our data set.

HP_train_out4 = HP_train_mod[-c(339,131,190),] #Removing row 339, 131, and 190 only from our data.
modelout4 = lm(SalePrice~GrLivArea, data = HP_train_out4) #Our regression model
summary(modelout4) #See summary statistics of the model. Adjusted R-squared increases so more is explained

HP_train_out5 = HP_train_mod[-c(339,131,190,372),] #Removing row 339, 131, 190, and 372 only from our data
modelout5 = lm(SalePrice~GrLivArea, data = HP_train_out5) #Our regression model
summary(modelout5) #See summary statistics of the model. Adjusted R-squared reduces so less is explained. We will keep row 372 in our data set.

# It seems modelout4 gives us the highest percentage of Sale Price explained by Living Area (Adj R-squared = 0.4676)
# As a result, we will drop rows 339, 131, and 190 as the rest seem to be adding value to our model while these rows are not.




# Fit full regression model with influential outliers removed
modelfull <- lm(SalePrice ~ relevel(Neighborhood, ref = "BrkSide") + GrLivArea, data = HP_train_out4) #Full regression model
summary(modelfull) #View summary statisctics for the full model. We are paying attention to the adjusted R-squared.
confint(modelfull) #View the confidence intervals

summary(modelfull)$coefficients
# Fit Reduced regression model with influential outliers removed
modelreduced <- lm(SalePrice ~ relevel(Neighborhood, ref = "BrkSide") * GrLivArea, data = HP_train_out4) #Reduced regression model
summary(modelreduced) #View summary statisctics for the reduced model. The adjusted R-squred increased from what it was in the full model. Thus this is a better model as a higher percentage of the Sale Price is explained by the Living Area and Neighborhood (Adj R-squared = .5253).
confint(modelreduced) #View the confidence intervals 




# The relationship of Sale Price to square footage of living area can be derived from the coefficients below
summary(modelreduced)$coefficients #View intercept and slopes


