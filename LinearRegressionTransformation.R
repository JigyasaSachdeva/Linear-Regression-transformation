#IDS 575- Assignmnet 1
#Submitted by: Jigyasa Sachdeva
#UIN- 664791188
#Email: jsachd3@uic.edu


#Problem 4- Linear Regression

#Installing package: ISLR
install.packages("ISLR")
#Loading the library to use the datasets
library(ISLR)
#Loading data (shows promise in the toolbar)
data("Auto")

#Viewing the first 10 examples using the head function 
head(Auto, 10)
#There are 9 attributes (Including the name attribute): these could be used as predictor or response




#(a)

#(1)
dim(Auto)
#[1] 392   9
#Number of training examples are 392
#m = 392
#Number of features excluding 'name' are 8
#n = 8

#(2)
#Thinking of this as a data matrix: there are 8 features and 392 observations (m>>n)
#This is a tall/skinny matrix because it is overdetermined
#The number of equations are more than the unknonwns to be determined




#(b)
#Loading library dplys to use pipeline and select function
library(dplyr)
#Creating a dataset from Auto containing all numeric attributes
#Dropping name
num_data <- Auto %>% select(-name)

#(1)
#Loading corrplot library
library(corrplot)
#Calculation correlation of each variable with another in the numeric dataset
correlation <- cor(num_data)
#Computing correlation
correlation

#Visualizing correlation
#Creating a correlation plot
corrplot(correlation, method="circle", type="upper", order = "hclust")

#(2)
#Interpreting correlation: Done in the assignment




#(c)
#Creating a model with a built in function 'lm' 
#The formula of linear model is: lm(respondent~predictors, data)
mod <- lm(mpg~., data=num_data)
#Summary of the model shows coefficients, p-calues, intercepts, Adjusted R square and other statistics
summary(mod)
#Interpreting the results: done in the assignment



#(d)
#Dividing the plotting space into 4 equal spaces for 4 plots to be plotted
par(mfrow = c(2,2))
#Plotting the residual plots
plot(mod)
#Interpretation in the assignment



#(e)

#Loading library dplyr to use pipeline and select function
library(dplyr)
#Removing the variable mpg from the numeric data to transform the other variables
data_wo_mpg <- num_data %>% select(-mpg)


#log e xj
#Applying fucntion log e to all the values unless a value is 0: then a null value is put in the place
logefun = function(x) ifelse(x!=0, log(x), NA)
#Creating a data frame with all predictors transformed to respective logarithmic values
logedata <- as.data.frame(apply(data_wo_mpg, 2, logefun))
#This data is modelled with the same non-transformed target variable
mod_loge <- lm(num_data$mpg~., data=logedata)
#Summary of the variable is seen
summary(mod_loge)
#For removing the previous plot:
dev.off()
#Residual plot for the new model
par(mfrow=c(2,2))
plot(mod_loge)

#sqrt xj
#Repeating the same steps for a square root transformation of predictors
sqrtfun = function(x) ifelse(x>=0, sqrt(x), NA)
sqrtdata <- as.data.frame(apply(data_wo_mpg, 2, sqrtfun))
mod_sqrt <- lm(num_data$mpg~., data=sqrtdata)
summary(mod_sqrt)
dev.off()
par(mfrow=c(2,2))
plot(mod_sqrt)

#square xj
#Repeating the same steps for a square transformation of predictors
sqrfun = function(x) x*x
sqrdata <- as.data.frame(apply(data_wo_mpg, 2, sqrfun))
mod_sqr <- lm(num_data$mpg~., data=sqrdata)
summary(mod_sqr)
dev.off()
par(mfrow=c(2,2))
plot(mod_sqr)

#Interpretation in the assignment document




