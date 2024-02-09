
#1. Set your working directory.
getwd()
setwd("/Users/kevin-kindlerhotmail.com/Documents/uzh/FS24/R/day5/project")

#2. Install (if necessary) and load all necessary packages.
library(data.table)
library(lubridate)
library(myRFMpackage)
library(profvis)
library(microbenchmark)
library(fastmatch)
library(compiler)
library(doParallel)
library(foreach)

#3. Load the files data_customer.csv and data_personal.csv using the fread()function.
data_customer <- fread("data_customer.csv")
data_personal <- fread("data_personal.csv")

#2.1. Merge the two data tables on the column CustomerId.
data <- merge(data_customer, data_personal, by = "CustomerId")

#2.2. Set the following columns to factors: Exited, Gender. Hint: use the function as.factor().
data$Exited <- as.factor(data$Exited)
data$Gender <- as.factor(data$Gender)

#2.3. Make a quick check on the data, that everything is as expected. Hint: use the functions str() and summary().
str(data)
summary(data)

#3.1 1. Create a model for churn probability using logistic regression. The outcome variable is Exit (showing if a customer churned or not). 
# Use as predictors the following variables: CreditScore, Gender, Age, Tenure, Balance, NumOfProducts, HasCrCard, IsActiveMember, EstimatedSalary. 
# Hint: use the function glm() with the argument family="binomial".
# Assuming 'data' is the data frame containing the relevant columns
model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, 
             data = data, 
             family = "binomial")

#summary(model)

#3.2. Predict the churn probability for each customer and add this as a new column to your table. 
# Hint: use the following function: predict(object, newdata, type="response"). 
# Object is your model for churn probability from the previous step, newdata is your merged data from step 2.1.
data$churn_prob <- predict(model, data, type = "response")

#3.3. Which are the customer with the highest and the lowest churn probability?
data[which.max(data$churn_prob),]
data[which.min(data$churn_prob),]

# 3.4. Compute the average churn probability for men and women.
average_churn_prob <- aggregate(churn_prob ~ Gender, data = data, FUN = mean)
print(average_churn_prob)

# 4.1. Create a function that takes as input the dataset and a customer id 
# and returns the churn probability for that customer id only.
# 4.2. 2. The function should check if the customer id provided exists in the dataset and throw an error otherwise.
churn_prob_for_customer <- function(data, customer_id) {
  if (!customer_id %in% data$CustomerId) {
    stop("Customer ID not found")
  }
  
  data$Exited <- as.factor(data$Exited)
  data$Gender <- as.factor(data$Gender)
  model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
               data = data,
               family = "binomial")
  data$churn_prob <- predict(model, data, type = "response")
  
  # Extract churn_prob for the specified customer_id
  churn_prob <- data[data$CustomerId == customer_id, "churn_prob"]
  return(churn_prob)
}

# test if the function works
churn_prob_for_customer(data, 15653251)

