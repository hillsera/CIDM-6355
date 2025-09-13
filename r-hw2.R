# define and choose training and prediction datasets
Lab5Train<-read.csv(file.choose(), header = T, stringsAsFactors = T)
Lab5Predict <- read.csv(file.choose(), header = T, stringsAsFactors = T)
# view column names and dimensions of the training dataset (please notice that dash is changed to dot in R)
names(Lab5Train)
dim(Lab5Train)
# view the summary statistics of the training dataset
summary(Lab5Train)
# install package e1071
install.packages("e1071")
# invoke the library e1071
library(e1071)
# build a NB model using naiveBayes function and including all attributes
Lab5NB <- naiveBayes(education.spending ~., data = Lab5Train)
# view the NB model generated and conditional probabilities
Lab5NB
# apply the NB model to the prediction dataset
Lab5Score <- predict(Lab5NB, Lab5Predict)
# view the prediction result
Lab5Score
# show the summary statistics of Lab5Score
summary(Lab5Score)
#the following three rows of script help us write the prediction value of education.spending to the prediction dataset
Score <- data.frame(Lab5Score)
Lab5Predict$education.spending <- Score$Lab5Score
Lab5Predict
# write results to csv file
write.csv(Lab5Predict, "~/Documents/r-predict-results.csv", row.names = FALSE)
# Logistic Regression Classifier
# change the value "abstain" to "n" to make the variable binomial
Lab5Train$education.spending[Lab5Train$education.spending == "abstain"] <- "n"
# develop a logistic regression model using glm function
LogModel <- glm(education.spending ~., family = "binomial", data = Lab5Train)
# view the logistic regression model
summary(LogModel)
# use the LR model to make prediction
Lab5ScoreLR <- predict(LogModel, Lab5Predict, type = "response")
# view the prediction; round all those predicted probabilities to the third decimal place
round(Lab5ScoreLR, 3)
# check if each predicted probability is greater than 0.5 (i.e., with y as the predicted class)
Lab5ScoreLR > 0.5
# count how many of them are predicted as y (i.e., probability greater than 0.5)
sum(Lab5ScoreLR > 0.5)
  