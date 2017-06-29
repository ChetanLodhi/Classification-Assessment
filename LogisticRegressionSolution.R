setwd("D:/R/Projects/People Interactive")

library(caTools)
library(ROCR)

# Setting the seed
set.seed(123)

# Storing the data set named "ClassificationProblem1.txt" into DataFrame
DataFrame <- read.table("ClassificationProblem1.txt", sep="\t", stringsAsFactors = FALSE, header = TRUE)

# Check out the dimension and structure of the data
dim(DataFrame)
str(DataFrame)

# If this data has blanks, convert the blanks to "NA":
DataFrame[DataFrame==""] <- NA

# Checking for missing values "NA":
sapply(DataFrame, function(x) sum(is.na(x)))

# To check unique values there are for each column:
sapply(DataFrame, function(x) length(unique(x)))

# To check correlation convert the chr (F15, F16) to numeric variables
DataFrame$F15 <- as.numeric(as.Date(DataFrame$F15, format='%m/%d/%Y'))
DataFrame$F16 <- as.numeric(as.Date(DataFrame$F16, format='%m/%d/%Y'))
cor(DataFrame)

# Model fitting
# Split the data into two chunks: training and testing set
# The training set will be used to fit our model which we will be testing over the testing set
ind <- sample.split(Y = DataFrame$C, SplitRatio = 0.7)
trainDF <- DataFrame[ind,]
testDF <- DataFrame[!ind,]

# Lets perform a logistic regression
modelLogistic <- glm(C ~ ., family=binomial(link = 'logit'), data= trainDF)

# By using function summary() we obtain the results of our model:
summary(modelLogistic)

# confint function gives confidence intervals for the coefficients
confint(modelLogistic)

# Now we run the anova() function on the model to analyze the table of deviance
anova(modelLogistic, test="Chisq")

# Create a small data set, containing significant variables
train <- subset(trainDF, select = c(3, 5, 18, 19, 20, 21, 22, 23, 24))
modelLogistic <- glm(C ~ ., family=binomial(link = 'logit'), data= train)
summary(modelLogistic)

# Assessing the predictive ability of the model
fit.results <- predict(modelLogistic,newdata=subset(testDF,select=c(3, 5, 18, 19, 20, 21, 22, 23, 24)),type='response')

# Our decision boundary will be 0.5. If P(y=1|X) > 0.5 then y = 1 otherwise y=0
fit.results <- ifelse(fit.results > 0.5, 1, 0)

# Calculating accuracy
P.Error <- mean(fit.results != testDF$C)
print(paste('Accuracy',1 - P.Error))


# Plot the ROC curve and calculate the AUC (area under the curve) which are typical performance measurements for a binary classifier.
p <- predict(modelLogistic, newdata=subset(testDF,select=c(3, 5, 18, 19, 20, 21, 22, 23, 24)), type="response")
pr <- prediction(p, testDF$C)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc