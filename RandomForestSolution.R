library(caTools)
library(randomForest)
library(pROC)

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

# Creating a data set without index variable
data.new <- subset(DataFrame, select = c(-1))

# Date to numeric
data.new$F15 <- as.numeric(as.Date(data.new$F15, format='%m/%d/%Y'))
data.new$F16 <- as.numeric(as.Date(data.new$F16, format='%m/%d/%Y'))

# To check unique values there are for each column:
sapply(data.new, function(x) length(unique(x)))

# Seems like variables F17, F18, C are categorial variables
data.new$C <- as.factor(data.new$C)
data.new$F17 <- as.factor(data.new$F17)
data.new$F18 <- as.factor(data.new$F18)

# Check out the structure of the date set
str(data.new)

# Model fitting
# Split the data into two chunks: training and testing set
# The training set will be used to fit our model which we will be testing over the testing set
ind <- sample.split(Y = data.new$C, SplitRatio = 0.7)
trainDF <- data.new[ind,]
testDF <- data.new[!ind,]

# Fitting the random forest
# mtry = number of variables selected at each split
# mtry (categorical) = floor(sqrt(no. of independent variables))
modelRandom <- randomForest(C ~ ., data = trainDF, mtry = 4, ntree = 500)
modelRandom

# Plotting the importance of each variable
# Higher the value of mean decrease accuracy,
# higher the importance of the variable in the model
importance(modelRandom)
varImpPlot(modelRandom)

# Assessing the predictive ability of the model
fitted.results <- predict(modelRandom,newdata=subset(testDF),type='class')
t <- table(predictions = fitted.results, actual = testDF$C)
t

# Calculating accuracy
Accuracy <- sum(diag(t))/sum(t)
Accuracy

# Finding best mtry
bestmtry <- tuneRF(trainDF, trainDF$C, ntreeTry=200, stepFactor=1, improve=0.01, trace=T, plot=T)
bestmtry

# Plot the ROC curve and calculate the AUC (area under the curve) which are typical performance measurements for a binary classifier.
pwp <- predict(modelRandom, testDF, type = 'prob')
plot(roc(testDF$C, pwp[,2]))
auc <- auc(testDF$C, pwp[,2])
auc