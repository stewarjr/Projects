library(foreign)
library(VIM)

fileloc1 <- "C:/Users/Jordan/Documents/R/LLCP2013.xpt"
fileloc2 <- "C:/Users/Jordan/Documents/R/LLCP2011.xpt"
fileloc3 <- "C:/Users/Jordan/Documents/R/LLCP2012.xpt"

### Creating the test set (BRFSS 2013 for only MI)
brfss2013 <- read.xport(fileloc1)

mi.only <- brfss2013[brfss2013$X_STATE == 26, ]
table(mi.only$FLUSHOT6, exclude=NULL)
  # 1 = Yes; 2 = No; 7 = Don't know; 9 = Refused
mi.ymiss <- mi.only[!is.na(mi.im$FLUSHOT6), ] # Removed 655 missing values
mi.2013.final <- mi.ymiss[mi.ymiss$FLUSHOT6 == 1 | mi.ymiss$FLUSHOT6 == 2, ]
  # "Don't know" and "Refused" removed (combined n = 22)
write.csv(mi.2013.final, "mi.2013.csv")

### Creating merged training set (BRFSS 2011 and 2012 for only MI)
brfss2011 <- read.xport(fileloc2)
brfss2012 <- read.xport(fileloc3)

# Removed 7 = "Don't know" and 9 = "Refused" (combined n=41)
  # 510 NAs may be imputated
mi.2011 <- brfss2011[brfss2011$X_STATE == 26 &
                       !(brfss2011$FLUSHOT5 %in% c(7, 9)), ]
mi.2012 <- brfss2012[brfss2012$X_STATE == 26 &
                       !(brfss2012$FLUSHOT5 %in% c(7, 9)), ]

# Combined 2011 and 2012 data for only common columns
commonCol <- colnames(mi.2011) %in% colnames(mi.2012)
commonCols <- colnames(mi.2012) %in% colnames(mi.2011)
  # Columns (Variable names) in common
mi.train <- rbind(mi.2011[ ,commonCol], mi.2012[ ,commonCols])

# Validating that uncommon columns are indeed not in common
colCompare <- rep("ZZZ", length(colnames(mi.2011[ ,!commonCol])))
colCompare[1:63] <- colnames(mi.2012[ ,!commonCols])

colVerify <- data.frame("2011"=sort(colnames(mi.2011[ ,!commonCol])),
                        "2012"=sort(colCompare))
colVerify2 <- data.frame("X2011"=colVerify[-(1:32), 1],
                         "X2012"=colVerify[-(127:158), 2])

# Variables are likely coded the same, but slight name differences
missed2011 <- colVerify2[c(1, 2, 37, 56:59, 80, 81, 94:103), 1]
missed2012 <- colVerify2[c(2, 3, 22, 36:39, 41:52), 2]
missedCommon <- data.frame(missed2011, missed2012)

getridof <- missedCommon[-c(2:19), ]
  # 2: CHCVIS - Questions are now different
  # 3:HIVRDTST - has a better variable HIVTST6 that is already common
  # 4/5:PCPSA(DIS/ADV) - low response and two sides of a question
  # 6:PCPSAREC - different questions and low response rate
  # 7:PCPSARS - low response for reason why prostate test was taken
  # 8/9:SSB(FRUT/SUGAR) - low response pertaining fruit/sugary drinks
  # 10:VICTRCT - slightly different questions and low response (cataracts)
  # 11:VIDFCLT - low response about recognizing friends from a distance
  # 12:VIEYEXM - low response about if eye dilation was used in exam
  # 13:VIGLUMA - low response about if you've been told you had glaucoma
  # 14:VIINSUR - low response asking about eye insurance
  # 15:VIMACDG - low response about if you've been told you had mac degen.
  # 16:VINOCRE - low response about why you haven't had eye care
  # 17:VIPRFVS -  low response about when you last had eyes checked
  # 18:VIREDIF - low response about difficulty reading text
  # 19:WHRTST -  low response asking where your last HIV test was

# Adding CHCCOPD1 variable to training set
names(mi.2011)[names(mi.2011) == "CHCCOPD"] <- "CHCCOPD1"
commonCol <- colnames(mi.2011) %in% colnames(mi.2012)
commonCols <- colnames(mi.2012) %in% colnames(mi.2011)
mi.train <- rbind(mi.2011[ ,commonCol], mi.2012[ ,commonCols])

write.csv(mi.train, "mi.train.csv")

### Variable selection of the training set

mi.train <- read.csv("C:/Users/Jordan/R/mi.train.csv")

# If a variable is missing about half its values, then it was removed
missIndex <- NULL
for (i in 1:dim(mi.train)[2]) {
  if (sum(is.na(mi.train[ ,i])) > 10000) {
    missIndex[i] <- FALSE
  } else {
    missIndex[i] <- TRUE
  }
}

sum(missIndex) # 163 variables missing about half of its values
  # With NA > 5000, total variables = 132
  # With NA > 13000, total variables = 151

miTrain <- mi.train[ ,missIndex]

# Getting rid of X variable generated from csv file creation
names(miTrain)[1] <- "ID"
miTrain$ID <- 1:dim(miTrain)[1]
row.names(miTrain) <- miTrain$ID

# Variables associated with record identification removed
recordI <- c(2:11, 13, 15:22)
miTrain <- miTrain[ ,-recordI]

# Variables in common with training and test set
  # Getting rid of X variale generated from csv file creation
mi.2013 <- read.csv("C:/Users/Jordan/Documents/R/mi.2013.csv")
names(mi.2013)[1] <- "ID"
mi.2013$ID <- 21508:33589
row.names(mi.2013) <- mi.2013$ID

commonCol <- colnames(miTrain) %in% colnames(mi.2013)
commonCols <- colnames(mi.2013) %in% colnames(miTrain)
mi.Train <- miTrain[ ,commonCol]
mi.test <- mi.2013[ ,commonCols]

# Checking if different named variables are indeed different
validCheck <- colnames(miTrain[ ,!commonCol])
validCheck2 <- colnames(mi.2013[ ,!commonCols])

datacheck <- data.frame(test=validCheck2, train=rep(NA, 245),
                        trainb=rep(NA, 245))
datacheck$train[1:30] <- validCheck
datacheck$trainb[1:116] <- colnames(miTrain)
  # ******* FLUSHOT5 = FLUSHOT6
  # EMPLOY = EMPLOY1

# Renaming training variables to new test variables
names(miTrain)[names(miTrain) == "FLUSHOT5"] <- "FLUSHOT6"
names(miTrain)[names(miTrain) == "EMPLOY"] <- "EMPLOY1"

# Readjusting for common variables
commonCol <- colnames(miTrain) %in% colnames(mi.2013)
commonCols <- colnames(mi.2013) %in% colnames(miTrain)
mi.Train <- miTrain[ ,commonCol]
mi.test <- mi.2013[ ,commonCols]

exclude <- c(35:37, 46, 48, 49, 51:72, 74:88)
  # Variables 35-37: Phone variables
  # Variable 46: Sloppily coded variable about alcohol
  # Variables 48 - 54: Survery design (stratification) and weighting
  # Variables after 55: Imputed values (phone/race); quantitative -> cat.

train <- mi.training[, -exclude]

# Reduce test set variables as well
commonCol <- colnames(mi.testing) %in% colnames(train)
test <- mi.testing[ ,commonCol]

# Combining data sets to see if everything is consistent
train.index <- dim(train)[1]
combined <- rbind(train, test)

# Removing variables because of awkward coding
combined <- combined[, -c(33, 34)]

# Giving categorical variables a factor structure
for (i in c(3, 7, 10:28, 30:43)) {
  combined[, i] <- factor(combined[, i])
}

# Renaming inconsistent factors (latest year used)
  # Refused and Don't know/Not sure entries labeled as NA
for (i in c(7, 10:28, 30, 31, 33, 35:38, 40, 41)) {
  levels(combined[, i])[levels(combined[, i]) == "9"] <- NA
}

for (i in c(7, 10:13, 14:27, 33, 35:38, 40, 41)) {
  levels(combined[, i])[levels(combined[, i]) == "7"] <- NA
}

levels(combined$DISPCODE)[levels(combined$DISPCODE) == "110"] <- "1100"
levels(combined$DISPCODE)[levels(combined$DISPCODE) == "120"] <- "1200"

levels(combined$INCOME2)[levels(combined$INCOME2) == "77"] <- NA
levels(combined$INCOME2)[levels(combined$INCOME2) == "99"] <- NA

# Quantitative values for Don't know/Not sure and refused are now NAs
  # 88 for "None" also changed to 0
combined$PHYSHLTH[combined$PHYSHLTH == 77] <- NA
combined$PHYSHLTH[combined$PHYSHLTH == 99] <- NA
combined$PHYSHLTH[combined$PHYSHLTH == 88] <- 0

combined$MENTHLTH[combined$MENTHLTH == 77] <- NA
combined$MENTHLTH[combined$MENTHLTH == 99] <- NA
combined$MENTHLTH[combined$MENTHLTH == 88] <- 0

combined$CHILDREN[combined$CHILDREN == 99] <- NA
combined$CHILDREN[combined$CHILDREN == 88] <- 0

# Variable removal, or adjustment, for too few people/category
  # If about less than 5% of the total, if dichotomous, removed
levels(combined$DIABETE3)[levels(combined$DIABETE3) == "2"] <- "1"
levels(combined$DIABETE3)[levels(combined$DIABETE3) == "4"] <- "3"
  # Renaming "3" to "2" to match 1=Yes and 2=No dichotomy
levels(combined$DIABETE3)[levels(combined$DIABETE3) == "3"] <- "2"

limited <- c(11, 16, 23, 26, 38)
  #PERSDOC2, CVDSTRK3, CHCKIDNY, USENOW3, SEATBELT
combined <- combined[, -limited]

# Removing 2 observations since they're from the year 2014
combined <- combined[-c(21944, 29147), ]

# Split back into train and test set
train <- combined[1:train.index, ]
test <- combined[(train.index+1):dim(combined)[1], ]
  # Removing temporary ID
train <- train[, -1]
test <- test[, -1]

# Saving new test and train
write.csv(train, "train.csv")
write.csv(test, "test.csv")

### Exploratory data analysis -- missing data

train$IYEAR <- factor(train$IYEAR)
test$IYEAR <- factor(test$IYEAR)

missno <- NULL
for (i in 1:dim(train)[2]) {
  missno[i] <- sum(is.na(train[, i]))
}
barplot(missno, ylab="Number of Missing Observations",
        names.arg=colnames(train), las=2, cex.names=0.6, col="red",
        ylim=c(0, 3800))

# Discovery about questionable Michigan status
# MSCODE with NA meant GU, PR, VI
  # Count variables (e.g. NUMADULT) had same missing values
train <- train[!is.na(train$MSCODE), ]
test <- test[!is.na(test$MSCODE), ]

# Appears that HIVTST6 is missing too many observations to be useful
train <- train[, -35]
test <- test[, -35]

# Assesing missing data "trends"
matrixplot(train, sortby="GENHLTH")

misstrain <- as.data.frame(abs(is.na(train)))
  # Extract variables that at least have some missing values
missingy <- misstrain[sapply(misstrain, sd) > 0]
cor(missingy)
  # EXERANY2 and (FLUSHOT6, QLACTLM2, USEEQUIP) [0.7001, 0.7149, 0.7503]
  # FLUSHOT6 and USEEQUIP 0.9032
  
### Using trained model on test data set
library(mice)
library(MASS)
library(class)

train <- read.csv("C:/Users/Jordan/Documents/R/train.csv")
test <- read.csv("C:/Users/Jordan/Documents/R/test.csv")


# Data imputation
set.seed(123)

impute <- mice(train, m=1, seed=123)
train_total <- complete(impute, "long")

imputetest <- mice(test, m=1, seed=123)
test_total <- complete(imputetest, "long")

levels(train_total$FLUSHOT6)[1] <- "Yes"
levels(train_total$FLUSHOT6)[2] <- "No"
levels(test_total$FLUSHOT6)[1] <- "Yes"
levels(test_total$FLUSHOT6)[2] <- "No"
train_total <- train_total[, -c(1, 2)]
test_total <- test_total[, -c(1, 2)]
train_total <- train_total[, -c(1, 5)]
test_total <- test_total[, -c(1, 5)]
train_total$FLUSHOT6 <- relevel(train_total$FLUSHOT6, ref="No")

## Multiple Logistic Regression
  # 1 = Yes; 2 = No
glm.fit <- glm(FLUSHOT6 ~ ., family="binomial", data=train_total)
glm.probs <- predict(glm.fit, test_total, type="response")
glm.pred <- rep("No", nrow(test_total))
glm.pred[glm.probs > 0.5] <- "Yes"

datacheck <- data.frame(actual=test_total$FLUSHOT6, probs=glm.probs, pred=glm.pred)

table(glm.pred, test_total$FLUSHOT6)
  # (2340 + 3124)/8402 = 65.03% correct

## Linear Discriminant Analysis
lda.fit <- lda(FLUSHOT6 ~ ., data=train_total)
lda.pred <- predict(lda.fit, test_total)

table(lda.pred$class, test_total$FLUSHOT6)
mean(lda.pred$class == test_total$FLUSHOT6)
  # 65.06% correct

## Quadratic Discriminant Analysis
qda.fit <- qda(FLUSHOT6 ~ ., data=train_total)
  # Cannot fit a QDA because of factor variables being collinear
qda.class <- predict(qda.fit, test_total[, -c(1:3)])

## K-Nearest Neighbors
train.X <- as.matrix(train_total[, -31])
test.X <- as.matrix(test_total[, -31])
train.flustat <- train_total[, 31]

knn.pred <- knn(train.X, test.X, train.flustat, k=5)
table(knn.pred, test_total$FLUSHOT6)
mean(knn.pred == test_total$FLUSHOT6)
  # 58.51% correct

## Classification Tree (rpart)
library(rpart)
library(RColorBrewer)
library(rpart.plot)
library(rattle)

tree.fit <- rpart(FLUSHOT6 ~ ., method="class", data=train_total)
summary(tree.fit)
  # Biggest split on PNEUVAC3
tree.fit2 <- rpart(FLUSHOT6 ~ ., method="class", data=train_total, 
                  control=rpart.control(minsplit=500))
fancyRpartPlot(tree.fit2)

tree.pred <- predict(tree.fit, test_total, type="class")
table(tree.pred, test_total$FLUSHOT6)
mean(tree.pred == test_total$FLUSHOT6)
  # 64.66% correct

## Random Forest for determine important parameters
library(randomForest)
set.seed(123)

rf.imp <- randomForest(FLUSHOT6 ~ ., data=train_total, importance=TRUE)
importance(rf.imp)

## Logistic Regression with "important" parameters
glm.fit2 <- glm(FLUSHOT6 ~ PNEUVAC3 + INCOME2 + EMPLOY1, family="binomial",
                data=train_total)
glm.probs2 <- predict(glm.fit2, test_total, type="response")
glm.pred2 <- rep("No", nrow(test_total))
glm.pred2[glm.probs2 > 0.5] <- "Yes"
table(glm.pred2, test_total$FLUSHOT6)
mean(glm.pred2 == test_total$FLUSHOT6)
  # 64.68% correct