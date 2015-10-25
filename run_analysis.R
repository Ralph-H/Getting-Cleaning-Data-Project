############################################################################# 
# File name: run_analysis.R                                                 #
# Author: Ralph Hurtado                                                     #
# Last modified: 10/25/2015                                                 #           #                                                                           #
# Purpose: Project for Coursera Getting & Cleaning Data                     #
# Inputs:  data files from the UCI HAR Dataset: X_test.txt, y_test.txt,     #
#          X_train.txt, y_train.txt, features.txt, activity_lablest.txt,    #
#          subject_train.txt, subject_test.txt                              #
# Outputs: tidy_averages.txt                                                #
#############################################################################

setwd("~/R/coursera/Getting&CleaningData/Project/UCIHARDataset/")

# Get test data
X_testData <- read.table("test/X_test.txt", colClasses = rep("numeric", 561))
y_testData <- read.table("test/y_test.txt", colClasses = "integer")

# Get training data
X_trainData <- read.table("train/X_train.txt", colClasses = rep("numeric", 561))
y_trainData <- read.table("train/y_train.txt", colClasses = "integer")

# Get features
features <- read.delim("features.txt", header = F, fill = F, stringsAsFactors = F)

# Remove number from the beginning of each feature
nums <- 1:max(dim(features))
require(stringr)
featuresFixed <- str_replace(string = features$V1, pattern = as.character(nums), replacement = "")
# Remove white space from the beginning of each feature
featuresFixed <- str_replace(string = featuresFixed, pattern = " ", replacement = "")

# Get activity lables
actLabels <- read.table("activity_labels.txt", stringsAsFactors = FALSE)

y_testVals <- y_testData
y_trainVals <- y_trainData
require(plyr)
y_testData$V1 <- mapvalues(y_testData$V1, from = actLabels$V1, to = actLabels$V2)
y_trainData$V1 <- mapvalues(y_trainData$V1, from = actLabels$V1, to = actLabels$V2)

names(X_testData) <- featuresFixed
names(X_trainData) <- featuresFixed

trainID <- read.table("train/subject_train.txt")
testID <- read.table("test/subject_test.txt")

# Merge data
mrgTrain <- cbind(trainID, y_trainData, y_trainVals, X_trainData)
mrgTest <- cbind(testID, y_testData, y_testVals, X_testData)
mrgFinal <- rbind(mrgTrain, mrgTest)

# Give the first column a better name
names(mrgFinal)[1] <- "SubjectID"

# Find all columns with mean or std 
goodColumns <- (str_detect(string = names(mrgFinal), pattern = fixed("mean", ignore_case = TRUE)) == 1) | (str_detect(string = names(mrgFinal), pattern = fixed("std", ignore_case = TRUE)) == 1)
 
# Create the tidy data frame       
 tidyDf <- cbind(mrgFinal[,1:3], mrgFinal[,goodColumns == TRUE])
 names(tidyDf)[1] <- "subjectID"
 names(tidyDf)[2] <- "activityName"
 names(tidyDf)[3] <- "activityCode"
 
avgs <- data.frame(matrix(nrow = 180, ncol = 86))
subjectID <- vector(length = 180)
activity <- vector(length = 180)
for (i in 1:max(tidyDf$subjectID))
{
        for (j in 1:max(tidyDf$activityCode))
        {
                temp1 <- subset(tidyDf, tidyDf$subjectID == i)
                temp2 <- subset(temp1, temp1$activityCode == j)
                avg <- colMeans(temp2[,4:ncol(temp2)])
                avgs[j+(i-1)*6, ] <- rbind(avg)
                subjectID[j+(i-1)*6] <- i
                activity[j+(i-1)*6] <- j
                
        }
}
names(avgs) <- names(tidyDf)[4:89]
final <- cbind(subjectID, activity, avgs)
final$activity <- as.factor(actLabels$V2[final$activity])

# Output
write.table(final, file = "tidy_averages.txt", row.names = FALSE)