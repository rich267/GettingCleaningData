
# run_analysis.R
# Course project for Getting and Cleaning Data, June 2015
#
# Assumptions:
# (1) The dataset https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#     has been unzipped into the same folder containing this script
# (2) That unzipped dataset has a folder hierarchy and contents like
#     UCI HAR Dataset
#         activity_labels.txt
#         features.txt
#         test/X_test.txt
#         test/y_test.txt
#         train/X_train.txt
#         train/y_train.txt
#
# Output: A data frame with means of the mean() and std() variables, grouped
# by activity name (laying, sitting, ...) and subject id, in a variable named
# data_step5.
#
# Note: this script starts out by removing all global variables and functions,
# to ensure we start "clean" and there are no ill effects from or dependencies
# on previous runs of this script or from other artifacts left over from other 
# scripts. If you don't want this to happen, comment out the rm command just below.

# start clean!
rm(list=ls())

# Capitalize first letter of word
.simpleCap <- function(word)
{
  paste(toupper(substring(word, 1, 1)), substring(word, 2), sep = "", collapse="")
}

# Split str on dash or paren if found and camel case the str.
camelCase <- function(str)
{  
  if(grepl("-", str))
    splitOn = "-"
  else if(grepl(",", str))
    splitOn = ","
  else
    splitOn = ""
  
  ret = if(splitOn == "")
    str
  else
  {
    parts <- unlist(strsplit(str, splitOn))
  
    tmp <- c(parts[1], sapply(parts[2:length(parts)], .simpleCap))
    paste(tmp, sep="", collapse="")
  }
}

# Read features.txt file and return vector of feature names; omit the leading
# feature number. Preconditions:
# (1) File "features.txt" is in rootDir and has one feature per line
# (2) Each line is of format "<feature number> <feature label>"
# (3) The <feature label> does not have any spaces
getRawFeatures <- function(rootDir)
{
  filePath = paste(rootDir, "features.txt", sep="")
  lines <- readLines(filePath)  

  # lines are line number, space, feature name (w/o spaces)
  # example - "1 tBodyAcc-mean()-X"
  
  # split on space, grab element following the space
  f <- function(x) unlist(strsplit(x, " "))[[2]]
  features <- lapply(lines, f)
}

# Make those feature names pretty! (See comments in body of function)
tidyFeatureNames <- function(data)
{
  features <- names(data)
  
  # clean up feature names
  # (1) convert to Camel Case and remove dashes
  tidyFeatures <- sapply(features, camelCase)
  # (2) remove open parens
  tidyFeatures <- gsub("\\(", "", tidyFeatures)
  # (3) remove close parens
  tidyFeatures <- gsub(")", "", tidyFeatures)
  # (4) remove commas
  tidyFeatures <- gsub(",", "", tidyFeatures)
  # (5) remove duplicate "Body"
  tidyFeatures <- gsub("BodyBody", "Body", tidyFeatures)
  # so "tBodyAcc-mean()-X" becomes "tBodyAccMeanX", "angle(Y,gravityMean)" becomes "angleYGravityMean"
  
  names(data) <- tidyFeatures
  
  data
}

# Return only those features that correspond to mean and std data - those
# with mean() and std() in their names - plus the activity and subject
# features.
getRelevantFeatures <- function(data)
{
  features <- names(data)
  
  # meanFreq() intentionally excluded
  relevant = grepl("mean\\()", features) |
    grepl("std\\()", features) | 
    grepl("^activity", features) |
    grepl("^subject$", features)
}

# Load test or train data and join it with the subject ids and activity ids.
# Apply column names (features) and return as data frame.
loadSet <- function(type, rootDir, features)
{
  path = paste(rootDir, type, "/", sep="")
  
  # in comments below, <type> is either "test" or "train"
  
  # subjects are in subject_<type>.txt
  subjects <- as.numeric(readLines(paste(path, "subject_", type, ".txt", sep="")))
  
  # data is in X_<type>.txt
  data <- read.table(paste(path, "X_", type, ".txt", sep=""))
  
  # activity labels are in y_<type>.txt
  labels <- as.numeric(readLines(paste(path, "y_", type, ".txt", sep="")))
  
  # sanity check - number of subjects should equal number of labels
  # and should equal number of rows in data frame
  numSubjects = length(subjects)
  numLabels = length(labels)
  numDFRows = nrow(data)
  if(! (numSubjects == numLabels & numSubjects == numDFRows))
    stop(sprintf("expected same number of subjects, labels, DF rows; got numSubjects=%d, numLabels=%d, numDFRows=%d", numSubjects, numLabels, numDFRows))

  # sanity check - number of features should equal number of cols in data frame
  numFeatures = length(features)
  numDFCols = ncol(data)
  if(numFeatures != numDFCols)
    stop(sprintf("number of %s features (%d) != number of DF cols (%d)", type, numFeatures, numDFCols))
  
  names(data) <- features
  
  # add subject and activity label columns to data frame
  data$subject <- subjects
  data$activityLbl <- labels
  
  data
}

# Return entire data set - the concatenation of the test and training data.
getMergedDataSet <- function(rootDir, features)
{
  # get data from test set
  testData <- loadSet("test", rootDir, features)
  # get data from training set
  trainData <- loadSet("train", rootDir, features)
  
  # both sets have same column content/ordering, so simply concat them
  rbind(testData, trainData)
}

# Return activity labels as a data frame with columns activityLbl, activityName.
# Preconditions:
# (1) File "activity_labels.txt" is in rootDir and has one label per line
# (2) Each line is of format "<activity number> <activity label>"
# (3) The <activity label> does not have any spaces
getActivityLabels <- function(rootDir)
{
  filePath = paste(rootDir, "activity_labels.txt", sep="")
  labelsRaw <- readLines(filePath)
  numLines <- length(labelsRaw)
  
  # lines are like "2 WALKING_UPSTAIRS"
  # get activity numbers
  labelNum <- lapply(labelsRaw, function(x) strsplit(x, " ")[[1]][1])
  # get activity names
  labelTxtRaw <- lapply(labelsRaw, function(x) strsplit(x, " ")[[1]][2])
  
  # convert underscore to space, capitalize first letter
  firstLetterCap <- function(x)
  {
    str = tolower(gsub("_", " ", x))
    .simpleCap(str)
  }
  # so "WALKING_DOWNSTAIRS" becomes "Walking downstairs"
  labelTxt <- lapply(labelTxtRaw, firstLetterCap)
  
  df <- data.frame(matrix(data=c(labelNum, labelTxt), nrow=numLines, ncol=2))
  names(df) <- c("activityLbl", "activityName")
  df
}

# Union activity labels into data. Key is activityLbl column which must be
# present in both data frames. Preserves row order.
applyActivityLabels <- function(data, labels) 
{
  # add row number
  data$rownum <- 1:nrow(data)
  
  # merge activity labels into data
  labeledData <- merge(data, labels, by="activityLbl")
  
  # preserve ordering on rownum
  labeledData[order(labeledData$rownum), ]
  
  # drop the rownum col we added above
  df = subset(labeledData, select=-c(rownum))
  df$activityName <- unlist(df$activityName)
  
  df
}

# The final step! Take the data and
# (a) Remove the activity name col, since we can't compute the mean of an alpha col
# (b) Compute the means of the (remaining numeric) columns
# (c) Add activity name back in
# (d) Change the names of the cols with the means to reflect their content
# (e) Return data frame with proper column ordering and rows ordered by activity 
#     name and subject
avgByActivityAndSubject <- function(data, labels)
{
  # remove activityName (character) since mean of non-numeric is a bad thing to do :)
  numericData <- subset(data, select=-c(activityName))
  
  activityLbl <- data$activityLbl
  subject <- data$subject
  agg <- aggregate(numericData, by=list(activityLbl, subject), FUN=mean)
  
  # bring activityName back in
  df <- applyActivityLabels(agg, labels)

  makeColNames <- function(word)
  {
    if(grepl("Mean|Std", word))
      paste("MeanOf", toupper(substring(word, 1, 1)), substring(word, 2), sep="") 
    else
      word
  }
  
  # update col names to reflect they are means
  names(df) <- lapply(names(df), makeColNames)
  
  # col projection
  meanCols <- grep("^MeanOf", names(df), value=T)
  colProjection <- c("activityName", "subject", meanCols)
  projection <- df[, colProjection]
  
  # order by activity name, subject
  projection[order(unlist(projection$activityName), projection$subject), ]
}

# data set should be unzipped into this folder, which resides in the same
# parent folder as this script does
rootDir = "UCI HAR Dataset/"

# get raw feature names (names of the vars in the data set)
rawFeatures <- getRawFeatures(rootDir)

# (1) Merge the training and the test sets to create one data set.
data_step1 <- getMergedDataSet(rootDir, rawFeatures)

# (2) Extract only the measurements on the mean and standard deviation for each measurement. 
relevantFeatures <- getRelevantFeatures(data_step1)
data_step2 <- data_step1[, relevantFeatures]

# (3) Use descriptive activity names to name the activities in the data set
activityLabels <- getActivityLabels(rootDir)
data_step3 <- applyActivityLabels(data_step2, activityLabels)

# (4) Appropriately label the data set with descriptive variable names. 
data_step4 <- tidyFeatureNames(data_step3)

# (5) From the data set in step 4, create a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
data_step5 <- avgByActivityAndSubject(data_step4, activityLabels)

# end of script