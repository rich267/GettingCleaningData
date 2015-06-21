## About the run_analysis.R script ##

The run_analysis.R script was created for the June 2015 session of **Coursera's Getting and Cleaning Data** course.

## Preconditions ##

You must have the dataset *https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip* unzipped into the same folder containing this script.

## What the script does ##

1. Reads the feature names from features.txt
2. Merges the training and the test data sets to create one data set, by performing the following operations on both sets and then merging them into one set:
  + Loads the subject data from subject_*type*.txt
  + Loads the data from X_*type*.txt
  + Loads the activity codes from y_*type*.txt
  + Ensures the number of items in the subject data is equal to the number of lines in the activity codes, and is also equal to the number of rows in the data
  + Ensures the number of columns in the data is equal to the number of feature names
  + Merges the subject data and activity codes into the data
3. Removes those columns from the data that do **not** correspond to mean or standard deviation values
4. Reads the (English) activity names from activity_labels.txt and merges them into the data set
5. Cleans up and applies camel casing to the column names
6. Creates a new data set that contains the means of the mean and standard deviation measurements, grouped by activity name and subject id
  
  