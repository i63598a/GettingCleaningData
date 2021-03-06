# GettingCleaningData
This repo contains files for final course project

---
title: "Read_me"
author: "i63598"
date: "February 27, 2016"
output: html_document
---

# GETTING AND CLEANING DATA

### This is a repo to hold the materials required for the final project
---

## Files in this project
* Read_me.md
* Code_book.md
* run_analysis.R

---

## Getting started
Have a recent version of R installed, load the script, run_analysis.R and execute
The script downloads the packages needed
The output data sets are:
* dtFinal
* tidyData

The tidy data is also exported to a text file, tidy_data.txt

---

## What is the project about?
For this final course project we are demonstrating our ability to create a tidy data set.  A tidy data set exhibits these main features:
1. Each variable you measure should be in exactly one column
2. Each different observation of that variable should be in a different row (e.g. #Tweets -> column, per person -> rows)
3. There should be one table for each kind of variable (e.g. Twitter, Facebook)
4. If you have multiple tables there should be a column in the tables that allows them to be linked up

The data we are working with has been collected from the accelerometers from the Samsung Galaxy S smartphone.  The experiments have been carried out with a group of 30 volunteers. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, the study captured several readings.  It is these readings that can be found in the raw files called "X_train.txt" and "X_test.txt". The file called "features.txt" provides the labels for all the readings. For full documentation on the data set, please see the README.txt file found here:  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

---

## Description of the script, run_analysis.R
The script does the following things:
1. Takes care of some preliminaries, like installing the necessary packages, setting the working directory as the primary input path
2. Establishes a path to the URL where the project data lives, downloads the data file, which is zipped.  The time downloaded is recorded in dateDownloaded
3. Unzips the data into a sub-directory called "UCI HAR Dataset"
4. Imports the following data tables in for use in the project:
  a. 'activity_labels.txt': Links the class labels with their activity name.
  b. 'train/X_train.txt': Training set.
  c. 'train/y_train.txt': Training labels.
  d. 'test/X_test.txt': Test set.
  e. 'test/y_test.txt': Test labels.
  f. 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
  g. 'features.txt': Contains the variable names for all the columns in the test and train data
5. Views some parts of the various raw data tables that were imported, takes note of features of the data by using str(); this is to aid in the merging that follows
6. Merge the test and train data, bring in the activity labels and features labels
7. Only the readings labeled with *mean() or *std() are retained, as per the instructions.  Note that some of the readings may also contain the word "mean", but they have not been retained.
8. Makes the labels more readable
9. Creates the tidy data set and exports it to a file
10. Finally, recognizing that others will be peer reviewing my work, I've created a small script to (hopefully) assist you importing the tidy data back in to your R environment

## SOME NOTES ON THE DATA WE ARE USING
#### Use of this dataset in publications must be acknowledged by referencing the following publication [1]:
#### [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
#### Note: For the purposes of this project the data inside the Inertial Signals folder will be ignored

