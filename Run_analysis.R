# run_analysis

####################################################################################################################################################
# This R script accomplishes the following tasks
## Download source data from the URL provided by the project outline, unzip it, load the required text files into R
## Combine the data, clean up the labels and filter out feature results not related to mean and standard deviation values, as per instructions
## Create a tidy data set from the results above, by calculating the average of each variable for each activity and each subject
## Export this table for upload to Coursera; and include a little script to make it easy for the peer reviewer to load it into their R

####################################################################################################################################################
## PRELIMINARIES
### Load the packages required for the analysis

pkgs <- c("plyr", "dplyr", "data.table")
sapply(pkgs, require, character.only=TRUE, quietly=TRUE)

### Tell R where the working folder is
path <- getwd()

## RAW DATA
### Download the data
if(!file.exists("./Dataset")){dir.create("./Dataset")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "Dataset.zip")
download.file(fileurl, f) 
dateDownloaded <- today()

### Unzip the data file, after which the data will be inside a folder called "UCI HAR Dataset"
unzip(f, files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)

### Direct R to the location of the data files and give you a list of the files we are starting with
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)

### Information about the data we are using
#### Note: For the purposes of this project the data inside the Inertial Signals folder will be ignored
#### 'activity_labels.txt': Links the class labels with their activity name.
#### 'train/X_train.txt': Training set.
#### 'train/y_train.txt': Training labels.
#### 'test/X_test.txt': Test set.
#### 'test/y_test.txt': Test labels.
#### 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

## SOME NOTES ON THE DATA WE ARE USING
#### Use of this dataset in publications must be acknowledged by referencing the following publication [1]:
#### [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

#### As seen in the README file for the UCI HAR Dataset, the obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.
#### Therefore our first step will be to recombine the training and test data sets

## IMPORT THE RAW DATA
### Load Test data: subject_test.txt, x_test.txt, y_test.txt
filePath <- file.path(pathIn, "test", "subject_test.txt")
testSub <- read.table(filePath, header=FALSE)
filePath <- file.path(pathIn, "test", "X_test.txt")
testX <-  read.table(filePath, header=FALSE)
filePath <- file.path(pathIn, "test", "y_test.txt")
testY <-  read.table(filePath, header=FALSE)

### Load Train data: subject_train.txt, x_train.txt, y_train.txt
filePath <- file.path(pathIn, "train", "subject_train.txt")
trainSub <- read.table(filePath, header=FALSE)
filePath <- file.path(pathIn, "train", "X_train.txt")
trainX <-  read.table(filePath, header=FALSE)
filePath <- file.path(pathIn, "train", "y_train.txt")
trainY <-  read.table(filePath, header=FALSE)

### Load features data, which contains the variable names for all the columns in the X_test or X_train data.
filePath <- file.path(pathIn, ".", "features.txt")
feat <- read.table(filePath, header=FALSE, stringsAsFactors = FALSE)

### Load activity label data, which contains the descriptive name for each activity ID
filePath <- file.path(pathIn, ".", "activity_labels.txt")
act <- read.table(filePath, header=FALSE)

### View the data
head(trainSub)
head(trainX,1)
head(trainY)
head(feat)
head(act)

## MANIPULATE THE DATA TOWARD TIDINESS
### Change the data frames to data tables   
testSub <- data.table(testSub)
testX <- data.table(testX)
testY <- data.table(testY)
trainSub <- data.table(trainSub)
trainX <- data.table(trainX)
trainY <- data.table(trainY)
feat <- data.table(feat)

### Take note of the shape of each data set
str(testSub) ####2947 obs. of 1 variable, values are integer
str(testX)  ####2947 obs. of 561 variables, values are numeric
str(testY)  ####2947 obs. of 1 variable, values are integer
str(trainSub)  ####7352 obs. of 1 variable, values are integer
str(trainX)  ####7352 obs. of 561 variables, values are numeric
str(trainY)  ####7352 obs. of 1 variable, values are integer
str(feat)  ####561 obs. of 2 variables; first is integer, second is character
str(act)  ####6 obs. of 2 variables; first is integer, second is factor

### Merge the training and test data sets
#### Step 1: Combine the subject ID data
allSubjectIDs <- rbind(testSub, trainSub)
setnames(allSubjectIDs, "V1", "SubjectID")
View(allSub)
#### Step 2: Combine the activity label ID data
allActivityIDs <- rbind(testY, trainY)
setnames(allActivityIDs, "V1", "ActivityID")
View(allActivityIDs)
#### Step 2b: Bring in ActivityName
setnames(act, names(act), c("ActivityID", "ActivityName"))
allActivityIDs <- merge(allActivityIDs, act, by.x = "ActivityID", by.y = "ActivityID", all.x = TRUE)
#### Step 3: Combine the test result data
allTestResults <- rbind(testX, trainX)
head(allTestResults,1)
#### Step 3b:  Bring in FeatureNames
setnames(feat, names(feat), c("FeatureID", "FeatureName"))
setnames(allTestResults, names(allTestResults), feat$FeatureName)
View(head(allTestResults, 1))
#### Step 4:  Merge the allSubjectIDs to allActivityIDs
dt <- cbind(allSubjectIDs, allActivityIDs)
dt <- cbind(dt, allTestResults)
View(head(dt))

### Limit dataset to only columns with mean and stdev
#### For the purpose of the assignment assuming that this means feature names that include mean() or std()
#### Identify features of interest
FeaturesWant <- feat[grepl("mean\\(\\)|std\\(\\)", FeatureName),]
a <- print(FeaturesWant$FeatureName)
#### Filter down dt to just include the features of interest
dtFiltered <- select(dt, SubjectID, ActivityID, ActivityName, one_of(a))

### Making the column names readable
#### Capture current names
origNames <- colnames(dtFiltered)
#### Remove bad bits of feature names
cleanNames <- colnames(dtFiltered)
for (i in 1:length(cleanNames)) 
{
        cleanNames[i] = gsub("\\()","",cleanNames[i])
        cleanNames[i] = gsub("-std","StdDev",cleanNames[i])
        cleanNames[i] = gsub("-mean","Mean",cleanNames[i])
        cleanNames[i] = gsub("^(t)","time",cleanNames[i])
        cleanNames[i] = gsub("^(f)","freq",cleanNames[i])
        cleanNames[i] = gsub("([Gg]ravity)","Gravity",cleanNames[i])
        cleanNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",cleanNames[i])
        cleanNames[i] = gsub("[Gg]yro","Gyro",cleanNames[i])
        cleanNames[i] = gsub("AccMag","AccMagnitude",cleanNames[i])
        cleanNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",cleanNames[i])
        cleanNames[i] = gsub("JerkMag","JerkMagnitude",cleanNames[i])
        cleanNames[i] = gsub("GyroMag","GyroMagnitude",cleanNames[i])
        cleanNames[i] = gsub("-Y","",cleanNames[i])
        cleanNames[i] = gsub("-Z","",cleanNames[i])
        cleanNames[i] = gsub("-X","",cleanNames[i])
        cleanNames[i] = gsub("-","",cleanNames[i])
        cleanNames[i] = gsub("\\.","",cleanNames[i])
};

### Replace original names with clean
colnames(dtFiltered) <- cleanNames
View(head(dtFiltered))

## THE TIDY DATA
### Create separate tidy dataset containing the average of each variable for each activity and each subject.
#### Group the data table by ActivityName, SubjectID
dtMeans <- dtFiltered %>% 
        group_by(ActivityID, ActivityName, SubjectID) %>%   
        summarize_each(funs(mean))
#### Take a look at your tidy data!
View(dtMeans)

### Export the tidy data set as part of project requirements
write.table(dtMeans, file = "tidy_data.txt", row.names = FALSE)

## PEER REVIEW FILE IMPORT SCRIPT
### Read the tidy data file I submitted to Coursera into R; put tidy_data.txt in your working directory
path <- getwd()
filePath <- file.path(path, ".", "tidy_data.txt")
tidyData <- read.table(filePath, header=TRUE)
head(tidyData)

# THANK YOU FOR YOUR REVIEW!
