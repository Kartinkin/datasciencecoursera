# Load packages
require("data.table")

# Set path to data
#setwd("~/GettingAndCleaningData")
dataPath <- "UCI HAR Dataset"

# The dataset includes the following files:
# =========================================
#  - 'README.txt'
#  - 'features_info.txt': Shows information about the variables used on the feature vector.
#  - 'features.txt': List of all features.
#  - 'activity_labels.txt': Links the class labels with their activity name.
#  - 'train/X_train.txt': Training set.
#  - 'train/y_train.txt': Training labels.
#  - 'test/X_test.txt': Test set.
#  - 'test/y_test.txt': Test labels.
# The following files are available for the train and test data.Their descriptions are equivalent. 
#  - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample.

##############################################################################
# 1. Merge the training and the test sets to create one data set

# Read the activities observations
activityTrain <- fread(file.path(dataPath, "train", "y_train.txt"))
activityTest  <- fread(file.path(dataPath, "test", "y_test.txt"))
# Read the subjects
subjectTrain <- fread(file.path(dataPath, "train", "subject_train.txt"))
subjectTest  <- fread(file.path(dataPath, "test", "subject_test.txt"))
# Read the data files
dataTrain <- fread(file.path(dataPath, "train", "X_train.txt"))
dataTest  <- fread(file.path(dataPath, "test", "X_test.txt"))

wholeDataSet <- cbind(
  rbind(subjectTrain, subjectTest),
  rbind(activityTrain, activityTest),
  rbind(dataTrain, dataTest))
names(wholeDataSet)[1] <- "subject"
names(wholeDataSet)[2] <- "activityNum"
# Sort data by subject and activity
setkey(wholeDataSet, subject, activityNum)


##############################################################################
# 2. Extract only the measurements on the mean and standard deviation for each measurement.

# Read all features
features <- fread(file.path(dataPath, "features.txt"))
setnames(features, names(features), c("featureNum", "featureName"))
# Filter only the mean and standard deviation features
features <- features[grepl("(mean|std)\\(\\)", featureName)]
# Convert the feature number to variable name in wholeDataSet
features$featureCode <- paste0("V", features$featureNum)

# Subset only variables of the mean and standard deviation
wholeDataSet <- wholeDataSet[, c(key(wholeDataSet), features$featureCode), with = FALSE]


##############################################################################
# 3. Set descriptive activity names to the activities in the data set

# Read descriptive activities names
activityNames <- fread(file.path(dataPath, "activity_labels.txt"))
setnames(activityNames, names(activityNames), c("activityNum", "activityName"))

# Replace activityNum with activityName
wholeDataSet <- merge(wholeDataSet, activityNames, by = "activityNum", all.x = TRUE)[, activityNum:=NULL]
setkey(wholeDataSet, subject, activityName)


##############################################################################
# 4. Appropriately labels the data set with descriptive variable names

# Convert data set to long form where each feature is a separate observation.
wholeDataSet <- melt(wholeDataSet, id.vars = key(wholeDataSet), variable.name = "featureCode")

# Replace featureCode with featureName
wholeDataSet <- merge(wholeDataSet, features, by = "featureCode", all.x = TRUE)[, c("featureCode", "featureNum"):=NULL]

wholeDataSet$activityName <- factor(wholeDataSet$activityName)
setnames(wholeDataSet, "activityName", "activity")

# Seperate features from featureName and remove featureName
wholeDataSet$featureDevice <- factor(
  grepl("Gyro", wholeDataSet$featureName), 
  labels = c("Accelerometer", "Gyroscope"))

x <- matrix(c(1,2), nrow = 2)
wholeDataSet$featureAcceleration <- factor(
  cbind(grepl("BodyAcc", wholeDataSet$featureName),
        grepl("GravityAcc", wholeDataSet$featureName)) %*% x,
  labels = c(NA, "Body", "Gravity"))

wholeDataSet$featureDomain <- factor(
  grepl("^t", wholeDataSet$featureName), 
  labels = c("Frequency", "Time"))

wholeDataSet$featureJerk <- factor(
  grepl("Jerk", wholeDataSet$featureName),
  labels = c(NA, "Jerk"))

wholeDataSet$featureMagnitude <- factor(
  grepl("Mag", wholeDataSet$featureName),
  labels = c(NA, "Magnitude"))

wholeDataSet$featureVariable <- factor(
  grepl("mean", wholeDataSet$featureName),
  labels = c("Standard deviation", "Mean value"))

x <- matrix(c(1, 2, 3), nrow = 3)
wholeDataSet$featureAxis <- factor(
  cbind(grepl("-X",  wholeDataSet$featureName),
        grepl("-Y",  wholeDataSet$featureName),
        grepl("-Z",  wholeDataSet$featureName)) %*% x,
  labels = c(NA, "X", "Y", "Z"))

wholeDataSet <- wholeDataSet[, featureName:=NULL]

##############################################################################
# 5. Create a second, independent tidy data set with the average of each
# variable for each activity and each subject

tidy <- wholeDataSet[, list(count = .N, average = mean(value)),
                     by = c("subject", "activity", "featureDevice",
                            "featureAcceleration", "featureDomain",
                            "featureJerk", "featureMagnitude",
                            "featureVariable", "featureAxis")]
write.table(tidy, "tidy dataset.txt", row.names = FALSE)
