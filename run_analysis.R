library(reshape2)

#
# 0. Read the data files
#

# 0.1. Get the files paths
trainX.file <- "test/X_test.txt"
trainY.file <- "test/y_test.txt"
train.subjects.file <- "train/subject_train.txt" 
testX.file <- "train/X_train.txt"
testY.file <- "train/y_train.txt"
test.subjects.file <- "test/subject_test.txt"
activities.file <- "activity_labels.txt"
features.file <- "features.txt"

# 0.2. Read the data files in memory
data.trainX <- read.table(trainX.file)
data.trainY <- read.table(trainY.file)
train.subjects <- read.table(train.subjects.file)
data.testX <- read.table(testX.file)
data.testY <- read.table(testY.file)
test.subjects <- read.table(test.subjects.file)
activities <- read.table(activities.file)
features <- read.table(features.file)
  
#
# 1. Merge the training and the test sets to create one data set.
# 4. Appropriately label the data set with descriptive variable names. 
#

# 1.1. Merge the X data and label the columns with descriptive variable names
X.data <- rbind(data.trainX,data.testX)
names(X.data) <- features[,2]

# 1.2. Merge the Y data and label the columns with descriptive variable names
Y.data <- rbind(data.trainY,data.testY)
Y.subjects <- rbind(train.subjects, test.subjects)
Y.data <- cbind(Y.data, Y.subjects)
names(Y.data) <- c("activity", "subjects")

# 1.3. Merge the X and Y data
full.data <- cbind(X.data, Y.data)

#
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
#

msd_features <- grepl("mean|std", names(X.data))
msd.data <- full.data[, msd_features]

#
# 3. Use descriptive activity names to name the activities in the data set
#

colnames(activities) <- c("activity", "activity_label")
intersect(names(activities), names(msd.data))
msd.data <- merge(msd.data, activities, by="activity", all = TRUE)

#
# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
#

variables = setdiff(names(msd.data), c("subjects", "activity", "activity_label"))
msd.data <- melt(msd.data, id=c("subjects", "activity", "activity_label"), measure.vars=variables)
tidy.data <- dcast(msd.data, subjects + activity_label ~ variable, mean)
write.table(tidy.data, "tidy.txt",row.name=FALSE)
