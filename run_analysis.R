## Getting and Cleaning Data Week 4 Project
## Download the dataset from the following Url either manually or by console and unzip the files.
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


# Read the features and activity labels dataset
features <- read.table("./UCI HAR Dataset/features.txt")
act_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Set the variable names for act_labels
colnames(act_labels) <- c("activityId", "activityType")

# Read the train data set and give appropriate variable names.
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
colnames(x_train) <- features[,2]
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
colnames(y_train) <- "activityId"
sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
colnames(sub_train) <- "subId"
training_set <- cbind(x_train, y_train, sub_train)

# Read the test data set and give appropriate variable names.
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
colnames(x_test) <- features[,2]
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
colnames(y_test) <- "activityId"
sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
colnames(sub_test) <- "subId"
test_set <- cbind(x_test, y_test, sub_test)

# Merge the training and test data set
merged_set <- rbind(training_set, test_set)

# Extract the necessary features, activityId and subId from the merged data set.
col_vector <- !grepl(".*meanFreq.*", colnames(merged_set)) & grepl(".*mean().*|.*std().*|activityId|subId", colnames(merged_set))
merged_set <- merged_set[, col_vector]

# Use descriptive activity names to name activities in data set.
merged_set$activityId <- act_labels[,2][match(merged_set$activityId, act_labels[,1])]
colnames(merged_set)[colnames(merged_set) == 'activityId'] <- 'activityType'
columns_merged <- colnames(merged_set)

# Tidy the column names of the merged dataset.
for (i in 1 : length(columns_merged)){
  columns_merged[i] = gsub("\\()", "", columns_merged[i])
  columns_merged[i] = gsub("-mean()", "Mean", columns_merged[i])
  columns_merged[i] = gsub("-std()", "StdDev", columns_merged[i])
}

# Update the column names of the merged dataset.
colnames(merged_set) <- columns_merged

# Create a tidy dataset with average of each variable for each activity and each subject.
tidy_set <- merged_set %>% group_by(activityType, subId) %>% summarize_all(list(mean))

# Write the tidy data set to tidy.txt file.
write.table(tidy_set, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)