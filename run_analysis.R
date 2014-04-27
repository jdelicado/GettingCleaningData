# Set working directory in my computer
# setwd("/home/jdelicado/cursos/coursera/Getting and Cleaning Data/UCI HAR Dataset")

# create a new dataframe 'test' from data in the file X_test.txt (testing)
test <- read.table(file="test/X_test.txt")
# Create a new dataframe 'activities_test' from the data in the file y_test.txt,
# which contains the activity code of testing
activities_test <- read.table(file="test/y_test.txt")
# Create a new dataframe 'subjects_test' from the data in the file subject_test.txt,
# which contains the subject number of testing
subjects_test <- read.table(file="test/subject_test.txt")

# Create a new dataframe 'train' from data in the file X_train.txt (training)
train <- read.table(file="train/X_train.txt")
# Create a new dataframe 'activities_train' from the data in the file y_train.txt,
# which contains the activity code of training
activities_train <- read.table(file="train/y_train.txt")
# Create a new dataframe 'subjects_train' from the data in the file subject_train.txt,
# which contains the subject number of training
subjects_train <- read.table(file="train/subject_train.txt")

# Create a new dataframe 'merged', which is the merge of two previous created dataframes
merged <- merge(test, train, all=TRUE)
# Create a new dataframe 'col_names', according the data in the "features.txt" file
col_names <- read.table(file="features.txt")
# Set the column name
colnames(merged) <- col_names$V2

# Create a logical object 'indexes', which is TRUE if the column name contains the words
# 'mean' or 'std'
indexes <- grepl("mean|std", col_names$V2, perl=TRUE)
# Create a new dataframe 'mean_std_data', which only incluyes the mean and standard
# deviation for each measurement
mean_std_data <- merged[indexes]

# Create a list 'subjects_numbers', which contains the subject numbers.
# First element of the list is the subject numbers of testing and the second for training
subjects_numbers <- list(subjects_test, subjects_train)
# Create a list 'activities_codes', which contains the activity codes.
# First element of the list is the activity codes of testing and the second for training
activities_codes <- list(activities_test, activities_train)

# Create a new column called 'Subject' with the correspond subject number of testing
# and training, using a temporary variable 'tmp'
tmp <- rbind(subjects_numbers[[1]], subjects_numbers[[2]])
mean_std_data$Subject <- as.integer(tmp$V1)

# Create a new column called 'ActivityCode' with the correspond activity code of testing
# and training, using a temporary variable 'tmp'
tmp <- rbind(activities_codes[[1]], activities_codes[[2]])
mean_std_data$ActivityCode <- as.integer(tmp$V1)

# Store the colnames of the dataframe, before adding the column 'ActivityName', to
# assign the column names to the final second tidy data set
colnames_second_tidy_data <- colnames(mean_std_data)

# Create a new dataframe 'activities_labels' with the activity labels, according to the
# data in "activity_labels.txt" file
activities_labels <- read.table(file="activity_labels.txt")

# Create a new "character" object 'x' to store the correspond description of each activity
x <- vector(mode="character", length = dim(mean_std_data)[1])

for (i in 1:dim(mean_std_data)[1]) {
  code <- mean_std_data$ActivityCode[i]
  x[i] <- as.character(activities_labels[activities_labels$V1 == code, 2])
}

# Create a new column called 'ActivityName' with the correspond activity name
mean_std_data$ActivityName <- as.character(rbind(x))

### Activity point 5
# Create a new dataframe 'second_tidy_data' to store the mean per subject/activity
second_tidy_data = data.frame()

# Create a vector 'media_vector' where it is stored the temporary means per column/variable,
# dropping one column (ActivityName)
media_vector <- vector(mode="numeric", length = (dim(mean_std_data)[2] - 1))

# Do the mean for each subject and activity
for (subject in 1:length(levels(factor(mean_std_data$Subject)))) {
  for (activity in 1:dim(activities_labels)[1]) {
    subject_data <- subset(mean_std_data, mean_std_data$Subject == subject)
    sub_act_data <- subset(subject_data, subject_data$ActivityCode == activity)
    
    # for each column, it calculates its mean, storing it in a column of 'media_vector'
    media <- 0
    for (i in 1:(dim(sub_act_data)[2] - 3)) {
      media_vector[i] <- mean(sub_act_data[, i])
    }
    
    # Add the values of the corresponding subject and activity
    media_vector[dim(sub_act_data)[2] - 2] <- subject
    media_vector[dim(sub_act_data)[2] - 1] <- activity
    
    # Insert the data in the dataframe, in a new row
    second_tidy_data <- rbind(second_tidy_data, media_vector)
  }
}

# Assign the correspond column names to the new dataframe
colnames(second_tidy_data) <- colnames_second_tidy_data

# Finally, write the dataframe in a file 'SecondTidyData.txt'
write.table(second_tidy_data, file="SecondTidyData.txt", quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
