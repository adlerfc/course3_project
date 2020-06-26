# Initial settings -----------------------------------------------------
rm(list = ls())

library(data.table)
library(dplyr, warn.conflicts = FALSE)


dir_data <- "./UCI HAR Dataset/"
dir_test <- paste(dir_data, "test/", sep = "")
dir_train <- paste(dir_data, "train/", sep = "")

file_activity_labels <- paste(dir_data, "activity_labels.txt", sep = "")
file_features <- paste(dir_data, "features.txt", sep = "")
file_subject_train <- paste(dir_train, "subject_train.txt", sep = "")
file_X_train <- paste(dir_train, "X_train.txt", sep = "")
file_y_train <- paste(dir_train, "y_train.txt", sep = "")

file_subject_test <- paste(dir_test, "subject_test.txt", sep = "")
file_X_test <- paste(dir_test, "X_test.txt", sep = "")
file_y_test <- paste(dir_test, "y_test.txt", sep = "")


# Read data with labeled variable names ---------------------------------------------------------------

activity_labels <- as_tibble( fread(file_activity_labels, col.names = c("code", "activity")) )
activity_labels <- factor(x = activity_labels$activity, levels = activity_labels$activity)

features <- as_tibble( fread(file_features, col.names = c("index", "feature")) )

subject_train <- as_tibble(fread(file_subject_train, col.names = "subject"))
data_train <- as_tibble(fread(file_X_train))
names(data_train) <- features$feature
activity_train <- as_tibble(fread(file_y_train, col.names = "activity"))

subject_test <- as_tibble(fread(file_subject_test, col.names = "subject"))
data_test <- as_tibble(fread(file_X_test))
names(data_test) <- features$feature
activity_test <- as_tibble(fread(file_y_test, col.names = "activity"))


# Merge training and test sets -----------------------------------------------------------

dataset_train <- bind_cols(subject_train, activity_train, data_train)
dataset_train <- mutate(dataset_train, datatype = "train")

dataset_test <- bind_cols(subject_test, activity_test, data_test)
dataset_test <- mutate(dataset_test, datatype = "test")

dataset <- rbind(dataset_train, dataset_test)
dataset <- mutate(dataset, datatype = factor(datatype, levels = unique(datatype)) )


# Extract measurements on mean and sd for each measurement, use descriptive activity names ----------------

dataset <- dataset %>% 
        select(subject, activity, (contains("mean()") | contains("std()")), datatype) %>%
        mutate(subject = as.factor(subject), activity = activity_labels[activity]) %>%
        print

# Tidy variable names -----------------------------------------------------

names(dataset) <- sapply(names(dataset), function(arg){
        arg <- gsub(pattern = "()", replacement = "", fixed = TRUE, x = arg)
        gsub(pattern = "-", replacement = ".", x = arg)
        }
)

# Create second tidy dataset with average of each variable for each activity and subject  --------

measureNames <- names(select(dataset, -c(subject, activity, datatype)) )

dataset_avg <-  dataset %>% 
                group_by(activity, subject) %>%
                summarise(across(measureNames, mean, .names = "avg.{col}"))
# View(dataset)
# View(dataset_avg)

# Output dataset with averages

write.table(dataset_avg, file = "tidy_dataset.txt", row.names = FALSE)







