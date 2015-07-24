#Reading used libraries
require(dplyr)
require(data.table)
require(magrittr)
#require(downloader)
#Downloading and reading data
#download("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "Dataset.zip", mode = "wb")
#unzip("Dataset.zip")
#Reading features and labels
features <- tbl_df(fread("UCI HAR Dataset/features.txt", header = FALSE))
activity_labels <- tbl_df(fread("UCI HAR Dataset/activity_labels.txt", header = FALSE))
#Reading test data
subject_test <- read_table(col_names = FALSE, "UCI HAR Dataset/test/subject_test.txt")
x_test <- read_table(col_names = FALSE, "UCI HAR Dataset/test/X_test.txt")
y_test <- read_table(col_names = FALSE, "UCI HAR Dataset/test/y_test.txt")
#Reading train data
subject_train <- read_table(col_names = FALSE, "UCI HAR Dataset/train/subject_train.txt")
x_train <- read_table(col_names = FALSE, "UCI HAR Dataset/train/X_train.txt")
y_train <- read_table(col_names = FALSE, "UCI HAR Dataset/train/y_train.txt")
#Binding test and train data
x_bound <- bind_rows(x_test, x_train)
y_bound <- bind_rows(y_test, y_train)
subject_bound <- bind_rows(subject_test, subject_train)
#Finding column names containing mean and std
mea <- filter(features, grepl("[Mm][Ee][Aa][Nn]", features$V2))
std <- filter(features, grepl("[Ss][Tt][Dd]", features$V2))
#Selecting found columns
xmea <- select(x_bound, mea$V1)
xstd <- select(x_bound, std$V1)
#Naming columns
names(xmea) <- mea$V2
names(xstd) <- std$V2
names(y_bound) <- "Activity.number"
names(subject_bound) <- "Subject.number"
#Binding mean, std, activity and subject columns
meastd <- bind_cols(xmea, xstd, y_bound, subject_bound)
#Creating new columns for activity and subject and converting to factor
meastdf <- meastd %>%
        mutate(Activity = factor(Activity.number), Subject = factor(Subject.number))
#Giving descriptive names to activities factor levels
levels(meastdf$Activity) <- activity_labels$V2
#Creating tidy data frame, grouping by activity and subject,
#Subtracting some colums and calculating mean
tidy <- meastdf %>%
        group_by(Activity, Subject) %>%
        select(-Activity.number, -Activity, -Subject.number, -Subject) %>%
        summarise_each(funs(mean))
#Creating descriptive column names
names(tidy) <- names(tidy) %>%
        gsub("Freq()", "Frequency", .) %>%
        gsub("^f", "Frequency", .) %>%
        gsub("^t", "Time", .) %>%
        gsub("tBody", "Body", .) %>%
        gsub("BodyBody", "Body", .) %>%
        gsub("Acc", "Accelelerometer", .) %>%
        gsub("Gyro", "Gyroscope", .) %>%
        gsub("Mag", "Magnitude", .) %>%
        gsub("angle", "Angle", .) %>%
        gsub("gravity", "Gravity", .) %>%
        gsub("-std()", "StandardDeviation", .) %>%
        gsub("-mean", "Mean", .)
#Writing data to the file
write.table(tidy, file = "tidy.txt", row.names = FALSE)
