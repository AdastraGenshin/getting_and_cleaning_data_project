getting_and_cleaning_data_project
=================================

This course project is done as follow

# STEP 1: Merges the training and the test sets to create one data set.

train_x <- read.table("UCI HAR Dataset/train/X_train.txt")
test_x  <- read.table("UCI HAR Dataset/test/X_test.txt")
data_pre_1 <- rbind(train_x, test_x)
# > str(data_pre_1)
# 'data.frame':  10299 obs. of  561 variables

subject_train <- fread("UCI HAR Dataset/train/subject_train.txt")
subject_test <- fread("UCI HAR Dataset/test/subject_test.txt")
subject <- rbind(subject_train,subject_test)

train_y <- fread("UCI HAR Dataset/train/y_train.txt")
test_y <- fread("UCI HAR Dataset/test/y_test.txt")
activity <- rbind(train_y, test_y)

data_pre_2 <- cbind(data_pre_1, activity, subject) 
# > str(data_pre_2)
# 'data.frame':  10299 obs. of  563 variable

# STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- readLines("UCI HAR Dataset/features.txt")
select1 <-grep("mean()",features,fixed=TRUE)
select2 <-grep("std()",features,fixed=TRUE)
select <- c(select1,select2)
data_sel <- data_pre_1[,select]
# > str(data_sel)
# 'data.frame':  10299 obs. of  79 variable

# STEP 3: Uses descriptive activity names to name the activities in the data set
activity[activity==1] <- "WALKING"
activity[activity==2] <- "WALKING_UPSTAIRS"
activity[activity==3] <- "WALKING_DOWNSTAIRS"
activity[activity==4] <- "SITTING"
activity[activity==5] <- "STANDING"
activity[activity==6] <- "LAYING"

# STEP 4: Appropriately labels the data set with descriptive variable names. 

features_1 <- substr(features, regexpr(" ",features)+1, 100)
label <- c(features_1[select], "activity","subject")
data_pre_3 <- cbind(data_sel, activity, subject)
colnames(data_pre_3) <- label 

# STEP 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#mean.sel <- function(x) sapply(split(data_pre_3[,x],list(data_pre_3$activity,data_pre_3$subject),drop=TRUE),mean)
res_tidy <- aggregate(data_pre_3,list(data_pre_3$activity,data_pre_3$subject),mean)
res_tidy <- res_tidy[,1:68]
colnames(res_tidy)[1] <- "activity"
colnames(res_tidy)[2] <- "subject"


#output
write.table(res_tidy, "tidy_res.txt")
