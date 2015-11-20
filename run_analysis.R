# ----------------------------------------------------------------------------------------
#
#   run_analysis.R
#
#   Author  :Rory Gaddin
#   Date    :2015-11-20
#
#   Performs analysis on UCI HAR Dataset on Samsung SII accelerometer readings.
#   Part of the course requirements for Johns Hopkins Data Science specialization.
#
# ----------------------------------------------------------------------------------------
# Import required libraries

library(dplyr)

# ----------------------------------------------------------------------------------------
# 1. Merge the training and the test sets to create one data set.

# Load metadata about activities and feature names
act.lbls.tbl <- read.table("./activity_labels.txt")
feat.tbl <- read.table("./features.txt")

# Define a function to construct the datasets from their constituent files
get.dataset <- function (ds.name) {
    # Get the data set observations
    ds.vals <- read.table(paste("./",ds.name,"/X_",ds.name,".txt", sep=""))
    
    # 4. Appropriately label the data set with descriptive variable names. 
    names(ds.vals) <- feat.tbl$V2
    
    # 2. Extract only the measurements on the mean and standard deviation for each measurement.
    ds.vals <- ds.vals[
        , c(
                feat.tbl[grep("-mean\\(\\)$", feat.tbl$V2), 1]
            ,   feat.tbl[grep("-std\\(\\)$", feat.tbl$V2), 1]
        )
    ]
    
    # Get the data set subject id
    ds.subj <- read.table(paste("./",ds.name,"/subject_",ds.name,".txt", sep=""))
    ds.vals$subject <- ds.subj$V1
    ds.vals$subject <- ds.subj$V1
    
    # 3. Use descriptive activity names to name the activities in the data set.
    ds.actv <- read.table(paste("./",ds.name,"/y_",ds.name,".txt", sep=""))
    ds.vals$activity <- factor(ds.actv$V1, labels=act.lbls.tbl$V2)
    
    # Add a field indicating which dataset the record originated from
    ds.vals$data.set <- ds.name
    
    ds.vals
}

# Get the test dataset
test.ds <- get.dataset("test")

# Get the training dataset
train.ds <- get.dataset("train")

# Merge the datasets together
all.ds <- union(test.ds, train.ds)

# ----------------------------------------------------------------------------------------
 
# ----------------------------------------------------------------------------------------
# 5. From the data set in step 4, create a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
# ----------------------------------------------------------------------------------------

summ.ds <- group_by(all.ds, activity, subject) %>% select(-(data.set)) %>% summarize_each(funs(mean))
write.table(summ.ds, file="output.txt", row.name=FALSE)