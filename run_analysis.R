# Code for course-3 "Getting and Cleaning Data"
# Author: #letspairup

library(dplyr)
library(mgsub)
library(stringr)

# function to download data and save as
download.data <- function(data.source = data.source, saveas.filename = saveas.filename, saveas.ext = saveas.ext) {
    if (!file.exists(saveas.filename)){
        download.file(data.source,
                      construct.filename(saveas.filename, saveas.ext), method="curl")
    }
}

# utility function to construct file name
construct.filename <- function(saveas.filename = saveas.filename, saveas.ext = saveas.ext)
{
    paste(sep= "", saveas.filename, ".", saveas.ext)
}

# function to unzip data
unzip.data <- function(file.name) {
    unzip(file.name)
}

# function to rename column name
rename.col <- function(x) {
    mgsub(x, c("-mean", "-std", "[-()]"), c("Mean", "Std", ""))
}

# data file name and saveas params
data.source.file <-  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
saveas.filename <- "human_activity_recognition_using_phones"
saveas.ext <- "zip"
FOLDER.PATH <- "UCI HAR Dataset/"

download.data(data.source.file, saveas.filename, saveas.ext)

unzip.data(construct.filename(saveas.filename, saveas.ext))

print("file downloaded and unzip done !!")

# load features and create df with required features,index and rename values
features.required.df <- read.table(paste0(FOLDER.PATH, "features.txt")) %>% 
    filter(str_detect(V2, ".*mean.*|.*std.*")) %>%
    mutate(V2 = lapply(V2, rename.col))

# create train data-set df
train.df <- cbind(read.table(paste0(FOLDER.PATH, "train/subject_train.txt")),
                  read.table(paste0(FOLDER.PATH, "train/Y_train.txt")),
                  read.table(paste0(FOLDER.PATH, "train/X_train.txt"))[features.required.df$V1])
# create test data-set df
test.df <- cbind(read.table(paste0(FOLDER.PATH, "test/subject_test.txt")),
                 read.table(paste0(FOLDER.PATH, "test/Y_test.txt")),
                 read.table(paste0(FOLDER.PATH, "test/X_test.txt"))[features.required.df$V1])

# test the columns in test and train data-sets
if(identical(colnames(train.df),colnames(test.df))) {
    print("test and train datasets are having same columns!!")
}

# merge by row and create combined data-frame
combined.df <- rbind(train.df,test.df)

# assign proper names to columns
colnames(combined.df) <- c("subject", "activity", features.required.df$V2)

print("test and train data-sets are merged !! ")

# load activity file
activity.lables <- read.table(paste0(FOLDER.PATH, "activity_labels.txt"), 
                              col.names = c("number", "name"))

# convert subject and activity to factors
combined.df <- combined.df %>% mutate(subject = sapply(subject, as.factor)) %>% 
    mutate(activity = sapply(activity, factor, 
                             levels = activity.lables$number, labels = activity.lables$name))

# create tidy data-frame with mean of variables for each subject and activity
combined.df <-  combined.df %>% 
    group_by(subject, activity) %>%
    summarise_at(vars(-group_cols()), .funs = mean)

print(paste(sep = " ", nrow(combined.df), "by", ncol(combined.df), " tidy data-frame is created !!"))

# write tidy data-frame to text file as table
write.table(combined.df, "tidy.txt", row.names = FALSE, quote = FALSE)

print("wrote tidy.txt to disk !!")

