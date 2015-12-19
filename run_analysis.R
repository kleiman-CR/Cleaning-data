# Upload packeges we are goint to use

library(dplyr)
library(gdata)
library(plyr)

# File Loading

activity_labels <- read.table("C:/Users/iair/Desktop/Irit/Coursera/activity_labels.txt", quote="\"", comment.char="")
features <- read.table("C:/Users/iair/Desktop/Irit/Coursera/features.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)

subject_test <- read.table("C:/Users/iair/Desktop/Irit/Coursera/test/subject_test.txt", quote="\"", comment.char="")
X_test <- read.table("C:/Users/iair/Desktop/Irit/Coursera/test/X_test.txt", quote="\"", comment.char="")
y_test <- read.table("C:/Users/iair/Desktop/Irit/Coursera/test/y_test.txt", quote="\"", comment.char="")

subject_train <- read.table("C:/Users/iair/Desktop/Irit/Coursera/train/subject_train.txt", quote="\"", comment.char="")
X_train <- read.table("C:/Users/iair/Desktop/Irit/Coursera/train/X_train.txt", quote="\"", comment.char="")
y_train <- read.table("C:/Users/iair/Desktop/Irit/Coursera/train/y_train.txt", quote="\"", comment.char="")


#Merges the training and the test sets to create one data set.


Features_1 <- features %>% select(V2) 


names(X_train) <- Features_1$V2
names(X_test)<- Features_1$V2
names(subject_test) <- "Subject"
names(subject_train) <- "Subject"
names(y_test) <- "Activity"
names(y_train) <- "Activity"

# Merging the data frames into Data.set

A_test = cbind(y_test, X_test)
A_test = cbind(subject_test, A_test)



A_train = cbind(y_train, X_train)
A_train = cbind(subject_train, A_train)


Data.set <- rbind.data.frame(A_train, A_test)
 


 # Extracts only the measurements on the mean and standard deviation for each measurement.


library(tidyr)


clean <- Data.set[, !duplicated(colnames(Data.set))]
Mean_SD <- select(clean, Subject, Activity, contains("mean"), contains("std"))

# Uses descriptive activity names to name the activities in the data set

Mean_SD$Activity<- factor(Mean_SD$Activity,
                    levels = c(1,2,3,4,5,6),
                    labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

# Uses descriptive activity names to name the activities in the data set


Names <- names(Mean_SD)

library(stringr)
Names1 <- str_replace_all(Names, "\\(", "") 
Names1  <- str_replace_all(Names1, "\\)", "")
Names1 <- str_replace_all(Names1, "tBody", "Total_Body")
Names1 <- str_replace_all(Names1, "fBody", "Freq_Body")
Names1 <- str_replace_all(Names1, "tGravityAcc", "Gravity_Acceleration")
Names1 <- str_replace_all(Names1, "Acc", "Acceleration")
Names1 <- str_replace_all(Names1, "Gyro", "Gyroscope")
Names1 <- str_replace_all(Names1, "sd", "Standar_Deviation")
Names1 <- str_replace_all(Names1, "graviyMean", "Gravity_Mean")
Names1 <- str_replace_all(Names1, "angle", "Angle_between_to_vectors")

Names1 <- str_replace_all(Names1, "GraviyAcc", "Gravity_Acceleration")
# Names1 <- str_replace_all(Names1, "-", "")
Names1 <- str_replace_all(Names1, "meanFreq", "Mean_Frequency")
Names1 <- str_replace_all(Names1, "GyroscopeMean", "Gyroscope_Mean")
Names1 <- str_replace_all(Names1, "GyroscopeJerk", "Gyroscope_Jerk")
Names1 <- str_replace_all(Names1, "GyroscopeJerkMag", "Gyroscope_Jerk_Magnitud")
Names1 <- str_replace_all(Names1, "AccelerationMag", "Acceleration_Magnitud")
Names1 <- str_replace_all(Names1, " ", "_")

names(Mean_SD) <- Names1



# Extracts only the measurements on the mean and standard deviation for each measurement.

clean <- Mean_SD[, !duplicated(colnames(Mean_SD))]
Tidy_data <- Mean_SD %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
