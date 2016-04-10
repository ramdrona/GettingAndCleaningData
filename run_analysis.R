library(datasets)
library(data.table)
library(plyr)

setwd("D:/R_Dec2014/JH_DataSciences")

# Load activity labels + features
activityLabels <- read.table("./UCI_HAR_Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("./UCI_HAR_Dataset/features.txt")
features[,2] <- as.character(features[,2])
# Extract only the data on mean and standard deviation
featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
featuresWantedNames <- features[featuresWanted,2]


dftrain <- read.table("./UCI_HAR_Dataset/X_train.txt", colClasses = "numeric", header=FALSE)
dftrainDT <- data.table(dftrain[featuresWanted])
dftrainlbl <- read.table("./UCI_HAR_Dataset/y_train.txt", colClasses = "numeric", header=FALSE)
subjectTrain = read.table("./UCI_HAR_Dataset/subject_train.txt",header=FALSE) #imports subject_train.txt
# str(dftrainDT); head(dftrainDT)
rm(dftrain)


dftest <- read.table("./UCI_HAR_Dataset/X_test.txt", colClasses = "numeric", header=FALSE)
dftestlbl <- read.table("./UCI_HAR_Dataset/y_test.txt", colClasses = "numeric", header=FALSE)
subjectTest = read.table("./UCI_HAR_Dataset/subject_test.txt",header=FALSE) #imports subject_test.txt
dftestDT <- data.table(dftest[featuresWanted])
rm(dftest)

# assign names to column names
colnames(dftrainlbl) <- "ActivityID"
colnames(subjectTrain) <- "subjectId"

# Column binding dftrainlbl,subjectTrain and mean and STd deviation columns of df train data frame 
trainData <-  data.table(cbind(dftrainlbl, subjectTrain, dftrainDT))

rm(dftrainDT)
rm(dftrainlbl)

# assign names to column names
colnames(dftestlbl) <- "ActivityID"
colnames(subjectTest) <- "subjectId"

testData <-  data.table(cbind(dftestlbl,subjectTest, dftestDT))

#str(testData)
# Remove testlnl and dftestDT as the merged testData table is available
rm(dftestDT)
rm(dftestlbl)

# 3 create the merged Train and Test data table 
# merge data sets - train and test
finalData <- rbind(trainData,testData)
# Assign column names from measurements
colnames(finalData) <- c("ActivityID", "subjectId", featuresWantedNames)


# 4. Appropriately label the data set with descriptive activity names. 

allDatacolNames <- colnames(finalData)
# Cleaning up the variable names
for (i in 1:length(allDatacolNames)) 
{
  allDatacolNames[i] = gsub("\\()","",allDatacolNames[i])
  allDatacolNames[i] = gsub("-std$","StdDev",allDatacolNames[i])
  allDatacolNames[i] = gsub("-mean","Mean",allDatacolNames[i])
  allDatacolNames[i] = gsub('[-()]', '',allDatacolNames[i])
  allDatacolNames[i] = gsub("^(t)","time",allDatacolNames[i])
  allDatacolNames[i] = gsub("^(f)","freq",allDatacolNames[i])
  allDatacolNames[i] = gsub("([Gg]ravity)","Gravity",allDatacolNames[i])
  allDatacolNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",allDatacolNames[i])
  allDatacolNames[i] = gsub("[Gg]yro","Gyro",allDatacolNames[i])
  allDatacolNames[i] = gsub("AccMag","AccMagnitude",allDatacolNames[i])
  allDatacolNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",allDatacolNames[i])
  allDatacolNames[i] = gsub("JerkMag","JerkMagnitude",allDatacolNames[i])
  allDatacolNames[i] = gsub("GyroMag","GyroMagnitude",allDatacolNames[i])  
}

colnames(finalData) <- allDatacolNames
#str(finalData)

# 5 Create separate independent tidy data set with the average of each variable for each activity
# It is fastest to bring data first into the long format and do aggregation next
# turn "ActivityID" and "subjectId" into factors

finalData$ActivityID <- as.factor(finalData$ActivityID ) 
finalData$subjectId <- as.factor(finalData$subjectId)

finalDataMelted <- melt(finalData, id = c("subjectId", "ActivityID"))
#DataMean <- ddply(finalDataMelted, c("subjectId", "ActivityID" , "variable"), 
#                    summarise, mean=mean(value))
finalDataMean <- dcast(finalDataMelted, subjectId + ActivityID ~ variable, mean)
write.table(finalDataMean, "./UCI_HAR_Dataset/tidyData.txt", row.names = FALSE, quote = FALSE)
