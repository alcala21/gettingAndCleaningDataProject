## 1. MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET.

# Set arrays with file names
trainFiles <- c('X_train','subject_train', 'y_train')
testFiles <- c('X_test','subject_test', 'y_test')


# Load and merge all data from the text files in the train and test folders
merge_similar_data <- function(fileNames) {
	fileName <- paste0(fileNames[1],'.txt')
	subset1 <- read.table(fileName, stringsAsFactors = FALSE)

	for (i in seq(2,length(fileNames))) {	
		fileName <- paste0(fileNames[i],'.txt')
		subset1 <- cbind(subset1, read.table(fileName, stringsAsFactors = FALSE))
	}
	return(subset1)
}

# Merge the combined test and train data frames
set1 <- rbind(merge_similar_data(trainFiles), 
	merge_similar_data(testFiles))


## 2. EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR 
## EACH MEASUREMENT.
require(stringr)

# Extract the information of the columns
fileName <- 'features.txt'	
column_names <- read.table(fileName, sep= ' ', stringsAsFactors = F)

# Find which columns have 'mean' and 'std' in the description
column_position <- which(str_detect(column_names$V2, '\\b-mean()\\b|\\b-std()\\b'))

# Create a data frame with 'mean' and 'std' columns
set2 <- set1[,column_names$V1[column_position]]

# Label the data set
names(set2) <- column_names$V2[column_position]

# Add subject and activity id from set1 to set2 
nColumns <- ncol(set1)
set2$subject <- set1[,nColumns -1]
set2$activity.id <- set1[,nColumns]


## 3. USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET

# Load file with activity labes
fileName <- 'activity_labels.txt'	
activity_labels <- read.table(fileName, sep = ' ', stringsAsFactors = F)
set2$activity <- activity_labels[set2$activity.id, 2]

# Remove the activity.id column since it's not needed anymore
set2$activity.id <- NULL

## 4. APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES.
# This action will be achieved with the creation of the tidy data set in
# part 5.

## 5. CREATES A SECOND, INDEPENDENT TIDY DATA SET WITH THE AVERAGE OF EACH
## VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT.

# Get average of variables for each activity and subject
require(plyr)
require(reshape2)

set3 <- ddply(set2,c('subject','activity'), colwise(mean))

# We now have a lot of variables with 'mean' and 'std', and X, Y and Z in
# their names. We can create a new variable called 'statistic' that shows
# whether a measurement corresponds to a 'mean' or 'std' value, and a
# variable called 'axis' that shows whether a measurement corresponds to 
# the X, Y or Z axis.

set4 <- melt(set3, id.vars = c('subject', 'activity'))

# Create the columns statistic and signal
dash_position <- str_locate(set4$variable, '-')	
set4$signal <- str_sub(set4$variable, start = 1, end = (dash_position[, 1] - 1) )
set4$statistic <- str_sub(set4$variable, start = (dash_position[, 2] + 1))

# Remove the variable column
set4$variable <- NULL

# Create extra column with axis values
dash_position <- str_locate(set4$statistic, '-')	
set4$axis <- str_sub(set4$statistic, start = (dash_position[, 1] + 1) )

par_position <- str_locate(set4$statistic, '\\(')
set4$statistic <- str_sub(set4$statistic, 1, end = (par_position[, 2] - 1))

# Now that we have the statistic column with the mean and std labels, we 
# can cast the data set respect to this variable to have a columns with 
# the mean and std values. We'll name this the tidy set.

# Cast respect to statistic
tidySet <- dcast(set4, subject + activity + signal + axis  ~ statistic, value.var = 'value')

# Now we have six columns with values for the subject, activity, signal,
# axis, mean and std. However, the signal column contains values for time
# and frequency domain signals. The set would be tidier if we had a column
# that showed whether a signal was in the time or frequency domain.

# Get signal domain
tidySet$signal.domain <- str_sub(tidySet$signal, 1, 1)	
tidySet$signal <- str_sub(tidySet$signal, start = 2)

domain_names <- c('f' = 'frequency', 't' = 'time')
tidySet$signal.domain <- domain_names[tidySet$signal.domain]

# Now, we will create a column whose values show if a signal corresponds 
# to a 'Body' or 'Gravity' measurement. To do this we first need to fix a 
# typo in the values. To fix the typo, the word 'BodyBody' should be 
# replaced by 'Body'.

# Fix the signals with BodyBody names	
tidySet$signal <- str_replace(tidySet$signal, 'BodyBody', 'Body')

# Extract column with Body and Gravity 
y_position <- str_locate(tidySet$signal, 'y')
tidySet$acceleration.type <- str_sub(tidySet$signal, end = y_position[ , 1])
tidySet$signal <- str_sub(tidySet$signal, start = y_position[ , 1] + 1)


# We can also differenciate if a signal comes from the accelerometer or 
# gyroscope. This can be done by creating a column with the source of this 
# signal.

# Add signal source
name_position <- str_locate(tidySet$signal,'Acc|Gyro')
tidySet$source <- str_sub(tidySet$signal, start = name_position[,1], end = name_position[, 2])

source_names <- c('Acc' = 'Accelerometer', 'Gyro' = 'Gyroscope')
tidySet$source <- source_names[tidySet$source]	

# Remove source from signal column
tidySet$signal <- str_replace(tidySet$signal, 'Acc|Gyro', '')
empty_position <- which(str_length(tidySet$signal) == 0)

# Substitute with 'None' any empty positions left on the signal column
tidySet$signal[empty_position] <- 'None'

# The remaining values in the 'signal' column correspond to the calculated 
# values of the main signals. Let's rename the 'signal' column with the 
# 'calculated.signal', and the change abbreviated values to their full name

# Replace abbreviated names
signal_names <- c('None' = 'None', 'Jerk' = 'Jerk.Vector', 
	'JerkMag' = 'Jerk.Vector.Magnitude', 'Mag' = 'Vector.Magnitude') 			

tidySet$signal <- signal_names[tidySet$signal]

tidySet$calculated.signal <- tidySet$signal
tidySet$signal <- NULL


# Finally let's reorder the columns and sort them.

# Shuffle columns
tidySet <- tidySet[c('subject','activity', 'source', 'acceleration.type', 'calculated.signal', 'signal.domain', 'axis', 'mean', 'std')]

# Sort the rows
tidySet <- arrange(tidySet, subject, activity, source, acceleration.type, calculated.signal, signal.domain, axis)