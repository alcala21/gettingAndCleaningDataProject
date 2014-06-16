## 1. MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET.

	# Set paths for data files
	dataPath <- './data/'
	trainFiles <- c('train/X_train','train/subject_train', 'train/y_train')
	testFiles <- c('test/X_test','test/subject_test', 'test/y_test')


	# Load and merge all data from the text files in the train and test folders
	merge_similar_data <- function(dataPath,fileNames) {
		fileName <- paste0(dataPath,fileNames[1],'.txt')
		subset1 <- read.table(fileName, stringsAsFactors = FALSE)

		for (i in seq(2,length(fileNames))) {	
			fileName <- paste0(dataPath,fileNames[i],'.txt')
			subset1 <- cbind(subset1, read.table(fileName))
		}
		return(subset1)
	}

	# Merge the combined test and train data frames
	set1 <- rbind(merge_similar_data(dataPath, trainFiles), 
		merge_similar_data(dataPath, testFiles))


## 2. EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH
## MEASUREMENT.
	require(stringr)

	# Extract the information of the columns
	file <- 'features.txt'
	fileName <- paste0(dataPath, file)
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
	file <- 'activity_labels.txt'
	fileName <- paste0(dataPath, file)
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
	# variable called 'axis' that shows whether a measurement corresponds to the
	# X, Y or Z axis.

	set4 <- melt(set3, id.vars = c('subject', 'activity'), 
		variable.name = 'feature', value.name = 'measurement' )

	# Create features
	dash_position <- str_locate(set4$feature, '-')	
	set4$signal <- str_sub(set4$feature, start = 1, end = (dash_position[, 1] - 1) )
	set4$statistic <- str_sub(set4$feature, start = (dash_position[, 2] + 1))

	# Remove the feature column
	set4$feature <- NULL

	# Create extra column with axis values
	dash_position <- str_locate(set4$statistic, '-')	
	set4$axis <- str_sub(set4$statistic, start = (dash_position[, 1] + 1) )
	
	par_position <- str_locate(set4$statistic, '\\(')
	set4$statistic <- str_sub(set4$statistic, 1, end = (par_position[, 2] - 1))

	# Now that we have the statistic column with the mean and std labels, we can cast the data set respect to this variable to have a columns with the mean and std values. We'll name this the tidy set.

	# Cast respect to statistic
	tidy_set <- dcast(set4, subject + activity + signal + axis  ~ statistic, value.var = 'measurement')

	# Now we have six columns with values for the subject, activity, signal,
	# axis, mean and std. However, the signal column contains values for time
	# and frequency domain signals. The set would be tidier if we had a column
	# that showed whether a signal was in the time or frequency domain.

	# Get signal domain
	tidy_set$signal.domain <- str_sub(tidy_set$signal, 1, 1)	
	tidy_set$signal <- str_sub(tidy_set$signal, start = 2)

	domain_names <- c('f' = 'frequency', 't' = 'time')
	tidy_set$signal.domain <- domain_names[tidy_set$signal.domain]

	# Now, we will create a column whose values show if a signal corresponds to a 'Body' or 'Gravity' measurement. To do this we first need to fix a typo in the values. To fix the typo, the word 'BodyBody' should be replaced by 'Body'.

	# Fix the signals with BodyBody names	
	tidy_set$signal <- str_replace(tidy_set$signal, 'BodyBody', 'Body')

	# Extract column with Body and Gravity 
	y_position <- str_locate(tidy_set$signal, 'y')
	tidy_set$acceleration.type <- str_sub(tidy_set$signal, end = y_position[ , 1])
	tidy_set$signal <- str_sub(tidy_set$signal, start = y_position[ , 1] + 1)


	# We can also differenciate if a signal comes from the accelerometer or gyroscope. This can be done by creating a column with the source of this signal.

	# Add signal source
	name_position <- str_locate(tidy_set$signal,'Acc|Gyro')
	tidy_set$source <- str_sub(tidy_set$signal, start = name_position[,1], end = name_position[, 2])

	source_names <- c('Acc' = 'Accelerometer', 'Gyro' = 'Gyroscope')
	tidy_set$source <- source_names[tidy_set$source]	

	# Remove source from signal column
	tidy_set$signal <- str_replace(tidy_set$signal, 'Acc|Gyro', '')
	empty_position <- which(str_length(tidy_set$signal) == 0)

	# Substitute with 'None' any empty positions left on the signal column
	tidy_set$signal[empty_position] <- 'None'

	# The remaining values in the 'signal' column correspond to the calculated values of the main signals. Let's rename the 'signal' column with the 'calculated.signal', and the change abbreviated values to their full name.


	# Replace abbreviated names
	signal_names <- c('None' = 'None', 'Jerk' = 'Jerk.Vector', 
		'JerkMag' = 'Jerk.Vector.Magnitude', 'Mag' = 'Vector.Magnitude') 			

	tidy_set$signal <- signal_names[tidy_set$signal]

	tidy_set$calculated.signal <- tidy_set$signal
	tidy_set$signal <- NULL


	# Finally let's reorder the columns and sort them.

	# Shuffle columns
	tidy_set <- tidy_set[c('subject','activity', 'source', 'acceleration.type', 'calculated.signal', 'signal.domain', 'axis', 'mean', 'std')]

	# Order the rows by column
	tidy_set <- arrange(tidy_set, subject, activity, source, acceleration.type, calculated.signal, signal.domain, axis)