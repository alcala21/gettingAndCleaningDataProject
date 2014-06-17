Course Project for Getting and Cleaning Data 
=========================

# Variables in the data set *tidySet*

The tidy set obtained with run_analysis.R contains the following column names: subject, activity, source, acceleration.type, calculated.signal, signal.domain, axis, mean, and std. The description and possible values for each columns are

- **subject**  
	Shows the Id of the person that performs a given activity. The possible values for this variable are 
	+ 1
	+ 2
	+ 3	
	+ ...
	+  30

- **activity**  
	This is the name of the activity performed by a subject. The possible values for this variable are:
	+ WALKING
	+ WALKING_UPSTAIRS
	+ WALKING_DOWNSTAIRS
	+ SITTING
	+ STANDING
	+ LAYING
	
- **source**  
	Tells you whether the measurement comes from the accelerometer or the gyroscope. The possible values are
	+ Accelerometer
	+ Gyroscope

- **acceleration.type**  
	This variable shows whether the acceleration shown corresponds to body acceleration or gravity. Its possible values are
	+ Body
	+ Gravity

- **calculated.signal**  
	This variable shows if a signal is calculated, and what kind of calculated signal is. The possible values are
	+ None - No calculation was performed
	+ Jerk.Vector - The jerk vector was calculated
	+ Vector.Magnitude - The magnitude of the three-dimensional signals calculated using the Euclidean norm
	+ Jerk.Vector.Magnitude - The magnitude of the jerk vector was calculated using the Euclidean norm.

- **signal.domain**  
	Tells you if a signal is in the time or frequency domain. It can take the values 
	+ frequency
	+ time

- **axis**  
	This variable shows if a measurement corresponds to the x, y or z axis. The possible values are
	+ X
	+ Y
	+ Z
	+ NA. These values appear when we select a vector magnitude from the calculated signal column.

- **mean**  
	Shows the mean value of the measurements. This is an average value for each activity and subject.  

- **std**  
	Shows the standard deviation of the measurements. This is an average value for each activity and subject.

# Building of *tidySet*

First we have to merge the training and data sets in order to create one set. The training and test files are in the same working directory as run_analysis.R. The following code was used to merge the training and test files.


```r
# Set arrays with file names
trainFiles <- c('X_train','subject_train', 'y_train')
testFiles <- c('X_test','subject_test', 'y_test')


# Load and merge all train and test files
merge_similar_data <- function(fileNames) {
	fileName <- paste0(fileNames[1],'.txt')
	subset1 <- read.table(fileName, stringsAsFactors = FALSE)

	for (i in seq(2,length(fileNames))) {	
		fileName <- paste0(fileNames[i],'.txt')
		subset1 <- cbind(subset1, read.table(fileName))
	}
	return(subset1)
}

# Merge the combined test and train data frames
set1 <- rbind(merge_similar_data(trainFiles), 
	merge_similar_data(testFiles))
```

After creating the set with the training and testing data, we have to extract the columns with the mean and standard deviation of the values. In order to do that, we have to load the file with the column names, and find which columns contain the mean and std values. We do that with the following code,


```r
require(stringr)
```

```
## Loading required package: stringr
```

```r
# Extract the information of the columns
fileName <- 'features.txt'	
column_names <- read.table(fileName, sep= ' ', stringsAsFactors = F)

# Find which columns have 'mean' and 'std' in the description
column_position <- which(str_detect(column_names$V2, '\\b-mean()\\b|\\b-std()\\b'))
```

After finding the desired columns, we extract them from the original data set *set1* and create *set2*. We also rename the columns in *set2*.


```r
# Create a data frame with 'mean' and 'std' columns
set2 <- set1[,column_names$V1[column_position]]

# Label the data set
names(set2) <- column_names$V2[column_position]

# Add subject and activity id from set1 to set2 
nColumns <- ncol(set1)
set2$subject <- set1[,nColumns -1]
set2$activity.id <- set1[,nColumns]
```

In order to label the activities with their names, we load the file with the activity labels and substitute them in *set2*.


```r
# Load file with activity labes
fileName <- 'activity_labels.txt'	
activity_labels <- read.table(fileName, sep = ' ', stringsAsFactors = F)
set2$activity <- activity_labels[set2$activity.id, 2]

# Remove the activity.id column since it's not needed anymore
set2$activity.id <- NULL
```

Once we have labeled columns and activities, we can now start tidying up the data. First we will average the values in the columns for each subject and activity in *set2*. Let's call the new set *set3*.


```r
# Get average of variables for each activity and subject
require(plyr)
```

```
## Loading required package: plyr
```

```r
require(reshape2)
```

```
## Loading required package: reshape2
```

```r
set3 <- ddply(set2,c('subject','activity'), colwise(mean))
```

We can melt *set3* with respect to subject and activity in order to aggregate all the columns into two columns with the variable names and values. The resulting set, *set4*, has the columns columns subject, activity, variable and value. 


```r
set4 <- melt(set3, id.vars = c('subject', 'activity'))
head(set4)
```

```
##   subject           activity          variable  value
## 1       1             LAYING tBodyAcc-mean()-X 0.2216
## 2       1            SITTING tBodyAcc-mean()-X 0.2612
## 3       1           STANDING tBodyAcc-mean()-X 0.2789
## 4       1            WALKING tBodyAcc-mean()-X 0.2773
## 5       1 WALKING_DOWNSTAIRS tBodyAcc-mean()-X 0.2892
## 6       1   WALKING_UPSTAIRS tBodyAcc-mean()-X 0.2555
```

We can see that values in the column variable include parameters like t, f, mean, std, X, Y, Z, among others, to specify the axis, statistic calculated, domain of measurement, and other variables. We can extract this information from those values and create appropriate columns to specify such parameters.  

First, let's create the columns that show if a value corresponds to a mean or a standard deviation (std). 


```r
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
```

Now that we have a column with the mean and std labels, we can cast the data set respect to this variable to have columns with the mean and std values. We'll name this set *tidySet*.


```r
# Cast respect to statistic
tidySet <- dcast(set4, subject + activity + signal + axis  ~ statistic, value.var = 'value')
head(tidySet)
```

```
##   subject activity       signal axis    mean     std
## 1       1   LAYING     fBodyAcc    X -0.9391 -0.9244
## 2       1   LAYING     fBodyAcc    Y -0.8671 -0.8336
## 3       1   LAYING     fBodyAcc    Z -0.8827 -0.8129
## 4       1   LAYING fBodyAccJerk    X -0.9571 -0.9642
## 5       1   LAYING fBodyAccJerk    Y -0.9225 -0.9322
## 6       1   LAYING fBodyAccJerk    Z -0.9481 -0.9606
```
Now we have six columns with values for the subject, activity, signal, axis, mean and std. However, the signal column contains values for time and frequency domain signals. The set would be tidier if we had a column that showed whether a signal was in the time or frequency domain. 


```r
# Get signal domain
tidySet$signal.domain <- str_sub(tidySet$signal, 1, 1)	
tidySet$signal <- str_sub(tidySet$signal, start = 2)

domain_names <- c('f' = 'frequency', 't' = 'time')
tidySet$signal.domain <- domain_names[tidySet$signal.domain]
head(tidySet)
```

```
##   subject activity      signal axis    mean     std signal.domain
## 1       1   LAYING     BodyAcc    X -0.9391 -0.9244     frequency
## 2       1   LAYING     BodyAcc    Y -0.8671 -0.8336     frequency
## 3       1   LAYING     BodyAcc    Z -0.8827 -0.8129     frequency
## 4       1   LAYING BodyAccJerk    X -0.9571 -0.9642     frequency
## 5       1   LAYING BodyAccJerk    Y -0.9225 -0.9322     frequency
## 6       1   LAYING BodyAccJerk    Z -0.9481 -0.9606     frequency
```

Now, we will create a column whose values show if a signal corresponds to a 'Body' or 'Gravity' acceleration. To do this we first need to fix a typo in the values. To fix the typo, the word 'BodyBody' should be replaced by 'Body'.


```r
# Fix the signals with BodyBody names	
tidySet$signal <- str_replace(tidySet$signal, 'BodyBody', 'Body')

# Extract column with Body and Gravity 
y_position <- str_locate(tidySet$signal, 'y')
tidySet$acceleration.type <- str_sub(tidySet$signal, end = y_position[ , 1])
tidySet$signal <- str_sub(tidySet$signal, start = y_position[ , 1] + 1)
head(tidySet)
```

```
##   subject activity  signal axis    mean     std signal.domain
## 1       1   LAYING     Acc    X -0.9391 -0.9244     frequency
## 2       1   LAYING     Acc    Y -0.8671 -0.8336     frequency
## 3       1   LAYING     Acc    Z -0.8827 -0.8129     frequency
## 4       1   LAYING AccJerk    X -0.9571 -0.9642     frequency
## 5       1   LAYING AccJerk    Y -0.9225 -0.9322     frequency
## 6       1   LAYING AccJerk    Z -0.9481 -0.9606     frequency
##   acceleration.type
## 1              Body
## 2              Body
## 3              Body
## 4              Body
## 5              Body
## 6              Body
```

We can also differenciate if a signal comes from the accelerometer or gyroscope. This can be done by creating a column with the source of this signal.


```r
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
head(tidySet)
```

```
##   subject activity signal axis    mean     std signal.domain
## 1       1   LAYING   None    X -0.9391 -0.9244     frequency
## 2       1   LAYING   None    Y -0.8671 -0.8336     frequency
## 3       1   LAYING   None    Z -0.8827 -0.8129     frequency
## 4       1   LAYING   Jerk    X -0.9571 -0.9642     frequency
## 5       1   LAYING   Jerk    Y -0.9225 -0.9322     frequency
## 6       1   LAYING   Jerk    Z -0.9481 -0.9606     frequency
##   acceleration.type        source
## 1              Body Accelerometer
## 2              Body Accelerometer
## 3              Body Accelerometer
## 4              Body Accelerometer
## 5              Body Accelerometer
## 6              Body Accelerometer
```

The remaining values in the 'signal' column correspond to the calculated values of the main signals. Let's rename the 'signal' column with the 'calculated.signal', and change the abbreviated values to their full name


```r
# Replace abbreviated names
signal_names <- c('None' = 'None', 'Jerk' = 'Jerk.Vector', 
	'JerkMag' = 'Jerk.Vector.Magnitude', 'Mag' = 'Vector.Magnitude') 			

tidySet$signal <- signal_names[tidySet$signal]

tidySet$calculated.signal <- tidySet$signal
tidySet$signal <- NULL
head(tidySet)
```

```
##   subject activity axis    mean     std signal.domain acceleration.type
## 1       1   LAYING    X -0.9391 -0.9244     frequency              Body
## 2       1   LAYING    Y -0.8671 -0.8336     frequency              Body
## 3       1   LAYING    Z -0.8827 -0.8129     frequency              Body
## 4       1   LAYING    X -0.9571 -0.9642     frequency              Body
## 5       1   LAYING    Y -0.9225 -0.9322     frequency              Body
## 6       1   LAYING    Z -0.9481 -0.9606     frequency              Body
##          source calculated.signal
## 1 Accelerometer              None
## 2 Accelerometer              None
## 3 Accelerometer              None
## 4 Accelerometer       Jerk.Vector
## 5 Accelerometer       Jerk.Vector
## 6 Accelerometer       Jerk.Vector
```

Finally let's reorder the columns and sort them.


```r
# Shuffle columns
tidySet <- tidySet[c('subject','activity', 'source', 'acceleration.type', 'calculated.signal', 'signal.domain', 'axis', 'mean', 'std')]

# Sort the rows
tidySet <- arrange(tidySet, subject, activity, source, acceleration.type, calculated.signal, signal.domain, axis)
```

We have now obtained a tidy data set. The head and tail of this set are


```r
head(tidySet)
```

```
##   subject activity        source acceleration.type calculated.signal
## 1       1   LAYING Accelerometer              Body       Jerk.Vector
## 2       1   LAYING Accelerometer              Body       Jerk.Vector
## 3       1   LAYING Accelerometer              Body       Jerk.Vector
## 4       1   LAYING Accelerometer              Body       Jerk.Vector
## 5       1   LAYING Accelerometer              Body       Jerk.Vector
## 6       1   LAYING Accelerometer              Body       Jerk.Vector
##   signal.domain axis      mean     std
## 1     frequency    X -0.957074 -0.9642
## 2     frequency    Y -0.922463 -0.9322
## 3     frequency    Z -0.948061 -0.9606
## 4          time    X  0.081087 -0.9585
## 5          time    Y  0.003838 -0.9241
## 6          time    Z  0.010834 -0.9549
```


```r
tail(tidySet)
```

```
##      subject         activity    source acceleration.type
## 5935      30 WALKING_UPSTAIRS Gyroscope              Body
## 5936      30 WALKING_UPSTAIRS Gyroscope              Body
## 5937      30 WALKING_UPSTAIRS Gyroscope              Body
## 5938      30 WALKING_UPSTAIRS Gyroscope              Body
## 5939      30 WALKING_UPSTAIRS Gyroscope              Body
## 5940      30 WALKING_UPSTAIRS Gyroscope              Body
##      calculated.signal signal.domain axis     mean      std
## 5935              None     frequency    Z -0.31894 -0.25343
## 5936              None          time    X -0.00356 -0.49384
## 5937              None          time    Y -0.07796 -0.08405
## 5938              None          time    Z  0.08147 -0.21157
## 5939  Vector.Magnitude     frequency <NA> -0.44915 -0.15147
## 5940  Vector.Magnitude          time <NA> -0.11361 -0.16929
```

Finally, we save the tidy set in a text file.


```r
# Save tidySet in a text file
write.table(tidySet, file = 'tidySet.txt', sep = '\t', na = 'NA', col.names = TRUE, row.names = FALSE, quote = FALSE)
```
