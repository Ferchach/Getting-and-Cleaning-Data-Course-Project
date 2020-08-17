# change the directories according to the location of the dataset in your computer
infiles <- dir("C:/Users/PC/Documents/R Programming course/Project_cleaning_data/UCI HAR Dataset", full.name = T) 
infiles_train <- dir("C:/Users/PC/Documents/R Programming course/Project_cleaning_data/UCI HAR Dataset/train", full.names = T) 
infiles_test<- dir("C:/Users/PC/Documents/R Programming course/Project_cleaning_data/UCI HAR Dataset/test", full.names = T)

#loading the data
X_train <- read.delim(infiles_train[3], header = F)
X_test <- read.delim(infiles_test[3], header = F)
y_train <- read.delim(infiles_train[4], header = F)
y_test <- read.delim(infiles_test[4], header = F)
features <- read.delim(infiles[2], header = F, sep = " ")[, 2]
activities_labels <- read.delim(infiles[1], header = F, sep = " ")

#manipulating data
	# 1) merging the training ant testint data without lables
X_data <- rbind(X_train, X_test)

	# 2) delete the spaces at the beginig of each row
X_data <- sapply(X_data[[1]], trimws)

	# 3) create a temporary header for dataset
new_header <- sprintf("V%s", 1:561)
X_data <- as.data.frame(X_data)
	# 4) separate the only column into 561 columns(features)
X_data <- separate(X_data, X_data, new_header, sep = "\\s+")
colnames(X_data) <- features

	# 5) Extracts only the measurements on the mean and standard deviation for each measurement.
meanF <- grep("mean", colnames(X_data))
stdF <- grep("std", colnames(X_data))
mean_std <- sort(c(meanF, stdF))
X_data <- select(X_data, mean_std)
X_data <- sapply(X_data, as.numeric)
X_data <- as.data.frame(X_data)

	# 6) merging lables traing & testing data
y_data <- rbind(y_train, y_test)
	# 7) adding column y_data(id of activities) to X_data
tidy_data <- cbind(y_data, X_data)
activities <- merge(x = y_data, y = activities_labels, by = 'V1', all.x = T)[,2]

	# 8) Uses descriptive activity names to name the activities in the data set
tidy_data <- merge(x = tidy_data, y = activities_labels, by = 'V1', all.x = T)
tidy_data <- cbind(activities, tidy_data)
colnames(tidy_data) <- gsub('[-()]', '', colnames(tidy_data))


 #last step 
# loading the subjects data
subject_train <- read.delim(infiles_train[2], header = F, sep = ' ')
subject_test <- read.delim(infiles_test[2], header = F)
# joining the subject data
subject <- rbind(subject_train, subject_test)
# coerce the column of subject (numeric column create problem at group_by())
subject[, 1] <- as.character(subject[, 1])
colnames(subject) <- 'subject'
subject_act <- cbind(subject, y_data)
subject_act <- merge(x = subject_act, y = activities_labels, by = 'V1', all.x = T)
data_set <- cbind(subject = subject_act[, 2], tidy_data[, c(1, 3:81)])
data_set <- data_set %>% group_by(subject, activities) %>% summarize_all(mean)





