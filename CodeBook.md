# Code Book

Files to read:

- activity_labels.txt contains types of activities
- features.txt contains names of the features
- X_train.txt contains variables for features for training
- y_train.txt contains the activities numbers corresponding to X_train.txt
- subject_train.txt contains information on the subjects for train data
- X_test.txt contains variables for features for testing
- y_test.txt contains the activities numbers corresponding to X_test.txt
- subject_test.txt contains information on the subjects for test data

##Names of the columns in the result dataframe are corrected to be as selfdescriptive as possible:

- "Activity"
- "Subject"
- "TimeBodyAccelelerometerMean()-X"
- "TimeBodyAccelelerometerMean()-Y"
- "TimeBodyAccelelerometerMean()-Z"
- "TimeGravityAccelelerometerMean()-X"
- "TimeGravityAccelelerometerMean()-Y"
- "TimeGravityAccelelerometerMean()-Z"
- "TimeBodyAccelelerometerJerkMean()-X"
- "TimeBodyAccelelerometerJerkMean()-Y"
- "TimeBodyAccelelerometerJerkMean()-Z"
- "TimeBodyGyroscopeMean()-X"
- "TimeBodyGyroscopeMean()-Y"
- "TimeBodyGyroscopeMean()-Z"
- "TimeBodyGyroscopeJerkMean()-X"
- "TimeBodyGyroscopeJerkMean()-Y"
- "TimeBodyGyroscopeJerkMean()-Z"
- "TimeBodyAccelelerometerMagnitudeMean()"
- "TimeGravityAccelelerometerMagnitudeMean()"
- "TimeBodyAccelelerometerJerkMagnitudeMean()"
- "TimeBodyGyroscopeMagnitudeMean()"
- "TimeBodyGyroscopeJerkMagnitudeMean()"
- "FrequencyBodyAccelelerometerMean()-X"
- "FrequencyBodyAccelelerometerMean()-Y"
- "FrequencyBodyAccelelerometerMean()-Z"
- "FrequencyBodyAccelelerometerMeanFrequency()-X"
- "FrequencyBodyAccelelerometerMeanFrequency()-Y"
- "FrequencyBodyAccelelerometerMeanFrequency()-Z"
- "FrequencyBodyAccelelerometerJerkMean()-X"
- "FrequencyBodyAccelelerometerJerkMean()-Y"
- "FrequencyBodyAccelelerometerJerkMean()-Z"
- "FrequencyBodyAccelelerometerJerkMeanFrequency()-X"
- "FrequencyBodyAccelelerometerJerkMeanFrequency()-Y"
- "FrequencyBodyAccelelerometerJerkMeanFrequency()-Z"
- "FrequencyBodyGyroscopeMean()-X"
- "FrequencyBodyGyroscopeMean()-Y"
- "FrequencyBodyGyroscopeMean()-Z"
- "FrequencyBodyGyroscopeMeanFrequency()-X"
- "FrequencyBodyGyroscopeMeanFrequency()-Y"
- "FrequencyBodyGyroscopeMeanFrequency()-Z"
- "FrequencyBodyAccelelerometerMagnitudeMean()"
- "FrequencyBodyAccelelerometerMagnitudeMeanFrequency()"
- "FrequencyBodyAccelelerometerJerkMagnitudeMean()"
- "FrequencyBodyAccelelerometerJerkMagnitudeMeanFrequency()"
- "FrequencyBodyGyroscopeMagnitudeMean()"
- "FrequencyBodyGyroscopeMagnitudeMeanFrequency()"
- "FrequencyBodyGyroscopeJerkMagnitudeMean()"
- "FrequencyBodyGyroscopeJerkMagnitudeMeanFrequency()"
- "Angle(BodyAccelelerometerMean,Gravity)"
- "Angle(BodyAccelelerometerJerkMean),GravityMean)"
- "Angle(BodyGyroscopeMean,GravityMean)"
- "Angle(BodyGyroscopeJerkMean,GravityMean)"
- "Angle(X,GravityMean)"
- "Angle(Y,GravityMean)"
- "Angle(Z,GravityMean)"
- "TimeBodyAccelelerometerStandardDeviation()-X"
- "TimeBodyAccelelerometerStandardDeviation()-Y"
- "TimeBodyAccelelerometerStandardDeviation()-Z"
- "TimeGravityAccelelerometerStandardDeviation()-X"
- "TimeGravityAccelelerometerStandardDeviation()-Y"
- "TimeGravityAccelelerometerStandardDeviation()-Z"
- "TimeBodyAccelelerometerJerkStandardDeviation()-X"
- "TimeBodyAccelelerometerJerkStandardDeviation()-Y"
- "TimeBodyAccelelerometerJerkStandardDeviation()-Z"
- "TimeBodyGyroscopeStandardDeviation()-X"
- "TimeBodyGyroscopeStandardDeviation()-Y"
- "TimeBodyGyroscopeStandardDeviation()-Z"
- "TimeBodyGyroscopeJerkStandardDeviation()-X"
- "TimeBodyGyroscopeJerkStandardDeviation()-Y"
- "TimeBodyGyroscopeJerkStandardDeviation()-Z"
- "TimeBodyAccelelerometerMagnitudeStandardDeviation()"
- "TimeGravityAccelelerometerMagnitudeStandardDeviation()"
- "TimeBodyAccelelerometerJerkMagnitudeStandardDeviation()"
- "TimeBodyGyroscopeMagnitudeStandardDeviation()"
- "TimeBodyGyroscopeJerkMagnitudeStandardDeviation()"
- "FrequencyBodyAccelelerometerStandardDeviation()-X"
- "FrequencyBodyAccelelerometerStandardDeviation()-Y"
- "FrequencyBodyAccelelerometerStandardDeviation()-Z"
- "FrequencyBodyAccelelerometerJerkStandardDeviation()-X"
- "FrequencyBodyAccelelerometerJerkStandardDeviation()-Y"
- "FrequencyBodyAccelelerometerJerkStandardDeviation()-Z"
- "FrequencyBodyGyroscopeStandardDeviation()-X"
- "FrequencyBodyGyroscopeStandardDeviation()-Y"
- "FrequencyBodyGyroscopeStandardDeviation()-Z"
- "FrequencyBodyAccelelerometerMagnitudeStandardDeviation()"
- "FrequencyBodyAccelelerometerJerkMagnitudeStandardDeviation()"
- "FrequencyBodyGyroscopeMagnitudeStandardDeviation()"
- "FrequencyBodyGyroscopeJerkMagnitudeStandardDeviation()"

##Tidy data are in tidy.txt file. There are 88 means for every combination of activity and subject possible.

