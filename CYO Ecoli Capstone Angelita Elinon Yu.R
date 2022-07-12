knitr::opts_chunk$set(echo = TRUE)
# Executive Summary
# Introduction
# Data 
## Preparation
# Install required packages and libraries.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr))
  install.packages("stringr", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
library(stringr)

# Download the Ecoli dataset
ecolidata <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/ecoli/ecoli.data"))
nrow(ecolidata)

#The downloaded Ecoli dataset is already a dataframe with 335 instances and one column only.
dim(ecolidata)
class(ecolidata)
head (ecolidata)

#There is a need to parse the contents of the one column to bring out the attributes 
#- eight predictors and one predicted value. 

#To do this, the str_split_fixed function is used from the stringr package  

#But the one column needs to be named in order to address it.  
#The one column is named "ecolivector" for this purpose.
colnames(ecolidata) <- c("ecolivector")

#Parse ecolivector
ecolisplit<-str_split_fixed(ecolidata$ecolivector, "  ", 9)

#After parsing or splitting the column, the dataset still has 335 instances but with 9 columns.
dim(ecolisplit)

#The parsed dataset, however, has now become a matrix with each row considered an array.
class(ecolisplit)

#The parsed dataset is transformed back to a dataframe as follows.  It has 335 rows and 9 columns.
ecolidataframe<- as.data.frame(ecolisplit)
dim(ecolidataframe)
class(ecolidataframe)

#The columns have no names and are by default generically referred to as V1 to V9.  
head(ecolidataframe)

#The column names are assigned as follows:
colnames(ecolidataframe) <- c("seqname", "mcg", "gvh", "lip", "chg", "aac", "alm1", "alm2", "locsite")

#The dataset that has now been transformed back to a dataframe still has 335 rows and 9 columns.  
#But this time, the columns are properly named.
dim(ecolidataframe)
class(ecolidataframe)
head(ecolidataframe)
str(ecolidataframe)

## Analysis, Visualisation, and Insights

# remaining data issues
ecolidataframe[47,]
ecolidataframe %>% group_by(locsite) %>% summarize(count=n())

#Values of row 47 are corrected as follows:
# replace values of instance 47
ecolidataframe[47,2]= 0.43
ecolidataframe[47,3]= 0.40
ecolidataframe[47,4]= 0.48
ecolidataframe[47,5]= 0.50
ecolidataframe[47,6]= 0.39
ecolidataframe[47,7]= 0.28
ecolidataframe[47,8]= 0.39
ecolidataframe[47,9]= "cp"
ecolidataframe[47,]
# Row 47 values have been corrected and the only remaining data issue is the leading space in certain locsites.
ecolidataframe %>% group_by(locsite) %>% summarize(count=n())
# The space in front of some locsites is removed as follows:
# remove space in front of some locsites
ecolidataframe$locsite<-trimws(ecolidataframe$locsite, which = c("left"))
ecolidataframe %>% group_by(locsite) %>% summarize(count=n())

#All data issues have been resolved.
# data cleansing check
dim(ecolidataframe)
class(ecolidataframe)
head (ecolidataframe)
str(ecolidataframe)

##Training and Test Data Set Partitions

#The ecolidataframe is partitioned into training and test data sets as follows:
#Partition into training and test data sets
set.seed(1)
# Since the original data set is only 335 instances, allocate 80% for training and 
# 20% for testing to allow more opportunity to test and validate the model.
test_index <- createDataPartition(y=ecolidataframe$locsite, times = 1, p = 0.2, list = FALSE)
ecoli_train <- ecolidataframe[-test_index,]
ecoli_test <- ecolidataframe[test_index,]

#The ecoli training and testing data sets resulting from partitioning the ecolidataframe are now ready for analysis and model development.
#They have the following number of instances:
nrow (ecoli_train)
nrow (ecoli_test)

## Analysis

### Predicted Variable
# The predicted variable is locsite.  The possible locsites defined in an earlier section are distributed as follows in the train set.
# locsite distribution in training set
ecoli_train %>% group_by(locsite) %>% summarize(count = n()) %>% arrange(desc(count))

# The distribution shows that the top 5 locsites are cp, im, pp, imU, and om.  The first in rank, cp, has almost double the frequency of the second in rank, im, indicating a significant lead.

### Predictor Variables

# The eight candidate predictor variables - seqname, mcg, gvh, lip, chg, aac, alm1, alm2 - are defined in an earlier section.  Except for the seqname, all predictor variables represent numeric scores that indicate signal sequence, consensus sequence, presence of charge, discriminant analysis of the amino acid content, ALOM membrane, and ALOM program.

# 1. seqname
# distinct seqname
n_distinct(ecoli_train$seqname)
n_distinct(ecolidataframe$seqname)

# 2. mcg
# distinct mcg summary
n_distinct(ecoli_train$mcg)
ecoli_train %>% group_by(mcg) %>% summarize(count = n()) %>% arrange (desc(count))

# 3. gvh
# distinct gvh summary
n_distinct(ecoli_train$gvh)
ecoli_train %>% group_by(gvh) %>% summarize(count = n()) %>% arrange (desc(count))

# 4. lip
# distinct lip summary
n_distinct(ecoli_train$lip)
ecoli_train %>% group_by(lip) %>% summarize(count = n()) %>% arrange (desc(count))

# 5. chg 
# distinct chg summary
n_distinct(ecoli_train$chg)
ecoli_train %>% group_by(chg) %>% summarize(count = n()) %>% arrange (desc(count))

# 6. aac
# distinct aac summary
n_distinct(ecoli_train$aac)
ecoli_train %>% group_by(aac) %>% summarize(count = n()) %>% arrange (desc(count))

# 7. alm1
# distinct alm1 summary}
n_distinct(ecoli_train$alm1)
ecoli_train %>% group_by(alm1) %>% summarize(count = n()) %>% arrange (desc(count))

# 8. alm2
# distinct alm2 summary}
n_distinct(ecoli_train$alm2)
ecoli_train %>% group_by(alm2) %>% summarize(count = n()) %>% arrange (desc(count))

## Visualisation and Insights
# To visually appreciate the content of the train data set on which the machine learning algorithm will be modelled, the visual relationships between the predicted and predictor variables in the ecoli train data set are shown in this section.
# However, this data visualisation exercise is not done for the ecoli test data set because the ecoli test data set is supposed to be of an "unknown" nature and will be used to evaluate the performance of the model developed from the ecoli train data set.

## Data Subsets by whether Locsite was Predicted or Not

#1.  cp or not cp
#Subset data set that predicted locsite cp
ecoli_train_cp<- ecoli_train %>% filter(locsite == "cp")
nrow(ecoli_train_cp)
head(ecoli_train_cp)
#Subset data set that did not predict locsite cp
ecoli_train_not_cp<- ecoli_train %>% filter(locsite != "cp")
nrow(ecoli_train_not_cp)
head(ecoli_train_not_cp)

#2.  im or not im
#Subset data set that predicted locsite im
ecoli_train_im<- ecoli_train %>% filter(locsite == "im")
nrow(ecoli_train_im)
head(ecoli_train_im)
#Subset data set that did not predict locsite im
ecoli_train_not_im<- ecoli_train %>% filter(locsite != "im")
nrow(ecoli_train_not_im)
head(ecoli_train_not_im)

#3.  pp or not pp
#Subset data set that predicted locsite pp
ecoli_train_pp<- ecoli_train %>% filter(locsite == "pp")
nrow(ecoli_train_pp)
head(ecoli_train_pp)
#Subset data set that did not predict locsite pp
ecoli_train_not_pp<- ecoli_train %>% filter(locsite != "pp")
nrow(ecoli_train_not_pp)
head(ecoli_train_not_pp)

#4.  imU or not imU
#Subset data set that predicted locsite imU
ecoli_train_imU<- ecoli_train %>% filter(locsite == "imU")
nrow(ecoli_train_imU)
head(ecoli_train_imU)
#Subset data set that did not predict locsite imU
ecoli_train_not_imU<- ecoli_train %>% filter(locsite != "imU")
nrow(ecoli_train_not_imU)
head(ecoli_train_not_imU)

#5.  om or not om
#Subset data set that predicted locsite om
ecoli_train_om<- ecoli_train %>% filter(locsite == "om")
nrow(ecoli_train_om)
head(ecoli_train_om)
#Subset data set that did not predict locsite om
ecoli_train_not_om<- ecoli_train %>% filter(locsite != "om")
nrow(ecoli_train_not_om)
head(ecoli_train_not_om)

#6.  omL or not omL
#Subset data set that predicted locsite omL
ecoli_train_omL<- ecoli_train %>% filter(locsite == "omL")
nrow(ecoli_train_omL)
head(ecoli_train_omL)
#Subset data set that did not predict locsite omL
ecoli_train_not_omL<- ecoli_train %>% filter(locsite != "omL")
nrow(ecoli_train_not_omL)
head(ecoli_train_not_omL)

#7.  imL or not imL
#Subset data set that predicted locsite imL
ecoli_train_imL<- ecoli_train %>% filter(locsite == "imL")
nrow(ecoli_train_imL)
head(ecoli_train_imL)
#Subset data set that did not predict locsite imL
ecoli_train_not_imL<- ecoli_train %>% filter(locsite != "imL")
nrow(ecoli_train_not_imL)
head(ecoli_train_not_imL)

#8.  imS or not imS
#Subset data set that predicted locsite imS
ecoli_train_imS<- ecoli_train %>% filter(locsite == "imS")
nrow(ecoli_train_imS)
head(ecoli_train_imS)
#Subset data set that did not predict locsite imS
ecoli_train_not_imS<- ecoli_train %>% filter(locsite != "imS")
nrow(ecoli_train_not_imS)
head(ecoli_train_not_imS)

## Predictor Profile vis-a-vis Predicted Locsite

#1.  mcg vs. cp and not cp

hist(as.numeric(ecoli_train_cp$mcg))
hist(as.numeric(ecoli_train_not_cp$mcg))

#2.  mcg vs. im and not ims

hist(as.numeric(ecoli_train_im$mcg))
hist(as.numeric(ecoli_train_not_im$mcg))

#3.  mcg vs. pp and not pp

hist(as.numeric(ecoli_train_pp$mcg))
hist(as.numeric(ecoli_train_not_pp$mcg))

#4.  mcg vs. imU and not imU

hist(as.numeric(ecoli_train_imU$mcg))
hist(as.numeric(ecoli_train_not_imU$mcg))

#5.  mcg vs. om and not om

hist(as.numeric(ecoli_train_om$mcg))
hist(as.numeric(ecoli_train_not_om$mcg))

#6.  mcg vs. omL and not omL

hist(as.numeric(ecoli_train_omL$mcg))
hist(as.numeric(ecoli_train_not_omL$mcg))

#7.  mcg vs. imL and not imL

hist(as.numeric(ecoli_train_imL$mcg))
hist(as.numeric(ecoli_train_not_imL$mcg))

#8.  mcg vs. imS and not imS

hist(as.numeric(ecoli_train_imS$mcg))
hist(as.numeric(ecoli_train_not_imS$mcg))

#9.  gvh vs. cp and not cp

hist(as.numeric(ecoli_train_cp$gvh))
hist(as.numeric(ecoli_train_not_cp$gvh))

#10. gvh vs. im and not im

hist(as.numeric(ecoli_train_im$gvh))
hist(as.numeric(ecoli_train_not_im$gvh))

#11. gvh vs. pp and not pp

hist(as.numeric(ecoli_train_pp$gvh))
hist(as.numeric(ecoli_train_not_pp$gvh))

#12. gvh vs. imU and not imU

hist(as.numeric(ecoli_train_imU$gvh))
hist(as.numeric(ecoli_train_not_imU$gvh))

#13. gvh vs. om and not om

hist(as.numeric(ecoli_train_om$gvh))
hist(as.numeric(ecoli_train_not_om$gvh))

#14. gvh vs. omL and not omL

hist(as.numeric(ecoli_train_omL$gvh))
hist(as.numeric(ecoli_train_not_omL$gvh))

#15. gvh vs. imL and not imL

hist(as.numeric(ecoli_train_imL$gvh))
hist(as.numeric(ecoli_train_not_imL$gvh))

#16. gvh vs. imS and not imS

hist(as.numeric(ecoli_train_imS$gvh))
hist(as.numeric(ecoli_train_not_imS$gvh))

#17. lip vs. cp and not cp

hist(as.numeric(ecoli_train_cp$lip))
hist(as.numeric(ecoli_train_not_cp$lip))

#18. lip vs. im and not im

hist(as.numeric(ecoli_train_im$lip))
hist(as.numeric(ecoli_train_not_im$lip))

#19. lip vs. pp and not pp

hist(as.numeric(ecoli_train_pp$lip))
hist(as.numeric(ecoli_train_not_pp$lip))

#20. lip vs. imU and not imU

hist(as.numeric(ecoli_train_imU$lip))
hist(as.numeric(ecoli_train_not_imU$lip))

#21. lip vs. om and not om

hist(as.numeric(ecoli_train_om$lip))
hist(as.numeric(ecoli_train_not_om$lip))

#22. lip vs. omL and not omL

hist(as.numeric(ecoli_train_omL$lip))
hist(as.numeric(ecoli_train_not_omL$lip))

#23. lip vs. imL and not imL

hist(as.numeric(ecoli_train_imL$lip))
hist(as.numeric(ecoli_train_not_imL$lip))

#24. lip vs. imS and not imS

hist(as.numeric(ecoli_train_imS$lip))
hist(as.numeric(ecoli_train_not_imS$lip))

#25. chg vs. cp and not cp

hist(as.numeric(ecoli_train_cp$chg))
hist(as.numeric(ecoli_train_not_cp$chg))

#26. chg vs. im and not im

hist(as.numeric(ecoli_train_im$chg))
hist(as.numeric(ecoli_train_not_im$chg))

#27. chg vs. pp and not pp

hist(as.numeric(ecoli_train_pp$chg))
hist(as.numeric(ecoli_train_not_pp$chg))

#28. chg vs. imU and not imU

hist(as.numeric(ecoli_train_imU$chg))
hist(as.numeric(ecoli_train_not_imU$chg))

#29. chg vs. om and not om

hist(as.numeric(ecoli_train_om$chg))
hist(as.numeric(ecoli_train_not_om$chg))

#30. chg vs. omL and not omL

hist(as.numeric(ecoli_train_omL$chg))
hist(as.numeric(ecoli_train_not_omL$chg))

#31. chg vs. imL and not imL

hist(as.numeric(ecoli_train_imL$chg))
hist(as.numeric(ecoli_train_not_imL$chg))

#32. chg vs. imS and not imS

hist(as.numeric(ecoli_train_imS$chg))
hist(as.numeric(ecoli_train_not_imS$chg))

#33. aac vs. cp and not cp

hist(as.numeric(ecoli_train_cp$aac))
hist(as.numeric(ecoli_train_not_cp$aac))

#34. aac vs. im and not im

hist(as.numeric(ecoli_train_im$aac))
hist(as.numeric(ecoli_train_not_im$aac))

#35. aac vs. pp and not pp

hist(as.numeric(ecoli_train_pp$aac))
hist(as.numeric(ecoli_train_not_pp$aac))

#36. aac vs. imU and not imU

hist(as.numeric(ecoli_train_imU$aac))
hist(as.numeric(ecoli_train_not_imU$aac))

#37. aac vs. om and not om

hist(as.numeric(ecoli_train_om$aac))
hist(as.numeric(ecoli_train_not_om$aac))

#38. aac vs. omL and not omL

hist(as.numeric(ecoli_train_omL$aac))
hist(as.numeric(ecoli_train_not_omL$aac))

#39. aac vs. imL and not imL

hist(as.numeric(ecoli_train_imL$aac))
hist(as.numeric(ecoli_train_not_imL$aac))

#40. aac vs. imS and not imS

hist(as.numeric(ecoli_train_imS$aac))
hist(as.numeric(ecoli_train_not_imS$aac))

#41. alm1 vs. cp and not cp

hist(as.numeric(ecoli_train_cp$alm1))
hist(as.numeric(ecoli_train_not_cp$alm1))

#42. alm1 vs. im and not im

hist(as.numeric(ecoli_train_im$alm1))
hist(as.numeric(ecoli_train_not_im$alm1))

#43. alm1 vs. pp and not pp

hist(as.numeric(ecoli_train_pp$alm1))
hist(as.numeric(ecoli_train_not_pp$alm1))

#44. alm1 vs. imU and not imU

hist(as.numeric(ecoli_train_imU$alm1))
hist(as.numeric(ecoli_train_not_imU$alm1))

#45. alm1 vs. om and not om

hist(as.numeric(ecoli_train_om$alm1))
hist(as.numeric(ecoli_train_not_om$alm1))

#46. alm1 vs. omL and not omL

hist(as.numeric(ecoli_train_omL$alm1))
hist(as.numeric(ecoli_train_not_omL$alm1))

#47. alm1 vs. imL and not imL

hist(as.numeric(ecoli_train_imL$alm1))
hist(as.numeric(ecoli_train_not_imL$alm1))

#48. alm1 vs. imS and not imS

hist(as.numeric(ecoli_train_imS$alm1))
hist(as.numeric(ecoli_train_not_imS$alm1))

#49. alm2 vs. cp and not cp

hist(as.numeric(ecoli_train_cp$alm2))
hist(as.numeric(ecoli_train_not_cp$alm2))

#50. alm2 vs. im and not im

hist(as.numeric(ecoli_train_im$alm2))
hist(as.numeric(ecoli_train_not_im$alm2))

#51. alm2 vs. pp and not pp

hist(as.numeric(ecoli_train_pp$alm2))
hist(as.numeric(ecoli_train_not_pp$alm2))

#52. alm2 vs. imU and not imU

hist(as.numeric(ecoli_train_imU$alm2))
hist(as.numeric(ecoli_train_not_imU$alm2))

#53. alm2 vs. om and not om

hist(as.numeric(ecoli_train_om$alm2))
hist(as.numeric(ecoli_train_not_om$alm2))

#54. alm2 vs. omL and not omL

hist(as.numeric(ecoli_train_omL$alm2))
hist(as.numeric(ecoli_train_not_omL$alm2))

#55. alm2 vs. imL and not imL

hist(as.numeric(ecoli_train_imL$alm2))
hist(as.numeric(ecoli_train_not_imL$alm2))

#56. alm2 vs. imS and not imS

hist(as.numeric(ecoli_train_imS$alm2))
hist(as.numeric(ecoli_train_not_imS$alm2))

# PROPOSED MODELING APPROACHES

## Baseline Accuracy: 81%

## Remove seqname from Ecoli Train and Test Data Sets
ecoli_train_seqname_out <- subset(ecoli_train, select = -c(seqname))

ecoli_test_seqname_out <- subset(ecoli_test, select = -c(seqname))

head(ecoli_train_seqname_out)
head(ecoli_test_seqname_out)

## Model 1: Cross-Validated K Nearest Neighbours (CV KNN)
### Model Development
set.seed(8)
train_knn_cv <- train(locsite ~., 
                      method = "knn", 
                      data = ecoli_train_seqname_out, 
                      tuneGrid = data.frame(k = seq(3, 51, 2)), 
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))

#The best value for k that maximizes accuracy is:
train_knn_cv$bestTune

#The code: "knn_cv_preds <- predict(train_knn_cv, ecoli_test_seqname_out)" results into an error that reveals that the ecoli_test_seqname_out has factors, such as mcg, that contain new levels that are not in ecoli_train_seqname_out.  
#To correct this error, set all observations in the ecoli test data set that represent the level that does not exist in the ecoli train data set to "NA"
ecoli_test_seqname_out_new<- ecoli_test_seqname_out
ecoli_test_seqname_out_new$mcg[which(!(ecoli_test_seqname_out_new$mcg %in% unique (ecoli_train_seqname_out$mcg)))] <- NA 
ecoli_test_seqname_out_new$gvh[which(!(ecoli_test_seqname_out_new$gvh %in% unique (ecoli_train_seqname_out$gvh)))] <- NA 
ecoli_test_seqname_out_new$lip[which(!(ecoli_test_seqname_out_new$lip %in% unique (ecoli_train_seqname_out$lip)))] <- NA 
ecoli_test_seqname_out_new$chg[which(!(ecoli_test_seqname_out_new$chg %in% unique (ecoli_train_seqname_out$chg)))] <- NA 
ecoli_test_seqname_out_new$aac[which(!(ecoli_test_seqname_out_new$aac %in% unique (ecoli_train_seqname_out$aac)))] <- NA 
ecoli_test_seqname_out_new$alm1[which(!(ecoli_test_seqname_out_new$alm1 %in% unique (ecoli_train_seqname_out$alm1)))] <- NA 
ecoli_test_seqname_out_new$alm2[which(!(ecoli_test_seqname_out_new$alm2 %in% unique (ecoli_train_seqname_out$alm2)))] <- NA 

# As shown below, the ecoli_test_seqname_out_new now contains the <NA> that replaced the values of the levels in the predictor variable that cannot be found in the ecoli_train_seqname_out data set for the same predictor variable.
ecoli_test_seqname_out_new 

#The same prediction code that resulted in an error earlier is tried again below with the updated test data set.  No error was generated. 
knn_cv_preds <- predict(train_knn_cv, ecoli_test_seqname_out_new)
knn_cv_preds

#Accurancy of CV KNN Model
mean(knn_cv_preds == ecoli_test_seqname_out_new$locsite)


## Model 2: Random Forest
### Model Development
set.seed(14)
train_rf <- train(locsite ~., 
                  data = ecoli_train_seqname_out, 
                  method = "rf", 
                  ntree = 100, 
                  tuneGrid = data.frame(mtry = seq(1:7)))

#The best value for mtry that maximizes accuracy is:
train_rf$bestTune

#Apply the random forest model, train_rf, to the ecoli test data set.

#The ecoli_test_seqname_out_new is used also for the random forest prediction otherwise 
#the same error is encountered.  The error detects that there are new levels in the test 
#data set that are not in the train data set.

#There is also a need to set these new levels to NA as was done in ecoli_test_seqname_out_new 
#earlier generated for the CV KNN model development.

rf_preds <- predict(train_rf, ecoli_test_seqname_out_new)
rf_preds
#Accuracy of random forest
mean(rf_preds == ecoli_test_seqname_out$locsite)

#Most important variable
varImp(train_rf)

## Comparison of Modeling Approaches
# CV KNN Predictions
knn_cv_preds
# Random Forest Predictions
rf_preds
#Actual Test Data
ecoli_test_seqname_out_new$locsite
#Full Actual Test Data to show NA replacement
ecoli_test_seqname_out_new
nrow(ecoli_test_seqname_out_new)

## Re-calculation of CV KNN and Random Forest Accuracy Based on Removal of 23 NA Rows
NA_removed_test <- ecoli_test_seqname_out_new[-c(3,5,9,11,17,19,21,23,25,26,27,31,41,44,46,51,52,55,58,61,66,68,69), ]
NA_removed_test
nrow(NA_removed_test)
# CV KNN
knn_cv_preds_NA_removed <- predict(train_knn_cv, NA_removed_test)
knn_cv_preds_NA_removed
mean(knn_cv_preds_NA_removed == NA_removed_test$locsite)

# Random Forest
rf_preds_NA_removed <- predict(train_rf, NA_removed_test)
rf_preds_NA_removed
#Accuracy of random forest
mean(rf_preds_NA_removed == NA_removed_test$locsite)

#Actual Test Data NA Removed
NA_removed_test$locsite



