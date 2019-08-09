#------------------------------------------------------------------------------
#AmExpert decipHER - Women Machine Learning Hackathon
#------------------------------------------------------------------------------

# load required libraries

load.libraries <- c('data.table', 'dplyr', 'MASS', 'dummies','car','caTools',
                    'pls','lubridate', 'stringr', 'tidyr', 'ggplot2',
                    'ridge','glmnet','h2o')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]

for(libs in install.lib) install.packages(libs, dependencies = TRUE)
sapply(load.libraries, require, character = TRUE)

# Read dataset - train and test
train <- fread('train.csv')
test <- fread('test_9K3DBWQ.csv')

# Preliminary analysis
head(train)
str(train)
summary(train)
dim(train) # 32820 * 44

head(test)
str(test)
summary(test)
dim(test) # 14047 * 43

# Let's merge train and test to begin with data prep.
# Add a new column is_train to differentiate between train and test records
test$cc_cons <- 0
train$is_train <- TRUE
test$is_train <- FALSE


dataset_1 <- rbind(train,test)

#Missing values - multiple columns with NA values.
#Let's remove the columns having more than 11 % of missing values for analysis

sapply(dataset_1, function(x) sum(is.na(x))/nrow(dataset_1)*100)

dataset <- dataset_1[,c(1:6,8,10,12:14,18,27:45)]

sapply(dataset, function(x) sum(is.na(x))/nrow(dataset)*100)

# duplicate rows - none
uniqueN(dataset)

# ------------------------ univariate analysis -------------------------

# id - unique_ID # Will drop this column before model building
uniqueN(dataset$id)
head(dataset$id)

#dataset$reservation_id <- NULL

# account type - majority is of current account type
table(dataset$account_type)

ggplot(data = dataset,aes(x = account_type)) + geom_bar(color = 'black')

# gender - majority are male
table(dataset$gender)

ggplot(data = dataset,aes(x = gender)) + geom_bar(color = 'black')

# age - majority is of current account type
table(dataset$age)

ggplot(data = dataset,aes(x = age)) + geom_histogram(color = 'black')

# there are few error records having age more than 100 years.
# let's impute with them the median value
median(dataset$age)
dataset[which(dataset$age > 100),'age'] <- median(dataset$age)

# let's prepare age_bin for further analysis
dataset[which(dataset$age >= 20 & dataset$age < 30),'age_bin'] <- '20-30'
dataset[which(dataset$age >= 30 & dataset$age < 40),'age_bin'] <- '30-40'
dataset[which(dataset$age >= 40 & dataset$age < 50),'age_bin'] <- '40-50'
dataset[which(dataset$age >= 50 & dataset$age < 60),'age_bin'] <- '50-60'
dataset[which(dataset$age >= 60 & dataset$age < 70),'age_bin'] <- '60-70'
dataset[which(dataset$age >= 70),'age_bin'] <- '70+'

table(dataset$age_bin)

ggplot(data = dataset,aes(x = age_bin)) + geom_bar(color = 'black')

# region_code
table(dataset$region_code)

#cc_cons_apr
summary(dataset$cc_cons_apr)

ggplot(data = dataset,aes(x = cc_cons_apr)) + geom_histogram(color = 'black')

#outlier detection - at 99 to 100 %. let's cap it.
quantile(dataset$cc_cons_apr,seq(0,1,0.01))
#dataset[which(dataset$cc_cons_apr > 112452),'cc_cons_apr']<- 112452

#cc_cons_may
summary(dataset$cc_cons_may)

ggplot(data = dataset,aes(x = cc_cons_may)) + geom_histogram(color = 'black')

#outlier detection - at 99 to 100 %. let's cap it.
quantile(dataset$cc_cons_may,seq(0,1,0.01))
#dataset[which(dataset$cc_cons_may > 103826.1342),'cc_cons_may']<- 103826.1342

#cc_cons_jun
summary(dataset$cc_cons_jun)

ggplot(data = dataset,aes(x = cc_cons_jun)) + geom_histogram(color = 'black')

#outlier detection - at 99 to 100 %. let's cap it.
quantile(dataset$cc_cons_jun,seq(0,1,0.01))
#dataset[which(dataset$cc_cons_jun > 71226.882),'cc_cons_jun']<- 71226.882

#cc_count_apr - let's impute NA with median value
table(dataset$cc_count_apr)
median(dataset$cc_count_apr,na.rm = T)

dataset[which(is.na(dataset$cc_count_apr)),'cc_count_apr']<- median(dataset$cc_count_apr,na.rm = T)

#cc_count_may - let's impute NA with median value
table(dataset$cc_count_may)
median(dataset$cc_count_may,na.rm = T)

dataset[which(is.na(dataset$cc_count_may)),'cc_count_may']<- median(dataset$cc_count_may,na.rm = T)

#cc_count_may - let's impute NA with median value
table(dataset$cc_count_jun)
median(dataset$cc_count_jun,na.rm = T)

dataset[which(is.na(dataset$cc_count_jun)),'cc_count_jun']<- median(dataset$cc_count_jun,na.rm = T)

#card_lim
summary(dataset$card_lim)

dataset[which(is.na(dataset$card_lim)),'card_lim']<- median(dataset$card_lim,na.rm = T)

quantile(dataset$card_lim,seq(0,1,0.01))

#dataset[which(dataset$card_lim > 600000),'card_lim'] <- 600000

#debit_amount_apr
summary(dataset$debit_amount_apr)

dataset[which(is.na(dataset$debit_amount_apr)),'debit_amount_apr']<- median(dataset$debit_amount_apr,na.rm = T)

quantile(dataset$debit_amount_apr,seq(0,1,0.01))

#dataset[which(dataset$debit_amount_apr > 489746.369),'debit_amount_apr'] <- 489746.369

#debit_amount_may
summary(dataset$debit_amount_may)

dataset[which(is.na(dataset$debit_amount_may)),'debit_amount_may']<- median(dataset$debit_amount_may,na.rm = T)

quantile(dataset$debit_amount_may,seq(0,1,0.01))

#dataset[which(dataset$debit_amount_may > 438972.072),'debit_amount_may'] <- 438972.072

#debit_amount_june
summary(dataset$debit_amount_jun)

dataset[which(is.na(dataset$debit_amount_jun)),'debit_amount_jun']<- median(dataset$debit_amount_jun,na.rm = T)

quantile(dataset$debit_amount_jun,seq(0,1,0.01))

#dataset[which(dataset$debit_amount_jun > 466250.708),'debit_amount_jun'] <- 466250.708

#credit_amount_apr
summary(dataset$credit_amount_apr)

dataset[which(is.na(dataset$credit_amount_apr)),'credit_amount_apr']<- median(dataset$credit_amount_apr,na.rm = T)

quantile(dataset$credit_amount_apr,seq(0,1,0.01))

#dataset[which(dataset$credit_amount_apr > 506270.755),'credit_amount_apr'] <- 506270.755

#credit_amount_may
summary(dataset$credit_amount_may)

dataset[which(is.na(dataset$credit_amount_may)),'credit_amount_may']<- median(dataset$credit_amount_may,na.rm = T)

quantile(dataset$credit_amount_may,seq(0,1,0.01))

#dataset[which(dataset$credit_amount_may > 4.790300e+05),'credit_amount_may'] <- 4.790300e+05

#credit_amount_jun
summary(dataset$credit_amount_jun)

dataset[which(is.na(dataset$credit_amount_jun)),'credit_amount_jun']<- median(dataset$credit_amount_jun,na.rm = T)

quantile(dataset$credit_amount_jun,seq(0,1,0.01))

#dataset[which(dataset$credit_amount_jun > 4.962683e+05),'credit_amount_jun'] <- 4.962683e+05

#debit_count_apr
summary(dataset$debit_count_apr)

dataset[which(is.na(dataset$debit_count_apr)),'debit_count_apr']<- median(dataset$debit_count_apr,na.rm = T)

quantile(dataset$debit_count_apr,seq(0,1,0.01))

#dataset[which(dataset$debit_count_apr > 53),'debit_count_apr'] <- 53

#debit_count_may
summary(dataset$debit_count_may)

dataset[which(is.na(dataset$debit_count_may)),'debit_count_may']<- median(dataset$debit_count_may,na.rm = T)

quantile(dataset$debit_count_may,seq(0,1,0.01))

#dataset[which(dataset$debit_count_may > 54),'debit_count_may'] <- 54

#debit_count_jun
summary(dataset$debit_count_jun)

dataset[which(is.na(dataset$debit_count_jun)),'debit_count_jun']<- median(dataset$debit_count_jun,na.rm = T)

quantile(dataset$debit_count_jun,seq(0,1,0.01))

#dataset[which(dataset$debit_count_jun > 62),'debit_count_jun'] <- 62

#credit_count_apr
summary(dataset$credit_count_apr)

dataset[which(is.na(dataset$credit_count_apr)),'credit_count_apr']<- median(dataset$credit_count_apr,na.rm = T)

quantile(dataset$credit_count_apr,seq(0,1,0.01))

#dataset[which(dataset$credit_count_apr > 16),'credit_count_apr'] <- 16

#credit_count_may
summary(dataset$credit_count_may)

dataset[which(is.na(dataset$credit_count_may)),'credit_count_may']<- median(dataset$credit_count_may,na.rm = T)

quantile(dataset$credit_count_may,seq(0,1,0.01))

#dataset[which(dataset$credit_count_may > 19),'credit_count_may'] <- 19

#credit_count_jun
summary(dataset$credit_count_jun)

dataset[which(is.na(dataset$credit_count_jun)),'credit_count_jun']<- median(dataset$credit_count_jun,na.rm = T)

quantile(dataset$credit_count_jun,seq(0,1,0.01))

#dataset[which(dataset$credit_count_jun > 23),'credit_count_jun'] <- 23

#max_credit_amount_apr
summary(dataset$max_credit_amount_apr)

dataset[which(is.na(dataset$max_credit_amount_apr)),'max_credit_amount_apr']<- median(dataset$max_credit_amount_apr,na.rm = T)

quantile(dataset$max_credit_amount_apr,seq(0,1,0.01))

#dataset[which(dataset$max_credit_amount_apr > 300000.00),'max_credit_amount_apr'] <- 300000.00

#max_credit_amount_may
summary(dataset$max_credit_amount_may)

dataset[which(is.na(dataset$max_credit_amount_may)),'max_credit_amount_may']<- median(dataset$max_credit_amount_may,na.rm = T)

quantile(dataset$max_credit_amount_may,seq(0,1,0.01))

#dataset[which(dataset$max_credit_amount_may > 2.969133e+05),'max_credit_amount_may'] <- 2.969133e+05

#max_credit_amount_jun
summary(dataset$max_credit_amount_jun)

dataset[which(is.na(dataset$max_credit_amount_jun)),'max_credit_amount_jun']<- median(dataset$max_credit_amount_jun,na.rm = T)

quantile(dataset$max_credit_amount_jun,seq(0,1,0.01))

#dataset[which(dataset$max_credit_amount_jun >296283.060),'max_credit_amount_jun'] <- 296283.060

#loan_enq
summary(factor(dataset$loan_enq))
dataset[which(dataset$loan_enq != 'Y'),'loan_enq'] <- 'N'

#emi_active
summary(dataset$emi_active)

quantile(dataset$emi_active,seq(0,1,0.01))

#dataset[which(dataset$emi_active >4.866634e+04),'emi_active'] <- 4.866634e+04

#cc_cons
summary(dataset$cc_cons)

sapply(dataset, function(x) sum(is.na(x))/nrow(dataset)*100)

str(dataset)
#------------------------------------------------------------------


# dummy variables to convert categorical columns(2,3,28,32) to one hot encoding
dummies <-
  data.frame(sapply(dataset[,c(2,3,28,32)], function(x)
    data.frame(
      model.matrix( ~ x - 1, data = dataset[,c(2,3,28,32)])
    )[, -1]))

# combine dummies , numeric and output column together
dataset_linear <- cbind(dummies,dataset[,-c(1,2,3,4,28,32)])

str(dataset_linear)

# correlation plot
#corrplot::corrplot(corr = cor(dataset[,-c(1,2,3,4,28,32)]))

# split train and test from merge dataset

train_data_full <- dataset_linear[which(dataset_linear$is_train == TRUE),]
train_data_full$is_train <- NULL

test_data <- dataset_linear[which(dataset_linear$is_train == FALSE),]
test_data$is_train <- NULL

#------------------Data Split into train and Val set -----------------
set.seed(10)

# backup copy before split
#train_data_indices <- train_data
?sample.split
split_indices <- sample.split(train_data_full, SplitRatio = 0.7)

train_data<- train_data_full[split_indices,]

val_data <- train_data_full[!split_indices,]

#------------------- -Linear regression ------------------------------



#-------------------------H20 Automl --------------------------------
# afrter trying convebtional approach of Linear regression combined with PCR, lasso and ridge.
#Let's try Auto.ml 

# initialise h2o instance
h2o.init()

gbm_h2o <- h2o.gbm(y = 'cc_cons',
                   training_frame = as.h2o(train_data_full),
                   nfolds = 5,
                   ntrees = 60,
                   max_depth = 8
)

summary(gbm_h2o)

pred_1 <- h2o.predict(gbm_h2o, as.h2o(test_data))

h2o.performance(gbm_h2o,as.h2o(val_data)) 

write.csv(cbind(test$id,as.data.frame(pred_1)), 'sub_1.csv',row.names = F)
# 133.69 RMSLE on leaderboard

#-----------------------------------------------------------
?h2o.performance

aml_final <- h2o.automl(y = 'cc_cons',
                        training_frame = as.h2o(train_data_full),
                        #validation_frame = as.h2o(val_data),
                        max_models = 10,
                        sort_metric = "RMSLE",
                        seed = 1)


# View the AutoML Leaderboard
lb <- aml_final@leaderboard
lb

pred_auto_ml_final <- h2o.predict(aml_final, as.h2o(test_data))

h2o.performance (aml_final, as.h2o(val_data)) 

write.csv(cbind(test$id,as.data.frame(pred_auto_ml_final)), 'sub_3.csv',row.names = F)

# 132.51  RMSLE on leaderboard

#-----------------------------------Neural Net using H2O---------------------------------------------
# initialise h2o instance
#h2o.init()

model_h2o = h2o.deeplearning(y = 'cc_cons', 
                             training_frame = as.h2o(train_data),
                             seed = 1,
                             sparse = T,
                             variable_importances = T,
                             standardize = T,
                             activation = 'Rectifier',
                             nfolds = 5,
                             epochs = 100)

summary(model_h2o)

test_prediction_h2o <- h2o.predict(model_h2o, newdata = as.h2o(val_data[c(1:42)]))

#write.csv(cbind(test$reservation_id,as.data.frame(test_prediction_h2o)), 'sub_3_h2o.csv',row.names = F)

h2o.performance(model_h2o, newdata = as.h2o(val_data))

# Parameter tuning for deep learning

# #set parameter space
activation_opt <- c("Rectifier","RectifierWithDropout", "Maxout","MaxoutWithDropout")
hidden_opt <- list(c(10,10),c(20,15),c(50,50,50),c(100,100))
l1_opt <- c(0,1e-3,1e-5)
l2_opt <- c(0,1e-3,1e-5)

hyper_params <- list( activation=activation_opt,
                      hidden=hidden_opt,
                      l1=l1_opt,
                      l2=l2_opt )

#set search criteria
search_criteria <- list(strategy = "RandomDiscrete", max_models=10)

#train model

h2o_train_grid <- as.h2o(train_data_indices)
dl_grid <- h2o.grid("deeplearning"
                    ,grid_id = "deep_learn_new"
                    ,hyper_params = hyper_params
                    ,search_criteria = search_criteria
                    ,training_frame = h2o_train_grid
                    ,y = 'amount_spent_per_room_night_scaled', 
                    nfolds = 5
                    ,epochs = 20)

#get best model

d_grid <- h2o.getGrid("deep_learn_new",sort_by = "RMSE")

best_dl_model <- h2o.getModel(d_grid@model_ids[[1]])

#test_prediction_h2o_grid <- h2o.predict(best_dl_model, newdata = as.h2o(val_data[c(1:42)]))
#h2o.performance (best_dl_model, as.h2o(val_data)) 

test_prediction_h2o_grid <- h2o.predict(best_dl_model, newdata = as.h2o(test_data))

write.csv(cbind(test$reservation_id,as.data.frame(test_prediction_h2o_grid)), 'sub_2_grid_h2o.csv',row.names = F)

# leaderboard rmse - 98.2911017636142
# h2o shutdown
h2o.shutdown()

Y
#---------------------------------------------------------------
# Final model selected is Stacked Ensemble

