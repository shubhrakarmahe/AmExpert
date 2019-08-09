#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#AmExpert decipHER - Women Machine Learning Hackathon
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# load the required libraries

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

# account type - majority is of current account type
table(dataset$account_type)

ggplot(data = dataset,aes(x = as.factor(account_type),fill = as.factor(is_train))) + 
  geom_bar(color = 'black') + xlab('Account') +labs(fill = "Train (True/False)")

# gender - majority are male
table(dataset$gender)

ggplot(data = dataset,aes(x = factor(gender),fill = as.factor(is_train))) + 
  geom_bar(color = 'black') + xlab('Gender') +labs(fill = "Train (True/False)")

# age - majority is of current account type
table(dataset$age)

ggplot(data = dataset,aes(x = age)) + geom_histogram(color = 'black',fill ='blue')

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

ggplot(data = dataset,aes(x = cc_cons_apr)) + geom_histogram(color = 'black',fill ='orange')

#outlier detection - at 99 to 100 %. let's cap it.
quantile(dataset$cc_cons_apr,seq(0,1,0.01))
dataset[which(dataset$cc_cons_apr > 112452),'cc_cons_apr']<- 112452

#cc_cons_may
summary(dataset$cc_cons_may)

ggplot(data = dataset,aes(x = cc_cons_may)) + geom_histogram(color = 'black',fill ='pink')

#outlier detection - at 99 to 100 %. let's cap it.
quantile(dataset$cc_cons_may,seq(0,1,0.01))
dataset[which(dataset$cc_cons_may > 103826.1342),'cc_cons_may']<- 103826.1342

#cc_cons_jun
summary(dataset$cc_cons_jun)

ggplot(data = dataset,aes(x = cc_cons_jun)) + geom_histogram(color = 'black',fill = 'green')

#outlier detection - at 99 to 100 %. let's cap it.
quantile(dataset$cc_cons_jun,seq(0,1,0.01))
dataset[which(dataset$cc_cons_jun > 71226.882),'cc_cons_jun']<- 71226.882

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

dataset[which(dataset$card_lim > 600000),'card_lim'] <- 600000

#debit_amount_apr
summary(dataset$debit_amount_apr)

dataset[which(is.na(dataset$debit_amount_apr)),'debit_amount_apr']<- median(dataset$debit_amount_apr,na.rm = T)

quantile(dataset$debit_amount_apr,seq(0,1,0.01))

dataset[which(dataset$debit_amount_apr > 489746.369),'debit_amount_apr'] <- 489746.369

#debit_amount_may
summary(dataset$debit_amount_may)

dataset[which(is.na(dataset$debit_amount_may)),'debit_amount_may']<- median(dataset$debit_amount_may,na.rm = T)

quantile(dataset$debit_amount_may,seq(0,1,0.01))

dataset[which(dataset$debit_amount_may > 438972.072),'debit_amount_may'] <- 438972.072

#debit_amount_june
summary(dataset$debit_amount_jun)

dataset[which(is.na(dataset$debit_amount_jun)),'debit_amount_jun']<- median(dataset$debit_amount_jun,na.rm = T)

quantile(dataset$debit_amount_jun,seq(0,1,0.01))

dataset[which(dataset$debit_amount_jun > 466250.708),'debit_amount_jun'] <- 466250.708

#credit_amount_apr
summary(dataset$credit_amount_apr)

dataset[which(is.na(dataset$credit_amount_apr)),'credit_amount_apr']<- median(dataset$credit_amount_apr,na.rm = T)

quantile(dataset$credit_amount_apr,seq(0,1,0.01))

dataset[which(dataset$credit_amount_apr > 506270.755),'credit_amount_apr'] <- 506270.755

#credit_amount_may
summary(dataset$credit_amount_may)

dataset[which(is.na(dataset$credit_amount_may)),'credit_amount_may']<- median(dataset$credit_amount_may,na.rm = T)

quantile(dataset$credit_amount_may,seq(0,1,0.01))

dataset[which(dataset$credit_amount_may > 4.790300e+05),'credit_amount_may'] <- 4.790300e+05

#credit_amount_jun
summary(dataset$credit_amount_jun)

dataset[which(is.na(dataset$credit_amount_jun)),'credit_amount_jun']<- median(dataset$credit_amount_jun,na.rm = T)

quantile(dataset$credit_amount_jun,seq(0,1,0.01))

dataset[which(dataset$credit_amount_jun > 4.962683e+05),'credit_amount_jun'] <- 4.962683e+05

#debit_count_apr
summary(dataset$debit_count_apr)

dataset[which(is.na(dataset$debit_count_apr)),'debit_count_apr']<- median(dataset$debit_count_apr,na.rm = T)

quantile(dataset$debit_count_apr,seq(0,1,0.01))

dataset[which(dataset$debit_count_apr > 53),'debit_count_apr'] <- 53

#debit_count_may
summary(dataset$debit_count_may)

dataset[which(is.na(dataset$debit_count_may)),'debit_count_may']<- median(dataset$debit_count_may,na.rm = T)

quantile(dataset$debit_count_may,seq(0,1,0.01))

dataset[which(dataset$debit_count_may > 54),'debit_count_may'] <- 54

#debit_count_jun
summary(dataset$debit_count_jun)

dataset[which(is.na(dataset$debit_count_jun)),'debit_count_jun']<- median(dataset$debit_count_jun,na.rm = T)

quantile(dataset$debit_count_jun,seq(0,1,0.01))

dataset[which(dataset$debit_count_jun > 62),'debit_count_jun'] <- 62

#credit_count_apr
summary(dataset$credit_count_apr)

dataset[which(is.na(dataset$credit_count_apr)),'credit_count_apr']<- median(dataset$credit_count_apr,na.rm = T)

quantile(dataset$credit_count_apr,seq(0,1,0.01))

dataset[which(dataset$credit_count_apr > 16),'credit_count_apr'] <- 16

#credit_count_may
summary(dataset$credit_count_may)

dataset[which(is.na(dataset$credit_count_may)),'credit_count_may']<- median(dataset$credit_count_may,na.rm = T)

quantile(dataset$credit_count_may,seq(0,1,0.01))

dataset[which(dataset$credit_count_may > 19),'credit_count_may'] <- 19

#credit_count_jun
summary(dataset$credit_count_jun)

dataset[which(is.na(dataset$credit_count_jun)),'credit_count_jun']<- median(dataset$credit_count_jun,na.rm = T)

quantile(dataset$credit_count_jun,seq(0,1,0.01))

dataset[which(dataset$credit_count_jun > 23),'credit_count_jun'] <- 23

#max_credit_amount_apr
summary(dataset$max_credit_amount_apr)

dataset[which(is.na(dataset$max_credit_amount_apr)),'max_credit_amount_apr']<- median(dataset$max_credit_amount_apr,na.rm = T)

quantile(dataset$max_credit_amount_apr,seq(0,1,0.01))

dataset[which(dataset$max_credit_amount_apr > 300000.00),'max_credit_amount_apr'] <- 300000.00

#max_credit_amount_may
summary(dataset$max_credit_amount_may)

dataset[which(is.na(dataset$max_credit_amount_may)),'max_credit_amount_may']<- median(dataset$max_credit_amount_may,na.rm = T)

quantile(dataset$max_credit_amount_may,seq(0,1,0.01))

dataset[which(dataset$max_credit_amount_may > 2.969133e+05),'max_credit_amount_may'] <- 2.969133e+05

#max_credit_amount_jun
summary(dataset$max_credit_amount_jun)

dataset[which(is.na(dataset$max_credit_amount_jun)),'max_credit_amount_jun']<- median(dataset$max_credit_amount_jun,na.rm = T)

quantile(dataset$max_credit_amount_jun,seq(0,1,0.01))

dataset[which(dataset$max_credit_amount_jun >296283.060),'max_credit_amount_jun'] <- 296283.060

#loan_enq
summary(factor(dataset$loan_enq))
dataset[which(dataset$loan_enq != 'Y'),'loan_enq'] <- 'N'

#emi_active
summary(dataset$emi_active)

quantile(dataset$emi_active,seq(0,1,0.01))

dataset[which(dataset$emi_active >4.866634e+04),'emi_active'] <- 4.866634e+04

#cc_cons
summary(dataset$cc_cons)

# cross check NA values,if any

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

#-------------Feature engineering------------------------------
#credit limit utilization
dataset_linear$cc_util_apr <- dataset_linear$debit_amount_apr/dataset_linear$card_lim
dataset_linear$cc_util_may <- dataset_linear$debit_amount_may/dataset_linear$card_lim
dataset_linear$cc_util_jun <- dataset_linear$debit_amount_jun/dataset_linear$card_lim

# over spending above limit
dataset_linear$spend_overlimit_cnt<- 0
dataset_linear[which(dataset_linear$cc_util_apr > 1 | dataset_linear$cc_util_may > 1 | dataset_linear$cc_util_jun > 1),'spend_overlimit_cnt']<- 1

# Total balance and transactions
dataset_linear$total_bal <- (dataset_linear$credit_amount_apr+dataset_linear$credit_amount_may+dataset_linear$credit_amount_jun) - (dataset_linear$debit_amount_apr+dataset_linear$debit_amount_may+dataset_linear$debit_amount_jun) 

dataset_linear$total_trans <- (dataset_linear$debit_count_apr+dataset_linear$debit_count_may+dataset_linear$debit_count_jun) + (dataset_linear$credit_count_apr+dataset_linear$credit_count_may+dataset_linear$credit_count_jun)

# Avg of amt and cnt
dataset_linear$avg_credit_amt <- (dataset_linear$credit_amount_apr+dataset_linear$credit_amount_may+dataset_linear$credit_amount_jun)/3

dataset_linear$avg_debit_amt <- (dataset_linear$debit_amount_apr+dataset_linear$debit_amount_may+dataset_linear$debit_amount_jun)/3

dataset_linear$avg_cc_cons <- (dataset_linear$cc_cons_apr+dataset_linear$cc_cons_may+dataset_linear$cc_cons_jun)/3

dataset_linear$avg_cc_count <- round((dataset_linear$cc_count_apr+dataset_linear$cc_count_may+dataset_linear$cc_count_jun)/3,0)

dataset_linear$avg_debit_count <- round((dataset_linear$debit_count_apr+dataset_linear$debit_count_may+dataset_linear$debit_count_jun)/3,0)

dataset_linear$avg_credit_count <- round((dataset_linear$credit_count_apr+dataset_linear$credit_count_may+dataset_linear$credit_count_jun)/3,0)

sapply(dataset_linear, function(x) sum(is.na(x))/nrow(dataset)*100)

# correlation plot
#corrplot::corrplot(corr = cor(dataset[,-c(1,2,3,4,28,32)]))

# split train and test from merge dataset

#train_data_full <- dataset_linear[which(dataset_linear$is_train == TRUE),]
train_data_full <- dataset_linear[which(dataset_linear$is_train == TRUE),c(1:34)]
train_data_full$is_train <- NULL

test_data <- dataset_linear[which(dataset_linear$is_train == FALSE),c(1:34)]
#test_data <- dataset_linear[which(dataset_linear$is_train == FALSE),c(1:9,16,32:34,35:40)]
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

model_1 <- lm(data = train_data,formula = cc_cons~.)
summary(model_1)

model_2 <- stepAIC(model_1, direction = "both")

summary(model_2)
#vif(model_2)

#avg_debit_count has high vif

model_3 <- lm(formula = cc_cons ~ account_type + gender + loan_enq + age_bin.x40.50 + 
                region_code + cc_cons_apr + cc_cons_may + cc_cons_jun + cc_count_jun + 
                card_lim + debit_count_apr + max_credit_amount_apr + credit_amount_may + 
                max_credit_amount_may + debit_amount_jun + credit_amount_jun + 
                credit_count_jun + debit_count_jun + emi_active + avg_cc_count 
                , data = train_data)

summary(model_3)
vif(model_3)

#avg_cc_count has high vif
model_4 <- lm(formula = cc_cons ~ account_type + gender + loan_enq + age_bin.x40.50 + 
                region_code + cc_cons_apr + cc_cons_may + cc_cons_jun + cc_count_jun + 
                card_lim + debit_count_apr + max_credit_amount_apr + credit_amount_may + 
                max_credit_amount_may + debit_amount_jun + credit_amount_jun + 
                credit_count_jun + debit_count_jun + emi_active 
              , data = train_data)
summary(model_4)

# dbit_count_apr 
model_5 <- lm(formula = cc_cons ~ account_type + gender + loan_enq + age_bin.x40.50 + 
                region_code + cc_cons_apr + cc_cons_may + cc_cons_jun + cc_count_jun + 
                card_lim +  max_credit_amount_apr + credit_amount_may + 
                max_credit_amount_may + debit_amount_jun + credit_amount_jun + 
                credit_count_jun + debit_count_jun + emi_active 
              , data = train_data)
summary(model_5)

#debit_count_june
model_6 <- lm(formula = cc_cons ~ account_type + gender + loan_enq + age_bin.x40.50 + 
                region_code + cc_cons_apr + cc_cons_may + cc_cons_jun + cc_count_jun + 
                card_lim +  max_credit_amount_apr + credit_amount_may + 
                max_credit_amount_may + debit_amount_jun + credit_amount_jun + 
                credit_count_jun + emi_active 
              , data = train_data)
summary(model_6)

#cc_count_jun
model_7 <- lm(formula = cc_cons ~ account_type + gender + loan_enq + age_bin.x40.50 + 
                region_code + cc_cons_apr + cc_cons_may + cc_cons_jun  + 
                card_lim +  max_credit_amount_apr + credit_amount_may + 
                max_credit_amount_may + debit_amount_jun + credit_amount_jun + 
                credit_count_jun + emi_active 
              , data = train_data)
summary(model_7)

#remove loan_enq
model_8 <- lm(formula = cc_cons ~ account_type + gender +  age_bin.x40.50 + 
                region_code + cc_cons_apr + cc_cons_may + cc_cons_jun  + 
                card_lim +  max_credit_amount_apr + credit_amount_may + 
                max_credit_amount_may + debit_amount_jun + credit_amount_jun + 
                credit_count_jun + emi_active 
              , data = train_data)
summary(model_8)

#remove max_credit_amount_may
model_9 <- lm(formula = cc_cons ~ account_type + gender +  age_bin.x40.50 + 
                region_code + cc_cons_apr + cc_cons_may + cc_cons_jun  + 
                card_lim +  max_credit_amount_apr + credit_amount_may + 
                 debit_amount_jun + credit_amount_jun + 
                credit_count_jun + emi_active 
              , data = train_data)
summary(model_9)

model_10 <- lm(formula = cc_cons ~ account_type + gender +  age_bin.x40.50 + 
                 region_code + cc_cons_apr + cc_cons_may + cc_cons_jun  + 
                 card_lim +  max_credit_amount_apr + credit_amount_may + 
                  credit_amount_jun + 
                 credit_count_jun + emi_active 
               , data = train_data)
summary(model_10)


prediction_l_final <- predict(model_14,test_data)
Metrics::rmsle(val_data$cc_cons, prediction_l_final)
head(as.data.frame(cbind(val_data$cc_cons, prediction_l_final)))

write.csv(cbind(test$id,as.data.frame(prediction_l_final)), 'sub_linaer.csv',row.names = F)

#-------------------------H20 GBM --------------------------------
# afrter trying convebtional approach of Linear regression combined with PCR, lasso and ridge.
#Let's try Auto.ml 

# initialise h2o instance
h2o.init()

#?h2o.gbm

gbm_h2o <- h2o.gbm(y = 'cc_cons',
                  training_frame = as.h2o(train_data_full),
                  nfolds = 10,
                  ntrees = 70,
                  max_depth = 10,
                  stopping_metric = "RMSLE",
                  stopping_tolerance = 0.115,
                  stopping_rounds = 20,
                  seed = 1,
                  learn_rate = 0.05,
                  learn_rate_annealing = 0.99
)

summary(gbm_h2o)

pred_1 <- h2o.predict(gbm_h2o, as.h2o(test_data))

h2o.performance(gbm_h2o,as.h2o(val_data)) 

write.csv(cbind(test$id,as.data.frame(pred_1)), 'sub_5.csv',row.names = F)
# 133.69 RMSLE on leaderboard

#-----------Grid search hyperparameter for gbm -------------------------------

# GBM hyperparamters (bigger grid than above)
gbm_params2 <- list(learn_rate = seq(0.01, 0.1, 0.01),
                    max_depth = seq(2, 10, 1),
                    sample_rate = seq(0.5, 1.0, 0.1),
                    col_sample_rate = seq(0.1, 1.0, 0.1))
search_criteria <- list(strategy = "RandomDiscrete", max_models = 50, seed = 1)

# Train and validate a random grid of GBMs
gbm_grid3 <- h2o.grid("gbm", y = 'cc_cons',
                      grid_id = "gbm_grid3",
                      training_frame = as.h2o(train_data),
                      validation_frame = as.h2o(val_data),
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params2,
                      search_criteria = search_criteria)

gbm_gridperf3 <- h2o.getGrid(grid_id = "gbm_grid3",
                             sort_by = "rmsle"
                             )
print(gbm_gridperf3)

# Grab the top GBM model, chosen by validation AUC
best_gbm2 <- h2o.getModel(gbm_gridperf2@model_ids[[1]])

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_gbm_perf2 <- h2o.performance(model = best_gbm2,
                                  newdata = as.h2o(val_data))
h2o.rmsle(best_gbm_perf2)
# 0.7810757

# Look at the hyperparamters for the best model
print(best_gbm2@model[["model_summary"]])
#---------------------Auto ML--------------------------------------
?h2o.automl

aml_final <- h2o.automl(y = 'cc_cons',
                        training_frame = as.h2o(train_data_full),
                        #validation_frame = as.h2o(val_data),
                        max_models = 10,
                        #stopping_metric = "RMSLE",
                        #stopping_rounds = 0,
                        sort_metric = "RMSLE",
                        seed = 1)


# View the AutoML Leaderboard
lb <- aml_final@leaderboard
lb

pred_auto_ml_final <- h2o.predict(aml_final, as.h2o(test_data))

#h2o.performance (aml_final, as.h2o(val_data)) 

write.csv(cbind(test$id,as.data.frame(pred_auto_ml_final)), 'sub_final_1.csv',row.names = F)

# 132.51  RMSLE on leaderboard


h2o.shutdown()

Y
#---------------------------------------------------------------
# Final model selected is GBM_5_AutoML_20190719_225023 from automl

