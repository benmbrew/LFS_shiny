library(ggplot2)
library(dplyr)

# read in clinicalical data
clinical <- read.csv('clinical.csv')

# get cancer and tp53 counts
cancer <- clinical %>%
  group_by(tp53, cancer) %>%
  summarise(counts = n(),
            age_of_onset = mean(age_of_onset, na.rm = T),
            male = sum(gender == 'male', na.rm = T),
            female = sum(gender == 'female', na.rm = T))

################################################# Use models 
library(ROCR)
library(AUC)
library(randomForest)
library(e1071)
library(glmnet)

# predict ACC status 
# create label for acc status
clinical$acc_status <- ifelse(clinical$cancer == 'ACC', TRUE, FALSE)
clinical$acc_status <- as.factor(clinical$acc_status)


model_formula <- 'acc_status ~ tp53 + codon_72 + mdm2 + gender'
model_formula <- as.formula(model_formula)

# use k fold cross validation
folds <- 10 

# randomly assign samples to 1 of 10 folds 
clinical$folds <- sample(1:folds, nrow(clinical), replace = TRUE)
x <- clinical[, c('acc_status', 'tp53', 'codon_72', 'mdm2', 'gender', 'folds')]
x <- x[complete.cases(x),]


# make list to store results
results_list <- list()


for(i in 1:folds){
  
  # Break into training and test
  train_x <- x[x$folds != i,]
  test_x <- x[x$folds == i,]
  
  ## Fit models 
  fit_logit <- glm(model_formula, 
                   data = train_x, 
                   family = 'binomial',
                   na.action = na.omit)
  
  fit_rf <- randomForest(model_formula, 
                         data = train_x, 
                         na.action = na.omit)
  
  fit_svm <- svm(as.factor(acc_status) ~ tp53 + codon_72 + mdm2 + gender, 
                 data= train_x, 
                 kernel = "radial", 
                 cost =10, 
                 scale = FALSE, 
                 probability = TRUE, 
                 na.action = na.omit, 
                 type="C-classification")
  
  y_train <- train_x$acc_status
  fit_lasso <- cv.glmnet(model.matrix(model_formula, data = train_x), 
                         y_train, 
                         family = 'binomial',
                         nfolds = 5, 
                         alpha = 1,
                         standardize = F)
  
  fit_ridge <- cv.glmnet(model.matrix(model_formula, data = train_x), 
                         y_train, 
                         family = 'binomial',
                         nfolds = 5, 
                         alpha = 0,
                         standardize = F)  
  
  fit_tune <- tuneRF(train_x[, -1], y_train, 
                     trace = FALSE, 
                     doBest = TRUE, 
                     probability = TRUE) 
  
  
  #Predict on models and store results in test_x
  test_x$rf_prob <- predict(fit_rf, test_x, 
                            type = 'prob',
                            na.action = na.omit)
  
  test_x$rf <- predict(fit_rf, test_x, 
                       na.action = na.omit)
  
  svm <- predict(fit_svm, test_x, 
                 probability = TRUE, 
                 na.action = na.omit)
  
  test_x$svm <- attr(svm, 'probabilities')
  
  test_x$logit <- predict(fit_logit, 
                          test_x, 
                          type = 'response', 
                          na.action = na.omit)
  
  test_x$lasso <- predict(fit_lasso, model.matrix(model_formula, test_x), 
                          type = 'response', 
                          s ="lambda.min", 
                          na.action = na.omit)
  
  test_x$ridge <- predict(fit_ridge, model.matrix(model_formula, test_x), 
                          type = 'response', 
                          s ="lambda.min", 
                          na.action = na.omit)
  
  test_x$tune <- predict(fit_tune, test_x, 
                         type = 'prob') 
  
  
  # populate results list 
  results_list[[i]] <- test_x 
  
  # print update 
  cat(paste0('just finished ', i, ' of ', folds, '\n'))
}

# bind results list together, overwriting df_small
test <- do.call('rbind', results_list)

# extract probabilities for svm and random forest
test$rf_true <- test$rf_prob[,2]
test$svm_true <- test$svm[, 1]
test$tune_true <- test$tune[,2]


