# library
library(dplyr)
library(vtreat)
library(xgboost)

# read in data
test =     ## Fill with your test data ##
train =    ## Fill with your train data ##

# data preprocessing
  
train$Garage_Yr_Blt = NULL
test$Garage_Yr_Blt = NULL

# MODEL 1 Xgboost

features = setdiff(names(train), "Sale_Price")
treatplan = vtreat::designTreatmentsZ(train, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars = treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)    

# Prepare the training data
features_train = vtreat::prepare(treatplan, train, varRestriction = new_vars) %>% as.matrix()
response_train = train$Sale_Price

# Prepare the test data
features_test = vtreat::prepare(treatplan, test, varRestriction = new_vars) %>% as.matrix()
response_test = test$Sale_Price

# parameter list
params = list(
  eta = 0.01,
  max_depth = 5,
  min_child_weight = 5,
  subsample = 0.65,
  colsample_bytree = 1
)

# train final model
set.seed(123)
xgb.fit.final = xgboost(
  params = params,
  data = features_train,
  label = log(response_train),
  nrounds = 1576,
  objective = "reg:linear",
  verbose = 0
)

pred1 = predict(xgb.fit.final, newdata = features_test)

# results
Sale_Price = exp(pred1)
results = data.frame(a = testID, b = round(Sale_Price, digits = 1))
colnames(results) = c("PID", "Sale_Price")

# save as txt.file
write.table(results, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/mysubmission1.txt"),row.names=F,quote=F, sep=",")

#print(sqrt(mean((log(results$Sale_Price) - log(response_test))^2)))

