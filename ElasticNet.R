# library
library(caret)
library(glmnet)

# read in data
test =        ## Fill with your train data ##
train =       ## Fill with your test data ##

# data preprocessing
train[,c('Longitude', 'Latitude', 'Condition_2', 'Utilities', 'Street', 'Heating',
           'Pool_QC', 'Misc_Feature', 'Pool_Are', 'Land_Slope', 'Low_Qual_Fin_SF', 'Misc_Val',
           'Roof_Matl', 'Three_season_porch', 'Garage_Yr_Blt')] = list(NULL)

test[,c('Longitude', 'Latitude', 'Condition_2', 'Utilities', 'Street', 'Heating',
        'Pool_QC', 'Misc_Feature', 'Pool_Are', 'Land_Slope', 'Low_Qual_Fin_SF', 'Misc_Val',
        'Roof_Matl', 'Three_season_porch', 'Garage_Yr_Blt')] = list(NULL)

test_X = test[,-c(1,69)]
test_Y = test$Sale_Price

form =  formula(log(Sale_Price) ~ .)

# MODEL 2 Elastic Net
set.seed(123)
model_glm = train(form, data = train[,-1], method = "glmnet",
                  trControl = trainControl(
                    method = "cv", 
                    number  = 5) 
                  
)
pred1 = predict(model_glm, newdata = test_X)

# results
Sale_Price = exp(pred1)
results = data.frame(a = testID, b = round(Sale_Price, digits = 1))
colnames(results) = c("PID", "Sale_Price")

# save as txt.file
write.table(results, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/mysubmission2.txt"),row.names=F,quote=F, sep=",")

#print(sqrt(mean((log(test_Y) - pred1)^2)))


