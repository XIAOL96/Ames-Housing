# library
library(caret)
library(glmnet)

# read in data
setwd('~/Downloads')
ames = read.csv("Ames_data.csv", stringsAsFactors = TRUE, header = TRUE)
test_id = read.table("Project1_test_id.txt")
accu = matrix(0,1,10)
for (i in 1:10)
{testID = test_id[,i]
test = ames[which(ames$PID %in% testID),] 
train = ames[-which(ames$PID %in% testID),] 
write.csv(test,"test.csv",row.names = F)       
write.csv(train,"train.csv",row.names = F)      

train = read.csv("train.csv")
test = read.csv("test.csv")
# data preprocessing
train[,c('Longitude', 'Latitude', 'Condition_2', 'Utilities', 'Street', 'Heating',
           'Pool_QC', 'Misc_Feature', 'Pool_Are', 'Land_Slope', 'Low_Qual_Fin_SF', 'Misc_Val',
           'Roof_Matl', 'Three_season_porch', 'Garage_Yr_Blt', 'Exterior_1st', 'Exterior_2nd', 
         'Mas_Vnr_Type', 'Kitchen_Qual', 'Electrical', 'Sale_Type' , 'MS_Zoning', 'Functional', 'Exter_Cond')] = list(NULL)

test[,c('Longitude', 'Latitude', 'Condition_2', 'Utilities', 'Street', 'Heating',
        'Pool_QC', 'Misc_Feature', 'Pool_Are', 'Land_Slope', 'Low_Qual_Fin_SF', 'Misc_Val',
        'Roof_Matl', 'Three_season_porch', 'Garage_Yr_Blt','Exterior_1st', 'Exterior_2nd', 
        'Mas_Vnr_Type', 'Kitchen_Qual', 'Electrical','Sale_Type', 'MS_Zoning', 'Functional', 'Exter_Cond')] = list(NULL)

train[which(train$MS_SubClass == 'One_and_Half_Story_PUD_All_Ages'),'MS_SubClass'] = 'One_and_Half_Story_Unfinished_All_Ages'
test[which(test$MS_SubClass == 'One_and_Half_Story_PUD_All_Ages'),'MS_SubClass'] = 'One_and_Half_Story_Unfinished_All_Ages'
train[which(train$Neighborhood == 'Landmark'),'Neighborhood'] = 'Northridge_Heights'
test[which(test$Neighborhood == 'Landmark'),'Neighborhood'] = 'Northridge_Heights'


test_X = test[,-c(1,dim(test)[2])]
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
accu[1,i] = sqrt(mean((log(test_Y) - pred1)^2))}

# results
Sale_Price = exp(pred1)
results = data.frame(a = testID, b = round(Sale_Price, digits = 1))
colnames(results) = c("PID", "Sale_Price")

# save as txt.file
write.table(results, file = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/mysubmission2.txt"),row.names=F,quote=F, sep=",")

#print(sqrt(mean((log(test_Y) - pred1)^2)))


