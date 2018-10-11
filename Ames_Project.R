setwd('~/Downloads')
ames = read.csv("Ames_data.csv", stringsAsFactors = TRUE, header = TRUE)

# check missing values
NAcol = which(colSums(is.na(ames)) > 0) #index
sort(colSums(sapply(ames[NAcol], is.na)))

# remove the column which contains missing value
ames$Garage_Yr_Blt = NULL

# remove because they have one dominate level
# for (i in 1:82) {
#  if (typeof(ames[,i]) == "character") {
#    print(table(ames[,i]))}
#}

ames$Condition_2 = NULL
ames$Utilities = NULL
ames$Paved_Drive = NULL
ames$Street = NULL
ames$Heating = NULL
ames$Pool_QC = NULL
ames$Misc_Feature = NULL
ames$Pool_Area = NULL
# remove latitue and longitude
ames$Longitude = NULL
ames$Latitude = NULL
# remove suggested variables
ames$Roof_Matl = NULL
ames$Three_season_porch = NULL


# code factor variables
ames$Lot_Shape = factor(ames$Lot_Shape, levels = c("Irregular", "Moderately_Irregular","Slightly_Irregular", "Regular"), 
                        ordered = TRUE)
ames$Land_Slope = factor(ames$Land_Slope, levels = c("Sev", "Mod", "Gtl"), ordered = TRUE)
ames$Overall_Qual = factor(ames$Overall_Qual, levels = c("Very_Poor","Poor", "Fair","Below_Average","Average",
                                                         "Above_Average", "Good","Very_Good","Excellent",
                                                         "Very_Excellent"), ordered = TRUE)

ames$Overall_Cond = factor(ames$Overall_Cond, levels = c( "Very_Poor","Poor","Fair","Below_Average","Average",
                                                          "Above_Average", "Good","Very_Good","Excellent",
                                                          "Very_Excellent"), ordered = TRUE)

ames$Exter_Qual = factor(ames$Exter_Qual, levels = c("Fair", "Typical" ,"Good","Excellent"), ordered = TRUE)
ames$Exter_Cond = factor(ames$Exter_Cond, levels = c("Poor","Fair", "Typical" ,"Good","Excellent"), ordered = TRUE)
ames$Bsmt_Cond = factor(ames$Bsmt_Cond, levels = c("No_Basement","Poor","Fair", "Typical", "Good","Excellent"), ordered = TRUE)
ames$Bsmt_Qual = factor(ames$Bsmt_Qual, levels = c("No_Basement","Poor","Fair", "Typical", "Good","Excellent"), ordered = TRUE)
ames$Bsmt_Exposure = factor(ames$Bsmt_Exposure, levels= c("No_Basement", "No" ,"Mn","Av","Gd" ),ordered = TRUE)
ames$BsmtFin_Type_1 = factor(ames$BsmtFin_Type_1, levels =c("No_Basement", "Unf", "LwQ","Rec", "BLQ","ALQ","GLQ" ),ordered = TRUE)
ames$BsmtFin_Type_2 = factor(ames$BsmtFin_Type_2, levels =c("No_Basement", "Unf", "LwQ","Rec", "BLQ","ALQ","GLQ" ),ordered = TRUE)
ames$Heating_QC = factor(ames$Heating_QC, levels = c("Poor" ,"Fair","Typical","Good","Excellent" ), ordered = TRUE)
ames$Electrical = factor(ames$Electrical, levels =  c("Unknown",  "Mix","FuseP","FuseF", "FuseA" , "SBrkr"),ordered = TRUE)
ames$Kitchen_Qual = factor(ames$Kitchen_Qual, levels = c("Poor","Fair", "Typical" ,"Good","Excellent"), ordered = TRUE)
ames$Functional = factor(ames$Functional, levels = c("Sal", "Sev", "Maj2","Maj1","Mod","Min2","Min1","Typ" ), ordered = TRUE)
ames$Fireplace_Qu = factor(ames$Fireplace_Qu ,levels = c("No_Fireplace" ,"Poor",  "Fair", "Typical",  "Good","Excellent"),
                           ordered = TRUE)
ames$Garage_Finish = factor(ames$Garage_Finish, levels = c( "No_Garage","Unf","RFn","Fin"), ordered = TRUE)
ames$Garage_Qual = factor(ames$Garage_Qual, levels = c("No_Garage" ,"Poor","Fair","Typical", "Good", "Excellent"),
                          ordered = TRUE)
ames$Garage_Cond = factor(ames$Garage_Cond, levels = c("No_Garage" ,"Poor","Fair","Typical", "Good", "Excellent"),
                          ordered = TRUE)
#ames$Pool_QC = factor(ames$Pool_QC,levels = c("No_Pool","Fair","Typical", "Good", "Excellent"),
#             ordered = TRUE)
ames$Fence = factor(ames$Fence, levels = c("No_Fence",  "Minimum_Wood_Wire","Good_Wood","Minimum_Privacy","Good_Privacy"),
                    ordered = TRUE)



test_id = read.table("Project1_test_id.txt")
test_id = test_id[,1]
test = ames[which(ames$PID %in% test_id),] 
train = ames[-which(ames$PID %in% test_id),]  

# random forest
rf = randomForest(Sale_Price~., data = train[,-1], importance = TRUE)
importance = as.data.frame(importance(rf))
variable_selected = rownames(importance)[which(importance[,1]>5)]

train_select_X = train[,variable_selected]
train_Y = train[,dim(train)[2]]
test_select_X = test[,variable_selected]
test_Y = test[,dim(train)[2]]

# fit a random forest model using selected variables
# model 1 
set.seed(123)
rf1 = randomForest(x = train_select_X, y = log(train_Y))
#p1 = predict(rf1, newdata = test_select_X)
#sqrt(mean((log(test_Y) - p1)^2))


# loop over

set.seed(123)
rf_rmse = matrix(0,1,10)
for (i in 1:10){
  test_id = read.table("Project1_test_id.txt")
  test_id = test_id[,i]
  test = ames[which(ames$PID %in% test_id),] 
  train = ames[-which(ames$PID %in% test_id),] 
  
  train_select_X = train[,variable_selected]
  train_Y = train[,dim(train)[2]]
  test_select_X = test[,variable_selected]
  test_Y = test[,dim(train)[2]]
  
  rf_pred = predict(rf1, newdata = test_select_X)
  rf_rmse[,i] = sqrt(mean((log(test_Y) - rf_pred)^2))
}



# model 2 
rf2 = randomForest(x = train_select_X, y = log(train_Y), ntree = 550)

set.seed(123)
rf_rmse2 = matrix(0,1,10)
for (i in 1:10){
  test_id = read.table("Project1_test_id.txt")
  test_id = test_id[,i]
  test = ames[which(ames$PID %in% test_id),] 
  train = ames[-which(ames$PID %in% test_id),] 
  
  train_select_X = train[,variable_selected]
  train_Y = train[,dim(train)[2]]
  test_select_X = test[,variable_selected]
  test_Y = test[,dim(train)[2]]
  
  rf_pred = predict(rf2, newdata = test_select_X)
  rf_rmse2[,i] = sqrt(mean((log(test_Y) - rf_pred)^2))
}