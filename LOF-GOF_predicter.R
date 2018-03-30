library(e1071)

# load model
load("svm.model.RData")

# read prediction set
# it should be noted that Protien ids are repetative so we can not use those as rownames. but we do an alternate way.

pred_set <- read.csv("Features-Insertion-Neutral.csv",header=T)
# keep only complete data 
pred_set <- na.omit(pred_set)

# take a copy of protein ids alobg with mutataion information

ids_mutation <- pred_set[,1:2]

# original model was built using 11 features we need to create data with exactly similiar feature 
pred_data_new <- pred_set[,3:length(pred_set)]

# check dim
dim(pred_data_new)
dim(ids_mutation)

# prediction 
pred <- predict(object=svm.model,newdata=pred_data_new)

# arrange result 

final_pred <- cbind(ids_mutation,pred)
# reassign column names
colnames(final_pred) <- c("Protien_ID","Substitution","Prediction")
# write result
write.table(final_pred,file="Result.csv",row.names=F)


