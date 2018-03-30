set.seed(12345)
library(e1071)
args = commandArgs(trailingOnly=TRUE)

#data <- read.csv("RF_OGvsTS.csv",header=TRUE,row.names = 1) # Read Data 
data <- read.csv(args[1],header=TRUE,row.names = 1) # Read Data 
print("data loaded sucessfully !")
colnames(data)[1] <- "Class"  # assign response class 
### separate data into matix and response
dataX <- data[,2:length(data)]
dataY <- as.factor(data[,1])


### handle missing values to median. Can also use mean , mode ###
impute.value <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
data.new <- sapply(dataX, function(x){
    if(is.numeric(x)){
            impute.value(x)
        } else {
            x
        }
    }
)
dataX <- as.data.frame(data.new) # create data frame
data <- cbind(dataX,dataY)
colnames(data)[length(data)] <- "Class"
accuracies <-c()
Numbers <- as.numeric(args[4])

#### split data into test and train set ####################################

for (i in 1:Numbers) {
print (i)
print("##############################")
# First decide percent and and creat random test and train set ##
# following coomand gives persent which later used for data split
# eg. if 25 given the percent is 25 (25 %) means test set contain 1/4 of data
#rest 3/4 data will be in train set.    

prct <- as.numeric(args[2])
prct <- prct / 100 # this creats percent   

sample.points <- 1:nrow(data) # this tells about how many samples are in data

# following command  will create random range of index values
ind <- sample(sample.points,floor(length(sample.points) * prct)) 

# Now create test and train set
test.set <- data[ind,] # creates a data.frame with random sample
train.set <- data[-ind,]

##############################################################################


#### Following Section is for parameter tune ###

# following command creates a grid range for optimal parameter search 
# as svm has two parameters "gamma" and "cost" it searches best values over different combinations of both parameters

# user may define own grid such as gamma= c(0.00001,0.0001,0.001) cost = c(0.001,0.001,0.1,1) etc.

# parameters are tuned using 10 fold cross validation 

#parameter.tune <- tune.svm(Class~.,data = train.set, gamma= 10^(seq(-8,-1,by=0.5)), cost = 10^seq(-3,1,by=0.2)) 
parameter.tune <- tune.svm(Class~.,data = train.set, gamma= 10^(seq(-6,-1)), cost = 10^seq(-3,1)) 

cost <- format(parameter.tune$best.parameters$cost,digits=5,nsmall=4)
gamma <- format(parameter.tune$best.parameters$gamma,digits=5,nsmall=4)

# you may check their best values by typing summary(parameter.tune) #

paste("Tuned Values :","gamma",gamma,"and cost",cost)

# now use tuned parameter for model building  #

svm.model <- svm(Class~.,data=train.set,kernel=args[3],gamma = gamma, cost = cost)
summary(svm.model)
save(svm.model,file="svm.model.RData")
##############################################################################

# validation

validation <- predict(svm.model,test.set[,-length(test.set)])

# create confusion matrix

confusion.matrix <- table(pred = validation, true = test.set[,length(test.set)])
confusion.matrix

# calculate performance measures
# sensitivity = TP / TP + FN
# specificity = TN / TN + FP
# Accuarcy = TP + TN / TP + TN + FP + FN
sensitivity <- format(confusion.matrix[4] / (confusion.matrix[4] + confusion.matrix[3]),digits=4)
specificity <- format(confusion.matrix[1] / (confusion.matrix[1] + confusion.matrix[2]),digits=4)
accuracy <- format((confusion.matrix[4] + confusion.matrix[1]) / sum(confusion.matrix),digits=4,nsmall=3)

paste("For data set :",args[1]," the model spliltted in test and train set where train set has ",nrow(train.set),"samples while test set has ",nrow(test.set),
      ".Model was generated using training set and optimal parameter were selected usinng 10 fold cross validation result.",
       "The specificity of model on test set is ",specificity," sensitivity is ",sensitivity," and accuracy is ",accuracy)

accuracies <- c(accuracies,accuracy)
}
accuracies
mean_accuracy <- mean(as.numeric(accuracies))
deviation <- format(sd(as.numeric(accuracies)),digits=5)


paste("Accuracy is ",mean_accuracy,"±",deviation)


