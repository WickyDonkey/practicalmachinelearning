library(ggplot2)
library(caret)
library(dplyr)
library(rattle)

getwd()
setwd("~/R/Assignment Machine")

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")

training <- read.csv("pml-training.csv",na.strings = c("NA","NaN","","#DIV/0!"))
testing <- read.csv("pml-testing.csv",na.strings = c("NA","NaN","","#DIV/0!"))

summary(training)
names(training)

# 4 accelerometers belt, arm, dumbbell, forearm
# 6 participants
# 5 different ways A,B,C,D,E - A is correct
# exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).

summary(training$user_name)
summary(training$classe)

# Does not work: # featurePlot(x=training[,8],y=training$classe,plot = "scatter")

#model <- lm(classe ~ .,data = training)
#model <- lm(classe ~ .,data = training)

training1 <- training[,-(1:7)]

#summary(training1[1:20])
#model <- glm(classe ~ roll_belt,data = training1)

#M <- abs(cor(training1))
# error # only possible with numberic values
#diag(M)<-0
#which(M > 0.8,arr.ind=TRUE)

#model <- train(classe ~.,data=training1[c(1:2,153)],method="glm")
# error # stopping: all the accuracy metric values are missing

summary(training1[1:5])
training1 <- training1 %>% select(-kurtosis_roll_belt)
sum(is.na(training1))

# remove all rows with NA's (almost all values in column)
na <- apply(training1,2,is.na)
nna <- apply(na,2,sum)
selectnonna <- nna == 0
training1 <- training1[,selectnonna]
summary(training1)

# split the training set into train and validate
inTrain <- createDataPartition(y=training1$classe, p=0.75, list=FALSE)
training2 <- training1[inTrain,]
testing2 <- training1[-inTrain,]
dim(training2)
dim(testing2)

# Regression tree
model_rpart <- train(classe ~.,data=training2,method="rpart")
print(model_rpart$finalModel)
fancyRpartPlot(model_rpart$finalModel)

# Bagging trees
model_bag <- train(classe ~.,data=training2,method="treebag")
print(model_bag$finalModel)

# Random forest trees
model_rf <- train(classe ~.,data=training2,method="rf")
print(model_rf$finalModel)

# Boosting trees
model_gbm <- train(classe ~.,data=training2,method="gbm",verbose=FALSE)
print(model_gbm$finalModel)

# Naive Bayes trees
model_nb <- train(classe ~.,data=training2,method="nb",verbose=FALSE)
#print(model_nb$finalModel)

# Combined predictor
pred_rpart <- predict(model_rpart,testing2)
pred_bag <- predict(model_bag,testing2)
pred_rf <- predict(model_rf,testing2)
pred_gbm <- predict(model_gbm,testing2)
pred_nb <- predict(model_nb,testing2)

predDF <- data.frame(pred_rpart,pred_bag,pred_rf,pred_gbm,pred_nb, classe=testing2$classe)
model_all <- train(classe ~., data=predDF)
pred_all <- predict(model_all,predDF)

sum(pred_rpart==predDF$classe) / 4904
sum(pred_bag==predDF$classe) / 4904
sum(pred_rf==predDF$classe) / 4904
sum(pred_gbm==predDF$classe) / 4904
sum(pred_nb==predDF$classe) / 4904
sum(pred_all==predDF$classe) / 4904
summary(predDF$classe)

#check: using only 3 models does not improve the overall result
predDF1 <- data.frame(pred_bag,pred_rf,pred_gbm, classe=testing2$classe)
model_all1 <- train(classe ~., data=predDF1)
pred_all1 <- predict(model_all1,predDF1)
sum(pred_all1==predDF$classe) / 4904

# calculate the excercise quality of the "testing" set

pred_t_rpart <- predict(model_rpart,testing)
pred_t_bag <- predict(model_bag,testing)
pred_t_rf <- predict(model_rf,testing)
pred_t_gbm <- predict(model_gbm,testing)
pred_t_nb <- predict(model_nb,testing)
pred_t_DF <- data.frame(pred_rpart=pred_t_rpart,pred_bag=pred_t_bag,pred_rf=pred_t_rf,pred_gbm=pred_t_gbm,pred_nb=pred_t_nb)

pred_t_all <- predict(model_all,pred_t_DF)

print(model_all)

pred_t_rpart
pred_t_bag
pred_t_rf
pred_t_gbm
pred_t_nb

pred_t_DF <- data.frame(pred_t_DF,pred_all=pred_t_all)












## ASSIGNMENT REGRESSION ##

data(mtcars)
View(mtcars)
ggplot(mtcars,aes(am,mpg)) + geom_point()
mdl <- lm(mpg ~ ., mtcars)
summary(mdl)

#based on full model the weight has the most influence on the miles per gallon outcom

mdl1 <- lm(mpg ~ am, mtcars)
summary(mdl1)

mdl2 <- lm(mpg ~ am + wt, mtcars)
summary(mdl2)

# from this model it shows that am is not significant.

mtcars$am1 <- as.factor(mtcars$am)
ggplot(mtcars,aes(wt,mpg,color=am1)) + geom_point()

ggplot(mdl2) + geom_point(aes(x=.fitted, y=.resid))

ggplot(mtcars,aes(wt,mpg,color=as.factor(am))) + geom_point()

# extra check: comparing the different models
anova(mdl,mdl1,mdl2)
anova(mdl1,mdl2)
# conclusion: am is not significant



# The following section is excluded from the assignment report

### Comparing all models

By using Anova the different models can be compared.

```{r}
anova(mdl,mdl1,mdl2)
anova(mdl1,mdl2)
```

The first check shows that the full model is worse than the models with transmission (am) included.

The second check shows that the model with weight included (model 3) is the best model.

These checks confirm the result of the analysis in the section above.

