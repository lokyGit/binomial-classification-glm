library(nnet)
library(Boruta)
library(ROCR)

#Reading the data from csv
oddEven.data = read.csv(file.choose(), header = T)

#Structure of the datset
str(oddEven.data)

#Changing the odd_degree column to factor
oddEven.data$odd_degree = as.factor(oddEven.data$odd_degree)

#Structure of the datset
str(oddEven.data)

table(oddEven.data$odd_degree)

#Partition of the dataset into train and test
set.seed(1)
oddEven_indi =  sample(2, nrow(oddEven.data), replace = T, prob =  c(0.7, 0.3))
oddEven.train = oddEven.data[oddEven_indi == 1,]
oddEven.test = oddEven.data[oddEven_indi == 2,]

#Logistic regression model

oddEven.glm.model = glm(odd_degree~. , family=binomial(link = "logit"), 
                        data = oddEven.test, maxit = 150)

summary(oddEven.glm.model)

#implementing Boruta method to decide on feature importance.

boruta_oddEven = Boruta(odd_degree ~ ., data = oddEven.test, doTrace=2)
boruta_oddEven$finalDecision

# collect Confirmed variables
oddEven_signif = names(boruta_oddEven$finalDecision[boruta_oddEven$finalDecision 
                                                    %in% c("Confirmed" )])  
View(oddEven_signif)  # significant variables

plot(boruta_oddEven, cex.axis=.7, las=2, xlab="Variables", main="Odd Even - Variable Importance")


# Considering significant columns

oddEven.glm.model.sigTrain = glm(odd_degree~ x1 +	x2 +	x3 +	x4 +	x5 +	
                                   x6 +	x7 +	x8 +	x9 +	x10 +	x11 +	x12 +	x13 +	x15 +	x16 +	
                                   x17 +	x18 +	x19 +	x20 +	x21 +	x23 +	x182 +	x184 +
                                   x185 +	x186 +	x187 +	x188 +	x189 +	x190 +	x191 +	
                                   x192 + x193 +	x194 +	x195 +	x196 +	x197 +	x198 +	
                                   x199 +	x200 + x201, 
                                 family=binomial(link = "logit"), data = oddEven.train, 
                                 maxit = 100)

oddEven.glm.model.sigTrain$y

summary(oddEven.glm.model.sigTrain)

# # further filtering the significant columns
oddEven.glm.model.sigTrain = glm(odd_degree ~ x1 +	x2 +	x3 +	x4 +	
                                   x5 +	x6 +	x7 +	x8 +	x9 +	x11   +	x187 +	x188 +	
                                   x189 +	x190 +	x191  +	x193 +	x194 +	x195 +	x199 +	x200, 
                                 family=binomial(link = "logit"), data = oddEven.train, 
                                 maxit = 100)

oddEven.glm.model.sigTrain$y

summary(oddEven.glm.model.sigTrain)

# Prediction on train data

trainPred = predict(oddEven.glm.model.sigTrain, oddEven.train, type = "response")

View(trainPred)
head(trainPred)
head(oddEven.train)

# Prediction on test data

testPred =predict(oddEven.glm.model.sigTrain, oddEven.test, type = "response")

View(testPred)
head(testPred)
head(oddEven.test)

# Misclassification error - train data
pred.train = ifelse(trainPred>0.5, 1, 0)
trainTab = table (Predicted = pred.train, Actual = oddEven.train$odd_degree)
trainTab 
trainError = 1-sum(diag(trainTab))/sum(trainTab) # error rate-train = 0.4137931
accuracy_train = 1-trainError

# Misclassification error - test data
pred.test = ifelse(testPred>0.5, 1, 0)
View(pred.test)
testTab = table( Predicted = pred.test, Actual = oddEven.test$odd_degree)
testTab
testError = 1 - sum(diag(testTab))/sum(testTab) # error rate-test = 0.4901316
accuracy_test = 1-testError

# Multinomial logistic regression, not asked in the assignment. measured out of curosity

oddEven.multi.model = multinom(odd_degree ~., data = oddEven.train)

oddEven.multi.model

#predictions using multinomial logistic regression, measured out of curosity
pred.multi = predict(oddEven.multi.model, oddEven.train, type = "probs")
pred.multi

View(pred.multi)
head(oddEven.multi.model)

# Misclassification error for multinomial logistic regression

multi.Pred = ifelse(pred.multi > 0.5 , 1, 0)
multi.Pred

# Prediction table for multinomial logistic regression
multiPred.tab =  table (Predicted = multi.Pred, Actual =oddEven.train$odd_degree)

multi.Error = 1 - sum(diag(multiPred.tab))/sum(multiPred.tab)

multi.Error # multinomial error rate = 0.4425287

# Recieving Operating Characteristic ( ROC ) curve - train 

rocPred = prediction(pred.train, oddEven.train$odd_degree)
roc.train = performance(rocPred, "tpr", "fpr")
#rocSpec = performance(rocPred, "sens", "spec")
plot(roc.train, colorize = T, main =  "ROC Curve - Train")
abline(a=0, b=1)
#plot(rocSpec)

# Recieving Operating Characteristic ( ROC ) curve - test 
rocPred.test = prediction(pred.test, oddEven.test$odd_degree)
roc.train = performance(rocPred.test, "tpr", "fpr")
plot(roc.train, colorize = T, main =  "ROC Curve - Test")
abline(a=0, b=1)

# Recieving Operating Characteristic ( ROC ) curve - multinomial - self measured

rocPred.multi = prediction(multi.Pred, oddEven.train$odd_degree)
roc.multi = performance(rocPred.multi, "tpr", "fpr")
plot(roc.multi, colorize = T, main = "ROC Curve - multinomial")
abline(a=0, b=1)

# Area under the curve (AUC) - train
auc.train = performance(rocPred, "auc")
auc.train = unlist(slot(auc.train,"y.values"))
auc.train = round(auc.train, 4)
legend(0.6,0.3, auc.train, title ='AUC')


# Area under the curve (AUC) - test
auc.test = performance(rocPred.test, "auc")
auc.test = unlist(slot(auc.test,"y.values"))
auc.test = round(auc.test, 4)
legend(0.6,0.3, auc.test, title ='AUC')


# Area under the curve (AUC) - multinomial -self measured
auc.multi = performance(rocPred.multi, "auc")
auc.multi = unlist(slot(auc.multi,"y.values"))
auc.multi = round(auc.test, 4) #0.5573741
legend(0.6,0.3, auc.multi, title ='AUC', cex = 1.2)











