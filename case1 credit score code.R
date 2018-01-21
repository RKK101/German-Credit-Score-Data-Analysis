
#Credit score data

#Loading data
german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")
german_credit$response = german_credit$response - 1
head(german_credit)
dim(credit.train)
german_credit$response = as.factor(german_credit$response)
set.seed(10714314)
#subset
subset <- sample(nrow(german_credit), nrow(german_credit) * 0.75)
credit.train = german_credit[subset, ]
credit.test = german_credit[-subset, ]


#glm
set.seed(10714314)
credit.glm0 <- glm(response~., family = binomial, credit.train)
summary(credit.glm0)
credit.glm.step <- step(credit.glm0, direction = c("both")) #variable selection with AIC
credit.glm1<-glm(response ~ chk_acct + duration + credit_his + purpose + amount + 
                   saving_acct + installment_rate + sex + property + age + other_install + 
                   n_credits + telephone,family = binomial, credit.train)
summary(credit.glm1)
AIC(credit.glm1)
BIC(credit.glm1)
credit.glm1$deviance/credit.glm1$df.residual
#in sample
prob.glm.insample <- predict(credit.glm1, type = "response")
predicted.glm.insample <- prob.glm.insample > 1/6
predicted.glm.insample <- as.numeric(predicted.glm.insample)
#confusion matrix
table(credit.train$response, predicted.glm.insample, dnn = c("Truth", "Predicted"))
#misclassification cost
mean(ifelse(credit.train$response!= predicted.glm.insample, 1, 0))
#asymmetric cost
creditcost(credit.train$response, predicted.glm.insample)
# ROC CURVE
library(ROCR)
predin=prediction(prob.glm.insample,credit.train$response)#1st arg is pred prob
perfin=performance(predin,"tpr","fpr")
plot(perfin,colorize=TRUE)#ROC PLOT
AUCin=as.numeric(performance(predin,"auc")@y.values)#AUC
AUCin
#out sample
prob.glm.outsample <- predict(credit.glm1, credit.test, type = "response") #pred probabilities
predicted.glm.outsample<-prob.glm.outsample>1/6
predicted.glm.outsample=as.numeric(predicted.glm.outsample)

#confusion matrix
table(credit.test$response, predicted.glm.outsample, dnn = c("Truth", "Predicted"))
#misclassification rate
mean(ifelse(credit.test$response != predicted.glm.outsample, 1, 0))
#asymmetric cost
creditcost(credit.test$response, predicted.glm.outsample)
#ROC CURVE
predout=prediction(prob.glm.outsample,credit.test$response)#1st arg is pred prob
perfout=performance(predout,"tpr","fpr")
plot(perfout,colorize=TRUE)#ROC PLOT
AUCout=as.numeric(performance(predout,"auc")@y.values)#AUC
AUCout
#cv
pcut=1/6
cost1 <- function(r, pi) {
  mean(((r == 0) & (pi > pcut)) | ((r == 1) & (pi < pcut)))
}

#cost
creditcost <- function(observed, predicted) {
  weight1 = 5
  weight0 = 1
  c1 = (observed == 1) & (predicted == 0)  #logical vector - true if actual 1 but predict 0
  c0 = (observed == 0) & (predicted == 1)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

library(boot)
credit.glm.cv<- glm(response ~ chk_acct + duration + credit_his + purpose + amount + 
                      saving_acct + installment_rate + sex + property + age + other_install + 
                      n_credits + telephone, family = binomial, german_credit)
cv.result = cv.glm(german_credit, credit.glm.cv,cost=creditcost,3)
cv.result$delta[2]


prob.glm.CV.outsample <- predict(credit.glm1, credit.test, type = "response") #pred probabilities
predicted.glm.outsample<-prob.glm.outsample>1/6
predicted.glm.outsample=as.numeric(predicted.glm.outsample)
?cv.glm

library(DAAG)
a=CVbinary(credit.glm.cv, rand=NULL, nfolds=3, print.details=TRUE)
a

#CART
library(rpart)
credit.rpart <- rpart(formula = response ~ . , data = credit.train, method = "class", 
                      parms = list(loss = matrix(c(0, 5, 1, 0), nrow = 2)))
credit.rpart
plot(credit.rpart)
text(credit.rpart)
plotcp(credit.rpart)
prune.credit<-prune(credit.rpart,cp=0.013)
printcp(prune.credit)
plot(prune.credit)
text(prune.credit)

library(tree)

tree1 <- tree(response ~ ., data = credit.train)
m1 = prune.misclass(tree1, best = 16)

summary(m1)

#in smaple prediction
credit.train.pred.tree=predict(prune.credit, credit.train, type = "class")
table(credit.train$response, credit.train.pred.tree, dnn = c("Truth", "Predicted"))
#Aymmetric cost
creditcost(credit.train$response, credit.train.pred.tree)
#misclassification rate
mean(ifelse(credit.train$response != credit.train.pred.tree, 1, 0))

#ROC
credit.train.prob.rpart = predict(prune.credit, credit.train, type = "prob")
predtreein=prediction(credit.train.prob.rpart[,2],credit.train$response)
perftreein=performance(predtreein,"tpr","fpr")
plot(perftreein,colorize=TRUE)#ROC PLOT
AUCtreein=as.numeric(performance(predtreein,"auc")@y.values)#AUC
AUCtreein

#out sample prediction
credit.test.pred.tree = predict(prune.credit, credit.test, type = "class")
#confusion matrix
table(credit.test$response, credit.test.pred.tree, dnn = c("Truth", "Predicted"))
#Aymmetric cost
creditcost(credit.test$response, credit.test.pred.tree)
#misclassification rate
mean(ifelse(credit.test$response != credit.test.pred.tree, 1, 0))

#ROC
credit.test.prob.rpart = predict(prune.credit, credit.test, type = "prob")
predtreeout=prediction(credit.test.prob.rpart[,2],credit.test$response)
perftreeout=performance(predtreeout,"tpr","fpr")
plot(perftreeout,colorize=TRUE)#ROC PLOT
AUCtreeout=as.numeric(performance(predtreeout,"auc")@y.values)#AUC
AUCtreeout

#GAM

head(credit.train)
library(mgcv)

k1<-length(unique(credit.train$present_resid))-1
k2<-length(unique(credit.train$n_credits))-1

credit.gam0 <- gam(response ~ chk_acct+s(duration)+credit_his+purpose+ 
                  s(amount)+ saving_acct+ present_emp+(installment_rate)+sex+other_debtor+ 
                  s(present_resid, k=k1)+property+age+other_install+housing+s(n_credits,k=k2)+
                  job+telephone+foreign,family = binomial, data = credit.train)


summary(credit.gam0)
AIC(credit.gam)
BIC(credit.gam)

plot(credit.gam0, shade = TRUE, seWithMean = TRUE, scale = 0)
credit.gam<-gam(response~ chk_acct+s(duration)+credit_his+purpose+ 
                  s(amount)+ saving_acct+ present_emp+(installment_rate)+sex+other_debtor+ property+other_install+housing+
                  job+telephone+foreign,family = binomial, data = credit.train)
summary(credit.gam)
#Performance 
AIC(credit.gam)
BIC(credit.gam)
credit.gam$deviance/credit.gam$df.residual #mean residual deviance

#in sample performance

pcut.gam=1/6

prob.gam.in <- predict(credit.gam, credit.train, type = "response")
pred.gam.in <- (prob.gam.in >= pcut.gam) * 1
table(credit.train$response, pred.gam.in, dnn = c("Observation", "Prediction"))
creditcost(credit.train$response,pred.gam.in)#asymmetric classification
mean(ifelse(credit.train$response != pred.gam.in, 1, 0))#asymmetric classification

#ROC

pred.roc.gam.in=prediction(as.numeric(prob.gam.in),as.numeric(credit.train$response))#1st arg is pred prob
perf.roc.gam.in=performance(pred.roc.gam.in,"tpr","fpr")
plot(perf.roc.gam.in,colorize=TRUE)#ROC PLOT
AUC.roc.gam.in=as.numeric(performance(pred.roc.gam.in,"auc")@y.values)#AUC
AUC.roc.gam.in


#out of sample prediction
pcut=1/6
prob.gam.out=predict(credit.gam,credit.test,type="response")#predicted prob of Y=1
pred.gam.out <- (prob.gam.out >= pcut) * 1#predicted 0,1
table(credit.test$response, pred.gam.out, dnn = c("Observation", "Prediction"))
#mis-classifciation rate is
mean(ifelse(credit.test$response != pred.gam.out, 1, 0))
# COST ASSOCIATED WITH misclassification
creditcost(credit.test$response,pred.gam.out)#asymmetric classification
#ROC
pred.roc.gam.out=prediction(as.numeric(prob.gam.out),as.numeric(credit.test$response))
perf.roc.gam.out=performance(pred.roc.gam.out,"tpr","fpr")
plot(perf.roc.gam.out,colorize=TRUE)#ROC PLOT
AUC.roc.gam.out=as.numeric(performance(pred.roc.gam.out,"auc")@y.values)#AUC
AUC.roc.gam.out


#Linear Discriminant Analysis LDA
library(MASS)
credit.lda <- lda(response ~ ., data = credit.train)
summary(credit.lda)
AIC(credit.lda)
#in sample prediciton
pcut=1/6
prob.lda.in <- predict(credit.lda, data = credit.train) #returns a 1ist
pred.lda.in <- (prob.lda.in$posterior[, 2] >= pcut) * 1 # need posterior prob of Y=1 frmo  prob.lds.in[,2] 
table(credit.train$response, pred.lda.in, dnn = c("Obs", "Pred"))
mean(ifelse(credit.train$response != pred.lda.in, 1, 0))
creditcost(credit.train$response,pred.lda.in) #asymmetric cost

#out of sample prediction
prob.lda.out <- predict(credit.lda, newdata = credit.test) #returns a 1ist #pred prob
pred.lda.out <- as.numeric((prob.lda.out$posterior[, 2] >= pcut)) # 0,1 prediction
table(credit.test$response, pred.lda.out, dnn = c("Obs", "Pred")) #confusion matrix
mean(ifelse(credit.test$response != pred.lda.out, 1, 0))
creditcost(credit.test$response,pred.lda.out) #asymmetric cost

pred_train_lda <- prediction(prob.lda.in$posterior[, 2], credit.train$response)
perf_train_lda <- performance(pred_train_lda, "tpr", "fpr")
plot(perf_train_lda, colorize = TRUE, main = "ROC Curve: Training Data")
as.numeric(performance(pred_train_lda, 'auc')@y.values)




#ROC#####
pred.roc.lda.out=prediction(prob.lda.out$posterior[, 2],credit.test$response)
perf.roc.lda.out=performance(pred.roc.lda.out,"tpr","fpr")
plot(perf.roc.lda.out,colorize=TRUE)
AUC.roc.lda.out=as.numeric(performance(pred.roc.lda.out,"auc")@y.values)#AUC
AUC.roc.lda.out

#Neural Networks
library(nnet)

credit.nnet <- nnet(response ~ ., data = credit.train, size = 1, maxit = 500)
#in samples

#out of sample prediction
prob.nnet = predict(credit.nnet, credit.test)#predicted prob
pred.nnet = as.numeric(prob.nnet > 1/6)
table(credit.test$response, pred.nnet, dnn = c("Observation", "Prediction"))
creditcost(credit.test$response,pred.nnet)#asymmetric misclassification cost
mean(ifelse(credit.test$response != pred.nnet, 1, 0))
