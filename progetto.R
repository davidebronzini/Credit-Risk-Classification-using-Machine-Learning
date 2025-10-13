library(ISLR2)
library(dplyr)
library(nnet)
library(glmnet)
library(readxl)
Dati <- read_excel("C:/Users/WINDOWS 10/Desktop/statistica/Dati.xlsx")
View(Dati)
head(Dati)
### data pre-processing
#to clean data from NAN values 
Dati[Dati=="n.a."]<-NA  #the string for NAN is wrong we need to substitue it
clean_data <- na.omit(Dati)
summary(clean_data)
# the purpouse is classification with 3 classes for credit score, 
 #but since in the data we have more than 3 classes it is necessary to convert the variable to simplify it
clean_data$Score<-as.factor(clean_data$Score)
clean_data<-clean_data %>% 
  mutate(Score=case_when(
    Score %in% c("AAA","AA","A")~"high",
    Score %in% c("BBB","BB","B","NA")~"medium",
    Score %in% c("CCC","CC","C","D")~"low"))
str(clean_data)
any(is.na(clean_data$Score)) # there is some value of score wtih strange sign which are not classified
clean_data <- clean_data %>%
  mutate(Score = ifelse(is.na(Score), "medium", Score))
any(is.na(clean_data$Score))
any(is.na(clean_data))
clean_data <- clean_data %>% select(-`Company ID`, -Year)
numeric_cols <- clean_data %>% select(-Score) %>% names()
clean_data <- clean_data %>%
  mutate(across(all_of(numeric_cols), as.numeric))

# Check the structure and summary of clean_data after conversion
str(clean_data)
summary(clean_data)
pairs(clean_data [,-1])
cor(clean_data[,-1])
x <- model.matrix(Score ~ ., data = clean_data)[, -1]
y <- clean_data$Score

###linear classifcation models
#multinomial logistic regression with ridge for penalization and cross-k validation approach
fit <-glmnet(x, as.factor(y), family = "multinomial", alpha = 0)# we use as factor function to ensure categorical response
coef(fit) #the glmnet for logistic regression coeff given each class case
#sparse matrix means that almost is elements is 0. This is due to regularization componet
set.seed(17)
cv_fit <- cv.glmnet(x, as.factor(y), family = "multinomial", alpha = 0, nfolds = 10,type="class") #specify class to have misclassifcation error taye instead of deviance
cv_fit# here we have minimum lambda with relative best cv error

best_lambda <- cv_fit$lambda.min

final_model <- glmnet(x, as.factor(y), family = "multinomial", alpha = 0, lambda = best_lambda)
coef(final_model)

predictions <- predict(final_model, s = best_lambda, newx = x, type = "class")
conf_matrix <- table( Predicted = predictions, actual=clean_data$Score  )
conf_matrix
(56+31+3127)/4419 #accurancy of trainig set


#LDA with cross k-validatio
library(caret)
set.seed(17)
train_control <- trainControl(method = "cv", number = 10)
lda_model <- train(x, as.factor(y), method = "lda", trControl = train_control)
cv_error2 <- 1 - lda_model$results$Accuracy
cv_error2
lda_model$results
final_lda_model <- lda_model$finalModel
lda_model$finalModel
lda_predictions <- predict(lda_model, x)
conf_matrix2<- table(Predicted = lda_predictions, Actual = y)
conf_matrix2
(10+11+3128)/4419 #training set accurancy

#ft QDA with cross k validation
qda_model <- train(x, as.factor(y), method = "qda", trControl = train_control)
cv_error3 <- 1 - qda_model$results$Accuracy
cv_error3
qda_model$results
final_qda_model <- qda_model$finalModel
qda_model$finalModel

qda_predictions <- predict(qda_model, x)
conf_matrix3<- table(Predicted = qda_predictions, Actual = y)
conf_matrix3
(830+41+576)/4419 #training set accurancy

# naive bayes
library(klaR) 
nv.bayesmodel <- train(x, as.factor(y), method = "naive_bayes", trControl = train_control)
cv_error4 <- 1 - nv.bayesmodel$results$Accuracy
cv_error4
nv.bayesmodel$results
final_nv.bayesmodel <- nv.bayesmodel$finalModel
nv.bayesmodel$finalModel
class_means <- final_nv.bayesmodel$tables
class_means


nv.bayespredictions <- predict(nv.bayesmodel, x)
conf_matrix4<- table(Predicted = nv.bayespredictions, Actual = y)
conf_matrix4
(318+199+2861)/4419 #training set accurancy

#K nearest neighbor
knn_model <- train(x, as.factor(y), method = "knn", trControl = train_control)
cv_error5 <- 1 - knn_model$results$Accuracy
cv_error5   
knn_model$results
final_knn_model <- knn_model$finalModel
knn_model$finalModel

knn_predictions <- predict(knn_model, x)
conf_matrix5 <- table(Predicted = knn_predictions, Actual = y)
conf_matrix5
(3069+152+618)/4419   #training set accurancy



#box plot of test errorr
cv_errors <- list(
  LDA = cv_error2,
  QDA = cv_error3,
  Naive_Bayes = cv_error4,
  kNN = cv_error5,
  Multinomial_Logistic = 0.2734
)

boxplot(cv_errors,
        main = "TEST ERRORS FOR DIFFERENT CLASSIFICATION METHODS",
        ylab = "TEST ERROR ", xlab = "METHOD",
        col = "skyblue", border = "red", notch = FALSE)

###non linear classification model

#tree
library(tree)
set.seed(17)
datatree<-data.frame(x,y)
train<-sample(1:nrow(datatree),size = 0.7 * nrow(datatree))
datatree.test<-datatree[-train,]
y.test<-y[-train]
tree.fit<-tree(as.factor(y)~.,datatree, subset=train)
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty=0)
tree.fit
set.seed(17)
cv_tree<-cv.tree(tree.fit,FUN = prune.misclass)
cv_tree
par(mar = c(5, 5, 5, 5))
plot(cv_tree$size,cv_tree$dev)
prune_tree<-prune.misclass(tree.fit,best=10)
par(mar = c(0.1, 0.1, 0.1, 0.1))
summary(prune_tree)
plot(prune_tree)
text(prune_tree, pretty=0)
tree.pred<-predict(prune_tree,datatree.test,type="class")
tb_tree<-table(tree.pred,as.factor(y.test))
tb_tree
(164+97+817)/1326 # accurancy

#bagging
library(randomForest)
set.seed(17)
bagging.fit<-randomForest(as.factor(y)~.,datatree,subset=train,mtry=8,ntree=500,importance=TRUE)
bagging.fit
pred_bagg<-predict(bagging.fit,datatree.test,type="class")
tb_bag<-table(Predicted=pred_bagg, Actual=as.factor(y.test))
tb_bag
(166+80+839)/1326
importance(bagging.fit)
varImpPlot(bagging.fit)

#random forest
set.seed(17)
forest.fit<-randomForest(as.factor(y)~.,datatree,subset=train,mtry=3,ntree=500,importance=TRUE)
forest.fit
pred_forest<-predict(forest.fit,datatree.test,type="class")
tb_forest<-table(Predicted=pred_forest, Actual=as.factor(y.test))
tb_forest
(148+80+865)/1326

importance(forest.fit)
varImpPlot(forest.fit)

cv_errors_tot <- list(
  LDA = cv_error2,
  QDA = cv_error3,
  Naive_Bayes = cv_error4,
  kNN = cv_error5,
  Multinomial_Logistic = 0.2734,
  Tree=0.1871,
  Bagging=0.1818,
  Rand_for=0.1758
)

boxplot(cv_errors_tot,
        main = "TEST ERRORS FOR DIFFERENT CLASSIFICATION METHODS",
        ylab = "TEST ERROR ", xlab = "METHOD",
        col = "skyblue", border = "red", notch = FALSE)





