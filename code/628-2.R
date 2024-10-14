#visualization ignore it 
hist(data_clean_with_correct_height$BODYFAT,breaks =45)
hist(data_clean_with_correct_height$ABDOMEN, breaks = 30)
boxplot(data_clean_with_correct_height$BODYFAT)
plot(data_clean_with_correct_height$ABDOMEN,data_clean_with_correct_height$BODYFAT)
abline(lm(data_clean_with_correct_height$BODYFAT ~ data_clean_with_correct_height$ABDOMEN),col= "red")
#Trying different model     
#1 factor
model1_1<-lm(BodyFat$BODYFAT~BodyFat$WEIGHT)
summary(model1_1)
model1_2<-lm(BodyFat$BODYFAT~BodyFat$HEIGHT)
summary(model1_2)
model1_3<-lm(BodyFat$BODYFAT~BodyFat$ADIPOSITY)
summary(model1_3)
model1_4<-lm(BodyFat$BODYFAT~BodyFat$NECK)
summary(model1_4)
model1_5<-lm(BodyFat$BODYFAT~BodyFat$CHEST)
summary(model1_5)
model1_6<-lm(BodyFat$BODYFAT~BodyFat$ABDOMEN)
summary(model1_6)
model1_7<-lm(BodyFat$BODYFAT~BodyFat$HIP)
summary(model1_7)
model1_8<-lm(BodyFat$BODYFAT~BodyFat$THIGH)
summary(model1_8)
model1_9<-lm(BodyFat$BODYFAT~BodyFat$KNEE)
summary(model1_9)
model1_10<-lm(BodyFat$BODYFAT~BodyFat$ANKLE)
summary(model1_10)
model1_11<-lm(BodyFat$BODYFAT~BodyFat$BICEPS)
summary(model1_11)
model1_12<-lm(BodyFat$BODYFAT~BodyFat$FOREARM)
summary(model1_12)
model1_13<-lm(BodyFat$BODYFAT~BodyFat$WRIST)
summary(model1_13)
#After trying simple linear model, we choose more correlated factors: ABDOMEN, ADIPOSITY, WEIGHT,HIP, THIGH, CHEST


model4<-lm(data_clean_with_correct_height$BODYFAT~data_clean_with_correct_height$ADIPOSITY+data_clean_with_correct_height$ABDOMEN)
summary((model4))

model5<-lm(data_clean_with_correct_height$BODYFAT~data_clean_with_correct_height$ABDOMEN+data_clean_with_correct_height$WEIGHT_kg)
summary(model5)

model6<-lm(data_clean_with_correct_height$BODYFAT~data_clean_with_correct_height$ABDOMEN+data_clean_with_correct_height$HIP)
summary(model6)

model7<-lm(data_clean_with_correct_height$BODYFAT~data_clean_with_correct_height$ABDOMEN+data_clean_with_correct_height$THIGH)
summary(model7)

model8<-lm(data_clean_with_correct_height$BODYFAT~data_clean_with_correct_height$ABDOMEN+data_clean_with_correct_height$CHEST)
summary(model8)
#Testing correlation: these factors are highly correlated, but the VIF<10
cor.test(data_clean_with_correct_height$HIP, data_clean_with_correct_height$ABDOMEN)
library(car)
vif(lm(data_clean_with_correct_height$BODYFAT ~ data_clean_with_correct_height$ABDOMEN + data_clean_with_correct_height$CHEST + data_clean_with_correct_height$HIP))


#LASSO: add penalty
install.packages("glmnet")
library(glmnet)
X <- as.matrix(data_clean_with_correct_height[, c("ABDOMEN", "HIP","THIGH","CHEST")])  
y <- data_clean_with_correct_height$BODYFAT  
lasso_model <- glmnet(X, y, alpha = 1)
cv_lasso <- cv.glmnet(X, y, alpha = 1)
best_lambda <- cv_lasso$lambda.min
print(best_lambda)
lasso_coefficients <- coef(cv_lasso, s = "lambda.min")
print(lasso_coefficients)
predictions <- predict(cv_lasso, s = "lambda.min", newx = X)
y_true <- y  
tss <- sum((y_true - mean(y_true))^2)
rss <- sum((y_true - predictions)^2)
r_squared <- 1 - rss/tss
print(paste("R-squared: ", r_squared))











