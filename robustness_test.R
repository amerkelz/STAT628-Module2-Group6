#Robustness Test

data_clean2 <- read.csv("data_clean 2.csv",sep=";",dec=",")
data_use <- data_clean2[, !(names(data_clean2) %in% c("X","IDNO","DENSITY"))]
data_use$HEIGHT_cm <- data_use$HEIGHT_m *100
data_use <- data_use[, !(names(data_use) %in% c("HEIGHT_m"))]
data_use_new <- data_use

#take 4 randomly selected points from the cleaned data set, get the model predictions, then compare with US Navy Method result

set.seed(1) #set seed to ensure reproducibility
test_set <- data_use_new[sample(nrow(data_use_new),4),]

#Abdomen model:
ab_model <- lm("BODYFAT ~ ABDOMEN",data_use_new)
test_ab <- test_set$ABDOMEN*ab_model$coefficients[2] + ab_model$coefficients[1]


#Abdomen & Weight model:
ab_wt_model <- lm("BODYFAT ~ ABDOMEN + WEIGHT_kg", data_use_new)
test_ab_wt <- test_set$ABDOMEN*ab_wt_model$coefficients[2] + ab_wt_model$coefficients[1] + test_set$WEIGHT_kg*ab_wt_model$coefficients[3]

#Abdomen & Height model:
ab_ht_model <- lm("BODYFAT ~ ABDOMEN + HEIGHT_cm", data_use_new)
test_ab_ht <- test_set$ABDOMEN*ab_ht_model$coefficients[2] + ab_ht_model$coefficients[1] + test_set$HEIGHT_cm*ab_ht_model$coefficients[3]

#US Navy Method Results for test set, according to https://www.calculator.net/body-fat-calculator.html
test_prior_model <- c(28.2,17.9,21.0,20)

#prior model vs. abdomen model:
ab_ss <- sum((test_ab - test_prior_model)**2)

#prior model vs. abdomen and weight model:
ab_wt_ss <- sum((test_ab_wt - test_prior_model)**2)

#prior model vs abdomen and height model:
ab_ht_ss <- sum((test_ab_ht - test_prior_model)**2)

print(ab_ss)
print(ab_wt_ss)
print(ab_ht_ss)