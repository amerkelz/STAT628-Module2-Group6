# find R^2 and RMSE of all two factors model(one is ABDOMEN,and try one of the remaining factors.)
data_clean2 <- read.csv("data_clean 2")
data_use <- data_clean2[, !(names(data_clean2) %in% c("IDNO","DENSITY"))]
data_use$HEIGHT_cm <- data_use$HEIGHT_m *100
data_use <- data_use[, !(names(data_use) %in% c("HEIGHT_m"))]
data_use_new <- data_use
results_new <- list()   # the result of every model
r_squared_new <- c()  # find the R^2 of every model
rmse_values_new <- c() # find the RMSE of every model
variables_new <- names(data_use_new)
variables_new <- variables_new[variables_new != "BODYFAT"]
variables_new <- variables_new[variables_new != "ABDOMEN"]
for (var in variables_new) {
  formula <- as.formula(paste("BODYFAT ~ ABDOMEN + ", var))
  model <- lm(formula, data = data_use_new)
  predictions <- predict(model, data_use_new)
  actuals <- data_use_new$BODYFAT
  rmse <- sqrt(mean((actuals - predictions)^2))
  r_squared_new[var] <- summary(model)$r.squared
  rmse_values_new[var] <- rmse
  results_new[[var]] <- summary(model)
}
sort(r_squared_new)
sort(rmse_values_new)