
#boxplots and histograms 
    boxplot(data_clean$BODYFAT,main= paste("Boxplot of BODYFAT(%)"),col= "lightblue")
    hist(data_clean$BODYFAT,main= paste("Histogram of BODYFAT"),col= "lightblue",xlab= "BODYFAT(%)",border = "black",breaks = 20)

    boxplot(data_clean$ABDOMEN,main= paste("Boxplot of ABDOMEN"),col= "lightblue")
    hist(data_clean$ABDOMEN,main= paste("Histogram of ABDOMEN",column),col= "lightblue",xlab= "ABDOMEN",border = "black",breaks = 20)

    boxplot(data_clean$AGE,main= paste("Boxplot of AGE"),col= "lightblue")
    hist(data_clean$AGE,main= paste("Histogram of AGE"),col= "lightblue",xlab= "AGE",border = "black",breaks = 20)
    boxplot(data_clean$ADIPOSITY,main= paste("Boxplot of ADIPOSITY"),col= "lightblue")
    hist(data_clean$ADIPOSITY,main= paste("Histogram of ADIPOSITY"),col= "lightblue",xlab= "ADIPOSITY",border = "black",breaks = 20)
    
    boxplot(data_clean$NECK,main= paste("Boxplot of NECK"),col= "lightblue")
    hist(data_clean$NECK,main= paste("Histogram of NECK"),col= "lightblue",xlab= "NECK",border = "black",breaks = 20)
    
    boxplot(data_clean$CHEST,main= paste("Boxplot of CHEST"),col= "lightblue")
    hist(data_clean$CHEST,main= paste("Histogram of CHEST"),col= "lightblue",xlab= "CHEST",border = "black",breaks = 20)
    
    boxplot(data_clean$HIP,main= paste("Boxplot of HIP"),col= "lightblue")
    hist(data_clean$HIP,main= paste("Histogram of HIP"),col= "lightblue",xlab= "HIP",border = "black",breaks = 20)
    

    boxplot(data_clean$THIGH,main= paste("Boxplot of THIGH"),col= "lightblue")
    hist(data_clean$THIGH,main= paste("Histogram of THIGH"),col= "lightblue",xlab= "THIGH",border = "black",breaks = 20)
    
    
    boxplot(data_clean$KNEE,main= paste("Boxplot of KNEE"),col= "lightblue")
    hist(data_clean$KNEE,main= paste("Histogram of KNEE"),col= "lightblue",xlab= "KNEE",border = "black",breaks = 20)
    
    boxplot(data_clean$ANKLE,main= paste("Boxplot of ANKLE"),col= "lightblue")
    hist(data_clean$ANKLE,main= paste("Histogram of ANKLE"),col= "lightblue",xlab= "ANKLE",border = "black",breaks = 20)
    
    
    boxplot(data_clean$BICEPS,main= paste("Boxplot of BICEPS"),col= "lightblue")
    hist(data_clean$BICEPS,main= paste("Histogram of BICEPS"),col= "lightblue",xlab= "BICEPS",border = "black",breaks = 20)
    
    boxplot(data_clean$FOREARM,main= paste("Boxplot of FORARM"),col= "lightblue")
    hist(data_clean$FOREARM,main= paste("Histogram of FORARM"),col= "lightblue",xlab= "FORARM",border = "black",breaks = 20)
    
    boxplot(data_clean$WRIST,main= paste("Boxplot of WRIST"),col= "lightblue")
    hist(data_clean$WRIST,main= paste("Histogram of WRITST"),col= "lightblue",xlab= "WRIST",border = "black",breaks = 20)
    
    boxplot(data_clean$WEIGHT_kg,main= paste("Boxplot of WEIGHT"),col= "lightblue")
    hist(data_clean$WEIGHT_kg,main= paste("Histogram of WEIGHT"),col= "lightblue",xlab= "WEIGHT",border = "black",breaks = 20)
    
    boxplot(data_clean$HEIGHT_m,main= paste("Boxplot of HIGHT"),col= "lightblue")
    hist(data_clean$HEIGHT_m,main= paste("Histogram of HIGHT"),col= "lightblue",xlab= "HIGHT",border = "black",breaks = 20)
    
###for lm model or lasso model, we can plot the fitted vs real
    model<- lm(data_clean$BODYFAT~data_clean$ABDOMEN+data_clean$WEIGHT_kg)
    predicted<- predict(model)
    summary(model)   
    par(xaxs="i",yaxs="i")
    plot(predicted,data_clean$BODYFAT,col= "lightblue",pch=19,asp=1,xlim=c(0,50),ylim=c(0,50))
    abline(lm(data_clean$BODYFAT~predicted),col= "black")
    summary(lm(data_clean$BODYFAT~predicted))
    legend("topleft",legend = c("R_squared:0.68","Slope:1.00","Intercept:0"))
    install.packages("glmnet")
    library(glmnet)
    X <- as.matrix(data_clean[, c("ABDOMEN", "WRIST")])  
    y <- data_clean$BODYFAT  
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
    #QQ-plot and residual plot
     qqnorm(resid(model),main="Normal Q-Q plot of residuals",col="lightblue",pch=19)
     qqline(resid(model,col="darkgrey"))
     
     plot(fitted(model),resid(model),main = "Residual plot",xlab="Fitted values",ylab="Residuals",pch =19,col="lightblue")
     abline(h=0,col="darkgrey",lty=2)
    ###3D plot and heatplot from chatGPT
     
     install.packages("plotly")  
     library(plotly)
     plot_ly(x = ~data_clean$ABDOMEN, y = ~data_clean$WEIGHT_kg, z = ~data_clean$BODYFAT, type = "scatter3d", mode = "markers") %>%
       layout(title = "3D Scatter Plot",
              scene = list(xaxis = list(title = "ABDOMEN"),
                           yaxis = list(title = "WEIGHT"),
                           zaxis = list(title = "BODYFAT")))
    
     #heat
     fit <- lm(BODYFAT ~ ABDOMEN + WEIGHT_kg, data = data_clean)
     x_vals <- seq(min(data_clean$ABDOMEN), max(data_clean$ABDOMEN), length.out = 50)
     y_vals <- seq(min(data_clean$WEIGHT_kg), max(data_clean$WEIGHT_kg), length.out = 50)
     grid <- expand.grid(ABDOMEN = x_vals, WEIGHT_kg = y_vals)
     grid$predicted_BODYFAT <- predict(fit, newdata = grid)
     z_matrix <- matrix(grid$predicted_BODYFAT, nrow = length(x_vals), ncol = length(y_vals))
     heatmap(z_matrix, 
             Rowv = NA, Colv = NA,  # 不进行行列聚类
             xlab = "ABDOMEN", ylab = "WEIGHT_kg", 
             main = "Heatmap of Predicted BODYFAT",
             col = heat.colors(256))
    #3d
     x_vals <- seq(min(data_clean$ABDOMEN), max(data_clean$ABDOMEN), length.out = 50)
     y_vals <- seq(min(data_clean$WEIGHT_kg), max(data_clean$WEIGHT_kg), length.out = 50)

     grid <- expand.grid(ABDOMEN = x_vals, WEIGHT_kg = y_vals)

     grid$predicted_BODYFAT <- predict(fit, newdata = grid)

     z_matrix <- matrix(grid$predicted_BODYFAT, nrow = length(x_vals), ncol = length(y_vals))
     
       fig <- plot_ly() %>%
       add_markers(x = ~data_clean$ABDOMEN, 
                   y = ~data_clean$WEIGHT_kg, 
                   z = ~data_clean$BODYFAT, 
                   marker = list(size = 3),  
                   name = "Data Points") %>%
       add_surface(x = ~x_vals, y = ~y_vals, z = ~z_matrix, 
                   showscale = FALSE, name = "Fitted Plane") %>%
       layout(scene = list(xaxis = list(title = "ABDOMEN"),
                           yaxis = list(title = "WEIGHT_kg"),
                           zaxis = list(title = "BODYFAT")),
              title = "3D Scatter Plot with Fitted Regression Plane")

     fig
     
     