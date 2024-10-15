#fit the model
model<- lm(data_clean$BODYFAT~data_clean$ABDOMEN+data_clean$WEIGHT_kg)
predicted<- predict(model)
summary(model)   
par(xaxs="i",yaxs="i")
summary(lm(data_clean$BODYFAT~predicted))
#calculate VIF values
library(car)
vif_values <- vif(model)
print(vif_values)

#Fitted vs Real plot

# Calculate the ±50% bounds
lower_bound_50percent <- predicted * 0.50
upper_bound_50percent <- predicted * 1.50  
percent_error <- abs((data_clean$BODYFAT - predictions) / data_clean$BODYFAT) * 100

# Determine colors based on the percentage error
colors <- ifelse(percent_error <= 20, "lightgreen", "lightblue")
par(mar = c(5, 15, 5, 5))
plot(predictions, data_clean$BODYFAT,
     col = colors, pch = 19, 
     main = "Predictions vs. Actual: Abdomen+Weight",
     xlab = "Predicted BodyFat (%)", 
     ylab = "",
     las= 1,
     xlim = c(0, 50), ylim = c(0, 50))
mtext("Actual BodyFat (%)", side = 2, line = 4, las = 1) 
abline(0, 1, col = "blue", lty = 2)
# Add a legend
legend("topleft", legend = c("Within +/- 20%", "Outside +/- 20%","R_squared:0.69","+/-50% line"),
       col = c("lightgreen", "lightblue","white","lightgrey"), pch = 19)

# Plot the fitted vs. actual values
# Add the ±%50 lines
lines(predicted, lower_bound_50percent, col = "darkgrey", lty = 2, lwd = 2)  
lines(predicted, upper_bound_50percent, col = "darkgrey", lty = 2, lwd = 2)  
within_20_percent <- sum(percent_error <= 20)
cat("Number of points within 20% of the true value:", within_20_percent)

#residual plot
par(mar = c(5, 10, 5, 5)) 
plot(fitted(model),resid(model),main = "Residual plot of ABDOMEN+WEIGHT_kg",xlab="Fitted values(BodyFat(%))",ylab="",pch =19, las = 1,col="lightblue")
mtext("Residuals", side=2, line=3, las=1)
abline(h=0,col="darkgrey",lty=2)
