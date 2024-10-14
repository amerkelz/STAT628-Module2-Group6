rm(list=ls())
data_clean2 <- read.csv("data_clean 2")

data <- data_clean2
model <- lm(BODYFAT ~ WRIST + ABDOMEN, data = data)
# Create grid data for plotting
grid_data <- expand.grid(
  WRIST = seq(min(data$WRIST), max(data$WRIST), length.out = 100),
  ABDOMEN = seq(min(data$ABDOMEN), max(data$ABDOMEN), length.out = 100)
)

# Predict body fat values for the grid
grid_data$predicted_body_fat <- predict(model, newdata = grid_data)
grid_data <- grid_data %>%
  mutate(body_fat_category = case_when(
    predicted_body_fat > 20 ~ "Obese (>20%)",
    predicted_body_fat >= 13 & predicted_body_fat <= 20 ~ "Standard (13%-20%)",
    predicted_body_fat >= 5 & predicted_body_fat < 13 ~ "Lean (5%-13%)",
    TRUE ~ "Underweight (<5%)"  # Include an additional category if needed
  ))

ggplot(grid_data, aes(x = ABDOMEN, y = WRIST)) +
  geom_tile(aes(fill = body_fat_category)) +  
  geom_contour(aes(z = predicted_body_fat), color = "black", size = 0.5) +  
  scale_fill_manual(values = c("Lean (5%-13%)" = "blue", 
                               "Standard (13%-20%)" = "purple", 
                               "Obese (>20%)" = "red"),
                    name = "Body Fat Category") +
  labs(title = "Contour Plot of Predicted Body Fat",
       x = "Abdomen(cm)",
       y = "Wrist  (cm)") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(angle = 360, vjust = 0.5, hjust = 0.5)  # rotate y axis lable
  )