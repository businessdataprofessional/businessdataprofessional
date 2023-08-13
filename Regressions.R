#Building a regression model to forecast the time until the first engine overhaul
#Time in years based on four predictor variables: 
#(1) annual miles driven (Miles in 1,000s), 
#(2) average load weight (Load in tons)
#(3) average driving speed (Speed in mph)
#(4) oil change interval (Oil in 1,000s of miles). 

#    Fictitious sample of 25 trucks 

#Research questions to answer:
#1. For each predictor variable,will it have a positive or negative influence on time 
#   until the first engine overhaul? 
#   Answer: Miles, Load, and Oil are likely to have a negative influence on time until the next
#   overhaul, while Speed has a positive influence.

#2. What is the sample regression equation for the regression model (use all four predictor variables)
#   Answer: Time = 13.434279-0.089564(Miles) - 0.073179(Load) + 0.004753(Speed) - 0.029686(Oil)

#3. What is the time before the first engine overhaul for a particular truck driven 60,000 
#   miles per year with an average load of 22 tons, an average driving speed of 57 mph, 
#   and 18,000 miles between oil changes.
#   Answer: -0.089564(60,000) - 0.073179 (22) + 0.004753(57) - 0.029686(18000)= -5,881.323

library(readxl)
library(ggplot2)

# Read the Excel file
df <- read_excel("Engine.xlsx")

# Fit the linear regression model
Engine_lm <- lm(formula = Time ~ Miles + Load + Speed + Oil, data = df)

# Summary of the regression
summary(Engine_lm)

# Create scatter plots with fitted regression lines
scatter_plot <- ggplot(df, aes(x = Miles, y = Time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Time vs. Miles",
       x = "Miles",
       y = "Time")

scatter_plot

scatter_plot <- ggplot(df, aes(x = Load, y = Time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Time vs. Load",
       x = "Load",
       y = "Time")

scatter_plot

scatter_plot <- ggplot(df, aes(x = Speed, y = Time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Time vs. Speed",
       x = "Speed",
       y = "Time")

scatter_plot

scatter_plot <- ggplot(df, aes(x = Oil, y = Time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Time vs. Oil",
       x = "Oil",
       y = "Time")

scatter_plot

