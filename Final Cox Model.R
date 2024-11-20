# Load necessary libraries
library(survival)  #for cox ph
library(ggplot2)   #for plotting
library(gganimate) #for animation
library(dplyr)     #for data manipulation

# Step 1: Upload my .csv file
data <- read.csv("C:/Users/heath/OneDrive/Documents/Computational Modeling/R Projects/Computation Modeling R Projects/Time-to-event.csv")

# Create a new time variable representing day 1 to day 170.
data$time <- sample(1:170, nrow(data), replace = TRUE) 

# Check the first few rows to ensure the data is correct
head(data)

# Step 2: Fit Cox proportional hazards model for "support"
cox_model_support <- coxph(Surv(time, status) ~ support, data = data)

# Step 3: Fit Cox proportional hazards model for "satisfaction"
cox_model_satisfaction <- coxph(Surv(time, status) ~ satisfaction, data = data)

# Step 4: Predict survival probabilities for both models by using the means
mean_support <- mean(data$support)
mean_satisfaction <- mean(data$satisfaction)

# Predict survival for "support"
surv_fit_support <- survfit(cox_model_support, newdata = data.frame(support = mean_support))

# Predict survival for "satisfaction"
surv_fit_satisfaction <- survfit(cox_model_satisfaction, newdata = data.frame(satisfaction = mean_satisfaction))

# Create survival plot data for both support and satisfaction
surv_data_support <- data.frame(
  time = surv_fit_support$time,
  surv = surv_fit_support$surv,
  lower = surv_fit_support$lower,
  upper = surv_fit_support$upper,
  variable = "Support"
)

surv_data_satisfaction <- data.frame(
  time = surv_fit_satisfaction$time,
  surv = surv_fit_satisfaction$surv,
  lower = surv_fit_satisfaction$lower,
  upper = surv_fit_satisfaction$upper,
  variable = "Satisfaction"
)

# Combine the data for both support and satisfaction
surv_data_combined <- bind_rows(surv_data_support, surv_data_satisfaction)

# Step 5: Animate survival probabilities over time for both variables
p <- ggplot(surv_data_combined, aes(x = time, y = surv, color = variable, group = variable)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = variable), alpha = 0.1) + 
  labs(title = "Survival Probability Over Time by Support and Satisfaction", 
       x = "Time (Days)", y = "Survival Probability") +
  theme_minimal() +
  scale_color_manual(values = c("slategray4","saddlebrown")) +  # Colors of lines
  scale_fill_manual(values = c("lightblue", "indianred")) +   # Colors of ribbons (this represents the CI's)
  transition_reveal(time)

# Render the animation
animate(p, duration = 10, fps = 20)

# Save the animation
anim_save("cox_model_animation.gif")

cat("Animation completed. Check your working directory for cox_model_animation.gif\n")

# Display summary of the Cox models, these are our coefficients 
summary(cox_model_support)
summary(cox_model_satisfaction)

##References##

#Importance of social support and life satisfaction: https://www.researchgate.net/publication/328141633_Relationship_between_Social_Support_and_Life_Satisfaction_of_College_Students_Resilience_As_a_Mediator_and_Moderator
#Social support and recovery: https://pmc.ncbi.nlm.nih.gov/articles/PMC10259869/#:~:text=Additionally%2C%20past%20research%20has%20suggested,et%20al.%2C%201993).
#Life satisfaction and recovery: https://pmc.ncbi.nlm.nih.gov/articles/PMC2629650/
#MSPSS scale measurement for SS: https://www.researchgate.net/publication/311534896_Multidimensional_Scale_of_Perceived_Social_Support_MSPSS_-_Scale_Items_and_Scoring_Information?__cf_chl_tk=_2FKpFhShiwSjcy17LtqNq.5psrAi610FACIdywapxc-1730943256-1.0.1.1-ISGo4FpLNphYS4OUKzbFRKJWu3V.lvVxUrIR4vK181c
#survival models in SA research: https://pmc.ncbi.nlm.nih.gov/articles/PMC6527490/#S13
#DR Cox paper on hazards model: https://rss.onlinelibrary.wiley.com/doi/10.1111/j.2517-6161.1972.tb00899.x
#Life satisfaction measurement: https://labs.psychology.illinois.edu/~ediener/Documents/Diener-Emmons-Larsen-Griffin_1985.pdf