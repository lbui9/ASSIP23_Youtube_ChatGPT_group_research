data <- readr::read_csv("Final_Main_Coder.csv")

contingency_table <- table(data$`Type of Video`, data$`Risk of Bullshit`)
fisher_result <- fisher.test(contingency_table, simulate.p.value = TRUE)
p_value <- fisher_result$p.value
cat("p-value:", p_value, "\n")

# Extract observed values
observed_values <- contingency_table
print(observed_values)

# Perform chi-square test to calculate expected values
chi_result <- chisq.test(contingency_table)
expected_values <- chi_result$expected

print(expected_values)


# Convert observed_values to a data frame
observed_data <- as.data.frame.table(observed_values)

# Perform chi-square test to calculate expected values
chi_result <- chisq.test(observed_values)
expected_values <- chi_result$expected

# Convert expected_values to a data frame
expected_data <- as.data.frame.table(expected_values)

# Rename the columns
names(observed_data) <- c("Type_of_Video", "Risk_of_Bullshit", "Observed")
names(expected_data) <- c("Type_of_Video", "Risk_of_Bullshit", "Expected")

# Reorder the factor levels
observed_data$Type_of_Video <- factor(observed_data$Type_of_Video, levels = c("Advertisement", "Education", "Entertainment", "Events", "Experiment", "Experiment Illegal", "Cautionary", "Hype"))
observed_data$Risk_of_Bullshit <- factor(observed_data$Risk_of_Bullshit, levels = c("Reducing Risk", "Low", "High", "Bullshit"))
expected_data$Type_of_Video <- factor(expected_data$Type_of_Video, levels = c("Advertisement", "Education", "Entertainment", "Events", "Experiment", "Experiment Illegal", "Cautionary", "Hype"))
expected_data$Risk_of_Bullshit <- factor(expected_data$Risk_of_Bullshit, levels = c("Reducing Risk", "Low", "High", "Bullshit"))


library(ggplot2)
library(dplyr)


# Combine observed and expected data
combined_data <- bind_rows(
  observed_data %>% mutate(Source = "Observed"),
  expected_data %>% mutate(Source = "Expected")
)

# Reorder levels for better visualization
combined_data$Type_of_Video <- factor(
  combined_data$Type_of_Video,
  levels = c(
    "Advertisement", "Education", "Entertainment", "Events",
    "Experiment", "Experiment Illegal", "Cautionary", "Hype"
  )
)
combined_data$Risk_of_Bullshit <- factor(
  combined_data$Risk_of_Bullshit,
  levels = c("Reducing Risk", "Low", "High", "Bullshit")
)

# Balloon plot for overlapping observed and expected values
ggplot(combined_data, aes(x = Risk_of_Bullshit, y = Type_of_Video)) +
  geom_point(aes(size = ifelse(Source == "Observed", Observed, Expected),
                 fill = Source), shape = 21, position = position_nudge(x = -0.1)) +
  labs(title = "Combined Observed and Expected Values Balloon Plot",
       x = "Risk of Bullshit", y = "Type of Video") +
  theme_minimal() +
  scale_size(range = c(10, 20)) +  # Increase bubble sizes
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  guides(size = FALSE, fill = guide_legend(title = "Source")) +
  scale_fill_manual(values = c("blue", "red"),  # Set colors for observed and expected
                    guide = guide_legend(override.aes = list(size = 4, alpha = 0.5)))  # Decrease darkness shade of bubbles

# Balloon plot for combined observed and expected values
ggplot(combined_data, aes(x = factor(Risk_of_Bullshit), y = Type_of_Video)) +
  geom_point(aes(size = ifelse(Source == "Observed", Observed, Expected),
                 fill = Source), shape = 21, position = position_dodge(width = 0.75)) +
  labs(title = "Combined Observed and Expected Values Balloon Plot",
       x = "Risk of Bullshit", y = "Type of Video") +
  theme_minimal() +
  scale_size(range = c(5, 15)) +
  scale_x_discrete(
    breaks = levels(combined_data$Risk_of_Bullshit),
    labels = levels(combined_data$Risk_of_Bullshit),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  guides(size = FALSE, fill = guide_legend(title = "Source")) +
  scale_fill_manual(values = c("blue", "red")) +
  coord_cartesian(clip = "off")  
