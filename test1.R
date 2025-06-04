# Read csv and clean names and print column names
 
library(tidyverse)
library(ggplot2)
read.csv(“10_Property_stolen_and_recovered.csv”) -> property




#1. Read csv and clean column names and print column names (1)
read_csv("10_Property_stolen_and_recovered.csv") %>%
  clean_names() -> data1 
colnames(data1)


# 2. Rename area_name to state_ut
data1 %>%
  rename(state_ut = area_name)

# 3. How many state_ut are there?
n_distinct(data$state_ut)

#4. How many crime groups are there and what are those?
unique_crime_groups <- unique(data$group_name)
length(unique_crime_groups)
unique_crime_groups

# 5. What is “Total Property” in group_name and what should we do?
data1 %>%
  filter(group_name != "Total Property") -> data2
print(data)
 
# Q6: Total number of property stolen cases and total value – All India

# Sum total stolen cases and total value across India
total_cases <- sum(data$cognizable_ipc_crimes, na.rm = TRUE)
total_value <- sum(data$property_stolen_value_rs_in_lakhs, na.rm = TRUE)

cat("Total Stolen Cases (All India):", total_cases, "\n")
cat("Total Value of Property Stolen (Rs. in Lakhs):", total_value, "\n")


# Q7: Year-wise total stolen cases and total stolen value

library(dplyr)
library(ggplot2)

# Group by year
yearly_data <- data %>%
  group_by(year) %>%
  summarise(
    total_cases = sum(cognizable_ipc_crimes, na.rm = TRUE),
    total_value = sum(property_stolen_value_rs_in_lakhs, na.rm = TRUE)
  )

# Print table
print(yearly_data)

# Plotting
ggplot(yearly_data, aes(x = year)) +
  geom_line(aes(y = total_cases, color = "Cases"), size = 1.2) +
  geom_line(aes(y = total_value, color = "Value (in lakhs)"), size = 1.2) +
  labs(title = "Year-wise Stolen Cases and Value (All India)",
       y = "Count / Value",
       x = "Year") +
  scale_color_manual(name = "Legend", values = c("Cases" = "blue", "Value (in lakhs)" = "red")) +
  theme_minimal()

# Q8: Total stolen cases by state with a bar plot

state_data <- data %>%
  group_by(state_ut) %>%
  summarise(total_cases = sum(cognizable_ipc_crimes, na.rm = TRUE)) %>%
  arrange(desc(total_cases))

# Print table
print(state_data)

# Plot
ggplot(state_data, aes(x = reorder(state_ut, -total_cases), y = total_cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Stolen Cases by State", x = "State/UT", y = "Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), minimal = TRUE)


# Q9: Correlation between stolen and recovered value

# Load corrplot package
library(corrplot)

# Select relevant columns
cor_data <- data %>%
  select(property_stolen_value_rs_in_lakhs, property_recovered_value_rs_in_lakhs)

# Compute correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")

# Plot correlation
corrplot(cor_matrix, method = "circle", type = "upper",
         title = "Correlation Between Stolen and Recovered Value")



# Q10: Stolen and recovered values year-wise (in lakhs)

yearwise_cases <- data %>%
  group_by(year) %>%
  summarise(
    stolen = sum(property_stolen_value_rs_in_lakhs, na.rm = TRUE),
    recovered = sum(property_recovered_value_rs_in_lakhs, na.rm = TRUE)
  )

# Print table
print(yearwise_cases)

# Plot
ggplot(yearwise_cases, aes(x = year)) +
  geom_line(aes(y = stolen, color = "Stolen"), size = 1.2) +
  geom_line(aes(y = recovered, color = "Recovered"), size = 1.2) +
  labs(title = "Year-wise Stolen vs Recovered Property Value",
       x = "Year", y = "Value (Rs. in Lakhs)") +
  scale_color_manual(name = "Legend", values = c("Stolen" = "darkred", "Recovered" = "darkgreen")) +
  theme_minimal()



 