# Load necessary libraries
library(readxl)
library(pastecs)
library(dplyr)
library(ggplot2)

options(scipen = 999)

# Set working directory (if needed)
getwd()

# Import dataset
salaries <- read_excel("/Users/florenciastrada/Desktop/MAESTRIA DE DATOS/2.TALLER PYTHON/TRABAJO 1/TPE1_STRADA/DatosTPE1_STRADA.xlsx")
View(salaries)

# Filter only records where the currency is USD (to ensure comparability)
salaries_usd <- salaries %>% 
  filter(salary_currency == "USD")
View(salaries_usd)

# 1. General descriptive statistics in USD
descriptive_stats <- stat.desc(salaries_usd, basic = TRUE)
print(descriptive_stats)

# 2. Group statistics by year
stats_by_year <- salaries_usd %>%  
  group_by(work_year) %>% 
  summarise(
    average = mean(salary_in_usd, na.rm = TRUE),
    total   = sum(salary_in_usd, na.rm = TRUE),
    count   = n()
  )
print(stats_by_year)

# 3. Statistics by experience level using tapply
mean_salary   <- tapply(salaries_usd$salary_in_usd, salaries_usd$experience_level, mean, na.rm = TRUE)
sd_salary     <- tapply(salaries_usd$salary_in_usd, salaries_usd$experience_level, sd, na.rm = TRUE)
median_salary <- tapply(salaries_usd$salary_in_usd, salaries_usd$experience_level, median, na.rm = TRUE)
max_salary    <- tapply(salaries_usd$salary_in_usd, salaries_usd$experience_level, max, na.rm = TRUE)

descriptive_table <- round(cbind(mean_salary, median_salary, sd_salary, max_salary), digits = 1)
print(descriptive_table)

# Export the descriptive table to CSV
write.csv2(descriptive_table, file = "Descriptive_by_groups.csv", row.names = TRUE)

# 4. Average salary by currency type (for comparison, even though we filtered USD)
aggregate(salary_in_usd ~ salary_currency, salaries, mean)

# 5. Create a variable to classify salaries as "high" or "low"
salaries_usd <- salaries_usd %>% 
  mutate(high_low = ifelse(salary_in_usd >= 80000, "high", "low"))

# 6. Plots

## Histogram of Salaries in USD using ggplot2
ggplot(salaries_usd, aes(x = salary_in_usd)) +
  geom_histogram(binwidth = 5000, fill = "#1f78b4", color = "white", alpha = 0.8) +
  labs(title = "Histogram of Salaries in USD",
       x = "Salary in USD",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

## Histograms for Senior and Junior experience levels using facets
ggplot(salaries_usd %>% filter(experience_level %in% c("Senior", "Junior")),
       aes(x = salary_in_usd, fill = experience_level)) +
  geom_histogram(binwidth = 5000, color = "black", alpha = 0.7) +
  facet_wrap(~ experience_level) +
  labs(title = "Salary Distribution by Experience Level",
       x = "Salary in USD",
       y = "Count") +
  scale_fill_manual(values = c("Senior" = "darkgreen", "Junior" = "darkorange")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

## Scatter plot: Salary vs. Experience Level colored by Company Size
ggplot(salaries_usd, aes(x = experience_level, y = salary_in_usd, color = company_size)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Salary in USD by Experience Level and Company Size",
       x = "Experience Level",
       y = "Salary in USD",
       color = "Company Size") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

## Pie chart: Distribution of Experience Levels
ggplot(salaries_usd, aes(x = factor(1), fill = experience_level)) +
  geom_bar(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Experience Levels",
       fill = "Experience Level") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

## Bar chart: Count by Experience Level
ggplot(salaries_usd, aes(x = experience_level, fill = experience_level)) +
  geom_bar(width = 0.7, show.legend = FALSE) +
  coord_flip() +
  labs(title = "Count by Experience Level",
       x = "Experience Level",
       y = "Count") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

## Density plot of Salaries in USD
ggplot(salaries_usd, aes(x = salary_in_usd, fill = experience_level)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density of Salaries in USD",
       x = "Salary in USD",
       y = "Density",
       fill = "Experience Level") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

## Count plot: Job Titles by Experience Level
ggplot(salaries_usd, aes(x = experience_level, y = job_title)) +
  geom_count(color = "blue", alpha = 0.7) +
  labs(title = "Number of Job Titles by Experience Level",
       x = "Experience Level",
       y = "Job Title") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))


## Density Plot: Salaries in USD
ggplot(salaries_usd, aes(x = salary_in_usd, fill = experience_level)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density of Salaries in USD",
       x = "Salary in USD",
       y = "Density",
       fill = "Experience Level") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

## Heat Map: Salary in USD by Experience Level and Company Size
ggplot(salaries_usd, aes(x = experience_level, y = company_size, fill = salary_in_usd)) +
  geom_tile(color = "white") +
  labs(title = "Heat Map: Salary in USD",
       x = "Experience Level",
       y = "Company Size",
       fill = "Salary in USD") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )


# 7. NEW SECTION: Group by Country and Job Title

## Calculate average, median salary and count by country and job title
job_salary_by_country <- salaries_usd %>%
  group_by(employee_residence, job_title) %>%
  summarise(
    average = mean(salary_in_usd, na.rm = TRUE),
    median  = median(salary_in_usd, na.rm = TRUE),
    count   = n()
  ) %>%
  arrange(desc(average))
print(job_salary_by_country)

# 8. Group by Country: Calculate average, median, maximum salary and count
salary_by_country <- salaries_usd %>%
  group_by(employee_residence) %>%
  summarise(
    average = mean(salary_in_usd, na.rm = TRUE),
    median  = median(salary_in_usd, na.rm = TRUE),
    maximum = max(salary_in_usd, na.rm = TRUE),
    count   = n()
  ) %>%
  arrange(desc(average))
print(salary_by_country)

## Bar chart: Average Salary in USD by Country
ggplot(salary_by_country, aes(x = reorder(employee_residence, average), y = average)) +
  geom_bar(stat = "identity", fill = "#4682B4", width = 0.7) +
  coord_flip() +
  labs(title = "Average Salary in USD by Country",
       x = "Country",
       y = "Average Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))

