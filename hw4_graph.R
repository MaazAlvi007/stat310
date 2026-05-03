# Set working directory (adjust if needed)
setwd("~/Desktop/STAT310/final_project/")

# Load libraries
library(ggplot2)
library(dplyr)

# Read data
df <- read.csv("international_migration_flow.csv", stringsAsFactors = FALSE)

# Filter for country "AD"
df_ad <- df %>%
  filter(country_from == "AD")

# Fix date format (YYYY-MM → YYYY-MM-01 → Date)
df_ad$migration_month <- as.Date(paste0(df_ad$migration_month, "-01"))

# Aggregate total migrants per month
monthly_ad <- df_ad %>%
  group_by(migration_month) %>%
  summarise(num_migrants = sum(num_migrants, na.rm = TRUE)) %>%
  arrange(migration_month)

# Create time index
monthly_ad$month_index <- seq_len(nrow(monthly_ad))

# Fit linear model
model <- lm(num_migrants ~ month_index, data = monthly_ad)

# Print model summary
print(summary(model))


# Optional: Scatter + regression
ggplot(monthly_ad, aes(x = migration_month, y = num_migrants)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_date(
    date_breaks = "2 months",   # 6 ticks per year
    date_labels = "%b\n%Y"          # show only year as label
  ) +
  labs(title = "Monthly Migration Trend (AD)",
       x = "Year",
       y = "Number of Migrants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Seeing how many people travel from country A to B every year
# aggregate(num_migrants~ country_from, data = migration, FUN = sum)
# Country to / Country from: 181

# country_codes <- read.csv("world-countries/countries/_combined/countries.csv")
# Total country codes: 193 

# Changing to uppercase
# country_codes$alpha2 <- toupper(country_codes$alpha2) 

