library(readr)
# load the altered data
original_data <- read_csv("altered_data.csv")
#check the summary of original data
summary(original_data)
# delete the NA
clean_data <- na.omit(original_data)
# change the date format
clean_data$Transaction_Date <- as.Date(clean_data$Transaction_Date, format = "%Y/%m/%d")
library(dplyr)
# set the baseline
baseline_date <- as.Date("2019-12-31")
#get the numeric value for RFMT model
customer_stats <- clean_data %>%
  group_by(CustomerID) %>%
  summarise(
    Transaction_Count = n(),
    Tenure=first(Tenure_Months),
    Days_Since_Last_Transaction = as.numeric(baseline_date - max(Transaction_Date)),
    Average_Spending = mean(Online_Spend, na.rm = TRUE)
  ) %>%
  arrange(Transaction_Count)

#print and get summary of the numeric data
print(customer_stats)
summary(customer_stats$Tenure)
summary(customer_stats$Transaction_Count)
summary(customer_stats$Days_Since_Last_Transaction)
summary(customer_stats$Average_Spending)

# define a function to remove outliers based on the IQR method
remove_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25)
  Q3 <- quantile(df[[column]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df <- df %>% filter(df[[column]] >= lower_bound & df[[column]] <= upper_bound)
  return(df)
}

# Apply the function to the specified columns
columns_to_clean <- c('Tenure', 'Transaction_Count', 'Days_Since_Last_Transaction', 'Average_Spending')
for (column in columns_to_clean) {
  customer_stat_cleaned <- remove_outliers(customer_stats, column)
}

write.csv(customer_stat_cleaned, "customer_stat_cleaned.csv", row.names = FALSE)

#score the RFMT model
n=3
RFMT_data <- customer_stat_cleaned %>%
  mutate(
    T_Score = ntile(Tenure, n),
    F_Score = ntile(Transaction_Count, n),
    R_Score = (n+1)-ntile(Days_Since_Last_Transaction, n),
    M_Score = ntile(Average_Spending, n)
  ) %>%
  select(CustomerID, R_Score, F_Score, M_Score, T_Score)

write.csv(RFMT_data, "RFMT.csv", row.names = FALSE)
