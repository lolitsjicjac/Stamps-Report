library(tidyverse)
library(dplyr)
#Going to import the information


Purchase_Data <- readxl::read_excel("2018-03-12 Stamps.com Excel assessment.xlsx", 
                                    sheet = "Purchase Payment Data")
Purchase_Data$Date <- as.Date(Purchase_Data$Date, format = '%m/%d/%Y')
Purchase_Data <- Purchase_Data %>%
  mutate(month = format(Date, "%m"))

#1. Show total amount of purchases in each month by each company 
Purchase_Data %>%
  ggplot(aes(x = format(Date, "%Y-%m"), fill = Company))+
  geom_bar(color = 'Black') +
  labs(x = 'Month', y = "Count of Purchases by Month", title = "How many Purchases by Companies every Month")+
  geom_text(stat = 'count', aes(label=..count..), size = 3, position = position_stack(vjust = 0.5), color="white")


#2. Billing for each Company at the end of the month
Bill_Data <- Purchase_Data %>%
  group_by(month, Company) %>%
  summarise(sum_spend = sum(Spend, na.rm = TRUE), sum_payment = sum(Payment, na.rm = TRUE),
            Bill = sum_spend + sum_payment)

#This is the largest bill that Stamps will collect in the entire year
max(Bill_Data$Bill)

#3. What is the dollar amount of each customers highest bill
Bill_Data %>%
  select(Company, Bill) %>%
  group_by(Company) %>%
  summarise(max_bill = max(Bill))

#4 Profit margins is a flat percentage. 1st, 4th, 6th profitable person
Profitable_margin_customers <- Purchase_Data %>%
  group_by(Company) %>%
  summarise(total_purchase = sum(Spend, na.rm = TRUE)) %>%
  arrange(desc(total_purchase))

#These are the most profitable customers by purchases
Profitable_margin_customers[1,]
Profitable_margin_customers[4,]
Profitable_margin_customers[6,]

#Quick percentage breakdown of the information by the purchases amount over the year
#by each company
Profitable_margin_customers <- Profitable_margin_customers %>%
  mutate(prop = total_purchase / sum(total_purchase))
Profitable_margin_customers %>%
  ggplot(aes(x = '', y = prop, fill = Company)) +
  geom_bar(stat = 'identity', alpha = .8, color = 'Black')+
  coord_polar('y', start = 0) +
  labs(title = "Percentage of Purchases by Companies")

#The method would be similar with more companies as you are just finding the most profitable
# the ones that are buying the most.

#5 Credit Exposure
Bill_Data %>%
  group_by(month) %>%
  summarise(credit_exposure = sum(Bill))

#6 Most Valueable Customer YTD
Customer_Payments <- Purchase_Data %>%
  group_by(Company) %>%
  summarise(total_payments = sum(Payment, na.rm = TRUE)) %>%
  arrange(total_payments)

#The total payments broken down into percentages of companies
Customer_Payments <- Customer_Payments %>%
  mutate(prop = total_payments / sum(total_payments))

Customer_Payments %>%
  ggplot(aes(x = '', y = prop, fill = Company)) +
  geom_bar(stat = 'identity', alpha = .8, color = 'Black')+
  coord_polar('y', start = 0) +
  labs(title = "Percentage of Payments by Companies")

#If we go off the same as in question 4 and choose 1, 4, 6
Customer_Payments[1,]
Customer_Payments[4,]
Customer_Payments[6,]

## B) most valuable customer by month
Customer_Payments_month1 <- Purchase_Data %>%
  group_by(Company, month) %>%
  summarise(total_payments = sum(Payment, na.rm = TRUE)) %>%
  group_by(month) %>%
  summarise(total_payments = min(total_payments))
Customer_Payments_month2 <- Purchase_Data %>%
  group_by(Company, month) %>%
  summarise(total_payments = sum(Payment, na.rm = TRUE))
Customer_Payments_month <- left_join(Customer_Payments_month1, Customer_Payments_month2, by = c('month', 'total_payments'))

#7 Billed on 16 of each month

Purchase_Data %>%
  mutate(day = parse_date(fo))
