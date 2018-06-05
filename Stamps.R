library(tidyverse)
library(dplyr)

options(scipen=999)


#Going to import the information


Purchase_Data <- readxl::read_excel("2018-03-12 Stamps.com Excel assessment.xlsx", 
                                    sheet = "Purchase Payment Data")
Purchase_Data$Date <- as.Date(Purchase_Data$Date, format = '%m/%d/%Y')
Purchase_Data <- Purchase_Data %>%
  mutate(month = format(Date, "%m"), 
         day_of_week = lubridate::wday((Date), label = TRUE))

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

#7) Billing cycle 16 to 15 of each month for each purchase
#Here we are going to break up all the dates into the intervals that we need in order to
#have the desired billing cycles.
#January's Billing cycle
January_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-01-16" & Purchase_Data$Date <= "2017-02-15",]
January_Billing_Cycle <- January_Billing_Cycle %>%
                          group_by(Company) %>%
                          summarise(January = sum(Spend, na.rm = TRUE))
#February's Billing Cycle
February_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-02-16" & Purchase_Data$Date <= "2017-03-15",]
February_Billing_Cycle <- February_Billing_Cycle %>%
                          group_by(Company) %>%
                          summarise(February = sum(Spend, na.rm = TRUE))
#March's Billing Cycle
March_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-03-16" & Purchase_Data$Date <= "2017-04-15",]
March_Billing_Cycle <- March_Billing_Cycle %>%
                        group_by(Company) %>%
                        summarise(March = sum(Spend, na.rm = TRUE))
#April's Billing Cycle
April_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-04-16" & Purchase_Data$Date <= "2017-05-15",]
April_Billing_Cycle <- April_Billing_Cycle %>%
                        group_by(Company) %>%
                        summarise(April = sum(Spend, na.rm = TRUE))
#May's Billing Cycle
May_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-05-16" & Purchase_Data$Date <= "2017-06-15",]
May_Billing_Cycle <- May_Billing_Cycle %>%
                      group_by(Company) %>%
                      summarise(May = sum(Spend, na.rm = TRUE))
#June's Billing Cycle
June_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-06-16" & Purchase_Data$Date <= "2017-07-15",]
June_Billing_Cycle <- June_Billing_Cycle %>%
                      group_by(Company) %>%
                      summarise(June = sum(Spend, na.rm = TRUE))
#July's Billing Cycle
July_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-07-16" & Purchase_Data$Date <= "2017-08-15",]
July_Billing_Cycle <- July_Billing_Cycle %>%
                      group_by(Company) %>%
                      summarise(July = sum(Spend, na.rm = TRUE))
#August's BIlling Cycle
August_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-08-16" & Purchase_Data$Date <= "2017-09-15",]
August_Billing_Cycle <- August_Billing_Cycle %>%
                        group_by(Company) %>%
                        summarise(August = sum(Spend, na.rm = TRUE))
#September's Billing Cycle
September_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-09-16" & Purchase_Data$Date <= "2017-10-15",]
September_Billing_Cycle <- September_Billing_Cycle %>%
                            group_by(Company) %>%
                            summarise(September = sum(Spend, na.rm = TRUE))
#October's Billing Cycle
October_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-10-16" & Purchase_Data$Date <= "2017-11-15",]
October_Billing_Cycle <- October_Billing_Cycle %>%
                          group_by(Company) %>%
                          summarise(October = sum(Spend, na.rm = TRUE))
#November's Billing Cycle
November_Billing_Cycle <- Purchase_Data[Purchase_Data$Date >= "2017-11-16" & Purchase_Data$Date <= "2017-12-15",]
November_Billing_Cycle <- November_Billing_Cycle %>%
                          group_by(Company) %>%
                          summarise(November = sum(Spend, na.rm = TRUE))
#Now we want to combine all the segments into one data frame that hold the total amount
#of orders purchased by each company for each billing cycle
Billing_Cycle <- left_join(January_Billing_Cycle, February_Billing_Cycle, by='Company') %>%
  left_join(., March_Billing_Cycle, by = "Company") %>%
  left_join(., April_Billing_Cycle, by='Company') %>%
  left_join(., May_Billing_Cycle, by = "Company") %>%
  left_join(., June_Billing_Cycle, by = 'Company') %>%
  left_join(., July_Billing_Cycle, by= 'Company') %>%
  left_join(., August_Billing_Cycle, by = "Company") %>%
  left_join(., September_Billing_Cycle, by = "Company") %>%
  left_join(., October_Billing_Cycle, by = "Company") %>%
  left_join(., November_Billing_Cycle, by = "Company")
                           
                           
#7) Finally we have the final answer in a data frame that shows the billing cycle
#of each company for their purchase in that time
Billing_Cycle



############ Misc work ########################
#Getting ready to create some machine learning for a forecast of future purchases by each company
#for the year of 2018
Purchase_Data %>%
  ggplot(aes(x = Date, y = Spend, color = Company))+
  facet_wrap(~ Company, ncol = 5, scales = 'free')+
  geom_point(aes(color = day_of_week), na.rm = TRUE)+
  geom_line(na.rm = TRUE, show.legend = FALSE)+
  geom_smooth(na.rm = TRUE, color = 'black')


Purchase_Data %>%
  filter(Purchase_Data$Company == 'Scott') %>%
  group_by(month, Company) %>%
  summarise(sum = sum(Spend, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = sum))+
  geom_line(na.rm = TRUE, show.legend = FALSE)+
  geom_point(na.rm = TRUE)
  






