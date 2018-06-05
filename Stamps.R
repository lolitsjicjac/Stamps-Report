library(tidyverse)
library(dplyr)
library(tidyquant)
library(caret)
library(broom)
library(timetk)
library(modelr)
options(na.action = na.warn)
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
  ggplot(aes(x = Date, y = Spend, group = 1))+
  facet_wrap(~ Company, ncol = 5, scales = 'free')+
  geom_point(aes(color = day_of_week), na.rm = TRUE)+
  geom_line(na.rm = TRUE, show.legend = FALSE, color = palette_light()[[1]])+
  geom_smooth(na.rm = TRUE, color = 'blue')+
  scale_color_manual(values = palette_light())+
  theme_tq()+
  labs(color = "Days of the Week", title = "Purchases From The Entire Year by each Company", x = 'Days')


#Looking at the Graph Nancy
Purchase_Data %>%
  filter(Purchase_Data$Company == 'Nancy') %>%
  group_by(Date) %>%
  summarise(sum = sum(Spend, na.rm = TRUE)) %>%
  ggplot(aes(x = Date, y = sum, group = 1))+
  geom_line(size = 1, alpha = 0.8)+
  geom_point()+
  geom_smooth(se = FALSE)+
  labs(y = 'Spend', title = "Nancy's Purchases in 2017")
  
Nancy <- Purchase_Data %>%
  filter(Purchase_Data$Company == 'Nancy')

#If we consider the companies to be constant and we just look at total sales 
Purchase_Data %>%
  group_by(Date, day_of_week) %>%
  summarise(total_spend = sum(Spend, na.rm = TRUE)) %>%
  ggplot(aes(x = Date, y = total_spend, group = 1))+
  geom_point(na.rm = TRUE, aes(color = day_of_week))+
  geom_line(na.rm = TRUE)+
  geom_smooth()

#This is the total spend for the entire year.
purchase_spend <- Purchase_Data %>%
                  group_by(Date, day_of_week) %>%
                  summarise(total_spend = sum(Spend, na.rm = TRUE))
#Now we are going to create out training and test data to go ahead an do our machine learning
#Here just set the entire data set to training and then later we will filter out to have our test
#Data so that we can incorporate all of the values.
purchase_spend <- purchase_spend %>%
  mutate(model = ifelse(Date <= '2017-12-31', 'train', 'test'))

#Looking at our Data . Ignore this bc our data is sort so we must include all of it in our training.
purchase_spend %>%
  ggplot(aes(x = Date, y = total_spend, color = model))+
  geom_point(alpha = .5)+
  geom_line(alpha = .5)+
  scale_color_manual(values = palette_light())+
  theme_tq()
#Here we are augmenting the time series. So we have to make sure that the values that are in the 
#tk_augment are not going to overlap with the current names/labels we have in our data.
#So we make sure to change any name that may cause some problems to the variables that the 
#augmentation adds on to the data
purchase_spend_aug <- purchase_spend %>%
  rename(date = Date) %>%
  select(model, date, total_spend) %>%
  tk_augment_timeseries_signature()
purchase_spend_aug <- purchase_spend_aug[complete.cases(purchase_spend_aug),]

#Preprocess
#We want to get rid of values that will contribute nothing to our model. So values that do not 
#correlate
library(matrixStats)
#Here we are now creating a data frame that will return to us all of the variables with have a 
#0 correlation with our original augmented data set. We filter at the end to make sure it is all the 
#values that are 0
(var <- data.frame(colnames = colnames(purchase_spend_aug[, sapply(purchase_spend_aug, is.numeric)]),
                  colvars = colVars(as.matrix(purchase_spend_aug[, sapply(purchase_spend_aug, is.numeric)]))) %>%
  filter(colvars == 0))
#Here we are selecting our original augmentation and now using the one_of function to call all 
#of the variables that are present in the augmented data that match the var data, which is all the values
#which do no correlate with the data, and thus removes them.
purchase_spend_aug <- select(purchase_spend_aug, -one_of(as.character(var$colnames)))
#Now we remove those highly correlated values
library(ggcorrplot)
cor <- cor(purchase_spend_aug[, sapply(purchase_spend_aug, is.numeric)])
p.cor <- cor_pmat(purchase_spend_aug[, sapply(purchase_spend_aug, is.numeric)])
ggcorrplot(cor, type = 'upper', outline.col = 'white', hc.order = TRUE, p.mat = p.cor,
           colors = c(palette_light()[1], 'white', palette_light()[2]))

cor_cut <- findCorrelation(cor, cutoff=0.9) 
purchase_spend_aug <- select(purchase_spend_aug, -one_of(colnames(cor)[cor_cut]))
#Modeling. Here we must be sure to pick our testing data. Since out data is one full year (jan-dec)
#We had to include the entire data in training because then our testing data will have values that are
#also present in the data. By changing the range of our testing we can get a much more solid prediction our
#our actual data from our predicted values.
train <- purchase_spend_aug %>%
  filter(model == 'train') %>%
  select(-model)
test <- purchase_spend_aug %>%
  filter(date >= '2017-09-30') %>%
  mutate(model = 'test')

#Here we are creating a GLM regression for our data
fit_lm <- glm(total_spend ~., data = train)

#Examining out model by visualizing how it is coming
augment(fit_lm) %>%
  ggplot(aes(x= date, y = .resid))+
  geom_hline(yintercept = 0, color = 'red')+
  geom_point(alpha = .5)+
  geom_smooth()

#Now we will add predictions and residuals for test data
pred_test <- test %>%
  add_predictions(fit_lm, 'pred_lm') %>%
  add_residuals(fit_lm, 'resid_lm')
pred_test %>%
  ggplot(aes(x = date, y = resid_lm))+
  geom_hline(yintercept = 0, color = 'red')+
  geom_point(alpha = .5)+
  geom_smooth()

#Now we can compare our predicted data with the actual test data
pred_test %>%
  gather(x,y, total_spend, pred_lm) %>%
  ggplot(aes(x = date, y=y, color = x))+
  geom_point(alpha = .5)+
  geom_line(alpha=.5)+
  scale_color_manual(values = palette_light())+
  theme_tq()

#Forecasting
#Extracting the index (to create future time points we extract time index)
idx <- purchase_spend %>%
  tk_index()

purchase_spend_aug %>%
  ggplot(aes(x = date, y = diff))+
  geom_point(alpha = .5, aes(color = as.factor(diff)))+
  geom_line(alpha = .5)+
  scale_color_manual(values = palette_light())+
  theme_tq()

#Finding the days that are missing from our data
purchase_spend_aug %>%
  select(date, wday.lbl, diff) %>%
  filter(diff > 86400) %>%
  mutate(days_missing = diff / 86400 - 1) #here we are looking for those diff > 0 bc if 0 then it is ok and not skip day

#no values larger than this
purchase_spend_aug %>%
  select(date, wday.lbl, diff) %>%
  filter(diff > 172800) %>%
  mutate(days_missing = diff / 86400 - 1)

#This is the days that are skipped in our data. So we can be sure they might be skipped in our forecast.
off_days <- c('2018-01-01', '2018-03-04', '2018-04-26') %>%
  ymd()

#Here we are creating our future values for the days
idx_future <- idx %>%
  tk_make_future_timeseries(n_future = 365, inspect_weekdays = TRUE, skip_values = off_days)
idx_future %>%
  tk_get_timeseries_signature() %>%
  ggplot(aes(x = index, y = diff))+
  geom_point(alpha = .5, aes(color = as.factor(diff)))+
  geom_line(alpha = .5)+
  scale_color_manual(values = palette_light())+
  theme_tq()

#Now we are adding our future values to our prediction data
data_future <- idx_future %>%
  tk_get_timeseries_signature() %>%
  rename(date = index)
pred_future <- predict(fit_lm, newdata = data_future)
pred_future <- data_future %>%
  select(date) %>%
  add_column(total_spend = pred_future)


#We want to be sure to include our standard deviations so that we can go ahead and create the range
#for our standard error when we start to plot our graph.
test_residuals <- pred_test$resid_lm
test_resid_sd <- sd(test_residuals, na.rm = TRUE)

#The values for the future predictions kept coming out negative. (not sure about this?)
#Went ahead and just changed the values to absolute since we are dealing with purchases and we cannot have
#negative purchases.
pred_future$total_spend <- abs(pred_future$total_spend)
pred_future <- pred_future %>%
  mutate(
    lo.95 = (total_spend) - 1.96 * test_resid_sd,
    lo.80 = (total_spend) - 1.28 * test_resid_sd,
    hi.80 = (total_spend) + 1.28 * test_resid_sd,
    hi.95 = (total_spend) + 1.96 * test_resid_sd
      )

#Finally we graph our forecating and we can interpret the data accordingly
purchase_spend %>%
  select(Date, total_spend) %>%
  rename(date = Date) %>%
  ggplot(aes(x = date, y = total_spend))+
  geom_point(alpha = .5)+
  geom_line(alpha = .5)+
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), data = pred_future,
              fill = '#D5DBFF', color = NA, size = 0)+
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), data = pred_future,
              fill = "#596DD5", color = NA, size = 0, alpha =.8)+
  geom_point(aes(x = date, y = (total_spend)), data = pred_future,
             alpha = .5, color = palette_light()[2], na.rm = TRUE)+
  geom_smooth(aes(x = date, y = (total_spend)), data = pred_future,
              method = 'loess', color = 'white', na.rm = TRUE)+
  theme_tq()











