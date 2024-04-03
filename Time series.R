

# Removing all from environment before beginning.
rm(list=ls()) 

# Installing and lodaing required package.

install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("corrplot")
library("corrplot")

# importing the dataset
shop <- read.csv("C:\\Users\\bishe\\OneDrive\\Desktop\\Personal\\Softwerica\\Stat\\customer_data.csv")

# converting character dates to numeric dates. and creating agegrp.

shop_data <- shop %>% 
  mutate(date = as.Date(invoice_date, format = "%d/%m/%Y")) %>% 
  arrange(date) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  mutate(agegrp = case_when(
    age >= 18 & age <= 25 ~ '18 to 25',
    age >= 26 & age <= 43 ~ '26 to 43',
    age >=44 & age <= 56 ~ '44 to 56',
    age > 56 ~ '56+'))
#------------------------------------------------------------------------------------------------------
### Making data ready for different categories and subcategories
#------------------------------------------------------------------------------------------------------

######Total quantity time series data.

### time series
data_ts <- shop_data %>% 
  group_by(date) %>% 
  summarise(quan = sum(quantity),
            price = sum(price)) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))

### Gender group
data_sex <- shop_data %>% 
  group_by(date, gender) %>% 
  summarise(total = sum(quantity)) %>% 
  mutate(sexn = ifelse(toupper(gender) =="MALE", 1, 2))

### Category
data_cat <- shop_data %>% 
  group_by(date, category) %>% 
  summarise(total = sum(quantity)) %>% 
  mutate( categoryn = case_when(
    toupper(category) == "BOOKS" ~ 1,
    toupper(category) == "CLOTHING" ~ 2,
    toupper(category) == "COSMETICS" ~ 3,
    toupper(category) == "FOOD & BEVERAGE" ~ 4,
    toupper(category) == "SHOES" ~ 5,
    toupper(category) == "SOUVENIR" ~ 6,
    toupper(category) == "TECHNOLOGY" ~ 7,
    toupper(category) == "TOYS" ~ 8,
  ))


### age group
data_age <- shop_data %>% 
  group_by(date, agegrp) %>% 
  summarise(total = sum(quantity))

### payment method group
data_pay <- shop_data %>% 
  group_by(date, payment_method) %>% 
  summarise(total = sum(quantity)) %>% 
  mutate(paymentn = case_when(
    toupper(payment_method) == "CASH" ~ 1,
    toupper(payment_method) == "DEBIT CARD" ~ 2,
    toupper(payment_method) == "CREDIT CARD" ~ 3))


############################################# Plotting time series graph

############################################ Plots

################### Plots for total quantity

# Whole plot
ggplot(data_ts, aes(x = date, y = quan, color = month)) + 
  geom_line() + 
  labs(title = "Time series plot", x = "Date", y = "Total Quantity per day")

##### Plot year wise
ggplot(data_ts, aes(x = date, y = quan, color = month)) + 
  geom_line() + 
  labs(title = "Time series plot by year", x = "Date", y = "Total Quantity per day") +
  facet_wrap(~ year,scales = "free_x", nrow = 3)

#### Plot Gender wise

ggplot(data_sex, aes(x = date, y = total, color = gender)) + 
  geom_line() + 
  labs(title = "Time series plot by gender", x = "Date", y = "Total Quantity") +
  facet_wrap(~ gender,scales = "free_x", nrow = 2)

##### Plot category wise
ggplot(data_cat, aes(x = date, y = total, color = category)) + 
  geom_line() + 
  labs(title = "Time series plot by category", x = "Date", y = "Total Quantity") +
  facet_wrap(~ category,scales = "free_x", nrow = 4)

##### Plot age group wise
ggplot(data_age, aes(x = date, y = total, color = agegrp)) + 
  geom_line() + 
  labs(title = "Time series plot by age group", x = "Date", y = "Total Quantity") +
  facet_wrap(~ agegrp,scales = "free_x", nrow = 4)

##### Plot payment method group wise
ggplot(data_pay, aes(x = date, y = total, color = payment_method)) + 
  geom_line() + 
  labs(title = "Time series plot by payment method", x = "Date", y = "Total Quantity") +
  facet_wrap(~ payment_method,scales = "free_x", nrow = 4)


#-------------------------------------------------------------------------------------------------#
# Distribution Plots
#-------------------------------------------------------------------------------------------------#

################################## Plots for Quantity.

### density plot with distribution curve.

data_ts %>%
  ggplot( aes(x=quan)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(data_ts$quan), 
                                         sd = sd(data_ts$quan)), 
                color = "#6B5B95", linewidth = 1) + 
  labs(title = "density plot with normal distribution curve", x = "Quantity", y = "density")


### Histogram with distribution curve.

  ggplot(data_ts, aes(x=quan)) +
  geom_histogram(aes(y = ..density..),fill="#69b3a2", color="#e9ecef", alpha=0.8, bins = 30) +
  stat_function(fun = dnorm, args = list(mean = mean(data_ts$quan), 
                                         sd = sd(data_ts$quan)), 
                color = "#6B5B95", linewidth = 1,
                xlim = range(data_ts$quan)) + 
    labs(title = "Histogram with normal distribution curve", x = "Quantity", y = "density")

  
  data_ts %>%
    ggplot( aes(x=quan, y = price)) +
    geom_point()
    labs(title = "density plot with normal distribution curve", x = "Quantity", y = "density")  
  
#### Histogram by sex group

data_sex %>%
  ggplot( aes(x=total, fill=gender)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#FF6F61", "#6B5B95"))+
  labs(fill="Histogram by gender", x = "Total Quantity",y = "Total Count")

#### quantity by category group: histogram subplots

data_cat %>%
  ggplot( aes(x=total, fill=category)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  facet_wrap(~ category, ncol = 2)+
  labs(fill="", x = "Total Quantity","Total Count")


#### quantity by age: Density Plot

ggplot(data=data_age, aes(x=total, group=agegrp, fill=agegrp)) +
  geom_density(adjust=1.5, alpha=.4)

#### quantity by pay: Density Plot

ggplot(data=data_pay, aes(x=total, group=payment_method, fill=payment_method)) +
  geom_density(adjust=1.5, alpha=.4)

## --------------------------------------------------------------------------------------------- ##
# Correlation curve
## ---------------------------------------------------------------------------------------------- ##

# summarise to obtain sum of quantity by date and age.

data_agegrp <- shop_data %>% 
  group_by(date,age) %>% 
  summarise(quantity = sum(quantity))

########### Correlation or scatter plots


ggplot(data_ts, aes(x=price, y=quan)) + 
  geom_point(size=1) +
labs(title ="Scatter Plot", x = "Price",y = "Quantity")

ggplot(data_agegrp, aes(x=age, y=quantity)) + 
  geom_point(size=1)+
  labs(title ="Scatter Plot", x = "Age",y = "Quantity")

ggplot(data_sex, aes(x=sexn, y=total, color = gender)) + 
  geom_point(size=1) +
  labs(title ="Scatter Plot", x = "Gender",y = "Quantity")

ggplot(data_cat, aes(x=categoryn, y=total, color = category)) + 
  geom_point(size=1)+
  labs(title ="Scatter Plot", x = "Category",y = "Quantity")

ggplot(data_pay, aes(x=paymentn, y=total, color = payment_method)) + 
  geom_point(size=1)+
  labs(title ="Scatter Plot", x = "Payment",y = "Quantity")

