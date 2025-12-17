retail_data <- read.csv("online_retail.csv")
install.packages("tidyverse")
library(tidyverse)

#take a look at the summary 
glimpse(retail_data)
summary(retail_data)

#creating a new column
retail_clean <- retail_data %>%
  mutate(total_price= Quantity * UnitPrice)
View(retail_clean)
glimpse(retail_data)
View(retail_data)

#trimming data
trimmed_rd <- retail_data %>%
  select(InvoiceNo, Description, Quantity) #chooses the ones interested in
View(trimmed_rd)

#renaming for better understanding
trimmed_rd %>%
  select(InvoiceNo, Description, Quantity) %>%
  rename(Product = Description)

#arranging data for easy read (sorts from ascending order)
arrange(retail_data, Quantity)

#shows arrange in descending order
arrange(retail_data, desc(Quantity))
  
#using max and min functions to sort out without using arrange
max(retail_data$Quantity)
min(retail_data$Quantity)

#finding average use mean
mean(retail_data$Quantity)

#top 10 customers by total spending
retail_clean %>%
  group_by(CustomerID) %>%
  summarise(total_spent = sum(total_price)) %>%
  arrange(desc(total_spent)) %>%
  slice_head(n = 10) # takes only the first n rows from your data

#making revenue
retail_clean %>%
  group_by(Date) %>%
  summarise(DailyRevenue = sum(total_price)) %>%
ggplot(aes(x = Date, y = DailyRevenue)) +
  geom_line(color = "steelblue") +
  labs(title = "Daily Revenue Over Time", x = "Date", y = "Revenue")

#converting date to proper format
retail_clean <- retail_clean %>%
  mutate(InvoiceDate = as_datetime(InvoiceDate),
         Date = as_date(InvoiceDate),
         Hour = hour(InvoiceDate),
         Weekday = wday(InvoiceDate, label = TRUE),
         Month = month(InvoiceDate, label = TRUE))

#filtering out cancellations
retail_clean <- retail_clean %>%
  filter(!str_starts(InvoiceNo, "C"))

View(retail_clean)

#top selling products
retail_clean %>%
  group_by(Description) %>%
  summarise(Total_sold= sum(Quantity)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Description, Total_sold), y = Total_sold)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Top Selling Products", x = "Product", y = "Total_sold")

#revenue by weekday
retail_clean %>%
  group_by(Weekday) %>%
  summarise(Total_sold= sum(Quantity)) %>%
  slice_head(n=10) %>%
  ggplot(aes(x = Weekday, y = Total_sold)) +
  geom_col(fill = "pink") +
  labs(title = "Weekly revenue", x = "Weekday", y = "Total_sold")

#revenue by month, faceted by country
retail_clean %>%
  filter(Country %in% c("United Kingdom", "Germany", "France")) %>%
  group_by(Month, Country) %>%
  summarise(MonthlyRevenue = sum(total_price))%>%
  ggplot (aes(x=Month, y=MonthlyRevenue, fill=Country)) +
  geom_col(show.legend = FALSE)
  facet_wrap(~Country) +
  labs(title = "Monthly Revenue by Countries", x="Month", y="Revenue")
  
  
  


