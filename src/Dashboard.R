# Northwind Dashboard

# Pakker
install.packages("flexdashboard")
install.packages("readxl")
install.packages("dplyr")
install.packages("RCurl")
install.packages("ggplot2")

library(flexdashboard)
library(readxl)
library(dplyr)
library(RCurl)
library(ggplot2)

# Hent data fra Github
customers <- getURL("https://raw.githubusercontent.com/officegeek/data/master/customers.csv")
customers <- read.csv2(text = customers, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
orders <- getURL("https://raw.githubusercontent.com/officegeek/data/master/orders.csv")
orders <- read.csv2(text = orders, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
categories <- getURL("https://raw.githubusercontent.com/officegeek/data/master/categories.csv")
categories <- read.csv2(text = categories, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
order_details <-  getURL("https://raw.githubusercontent.com/officegeek/data/master/order_details.csv")
order_details <- read.csv2(text = order_details, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
products <- getURL("https://raw.githubusercontent.com/officegeek/data/master/products.csv")
products <- read.csv2(text = products, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
employess <- getURL("https://raw.githubusercontent.com/officegeek/data/master/employess.csv")
employess <- read.csv2(text = employess, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)

# DPLYR - Data

# MUTATE
order_details <- order_details %>%
  mutate(Total = UnitPrice * Quantity) 


# Salesdata
Salesdata <- customers %>%
  left_join(orders, by = c("CustomerID" = "CustomerID")) %>% 
  left_join(order_details, by = c("OrderID" = "OrderID")) %>% 
  left_join(products, by = c("ProductID" = "ProductID")) %>% 
  left_join(categories, by = c("CategoryID" = "CategoryID")) %>% 
  select(CompanyName, Country, OrderDate, ProductName, EmployeeID, CategoryName, Total)

# Salesdata Employess
SalesdataEmployess <- Salesdata %>% 
  left_join(employess, by = c("EmployeeID" = "EmployeeID")) %>% 
  select(OrderDate, LastName, Total)

" GGPLO2"
g <- group_by(data, ProductName) %>%
  summarize(qty = sum(Quantity)) %>%
  arrange(desc(qty)) %>%
  head(n = 5) %>%
  ggplot(data = ., aes(x = reorder(ProductName, qty), y = qty)) +
  geom_bar(stat = "identity", fill = "#fc9272", width = 0.5) +
  coord_flip()

g + theme(
  axis.text.y = element_text(size = rel(1), hjust = 0),
  axis.text.x = element_text(size = rel(1), face = "bold"),
  panel.background = element_rect(fill = "grey98"),
  plot.background = element_rect(fill = "grey98"),
  axis.title = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.x = element_line(color = "gray90"))


# Data top 5 products
SalesdataProduct <- group_by(Salesdata, ProductName) %>% 
  summarise(Total = sum(Total)) %>% 
  arrange(desc(Total)) %>% 
  head(n = 5)

num_keys <- c("Total")
SalesdataProduct <- colformat_num(x = SalesdataProduct, j = num_keys, big.mark = ".", decimal.mark = ",", digits = 0, na_str = "missing")

# Flextable
SalesdataProductTable <- flextable(
  head(SalesdataProduct),
  col_keys = c("ProductsName", "Total"))

SalesdataProductTable

# Ggplot2
ggplot(SalesdataProduct, aes(x = reorder(ProductName, Total), y = Total)) +
  geom_bar(stat="identity", fill = "lightblue", width = 0.5) +
  coord_flip() +
  xlab("Products") +
  ylab("Total sales") +
  theme_minimal()

  


