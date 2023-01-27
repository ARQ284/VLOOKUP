library(tidyverse)
library(lubridate)
library(openxlsx)

Employee = data.frame(
EmployeeID = paste0("E",sample(1:500))) %>% 
mutate(EmployeeFisrtName = randomNames::randomNames(nrow(.),
                           which.names = "first",
                           ethnicity = c(5,3)),
       EmployeeLastName = randomNames::randomNames(nrow(.),
                           which.names = "last",
                           ethnicity = c(5,3)),
         Dept = sample(c('Finance','Sales','Operations','Legal','IT'),
                       nrow(.),
                       prob=c(0.25, 0.30, 0.17, 0.21,0.08),
                       replace=TRUE),
         Region = sample(c('Hudson Valley','Capital District','Southern Tier','Western'),
                       nrow(.),
                       prob=c(0.35,0.40,0.20,0.10),
                       replace=TRUE),
         Salary = round(rnorm(nrow(.), 
                              mean = (150000 - 10000)/2, 
                              sd = 9000)),
       HireDate = sample(seq(as.Date('2007-01-01'),
                             today(), by="day"),
                             nrow(.),
                             replace = TRUE)
       )

###Sales
Sales = data.frame(
EmployeeID = sample(Employee[Employee$Dept == 'Sales',]$EmployeeID,
                      5000,
                      replace = T)) %>% 
mutate(
  SaleRevenue = abs(round(rnorm(nrow(.), 
                mean = 5000, 
                sd = 1000))),
  CustomerType = sample(c('Individual','Professional','Government'),
                 nrow(.),
                 prob=c(0.55,0.30,0.15),
                 replace=TRUE)) %>% 
  mutate(Commission = SaleRevenue * 0.02)

VL = createWorkbook()
addWorksheet(VL, "Employee")
addWorksheet(VL, "Sales")
writeData(VL, "Employee",Employee)
writeData(VL, "Sales",Sales)
saveWorkbook(VL,"VLOOKUPpractice.xlsx")

###plot distributions
ggplot(Employee) +
  aes(x = Region) +
  geom_bar(fill = "#255F65") +
  theme_classic()

ggplot(Sales) +
  aes(x = CustomerType) +
  geom_bar(fill = "#255F65") +
  theme_classic()

ggplot(Sales) +
  aes(x = SaleRevenue) +
  geom_histogram(bins = 40L, fill = "#255F65") +
  labs(x = "Sale Revenue", y = "Frequency") +
  theme_classic()
