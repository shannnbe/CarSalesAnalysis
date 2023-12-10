# CarSalesAnalysis
Car Sales Dataset Analysis using R with the main purpose of predicting second car prices in the future. 

This team project is to complete the Assurance of Learning of Data Mining subject, lectured by Mr. Ivan Halim Parmonangan, S.Kom., M.Eng.

The team consists of:
- Jonathan Lim as Project Manager
- Chelsea Marchelle as Data Analyst
- Galuh Bentang R. as Data Analyst
- Gloria Shanti (myself) as the Data Analyst

The dataset is taken from [Kaggle](https://www.kaggle.com/datasets/suraj520/car-sales-data)

For the full report, you can check the [Google Docs](https://docs.google.com/document/d/1PFkdOx2aVbXB_JnZhcopBOyd1oU0qz0bFjvxFczRhiM/edit) here. 

## Documentation.
Here are some basic and summarized ver. of the lines of the R script.

### Problem found in the Dataset
The dataset was clean and easy to analyze. However, we noticed that the dataset has anomalies such as the mismatching names of Car Models and Car Makers. For example, Corolla was made by Toyota, but some records recorded Corolla was made by Nissan.

### Values Available
We checked how many values or Car Names existed in this dataset, we also used `dplyr` library here.
```
# Check Values
library("dplyr")
car_data %>%
  count(car_data$Car.Model)
car_data %>%
  count(car_data$Car.Make)
```

### Data Cleaning
We checked if there were duplicated rows and removed them if they existed by using this script.
```
# Check duplicates and remove them (if exist)
car_data[!duplicated(car_data),]
```
After that, we clear all NULL rows in the dataset
```
## Clear All N/A
car_data <- car_data[complete.cases(car_data),]
```
By the values we have checked before and the problem we found before, we filtered out every Car with the wrong Car Maker and changed it. The script are not effective yet, we can use functions to minimize the use of loc.
```
## Replace Anomaly Data || Civic
civic_clean <- subset(car_data,
                car_data$Car.Model == "Civic"&
                car_data$Car.Make != "Honda")
civic_clean$Car.Make[civic_clean$Car.Make != "Honda" ] <- "Honda"

## Replace Anomaly Data || Altima
altima_clean <- subset(car_data,
                      car_data$Car.Model == "Altima"&
                      car_data$Car.Make != "Nissan")
altima_clean$Car.Make[altima_clean$Car.Make != "Altima" ] <- "Nissan"

## Replace Anomaly Data || F-150
F150_clean <- subset(car_data,
                      car_data$Car.Model == "F-150"&
                      car_data$Car.Make != "Ford", select = )
F150_clean$Car.Make[F150_clean$Car.Make != "Ford" ] <- "Ford"


## Replace Anomaly Data || Silverado
silverado_clean <- subset(car_data,
                     car_data$Car.Model == "Silverado"&
                     car_data$Car.Make != "Chevrolet")
silverado_clean$Car.Make[silverado_clean$Car.Make != "Chevrolet"] <- "Chevrolet"

## Replace Anomaly Data || Silverado
corolla_clean <- subset(car_data,
                        car_data$Car.Model == "Corolla"&
                        car_data$Car.Make != "Toyota")
corolla_clean$Car.Make[corolla_clean$Car.Make != "Toyota"] <- "Toyota"
```
After that, we joined them into one dataset again.
```
df_new <- rbind(civic_clean, altima_clean, F150_clean,
                silverado_clean, corolla_clean)
```

### Data Analysis
Because the data is pretty much ready now, we want to make a pie chart that shows sales of each year and total of 2022 & 2023, of top 3 cars.
```
# Data Analysis
percentage <- function(count, total_count) {
  percentage_count <- round(count/total_count *100,3)
}

top3 <- function(percent) {
  head(sort(percent, decreasing = TRUE), 3)
}

## 1. Top 3 Car with most sales in 2022 & 2023
count_all <- table(df_new$Car.Model)
total_all <- sum(count_all)
percentage_all <- percentage(count_all, total_all)
top3_all <- top3(percentage_all)

pieall <- pie(top3_all,
    main = "Top 3 Sales Car for 2022 & 2023",
    labels = paste0(names(top3_all), 
             sep ="\n", top3_all, "%"))

## 1.2 Top 3 Car with most sales in 2023
filter_2023 <- subset(df_new,
                      grepl("2023", df_new$Date))
count_2023 <- table(filter_2023$Car.Model)
total_2023 <- (sum(count_2023))
percentage_2023 <- percentage(count_2023, total_2023)
top3_2023 <- top3(percentage_2023)

pie2023 <- pie(top3_2023,
    main = "Top 3 Sales Car for 2023",
    labels = paste0(names(top3_2023), 
                    sep ="\n", top3_2023, "%"))

## 1.3 Top 3 Car with most sales in 2022
filter_2022 <- subset(df_new,
                      grepl("2022", df_new$Date))
count_2022 <- table(filter_2022$Car.Model)
total_2022 <- (sum(count_2022))
percentage_2022 <- percentage(count_2022, total_2022)
top3_2022 <- top3(percentage_2022)

pie2022 <- pie(top3_2022,
               main = "Top 3 Sales Car for 2022",
               labels = paste0(names(top3_2022), 
                               sep ="\n", top3_2022, "%"))

## Comparison with Bar Chart
install.packages("ggplot2")
library(ggplot2)


df_all <- data.frame(top3(count_all))
df_2023 <- data.frame(top3(count_2023))
df_2022 <- data.frame(top3(count_2022))
join <- rbind.data.frame(df_all, df_2023, df_2022)

gfg <- data.frame(join$Freq,
                  grp = rep(c("All", "2023", "2022"),
                      each = 3),
                  subgroup = join$Var1)

options(scipen = 100, digits = 4)

ggplot(gfg, aes(x=grp, y=join$Freq, fill=subgroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c('light blue', "steelblue2", "light green", "red")) +
  labs(title = "Car Sales Comparison for Each Year",
       colour="steelblue2",
       x = "Year",
       y = "Car Sold",
       fill = "Car Name")+
  ylim(0, 450000)
```

The rest of the script are still ongoing work.
