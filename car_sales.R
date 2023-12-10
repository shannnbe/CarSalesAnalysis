# Read CSV
car_data <- read.csv("./car_sales_data.csv", 
                     na.strings = c("", "N/A"),
                     fileEncoding = "UTF-8")
# View
View(car_data)

# Check Values
library("dplyr")
car_data %>%
  count(car_data$Car.Model)
car_data %>%
  count(car_data$Car.Make)

# Check Duplicate & Remove
car_data[!duplicated(car_data),]

# Data Cleaning/Pre-Processing
## Clear All N/A
car_data <- car_data[complete.cases(car_data),]

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

# Pre-processing
df_new <- rbind(civic_clean, altima_clean, F150_clean,
                silverado_clean, corolla_clean)

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
  