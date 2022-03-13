library(plm)
library(dplyr)


grunfeld_data <- read.csv('grunfeld_data.csv')

grunfeld_data <- grunfeld_data %>%
    dplyr::mutate(firm, as.numeric(as.factor(firm))) %>%
    dplyr::select(firm, year, invest, capital, value)


result<- plm(
    invest~value+capital,
    data=grunfeld_data,
    model="within"
)

summary(result)