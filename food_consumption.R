library(data.table)
library(readr)
library(ggplot2)
library(curl)
library(plotly)

library(data.table)
library(readr)
library(ggplot2)
library(curl)
library(plotly)


food_consumption <- data.table(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv'))


food_consumption[, food_category := factor(food_category, levels = c(food_consumption[, sort(unique(food_category))]))]


ggplotly(ggplot(data = food_consumption, aes(x = food_category, y = consumption, color = country)) +
           geom_jitter() +
           theme(legend.position = "none") +
           coord_flip())

sample(16:80, 10)
sample(15:35, 10)
