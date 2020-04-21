#====================================================#
#====================================================#

install.packages("data.table")
install.packages("ggplot2")
install.packages("tidytext")
install.packages("stringr")
install.packages("schrute")
install.packages("readr")

library(data.table)
library(ggplot2)
library(tidytext)
library(stringr)
library(readr)
library(schrute)


#==========================================================#
#==========================================================#

  office_ratings <- data.table(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv'))


  # extract data from package
  in_text <- data.table(schrute::theoffice) 
  
  
  # add a flag for who asks a question
  in_text[, flag_question := ifelse(grepl("\\?", text), 1, 0)]
  
  # flag for negative words
  # in_text[, flag_negative := ifelse(grepl(" no | not | never ", text), 1, 0)]
  
  in_text[, season := as.character(season)]
  
  
  # who asks the most qestiosn
  char_quests <- in_text[, .(n_questions = sum(flag_question),
                             n_lines     = .N), 
                         by = c("character", "season")][n_lines > 10]
  
  char_quests[, n_seasons := uniqueN(season), by = "character"]
  
  char_quests[, pct_questions := 100 * n_questions / n_lines]
  
  sub_char_quests <- subset(char_quests, n_seasons > 3)
  
  ggplot(sub_char_quests, aes(x = character, y = pct_questions, color = season)) + 
    geom_point(size = 3) + 
    coord_flip() + 
    theme(legend.position = "bottom")
  
  in_text[, air_date := as.Date(air_date)]
  
  ggplot(office_ratings, aes(x = imdb_rating, y = air_date, color = as.character(season))) + 
    geom_point(aes(size = total_votes)) + 
    theme_minimal()
  
  office_ratings[, air_month := month(air_date)]
  
  ggplot(office_ratings, aes(x = air_month, y = imdb_rating, color = as.character(season))) + 
    geom_point(aes(size = total_votes)) + 
    theme_minimal()  
  
  