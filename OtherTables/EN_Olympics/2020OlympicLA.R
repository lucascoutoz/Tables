#load libraries 
library(formattable)
library(ggplot2)
library(kableExtra)
library(showtext) 
library(tidyverse)
library(ragg) 


## Original: https://github.com/moriahtaylor1/tidy-tuesday/tree/main/2021_Week31


#Constructing data
Olympics_2020 = 
  tribble(
    ~country,~gold, ~silver, ~bronze, ~total, 
    "Brazil", 7, 6, 8, 21,
    "Cuba", 7, 3, 5, 15,
    "Jamaica", 4, 1, 4, 9,
    "Ecuador", 2, 1, 0, 3,
    "Venezuela", 1, 3, 0, 4,
    "Colombia", 0, 4, 1, 5,
    "Dominican Republic", 0, 3, 2, 5,
    "Argentina", 0, 1, 2, 3,
    "Mexico", 0, 0, 4, 4)

####create functions for each color tile####
gold_tile <- function() {
  formatter("span", 
            style = style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = csscolor("black"),
              "background-color" = "#be9625"
            )
  )
}

silver_tile <- function() {
  formatter("span", 
            style = style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = csscolor("black"),
              "background-color" = "#9F9F9F"
            )
  )
}

bronze_tile <- function() {
  formatter("span", 
            style = style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = csscolor("black"),
              "background-color" = "#9D836A"
            )
  )
}

total_tile <- function() {
  formatter("span", 
            style = function(y) style(
              display = "block",
              padding = "5 5px",
              "border-radius" = "10px",
              "color" = csscolor("black"),
              "background-color" = "#B5C8ED"
            )
  )
}

### the time has come: create kables
AL_summer_kable <- Olympics_2020 %>%
  mutate(COUNTRY = cell_spec(country, "html", color = "black", align = "left", bold = F),
         GOLD = gold_tile()(gold),
         SILVER = silver_tile()(silver),
         BRONZE = bronze_tile()(bronze),
         TOTAL = total_tile()(total)) %>%
  select(COUNTRY, GOLD, SILVER, BRONZE, TOTAL) %>%
  kable(
    "html", escape = F, align=c("lcccc"),
  ) %>%
  kable_minimal(html_font = "Nunito") %>%
  row_spec(0, color = "black") %>%
  add_header_above(c("2020 Olympic Medals by Latin American Countries" = 5), color = "black", font_size = 20) %>%
  column_spec(2:5,width_min='3cm')


AL_summer_kable
