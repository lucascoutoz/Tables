##Vondrousova's Performance in 2021
library(gt)
library(gtExtras)
library(tidyverse)

## My Version 
Marketa = 
  tribble(
    ~Tournament, ~Country, ~Series, ~Surface, ~Round, ~PointsGained,
    "Abu Dhabi", "United Arab Emirates", "WTA500", "Hard", "R64", 1,
    "Melbourne", "Australia", "WTA500","Hard", "SF", 185,
    "Australian Open", "Australia", "Slam", "Hard", "R16", 240,
    "Dubai", "United Arab Emirates", "WTA1000", "Hard", "R32", 60,
    "Miami", "USA", "WTA1000", "Hard", "R16", 120,
    "Stuttgart", "Germany", "WTA500", "Clay", "R16", 55,
    "Madrid", "Spain", "WTA1000", "Clay", "R64", 10,
    "Rome", "Italy", "WTA1000", "Clay", "R64", 1,
    "Roland Garros", "France", "Slam", "Clay", "R16", 240,
    "Berlin", "Germany","WTA500", "Grass", "R32", 1,
    "Eastbourne", "UK", "WTA500", "Grass", "R32", 1,
    "Wimbledon", "UK", "Slam", "Grass", "R64", 70,
    "Olympics", "Japan", "Special Event", "Hard", "Runner-Up", 0,
    "Cincinnati", "USA", "WTA1000", "Hard", "R64", 1,
    "Chicago", "USA", "WTA250", "Hard", "QF", 60,
    "US Open", "USA", "Slam", "Hard", "R64", 70,
    "Luxembourg", "Luxembourg", "WTA250", "Hard", "SF", 110,
    "Chicago", "USA", "WTA500", "Hard", "SF", 185,
    "Indian Wells", "USA", "WTA1000", "Hard", "R128", 10,
    "Moscow", "Russia", "WTA500", "Hard", "SF", 185)
    
Marketa <- Marketa %>%
  mutate(Country = case_when(
    str_detect(Country,'United Arab Emirates') ~ 'https://cdn.countryflags.com/thumbs/united-arab-emirates/flag-round-250.png',
    str_detect(Country,'Australia') ~ 'https://cdn.countryflags.com/thumbs/australia/flag-round-250.png',
    str_detect(Country,'USA') ~ 'https://cdn.countryflags.com/thumbs/united-states-of-america/flag-round-250.png',
    str_detect(Country,'Germany') ~ 'https://cdn.countryflags.com/thumbs/germany/flag-round-250.png',
    str_detect(Country,'Spain') ~ 'https://cdn.countryflags.com/thumbs/spain/flag-round-250.png',
    str_detect(Country,'Italy') ~ 'https://cdn.countryflags.com/thumbs/italy/flag-round-250.png',
    str_detect(Country,'France') ~ 'https://cdn.countryflags.com/thumbs/france/flag-round-250.png',
    str_detect(Country,'UK') ~ 'https://cdn.countryflags.com/thumbs/united-kingdom/flag-round-250.png',
    str_detect(Country,'Japan') ~ 'https://cdn.countryflags.com/thumbs/japan/flag-round-250.png',
    str_detect(Country,'Luxembourg') ~ 'https://cdn.countryflags.com/thumbs/luxembourg/flag-round-250.png',
    str_detect(Country,'Russia') ~ 'https://cdn.countryflags.com/thumbs/russia/flag-round-250.png'))

Marketa%>%
  mutate(Series = factor(Series,levels=c("Special Event", "Slam", "WTA1000", "WTA500", "WTA250"))) %>%
  arrange(Series) %>%
  arrange(-PointsGained) %>%
  mutate(bar = PointsGained) %>%
  mutate(bar = round(bar/1255, 3)*100) %>%
  gt()%>%
  gt_theme_538() %>%
  gt_img_rows(columns = Country, height = 20) %>%
  gt_plt_bar_pct(column = bar, scaled = T, fill = "blue") %>%
  cols_width(7 ~ px(125)) %>%
  tab_style(style = list(cell_text(align="center"), 
            table.align="center",
            heading.align = "center"), 
            locations=list(cells_body(columns = everything(),rows = everything())))%>%
  cols_align(align = "center", columns = everything()) %>%
  tab_source_note(md("@lucas_coutoz"))%>%
  tab_style(
    style = cell_text(
      align = "right"
    ), 
    locations = cells_source_notes()) %>%
  tab_header(
    title = "2021 Marketa Vondrousova Season"
  ) %>%
  tab_footnote(
    footnote = "This table doesn't include further international competitions where Vondrousova represented the Czech Republic, such as the Billie Jean King Cup.",
    locations = cells_title(groups = "title")
  ) %>%
  tab_footnote(
    footnote = "Bar underlines the share of each tournament in the total points Vondrosouva gained in 2021.",
    locations = cells_column_labels(
      columns = bar)
  )
    
 