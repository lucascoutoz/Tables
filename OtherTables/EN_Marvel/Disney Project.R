library(gt)
library(lubridate)
library(tidyverse)
library(showtext)

disney = 
  tribble(
    ~company, ~serie, ~name, ~season, ~episode, ~first_aired, ~rating,
    "Marvel", "The Falcon and the Winter Soldier", "New World Order", 1, 1, "19-03-2021", 7.7,
    "Marvel", "The Falcon and the Winter Soldier", "The Star-Spangled Man", 1, 2, "26-03-2021", 7.8,
    "Marvel", "The Falcon and the Winter Soldier", "Power Broker", 1, 3, "02-04-2021", 8.0,
    "Marvel", "The Falcon and the Winter Soldier", "                The Whole World Is Watching", 1, 4, "09-04-2021", 8.8,
    "Marvel", "The Falcon and the Winter Soldier", "Truth", 1, 5, "16-04-2021", 8.0,
    "Marvel", "The Falcon and the Winter Soldier", "One World, One People", 1, 6, "23-04-2021", 7.8,
    "Marvel", "WandaVision", "Filmed Before a Live Studio Audience", 1, 1, "15-01-2021", 7.5,
    "Marvel", "WandaVision", "Don't Touch That Dial", 1, 2, "16-01-2021", 7.8,
    "Marvel", "WandaVision", "Now in Color", 1, 3, "22-01-2021", 8.1,
    "Marvel", "WandaVision", "We Interrupt This Program", 1, 4, "29-01-2021", 8.8,
    "Marvel", "WandaVision", "On a Very Special Episode...", 1, 5, "05-02-2021", 9.0,
    "Marvel", "WandaVision", "All-New Halloween Spooktacular!", 1, 6, "12-02-2021", 8.8,
    "Marvel", "WandaVision", "Breaking the Fourth Wall", 1, 7, "19-02-2021", 8.4,
    "Marvel", "WandaVision", "Previously On", 1, 8, "26-02-2021", 9.0,
    "Marvel", "WandaVision", "The Series Finale", 1, 9, "05-03-2021", 8.4,
    "Marvel", "Loki", "                                        Glorious Purpose", 1, 1, "09-06-2021", 8.7,
    "Marvel", "Loki", "The Variant", 1, 2, "16-06-2021", 8.9,
    "Marvel", "Loki", "Lamentis", 1, 3, "23-06-2021", 7.9,
    "Marvel", "Loki", "The Nexus Event", 1, 4, "30-06-2021", 9.2,
    "Marvel", "Loki", "Journey Into Mystery", 1, 5, "07-07-2021", 9.0,
    "Marvel", "Loki", "For All Time. Always.", 1, 6, "14-07-2021", 8.8,
    "Marvel", "Hawkeye", "                           Never Meet Your Heroes", 1, 1, "24-11-2021", 7.7,
    "Marvel", "Hawkeye", "Hide and Seek", 1, 2, "24-11-2021", 7.8,
    "Marvel", "Hawkeye", "Echoes", 1, 3, "01-12-2021", 8.5,
    "Marvel", "Hawkeye", "Partners, Am I Right?", 1, 4, "08-12-2021", 8.2,
    "Marvel", "Hawkeye", "Ronin", 1, 5, "15-12-2021", 8.8,
    "Marvel", "Hawkeye", "So This Is Christmas?", 1, 6, "22-12-2021", 8.1,
    "Star Wars", "The Mandalorian", "Chapter 1: The Mandalorian", 1, 1, "12-11-2019", 8.7,
    "Star Wars", "The Mandalorian", "Chapter 2: The Child", 1, 2, "15-11-2019", 8.6,
    "Star Wars", "The Mandalorian", "Chapter 3: The Sin", 1, 3, "22-11-2019", 9.0,
    "Star Wars", "The Mandalorian", "Chapter 4: Sanctuary", 1, 4, "29-11-2019", 7.8,
    "Star Wars", "The Mandalorian", "Chapter 5: The Gunslinger", 1, 5, "06-12-2019", 7.6,
    "Star Wars", "The Mandalorian", "Chapter 6: The Prisoner", 1, 6, "13-12-2019", 8.3,
    "Star Wars", "The Mandalorian", "Chapter 7: The Reckoning", 1, 7, "18-12-2019", 9.1,
    "Star Wars", "The Mandalorian", "Chapter 8: Redemption", 1, 8, "27-12-2019", 9.3,
    "Star Wars", "The Mandalorian", "Chapter 9: The Marshal", 2, 1, "30-10-2020", 8.9,
    "Star Wars", "The Mandalorian", "Chapter 10: The Passenger", 2, 2, "06-11-2020", 7.9,
    "Star Wars", "The Mandalorian", "Chapter 11: The Heiress", 2, 3, "13-11-2020", 8.8,
    "Star Wars", "The Mandalorian", "Chapter 12: The Siege", 2, 4, "20-11-2020", 8.4,
    "Star Wars", "The Mandalorian", "Chapter 13: The Jedi", 2, 5, "27-11-2020", 9.4,
    "Star Wars", "The Mandalorian", "Chapter 14: The Tragedy", 2, 6, "04-12-2020", 9.2,
    "Star Wars", "The Mandalorian", "Chapter 15: The Believer", 2, 7, "11-12-2020", 9.0,
    "Star Wars", "The Mandalorian", "Chapter 16: The Rescue", 2, 8, "18-12-2020", 9.8)
    
data <- disney %>%
  mutate(first_aired = dmy(first_aired)) %>%
  mutate(episode_title = fct_reorder(name, episode)) %>%
  mutate(episode_title = fct_rev(episode_title))

#despues
disney1%>%
  mutate(greatest_rating = fct_reorder(name, rating)) %>%
  filter(company == "Star Wars") %>%
  mutate(season = as.character(season)) %>%
  ggplot(aes(greatest_rating, rating, fill = season)) +
  geom_col() +
  coord_flip() 

##GT -----
Marvel <- tibble(
  Series = c("WandaVision", "The Falcon and the Winter Soldier", "Loki", "Hawkeye"),
  Description = c("A blend of classic television and the Marvel Cinematic Universe
                  in which Wanda Maximoff and Vision - two super-powered beings living
                  idealized suburban lives - begin to suspect that everything is not as it seems.",
                  
                  "Falcon and The Winter Soldier team up on a global adventure that thests
                  their abilities - and their patience.",
                  
                  "The mercurial villain Loki resumes his role as the God of Mischief.",
                  
                  "Former Avenger, Clint Barton, a.k.a. Hawkeye, has a seemingly simple mission:
                  get back to his family for Christmas. But when a threat from his past shows up,
                  Hawkeye reluctantly teams up with Kate Bishop, a 22-year-old skilled archer
                  and his biggest fan, to unravel a criminal conspiracy."),
  Episodes = c(9, 6, 6, 6)
)

Marvel <- Marvel %>%
          mutate(URL = case_when(Series == "WandaVision" ~ "https://img.elo7.com.br/product/original/3816CC2/big-poster-serie-wandavision-lo001-cartaz.jpg",
                                 Series == "The Falcon and the Winter Soldier" ~ "https://m.media-amazon.com/images/M/MV5BODNiODVmYjItM2MyMC00ZWQyLTgyMGYtNzJjMmVmZTY2OTJjXkEyXkFqcGdeQXVyNzk3NDUzNTc@._V1_.jpg",
                                 Series == "Loki" ~ "https://disneyplusbrasil.com.br/wp-content/uploads/2021/05/Loki-Novo-Poster-1-691x1024.jpg",
                                 Series == "Hawkeye" ~ "https://m.media-amazon.com/images/M/MV5BZGRjYjNmYmQtZTI4NS00ZGQwLTg1YzQtMzJkOWJmYTNkODJmXkEyXkFqcGdeQXVyNTA3MTU2MjE@._V1_FMjpg_UX1000_.jpg"))

link_to_img <- function(x, width = 220) {
  glue::glue("<img src='{x}' width='{width}'/>")
}   

Marvel <- Marvel %>% 
          mutate(URL = link_to_img(URL)) %>%
          mutate(picture = purrr::map(URL, gt::html))

out2 <- Marvel %>%
        select(picture, Series, Description, Episodes) %>%
        gt()

##Text layout
format_text <- function(Series, Description){
  glue::glue(
    "<div style='line-height:24px; text-align:left'>
      <span style='font-weight:bold; font-size:30px'>{Series} </span>
    </div>
      <div style='line-height:20px; text-align:left'>
        <span style ='color:#2f2f2f; font-size:24px'>{Description}</span>
    </div>"
  )
}

Marvel1 <- Marvel %>%
           mutate(txt_glue = format_text(Series, Description),
                  txt_html = purrr::map(txt_glue, gt::html))

out3 <- Marvel1 %>%
        select(picture, txt_html, Episodes) %>%
        gt()

out3


##Graph

##Adding Fonts
font_add(family = "regular", "Barlow Semi Condensed-Regular.ttf")
font_add(family= "bold", "Barlow Semi Condensed-Bold.ttf")
showtext_auto()    



data %>%
  filter(serie == "WandaVision") %>%
  ggplot(aes(episode_title, rating, fill = "episode")) +
  geom_bar(stat = "identity", width = 0.9)+
  geom_text(aes(label=rating), position=position_dodge(width=0.9), hjust=-0.50, family = "bold")+
  scale_fill_manual(values = "#b31025") +
  scale_y_continuous(breaks = c(0, 2.5, 5.0, 7.5, 10.0),
                     limits = c(0,10)) +
  labs(x="", y="")+
  coord_flip(expand=FALSE) +
  theme(
    panel.border=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size=13, family="regular", color="black"),
    axis.text.y = element_text(size=18, family="regular", color="black"),
    axis.line = element_blank(),
    legend.position = "none")
    

data %>%
  filter(serie == "WandaVision") %>%
  ggplot(aes(episode, rating, group = "serie")) +
  geom_line(size = 1.5, color = "#b31025") +
  #scale_fill_manual(values = "#b31025") +
  scale_y_continuous(breaks = c(0, 2.5, 5.0, 7.5, 10.0),
                     limits = c(0,10)) +
  scale_x_continuous(breaks = 1:9,
                    limits = c(1,9))+
  labs(x="", y="")+
  coord_cartesian(expand=FALSE) +
  theme(
    panel.border=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none")




##WandaVision
fun_plot1 <- function(data){
  trend <- ggplot(data, aes(episode_title, rating, fill = "episode")) +
    geom_bar(stat = "identity", width = 0.9)+
    geom_text(aes(label=rating), position=position_dodge(width=0.9), hjust=-0.50, size = 9, family = "bold")+
    scale_fill_manual(values = "#b31025") +
    scale_y_continuous(breaks = c(0, 2.5, 5.0, 7.5, 12.0),
                       limits = c(0,10)) +
    labs(x="", y="")+
    coord_flip(expand=FALSE) +
    theme(
      panel.border=element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=30, family="regular", color="black"),
      axis.line = element_blank(),
      legend.position = "none")
  return(trend)
}


Base_Wanda <- data %>%
              group_by(serie) %>%
              nest() %>%
              mutate(gg = purrr::map(data,fun_plot1)) %>%
              select(Series=serie,gg) %>%
              filter(Series == "WandaVision")


##falcon
fun_plot2 <- function(data){
    trend <- ggplot(data, aes(episode_title, rating, fill = "episode")) +
      geom_bar(stat = "identity", width = 0.9)+
      geom_text(aes(label=rating), position=position_dodge(width=0.9), hjust=-0.50, size = 9, family = "bold")+
      scale_fill_manual(values = "#485b69") +
      scale_y_continuous(breaks = c(0, 2.5, 5.0, 7.5, 12.0),
                         limits = c(0,10)) +
      labs(x="", y="")+
      coord_flip(expand=FALSE) +
      theme(
        panel.border=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=30, family="regular", color="black"),
        axis.line = element_blank(),
        legend.position = "none")
    return(trend)
}

Base_Falcon <- data %>%
               group_by(serie) %>%
               nest() %>%
               mutate(gg = purrr::map(data,fun_plot2)) %>%
               select(Series=serie,gg) %>%
               filter(Series == "The Falcon and the Winter Soldier")



#loki
fun_plot3 <- function(data){
    trend <- ggplot(data, aes(episode_title, rating, fill = "episode")) +
      geom_bar(stat = "identity", width = 0.9)+
      geom_text(aes(label=rating), position=position_dodge(width=0.5), hjust=-0.50, size = 9, family = "bold")+
      scale_fill_manual(values = "#d2a56c") +
      scale_y_continuous(breaks = c(0, 2.5, 5.0, 7.5, 12.0),
                         limits = c(0,10)) +
      labs(x="", y="")+
      coord_flip(expand=FALSE) +
      theme(
        panel.border=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=30, family="regular", color="black"),
        axis.line = element_blank(),
        legend.position = "none")
    return(trend)
  }

Base_Loki <- data %>%
             group_by(serie) %>%
             nest() %>%
             mutate(gg = purrr::map(data,fun_plot3)) %>%
             select(Series=serie,gg) %>%
             filter(Series == "Loki")


#hawkeye
fun_plot4 <- function(data){
    trend <- ggplot(data, aes(episode_title, rating, fill = "episode")) +
      geom_bar(stat = "identity", width = 0.9)+
      geom_text(aes(label=rating), position=position_dodge(width=0.9), hjust=-0.50, size = 9, family = "bold")+
      scale_fill_manual(values = "#644073") +
      scale_y_continuous(breaks = c(0, 2.5, 5.0, 7.5, 12.0),
                         limits = c(0,10)) +
      labs(x="", y="")+
      coord_flip(expand=FALSE) +
      theme(
        panel.border=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=30, family="regular", color="black"),
        axis.line = element_blank(),
        legend.position = "none")
    return(trend)
  }

Base_Hawkeye <- data %>%
             group_by(serie) %>%
             nest() %>%
             mutate(gg = purrr::map(data,fun_plot4)) %>%
             select(Series=serie,gg) %>%
             filter(Series == "Hawkeye")
  

completo <- rbind(Base_Wanda, Base_Loki, Base_Falcon, Base_Hawkeye)



## Merge
Marvel2 <- Marvel1 %>%
           left_join(completo) %>%
           mutate(Episodes = as.integer(Episodes))

out3 <- Marvel2 %>%
        mutate(IMDB_Rating = NA) %>%
        select(Series = picture, Description = txt_html, Episodes, IMDB_Rating) %>%
        gt() %>%
    cols_width(
      Description ~ px(400),
      IMDB_Rating ~px(400)) %>%
        cols_label(IMDB_Rating = gt::html("<div><span>IMDB Rating</span></div> 
                                            <div><span> per Episode</span></div>")) %>%
        gt::text_transform(
          locations = cells_body(columns = IMDB_Rating),
          fn = function(x){
            purrr::map(
              Marvel2$gg, gt::ggplot_image,
              height = px(300), aspect_ratio = 2.7
            )}) %>%
  cols_align(
    align = "center",
    columns = Episodes
  ) %>%
  tab_header(
    title = "Marvel Series in 2021",
    subtitle = md("On the heels of **Avengers: Endgame (2019)**, the **</span><span style='color:#EF2D56'>Marvel Cinematic Universe (MCU)</span>** entered into
    its fourth phase in 2021. One of its most significant novelties is the production and release of several TV Series, which feature both
    known and acclaimed characters, such as Loki, and previously rather unexplored or even new persons, such as the Scarlet Wicher and Agatha Harkness, respectively.
    *Okay, you can ruin all my hard work and argue that **</span><span style='color:#EF2D56'>Marvel</span>** has been somewhat involved with TV Shows prior to the fourth phase because of Agents of S.H.I.E.L.D. Fair Enough*.
    Either way, in this brand-new phase, **</span><span style='color:#EF2D56'>Marvel</span>** has emphasised TV Series much more than it had ever done before. 
    In 2021 the franchise managed to deliver four TV Shows to the general public. This table highlights how many episodes each series has and how they were assessed by public opinion, measured through IMDB evaluations.")
  ) %>%
  tab_source_note(
    source_note = md("**Data:** IMDB | **Series Description:** Disney+ | **Author:** @lucas_coutoz")
  )%>%
  tab_style(
    locations = cells_title(groups = 'title'),
    style = list(
      cell_text(
        font=google_font(name = 'Barlow Semi Condensed'), 
        size='xxx-large',weight='bold',align='left',
        color='crimson'
      ))) %>%
  tab_style(
    locations = cells_title(groups = 'subtitle'),
    style = list(
      cell_text(
        font=google_font(name = 'Nunito'), 
        size='x-large',align='left'
      ))) %>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Nunito"), 
        align = "center",v_align = "middle",size='x-large',color='black')),
    locations = cells_column_labels(c(Series,Description, Episodes, IMDB_Rating))
  )%>%
  tab_style(
    style = list(
      cell_text(
        font=google_font(name = "Nunito"), 
        align = "center",v_align = "middle",size='x-large',color='black')),
    locations = cells_body(
      columns = Episodes,
      rows = Episodes == max(Episodes) | min(Episodes))
  ) %>%
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Barlow Semi Condensed"
      ),size='xx-large')),
    locations = cells_source_notes()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table_body.hlines.style = "solid",
    table_body.border.top.style = "solid"  
  ) 

gtsave(out3, "imoral1.html")