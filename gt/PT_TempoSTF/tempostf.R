##Tabla STF
library(gt)
library(gtExtras)
library(tidyverse)

## My Version 
STF = 
  tribble(
    ~Pos, ~Ministro, ~Antecessor, ~Respons?vel, ~Diasateposse,
    1, "Edson Fachin", "Joaquim Barbosa", "Rousseff", "320",
    2, "L. R. Barroso", "Ayres Britto", "Rousseff", "220",
    3, "Luiz Fux", "Eros Grau", "Rousseff", "212",
    4, "Rosa Weber", "Ellen Gracie", "Rousseff", "133",
    5, "????", "Marco Aurélio", "Bolsonaro", ">88",
    6, "C?rmen L?cia", "Nelson Jobim", "Lula","84",
    7, "A. de Moraes", "Teori Zavascki", "Temer","62",
    8, "Gilmar Mendes", "Neri da Silveira", "FHC",  "57",
    9, "Ricardo Lewandowski", "Carlos Velloso", "Lula",  "56",
    10, "Dias Toffoli", "Menezes Direito", "Lula",  "52",
    11, "Nunes Marques", "Celso de Mello", "Bolsonaro", "23")

tempostf <- STF%>%
  gt()%>%
  gt_theme_nytimes() %>%
  cols_label(Diasateposse = "Dias at? a Posse") %>%
  gt_merge_stack(col1 = Ministro, col2 = Respons?vel) %>%
  cols_width(2 ~ px(175)) %>%
  cols_width(3 ~ px(175)) %>%
  cols_width(4 ~ px(175)) %>%
  cols_align(align = "center", columns = Diasateposse) %>%
  tab_style(
    style = list(
      cell_fill(color = "#ea7073"),
      cell_text(color = "black")),
    locations = cells_body(
      rows = Antecessor == "Joaquim Barbosa")) %>%
  tab_style(
    style = list(
      cell_fill(color = "#ee888a"),
      cell_text(color = "black")),
        locations = cells_body(
          rows = Antecessor == "Ayres Britto")) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f1a0a1"),
      cell_text(color = "black")),
    locations = cells_body(
      rows = Antecessor == "Eros Grau")) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f5b8b9"),
      cell_text(color = "black")),
    locations = cells_body(
      rows = Antecessor == "Ellen Gracie")) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f8cfd0"),
      cell_text(color = "black")),
    locations = cells_body(
      rows = Antecessor == "Marco Aurélio")) %>%
  tab_source_note(md("@oc_ipolunb"))%>%
  tab_style(
    style = cell_text(
      align = "right"
    ), 
    locations = cells_source_notes()) %>%
  tab_header(
    title = "Tempo necess?rio at? a posse de um ministro no STF",
    subtitle = "Diferença em dias da renúncia/aposentadoria do antecessor até a posse de um novo"
  ) %>%  gtsave("tempostf.png", path = tempdir())

  