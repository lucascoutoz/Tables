##Tabla STF
library(gt)
library(gtExtras)
library(tidyverse)

##2019-2021

migracao <- tribble(
    ~partido, ~entradas, ~saidas, ~saldo,
    "PODE", 7, 3, "+4",
    "PSD", 6, 2, "+4",
    "MDB", 4, 1, "+3",
    "PROS", 3, 1, "+2",
    "PL", 2, 0, "+2",
    "Patriota", 2, 1 , "+1",
    "PP", 2, 1, "+1",
    "Cidadania", 2, 2, "0",
    "Republicanos", 1, 1, "0",
    "PDT", 0, 1, "-1",
    "PRP", 0, 1, "-1",
    "PSDB", 0, 1, "-1",
    "PTC", 0, 1, "-1",
    "Solidariedade", 0, 1, "-1",
    "PHS", 0, 2, "-2",
    "REDE", 0, 2, "-2",
    "PSB", 1, 3, "-2",
    "União Brasil", 1, 3, "-2",
    "PTB", 0, 3, "-3")
    

migracao%>%
    gt()%>%
    gt_theme_538() %>%
    cols_align(align = "center", columns = everything()) %>%
    gt_fa_repeats(
        column=entradas,
        palette = "green",
        name = "arrow-alt-circle-up",
        align='left'
    ) %>%
    gt_fa_repeats(
        column=saidas,
        palette = "red",
        name = "arrow-alt-circle-down",
        align='left'
    ) %>%
    tab_source_note(md("@oc_ipolunb"))%>%
    tab_style(
        style = cell_text(
            align = "right"
        ), 
        locations = cells_source_notes()) %>%
    tab_header(
        title = "Migração Partidária no Senado Federal desde 2019",
        subtitle = "Qual partido mais ganhou e qual partido mais perdeu?"
    ) %>% 
    tab_footnote(
        footnote = "DEM e PSL se fundiram para formar o União Brasil.",
        locations = cells_body(
            columns = partido,
            rows = partido == "União Brasil")
    ) %>%
    tab_style(
        style = cell_borders(
            sides = c("top"),
            style = "solid"
        ),
        locations = cells_body(
            rows = partido == "Cidadania")
        ) %>%
    tab_style(
        style = cell_borders(
            sides = c("top"),
            style = "solid"
        ),
        locations = cells_body(
            rows = partido == "PDT")
        ) %>% gtsave("migracao.png", path = tempdir())





    

##Marcio Bittar   -> MDB para Uni?o Brasil em 2021
##Fernando Collor -> PTC para PROS em 2019
##Lucas Barreto   -> PTB para PSD em 2019
##Eduardo Gir?o   -> PROS para PODE em 2019
##Leila Barros    -> PSB para Cidadania em 2021
##Reguffe         -> S/ Partido para PODE em 2021
##Marcos do Val   -> Cidadania para PODE em 2019
##Rose de Freitas -> PODE para MDB em 2021
##Jorge Kajuru    -> PRP para PSB em 2019
##Jorge Kajuru    -> PSB para Patriota em 2019
##Jorge Kajuru    -> Patriota para Cidadania em 2019
##Jorge Kajuru    -> Cidadania para PODE em 2021
##Vanderlan Cardoso -> PP para PSD em 2020
##Antonio Anastasia -> PSDB para PSD em 2020 
##Carlos Viana      -> PHS para PSD em 2019
##Rodrigo Pacheco   -> Uni?o Brasil para PSD em 2021
##Nelsinho Trad     -> PTB para PSD em 2019
##Vital do R?go     -> PSB para MDB em 2021
##lmano F?rrer     -> PODE para PP em 2020
##Fl?vio Arns       -> REDE para PODE em 2020
##Carlos Portinho   -> PSD para PL em 2020
##Fl?vio Bolsonaro -> Uni?o Brasil para Republicanos em 2020
##Fl?vio Bolsonaro -> Republicanos para Patriota em 2021
##Rom?rio          -> PODE para PL em 2021
##Styvenson Valentim -> REDE para PODE em 2019
##Zenaide Maia       -> PHS para PROS em 2019
##Telm?rio Mota      -> PTB para PROS em 2019
##Lasier Martins     -> PSD para PODE em 2019
##Giordano           -> Uni?o Brasil para MDB em 2021
##Eduardo Gomes      -> Solidariedade para MDB em 2019
##K?tia Abreu        -> PDT para PP em 2020

