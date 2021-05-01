library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
install.packages("ggrepel")
library(ggrepel)
library(tibble)

# Stelle den mitgegebenen Plot nach. Alle Daten dafür habt Ihr ja vorliegen!
# load df with all palamentarians  in the German BT


bundestag_2019 <- readRDS("data/bundestag_2019.rds")
nebeneinkuenfte <-read.csv("data/data-ROsix.csv") 

nebeneinkuenfte <- nebeneinkuenfte %>% 
  rename(Nebentatigkeiten=NebentÃ.tigkeiten., Mind_Einkunfte_Eur =Mindest.EinkÃ.nfte..in.Euro.) %>%
  janitor::clean_names() %>% 
  mutate(
    name_ = str_split(name, ", ") %>% map_chr(~rev(.) %>% paste(collapse = " ")), 
    nebentatigkeiten = nebentatigkeiten == "ja"
  ) %>% 
  select(nebentatigkeiten, mind_einkunfte_eur, name_)

nebeneinkuenfte$mind_einkunfte_eur <- as.numeric(nebeneinkuenfte$mind_einkunfte_eur)
  
bundestag_2019<-bundestag_2019 %>% 
  left_join(nebeneinkuenfte, by=c("name"="name_")) %>% 


str(bundestag_2019)


partei_farben <- list(
  "CDU" = "#444444",
  "SPD" = "#ff8989",
  "CSU" = "black",
  "FDP" = "yellow",
  "Grüne" = "green",
  "Linke" = "violet",
  "AfD" = "#90CBF9",
  "fraktionslos" = "grey"
)

data <- bundestag_2019 %>% 
  select(name, lebensdaten, land, fraktion, nebentatigkeiten, mind_einkunfte_eur)



ggplot(data, aes(lebensdaten, land,color=fraktion,#color=fraktion, 
                 size=mind_einkunfte_eur))+
  geom_point(position = "jitter")+
  geom_label(data=data %>% filter(mind_einkunfte_eur>400000), 
             aes(label=name, fill=factor(fraktion)),colour="white", 
             fontface="bold")+
 
  #geom_text(aes(label=ifelse(mind_einkunfte_eur>450000,
                             #as.character(name), ""), hjust=0.4, vjust=-0.5,))+
  scale_color_manual(values=partei_farben)+
  scale_fill_manual(values=partei_farben)+
  labs(
    x="Geburtsjahr", 
    y="", 
    size="Mindestnebeneinkünfte", 
    #color="Partei"
    fill="Partei")+ 
  theme_grey()

options(scipen=999)
