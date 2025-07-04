library(watina)
con <- connect_watina()

#extra functies ophalen
source(file.path("src", "VoerVoorMenyanthes.R"))

# hier een voorbeeld voor ZABP014
# eerst alle data ophalen voor ZAB
# dan worden de tijdreeksen aflopend gesorteerd op basis van aantal peilmetingen (beter zou zijn om dichtstbijzijnde te nemen en dat die pb zouden bijgehouden worden)
# eigenlijk is het alleen nodig om de recente/nieuwe data te bekijken

data_peilen_gw <- import_watina_peilmetingen(gebied = "ZAB", ookdefinities = TRUE, referentie = "beide", toMenyanthes = FALSE, inclIngegeven = TRUE)
colnames(data_peilen_gw) <- snakecase::to_snake_case(colnames(data_peilen_gw))
df <- data_peilen_gw
check <- data_peilen_gw %>% count(meetpunt_code)
df <- data_peilen_gw %>% filter(str_detect(meetpunt_code, pattern = "014|004|006"),
                                year(datum) > 2018)
g <- ggplot(df, aes(x = datum, y = peil_maaiveld, color = meetpunt_code) ) + geom_line()
g

# indien de grafische controle ok is, dan kunnen de data in Watina als visueel gevalideerd gemerkt worden.

