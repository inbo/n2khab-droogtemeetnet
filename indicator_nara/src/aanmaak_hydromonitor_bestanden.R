source("G:/.shortcut-targets-by-id/0B0xcP-eNvJ9dX1VaOTdHeUFfSVU/PRJ_GRONDWATERGEGEVENS/Tijdreeksanalyse/Scripts/Aanmaak invoer bestanden Menyanthes (Hydromonitor)/VoerVoorMenyanthes.R")

#opgaven van een gebiedscode
localpath_output <- "./data/local/watina_tijdreeksen_menyanthes/"

#je hebt de keuze tussen het ophalen van alle grond- of oppervlaktewaterpeilen van een gebied
# of van de peilen voor een opgegeven lijst van meetpunten (watinacode van 7 karakters)


# voor een LIJST van meetpunten
# ophalen grondwaterpeilen voor een lijst meetpunten
summary(meetpunten) 
peilen <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten = meetpunten, 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE,
                                              inclIngegeven = FALSE)

# wegschrijven grondwaterpeilen in hydromonitor-formaat
write_delim(as.data.frame(peilen), 
            file = paste0(localpath_output, "Hydromonitor_peilmetingen_2024.csv"), 
            col_names = FALSE, 
            escape = "none", 
            delim = "µ", 
            na = "")

# speciaal geval voor TEUP045 (gprs-data ontbreken nog in Watina)
data_teu.bron  <- read_csv2(file.path("data", "local", "watina_tijdreeksen_menyanthes", "teup045_gprs.csv"))


# daggemiddelden berekenen
data_teu.gprs <- data_teu.bron %>% 
  mutate(datum = lubridate::dmy(datum)) %>% 
  group_by(across(-contains("peil_"))) %>% 
  summarise(peil_maaiveld = mean(peil_maaiveld),
            peil_taw = mean(peil_taw)) %>% 
  ungroup()

data_teu.watina <- import_watina_peilmetingen(lijstmeetpunten = meetpunten %>% 
                                                filter(meetpunt == "TEUP045"), 
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE,
                                              inclIngegeven = FALSE,
                                              referentie = "beide",
                                              toMenyanthes = FALSE)

data_teu <- data_teu.watina %>% 
  bind_rows(data_teu.gprs)

# dubbels eruit halen
data_teu <- data_teu %>% 
  left_join(data_teu %>% count(datum) %>% 
              filter(n > 1) %>% 
              mutate(dubbel = TRUE), by = join_by(datum)) %>% 
  filter(is.na(dubbel) | dubbel & is.na(categorie) & n == 2 | 
           dubbel & categorie == "originele kalibratiemeting in het veld" & status == "Gevalideerd" ) %>% 
  dplyr::select(-n, -dubbel)

# debugonce(hydromonitor_peilmeting_csv_formaat)
# peilen.teu <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten = meetpunten %>% 
#                                                     filter(meetpunt == "TEUP045"), 
#                                               ookdefinities = TRUE,
#                                               oppervlaktewater = FALSE,
#                                               inclIngegeven = FALSE)

data_teu.men <- data_teu %>% 
  transmute(
    Name = MeetpuntCode,
    FilterNo = 1,
    DateTime = format(ymd(datum), "%d-%m-%Y %H:%M"),
    LoggerHead = NA,
    ManualHead = peil_taw
  ) %>% 
  arrange(Name, DateTime)

data_teu.men <- list(data_teu.men, wt_peilpunten)

peilen.teu <- hydromonitor_peilmeting_csv_formaat(lijstmeetpunten = meetpunten %>%
                                                    filter(meetpunt == "TEUP045"),
                                              ookdefinities = TRUE,
                                              oppervlaktewater = FALSE,
                                              inclIngegeven = FALSE,
                                              watinadata = data_teu.men)

# wegschrijven grondwaterpeilen in hydromonitor-formaat
write_delim(as.data.frame(peilen.teu), 
            file = paste0(localpath_output, "Hydromonitor_peilmetingen_teup045_2024.csv"), 
            col_names = FALSE, 
            escape = "none", 
            delim = "µ", 
            na = "")
