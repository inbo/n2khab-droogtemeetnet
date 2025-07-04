
##Aanmaak invoerbestanden Hydromonitor voor Menyanthes"
#inladen libraries 
library(readr)
library(tidyverse)
library(odbc)
library(dbplyr)
library(lubridate)

options(dplyr.summarise.inform = FALSE)

## Importeren basisdata over peilpunten en/of peilmetingen

# Basisdata zijn 
# tabellen bevragen van de productiedatabank (SQL07-server), met een lazy-sql (data worden nog niet geimporteerd )
# peildefinities uit watina (liefst in RD-projectie)
# peilgegevens uit watina, opvragen per gebied
# neerslag uit waterinfo (via package WateRinfo)
# evapotranspiratie (bijeensprokkelen)

# tabel ophalen 
table_Watina_Live <- function(con, table){
  strSQL <- paste0("SELECT * FROM [D0025_00_Watina].[report].[",table,"]")
  stopifnot(dbplyr::is.sql(dbplyr::sql(strSQL)))
  return( tbl(con, dbplyr::sql(strSQL), quite =  ))
}
# functies importeren meetpunten Watina

import_watina_meetpunten_en_transformatie_naar_hydromonitorformaat <- function(gebied = NULL, lijstmeetpunten = NULL, oppervlaktewater = FALSE){
  # om getoond te kunnen worden in de huidige versie van Menyanthes(3.x.b.w, 11 december 2018) moeten de meetpunten RD-coördinaten hebben (crs = 28992)
  # argumenten :
  # gebied = de gebiedscode, een string met 3 karakters
  # lijstmeetpunten = een dataframe met de watinacodes (7 karakters) waarvoor je de peilen wenst op te vragen
  
  #controles
  if (!is.null(gebied) & !is.null(lijstmeetpunten)) stop("Nog geen gebied of lijst meetpunten opgegeven. De gebiedscode is een code bestaande uit 3 karakters, lijstmeetpunten moet een dataframe zijn met watinacodes (7 karakters, dus zonder de suffix)")
  gebiedcheck <- "nietok"
  if (!is.null(gebied)) {
    if (nchar(gebied) != 3) {
      stop("Een verkeerde opgave van een gebied. Deze gebeurt via de gebiedscode (3 karakters)")
      gebiedcheck <- "nietok"
    } else {
      gebiedcheck <- "ok"
    }
  }
  if (!is.null(lijstmeetpunten)  & !is.data.frame(lijstmeetpunten)) stop("'lijstmeetpunten' moet een dataframe zijn")
  if (gebiedcheck == "ok" & is.data.frame(lijstmeetpunten)) message("Er werd zowel een gebiedscode als een lijst met meetpunten opgegeven. Het uitvoerbestand zal alleen peilen van de lijst met meetpunten bevatten.")  
  
  #peildefinities uit watina
  #connectie naar datawarehouse (depreciated)
  # con <- dbConnect(odbc::odbc(), .connection_string = "Driver=SQL Server;Server=inbo-sql08-prd.inbo.be,1433;Database=W0002_00_Watina;Trusted_Connection=Yes;")
  con <- dbConnect(odbc::odbc(), .connection_string = "Driver=SQL Server;Server=inbo-sql07-prd.inbo.be,1433;Database=D0025_00_Watina;Trusted_Connection=Yes;")
  
  wt_meetpunten_bron <- table_Watina_Live(con, "vw_Meetpunt")
  wt_peilpunten_bron <- table_Watina_Live(con, "vw_Peilpunt")
  wt_peilpunten_totaal <- wt_peilpunten_bron %>% 
    filter(X > 0)     
  
  #een meetpunt kan meerdere locaties hebben. Er wordt in dat geval het gemiddelde van deze genomen. Deze coördinaten worden in Menyanthes immers enkel gebruikt voor een visuele weergave en voor het zoeken van nabijgelegen punten
  wt_peilpunten_basis <- wt_peilpunten_totaal %>% 
      group_by(GebiedCode, MeetpuntCode) %>% 
      summarise(X = mean(X, na.rm = TRUE),
           Y = mean(Y, na.rm = TRUE)) %>%  
      rename(meetpuntcode =  MeetpuntCode,
             x_lamb = X,
             y_lamb = Y) 
  # wt_peilpunten_basis %>% show_query()

  if (is.data.frame(lijstmeetpunten)) {
    wt_keuze <- "meetpunt"
    
    lijstmeetpunten_str <- sapply(lijstmeetpunten, class)
    wt_meetpunten <- NULL
    for (i in seq(1:ncol(lijstmeetpunten))) {
      if (lijstmeetpunten_str[i] == "character" & 
          nchar(iconv(enc2utf8(as.character(lijstmeetpunten[1,i])),sub = "byte")) == 7) {
      wt_meetpunten <- lijstmeetpunten %>% dplyr::pull(i)
      break()  
      }
    }
    if (is.null(wt_meetpunten)) {
      dbDisconnect(con)
      stop("In lijstmeetpunten werd geen veld met meetpunten (7 karakters) gevonden.\n")
    }
    peildef_sel <- wt_peilpunten_basis %>% 
      filter(meetpuntcode %in% wt_meetpunten) 
  } else {
    wt_keuzegebied <- gebied
    peildef_sel <- wt_peilpunten_basis %>% 
      filter(GebiedCode == gebied) 
    wt_keuze <- "gebied"
  }

  # wt_peilpunten_totaal %>% show_query()
  if (oppervlaktewater == FALSE) {
    wt_peilpunten <- 
      wt_peilpunten_totaal %>% 
      filter(PeilpuntType != "Peilschaal") %>% 
      dplyr::select(PeilpuntCode,PeilpuntStartdatum,PeilpuntEinddatum, PeilpuntToestand,
            PeilpuntTAWNulpunt, PeilpuntTAWMaaiveld, PeilpuntLengteFilter, PeilpuntLengteBuis,
             PeilpuntDiameter, PeilpuntType, MeetpuntCode) %>% 
      inner_join(peildef_sel %>% 
                   dplyr::select(meetpuntcode, x_lamb, y_lamb) %>% 
                   rename(MeetpuntCode = meetpuntcode),
                 by = "MeetpuntCode", copy = TRUE) %>% 
      collect() %>% 
      transmute(
             Name = MeetpuntCode,
             NITGCode = NA,
             OLGACode = NA,
             FilterNo = 1,
             StartDateTime = format(ymd(PeilpuntStartdatum), "%d-%m-%Y %H:%M"),
             XCoordinate = x_lamb,
             YCoordinate = y_lamb,
             SurfaceLevel = PeilpuntTAWMaaiveld,
             WellTopLevel = PeilpuntTAWNulpunt,
             FilterTopLevel = PeilpuntTAWNulpunt - PeilpuntLengteBuis + PeilpuntLengteFilter,
             FilterBottomLevel = PeilpuntTAWNulpunt - PeilpuntLengteBuis,
             WellBottomLevel = FilterBottomLevel,
             Status = case_when(
                PeilpuntToestand == "OK - bemeten" ~ "Active",
                PeilpuntToestand == "OK - niet bemeten" ~ "Inactive",
                PeilpuntToestand == "Onvindbaar" ~ "Abandoned", 
                PeilpuntToestand == "Verwijderd" ~ "Abandoned",  
                PeilpuntToestand == "Onbekend" ~ "Unknown",
                PeilpuntToestand == "Stuk - onherstelbaar" ~ "Abandoned",
                PeilpuntToestand == "Stuk - te herstellen" ~ "Abandoned",            
                TRUE ~ "Adjust"
                ),         
             TubeDiameter = PeilpuntDiameter,
             TubeMaterial = case_when(
                PeilpuntType == "piëzometer PVC" ~ "PVC",
                PeilpuntType == "peilbuis PVC" ~ "PVC",
                PeilpuntType == "piëzometer HDPE" ~ "HDPE",
                PeilpuntType == "peilbuis HDPE" ~ "HDPE",
                TRUE ~ "Unknown"            
                ),
             TubeType = case_when(
                PeilpuntType == "piëzometer PVC" ~ "standaardbuis",
                PeilpuntType == "peilbuis PVC" ~ "volledigFilter",
                PeilpuntType == "piëzometer HDPE" ~ "standaardbuis",
                PeilpuntType == "peilbuis HDPE" ~ "volledigFilter",
                PeilpuntType == "peilbuis buiten" ~ "volledigFilter",            
                TRUE ~ "Unknown"            
                ),         
             ) %>% 
      arrange(Name, dmy_hm(StartDateTime))
  } else {
    wt_peilpunten <- 
      wt_peilpunten_totaal %>% 
      filter(PeilpuntType == "Peilschaal") %>% 
      dplyr::select(PeilpuntCode,PeilpuntStartdatum,PeilpuntEinddatum, PeilpuntToestand,
             PeilpuntTAWNulpunt, PeilpuntTAWMaaiveld, PeilpuntLengteBuis,
             PeilpuntType, MeetpuntCode) %>% 
      inner_join(peildef_sel %>% 
                   dplyr::select(meetpuntcode, x_lamb, y_lamb) %>% 
                   rename(MeetpuntCode = meetpuntcode), copy = TRUE) %>% 
      collect() %>% 
      transmute(
        Name = MeetpuntCode,
        NITGCode = NA,
        OLGACode = NA,
        StartDateTime = format(ymd(PeilpuntStartdatum), "%d-%m-%Y %H:%M"),
        XCoordinate = x_lamb,
        YCoordinate = y_lamb,
        SurfaceLevel = PeilpuntTAWMaaiveld,
        GaugeTopLevel = PeilpuntTAWNulpunt,
        GaugeBottomLevel = PeilpuntTAWNulpunt - PeilpuntLengteBuis,
        Status = case_when(
          PeilpuntToestand == "OK - bemeten" ~ "Active",
          PeilpuntToestand == "OK - niet bemeten" ~ "Inactive",
          PeilpuntToestand == "Onvindbaar" ~ "Abandoned", 
          PeilpuntToestand == "Verwijderd" ~ "Abandoned",  
          PeilpuntToestand == "Onbekend" ~ "Unknown",
          PeilpuntToestand == "Stuk - onherstelbaar" ~ "Abandoned",
          PeilpuntToestand == "Stuk - te herstellen" ~ "Abandoned",            
          TRUE ~ "Adjust"
        )
      ) %>% 
      arrange(Name, dmy_hm(StartDateTime))    
  }
  dbDisconnect(con)
  
  #controle op het aantal meetpunten
  if (nrow(wt_peilpunten) == 0) stop("Er werden geen meetpunten in de Watina-databank gevonden, bijv. bij oppervlaktewater= TRUE wordt enkel naar peilschalen gezocht.\n")
  
  #coördinaten transformatie: van Lambert naar RD
  crs_RD <- 28992
  crs_Lambert <- 31370
  peildef_sf <- sf::st_as_sf(wt_peilpunten, coords = c("XCoordinate", "YCoordinate"), crs = crs_Lambert )
  peildef_RD <- sf::st_transform(peildef_sf, crs = crs_RD)  
  peildef_RD_coord <- sf::st_coordinates(peildef_RD) %>% 
    as.data.frame() %>% 
    rename(XCoordinate = X, YCoordinate = Y ) 

  if (oppervlaktewater == FALSE) {
    wt_peilpunten <- bind_cols(peildef_RD, peildef_RD_coord) %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(everything(), -SurfaceLevel, -WellTopLevel, -FilterTopLevel, 
                    -FilterBottomLevel, -WellBottomLevel, -Status, -TubeDiameter, 
                    -TubeMaterial, -TubeType, SurfaceLevel, WellTopLevel, FilterTopLevel,
                    FilterBottomLevel, WellBottomLevel, Status,
                    TubeDiameter, TubeMaterial, TubeType)    
  } else {
    wt_peilpunten <- bind_cols(peildef_RD, peildef_RD_coord) %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(everything(), -SurfaceLevel, -GaugeTopLevel, -GaugeBottomLevel, 
                    -Status, SurfaceLevel, GaugeTopLevel, GaugeBottomLevel, Status)
  }  
  return(wt_peilpunten)
}


#voorbeeld ophalen peildefinities 
# wt_peilpunten <- import_watina_meetpunten_en_transformatie_naar_hydromonitorformaat(peildef_locaties = read_csv("./data/Watina_meetpunten_inRD.csv"), oppervlaktewater = FALSE)


#functie die de header van de hydromonitor-file voor het importeren van meetpunten en grond/oppervlaktewaterpeilen aanmaakt
hydromonitor_observationwell_header <- function(oppervlaktewater =  FALSE){
if (oppervlaktewater == FALSE) {
  aantalvelden <- 19
} else {
  aantalvelden <- 16  
}
r1 <- paste0("Format Name;HydroMonitor - open data exchange format", 
             paste(rep_len(";",aantalvelden - 2 + 1), collapse = ""))
r2 <- paste0("Format Version;1.1", 
             paste(rep_len(";",aantalvelden - 2 + 1), collapse = ""))
r3 <- paste0("Format Definition;http://hydromonitor.nl/downloads/hydromonitor_data_exchange_format.pdf", 
             paste(rep_len(";",aantalvelden - 2 + 1), collapse = ""))
r4 <- paste0("File Type;CSV", 
             paste(rep_len(";",aantalvelden - 2 + 1), collapse = ""))
r5 <- paste0("File Contents;Header;Metadata;Data", 
                                paste(rep_len(";",aantalvelden - 4 + 1), collapse = ""))
if (oppervlaktewater == FALSE) {
  r6 <-
    paste0("Object Type;ObservationWell", 
           paste(rep_len(";",aantalvelden - 2 + 1), collapse = ""))
  r7 <- 
    paste0("Object Identification;Name;FilterNo", 
               paste(rep_len(";",aantalvelden - 3 + 1), collapse = ""))
  } else {
  r6 <-
    paste0("Object Type;SurfaceWaterLevelGauge", 
           paste(rep_len(";",aantalvelden - 2 + 1), collapse = ""))
  r7 <- 
    paste0("Object Identification;Name", 
           paste(rep_len(";",aantalvelden - 2 + 1), collapse = ""))  
  }
r8 <- paste0(paste(rep_len(";",aantalvelden), collapse = ""))
r8
if (oppervlaktewater == FALSE) {
  rVeld <- "Name;NITGCode;OLGACode;FilterNo;StartDateTime;XCoordinate;YCoordinate;SurfaceLevel;WellTopLevel;FilterTopLevel;FilterBottomLevel;WellBottomLevel;Status;TubeDiameter;TubeMaterial;TubeType" # zeker de eerste vier velden zijn verplicht
  rEenheden <- "[String];[String];[String];[Integer];[dd-mm-yyyy HH:MM];[m];[m];[m+ref];[m+ref];[m+ref];[m+ref];[m+ref];[Categorical];[m];[Categorical];[Categorical]" #deze eenheden zijn verplicht: ze kunnen niet aangepast worden  
} else {
  rVeld <- "Name;NITGCode;OLGACode;StartDateTime;XCoordinate;YCoordinate;SurfaceLevel;GaugeTopLevel;GaugeBottomLevel;Status" # zeker de eerste vier velden zijn verplicht
  rEenheden <- "[String];[String];[String];[dd-mm-yyyy HH:MM];[m];[m];[m+ref];[m+ref];[m+ref];[Categorical]" #deze eenheden zijn verplicht: ze kunnen niet aangepast worden
}
header <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, rVeld, rEenheden, deparse.level = 0)
return(header)
}


## Transformeren van peilbuisdefinities in Hydromonitor-formaat (csv)

# Deze functie voegt de header, de peilpuntdefinities en een slotregel in een dataframe samen. Als dit object als een csv wordt geëxporteerd (zie voorbeeld) kan het rechtstreeks in Menyanthes worden ingelezen.
hydromonitor_peilpunt_csv_formaat <- function(gebied = NULL, oppervlaktewater = FALSE){
# temppad <-  "./data/temp/"  
# if (!hasArg("peildef_locaties") ) stop("het invoerbestand 'peildef_locaties' werd niet gevonden.")
  
#tussenweg om snel de peilpunten in het juiste csv-formaat te krijgen
peilpunten_HM <- import_watina_meetpunten_en_transformatie_naar_hydromonitorformaat(gebied = gebied, lijstmeetpunten = lijstmeetpunten ,oppervlaktewater = oppervlaktewater)  
write_csv2(peilpunten_HM, "zzzpeilpuntenvb.csv", col_names = FALSE, na = "")
ht_peilpunten <- read_delim("zzzpeilpuntenvb.csv", delim = "µ", col_names = FALSE, lazy = FALSE)
file.remove("zzzpeilpuntenvb.csv")
if (oppervlaktewater == FALSE) {
  aantalvelden <- 19
} else {
  aantalvelden <- 16
}
rEindeHeaderDefinities <- paste(rep_len(";",aantalvelden), collapse = "")

hydromonitor_observationwell_gauge_csv <- rbind(hydromonitor_observationwell_header(oppervlaktewater = oppervlaktewater), as.matrix(ht_peilpunten), as.matrix(rEindeHeaderDefinities)) 
hydromonitor_observationwell_gauge_csv <- as.data.frame(hydromonitor_observationwell_gauge_csv)
return(hydromonitor_observationwell_gauge_csv)
}

# #voorbeeld
# hydromonitor_observationwell_csv <- hydromonitor_peilpunt_csv_formaat(peildef_locaties = read_csv("./data/Watina_meetpunten_inRD.csv"), gebied = "SIL")
# 
# #voorbeeld wegschrijven naar csv-bestand
# write_delim(hydromonitor_observationwell_csv, "./data/Hydromonitor_peilpunten_test.csv", col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")



## Transformeren van peilmetingen in Hydromonitor-formaat (csv)
# Er is een functie opgesteld die voor een opgegeven gebied (3char gebiedscode) de peilmetingen ophaalt. Ze biedt ook de optie om tegelijkertijd de peildefinities van dat gebied op te halen (ookdefinities = TRUE): deze definities worden opgeslagen in een nieuw dataframe : wt_peilpunten.

import_watina_peilmetingen <- 
  function(gebied = NULL, 
           lijstmeetpunten = NULL, 
           ookdefinities = FALSE, 
           inclIngegeven = FALSE, 
           oppervlaktewater = FALSE, 
           referentie = c("maaiveld", "TAW", "beide"),
           herleiden_tot_daggemiddelden = TRUE, 
           toMenyanthes= TRUE){

#controle van de argumenten
  if (is.null(gebied) & is.null(lijstmeetpunten)) stop("Nog geen gebied of lijst meetpunten opgegeven. De gebiedscode is een code bestaande uit 3 karakters, lijstmeetpunten moet een dataframe zijn met watinacodes (7 karakters, dus zonder de suffix)")
  
  gebiedcheck <- "geen gebiedscode"
  if (!is.null(gebied)) {
    if (nchar(gebied) != 3) {
      stop("Een verkeerde opgave voor het argument 'gebied'. Alleen de Watina-gebiedscodes (3 karakters) zijn valabel")
      gebiedcheck <- "nietok"
    } else {
      gebiedcheck <- "ok"
    }
  }
  if (!is.null(lijstmeetpunten)  & !is.data.frame(lijstmeetpunten)) stop("'lijstmeetpunten' moet een dataframe zijn")
  if (gebiedcheck == "ok" & is.data.frame(lijstmeetpunten)) message("Er werd zowel een gebiedscode als een lijst met meetpunten opgegeven. Het uitvoerbestand zal alleen peilen van de lijst met meetpunten bevatten.")

if (!is.logical(ookdefinities)) stop("Het argument 'ookdefinities' moet FALSE of TRUE zijn")
if (gebiedcheck == "geen gebiedscode") {
  if (nrow(lijstmeetpunten) == 0) stop("de 'lijstmeetpunten' is leeg.")
}

if (!referentie %in% c("maaiveld", "TAW", "beide")) stop("'referentie' moet een vector = 'maaiveld', 'TAW' of 'beide' zijn")

if (!is.logical(herleiden_tot_daggemiddelden)) stop("Het argument 'herleiden_tot_daggemiddelden' moet FALSE of TRUE zijn")
  
if (toMenyanthes & !referentie == "TAW") {
  referentie <- "TAW"
  message("om het bestand in Menyanthes te importeren, wordt als 'referentie' 'TAW' genomen")
} 

#ophalen metingen
con <- dbConnect(odbc::odbc(), .connection_string = "Driver=SQL Server;Server=inbo-sql07-prd.inbo.be,1433;Database=D0025_00_Watina;Trusted_Connection=Yes;")

if (ookdefinities == TRUE) {
  suppressMessages(import_watina_meetpunten_en_transformatie_naar_hydromonitorformaat(gebied, lijstmeetpunten, oppervlaktewater = oppervlaktewater)) %>% 
 {. ->> wt_peilpunten}
  }
#gebiedsinformatie
  if (is.data.frame(lijstmeetpunten)) {
    wt_keuze <- "meetpunt"
    wt_meetpunten <- "NIHIL"
    lijstmeetpunten_str <- sapply(lijstmeetpunten, class)
    for (i in seq(1:ncol(lijstmeetpunten))) {
      NAindex <- which(is.na(lijstmeetpunten[,i]))
      firstNA <- min(NAindex)
      if (min(firstNA) == Inf & lijstmeetpunten_str[i] == "character" & nchar(iconv(enc2utf8(as.character(lijstmeetpunten[1,i])), sub = "byte")) == 7) {
        wt_meetpunten <- lijstmeetpunten %>% dplyr::pull(i)
      }
    }
    if (wt_meetpunten[1] == "NIHIL") {
      stop("Het dataframe bevat geen veld met een 7-delige watinacode")
    }
  } else {
    wt_keuzegebied <- gebied
    wt_keuze <- "gebied"
  }
# wt_gebieden_bron <- tbl(con, "vwDimGebied")

#peildefinities
wt_meetpunten_bron <- table_Watina_Live(con, "vw_Meetpunt") %>% suppressMessages()
wt_peilpunten_bron <- table_Watina_Live(con, "vw_Peilpunt") %>% suppressMessages()
wt_peilpunten_totaal <- wt_peilpunten_bron %>% 
  filter(X > 0) #

#peilmetingen
wt_peilmetingen_bron <- table_Watina_Live(con, "vw_Peilmeting") %>% suppressMessages()
valable_peilmetingencategorie <- c("originele kalibratiemeting in het veld",
                                   "maaiveld onder water")
referentie_select1 <- case_when(
  referentie == "maaiveld" ~ "PeilmetingMaaiveld",
  referentie == "TAW" ~ "PeilmetingTAW"
)
if (referentie == "beide") {
  referentie_select1 <- "PeilmetingMaaiveld"
  referentie_select2 <- "PeilmetingTAW"
} else {
  referentie_select2 <- referentie_select1
}
# PeilmetingStatus == "Gevalideerd" PeilmetingStatus == "Ingegeven"
if (wt_keuze == "gebied") {
  if (oppervlaktewater == TRUE) {
    wt_peilmetingen_keuze <- wt_peilmetingen_bron %>% 
      dplyr::select(!!referentie_select1, !!referentie_select2, GebiedCode, 
             MeetpuntCode, PeilpuntCode, PeilmetingCategorie, PeilmetingStatus, 
             PeilmetingDatum) %>% 
      filter(PeilmetingStatus == "Gevalideerd" | PeilmetingStatus == "Ingegeven") %>% 
      filter(GebiedCode == wt_keuzegebied) %>% 
      filter(is.na(PeilmetingCategorie) | PeilmetingCategorie %in% valable_peilmetingencategorie) %>% 
      semi_join(wt_peilpunten_totaal %>% 
                  filter(PeilpuntType == "Peilschaal"), by = "PeilpuntCode") 
    
    # wt_nogtevalideren_peilmetingen <- wt_peilmetingen_bron %>% 
    #   dplyr::select(!!referentie_select1, !!referentie_select2, GebiedCode, 
    #          MeetpuntCode, PeilpuntCode, PeilmetingCategorie, PeilmetingStatus, 
    #          PeilmetingDatum) %>% 
    #   filter(GebiedCode == wt_keuzegebied,
    #          is.na(PeilmetingCategorie) | PeilmetingCategorie %in% valable_peilmetingencategorie,
    #          PeilmetingStatus == "Ingegeven"
    #   ) %>% 
    #   semi_join(wt_peilpunten_totaal %>% 
    #                      filter(PeilpuntType == "Peilschaal"), by = "PeilpuntCode")  
  } else {
    wt_peilmetingen_keuze <- wt_peilmetingen_bron %>% 
      dplyr::select(!!referentie_select1, !!referentie_select2, GebiedCode, 
             MeetpuntCode, PeilpuntCode, PeilmetingCategorie, PeilmetingStatus, 
             PeilmetingDatum) %>% 
      filter(PeilmetingStatus == "Gevalideerd" | PeilmetingStatus == "Ingegeven") %>%       
      filter(GebiedCode == wt_keuzegebied) %>% 
      filter(is.na(PeilmetingCategorie) | PeilmetingCategorie %in% valable_peilmetingencategorie) %>% 
      semi_join(wt_peilpunten_totaal %>% 
                  filter(PeilpuntType != "Peilschaal"), by = "PeilpuntCode") 

    # wt_nogtevalideren_peilmetingen <- wt_peilmetingen_bron %>% 
    #   dplyr::select(!!referentie_select1, !!referentie_select2, GebiedCode, 
    #          MeetpuntCode, PeilpuntCode, PeilmetingCategorie, PeilmetingStatus, 
    #          PeilmetingDatum) %>% 
    #   filter(GebiedCode == wt_keuzegebied,
    #          is.na(PeilmetingCategorie) | PeilmetingCategorie %in% valable_peilmetingencategorie,
    #          PeilmetingStatus == "Ingegeven"
    #   ) %>% 
    #   semi_join(wt_peilpunten_totaal %>% 
    #               filter(PeilpuntType != "Peilschaal"), by = "PeilpuntCode")  
  }
} else {

  if (oppervlaktewater == TRUE) {  
    wt_peilmetingen_keuze <- wt_peilmetingen_bron %>% 
      dplyr::select(!!referentie_select1, !!referentie_select2, GebiedCode, 
             MeetpuntCode, PeilpuntCode, PeilmetingCategorie, PeilmetingStatus, 
             PeilmetingDatum) %>%     
      filter(PeilmetingStatus == "Gevalideerd" | PeilmetingStatus == "Ingegeven") %>%       
      filter(MeetpuntCode %in% wt_meetpunten) %>% 
      filter(is.na(PeilmetingCategorie) | PeilmetingCategorie %in% valable_peilmetingencategorie) %>% 
      semi_join(wt_peilpunten_totaal %>% 
                  filter(PeilpuntType == "Peilschaal"), by = "PeilpuntCode") 
    
    # wt_nogtevalideren_peilmetingen <- wt_peilmetingen_bron %>% 
    #   dplyr::select(!!referentie_select1, !!referentie_select2, GebiedCode, 
    #          MeetpuntCode, PeilpuntCode, PeilmetingCategorie, PeilmetingStatus, 
    #          PeilmetingDatum) %>%     
    #   filter(meetpuntcode %in% wt_meetpunten,
    #          is.na(PeilmetingCategorie) | PeilmetingCategorie %in% valable_peilmetingencategorie
    #   ) %>% 
    #   semi_join(wt_peilpunten_totaal %>% 
    #               filter(PeilpuntType == "Peilschaal"), by = "PeilpuntCode")  
  } else {
    wt_peilmetingen_keuze <- wt_peilmetingen_bron %>% 
      dplyr::select(!!referentie_select1, !!referentie_select2, GebiedCode, 
             MeetpuntCode, PeilpuntCode, PeilmetingCategorie, PeilmetingStatus, 
             PeilmetingDatum) %>%     
      filter(PeilmetingStatus == "Gevalideerd" | PeilmetingStatus == "Ingegeven") %>% 
      filter(MeetpuntCode %in% wt_meetpunten) %>% 
      filter(is.na(PeilmetingCategorie) | PeilmetingCategorie %in% valable_peilmetingencategorie) %>% 
      semi_join(wt_peilpunten_totaal %>% 
                  filter(PeilpuntType != "Peilschaal"), by = "PeilpuntCode") 
  
    # wt_nogtevalideren_peilmetingen <- wt_peilmetingen_bron %>% 
    #   dplyr::select(!!referentie_select1, !!referentie_select2, GebiedCode, 
    #          MeetpuntCode, PeilpuntCode, PeilmetingCategorie, PeilmetingStatus, 
    #          PeilmetingDatum) %>%     
    #   filter(meetpuntcode %in% wt_meetpunten,
    #          is.na(PeilmetingCategorie) | PeilmetingCategorie %in% valable_peilmetingencategorie,
    #          PeilmetingStatus == "Ingegeven"
    #   ) %>% 
    #   semi_join(wt_peilpunten_totaal %>% 
    #               filter(PeilpuntType != "Peilschaal"), by = "PeilpuntCode")  
  }
}
# test <- wt_peilmetingen_keuze %>% collect

wt_peilmetingen_keuze <- wt_peilmetingen_keuze %>% 
  collect()
wt_nogtevalideren_peilmetingen <- wt_peilmetingen_keuze %>% 
  filter(PeilmetingStatus == "Ingegeven")

if (nrow(wt_nogtevalideren_peilmetingen) > 0) {
  message(paste0("Er zijn nog ", nrow(wt_nogtevalideren_peilmetingen), " te valideren metingen."))
  if (!inclIngegeven) {
    message(paste0("Deze zitten niet in het huidige bestand."))
  } else {
    message(paste0("Deze zitten wel in het huidige bestand."))    
  }
  {wt_nogtevalideren_peilmetingen ->> wt_nogtevalideren_peilmetingen}
}

if (herleiden_tot_daggemiddelden) {
  if (referentie == "beide") {
    wt_peilmetingen_keuze <- wt_peilmetingen_keuze %>% 
      group_by(GebiedCode, MeetpuntCode, PeilpuntCode, PeilmetingCategorie, 
               PeilmetingStatus, PeilmetingDatum) %>% 
      summarise(PeilmetingMaaiveld = mean(PeilmetingMaaiveld),
                PeilmetingTAW = mean(PeilmetingTAW)) %>% 
      ungroup()

  } else {
    if (referentie_select1 == "PeilmetingMaaiveld") {
      wt_peilmetingen_keuze <- wt_peilmetingen_keuze %>% 
        group_by(GebiedCode, MeetpuntCode, PeilpuntCode, PeilmetingCategorie, 
                 PeilmetingStatus, PeilmetingDatum) %>% 
        summarise(PeilmetingMaaiveld = mean(PeilmetingMaaiveld)) %>% 
        ungroup()    
    } else {
      wt_peilmetingen_keuze <- wt_peilmetingen_keuze %>% 
        group_by(GebiedCode, MeetpuntCode, PeilpuntCode, PeilmetingCategorie, 
                 PeilmetingStatus, PeilmetingDatum) %>% 
        summarise(PeilmetingTAW = mean(PeilmetingTAW)) %>% 
        ungroup()         
    }
  }
}



if (!inclIngegeven) {
  wt_peilmetingen_keuze <- wt_peilmetingen_keuze %>% 
    filter(PeilmetingStatus == "Gevalideerd")
}

if (toMenyanthes) {
  wt_peilmetingen <- 
    if (oppervlaktewater == TRUE) {
      wt_peilmetingen_keuze %>% 
      transmute(
        Name = MeetpuntCode,
        DateTime = format(ymd(PeilmetingDatum), "%d-%m-%Y %H:%M"),
        Level = PeilmetingTAW
      ) %>% 
      arrange(Name, DateTime)
    } else {
      wt_peilmetingen_keuze %>% 
        transmute(
          Name = MeetpuntCode,
          FilterNo = 1,
          DateTime = format(ymd(PeilmetingDatum), "%d-%m-%Y %H:%M"),
          LoggerHead = NA,
          ManualHead = PeilmetingTAW
        ) %>% 
        arrange(Name, DateTime)  
    }
} else {
  wt_peilmetingen <- wt_peilmetingen_keuze
  
  if (referentie_select2 == "PeilmetingMaaiveld") { 
    wt_peilmetingen <- wt_peilmetingen %>% 
      rename(peil_maaiveld = PeilmetingMaaiveld) %>% 
      dplyr::select(-PeilmetingTAW)
  } else {
    wt_peilmetingen <- wt_peilmetingen %>% 
      rename(peil_taw = PeilmetingTAW) 
    if (referentie_select1 == "PeilmetingMaaiveld") {
      wt_peilmetingen <- wt_peilmetingen %>% 
        rename(peil_maaiveld = PeilmetingMaaiveld) 
    } else {
      wt_peilmetingen <- wt_peilmetingen %>% 
        dplyr::select(-PeilmetingMaaiveld)
    }
  }
  wt_peilmetingen <- wt_peilmetingen %>% 
    mutate(datum = ymd(PeilmetingDatum)) %>% 
    rename(gebied_code = GebiedCode,
           categorie = PeilmetingCategorie,
           status = PeilmetingStatus
           ) %>% 
    dplyr::select(-PeilmetingDatum)
}

dbDisconnect(con)

return(wt_peilmetingen)
}

## voorbeelden
# voorbeeld <- import_watina_peilmetingen("SIL", FALSE)
# voorbeeld <- import_watina_peilmetingen("SIL", TRUE, peildef_locaties = read_csv("./data/Watina_meetpunten_inRD.csv"))


#de volgende functie haalt de peilmetingen op uit watina en transformeert ze in het hydromonitor-csv-formaat. Zie het voorbeeld voor hoe ze best in een csv-bestand worden weggeschreven. Dit bestand is in het hydromonitor-formaat en kan rechtstreeks in Menyanthes worden geïmporteerd.
#In de functie moet een gebied opgegeven worden (met de 3char-gebiedscode), men heeft de optie om de definities mee op te geven. De default is TRUE, omdat de definities een essentieel onderdeel zijn van het menyanthes-bestand.
#Men kan ook oppervlaktepeilen importeren: kies dan oppervlaktewater = TRUE. Vergewis je er wel van dat in het bestand met de locaties ook peilschalen staan.

hydromonitor_peilmeting_csv_formaat <- 
  function(gebied = NULL, 
           lijstmeetpunten = NULL, 
           ookdefinities = TRUE,
           oppervlaktewater = FALSE, 
           watinadata = NULL,
           herleiden_tot_daggemiddelden = TRUE,
           inclIngegeven = FALSE){
  # #controle van de argumenten
  #watinadata= list met 1 of 2 dataframes: steeds peilmetingen verkregen met de functie import_watina_peilmetingen en optioneel een dataset met de peildefinities
  # if (!is.null(gebied) & !is.null(lijstmeetpunten)) stop("Nog geen gebied of lijst meetpunten opgegeven. De gebiedscode is een code bestaande uit 3 karakters, lijstmeetpunten moet een dataframe zijn met watinacodes (7 karakters, dus zonder de suffix)")
  # if (nchar(gebied) != 3 | is.null(gebied)) stop("Een verkeerde opgave van een gebied. Deze gebeurt via de gebiedscode (3 karakters)")
  # if (!is.null(lijstmeetpunten)  & !is.data.frame(lijstmeetpunten)) stop("'lijstmeetpunten' moet een dataframe zijn")
  # if (!is.logical(ookdefinities)) stop("Het argument 'ookdefinities' moet FALSE of TRUE zijn")
  
  if (!is.null(watinadata) & !is.list(watinadata) ) stop("Een verkeerde opgave voor het argument watinadata. Het moet een lijst met 1 of 2 dataframes zijn, nl. een voor de metingen (obligaat) en een voor de peildefinities (optioneel)")
    
  if (!is.null(watinadata) & ookdefinities & length(watinadata) < 2) stop("Een verkeerde opgave voor het argument watinadata. Het moet een lijst met 2 dataframes zijn, omdat ookdefinities = TRUE is")

    #ophalen metingen en eventueel ook de peildefinities
  
  if (is.null(watinadata)) {
    wt_peilmetingen <- 
      import_watina_peilmetingen(gebied = gebied, 
                                 lijstmeetpunten =  lijstmeetpunten, 
                                 ookdefinities = ookdefinities, 
                                 oppervlaktewater = oppervlaktewater, 
                                 herleiden_tot_daggemiddelden =
                                   herleiden_tot_daggemiddelden, 
                                 referentie = "TAW",
                                 inclIngegeven = inclIngegeven)
    
    if (nrow(wt_peilmetingen) == 0) stop("Er werden voor de opgegeven argumenten geen peilmetingen gevonden.")

  } else {
    wt_peilmetingen <- watinadata[[1]]
  }
  # tussenweg om snel de peilmetingen in het juiste formaat te krijgen
  write_csv2(wt_peilmetingen, "zzzpeilmetingenvb.csv", col_names = FALSE, 
             na = "")
  ht_peilmetingen <- suppressMessages(
    read_delim("zzzpeilmetingenvb.csv", delim = "µ", col_names = FALSE, 
               lazy = FALSE))
  file.remove("zzzpeilmetingenvb.csv")
  
  if (oppervlaktewater == FALSE) {
    aantalvelden <- 19
  } else {
    aantalvelden <- 16
  }
  
  rEindeHeaderDefinities <- paste(rep_len(";",aantalvelden), collapse = "")  
  if (oppervlaktewater == FALSE) {
    rVeldHeadMetingen <- 
      paste0("Name;FilterNo;DateTime;LoggerHead;ManualHead",
             paste(rep_len(";",aantalvelden - 5 + 1), collapse = ""))
    
    rEenhedenMetingen <- 
      paste0("[String];[Integer];[dd-mm-yyyy HH:MM];[m+ref];[m+ref]",
             paste(rep_len(";",aantalvelden - 5 + 1), collapse = ""))
  } else {
    rVeldHeadMetingen <- 
      paste0("Name;DateTime;Level", paste(rep_len(";",aantalvelden - 3 + 1),
                                          collapse = ""))
    rEenhedenMetingen <- 
      paste0("[String];[dd-mm-yyyy HH:MM];[m+ref]",
             paste(rep_len(";",aantalvelden - 3 + 1), collapse = ""))    
  }  
  if (ookdefinities == TRUE) {
    if (!is.null(watinadata)) {
      wt_peilpunten <- watinadata[[2]]
    }
    write_csv2(wt_peilpunten, "zzzpeilpuntenvb.csv", 
               col_names = FALSE, na = "")
    ht_peilpunten <- suppressMessages(read_delim("zzzpeilpuntenvb.csv", 
                                                 delim = "µ", 
                                                 col_names = FALSE, 
                                                 lazy = FALSE) )
    file.remove("zzzpeilpuntenvb.csv")    
    hydromonitor_observationwell_peilmetingen_csv <- rbind(
      hydromonitor_observationwell_header(oppervlaktewater = oppervlaktewater),
      as.matrix(ht_peilpunten),
      as.matrix(rbind(rEindeHeaderDefinities,
                      rVeldHeadMetingen,
                      rEenhedenMetingen)),
      as.matrix(ht_peilmetingen))    
  } else {
    hydromonitor_observationwell_peilmetingen_csv <- rbind(
      hydromonitor_observationwell_header(oppervlaktewater = oppervlaktewater),
      as.matrix(rbind(rEindeHeaderDefinities, 
                      rVeldHeadMetingen,
                      rEenhedenMetingen)),
      as.matrix(ht_peilmetingen))  
  }
return(hydromonitor_observationwell_peilmetingen_csv)

}


# # voorbeelden
# 
# hydromonitor_observationwell_peilmetingen_csv <- hydromonitor_peilmeting_csv_formaat("SIL", FALSE)
# hydromonitor_observationwell_peilmetingen_csv <- hydromonitor_peilmeting_csv_formaat("SIL", TRUE, peildef_locaties = read_csv("./data/Watina_meetpunten_inRD.csv"))
# 
# write_delim(as.data.frame(hydromonitor_observationwell_peilmetingen_csv), "./data/Hydromonitor_peilmetingen_tor.csv", col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")




### invoeren van neerslag en PET gegevens
## meetstations

#functie voor het importeren van de metadata (naam, ligging, startdatum, naam beheerder) van de weerstations en deze data zal transformeren in een formaat dat goed in het hydromonitor-formaat kan omgezet worden.
import_meteo_meetpunten_en_transformatie_naar_hydromonitorformaat <- function(weerstations, x_rd, y_rd, veldnaam_met_naam, veldnaam_met_naam_suffix = "", veldnaam_met_begindatum= "BEGINDATUM", naambeheerder){

# weerstations <- read_csv("./data/Evapotranspiratie/stations_kmi_PET_metcoordinaten.csv")
# x_rd <- "X_RD"
# y_rd <- "Y_RD"
# veldnaam_met_naam <- "NAAM"
# veldnaam_met_begindatum <- "VAN"
# naambeheerder <- "KMI"
# veldnaam_met_naam_suffix <- ""

#checks

if (!hasArg("weerstations") ) stop("het bestand met de weerstations werd niet gevonden.")  
if (!hasArg("x_rd") ) stop("opgave x_rd ontbreekt. Dit is de veldnaam in het bestand met de weerstations met de x-coördinaten (in RD-formaat).")    
if (!hasArg("y_rd") ) stop("opgave y_rd ontbreekt. Dit is de veldnaam in het bestand met de weerstations met de y-coördinaten (in RD-formaat).")   
if (!hasArg("veldnaam_met_naam") ) stop("opgave veldnaam_met_naam ontbreekt. Dit is de veldnaam in het bestand met de weerstations met de namen van deze stations.")   
weerstations <- as.data.frame(weerstations)
if (!is.data.frame(weerstations) ) stop("het bestand met de weerstations moet als een dataframe kunnen ingelezen worden.")    
if (!hasArg("naambeheerder") ) stop("opgave van de naam van de beheerder (bijv. KMI, VMM, HIC) of de naam van het veld met de beheerder ontbreekt.")      
if (!(x_rd %in% colnames(weerstations))) stop("de opgegeven veldnaam met x_coordinaten (RD-projectie) werd niet gevonden")
if (!(y_rd %in% colnames(weerstations))) stop("de opgegeven veldnaam met y_coordinaten (RD-projectie) werd niet gevonden")
if (!(veldnaam_met_naam %in% colnames(weerstations))) stop("de opgegeven veldnaam van de stations werd niet gevonden")
if (is.na(naambeheerder) & !(naambeheerder %in% colnames(weerstations)))  { 
  stop("er werd geen beheerder opgegeven of het veld met de beheerder werd niet gevonden")
}
if (!(veldnaam_met_begindatum %in% colnames(weerstations))) {
  print("het veld met begindatum van de metingen werd niet gevonden. Er wordt een nieuw veld aangemaakt met begindatum 1 jan 1900")
  weerstations <- weerstations %>% mutate(StartDateTime = format(dmy("1 jan 1900"), "%d-%m-%Y"))
  veldnaam_met_begindatum <- "StartDateTime"
}

if (veldnaam_met_naam_suffix %in% colnames(weerstations)) {
  weerstations <- rename(weerstations, 
                         Suffix = veldnaam_met_naam_suffix
  )
} else {
  weerstations <- weerstations %>% 
    mutate(Suffix = "")
}

if (naambeheerder %in% colnames(weerstations)) {
  weerstations <- rename(weerstations, 
                         Name = veldnaam_met_naam,
                         StartDateTime = veldnaam_met_begindatum,
                         XCoordinate = x_rd,
                         YCoordinate = y_rd,
                         Beheerder = naambeheerder
  )
  weerstations_for_ht <- weerstations %>% 
    transmute(
      Name = paste0(Beheerder, "_", Name, Suffix),
      StartDateTime = format(if(is.Date(StartDateTime)){StartDateTime} else {dmy_hms(StartDateTime, truncated = 3)}, "%d-%m-%Y %H:%M"),
      XCoordinate,
      YCoordinate,
      SurfaceLevel = NA,
      LoggerSerial = NA,
      Organization = Beheerder,
      Status =  "Active",
      Comment =  NA,
      CommentBy = NA
    )
} else {
  weerstations <- rename(weerstations, 
                              Name = veldnaam_met_naam,
                              StartDateTime = veldnaam_met_begindatum,
                              XCoordinate = x_rd,
                              YCoordinate = y_rd
                              )
  weerstations_for_ht <- weerstations %>% 
    transmute(
      Name = paste0(naambeheerder, "_", Name, Suffix),
      StartDateTime = format(if(is.Date(StartDateTime)){StartDateTime} else {dmy_hms(StartDateTime, truncated = 3)}, "%d-%m-%Y %H:%M"),
      XCoordinate,
      YCoordinate,
      SurfaceLevel = NA,
      LoggerSerial = NA,
      Organization = naambeheerder,
      Status =  "Active",
      Comment =  NA,
      CommentBy = NA
    )
}
return(weerstations_for_ht)
}

# #voorbeelden
# voorbeeld <- import_meteo_meetpunten_en_transformatie_naar_hydromonitorformaat(weerstations = read_csv("./data/Evapotranspiratie/stations_kmi_PET_metcoordinaten.csv"), x_rd = "X_RD", y_rd = "Y_RD", veldnaam_met_naam = "LOCATION", veldnaam_met_begindatum = "VAN", naambeheerder = "KMI")

# voorbeeld <- import_meteo_meetpunten_en_transformatie_naar_hydromonitorformaat(weerstations = read_csv("./data/Neerslag/stations_vmm_neerslag_metcoordinaten.csv"), veldnaam_met_naam = "station_name", veldnaam_met_naam_suffix = "", naambeheerder = "VMM", x_rd = "X_RD", y_rd =  "Y_RD")


# hulpfunctie voor het aanmaken van de header (= 10 eerste regels) van het hydromonitor-bestand (neerslag/PET)
hydromonitor_weatherstation_header <- function(){
r1 <- "Format Name;HydroMonitor - open data exchange format;;;;;;;;"
r2 <- "Format Version;1.1;;;;;;;;"
r3 <- "Format Definition;http://hydromonitor.nl/downloads/hydromonitor_data_exchange_format.pdf;;;;;;;;"
r4 <- "File Type;CSV;;;;;;;;"
r5 <- "File Contents;Header;Metadata;Data;;;;;;"
r6 <- "Object Type;WeatherStation;;;;;;;;"
r7 <- "Object Identification;Name;;;;;;;;"
r8 <- ";;;;;;;;;"
rVeld <- "Name;StartDateTime;XCoordinate;YCoordinate;SurfaceLevel;LoggerSerial;Organization;Status;Comment;CommentBy" # alle velden zijn verplicht
rEenheden <- "[String];[dd-mm-yyyy HH:MM];[m];[m];[m+ref];[String];[String];[Categorical];[String];[String]" #deze eenheden zijn verplicht: ze kunnen niet aangepast worden
header <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, rVeld, rEenheden, deparse.level = 0)
return(header)
}



# hulpfunctie die de header, de metadata van de weerstations en een slotregel samenvoegt. 
hydromonitor_weerstation_csv_formaat <- function(weerstations, x_rd, y_rd, veldnaam_met_naam, veldnaam_met_naam_suffix = "", veldnaam_met_begindatum = "BEGINDATUM", naambeheerder){
weerstations_HM <- import_meteo_meetpunten_en_transformatie_naar_hydromonitorformaat(weerstations = weerstations, x_rd = x_rd, y_rd = y_rd, veldnaam_met_begindatum = veldnaam_met_begindatum, veldnaam_met_naam = veldnaam_met_naam, veldnaam_met_naam_suffix = veldnaam_met_naam_suffix, naambeheerder = naambeheerder)  
# temppad <-  "./data/temp/"  
#tussenweg om snel de peilpunten in het juiste csv-formaat te krijgen
write_csv2(weerstations_HM, "zzzweerstationsvb.csv", col_names = FALSE, na = "")
ht_weerstations <- read_delim("zzzweerstationsvb.csv", delim = "µ", col_names = FALSE, lazy = FALSE)
file.remove("zzzweerstationsvb.csv")

rEindeHeaderDefinities <- ";;;;;;;;;"

hydromonitor_weatherstation_csv <- rbind(hydromonitor_weatherstation_header(), as.matrix(ht_weerstations), as.matrix(rEindeHeaderDefinities)) 
# hydromonitor_weatherstation_csv <- as.data.frame(hydromonitor_weatherstation_csv)
return(hydromonitor_weatherstation_csv)
}

## voorbeeld
# voorbeeld <- hydromonitor_weerstation_csv_formaat(weerstations = read_csv("./data/Evapotranspiratie/stations_kmi_PET_metcoordinaten.csv"), x_rd = "X_RD", y_rd = "Y_RD", veldnaam_met_naam = "LOCATION", veldnaam_met_begindatum = "VAN", naambeheerder = "KMI")

#opgelet ! deze bestand niet als csv in menyanthes importeren. Verklarende reeksen kunnen enkel in hun geheel: metadata over de stations EN de metingen ervan worden geimporteerd
# not run write_delim(hydromonitor_weerstation_csv, "./data/neerslag/Hydromonitor_weerstation_vmm.csv", col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")


### hulpfunctie die de meteo-gegevens in een 'hydroformaat-ready'-formaat omzet

import_meteodata_en_transformatie_naar_hydromonitorformaat <- function(meteodata, veldnaam_met_neerslag_in_mm = "Neerslag-data", veldnaam_met_PET_in_mm = "PET-data", veldnaam_met_stationnaam, veldnaam_met_naam_suffix = "", veldnaam_met_datum, naambeheerder){

# test
# meteodata <- read_csv2("./data/Evapotranspiratie/Evapotr_Gras_Ukkel_2004_2006_KMI.csv")
# veldnaam_met_naam <- "Station"
# veldnaam_met_datum <- "Ukkel evapotranspiratie boven grass"
# veldnaam_met_neerslag_in_mm <- "Value"
# veldnaam_met_PET_in_mm <- "PET_mm"
# naambeheerder <- "VMM"
if (!exists("meteodata")) stop("het bestand met weerstations werd niet gevonden")
  
if (!hasArg("meteodata") ) stop("het bestand met de meteo-gegevens werd niet gevonden.") 
meteodata <- as.data.frame(meteodata)  
if (!is.data.frame(meteodata) ) stop("het bestand met de meteo-gegevens moet als een dataframe kunnen ingelezen worden.")   
if (!hasArg("veldnaam_met_neerslag_in_mm") & !hasArg("veldnaam_met_PET_in_mm")) stop("De opgave van de veldnaam_met_neerslag_in_mm OF de veldnaam_met_PET_in_mm ontbreekt.")
if (!hasArg("veldnaam_met_stationnaam") ) stop("opgave van de veldnaam_met_stationnaam ontbreekt. Dit is de veldnaam met de namen van deze stations.")
if (!hasArg("veldnaam_met_datum") ) stop("opgave van de veldnaam_met_datum ontbreekt.") 
if (!hasArg("naambeheerder") ) stop("opgave van de naam van de beheerder ontbreekt (een string of een veldnaam).")     
  
if (!(veldnaam_met_neerslag_in_mm %in% colnames(meteodata)) & !(veldnaam_met_PET_in_mm %in% colnames(meteodata))) stop("noch het veld met neerslaggegevens noch het veld met PET-gegevens werd  gevonden")  
if (!(veldnaam_met_stationnaam %in% colnames(meteodata))) stop("het veld met de naam van de stations werd niet gevonden")
if (!(veldnaam_met_datum %in% colnames(meteodata)))  stop("het veld met de dag van de meting werd niet gevonden")
if (is.na(naambeheerder) & !(naambeheerder %in% colnames(meteodata)))  { 
  stop("er werd geen beheerder opgegeven of het veld met de beheerder werd niet gevonden")
}
welkevelden <- 0
meteodata <- rename(meteodata, 
                            Name = veldnaam_met_stationnaam,
                            DateTime = veldnaam_met_datum
                            )
if (veldnaam_met_neerslag_in_mm %in% colnames(meteodata)) {
  meteodata <- rename(meteodata, Precipitation = veldnaam_met_neerslag_in_mm)
  welkevelden <- 1
}
if (veldnaam_met_PET_in_mm %in% colnames(meteodata)) { 
  meteodata <- rename(meteodata, Evaporation = veldnaam_met_PET_in_mm)
  welkevelden <- welkevelden + 2
}
if (welkevelden == 1) {
  meteodata <- meteodata %>% 
    mutate(Evaporation = NA)
}
if (welkevelden == 2) {
  meteodata <- meteodata %>% 
    mutate(Precipitation = NA)
}
# rep_len("NA", 98)
if (is.Date(meteodata$DateTime)) {
  meteodata <- meteodata %>% 
    mutate(DateTime =  format(DateTime, "%d-%m-%Y %H:%M"))
} else if (is.Date(as_date(dmy_hms(meteodata$DateTime,truncated = 3)))) {
  meteodata <- meteodata %>% 
    mutate(DateTime =  format(dmy_hms(meteodata$DateTime, truncated = 3), "%d-%m-%Y %H:%M"))
} else {
  meteodata <- meteodata %>% 
    mutate(DateTime =  format(dmy(meteodata$DateTime), "%d-%m-%Y %H:%M"))
}

if (veldnaam_met_naam_suffix %in% colnames(meteodata)) {
  meteodata <- meteodata %>% 
    rename(Suffix = veldnaam_met_naam_suffix)  
} else {
  meteodata <- meteodata %>% 
    mutate(Suffix = "")
}

if (naambeheerder %in% colnames(meteodata)) {
  meteodata <- meteodata %>% 
    rename(Beheerder = naambeheerder)
  meteodata_for_ht <- meteodata %>% 
    transmute(
      Name = paste0(Beheerder, "_", Name, Suffix),
      DateTime = DateTime,
      Precipitation,
      Evaporation,
      AirPressure = NA,
      Temperature = NA
    )
} else {
  meteodata_for_ht <- meteodata %>% 
    transmute(
      Name = paste0(naambeheerder, "_", Name, Suffix),
      DateTime = DateTime,
      Precipitation,
      Evaporation,
      AirPressure = NA,
      Temperature = NA
    )  
}
return(meteodata_for_ht)
}


# #voorbeeld
# voorbeeld <- import_meteodata_en_transformatie_naar_hydromonitorformaat(meteodata = read_csv("./data/Neerslag/neerslag_dagtotalen_waterinfo_VMMstations.csv"), veldnaam_met_stationnaam = "station_name", veldnaam_met_datum = "dag", naambeheerder = "VMM", veldnaam_met_neerslag_in_mm = "Value")


#functie die op basis van twee bestanden: nl. de meta-data van de weerstations en het databestand met de metingen (neerslag of PET) een bestand in het hydromonitor-formaat aanmaakt. Dit bestand kan rechtstreeks in Menyanthes worden ingevoerd (verklarende reeksen).

hydromonitor_weatherdata <- function(meteodata, veldinmeteo_met_neerslag_in_mm = "Neerslag-data", veldinmeteo_met_PET_in_mm = "PET-data", veldinmeteo_met_stationnaam, veldinmeteo_met_naam_suffix = "", veldinmeteo_met_datum, naambeheerder, weerstations, veldinstations_met_x_rd, veldinstations_met_y_rd, veldinstations_met_naam, veldinstations_met_naam_suffix = "", veldinstations_met_begindatum = "BEGINDATUM"){
  
#eerst ophalen van de metadata over de weerstations
  
ht_weerstations <- hydromonitor_weerstation_csv_formaat(weerstations = weerstations, x_rd = veldinstations_met_x_rd, y_rd = veldinstations_met_y_rd, veldnaam_met_begindatum = veldinstations_met_begindatum, veldnaam_met_naam = veldinstations_met_naam, veldnaam_met_naam_suffix = veldinstations_met_naam_suffix, naambeheerder = naambeheerder)  

#tussenweg om snel de metingen in het juiste formaat te krijgen
data <- import_meteodata_en_transformatie_naar_hydromonitorformaat(meteodata = meteodata, veldnaam_met_stationnaam = veldinmeteo_met_stationnaam, veldnaam_met_datum = veldinmeteo_met_datum, naambeheerder = naambeheerder, veldnaam_met_neerslag_in_mm = veldinmeteo_met_neerslag_in_mm, veldnaam_met_PET_in_mm = veldinmeteo_met_PET_in_mm, veldnaam_met_naam_suffix = veldinmeteo_met_naam_suffix) 

write_csv2(data, "zzzneerslagdatavb.csv", col_names = FALSE, na = "")
ht_neerslagdata <- read_delim("zzzneerslagdatavb.csv", delim = "µ", col_names = FALSE, lazy = FALSE)


rEindeHeaderDefinities <- ";;;;;;;;;"
rDatavelden <- "Name;DateTime;Precipitation;Evaporation;AirPressure;Temperature;;;;"
rDataEenheden <- "[String];[dd-mm-yyyy HH:MM];[mm];[mm];[hPa];[°C];;;;"



hydromonitor_weatherdata_csv <- rbind(as.matrix(ht_weerstations),
                                      as.matrix(rbind(rDatavelden,
                                                      rDataEenheden)),
                                      as.matrix(ht_neerslagdata)
                                      )
hydromonitor_weatherdata_csv <- as.data.frame(hydromonitor_weatherdata_csv)
return(hydromonitor_weatherdata_csv)
}

#voorbeeld
# voorbeeld <- hydromonitor_weatherdata_header(meteodata = read_csv("./data/Neerslag/neerslag_dagtotalen_waterinfo_VMMstations.csv"), veldinmeteo_met_stationnaam = "station_name", veldinmeteo_met_datum = "dag", naambeheerder = "VMM", veldinmeteo_met_neerslag_in_mm = "Value", weerstations = read_csv("./data/Neerslag/stations_vmm_neerslag_metcoordinaten.csv"), veldinstations_met_x_rd = "X_RD", veldinstations_met_y_rd = "Y_RD", veldinstations_met_naam = "station_name", veldinstations_met_begindatum = "VAN")
# 
# 
# 
# write_delim(voorbeeld, "./data/Evapotranspiratie/Hydromonitor_PETdata_VMM.csv", col_names = FALSE, quote_escape = FALSE, delim = "µ", na = "")

