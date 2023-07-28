library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# FUNZIONI
#-------------------------------------------------------------------------------

# Funzione per estrarre l'header del file
HeaderExtractor <- function(file){
  # estrai le informazioni contenute nell'header
  con <- file(file, "r")
  header <- character()
  while (TRUE) {
    line <- readLines(con, n = 1)
    if (length(line) == 0) {
      # Fine del file, esci dal loop
      break
    } else if (substr(line, 1, 1) == "#") {
      # La riga inizia con "#", salvala nell'oggetto header
      header <- c(header, line)
    } else {
      # Non è una riga di header, esci dal loop
      break
    }
  }
  close(con)
  # seleziona riga dell'header del file dove è presente l'header dei dati
  stringa <- header[grep("^#LAYOUT", header)]
  # Utilizza regmatches() per estrarre la sottostringa tra parentesi tonde
  valori <-  gsub("\\(|\\)", "", regmatches(stringa, gregexpr("\\([^\\)]+\\)", stringa)))
  # Dividi i valori separati da virgola in un nuovo array di stringhe
  data_header <- unlist(strsplit(valori, ","))
  return(header)
}

# funzione per fare il parsing del timestamp in un formato più leggibile
TimeParser <- function(df){
  output <- df %>%
    # Trasforma la colonna datetime_string in un oggetto di classe POSIXlt
    mutate_at(vars(timestamp), ~strptime(., format = "%Y%m%d%H%M%S")) %>%
    # separa data e ora scrivendole con il formato corretto
    mutate(date = format(timestamp, format = "%Y-%m-%d"),
           time = format(timestamp, format = "%H:%M:%S")) %>%
    # seleziona e ordina le colonne del df
    select(date, time, value, status, translatedStatus)
  return(output)
}

# funzione che estrae i dati e li mette in un dataframe
ZRX2DF <- function(file){
  head <- HeaderExtractor(file)
  # seleziona riga dell'header del file dove è presente l'header dei dati
  stringa <- head[grep("^#LAYOUT", head)]
  # Utilizza regmatches() per estrarre la sottostringa tra parentesi tonde
  valori <-  gsub("\\(|\\)", "", regmatches(stringa, gregexpr("\\([^\\)]+\\)", stringa)))
  # Dividi i valori separati da virgola in un nuovo array di stringhe
  data_header <- unlist(strsplit(valori, ","))
  # estrai i dati
  data <- read.table(file, skip = length(head), header = FALSE, fill = TRUE, sep = " ")
  # nomi colonne dataframe
  colnames(data) <- data_header
  # rendi il timestamp in un formato più leggibile
  data <- TimeParser(data)
  return(data)
}

#-------------------------------------------------------------------------------
# MAIN
#-------------------------------------------------------------------------------
# file di input
inputZRX <- '00590BL_AusserrojenMod.1_RoiadiFuoriMod.1_HNS_TagSum.zrx'

df <- ZRX2DF(inputZRX)
