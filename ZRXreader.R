library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# FUNZIONI
#-------------------------------------------------------------------------------

# Funzione per estrarre l'header del file
HeaderExtractor <- function(file){
  # Estrai le informazioni contenute nell'header
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
  return(header)
}

# Funzione per fare il parsing del timestamp in un formato più leggibile
TimeParser <- function(df){
  output <- df %>%
    # Trasforma la colonna timestamp in un oggetto di classe POSIXlt
    mutate_at(vars(timestamp), ~strptime(., format = "%Y%m%d%H%M%S")) %>%
    # Separa data e ora scrivendole con il formato corretto
    mutate(date = format(timestamp, format = "%Y-%m-%d"),
           time = format(timestamp, format = "%H:%M:%S")) %>%
    # Seleziona e ordina le colonne del df
    select(date, time, value, status, translatedStatus)
  return(output)
}

# Funzione che estrae i dati e li mette in un dataframe
ZRX2DF <- function(file){
  head <- HeaderExtractor(file)
  # Seleziona riga dell'header del file dove è presente l'header dei dati
  stringa <- head[grep("^#LAYOUT", head)]
  # Estrai la sottostringa tra parentesi tonde
  valori <-  gsub("\\(|\\)", "", regmatches(stringa, gregexpr("\\([^\\)]+\\)", stringa)))
  # Dividi i valori separati da virgola in un nuovo array di stringhe
  data_header <- unlist(strsplit(valori, ","))
  # Estrai i dati
  data <- read.table(file, skip = length(head), header = FALSE, fill = TRUE, sep = " ")
  # Nomi colonne dataframe
  colnames(data) <- data_header
  # Rendi il timestamp in un formato più leggibile
  data <- TimeParser(data)
  return(data)
}

#-------------------------------------------------------------------------------
# MAIN
#-------------------------------------------------------------------------------
# Percorso file di input
inputZRX <- '00590BL_AusserrojenMod.1_RoiadiFuoriMod.1_HNS_TagSum.zrx'

# da .zrx a dataframe
df <- ZRX2DF(inputZRX)
