#' Extract filepaths readrds_csv
#'
#' Functie die voor Genereer_outputbestanden_database bestandspaden achterhaald van ingelezen bestanden
#'
#' @param bestandspad Het complete bestandspad van het script
#'
#'
extract_filepaths_readrds_csv <- function(bestandspad) {
  ## Lees alle regels van het bestand in
  Regels <- base::readLines(bestandspad, warn = FALSE)
  ## Koppel deze achter elkaar zodat afgekapte regels een geheel worden
  Regels_collapsed <-
    base::paste(c(Regels, ""), collapse = "")
  ## Zoek met een regex naar plekken waar een rds of csv bestand wordt ingelezen
  ## en sla voor alle overeenkomsten alles tussen de () op in Regels_read.
  Regels_read <-
    unlist(
      stringr::str_extract_all(Regels_collapsed,
                               '(?<=readrds_csv\\()(.*?)(?=(\\)))')
    )
  ## Extraheer het bestandspad uit de gevonden ReadRDS_CSV regels
  ## met een regex die kijkt naar bestandspaden die beginnen met een nummer en punt en
  ## eindigen op .rds.
  Rds_string <- "[0-9]\\..*?\\.rds"
  Rds_paden <- stringr::str_extract(Regels_read, Rds_string)
  Rds_paden <- Rds_paden[!is.na(Rds_paden)]

  ## Koppel de gevonden bestandspaden aan het pad van het script waar ze uit komen
  ## en return dit als dataframe
  dependencies <- NULL
  for (pad in Rds_paden) {
    dependencies <-
      base::rbind(dependencies,
                  base::data.frame(Bestand = bestandspad,
                                   Afhankelijkheid = pad))
  }
  return(dependencies)
}
