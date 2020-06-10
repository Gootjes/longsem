


download_data <- function() {
  d <-
    readr::read_tsv(
      "https://raw.githubusercontent.com/jflournoy/misc-r-projects/master/lavaan_ri-clpm/srcddata.dat",
      na = "999.00",
      col_names = c(
        "anti1",
        "anti2",
        "anti3",
        "anti4",
        "read1",
        "read2",
        "read3",
        "read4",
        "gen",
        "momage",
        "kidage",
        "homecog",
        "homeemo",
        "id"
      )
    )


  antiread <- read.table("https://raw.githubusercontent.com/jflournoy/misc-r-projects/master/lavaan_ri-clpm/srcddata.dat",
                              na.strings = c("999.00"),
                              col.names = c("anti1", "anti2", "anti3", "anti4",
                                            "read1", "read2", "read3", "read4",
                                            "gen", "momage", "kidage", "homecog",
                                            "homeemo", "id")
  )
}
