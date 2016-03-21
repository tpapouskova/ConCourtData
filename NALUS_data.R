library(dplyr)

load_crawled_data <- function (filename = "NALUS data.csv", sample_size = NULL) {
        setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y") )
        data_all <- read.csv(filename, 
                         header = TRUE, 
                         sep = ";", 
                         encoding = "UTF-8", 
                         comment.char = "", 
                         colClasses = c(rep("myDate", 4), 
                                        rep("character", 2), 
                                        "factor", 
                                        "character", 
                                        rep("factor", 5), 
                                        rep("character", 6), 
                                        "factor",
                                        "character",
                                        rep("factor",2),
                                        "character",
                                        "factor",
                                        rep("character", 6)), 
                         quote="^", 
                         na.strings = "",
                         strip.white = T)
        colnames(data_all) <- make.names(colnames(data_all))
        data <- data_all
        data_all_named <- rename(data_all, Registry.Sign=Spisová.značka, Proposal.Date=Datum.podání, Decision.Date=Datum.rozhodnutí, 
                                 Advocate=lawyer, Advocate.Residence=residence, Rapporteur=Soudce.zpravodaj, Dissent=Odlišné.stanovisko, 
                                 Proceeding.Type=Typ.řízení, Proposer=Navrhovatel, Decision.Form=Forma.rozhodnutí, Decision.Type=Typ.výroku, 
                                 Importance=Význam)  
        data <- select(data_all_named, Registry.Sign, Proposal.Date, Decision.Date, Advocate, Advocate.Residence, Rapporteur, Dissent, Proceeding.Type, Proposer, 
                       Decision.Form, Decision.Type, Importance)
        if (is.null(sample_size)) {
                return (data)
        } else {
                return (data[sample(nrow(data), sample_size), ])
        }
}

load_const_court_data <- function (filename = "US data.csv") {
        setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )
        data <- read.csv(filename, 
                 header = TRUE, 
                 sep = ",", 
                 comment.char = "", 
                 colClasses = c("character","myDate", rep("character", 2), rep("factor", 2), "character"), 
                 na.strings = "",
                 strip.white = T)
        data_named <- rename(data, Registry.Sign=RegSign, Advocate.Name=name, Advocate.Surname=surname)
        select(data_named, -PropDate)
}

normalize_registry_sign <- function (registry_sign) {
        normalized <- gsub("^1 ", "I.", registry_sign)
        normalized <- gsub("^2 ", "II.", normalized)
        normalized <- gsub("^3 ", "III.", normalized)
        gsub("^4 ", "IV.", normalized)
}

merge_data <- function (crawled, const_court) {
        joined <- left_join(crawled, const_court, by = "Registry.Sign")
}

prepare_data <- function (data, min=10) {
        data_new <- mutate(data, Proceedings.Length = as.numeric(Decision.Date - Proposal.Date), Advocate.Known = paste (Advocate.Surname, Advocate.Name))
        advocates_with_min <- group_by(data_new, Advocate.Known) %>% 
                summarize(Cases=length(Decision.Type)) %>%
                filter(Cases>=min)
        data_new[(data_new$Advocate.Known %in% advocates_with_min$Advocate.Known) & (data_new$Advocate.Known!="NA NA"), ]
}