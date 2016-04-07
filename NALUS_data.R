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
        data_all_named <- rename(data_all, 
                                 Registry.Sign=Spisová.značka, 
                                 Proposal.Date=Datum.podání, 
                                 Decision.Date=Datum.rozhodnutí, 
                                 Advocate=lawyer, 
                                 Advocate.Residence=residence, 
                                 Rapporteur=Soudce.zpravodaj, 
                                 Dissent=Odlišné.stanovisko, 
                                 Proceeding.Type=Typ.řízení, 
                                 Proposer=Navrhovatel, 
                                 Decision.Form=Forma.rozhodnutí, 
                                 Decision.Type=Typ.výroku, 
                                 Importance=Význam)  
        data <- select(data_all_named, Registry.Sign, Proposal.Date, Decision.Date, 
                       Advocate, Advocate.Residence, Rapporteur, Dissent, Proceeding.Type, Proposer, 
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

prepare_data <- function (data = data, cases_min=NULL, cases_max=NULL) {
        data_new <- filter(data, (!is.na(Advocate.Surname) & !is.na(Advocate.Name))) %>% 
                mutate(Proceedings.Length = as.numeric(Decision.Date - Proposal.Date), 
                           Advocate.Known = paste (Advocate.Surname, Advocate.Name))
        if (is.null(cases_min) & is.null(cases_max)) {
                return (data_new)
        } 
        if (!is.null(cases_min) & is.null(cases_max)) {
                advocates_with_min <- group_by(data_new, Advocate.Known) %>% 
                        summarize(Cases=length(Decision.Type)) %>%
                        filter(Cases>=cases_min)
                data_new[data_new$Advocate.Known %in% advocates_with_min$Advocate.Known, ]
        }
        if (is.null(cases_min) & !is.null(cases_max)) {
                advocates_with_max <- group_by(data_new, Advocate.Known) %>% 
                        summarize(Cases=length(Decision.Type)) %>%
                        filter(Cases<=cases_max)
                data_new[data_new$Advocate.Known %in% advocates_with_max$Advocate.Known, ]
        } else {
                advocates_with_min_max <- group_by(data_new, Advocate.Known) %>% 
                        summarize(Cases=length(Decision.Type)) %>%
                        filter((Cases>=cases_min) & (Cases<=cases_max))
                data_new[data_new$Advocate.Known %in% advocates_with_min_max$Advocate.Known, ]
        }
}

cases_into_buckets <- function (data, arrange_by = data$Proceedings.Length, nbuckets = 10) {
        data_new <- arrange(data, arrange_by)
        Bucket <- rep(1:nbuckets, each = ceiling(nrow(data)/nbuckets))[1:nrow(data)]
        cbind(data_new, Bucket)
}

advocate_cases_bucketed <- function (data_bucketed) {
        data_grouped <- group_by(data_bucketed, Advocate.Known, Bucket) %>% 
                summarize(Cases=length(Registry.Sign),
                          Judgements=sum(Decision.Form=="Nález"))
        cases_per_advocate <- group_by(data_bucketed, Advocate.Known) %>% 
                summarize(Cases.Total=length(Registry.Sign),
                          Judgements.Total=sum(Decision.Form=="Nález"))
        data_grouped_with_total <- merge(data_grouped,  cases_per_advocate, by = "Advocate.Known")
        mutate(data_grouped_with_total, Cases.Share = Cases/Cases.Total, Cases.Share.Weighted = Cases/Cases.Total*Cases, 
               Judgements.Share = Judgements/Judgements.Total, Judgements.Share.Weighted = Judgements/Judgements.Total*Judgements) %>% 
                arrange(desc(Cases.Share.Weighted))
}

judge_cases_bucketed <- function (data_bucketed) {
        data_grouped <- group_by(data_bucketed, Rapporteur, Bucket) %>% 
                summarize(Cases=length(Registry.Sign),
                          Judgements=sum(Decision.Form=="Nález"))
        cases_per_judge <- group_by(data_bucketed, Rapporteur) %>% 
                summarize(Cases.Total=length(Registry.Sign),
                          Judgements.Total=sum(Decision.Form=="Nález"))
        data_grouped_with_total <- merge(data_grouped,  cases_per_advocate, by = "Advocate.Known")
        mutate(data_grouped_with_total, Cases.Share = Cases/Cases.Total, Cases.Share.Weighted = Cases/Cases.Total*Cases, 
               Judgements.Share = Judgements/Judgements.Total, Judgements.Share.Weighted = Judgements/Judgements.Total*Judgements) %>% 
                arrange(desc(Cases.Share.Weighted))
}

advocate_judge_cases_bucketed <- function (data_bucketed) {
        data_grouped <- group_by(data_bucketed, Advocate.Known, Rapporteur, Bucket) %>% 
                summarize(Cases=length(Registry.Sign),
                          Judgements=sum(Decision.Form=="Nález"))
        cases_per_advocate_judge <- group_by(data_bucketed, Advocate.Known, Rapporteur) %>% 
                summarize(Cases.Judge.Total=length(Registry.Sign),
                          Judgements.Judge.Total=sum(Decision.Form=="Nález"))
        cases_per_advocate <- group_by(data_bucketed, Advocate.Known) %>% 
                summarize(Cases.Total=length(Registry.Sign),
                          Judgements.Total=sum(Decision.Form=="Nález"))
        data_grouped_with_judge_total <- merge(data_grouped,  cases_per_advocate_judge, by = c("Advocate.Known", "Rapporteur"))
        data_grouped_with_totals <- merge(data_grouped_with_judge_total, cases_per_advocate, by = "Advocate.Known")
        mutate(data_grouped_with_totals, Cases.Judge.Share = Cases/Cases.Judge.Total, Cases.Judge.Share.Weighted = Cases/Cases.Judge.Total*Cases,
               Cases.Total.Share = Cases/Cases.Total, Cases.Total.Share.Weighted = Cases/Cases.Total*Cases,
               Judgements.Judge.Share = Judgements/Judgements.Judge.Total, Judgements.Judge.Share.Weighted = Judgements/Judgements.Judge.Total*Judgements,
               Judgements.Total.Share = Judgements/Judgements.Total, Judgements.Total.Share.Weighted = Judgements/Judgements.Total*Judgements) %>% 
                arrange(desc(Cases.Judge.Share.Weighted))
}

advocate_judge_statistics <- function (data) {
        advocate_stats <- group_by(data, Advocate.Known) %>% 
                summarize(Cases.Total=length(Registry.Sign),
                          Judgements.Total=sum(Decision.Form=="Nález"),
                          Mean.Proceedings.Length = mean(Proceedings.Length),
                          Judgement.Rate = sum(Decision.Form=="Nález")/length(Registry.Sign))
        advocate_judge_stats <- group_by(data, Advocate.Known, Rapporteur) %>% 
                summarize(Judge.Cases.Total=length(Registry.Sign),
                          Judge.Mean.Proceedings.Length = mean(Proceedings.Length),
                          Judge.Judgement.Rate = sum(Decision.Form=="Nález")/length(Registry.Sign))
        merge(advocate_stats, advocate_judge_stats, by = "Advocate.Known") %>%
                mutate(Cases.Judge.Share = Judge.Cases.Total/Cases.Total, 
                       Mean.Difference = Judge.Mean.Proceedings.Length - Mean.Proceedings.Length,
                       Judgement.Rate.Difference = Judge.Judgement.Rate - Judgement.Rate)
}
        