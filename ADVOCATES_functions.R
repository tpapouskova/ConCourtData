library(dplyr)
library(aod)
library(Rcpp)
library(ggplot2)
library(iterators)
library(reshape2)
setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y") )

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

completness_in_years <- function (all = raw_data, pined = data_pined, surnamed = data_surnamed) {
        cases_per_year <- group_by(all, Year = format(Decision.Date, "%Y")) %>% 
                summarise(Total=length(Registry.Sign))
        cases_pined_per_year <- group_by(pined, Year = format(Decision.Date, "%Y")) %>% 
                summarise(Total.Pined=length(Registry.Sign))
        cases_surnamed_per_year <- group_by(surnamed, Year = format(Decision.Date, "%Y")) %>% 
                summarise(Total.Surnamed=length(Registry.Sign))
        completness_data <- full_join(cases_per_year, cases_pined_per_year, by = "Year") %>% 
                full_join(cases_surnamed_per_year, by = "Year") %>%
                mutate(Share.Pined = Total.Pined/Total*100, 
                       Share.Surnamed = Total.Surnamed/Total*100)
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
        data_named$Registry.Sign <- gsub("^Pl ", "Pl.", data_named$Registry.Sign)
        data_named$Registry.Sign <- gsub("^1 ", "I.", data_named$Registry.Sign)
        data_named$Registry.Sign <- gsub("^2 ", "II.", data_named$Registry.Sign)
        data_named$Registry.Sign <- gsub("^3 ", "III.", data_named$Registry.Sign)
        data_named$Registry.Sign <- gsub("^4 ", "IV.", data_named$Registry.Sign)
        select(data_named, -PropDate)
}

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
                                 Rapporteur=Soudce.zpravodaj, 
                                 Dissent=Odlišné.stanovisko, 
                                 Proceeding.Type=Typ.řízení, 
                                 Proposer=Navrhovatel, 
                                 Decision.Form=Forma.rozhodnutí, 
                                 Decision.Type=Typ.výroku, 
                                 Citation=Paralelní.citace..Sbírka.nálezů.a.usnesení.,
                                 Importance=Význam,
                                 File=txt_file)  
        data <- select(data_all_named, Registry.Sign, Proposal.Date, Decision.Date, 
                       Rapporteur, Dissent, Proceeding.Type, Proposer, 
                       Decision.Form, Decision.Type, Citation, Importance, File) %>%
                filter(grepl(("vyhověno|zamítnuto|odmítnuto|zastaveno|mezinárodní"), 
                             Decision.Type)) %>%
                filter(Decision.Form == "Usnesení" | Decision.Form == "Nález") %>%
                mutate(Proceedings.Length = as.numeric(Decision.Date - Proposal.Date)) %>%
                arrange(Decision.Date)
        if (is.null(sample_size)) {
                return (data)
        } else {
                return (data[sample(nrow(data), sample_size), ])
        }
}

log_reg <- function (data, dependent, independent){
        logit_reg <- glm(dependent ~ independent, data = data, family = "binomial")
        summary(logit_reg)
        plot_data <- data.frame(independent = 0:900)
        plot_data$Probability <- predict(logit_reg, newdata = plot_data, type = "response")
        ggplot(plot_data, aes(x = independent, y = Probability)) +
                geom_line(size=1)
}

prepare_logit_reg <- function (data, discriminator) {
        data <- arrange(data, Decision.Date)
        Judgement.Dummy <- sapply(data$Decision.Form, function (form) {form == "Nález"})
        Win.Dummy <- sapply(data$Decision.Type, function (type) {grepl("vyhověno", type)})
        cases_before <- list()
        for (i in unique(data[,discriminator])) {
                cases_before[[i]] <- 0
        }
        Cases.Before <- c()
        for (i in 1:nrow(data)) {
                Cases.Before[i] <- cases_before[[data[,discriminator][i]]]
                cases_before[[data[,discriminator][i]]] <- cases_before[[data[,discriminator][i]]] + 1
        }
        judgements_before <- list()
        for (i in unique(data[,discriminator])) {
                judgements_before[[i]] <- 0
        }
        Judgements.Before <- c()
        for (i in 1:nrow(data)) {
                Judgements.Before[i] <- judgements_before[[data[,discriminator][i]]]
                if (data$Decision.Form[i] == "Nález") {
                        judgements_before[[data[,discriminator][i]]] <- judgements_before[[data[,discriminator][i]]] + 1  
                }
        }
        wins_before <- list()
        for (i in unique(data[,discriminator])) {
                wins_before[[i]] <- 0
        }
        Wins.Before <- c()
        for (i in 1:nrow(data)) {
                Wins.Before[i] <- wins_before[[data[,discriminator][i]]]
                if (grepl("vyhověno", data$Decision.Type[i])) {
                        wins_before[[data[,discriminator][i]]] <- wins_before[[data[,discriminator][i]]] + 1  
                }
        }
        cbind(data, Judgement.Dummy, Win.Dummy, Cases.Before, Judgements.Before, Wins.Before)
}

prepare_pined_data <- function (data) {
        data_new <- filter(data, PIN != "NA") %>% 
                mutate(Advocate.Known = paste (Advocate.Surname, Advocate.Name))
}

prepare_surnamed_data <- function (data) {
        data_new <- filter(data, !is.na(Advocate.Surname))
}

######## NOT SO USEFUL FUNCTIONS

filter_data <- function (data, cases_min=NULL, cases_max=NULL) {
        if (is.null(cases_min) & is.null(cases_max)) {
                return (data)
        } 
        if (!is.null(cases_min) & is.null(cases_max)) {
                advocates_with_min <- group_by(data, Advocate.Known) %>% 
                        summarize(Cases=length(Decision.Type)) %>%
                        filter(Cases>=cases_min)
                data[data$Advocate.Known %in% advocates_with_min$Advocate.Known, ]
        }
        if (is.null(cases_min) & !is.null(cases_max)) {
                advocates_with_max <- group_by(data, Advocate.Known) %>% 
                        summarize(Cases=length(Decision.Type)) %>%
                        filter(Cases<=cases_max)
                data[data$Advocate.Known %in% advocates_with_max$Advocate.Known, ]
        } else {
                advocates_with_min_max <- group_by(data, Advocate.Known) %>% 
                        summarize(Cases=length(Decision.Type)) %>%
                        filter((Cases>=cases_min) & (Cases<=cases_max))
                data[data$Advocate.Known %in% advocates_with_min_max$Advocate.Known, ]
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
        data_grouped_with_total <- merge(data_grouped,  cases_per_judge, by = "Advocate.Known")
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
        