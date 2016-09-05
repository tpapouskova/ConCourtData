setwd("C:/Users/Tereza Doležalová/Dropbox/Škola/Ph.D/ConCourtData")
source("NALUS_data.R", encoding = 'UTF-8')
crawled_data <- load_crawled_data()
library(reshape2)
library(scales)

#decision_length <- function (data) {
#        sapply(data$File, function(txt_file){
#                sum(nchar(readLines(txt_file)))
#        })
#}

justices_data <- read.csv("US_justices.csv", 
                          sep = "\t", 
                          colClasses = c(rep("character", 2), 
                                         "factor", 
                                         rep("myDate", 2), 
                                         "character", 
                                         rep("myDate", 2), 
                                         "character"))
senates_data <- read.csv("US_senates.csv", 
                         sep = "\t", 
                         colClasses = c(rep("character", 2), 
                                        "factor", 
                                        rep("myDate", 2))) %>%
        mutate(Justice=paste(Surname, Name, sep=" ")) %>%
        arrange(Start.Date) %>%
        mutate(Justice=factor(Justice, levels = unique(Justice)))

###############################################################################
# functions
###############################################################################
dissent_heatmap <- function(dissent_data = dissent_data) {
        dissent_data_new <- mutate(dissent_data, Rapporteur.Character = as.character(Rapporteur))
        dissent_data_groupped = group_by(dissent_data_new, Rapporteur, Dissent) %>% summarize(N = n())
        g <- ggplot(dissent_data_groupped, aes(Rapporteur, Dissent, fill = N))
        g <- g + scale_fill_gradient(low = "light grey", high = "black")
        g + geom_raster() + theme_bw() + theme(
                axis.text.x = element_text(angle = 90, hjust = 1),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
        ) + ggtitle ("Graf č. 5: Disentní schéma soudu") +
        scale_y_discrete(limits = rev(levels(unique(dissent_data$Dissent)))) + xlab("Souce zpravodaj") + ylab("Soudce s odlišným stanoviskem")
}

# dissent_heatmap_perc <- function(dissent_data = dissent_data) {
#         dissent_data_new <- mutate(dissent_data, Rapporteur.Character = as.character(Rapporteur))
#         dissent_data_groupped_raw <- group_by(dissent_data_new, Rapporteur, Dissent) %>% 
#                 summarize(N = n())
#         dissent_total <- group_by(dissent_data_groupped_raw,Dissent) %>% 
#                 summarize(Dissents.Total=sum(N))
#         dissent_data_groupped <- left_join (dissent_data_groupped_raw, dissent_total, by="Dissent") %>%
#                 mutate(Procento = (N/Dissents.Total)*100)
#         View(dissent_data_groupped)
#         g <- ggplot(dissent_data_groupped, aes(Rapporteur, Dissent, fill = Procento))
#         g <- g + scale_fill_gradient(low = "light grey", high = "black")
#         g + geom_raster() + theme_bw() + theme(
#                 axis.text.x = element_text(angle = 90, hjust = 1),
#                 panel.grid.major = element_blank(),
#                 panel.grid.minor = element_blank()
#         ) + ggtitle ("Graf č. 5: Disentní schéma soudu (v %)") +
#                 scale_y_discrete(limits = rev(levels(unique(dissent_data$Dissent)))) + xlab("Souce zpravodaj") + ylab("Soudce s odlišným stanoviskem")
# }

dissent_table <- function (data = crawled_data) {
        data_new <- mutate(
                data, 
                Dissent.Character = as.character(Dissent),
                Rapporteur.Character = as.character(Rapporteur)
        )
        Result.Rapporteur <- c()
        Result.Dissent <- c()
        for (row_index in 1:nrow(data)) {
                row <- data_new[row_index, ]
                rapporteur <- row[, "Rapporteur.Character"]
                dissents <- row[, "Dissent.Character"]
                if (nchar(dissents) < 5) {
                        next
                }
                for (dissent in unlist(strsplit(dissents, "---"))) {
                        Result.Rapporteur <- c(Result.Rapporteur, rapporteur)
                        Result.Dissent <- c(Result.Dissent, trimws(dissent))
                }
        }
        data.frame(Rapporteur = Result.Rapporteur, Dissent = Result.Dissent)
}

justices_timeline <- function (data = senates_data) {
        data$Senate <- factor(data$Zařazení, levels=rev(levels(data$Zařazení)))
        ggplot(data, aes(colour=Zařazení)) + 
                geom_segment(aes(x=Start.Date, xend=End.Date, y=Justice, yend=Justice), size=4.5) +
                ylab ("") + xlab ("") + ggtitle ("Graf č. 1: Funkční období a zařazení soudců do senátů") +
                scale_colour_grey(start=0.7, end=0) + 
                geom_text(aes(x = (Start.Date + ((End.Date - Start.Date)/2)), 
                              y = Justice, label = Zařazení), 
                          color = "white", 
                          size = 4, 
                          fontface = "bold",
                          nudge_y = 0.1) +
                scale_fill_discrete(
                        breaks=factor(
                                c("předseda","místopředseda","I. senát", "II. senát", "III. senát", "IV. senát"),
                                levels=c("předseda","místopředseda","I. senát", "II. senát", "III. senát", "IV. senát"),
                                ordered = T
                        )
                ) +
                scale_x_date(breaks = seq(min(data$Start.Date), max(data$End.Date), by="1 year"), labels = date_format("%Y")) +
                theme (legend.position = "bottom", 
                       axis.text = element_text(size = 12), 
                       axis.title = element_text(size=14), 
                       plot.title = element_text(size = 16), 
                       legend.text = element_text(size = 14)) +
                scale_y_discrete(limits = rev(levels(data$Justice)))
}

officials_load <- function (decisions_data = crawled_data, senates_data = senates_data) {
        officials <- senates_data[grepl("předseda", senates_data$Zařazení),]
        result <- data.frame()
        for (i in 1:nrow(officials)) {
                official <- officials[i, ]
                official_data <- decisions_data[
                        (decisions_data$Rapporteur == official$Justice) &
                        (decisions_data$Proposal.Date >= official$Start.Date) &
                        (decisions_data$Proposal.Date <= official$End.Date)
                , ]
                counts <- group_by (official_data, Year = format(official_data$Proposal.Date, "%Y")) %>%
                        summarize(N = n())
                for (j in 1:nrow(counts)) {
                        year_count <- counts[j, ]
                        result <- rbind(result, data.frame(
                                Official = official$Justice,
                                Year = year_count$Year,
                                N = year_count$N
                        ))
                }
                
        }
        cast(result, Year ~ Official, value = "N", fill = "-")
}

# probability_judgement_with_time <- function (data, nbuckets = 20) {
#         data_new <- group_by(data, Rapporteur) %>% do((function (group_data){
#                 group_data_new <- arrange(group_data, Proceedings.Length)
#                 Bucket <- rep(1:nbuckets, each = ceiling(nrow(group_data_new)/nbuckets))[1:nrow(group_data_new)]
#                 cbind(group_data_new, Bucket)
#         })(.))
#         to_plot <- group_by(data_new, Rapporteur, Bucket) %>% 
#                 summarize(Judgements.Share = sum(Decision.Form=="Nález")/length(Registry.Sign))
#         g <- ggplot(to_plot, aes(Bucket, Judgements.Share))
#         g <- g + geom_bar(stat="identity") + facet_grid(Rapporteur ~ .) + theme(strip.text.y = element_text(angle=0))
#         ggsave("probability_judgement_with_time.png", plot = g, width = 50, height = 100, units = "cm")
# }

# rapporteur_collection <- function (data) {
#         group_by(data, Rapporteur) %>%
#                 summarize(QS.Rulings.In.Collection = sum((grepl("SbNU",Citation))&(Decision.Form=="Usnesení")&grepl("zjevnou",Decision.Type)),
#                           NS.Rulings.In.Collection = sum((grepl("SbNU",Citation))&(Decision.Form=="Usnesení")&!grepl("zjevnou",Decision.Type)))
# }

rapporteur_decision_form <- function (data = crawled_data, justices_data = justices_data) {
        qs_ns_rulings <- rapporteur_decision_number(data, justices_data) %>% 
                select(Rapporteur, Quasi.Substantive.Rulings, Non.Substantive.Rulings, Decisions.Total)
        judgements_granted <- group_by(data[(data$Decision.Form == "Nález") & grepl("vyhověno|soulad", data$Decision.Type),], Rapporteur) %>%
                summarize(Judgements.Granted = n())
        judgements_rejected <- group_by(data[(data$Decision.Form == "Nález") & !grepl("vyhověno|soulad", data$Decision.Type),], Rapporteur) %>%
                summarize(Judgements.Rejected = n())
        merged_data <- left_join(qs_ns_rulings, judgements_granted, by = "Rapporteur") 
        merged_data <- left_join(merged_data, judgements_rejected, by = "Rapporteur")
        transmute(merged_data,
                  Rapporteur = Rapporteur,
                  Perc.Judgements.Granted = 100 * (Judgements.Granted/Decisions.Total),
                  Perc.Judgements.Rejected = 100 * (Judgements.Rejected/Decisions.Total),
                  Perc.Quasi.Substantive.Rulings = 100 * (Quasi.Substantive.Rulings/Decisions.Total),
                  Perc.Non.Substantive.Rulings = 100 * (Non.Substantive.Rulings/Decisions.Total))
}

rapporteur_decision_form_plot <- function (decision_form_data, data = crawled_data) {
        to_plot <- select(decision_form_data, Rapporteur, Perc.Judgements.Granted) %>%
                arrange(-(Perc.Judgements.Granted)) %>%
                mutate(Rapporteur=factor(Rapporteur, levels = unique(Rapporteur)))
        ggplot(to_plot, aes(Rapporteur, Perc.Judgements.Granted)) +
                geom_bar(stat="identity")+
                xlab("")+ylab("% v celkovém počtu zpravodajovaných konečných rozhodnutí") +
                ggtitle ("Graf č. 4: Podíl vyhovujících nálezů na celkovém počtu konečných rozhodnutí") +
                geom_hline(yintercept = 100 * sum((data$Decision.Form == "Nález") & grepl("vyhověno|soulad", data$Decision.Type))/length(data$Registry.Sign), color = "black", size=1.2) + 
                coord_flip() + scale_x_discrete(limits = rev(levels(to_plot$Rapporteur)))
}

rapporteur_decision_number <- function (data = crawled_data, justices_data_raw = justices_data) {
        decisions_total <- group_by(data, Rapporteur) %>%
                summarize(Decisions.Total = n())
        judgements <- group_by(data[data$Decision.Form == "Nález",], Rapporteur) %>%
                summarize(Judgements = n())
        quasi_substantive_rulings <- group_by(data[(data$Decision.Form == "Usnesení") & grepl("zjevnou", data$Decision.Type),], Rapporteur) %>%
                summarize(Quasi.Substantive.Rulings = n())
        non_substantive_rulings <- group_by(data[(data$Decision.Form == "Usnesení") & !grepl("zjevnou", data$Decision.Type) ,], Rapporteur) %>%
                summarize(Non.Substantive.Rulings = n())
        justices_data <- transmute(justices_data_raw, 
                                   Rapporteur = paste(Surname, Name, sep = " "), 
                                   Days = (ifelse(is.na(Start.Date.2),
                                                  (End.Date - Start.Date),
                                                  ((End.Date - Start.Date)+ (End.Date.2 - Start.Date.2)))))
        merged_data <- left_join(justices_data, decisions_total, by = "Rapporteur") 
        merged_data <- left_join(merged_data, judgements, by = "Rapporteur")
        merged_data <- left_join(merged_data, quasi_substantive_rulings, by = "Rapporteur")
        merged_data <- left_join(merged_data, non_substantive_rulings, by = "Rapporteur")
        mutate(merged_data, 
               Decisions.Per.Month = (Decisions.Total/Days)*30, 
               Judgements.Per.Month = (Judgements/Days)*30,
               Quasi.Substantive.Rulings.Per.Month = (Quasi.Substantive.Rulings/Days)*30,
               Non.Substantive.Rulings.Per.Month = (Non.Substantive.Rulings/Days)*30)
} 

rapporteur_decision_number_plot <- function (decision_number_data) {
        to_plot <- select(decision_number_data, Rapporteur, Judgements.Per.Month, Quasi.Substantive.Rulings.Per.Month, Non.Substantive.Rulings.Per.Month) %>%
                arrange(-(Judgements.Per.Month + Quasi.Substantive.Rulings.Per.Month + Non.Substantive.Rulings.Per.Month)) %>%
                mutate(Rapporteur=factor(Rapporteur, levels = unique(Rapporteur)))
        long_format <- melt(to_plot, id.vars = "Rapporteur")
        ggplot(long_format,aes(x=Rapporteur,y=value,fill=factor(variable, labels=c("nálezy", "kvazimeritorní usnesení", "nemeritorní usnesení"))))+
                geom_bar(stat="identity")+
                scale_fill_grey() +
                labs (x = "", y = "Průměrný počet za měsíc (počet na den působení ve funkci * 30)", fill = "Forma rozhodnutí") +
                ggtitle("Graf č. 2: Průměrný počet rozhodnutí určité formy za měsíc") +
                coord_flip() + scale_x_discrete(limits = rev(levels(to_plot$Rapporteur))) 
}

rapporteur_time <- function (data = crawled_data) {
        all <- group_by(data, Rapporteur) %>%
                summarize(Median.All = median(Proceedings.Length))
        judgements <- group_by(data[data$Decision.Form == "Nález",], Rapporteur) %>%
                summarize(Median.Judgements = median(Proceedings.Length))
        quasi_substantive_rulings <- group_by(data[(data$Decision.Form == "Usnesení") & grepl("zjevnou", data$Decision.Type),], Rapporteur) %>%
                summarize(Median.Quasi.Substantive.Rulings = median(Proceedings.Length))
        non_substantive_rulings <- group_by(data[(data$Decision.Form == "Usnesení") & !grepl("zjevnou", data$Decision.Type) ,], Rapporteur) %>%
                summarize(Median.Non.Substantive.Rulings = median(Proceedings.Length))
        merged_data <- left_join(all, judgements, by = "Rapporteur")
        merged_data <- left_join(merged_data, quasi_substantive_rulings, by = "Rapporteur")
        merged_data <- left_join(merged_data, non_substantive_rulings, by = "Rapporteur")
}

rapporteur_time_boxplots <- function (data = crawled_data) {
        grouped_data <- group_by(data, Rapporteur) %>%
                summarise(Median = median(Proceedings.Length)) %>%
                arrange(Median)
        data$Rapporteur <- factor(data$Rapporteur, levels = rev(unique(grouped_data$Rapporteur)), ordered = T)
        g <- ggplot(data, aes(Rapporteur, Proceedings.Length))
        g <- g + geom_boxplot()
        g <- g + geom_hline(yintercept = as.numeric(median(data$Decision.Date - data$Proposal.Date)), color = "black", size=1.2)
        g <- g + xlab("") + ylab("Délka řízení (ve dnech)") + ggtitle("Graf č. 3: Krabicový diagram délky řízení")
        g + coord_flip()
}

# year_decision_form <- function (data) {
#         to_plot <- group_by(data, year = format(Decision.Date, "%Y")) %>% summarise(perc = 100 * sum(Decision.Form == "Nález")/length(Registry.Sign))
#         g <- ggplot(to_plot, aes(year, perc))
#         g <- g + geom_bar(stat="identity")
#         g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# }

# year_time_boxplots <- function (data) {
#         g <- ggplot(data, aes(format(Decision.Date, "%Y"), Proceedings.Length))
#         g <- g + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#         g + xlab("Rok rozhodnutí") + ylab("Doba vyřízení")
# }

###############################################################################
# main
###############################################################################

# Graf 1
justices_timeline(senates_data)
ggsave("Graf 1.png", width = 20, height = 20, units = "cm")

# Tabulka 1
officials <- officials_load(crawled_data, senates_data)
write.csv(officials, "DATA PAPER_officials load.csv")

# Graf 2
decision_number <- rapporteur_decision_number(crawled_data, justices_data)
View(decision_number)
rapporteur_decision_number_plot(decision_number)
ggsave("Graf 2.png", width = 20, height = 20, units = "cm")

# Graf 3
decision_time <- rapporteur_time(crawled_data)
View(decision_time)
rapporteur_time_boxplots (crawled_data)
ggsave("Graf 3.png", width = 20, height = 20, units = "cm")
#year_time_boxplots(crawled_data)

# Graf 4
decision_form <- rapporteur_decision_form(crawled_data, justices_data)
View(decision_form)
rapporteur_decision_form_plot(decision_form)
ggsave("Graf 4.png", width = 20, height = 20, units = "cm")
#year_decision_form(crawled_data)
#probability_judgement_with_time(crawled_data)

# Graf 5
dissent_data <- dissent_table(crawled_data)
dissent_data_new <- mutate(dissent_data, Rapporteur.Character = as.character(Rapporteur))
dissent_data_groupped <- group_by(dissent_data_new, Rapporteur, Dissent) %>% summarize(N = n())
dissent_total <- group_by(dissent_data_groupped,Dissent) %>% 
        summarize(Dissents.Total=sum(N)) %>% 
        transmute(Rapporteur = Dissent, Diseents.Total = Dissents.Total)
View(dissent_total)
dissented_total <- group_by(dissent_data_groupped,Rapporteur) %>% 
        summarize(Dissented.Total=sum(N))
View(dissented_total)
dissent_heatmap(dissent_data)
ggsave("Graf 5.png", width = 20, height = 20, units = "cm")

# Tabulka 2
result <- select(decision_number, Rapporteur, Decisions.Per.Month, Judgements.Per.Month, Quasi.Substantive.Rulings.Per.Month, Non.Substantive.Rulings.Per.Month) %>%
        left_join(decision_time, by = "Rapporteur")
result <- left_join(result, decision_form, by = "Rapporteur")
result <- left_join(result, dissent_total, by = "Rapporteur")
result <- left_join(result, dissented_total, by = "Rapporteur")
result[,-1] <- round(result[,-1],2)
write.csv(result,"DATA PAPER_result.csv")




# collection <- rapporteur_collection(filter(crawled_data, format(Decision.Date, "%Y") <=2013))
# decision <- rapporteur_decision_number(filter(crawled_data, format(Decision.Date, "%Y") <=2013), justices_data)
# collection_percentage <- merge(decision, collection, by = "Rapporteur") %>%
#         transmute(Rapporteur=Rapporteur,
#                   QS.Rulings.In.Collection=QS.Rulings.In.Collection,
#                   Perc.QS.Rulings.In.Collection=QS.Rulings.In.Collection/Quasi.Substantive.Rulings,
#                   NS.Rulings.In.Collection=NS.Rulings.In.Collection,
#                   Perc.NS.Rulings.In.Collection=NS.Rulings.In.Collection/Non.Substantive.Rulings)
# View(collection_percentage)
