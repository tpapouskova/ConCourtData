source("NALUS_data.R", encoding = 'UTF-8')

crawled_data <- load_crawled_data(sample_size = 4000)

const_court_data <- load_const_court_data()
const_court_data$Registry.Sign <- normalize_registry_sign(const_court_data$Registry.Sign)

data <- merge_data(crawled_data, const_court_data)
glimpse(data)

filtered_data <- prepare_data(data)


################


anova <- aov(filtered_data$Proceedings.Length ~ filtered_data$Advocate.Known) # TO BE DELETED
summary(anova)

sum(is.na(data$Advocate.Surname))

advocate_success <- group_by(data, paste(Advocate.Surname, Advocate.Name)) %>% 
        summarize(Cases=length(Decision.Type),
                  Judgement=sum(Decision.Form=="Nález"),
                  Ruling=sum(Decision.Form=="Usnesení"),
                  Judgement.Rate=Judgement/Cases,
                  Success=sum(grepl("vyhověno", Decision.Type)), 
                  Success.Rate=Success/Judgement,
                  Fail=sum(grepl("zamítnuto", Decision.Type)),
                  Fail.Rate=Fail/Judgement)
arrange(advocate_success,desc(Judgement), desc(Judgement.Rate))

advocate_judge_success <- group_by(data, paste(Advocate.Surname, Advocate.Name), Rapporteur) %>% 
        summarize(Cases=length(Decision.Type),
                  Judgement=sum(Decision.Form=="Nález"),
                  Ruling=sum(Decision.Form=="Usnesení"),
                  Judgement.Rate=Judgement/Cases,
                  Success=sum(grepl("vyhověno", Decision.Type)), 
                  Success.Rate=Success/Judgement,
                  Fail=sum(grepl("zamítnuto", Decision.Type)),
                  Fail.Rate=Fail/Judgement)
arrange(advocate_judge_success, desc(Judgement))

