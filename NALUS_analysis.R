setwd("C:/Users/Tereza Doležalová/Dropbox/Škola/Ph.D/ConCourtData")

library(ggplot2)
source("NALUS_data.R", encoding = 'UTF-8')

crawled_data <- load_crawled_data()
const_court_data <- load_const_court_data()
const_court_data$Registry.Sign <- normalize_registry_sign(const_court_data$Registry.Sign)
raw_data <- merge_data(crawled_data, const_court_data)
glimpse(raw_data)

data_all <- prepare_data(raw_data)
glimpse(data_all)
length(unique(data_all$Advocate.Known))

################ interesting pieces of analysis
# doba rizeni je lognormalne rozlozena
qplot(log(Proceedings.Length), data=data_all, geom = "density")

# doba rizeni se zda byt zavisla na advokatovi NUTNO JESTE PODROBNEJI PROVERIT
anova <- aov(data_all$Proceedings.Length ~ data_all$Advocate.Known)
summary(anova)

# naivni rozdeleni si pripadu do 10 sochtliku dle delky rizeni
data_bucketed <- cases_into_buckets(data_all)
# vztah delky rizeni a advokata
advocate_data_bucketed <- advocate_cases_bucketed(data_bucketed)
# vztah delky rizeni a soudce
judge_data_bucketed <- judge_cases_bucketed(data_bucketed)
# vztah delky rizeni a soudce pro jednotliveho advokata
advocate_judge_data_bucketed <- advocate_judge_cases_bucketed(data_bucketed)

# je pro konkretniho advokata zavisla doba rizeni na soudci, kteremu stiznost napadne?
# je pro konkretniho advokata zavisla judgement.rate na soudci, kteremu stiznost napadne?
# je pro konkretniho advokata zavisla uspesnost nalezu na soudci, kteremu stiznost napadne?
advocate_judge_stat_data <- advocate_judge_statistics (data_all) # zajimavy je napr vztah Sokola a Wagnerove, mozna Gotz a Jurka


advocate_data <-  group_by(data_all, Advocate.Known) %>% 
        summarize (Cases=length(Registry.Sign),
                   Judgements=sum(Decision.Form=="Nález"),
                   Wins=sum(grepl("vyhověno", Decision.Type)),
                   Win.Rate=sum(grepl("vyhověno", Decision.Type))/sum(Decision.Form=="Nález"),
                   Losses=sum(grepl("zamítnuto", Decision.Type)),
                   Loss.Rate=sum(grepl("vyhověno", Decision.Type))/sum(Decision.Form=="Nález"))
successful_advocate_data <- filter(advocate_data,Judgements>=1)   

n <- nrow(caseses_per_advocate)
# kolik tak jednotlivi advokati podavaji stiznosti?
qplot((1:n - 1)/(n - 1), sort(advocate_data$Cases), 
      xlab = "Sample Fraction",
      ylab ="Sample Quantile",
      geom = "line",
      main = "Cases")
# kolik tak na advokata pripada nalezu
qplot((1:n - 1)/(n - 1), sort(advocate_data$Judgements), 
      xlab = "Sample Fraction",
      ylab ="Sample Quantile",
      geom = "line",
      main = "Judgements")
# a kolik vyhoveno
qplot((1:n - 1)/(n - 1), sort(advocate_data$Wins), 
      xlab = "Sample Fraction",
      ylab ="Sample Quantile",
      geom = "line",
      main = "Wins")

# kolik advokatu jde na jistotu a podava jen nalezy? kteri to jsou?
sum(advocate_judge_stat_data$Judgement.Rate==1)
who <- unique(filter(advocate_judge_stat_data, Judgement.Rate==1)$Advocate.Known)
length(who)

# je nejaky vztah mezi poctem podanych stiznosti (cases) a uspesnosti(judgements, pripadne wins)?
cor(advocate_data$Cases, advocate_data$Judgements)
qplot(Cases, Judgements, data = advocate_data) # zajimave jsou tam videt 3 outliers
cor(successful_advocate_data$Cases, successful_advocate_data$Win.Rate)
cor(successful_advocate_data$Judgements, successful_advocate_data$Win.Rate)
qplot(Judgements, Win.Rate, data = successful_advocate_data) # je tam videt nejaka obloukovita zavislost
# TO DO overit jeste poradovou zavislost
# TO DO overit lidi, co maji 20 nalezu a ve vsech vyhrali

# a co vztah delky advokatni praxe a uspesnosti
# a co treba nejaka znamost jmena (jak vycislist?google?) a delka rizeni
# lze na zaklade minulych uspechu predvidat budouci?



### filtered data
# Sladkova a Valdauf maji kratkou prumernou odbu rize (30 a 40, oproti prumeru populace 177) 
# a nizkou Judgement.Rate -> typicti kverulanti
data <- prepare_data(raw_data, cases_min = 100, cases_max = 3000)
group_by(data, Advocate.Known) %>% 
        summarize(Mean.Time = mean(Proceedings.Length),
                  Cases=length(Decision.Type),
                  Judgement.Rate=(sum(Decision.Form=="Nález"))/Cases) %>% 
        arrange(Mean.Time)
