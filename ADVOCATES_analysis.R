source("ADVOCATES_functions.R", encoding = 'UTF-8')

crawled_data <- load_crawled_data() %>%
        filter(Decision.Date <= "2015-12-31") %>%
        filter(!duplicated(Registry.Sign, fromLast=T))

const_court_data <- load_const_court_data()

raw_data <- left_join(crawled_data, const_court_data, by = "Registry.Sign")

data_pined <- prepare_pined_data(raw_data)
length(unique(data_pined$PIN))

data_surnamed <- prepare_surnamed_data(raw_data)
length(unique(data_surnamed$Advocate.Surname))

# how complete are data across years
completness_data <- completness_in_years()
View(completness_data)


################ interesting pieces of analysis
### LOGISTIC REGRESSION
data_pined_for_logit_reg <- prepare_logit_reg(data_pined, "PIN")
data_surnamed_for_logit_reg <- prepare_logit_reg(data_surnamed, "Advocate.Surname")

# je nejaka zavislost poctu stiznosti a judgement rate daneho advokata podle IČ
data_pined_for_logit_reg$Judgement.Dummy <- as.factor(data_pined_for_logit_reg$Judgement.Dummy)
cdplot(Judgement.Dummy ~ Cases.Before, data = data_pined_for_logit_reg)
pin_logit_reg <- glm(Judgement.Dummy ~ Cases.Before, data = data_pined_for_logit_reg, family = "binomial")
summary(pin_logit_reg) # -0.0034, -0.0032.. kdyz pouzito data_surname_logit_reg
pin_plot_data <- data.frame(Cases.Before = 0:900)
pin_plot_data$Judgement.Probability <- predict(pin_logit_reg, newdata = pin_plot_data, type = "response")
ggplot(pin_plot_data, aes(x = Cases.Before, y = Judgement.Probability)) + geom_line(size=1)

# je nejaka zavislost poctu nalezu a judgement rate daneho advokata podle IČ
data_pined_for_logit_reg$Judgement.Dummy <- as.factor(data_pined_for_logit_reg$Judgement.Dummy)
cdplot(Judgement.Dummy ~ Judgements.Before, data = data_pined_for_logit_reg)
pin_logit_reg <- glm(Judgement.Dummy ~ Judgements.Before, data = data_pined_for_logit_reg, family = "binomial")
summary(pin_logit_reg) # 0.044, kdyz pouzity data_name_logit_reg tak 0.043.., 0.019 kdyz pouzito data_surname_logit_reg
pin_plot_data <- data.frame(Judgements.Before = 0:100)
pin_plot_data$Judgement.Probability <- predict(pin_logit_reg, newdata = pin_plot_data, type = "response")
ggplot(pin_plot_data, aes(x = Judgements.Before, y = Judgement.Probability)) + geom_line(size=1)

# je nejaka zavislost poctu win a win.rate daneho advokata podle IČ
data_pined_for_logit_reg$Win.Dummy <- as.factor(data_pined_for_logit_reg$Win.Dummy)
cdplot(Win.Dummy ~ Wins.Before, data = data_pined_for_logit_reg)
pin_logit_reg <- glm(Win.Dummy ~ Wins.Before, data = data_pined_for_logit_reg, family = "binomial")
summary(pin_logit_reg) # 0.052.., kdyz pouzity data_name_logit_reg tak 0.051.., 0.027.. kdyz pouzito data_surname_logit_reg
pin_plot_data <- data.frame(Wins.Before = 0:100)
pin_plot_data$Win.Probability <- predict(pin_logit_reg, newdata = pin_plot_data, type = "response")
ggplot(pin_plot_data, aes(x = Wins.Before, y = Win.Probability)) + geom_line(size=1)



# doba rizeni je lognormalne rozlozena
qplot(log(Proceedings.Length), data=data_named, geom = "density")

# doba rizeni se zda byt zavisla na advokatovi NUTNO JESTE PODROBNEJI PROVERIT
# anova <- aov(data$Proceedings.Length ~ data$Advocate.Known)
# summary(anova)

# je pro konkretniho advokata zavisla doba rizeni na soudci, kteremu stiznost napadne?
muller <- filter(data_pined, PIN == "15286797")
anova <- aov(muller$Proceedings.Length ~ muller$Rapporteur)
summary(anova)
# je pro konkretniho advokata zavisla judgement.rate na soudci, kteremu stiznost napadne?
# je pro konkretniho advokata zavisla uspesnost nalezu na soudci, kteremu stiznost napadne?
advocate_judge_stat_data <- advocate_judge_statistics (data_named) # zajimavy je napr vztah Sokola a Wagnerove, mozna Gotz a Jurka


data_advocate_pined <-  group_by(data_pined, PIN) %>% 
        summarize (Advocate.Known = Advocate.Known,
                   Cases=length(Registry.Sign),
                   Judgements=sum(Decision.Form=="Nález"),
                   Judgement.Rate=sum(Decision.Form=="Nález")/length(Registry.Sign),
                   Wins=sum(grepl("vyhověno", Decision.Type)),
                   Win.Rate=sum(grepl("vyhověno", Decision.Type))/length(Registry.Sign),
                   Losses=sum(grepl("zamítnuto", Decision.Type)),
                   Loss.Rate=sum(grepl("zamítnuto", Decision.Type))/length(Registry.Sign))
successful_advocate_data <- filter(data_advocate_pined,Judgements>=1)   

n <- nrow(data_advocate_pined)
# kolik tak jednotlivi advokati podavaji stiznosti?
qplot((1:n - 1)/(n - 1), sort(data_advocate_pined$Cases), 
      xlab = "Sample Fraction",
      ylab ="Sample Quantile",
      geom = "line",
      main = "Cases")
# kolik tak na advokata pripada nalezu
qplot((1:n - 1)/(n - 1), sort(data_advocate_pined$Judgements), 
      xlab = "Sample Fraction",
      ylab ="Sample Quantile",
      geom = "line",
      main = "Judgements")
# a kolik vyhoveno
qplot((1:n - 1)/(n - 1), sort(data_advocate_pined$Wins), 
      xlab = "Sample Fraction",
      ylab ="Sample Quantile",
      geom = "line",
      main = "Wins")

# kolik advokatu jde na jistotu a podava jen nalezy? kteri to jsou?
sum(data_advocate_pined$Judgement.Rate==1)
data_advocate_pined[data_advocate_pined$Judgement.Rate==1,]

# a co vztah delky advokatni praxe a uspesnosti




### filtered data
# Sladkova a Valdauf maji kratkou prumernou odbu rize (30 a 40, oproti prumeru populace 177) 
# a nizkou Judgement.Rate -> typicti kverulanti
data <- prepare_named_data(raw_data, cases_min = 100, cases_max = 3000)
group_by(data, Advocate.Known) %>% 
        summarize(Mean.Time = mean(Proceedings.Length),
                  Cases=length(Decision.Type),
                  Judgement.Rate=(sum(Decision.Form=="Nález"))/Cases) %>% 
        arrange(Mean.Time)

# naivni rozdeleni si pripadu do 10 sochtliku dle delky rizeni
data_bucketed <- cases_into_buckets(data)
# vztah delky rizeni a advokata
advocate_data_bucketed <- advocate_cases_bucketed(data_bucketed)
# vztah delky rizeni a soudce
judge_data_bucketed <- judge_cases_bucketed(data_bucketed)
# vztah delky rizeni a soudce pro jednotliveho advokata
advocate_judge_data_bucketed <- advocate_judge_cases_bucketed(data_bucketed)
