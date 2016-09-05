setwd("C:/Users/Tereza Doležalová/Dropbox/Škola/Ph.D/ConCourtData")

library(dplyr)
library(tidyr)
library(stringi)

setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y") )
# advocates enrolled from July 1, 1990
advocates_all <- read.csv("CAK_all.csv", sep = ",", 
                          colClasses = c("numeric", rep("character", 4), "myDate", "factor", "numeric"))
# another file with addresses of some advocates
advocates_some <- read.csv("CAK_some.csv", sep = ",",
                           colClasses = c("numeric", rep("character", 6), "factor", "numeric", "myDate", "character"))
# another file with advocates enrolled till December 31, 1992 (suprisingly not completely overlapping with the first) 
advocates_till_1993 <- read.csv("CAK_to 1993.csv", sep = ";",
                                colClasses = c("numeric", rep("character", 5), "factor", "numeric", "myDate", "character"))
                         

advocates_all <- transmute(advocates_all, 
                      reg_no=Evidenční.číslo, title_front=Titul.před, name=Jméno, surname=Příjmení,
                      title_back=Titul.za, PIN=IČ)
advocates_all$city <- rep(NA, nrow(advocates_all))
advocates_some <- transmute(advocates_some,
                       reg_no=EČ, title_front=Titul.před, surname=Příjmení, name=Jméno, title_back=Titul.za,
                       city=Adresa...obec, PIN=IČ)
advocates_till_1993 <- transmute(advocates_till_1993,
                         reg_no=EČ, title_front=Titul.před, surname=Příjmení, name=Jméno, title_back=Titul.za,
                         city=Adresa...obec, PIN=IČ)

advocates_merged_all <- rbind(advocates_all, advocates_some, advocates_till_1993) %>% arrange(desc(city))
# decapitalizing surnames and names
advocates_merged_all$surname <- stri_trans_totitle(advocates_merged_all$surname)
advocates_merged_all$name <- stri_trans_totitle(advocates_merged_all$name)

advocates_selector <- !duplicated(select(advocates_merged_all, name, surname, PIN))
advocates_merged <- advocates_merged_all[advocates_selector,]

write.csv(advocates_merged,"CAK data.csv", row.names = FALSE)
