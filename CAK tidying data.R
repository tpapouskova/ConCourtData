setwd("C:/Users/Tereza Doležalová/Dropbox/Škola/Ph.D/ConCourtData")

library(dplyr)
library(tidyr)
library(stringi)

setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y") )
advocates_all <- read.csv("CAK_all.csv", sep = ",", 
                          colClasses = c("numeric", rep("character", 4), "myDate", "factor", "numeric"))
advocates_some <- read.csv("CAK_some.csv", sep = ",",
                           colClasses = c("numeric", rep("character", 6), "factor", "numeric", "myDate", "character"))
                         

advocates_all <- mutate(advocates_all,
                      helper=paste(advocates_all$Jméno,advocates_all$Příjmení,advocates_all$IČ, sep=" "))
advocates_some <- mutate(advocates_some,
                       helper=paste(advocates_some$Jméno,advocates_some$Příjmení,advocates_some$IČ, sep=" "))

advocates <- merge(advocates_all,advocates_some,by = "helper",all.x = TRUE) %>% 
        transmute(ID=helper, title_front=Titul.před.x, name=Jméno.x, surname=Příjmení.x, 
                  title_back=Titul.za.x, PIN=IČ.x, reg_no=EČ,
                  city=Adresa...obec, street=Adresa...ulice)

# decapitalizing surnames and names
advocates$surname <- stri_trans_totitle(advocates$surname)
advocates$name <- stri_trans_totitle(advocates$name)

write.csv(advocates,"CAK data.csv", row.names = FALSE)
