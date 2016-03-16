setwd("C:/Users/Tereza Doležalová/Dropbox/Škola/Ph.D/ConCourtData")

library(dplyr)
library(tidyr)
library(stringi)

setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y") )
advocates_in <- read.csv("CAK.csv", sep = ";", 
                        colClasses = c(rep("character", 4), "myDate", "factor", rep("character", 3)))
advocates_out <- read.csv("CAK_out.csv", sep = ";",  
                         colClasses = c(rep("character", 4), "factor", "myDate"))

advocates_in <- mutate(advocates_in,
                      helper=paste(advocates_in$Jméno,advocates_in$Příjmení,advocates_in$Aktuální.stav, sep=" "))
advocates_out <- mutate(advocates_out,
                       helper=paste(advocates_out$Jméno,advocates_out$Příjmení,advocates_out$Aktuální.stav, sep=" "))

advocates <- merge(advocates_in,advocates_out,by = "helper",all.x = TRUE) %>% 
        transmute(ID=helper, active_from=aktivn.od, active_to=Platnost.od, title_front=Titul.před.x,
                      name=Jméno.x, surname=Příjmení.x, title_back=Titul.za.x, city=Posta, street=Radek1) %>%
        arrange(active_from)

# silly imputation of missing values (setting the "active_to" date to the day of analysis)
advocates$active_to[is.na(advocates$active_to)] <- Sys.Date()

# decapitalizing surnames and names
advocates$surname <- stri_trans_totitle(advocates$surname)
advocates$name <- stri_trans_totitle(advocates$name)

write.csv(advocates,"CAK data.csv", row.names = FALSE)
