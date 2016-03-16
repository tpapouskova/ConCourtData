setwd("C:/Users/Tereza Doležalová/Dropbox/Škola/Ph.D/ConCourtData")

library(tidyr)
library(dplyr)

# date format specification
setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )

advocates <- read.csv("CAK data.csv", 
                      sep = ",",
                      header = TRUE,
                      colClasses = c("character", rep("myDate",2), rep("character",6))
                      )
registries <- read.csv("US_from registries.csv",
                       sep = ";",
                       header = TRUE,
                       colClasses = c("factor", "character", "Date", rep("character",10))
                       )

reg_sur = registries$surname
adv_sur = advocates$surname


# silly changes to "city" variables which are in different formats in the two data sets
reg_help <- separate(registries, City, c("city", "city_details"), sep = " ", remove = TRUE, extra = "drop")
adv_help <- separate(advocates, city, c("city", "city_details"), sep = " ", remove = TRUE, extra = "drop")
reg_sur_cit = c(NA,rep(nrow(registries)))
for (i in 1:nrow(registries)) {
        if ((reg_help$city[i] != "") && (registries$name) == ""){
                reg_sur_cit[i] <- paste(registries$surname[i], reg_help$city[i])
        }
}
adv_sur_cit = paste(advocates$surname, adv_help$city)

reg_ini = registries$name_initial
adv_nam = advocates$name

prop_date = registries$ProposalDate1

for (i in 1:nrow(registries)){
        if (sum(reg_sur_cit[i] == adv_sur_cit,na.rm = TRUE) == 1) {
                registries$name[i] <- advocates$name[reg_sur_cit[i] == adv_sur_cit]
        }
        if (sum(grepl(paste0("^",reg_ini[i]),adv_nam[reg_sur[i] == adv_sur]))==1) {
                registries$name[i] <- grep(paste0("^",reg_ini[i]),adv_nam[reg_sur[i] == adv_sur],value = TRUE)
        }
}

reg_sur_nam = c(NA,rep(nrow(registries)))
for (i in 1:nrow(registries)) {
        if (registries$name[i] != ""){
                reg_sur_nam[i] <- paste(registries$surname[i], registries$name[i])
        }
}
adv_sur_nam = paste(advocates$surname, advocates$name)
reg_sur_nam_cit = c(NA,rep(nrow(registries)))
for (i in 1:nrow(registries)) {
        if ((registries$name[i] != "") && (reg_help$city[i] != "")){
                reg_sur_nam_cit[i] <- paste(registries$surname[i], registries$name[i], reg_help$city[i])
        }
}
adv_sur_nam_cit = paste(advocates$surname, advocates$name, adv_help$city)

inCAK <- c(rep(NA,nrow(registries)))
unique <- c(rep(NA,nrow(registries)))
for (i in 1:nrow(registries)){
        if (sum(reg_sur_nam_cit[i] == adv_sur_nam_cit, na.rm = TRUE) == 1) {
                unique[i] <- "by surname, name and city"
                inCAK[i] <- "yes"
        }
        if ((sum(reg_sur_nam_cit[i] == adv_sur_nam_cit, na.rm = TRUE) == 1) 
            && !((prop_date[i] >= advocates$active_from[reg_sur_nam_cit[i] == adv_sur_nam_cit])
                 && (prop_date[i]<= advocates$active_to[reg_sur_nam_cit[i] == adv_sur_nam_cit]))){
                inCAK[i] <- "not in the time"
        }
        if (sum(reg_sur_nam[i] == adv_sur_nam, na.rm = TRUE) == 1) {
                unique[i] <- "by surname and name"
                inCAK[i] <- "yes"
        }
        if ((sum(reg_sur_nam[i] == adv_sur_nam, na.rm = TRUE) == 1) 
            && !((prop_date[i] >= advocates$active_from[adv_sur_nam == reg_sur_nam[i]])
                 && (prop_date[i]<= advocates$active_to[adv_sur_nam == reg_sur_nam[i]]))){
                inCAK[i] <- "not in the time"
        }
        if (sum(reg_sur[i] == adv_sur) == 1) {
                registries$name[i] <- advocates$name[adv_sur == reg_sur[i]]
                unique[i] <- "by surname"
                inCAK[i] <- "yes"
        }
        if ((sum(reg_sur[i] == adv_sur) == 1) 
            && !((prop_date[i] >= advocates$active_from[adv_sur == reg_sur[i]])
               && (prop_date[i]<= advocates$active_to[adv_sur == reg_sur[i]]))){
                inCAK[i] <- "not in the time"
        }
        if (sum(reg_sur[i] == adv_sur) == 0){
                inCAK[i] <- "not at all"
        }
}

# write.csv(cbind(registries,unique,inCAK),"data z rejstriku pro US.csv")

database <- read.csv("US_from database.csv",
                     sep = ",",
                     header = TRUE,
                     colClasses = c("factor", "character", "Date", rep("character",10))
                     )

# write.csv(data,"US data.csv")