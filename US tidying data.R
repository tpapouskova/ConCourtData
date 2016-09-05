setwd("C:/Users/Tereza Doležalová/Dropbox/Škola/Ph.D/ConCourtData")

library(tidyr)
library(dplyr)

# date format specification
setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )

advocates <- read.csv("CAK data.csv", 
                      sep = ",",
                      header = TRUE,
                      colClasses = c("numeric", rep("character",4), "numeric", "character")
                      )
registries <- read.csv("US_from registries.csv",
                       sep = "\t",
                       header = TRUE,
                       colClasses = c("factor", "character", "Date", rep("character",5))
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
PIN <- c(rep(NA,nrow(registries)))

for (i in 1:nrow(registries)){
        if (sum(reg_sur_nam_cit[i] == adv_sur_nam_cit, na.rm = TRUE) == 1) {
                unique[i] <- "by surname, name and city"
                inCAK[i] <- "yes"
                PIN[i] <- advocates$PIN[reg_sur_nam_cit[i] == adv_sur_nam_cit]
        }
        if (sum(reg_sur_nam[i] == adv_sur_nam, na.rm = TRUE) == 1) {
                unique[i] <- "by surname and name"
                inCAK[i] <- "yes"
                PIN[i] <- advocates$PIN[reg_sur_nam[i] == adv_sur_nam]
        }
        if (sum(reg_sur[i] == adv_sur) == 1) {
                registries$name[i] <- advocates$name[adv_sur == reg_sur[i]]
                unique[i] <- "by surname"
                inCAK[i] <- "yes"
                PIN[i] <- advocates$PIN[reg_sur[i] == adv_sur]
        }
        if (sum(reg_sur[i] == adv_sur) == 0){
                inCAK[i] <- "no"
        }
}

registries_uniqueness <- cbind(registries,unique,inCAK, PIN) %>% 
        transmute(RegSign=RegistrySign, PropDate=ProposalDate1,
                  surname=surname, name=name,inCAK=inCAK,unique=unique, PIN=PIN)


# new date format specification
setAs("character","myDate", function(from) as.Date(from, format="%d.%m.%Y") )

database_raw <- read.csv("US_from database.csv",
                     sep = "\t",
                     strip.white=TRUE,
                     header = TRUE,
                     colClasses = c("factor", "character", "myDate", rep("character",4))
                     ) 
database_all <- database_raw[(database_raw$surname != "")  & !is.na(database_raw$surname),]

dummy1 <- c(rep("by PIN", nrow(database_all[database_all$IC != "",])))
dummy2 <- c(rep("yes", nrow(database_all[database_all$IC != "",])))
database_ready <- cbind (database_all[database_all$IC != "",],dummy1, dummy2)%>%
        transmute(RegSign=RegistrySign, PropDate=ProposalDate1,
                  surname=surname, name=FirstName,
                  inCAK=dummy2,
                  unique=dummy1, 
                  PIN=IC)

database_to_impute <- database_all[database_all$IC == "",]
dat_sur = database_to_impute$surname
dat_sur_nam = paste(database_to_impute$surname, database_to_impute$FirstName)
inCAK2 <- c(rep(NA,nrow(database_to_impute)))
unique2 <- c(rep(NA,nrow(database_to_impute)))
PIN2 <- c(rep(NA,nrow(database_to_impute)))
for (i in 1:nrow(database_to_impute)){
        if (sum(dat_sur_nam[i] == adv_sur_nam, na.rm = TRUE) == 1) {
                unique2[i] <- "by surname and name"
                inCAK2[i] <- "yes"
                PIN2[i] <- advocates$PIN[dat_sur_nam[i] == adv_sur_nam]
        }
        if (sum(dat_sur[i] == adv_sur) == 1) {
                unique2[i] <- "by surname"
                inCAK2[i] <- "yes"
                PIN2[i] <- advocates$PIN[dat_sur[i] == adv_sur]
        }
        if (sum(dat_sur[i] == adv_sur) == 0){
                inCAK2[i] <- "no"
        }
}

database_uniqueness <- cbind(database_to_impute,unique2,inCAK2, PIN2) %>% 
        transmute(RegSign=RegistrySign, PropDate=ProposalDate1,
                  surname=surname, name=FirstName,inCAK=inCAK2,unique=unique2, PIN=PIN2)


# new date format specification
setAs("character","myDate", function(from) as.Date(from, format="%Y-%m-%d") )

nalus <- read.csv("US_from NALUS.csv",
                       sep = ",",
                       header = TRUE,
                       colClasses = c("character", "myDate", rep("character",4))
)

nal_help <- separate(nalus, city, c("city", "city_details"), sep = " ", remove = TRUE, extra = "drop")
nal_sur_nam_cit = c(NA,rep(nrow(nalus)))
for (i in 1:nrow(nalus)) {
        if ((nalus$name[i] != "") && (nal_help$city[i] != "")){
                nal_sur_nam_cit[i] <- paste(nalus$surname[i], nalus$name[i], nal_help$city[i])
        }
}
nal_sur_nam = paste(nalus$surname, nalus$name)
nal_sur = nalus$surname
inCAK3 <- c(rep(NA,nrow(nalus)))
unique3 <- c(rep(NA,nrow(nalus)))
PIN3 <- c(rep(NA,nrow(nalus)))
for (i in 1:nrow(nalus)){
        if (sum(nal_sur_nam_cit[i] == adv_sur_nam_cit, na.rm = TRUE) == 1) {
                unique[i] <- "by surname, name and city"
                inCAK[i] <- "yes"
                PIN[i] <- advocates$PIN[nal_sur_nam_cit[i] == adv_sur_nam_cit]
        }
        if (sum(nal_sur_nam[i] == adv_sur_nam, na.rm = TRUE) == 1) {
                unique3[i] <- "by surname and name"
                inCAK3[i] <- "yes"
                PIN3[i] <- advocates$PIN[nal_sur_nam[i] == adv_sur_nam]
        }
        if (sum(nal_sur[i] == adv_sur) == 0){
                inCAK2[i] <- "no"
        }
}

nalus_uniqueness <- cbind(nalus,unique3,inCAK3, PIN3) %>% 
        transmute(RegSign=RegSign, PropDate=PropDate,
                  surname=surname, name=name,inCAK=inCAK3,unique=unique3, PIN=PIN3)

US_data <- rbind(registries_uniqueness, database_uniqueness, database_ready, nalus_uniqueness)

write.csv(US_data,"US data.csv", row.names = F)
