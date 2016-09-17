library(tidyr)
library(dplyr)

###############################################################################
# functions
###############################################################################

database_choose_PINed <- function (data) {
        unique <- c(rep("by PIN", nrow(data[data$PIN != "",])))
        inCAK <- c(rep("yes", nrow(data[data$PIN != "",])))
        cbind (data[data$PIN != "",], unique, inCAK)
}

impute_unPINed <- function (data) {
        data_sur = data$surname
        data_sur_nam = c(NA,rep(nrow(data)))
        for (i in 1:nrow(data)) {
                if (data$name[i] != ""){
                        data_sur_nam[i] <- paste(data$surname[i], data$name[i])
                }
        }
        data_sur_nam_cit = c(NA,rep(nrow(data)))
        if ("city" %in% colnames(data)) {
                data_help <- separate(data, city, c("city", "city_details"), sep = " ", remove = TRUE, extra = "drop")
                for (i in 1:nrow(data)) {
                        if ((data$name[i] != "") && (data_help$city[i] != "")){
                                data_sur_nam_cit[i] <- paste(data$surname[i], data$name[i], data_help$city[i])
                        }
                }
        }
        inCAK <- c(rep(NA,nrow(data)))
        unique <- c(rep(NA,nrow(data)))
        PIN <- c(rep(NA,nrow(data)))
        for (i in 1:nrow(data)){
                if (sum(data_sur_nam_cit[i] == adv_sur_nam_cit, na.rm = TRUE) == 1) {
                        unique[i] <- "by surname, name and city"
                        inCAK[i] <- "yes"
                        PIN[i] <- advocates$PIN[data_sur_nam_cit[i] == adv_sur_nam_cit]
                }
                if (sum(data_sur_nam[i] == adv_sur_nam, na.rm = TRUE) == 1) {
                        unique[i] <- "by surname and name"
                        inCAK[i] <- "yes"
                        PIN[i] <- advocates$PIN[data_sur_nam[i] == adv_sur_nam]
                }
                if (sum(data_sur[i] == adv_sur) == 1) {
                        unique[i] <- "by surname"
                        inCAK[i] <- "yes"
                        PIN[i] <- advocates$PIN[data_sur[i] == adv_sur]
                }
                if (sum(data_sur[i] == adv_sur) == 0){
                        inCAK[i] <- "no"
                }
        }
        result <- cbind(data, unique,inCAK, PIN)
        result <- result[, !duplicated(colnames(result))]
        select(result, RegSign, PropDate, surname, name, unique, inCAK, PIN)
}

load_advocates <- function (filename = "CAK data.csv", date_format = "%Y-%m-%d") {
        setAs("character","myDate", function(from) as.Date(from, format=date_format) )
        advocates <- read.csv(filename, 
                              sep = ",",
                              header = TRUE,
                              colClasses = c("numeric", rep("character",4), "numeric", "character")
        )
}

load_database <- function (filename = "US_from database.csv", date_format = "%d.%m.%Y") {
        setAs("character","myDate", function(from) as.Date(from, format=date_format) )
        database_raw <- read.csv(filename,
                 sep = "\t",
                 strip.white=TRUE,
                 header = TRUE,
                 colClasses = c("factor", "character", "myDate", rep("character",4))
        ) 
        database_raw[(database_raw$surname != "")  & !is.na(database_raw$surname),] %>%
                transmute(RegSign = RegistrySign,
                          PropDate = ProposalDate1,
                          surname = surname,
                          name = FirstName,
                          PIN = IC)
}

load_nalus <- function (filename = "US_from NALUS.csv", date_format = "%Y-%m-%d") {
        setAs("character","myDate", function(from) as.Date(from, format=date_format) )
        read.csv(filename,
                 sep = ",",
                 header = TRUE,
                 colClasses = c("character", "myDate", rep("character",4))
        )
}

load_registries <- function (filename = "US_from registries.csv", date_format = "%Y-%m-%d") {
        setAs("character","myDate", function(from) as.Date(from, format=date_format) )
        registries <- read.csv(filename,
                               sep = "\t",
                               header = TRUE,
                               colClasses = c("factor", "character", "Date", rep("character",5))) %>%
                transmute(RegSign = RegistrySign,
                          PropDate = ProposalDate1,
                          surname = surname,
                          name = name,
                          name_initial = name_initial,
                          city = City,
                          street = Street)
        reg_ini = registries$name_initial
        reg_help <- separate(registries, city, c("city", "city_details"), sep = " ", remove = TRUE, extra = "drop")
        reg_sur = registries$surname
        reg_sur_cit = c(NA,rep(nrow(registries)))
        for (i in 1:nrow(registries)) {
                if ((reg_help$city[i] != "") && (registries$name) == ""){
                        reg_sur_cit[i] <- paste(registries$surname[i], reg_help$city[i])
                }
        }
        for (i in 1:nrow(registries)){
                if (sum(reg_sur_cit[i] == adv_sur_cit,na.rm = TRUE) == 1) {
                        registries$name[i] <- advocates$name[reg_sur_cit[i] == adv_sur_cit]
                }
                if (sum(grepl(paste0("^",reg_ini[i]),adv_nam[reg_sur[i] == adv_sur]))==1) {
                        registries$name[i] <- grep(paste0("^",reg_ini[i]),adv_nam[reg_sur[i] == adv_sur],value = TRUE)
                }
        }
        registries
}

###############################################################################
# main
###############################################################################

advocates <- load_advocates()
adv_nam = advocates$name
adv_sur = advocates$surname
adv_sur_nam = paste(advocates$surname, advocates$name)
adv_help = separate(advocates, city, c("city", "city_details"), sep = " ", remove = TRUE, extra = "drop")
adv_sur_cit = paste(advocates$surname, adv_help$city)
adv_sur_nam_cit = paste(advocates$surname, advocates$name, adv_help$city)

database <- load_database()
database_ready <- database_choose_PINed (database)
database_to_impute <- database[database$PIN == "",]
database_unique_PINed <- impute_unPINed (database_to_impute)

registries <- load_registries()
registries_unique_PINed <- impute_unPINed (registries)

nalus <- load_nalus()
nalus_unique_PINed <- impute_unPINed (nalus)

US_data <- rbind(database_ready, database_unique_PINed, registries_unique_PINed, nalus_unique_PINed)

write.csv(US_data, "US data.csv", row.names = F)
