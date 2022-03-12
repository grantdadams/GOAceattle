library(Rceattle)
library(readxl)

################################################
# Data and base models
################################################

# Pollock
mydata_pollock <- Rceattle::read_data( file = "Data/GOAceattle_pollock_single_species_1970-2018.xlsx")
# Scale n-at-age to vals
mydata_pollock$NByageFixed[,5:15] <- mydata_pollock$NByageFixed[,5:15] * 1000000
mydata_pollock$srv_biom$Observation <- mydata_pollock$srv_biom$Observation * 1000000
mydata_pollock$msmMode = 0
mydata_pollock$estDynamics = 0

# Cod and halibut
mydata_pcod_est <- Rceattle::read_data( file = "Data/GOAceattle_pcod_single_species_1977-2018.xlsx")
mydata_pcod_est$pmature[1,2:13] <- 2
mydata_pcod_est$estDynamics[1] = 0

# ATF
mydata_atf_est <- Rceattle::read_data( file = "Data/GOAceattle_arrowtooth_single_species_1961-2018.xlsx")
mydata_atf_est$estDynamics = 0

# Combine data
GOA_18_5_data <-  combine_data(data_list1 = combine_data(data_list1 = mydata_pollock, data_list2 = mydata_atf_est), data_list2 =  mydata_pcod_est)
GOA_18_5_data$msmMode <- 0
GOA_18_5_data$styr <- 1977
write_data(GOA_18_5_data, file = "Data/GOAceattle_data_1961-2018.xlsx")


