################################################
# Load libraries and directories
################################################
library(Rceattle)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

diet_dir  <- "Data/Diet"
halibut_alk_dir <- "Data/ALKs/GOA Halibut A-L keys"


################################################
# Set up age and length bins
################################################
# Length bins in ALKs (cm)
# atf_lbin <- c(0, 10, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 75, 999) # 26
# pollock_lbin = c(0, 17, 28, 36, 43, 51, 56, 999) # 7, Length transition 3 (From Shelikof Strait survey 1992-98)
# pcod_lbin <- c(0, seq(1.5, 116.5, by = 2), 999) # 0.5 is start of first bin and 116.5-999 is last bin
pollock_lbin <- c(0, seq(12, 58, by = 2), 999)
atf_lbin = c(0,seq(10, 70, by = 2), 999)
pcod_lbin <- c(0, seq(5.5, 99.5, by = 2), 999)
halibut_lbin = c(0,seq(16, 100, by = 2), 999)

# - ALK length bins, had to do a dimension reduction above or lost data
halibut_lbin_alk = c(0,seq(16, 100, by = 1), 999)

pollock_age_bins <- c(0:9 + 0.5, 99)
atf_age_bins <- c(0:20 + 0.5, 99)
pcod_age_bins <- c(0:11 + 0.5, 99)

pollock_ages <- 1:10
atf_ages <- 1:21
pcod_ages <- 1:12
halibut_ages <- 1:30


################################################
# Build ALKs
################################################
# Load alk data
pollock <- read.csv("Data/ALKs/pollock_alk_data.csv")
atf <- read.csv("Data/ALKs/atf_alk_data.csv")
cod <- read.csv("Data/ALKs/cod_alk_data.csv")
cod <- cod[which(cod$COMMON_NAME == "Pacific cod"),]

# Convert length mm to length cm
pollock$Length <- pollock$Length..mm./10
atf$Length <- atf$Length..mm./10
cod$Length <- cod$LENGTH/10

# Cut bins
# - Length
pollock$BIN = cut(pollock$Length, breaks = pollock_lbin)
atf$BIN = cut(atf$Length, breaks = atf_lbin)
cod$BIN = cut(cod$Length, breaks = pcod_lbin)

levels(pollock$BIN) = 1:length(pollock_lbin[-1])
levels(atf$BIN) = 1:length(atf_lbin[-1])
levels(cod$BIN) = 1:length(pcod_lbin[-1])

# - Age
pollock$AgeBIN = cut(pollock$Age..years., breaks = pollock_age_bins)
atf$AgeBIN = cut(atf$Age..years., breaks = atf_age_bins)
cod$AgeBIN = cut(cod$AGE, breaks = pcod_age_bins)

levels(pollock$AgeBIN) = pollock_ages
levels(atf$AgeBIN) = atf_ages
levels(cod$AgeBIN) = pcod_ages

# Set up alk matrices
# - Empirical
# -- Pollock
pollock <- pollock[-which(is.na(pollock$AgeBIN )),]
pollock_table <- with(pollock,table(BIN,AgeBIN))
alk_pollock <- prop.table(pollock_table,margin=1)
alk_pollock[is.na(alk_pollock)] <- 0

# -- ATF
# --- Males
atf_males <- atf[-which(is.na(atf$AgeBIN )),]
atf_males <- atf_males[which(atf_males$Sex == "Male"),]
atf_males_table <- with(atf_males,table(BIN,AgeBIN))
alk_atf_males <- prop.table(atf_males_table,margin=1)
alk_atf_males[is.na(alk_atf_males)] <- 0

# --- Females
atf_females <- atf[-which(is.na(atf$AgeBIN )),]
atf_females <- atf_females[which(atf_females$Sex == "Female"),]
atf_females_table <- with(atf_females,table(BIN,AgeBIN))
alk_atf_females <- prop.table(atf_females_table,margin=1)
alk_atf_females[is.na(alk_atf_females)] <- 0

# -- COD
cod <- cod[-which(is.na(cod$AgeBIN )),]
cod_table <- with(cod,table(BIN,AgeBIN))
alk_cod <- prop.table(cod_table,margin=1)
alk_cod[is.na(alk_cod)] <- 0

# -- Halibut
# Ages 2-15+, Lengths 15-100 cm
halibut_alk_files <- list.files(halibut_alk_dir, pattern = "*.dat")
halibut_alk_list <- lapply(paste0(halibut_alk_dir,"/",halibut_alk_files), read.table)
halibut_alk_list <- c(list(halibut_alk_list[[1]]), halibut_alk_list) # Assuming 1990 is the same as 1993
halibut_alk_list <- c(halibut_alk_list[1:4], list(halibut_alk_list[[5]]), halibut_alk_list[5:12], list(halibut_alk_list[[12]])) # Assuming 2001 is the same as 2003 and 2019 is the same as 2017
years <- c(1990, 1993, 1996, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
for(i in 1:length(halibut_alk_list)){
  halibut_alk_list[[i]] <- as.data.frame((halibut_alk_list[[i]])) # Make ages x lengths
  halibut_alk_list[[i]] <- cbind(halibut_alk_list[[i]][,1], halibut_alk_list[[i]]) # Make age 1
  for(age in 16:30){
    halibut_alk_list[[i]] <- cbind(halibut_alk_list[[i]], halibut_alk_list[[i]][,15]) # Make age 16:30 the same as 15
  }
  colnames(halibut_alk_list[[i]]) <- c(1:30) # Should be 2:15
  halibut_alk_list[[i]]$PredBINalk <- 1:nrow(halibut_alk_list[[i]])
  halibut_alk_list[[i]]$Yr = years[i]
}
halibut_alk_wide <- do.call(rbind.data.frame, halibut_alk_list)
halibut_alk_wide$Pred_species <- "P_Halibut"
halibut_alk_wide$PredSex <- 1 # Females
halibut_alk_tall <- halibut_alk_wide %>% pivot_longer(1:30, names_to = "PredAge",values_to = "PredAgeLengthProb")

# Add males
halibut_alk_tall_males <- halibut_alk_tall
halibut_alk_tall_males$PredSex <- 2
halibut_alk_tall <- rbind(halibut_alk_tall, halibut_alk_tall_males)


# -- Save
dim(alk_pollock)
colnames(alk_pollock) <- pollock_ages
rownames(alk_pollock) <- pollock_lbin[-1]

colnames(alk_cod) <- pcod_ages
rownames(alk_cod) <- pcod_lbin[-1]

colnames(alk_atf_females) <- atf_ages
rownames(alk_atf_females) <- atf_lbin[-1]

colnames(alk_atf_males) <- atf_ages
rownames(alk_atf_males) <- atf_lbin[-1]

write_xlsx(list(pollock = as.data.frame(alk_pollock), cod = as.data.frame(alk_cod), atf_fem = as.data.frame(alk_atf_females), atf_males = as.data.frame(alk_atf_males)),
           path = "LengthToAgeGOA.xlsx")


################################################
# Diet proportion estimation
################################################
# -  Read in diet DATA
load("Data/Diet data/Raw/cpue_allsp_noObs.Rdata") # Load CPUE
load("Data/Diet data/Raw/GOAPredPreyL.Rdata") # Subset of 100 fish per year of prey lengths
# PRED_LEN is in cm # PREY_SZ1_CM is prey length in cm

load("Data/Diet data/Raw/PACIFIC COD_GOA_byPred_preyWT_noObs.Rdata") # Gravimetric composition COD
preyWT <- agg.preyWT
load("Data/Diet data/Raw/WALLEYE POLLOCK_GOA_byPred_preyWT_noObs.Rdata") # Gravimetric composition POLLOCK
preyWT <- rbind(preyWT, agg.preyWT)
load("Data/Diet data/Raw/ARROWTOOTH FLOUNDR_GOA_byPred_preyWT_noObs.Rdata") # Gravimetric composition ATF
preyWT <- rbind(preyWT, agg.preyWT)
load("Data/Diet data/Raw/PACIFIC HALIBUT_GOA_byPred_preyWT_noObs.Rdata") # Gravimetric composition HALIBUT
preyWT <- rbind(preyWT, agg.preyWT)


#### Update species names
# - Prey spp wt
preyWT$Pred_species <- ifelse(preyWT$Species_name == "Arrow or Kam", "Arrowtooth", # All Arrow or Kam in GOA is Arrow per Kerim
                              ifelse(preyWT$Species_name == "Arrowtooth" , "Arrowtooth",
                                     ifelse(preyWT$Species_name == "W. Pollock", "W_Pollock",
                                            ifelse(preyWT$Species_name == "P. Cod", "P_Cod",
                                                   ifelse(preyWT$Species_name == "P. Halibut", "P_Halibut", NA)))))


# - CPUE
cpue$Pred_species <- ifelse(cpue$CN == "walleye pollock", "W_Pollock",
                            ifelse(cpue$CN == "Pacific cod", "P_Cod",
                                   ifelse(cpue$CN == "arrowtooth flounder", "Arrowtooth",
                                          ifelse(cpue$CN == "Pacific halibut", "P_Halibut", NA))))

# - Pred/prey length
GOAPredPreyL$Pred_species <- ifelse(GOAPredPreyL$CEATTLE_PRED == "Arrow or Kam", "Arrowtooth",
                                    ifelse(GOAPredPreyL$CEATTLE_PRED == "Arrowtooth" , "Arrowtooth",
                                           ifelse(GOAPredPreyL$CEATTLE_PRED == "W. Pollock", "W_Pollock",
                                                  ifelse(GOAPredPreyL$CEATTLE_PRED == "P. Cod", "P_Cod",
                                                         ifelse(GOAPredPreyL$CEATTLE_PRED == "P. Halibut", "P_Halibut", NA)))))

GOAPredPreyL$Prey_species <- ifelse(GOAPredPreyL$CEATTLE_PREY == "Arrow or Kam", "Arrowtooth",
                                    ifelse(GOAPredPreyL$CEATTLE_PREY == "Arrowtooth" , "Arrowtooth",
                                           ifelse(GOAPredPreyL$CEATTLE_PREY == "W. Pollock", "W_Pollock",
                                                  ifelse(GOAPredPreyL$CEATTLE_PREY == "P. Cod", "P_Cod",
                                                         ifelse(GOAPredPreyL$CEATTLE_PREY == "P. Halibut", "P_Halibut", NA)))))


####  Convert fork length measurements (mm to cm):
# - cpue
cpue$PredBIN = as.numeric(as.character(cpue$BIN))
cpue$PredLength = cpue$PredBIN / 10

# - Proportion of prey sp in pred
preyWT$PredLength = preyWT$PredL # In cm

# - Pred length and prey length
GOAPredPreyL$PredLength = GOAPredPreyL$PRED_LEN # In cm

#FIXME prey length is in mm standard length, need to convert to fork length. Maybe it already is?
GOAPredPreyL$PreyLength = GOAPredPreyL$PREY_SZ1 / 10 # mm to cm


#### Aggregate data to length bins:
# - cpue
cpue$FLBin_halibut = cut(cpue$PredLength, breaks = halibut_lbin)
cpue$FLBin_atf = cut(cpue$PredLength, breaks = atf_lbin)
cpue$FLBin_pollock = cut(cpue$PredLength, breaks = pollock_lbin)
cpue$FLBin_cod = cut(cpue$PredLength, breaks = pcod_lbin)

levels(cpue$FLBin_halibut) = 1:length(halibut_lbin)
levels(cpue$FLBin_atf) = 1:length(atf_lbin)
levels(cpue$FLBin_pollock) = 1:length(pollock_lbin)
levels(cpue$FLBin_cod) = 1:length(pcod_lbin)

# -- Make a generic "PredBIN"
cpue$PredBIN = ifelse(cpue$Pred_species == "P_Halibut", cpue$FLBin_halibut,
                      ifelse(cpue$Pred_species == "P_Cod", cpue$FLBin_cod,
                             ifelse(cpue$Pred_species == "W_Pollock", cpue$FLBin_pollock,
                                    ifelse(cpue$Pred_species == "Arrowtooth", cpue$FLBin_atf, NA))))


cpue = cpue %>% group_by(STRATUM, VESSEL, CRUISE, HAUL, YEAR, Pred_species, PredBIN) %>%
  summarise( NUM_KM2 = sum(NUM_KM2))

# - Proportion of prey sp in pred-at-length
preyWT$FLBin_halibut = cut(preyWT$PredLength, breaks = halibut_lbin)
preyWT$FLBin_atf = cut(preyWT$PredLength, breaks = atf_lbin)
preyWT$FLBin_pollock = cut(preyWT$PredLength, breaks = pollock_lbin)
preyWT$FLBin_cod = cut(preyWT$PredLength, breaks = pcod_lbin)

levels(preyWT$FLBin_halibut) = 1:length(halibut_lbin)
levels(preyWT$FLBin_atf) = 1:length(atf_lbin)
levels(preyWT$FLBin_pollock) = 1:length(pollock_lbin)
levels(preyWT$FLBin_cod) = 1:length(pcod_lbin)

# -- Make a generic "PredBIN"
preyWT$PredBIN = ifelse(preyWT$Pred_species == "P_Halibut", preyWT$FLBin_halibut,
                        ifelse(preyWT$Pred_species == "P_Cod", preyWT$FLBin_cod,
                               ifelse(preyWT$Pred_species == "W_Pollock", preyWT$FLBin_pollock,
                                      ifelse(preyWT$Pred_species == "Arrowtooth", preyWT$FLBin_atf, NA))))



# -- Save only columns we care about
preyWT$Arrowtooth <- preyWT$Arrowtooth + preyWT$`Arrow or Kam` # Per kerim, not much Kam in GOA
preyWT <- subset(preyWT, select = c("Vessel", "Cruise", "Haul", "Yr", "Strata", "PredBIN", "Pred_species", "Obs_TWT", "Arrowtooth" , "W. Pollock", "P. Cod", "P. Halibut"))

# -- Rename column names
colnames(preyWT) <- c("Vessel", "Cruise", "Haul", "Yr", "Strata", "PredBIN", "Pred_species", "Obs_TWT", "Arrowtooth" , "W_Pollock", "P_Cod", "P_Halibut")


# - Pred length and prey length
# -- For predators
GOAPredPreyL$FLBin_halibut = cut(GOAPredPreyL$PredLength, breaks = halibut_lbin)
GOAPredPreyL$FLBin_atf = cut(GOAPredPreyL$PredLength, breaks = atf_lbin)
GOAPredPreyL$FLBin_pollock = cut(GOAPredPreyL$PredLength, breaks = pollock_lbin)
GOAPredPreyL$FLBin_cod = cut(GOAPredPreyL$PredLength, breaks = pcod_lbin)

levels(GOAPredPreyL$FLBin_halibut) = 1:length(halibut_lbin)
levels(GOAPredPreyL$FLBin_atf) = 1:length(atf_lbin)
levels(GOAPredPreyL$FLBin_pollock) = 1:length(pollock_lbin)
levels(GOAPredPreyL$FLBin_cod) = 1:length(pcod_lbin)

# -- Make a generic "PredBIN"
GOAPredPreyL$PredBIN = ifelse(GOAPredPreyL$Pred_species == "P_Halibut", GOAPredPreyL$FLBin_halibut,
                              ifelse(GOAPredPreyL$Pred_species == "P_Cod", GOAPredPreyL$FLBin_cod,
                                     ifelse(GOAPredPreyL$Pred_species == "W_Pollock", GOAPredPreyL$FLBin_pollock,
                                            ifelse(GOAPredPreyL$Pred_species == "Arrowtooth", GOAPredPreyL$FLBin_atf, NA))))

# -- For prey
# Convert SL to FL (Cod and ATF assumed the same)
# Weights also need to be converted because they were based of a SL ^ b rather than a FL ^ b, where paremeters were estimated from RACE W ~ FL data
# -- Pollock SL = 92.6% FL
GOAPredPreyL$PredLength[which(GOAPredPreyL$Prey_species == "W_Pollock")] <- GOAPredPreyL$PreyLength[which(GOAPredPreyL$Prey_species == "W_Pollock")] / 0.926
GOAPredPreyL$preyWt_kg[which(GOAPredPreyL$Prey_species == "W_Pollock")] <- GOAPredPreyL$LW_a[which(GOAPredPreyL$Prey_species == "W_Pollock")] * GOAPredPreyL$PreyLength[which(GOAPredPreyL$Prey_species == "W_Pollock")] ^ GOAPredPreyL$LW_b[which(GOAPredPreyL$Prey_species == "W_Pollock")]

# -- Halibut
GOAPredPreyL$PredLength[which(GOAPredPreyL$Prey_species == "P_Halibut")] <- 2.353334 + GOAPredPreyL$PreyLength[which(GOAPredPreyL$Prey_species == "P_Halibut")] * 1.059473 # From Fisheries, Aquatic Science, and Technology Laboratory, Unpublished Data
GOAPredPreyL$preyWt_kg[which(GOAPredPreyL$Prey_species == "P_Halibut")] <- GOAPredPreyL$LW_a[which(GOAPredPreyL$Prey_species == "P_Halibut")] * GOAPredPreyL$PreyLength[which(GOAPredPreyL$Prey_species == "P_Halibut")] ^ GOAPredPreyL$LW_b[which(GOAPredPreyL$Prey_species == "P_Halibut")]

# - Convert to bins
GOAPredPreyL$FLBin_halibut_prey = cut(GOAPredPreyL$PreyLength, breaks = halibut_lbin)
GOAPredPreyL$FLBin_atf_prey = cut(GOAPredPreyL$PreyLength, breaks = atf_lbin)
GOAPredPreyL$FLBin_pollock_prey = cut(GOAPredPreyL$PreyLength, breaks = pollock_lbin)
GOAPredPreyL$FLBin_cod_prey = cut(GOAPredPreyL$PreyLength, breaks = pcod_lbin)

levels(GOAPredPreyL$FLBin_halibut_prey) = 1:length(halibut_lbin)
levels(GOAPredPreyL$FLBin_atf_prey) = 1:length(atf_lbin)
levels(GOAPredPreyL$FLBin_pollock_prey) = 1:length(pollock_lbin)
levels(GOAPredPreyL$FLBin_cod_prey) = 1:length(pcod_lbin)

# - Make a generic "PredBIN"
GOAPredPreyL$PreyBIN = ifelse(GOAPredPreyL$Prey_species == "P_Halibut", GOAPredPreyL$FLBin_halibut_prey,
                              ifelse(GOAPredPreyL$Prey_species == "P_Cod", GOAPredPreyL$FLBin_cod_prey,
                                     ifelse(GOAPredPreyL$Prey_species == "W_Pollock", GOAPredPreyL$FLBin_pollock_prey,
                                            ifelse(GOAPredPreyL$Prey_species == "Arrowtooth", GOAPredPreyL$FLBin_atf_prey, NA))))


#### Exclude data from 1981, 1984 and 1987 (survey methods were standardized in 1990):
cpue = subset(cpue, YEAR >= 1990)
preyWT = subset(preyWT, Yr >= 1990)
GOAPredPreyL = subset(GOAPredPreyL, YEAR >= 1990)

#### Remove all empty stomachs:
preyWT = subset(preyWT, Obs_TWT > 0) # remove hidden empties


#### Subset to management areas (610 to 659): Likely biased
# - Pollock model 610 to 640
# - Cod model 610 to 659
# - ATF 610 to 650


#### Numerical estimator of relative density (cpue)
# - Get area of each stratum
statum_area = data.frame(STRATUM = c(10, 11, 12, 13, 20, 21, 22, 30, 31, 32, 33, 35, 40, 41, 50, 110, 111, 112, 120, 121, 122, 130, 131, 132, 133, 134, 140, 141, 142, 143, 150, 151, 210, 220, 221, 230, 231, 232, 240, 241, 250, 251, 310, 320, 330, 340, 341, 350, 351, 410, 420, 430, 440, 450, 510, 520, 530, 540, 550),
                         AreaKM2 = c(833, 13681, 6876, 12399, 7941, 7302, 10792, 5766, 15401, 9887, 5260, 2200, 9947, 6714, 6546, 4245, 8154, 2278, 11104, 7735, 5011, 7912, 7336, 10981, 12077, 5026, 7346, 5277, 9032, 7728, 4196, 6888, 2788, 10018, 1528, 6659, 1623, 3208, 3043, 2127, 1125, 3927, 2531, 1604, 2912, 1107, 1521, 2344, 733, 2006, 1953, 1745, 1469, 1033, 1937, 3066, 3494, 1887, 1206))

# - Sum NUM_KM2 across length bins
cpue_aggregate = cpue %>% group_by(STRATUM, VESSEL, CRUISE, HAUL, YEAR, Pred_species) %>%
  summarise( AGG_NUM_KM2 = sum(NUM_KM2))

# - Calculate average density per strata
cpue_aggregate = merge(cpue_aggregate, statum_area, by = "STRATUM") # Merge with area

relative_density = cpue_aggregate %>% group_by(STRATUM, YEAR, Pred_species) %>%
  summarise( relative_density = mean(AGG_NUM_KM2) * mean(AreaKM2)) # Multiply average cpue across length bins per strata by stratum area

annual_density = relative_density %>% group_by(YEAR, Pred_species) %>%
  summarise( Annual_density = sum(relative_density)) # Multiply average cpue across length bins per strata by stratum area
colnames(annual_density) <- c("Yr", "Pred_species", "Annual_density")


#### Weighting factor of length subsampling (preyWT and cpue)
# - Calculate proportions of fish subsampled within each size PredBIN, year, and haul (food habits):
sumSizeDiets = preyWT %>%
  group_by(Yr, Strata, Haul, Pred_species, PredBIN) %>%
  summarise(sumFreq_Diets = length(Obs_TWT)) # Calculate number in each PredBIN

propSizeDiets = sumSizeDiets %>%
  group_by(Yr, Strata, Haul, Pred_species) %>%
  mutate(propFreq_Diets = sumFreq_Diets/sum(sumFreq_Diets)) # Calculate proportion in each PredBIN
propSizeDiets = na.omit(propSizeDiets)

# - Calculate proportions of fish subsampled within each size PredBIN, year, and haul (bottom trawl survey):
propSizeLengths = cpue %>%
  group_by(YEAR, STRATUM, HAUL, Pred_species) %>%
  mutate(propFreq_Lengths = NUM_KM2/sum(NUM_KM2))
propSizeLengths = na.omit(propSizeLengths)

# - Update names to merge
colnames(propSizeLengths) <- c("Strata","Vessel", "Cruise", "Haul", "Yr" ,"Pred_species", "PredBIN", "NUM_KM2", "propFreq_Lengths" )


# - Calculate sample weights from dividing length-based proportions of fish caught by those subsampled for gut content analysis in each survey year and grid cell:
LengthProp = merge(propSizeDiets, propSizeLengths)
LengthProp[is.na(LengthProp)] = 0

LengthProp$WT = LengthProp$propFreq_Lengths/LengthProp$propFreq_Diets
LengthProp$WT[is.infinite(LengthProp$WT)] = 0

# # - Error check (propFreq_Diets should == 0 if propFreq_Lengths == 0)
LengthPropErrorCheck = merge(propSizeDiets, propSizeLengths, all = TRUE)
LengthPropErrorCheck[is.na(LengthPropErrorCheck)] = 0
error_check <- LengthPropErrorCheck[which(LengthPropErrorCheck$propFreq_Lengths == 0 & LengthPropErrorCheck$propFreq_Diets > 0),]
table(error_check$Pred_species)/table(LengthPropErrorCheck$Pred_species)


#### Merge with relative density and length weights
# - Update names to merge
# preyWT, LengthProp, relative_density

# - Update colnames for mergine
colnames(relative_density) <- c("Strata", "Yr", "Pred_species", "RelativeDensity_N")

preyWTweighted = preyWT %>%
  left_join(LengthProp) %>%
  left_join(relative_density)

preyWTweighted = na.omit(preyWTweighted) # FIXME - Not sure about these na.omits vs setting to 0


#### Calculate average proportion of a prey sp in the stomach of a predator-at-length
# - Make prey spp, and prey wt 2 colums
preyWTweightedTall <- preyWTweighted %>% pivot_longer(c(Arrowtooth, W_Pollock, P_Cod, P_Halibut), names_to = "Prey_species", values_to = "Prey_wt")


# - Calculate proportion
propPreySppAnnual = preyWTweightedTall %>%
  group_by(Pred_species, PredBIN, Yr, Prey_species) %>%
  summarize(propPrey_num = sum(Prey_wt * WT * RelativeDensity_N),
            propPrey_den = sum( Obs_TWT * WT * RelativeDensity_N)) %>%
  mutate(propPreySpp = propPrey_num / propPrey_den)

propPreySpp = preyWTweightedTall %>%
  group_by(Pred_species, PredBIN, Prey_species) %>%
  summarize(propPrey_num = sum(Prey_wt * WT * RelativeDensity_N),
            propPrey_den = sum( Obs_TWT * WT * RelativeDensity_N)) %>%
  mutate(propPreySpp = propPrey_num / propPrey_den)


#### Calculate average proportion by weight of a prey-at-length compared to all lengths of that species in the stomach of a predator-at-length
propPreyLength = GOAPredPreyL %>%
  group_by(Pred_species, Prey_species, PredBIN, PreyBIN) %>%
  ## Calculate the numerator, summing together rows that have the same r, l, and i
  summarize(propPreyLength_num = sum(preyWt_kg)) %>%
  mutate(propPreyLength_den = sum(propPreyLength_num)) %>%
  mutate(propPreyLength = propPreyLength_num / propPreyLength_den)


#### Calculate average proportion by weight of a prey-at-length compared to prey/length of that species in the stomach of a predator-at-length
# - Merge datasets
propPrey = merge(propPreyLength, propPreySpp, all = TRUE)
propPreyAnnual <- merge(propPreyLength, propPreySppAnnual, all = TRUE)

# - Multiply proportion of sp by weight, and proportion of prey length by weight
propPrey$propPrey <- propPrey$propPreySpp * propPrey$propPreyLength
propPreyAnnual$propPrey <- propPreyAnnual$propPreySpp * propPreyAnnual$propPreyLength

propPrey[is.na(propPrey)] = 0 # Convert NAs to zeros
propPreyAnnual[is.na(propPreyAnnual)] = 0 # Convert NAs to zeros


#### Convert length bins from diet data to length bins used for diet
propPrey <- propPrey[-which(propPrey$PreyBIN == 0),] # Get rid of no prey lengths
propPreyAnnual <- propPreyAnnual[-which(propPreyAnnual$PreyBIN == 0),] # Get rid of no prey lengths

error_check2 <- propPreyAnnual[which(propPreyAnnual$Pred_species == "P_Halibut" & propPreyAnnual$Prey_species == "W_Pollock" & propPreyAnnual$Yr == 1990),]
length(unique(error_check2$propPrey[which(error_check2$propPrey > 0)])) /length(error_check2$propPrey[which(error_check2$propPrey > 0)]) # Should ideally be 1

#FIXME something going on here....
# Create data.frame of prey species, pred species, l bin diet, l bind diet prey, lbin diet alk, lbin diet pred alk
# -- Halibut ALK and Diet length bins are different
alk_lbin_diet_lbin_halibut <- data.frame(Pred_species = rep("P_Halibut", length(halibut_lbin_alk[-1])), FL = halibut_lbin_alk[1:length(halibut_lbin_alk[-1])] + 0.5,  BINalk = 1:length(halibut_lbin_alk[-1])) # - Note FL is the lower limit of the length bin
alk_lbin_diet_lbin_halibut$BIN = cut(alk_lbin_diet_lbin_halibut$FL, breaks = halibut_lbin)
levels(alk_lbin_diet_lbin_halibut$BIN) = 1:length(halibut_lbin)

# -- Cod, ATF and Pollock ALK and Diet Bins are the same
# -- Cod
alk_lbin_diet_lbin_cod <- data.frame(Pred_species = rep("P_Cod", length(pcod_lbin[-1])), FL = pcod_lbin[1:length(pcod_lbin[-1])] +0.5, BINalk = 1:length(pcod_lbin[-1])) # - Note FL is the lower limit of the length bin
alk_lbin_diet_lbin_cod$BIN = cut(alk_lbin_diet_lbin_cod$FL, breaks = pcod_lbin)
levels(alk_lbin_diet_lbin_cod$BIN) = 1:length(pcod_lbin)

# -- Pollock
alk_lbin_diet_lbin_pollock <- data.frame(Pred_species = rep("W_Pollock", length(pollock_lbin[-1])), FL = pollock_lbin[1:length(pollock_lbin[-1])] + 0.5,  BINalk = 1:length(pollock_lbin[-1])) # - Note FL is the lower limit of the length bin
alk_lbin_diet_lbin_pollock$BIN = cut(alk_lbin_diet_lbin_pollock$FL, breaks = pollock_lbin)
levels(alk_lbin_diet_lbin_pollock$BIN) = 1:length(pollock_lbin)

# -- ATF
alk_lbin_diet_lbin_atf <- data.frame(Pred_species = rep("Arrowtooth", length(atf_lbin[-1])), FL = atf_lbin[1:length(atf_lbin[-1])] + 0.5,  BINalk = 1:length(atf_lbin[-1])) # - Note FL is the lower limit of the length bin
alk_lbin_diet_lbin_atf$BIN = cut(alk_lbin_diet_lbin_atf$FL, breaks = atf_lbin)
levels(alk_lbin_diet_lbin_atf$BIN) = 1:length(atf_lbin)

# -- Combine data
alk_lbin_diet_lbin <- rbind(alk_lbin_diet_lbin_pollock, alk_lbin_diet_lbin_atf, alk_lbin_diet_lbin_cod, alk_lbin_diet_lbin_halibut)

# -- Make a pred and prey version
alk_lbin_diet_lbin_pred <- alk_lbin_diet_lbin
colnames(alk_lbin_diet_lbin_pred) <- c("Pred_species", "PredFL", "PredBINalk", "PredBIN")

alk_lbin_diet_lbin_prey <- alk_lbin_diet_lbin
colnames(alk_lbin_diet_lbin_prey) <- c("Prey_species", "PreyFL", "PreyBINalk", "PreyBIN")

# -- Make all combinations of Pred/Prey PredLength/PreyLength
alk_lbin_diet_lbin <- merge(alk_lbin_diet_lbin_pred, alk_lbin_diet_lbin_prey)
alk_lbin_diet_lbin_annual <- merge(alk_lbin_diet_lbin, data.frame(Yr = years))

# -- Merge with diet proportion
propPrey <- propPrey[,c("Pred_species", "Prey_species", "PredBIN", "PreyBIN", "propPrey")]
propPreyAnnual <- propPreyAnnual[,c("Yr", "Pred_species", "Prey_species", "PredBIN", "PreyBIN", "propPrey")]

propPreyLength <- merge(alk_lbin_diet_lbin, propPrey, all = TRUE)
propPreyLengthAnnual <- merge(alk_lbin_diet_lbin_annual, propPreyAnnual, all = TRUE)

propPreyLength[is.na(propPreyLength)] = 0 # Convert NAs to zeros
propPreyLengthAnnual[is.na(propPreyLengthAnnual)] = 0 # Convert NAs to zeros

# Reorder
propPreyLength <- propPreyLength[with(propPreyLength, order(PredBINalk, PreyBINalk)),]
propPreyLengthAnnual <- propPreyLengthAnnual[with(propPreyLengthAnnual, order(PredBINalk, PreyBINalk)),]

error_check3 <- propPreyLengthAnnual[which(propPreyLengthAnnual$Pred_species == "P_Halibut" & propPreyLengthAnnual$Prey_species == "W_Pollock" & propPreyLengthAnnual$Yr == 1990),]
length(unique(error_check3$propPrey[which(error_check3$propPrey > 0)])) /length(error_check3$propPrey[which(error_check3$propPrey > 0)]) # Should ideally be ~0.5

#### Convert length to age
# -- Get ALKs and make long
# --- Cod
alk_cod_long <- as.data.frame(alk_cod)
alk_cod_long$Species_name <- "P_Cod"
alk_cod_long$Sex <- 0

# --- Pollock
alk_pollock_long <- as.data.frame(alk_pollock)
alk_pollock_long$Species_name <- "W_Pollock"
alk_pollock_long$Sex <- 0

# --- ATF male
alk_atf_males_long <- as.data.frame(alk_atf_males)
alk_atf_males_long$Species_name <- "Arrowtooth"
alk_atf_males_long$Sex = 2

# --- ATF female
alk_atf_females_long <- as.data.frame(alk_atf_females)
alk_atf_females_long$Species_name <- "Arrowtooth"
alk_atf_females_long$Sex = 1


# -- Combine ALKs and update for merging with pred-prey data
alk_long <- rbind(alk_pollock_long, alk_cod_long, alk_atf_females_long, alk_atf_males_long)
alk_long <- alk_long[,c("Species_name","Sex", "AgeBIN", "BIN", "Freq")]
colnames(alk_long) <- c("Pred_species", "Sex","Age", "BINalk", "AgeLengthProb")
alk_long$BINalk <- as.numeric(as.character(alk_long$BINalk))

# -- Make a pred and prey version
alk_long_pred <- alk_long
colnames(alk_long_pred) <- c("Pred_species", "PredSex", "PredAge", "PredBINalk", "PredAgeLengthProb")

alk_long_prey <- alk_long
colnames(alk_long_prey) <- c("Prey_species", "PreySex", "PreyAge", "PreyBINalk", "PreyAgeLengthProb")

# -- Extend the alk for all years to merge with halibut
alk_long_pred_annual <- merge(alk_long_pred, data.frame(Yr = years), all = TRUE)
alk_long_prey_annual <- merge(alk_long_prey, data.frame(Yr = years), all = TRUE)

# -- Merge with halibut
halibut_alk_tall <- halibut_alk_tall[,c("Pred_species", "PredSex", "PredAge", "PredBINalk", "PredAgeLengthProb", "Yr")]
halibut_alk_tall_prey <- halibut_alk_tall
colnames(halibut_alk_tall_prey) <- c("Prey_species", "PreySex", "PreyAge", "PreyBINalk", "PreyAgeLengthProb", "Yr")

alk_long_pred_annual <- rbind(alk_long_pred_annual, halibut_alk_tall)
alk_long_prey_annual <- rbind(alk_long_prey_annual, halibut_alk_tall_prey)

# -- Make into character
alk_long_prey_annual$PreyAge <- as.numeric(alk_long_prey_annual$PreyAge)
alk_long_prey_annual$PreyBINalk <- as.numeric(alk_long_prey_annual$PreyBINalk)

alk_long_prey_annual$PreyAge <- as.numeric(alk_long_prey_annual$PreyAge)
alk_long_prey_annual$PreyBINalk <- as.numeric(alk_long_prey_annual$PreyBINalk)

##### Annual proportion by age
# -- Create data.frame to fill
pred_ages <- unique(alk_long_pred_annual[c("Pred_species", "PredSex", "PredAge")])
prey_ages <- unique(alk_long_prey_annual[c("Prey_species", "PreySex", "PreyAge")])
propPreyAgeAnnual <- merge(pred_ages, prey_ages, all = TRUE)
propPreyAgeAnnual <- merge(propPreyAgeAnnual, data.frame(Yr = years), all = TRUE)
propPreyAgeAnnual$propPreyAge = 0 # Initialize

for(pred in unique(alk_long_pred_annual$Pred_species)){ # Pred loop
  for(prey in unique(alk_long_prey_annual$Prey_species)){ # Prey loop

    # Get sex of alk
    pred_sexes <- as.numeric(unique(alk_long_pred_annual$PredSex[which(alk_long_pred_annual$Pred_species == pred)]))
    prey_sexes <- as.numeric(unique(alk_long_prey_annual$PreySex[which(alk_long_prey_annual$Prey_species == prey)]))

    # Get nages of pred and prey
    pred_ages <- as.numeric(unique(alk_long_pred_annual$PredAge[which(alk_long_pred_annual$Pred_species == pred)]))
    prey_ages <- as.numeric(unique(alk_long_prey_annual$PreyAge[which(alk_long_prey_annual$Prey_species == prey)]))

    # Get length bins of alk
    pred_lengths <- as.numeric(unique(alk_long_pred_annual$PredBINalk[which(alk_long_pred_annual$Pred_species == pred)]))
    prey_lengths <- as.numeric(unique(alk_long_prey_annual$PreyBINalk[which(alk_long_prey_annual$Prey_species == prey)]))

    for(pred_sex in pred_sexes){
      for(prey_sex in prey_sexes){
        for(yr in 1:length(years)){ # Year loop

          # Get pred alk values
          if(pred == "P_Halibut"){
            pred_alk = halibut_alk_list[[yr]][,halibut_ages]
          }
          if(pred == "W_Pollock"){
            pred_alk = alk_pollock
          }
          if(pred == "P_Cod"){
            pred_alk = alk_cod
          }
          if(pred == "Arrowtooth"){
            if(pred_sex == 1){
              pred_alk = alk_atf_females
            } else {
              pred_alk = alk_atf_males
            }
          }

          # Get prey alk values
          if(prey == "P_Halibut"){
            prey_alk = halibut_alk_list[[yr]][,halibut_ages]
          }
          if(prey == "W_Pollock"){
            prey_alk = alk_pollock
          }
          if(prey == "P_Cod"){
            prey_alk = alk_cod
          }
          if(prey == "Arrowtooth"){
            if(prey_sex == 1){
              prey_alk = alk_atf_females
            }
            if(prey_sex == 2){
              prey_alk = alk_atf_males
            }
          }
          prey_alk <- as.matrix(prey_alk)
          rownames(prey_alk) <- paste0("PreyLength" , prey_lengths)
          colnames(prey_alk) <- paste0("PreyAge" , prey_ages)

          pred_alk <- as.matrix(pred_alk)
          rownames(pred_alk) <- paste0("PredLength" , pred_lengths)
          colnames(pred_alk) <- paste0("PredAge" , pred_ages)


          # Get length-specific diet proportions
          prop_prey_length_tmp <- propPreyLengthAnnual[which(propPreyLengthAnnual$Pred_species == pred &
                                                            propPreyLengthAnnual$Prey_species == prey &
                                                            propPreyLengthAnnual$Yr == years[yr]),]

          prop_prey_length_mat <- matrix(prop_prey_length_tmp$propPrey, ncol = length(pred_lengths), nrow = length(prey_lengths), byrow = FALSE) # Rows =  pred_lengths, cols = prey_lengths

          colnames(prop_prey_length_mat) <- paste0("PredLength" , pred_lengths)
          rownames(prop_prey_length_mat) <- paste0("PreyLength" , prey_lengths)

          count_prey_length_mat <- matrix(1, ncol = length(pred_lengths), nrow = length(prey_lengths), byrow = FALSE) # Rows =  pred_lengths, cols = prey_lengths
          colnames(count_prey_length_mat) <- paste0("PredLength" , pred_lengths)
          rownames(count_prey_length_mat) <- paste0("PreyLength" , prey_lengths)

          # Multiply length based diet proportion by alks to get age based
          prop_prey_age_mat <- t(prey_alk) %*% prop_prey_length_mat %*% pred_alk
          count_prey_age_mat <- t(prey_alk) %*% count_prey_length_mat %*% pred_alk

          # Take weighted mean
          prop_prey_age_mat <- prop_prey_age_mat/count_prey_age_mat

          # Assign to data.frame
          propPreyAgeAnnual$propPreyAge[which(propPreyAgeAnnual$Pred_species == pred &
                                                propPreyAgeAnnual$PredSex == pred_sex &
                                                propPreyAgeAnnual$Prey_species == prey &
                                                propPreyAgeAnnual$PreySex == prey_sex &
                                                propPreyAgeAnnual$Yr == years[yr])] <- as.numeric(t(prop_prey_age_mat))

          rm(prop_prey_length_mat, prop_prey_length_tmp, prey_alk, pred_alk)
        }
      }
    }
  }
}


#### Reduce annual age specific prop to average by taking weighted average given relative density
propPreyAgeAnnual <- merge(propPreyAgeAnnual, annual_density, all = TRUE)
write.csv(propPreyAgeAnnual, file = paste0("Data/Diet data/Annual CEATTE_", Sys.Date() ,"_propPreyAge_1990-2018.csv"))
propPreyAgeAnnualShort <- propPreyAgeAnnual[which(propPreyAgeAnnual$Yr >= 1993), ]
propPreyAgeAnnualShort2 <- propPreyAgeAnnual[which(propPreyAgeAnnual$Yr >= 1996), ]


# - Take weighted mean
propPreyAge = propPreyAgeAnnual %>%
  group_by(Pred_species, PredSex, PredAge, Prey_species, PreySex, PreyAge) %>%
  summarize(propPreyAge = weighted.mean(propPreyAge, Annual_density))

propPreyAgeShort = propPreyAgeAnnualShort %>%
  group_by(Pred_species, PredSex, PredAge, Prey_species, PreySex, PreyAge) %>%
  summarize(propPreyAge = weighted.mean(propPreyAge, Annual_density))

propPreyAgeShort2 = propPreyAgeAnnualShort2 %>%
  group_by(Pred_species, PredSex, PredAge, Prey_species, PreySex, PreyAge) %>%
  summarize(propPreyAge = weighted.mean(propPreyAge, Annual_density))

# Give species numbers per ceattle
ceattle_pred = data.frame(Pred_species = c("W_Pollock", "Arrowtooth", "P_Cod", "P_Halibut"), Pred = 1:4)
ceattle_prey = data.frame(Prey_species = c("W_Pollock", "Arrowtooth", "P_Cod", "P_Halibut"), Prey = 1:4)


propPreyAge <- merge(propPreyAge, ceattle_pred, all = TRUE)
propPreyAge <- merge(propPreyAge, ceattle_prey, all = TRUE)

propPreyAgeShort <- merge(propPreyAgeShort, ceattle_pred, all = TRUE)
propPreyAgeShort <- merge(propPreyAgeShort, ceattle_prey, all = TRUE)

propPreyAgeShort2 <- merge(propPreyAgeShort2, ceattle_pred, all = TRUE)
propPreyAgeShort2 <- merge(propPreyAgeShort2, ceattle_prey, all = TRUE)


propPreyAge$Year = 0
propPreyAge$Sample_size = 20

propPreyAgeShort$Year = 0
propPreyAgeShort$Sample_size = 20

propPreyAgeShort2$Year = 0
propPreyAgeShort2$Sample_size = 20

# Reorder
propPreyAge <- propPreyAge[with(propPreyAge, order(Pred, PredAge, Prey, PreyAge)), c("Pred_species", "Prey_species", "Pred", "Prey",  "PredSex", "PreySex", "PredAge", "PreyAge", "Year", "Sample_size", "propPreyAge")]

propPreyAgeShort <- propPreyAgeShort[with(propPreyAgeShort, order(Pred, PredAge, Prey, PreyAge)), c("Pred_species", "Prey_species", "Pred", "Prey",  "PredSex", "PreySex", "PredAge", "PreyAge", "Year", "Sample_size", "propPreyAge")]

propPreyAgeShort2 <- propPreyAgeShort2[with(propPreyAgeShort2, order(Pred, PredAge, Prey, PreyAge)), c("Pred_species", "Prey_species", "Pred", "Prey",  "PredSex", "PreySex", "PredAge", "PreyAge", "Year", "Sample_size", "propPreyAge")]


# Save
write.csv(propPreyAge, file = paste0("Data/Diet data/CEATTE_", Sys.Date() ,"_propPreyAge_Avg1990-2018.csv"))
write.csv(propPreyAgeShort, file = paste0("Data/Diet data/CEATTE_", Sys.Date() ,"_propPreyAge_Avg1993-2018.csv"))
write.csv(propPreyAgeShort2, file = paste0("Data/Diet data/CEATTE_", Sys.Date() ,"_propPreyAge_Avg1996-2018.csv"))
