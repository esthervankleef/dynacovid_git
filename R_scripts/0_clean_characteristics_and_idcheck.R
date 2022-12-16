###################################################
# DATA CLEANING
###################################################

# Author: E van Kleef
# Date: 21 October 2021

rm(list = ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./Outputs/"
OutputDirectoryDataCheck <- "../Data/Raw/To_check/"
OutputDirectoryData <- "../Data/Clean/"
OutputDirectoryDataRaw <- "../Data/Raw/"


# Source functions files
source("./R_scripts/functions.R")
source("./R_scripts/multiplot.R")

#R_LIBS_SITE="C:/Users/evankleef/OneDrive - ITG/Documenten/R/win-library/3.6"
# 
# PACKAGES = c(
#   "readxl", "writexl", "lubridate", "zoo", "ggplot2", "tidyverse", "Hmisc")
# 
# # install packages if needed
# for (pack_name in PACKAGES) {
#   if (!pack_name %in% rownames(installed.packages()))
#     install.packages(pack_name)
# }

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, stringi,Hmisc)

###################################################################
# Load in data
###################################################################

hdss_r0 = read.csv("../Data/Raw/R0_HDSS_characteristics_individuelles.csv", header=T,
                   sep=";")
hdss_r1 = read.csv("../Data/Raw/R1_HDSS_characteristics_individuelles_changement.csv", header=T,
                   sep=";")
hdss_r2 = read.csv("../Data/Raw/R2_HDSS_characteristics_individuelles_changement.csv", header=T,
                   sep=",")

# R1 Social mixing
mix_gen_r1 = read_xlsx("../Data/Raw/R1_mix_soc_adj.xlsx", sheet=1)
mix_lieu_r1 = read_xlsx("../Data/Raw/R1_mix_soc_adj.xlsx", sheet=2)
mix_contact_r1 = read_xlsx("../Data/Raw/R1_mix_soc_adj.xlsx", sheet=3)

# To check with original data (for first round social mixing, as IDs have been manually changed)
mix_gen_r1_org = read_xlsx("../Data/Raw/R1_mix_soc_kobo.xlsx", sheet=1)

# R2 social mixing
mix_gen_r2 = read_xlsx("../Data/Raw/R2_mix_soc_adj.xlsx", sheet=1)
mix_lieu_r2 = read_xlsx("../Data/Raw/R2_mix_soc_adj.xlsx", sheet=2)
mix_contact_r2 = read_xlsx("../Data/Raw/R2_mix_soc_adj.xlsx", sheet=3)

# R3 Social mixing (=R4 seroprevalence, thus call it R4)
mix_gen_r4 = read_xlsx("../Data/Raw/R3_mix_soc_kobo.xlsx", sheet=1)
mix_lieu_r4 = read_xlsx("../Data/Raw/R3_mix_soc_kobo.xlsx", sheet=2)
mix_contact_r4 = read_xlsx("../Data/Raw/R3_mix_soc_kobo.xlsx", sheet=3)


# R1 Seroprevalence
sero_r1 = read_xlsx("../Data/Raw/R1_sero_adj.xlsx", sheet=1)
sero_r1_age_sex = read_xlsx("../Data/Raw/R1_sero_adj.xlsx", sheet=2)
# Link two to make one dataset
sero_r1 = left_join(sero_r1, sero_r1_age_sex)

# R2 Seroprevalence
sero_r2 = read_xlsx("../Data/Raw/R2_sero_kobo.xlsx", sheet=1)

# R3 Seroprevalence
sero_r3 = read_xlsx("../Data/Raw/BDD DYNA 07-10-2021.xlsx", sheet=5)

# R4 Seroprevalence
sero_r4 = read_xlsx("../Data/Raw/R4_sero_kobo.xlsx", sheet=1)

# R4 Seroprevalence
sero_r5 = read_xlsx("../Data/Raw/R5_sero_kobo.xlsx", sheet=1)


# IDs de membre de manage
idmenage =  read_xlsx("../Data/Raw/new_ids_membre_de_menage.xlsx", sheet=1)

###################################################################
# Rename variables
###################################################################
# HDSS
names(hdss_r0) = tolower(names(hdss_r0))
names(hdss_r1) = tolower(names(hdss_r1))
names(hdss_r2) = tolower(names(hdss_r2))

# Sero R1
names(sero_r1) = tolower(names(sero_r1))
names(sero_r1) = gsub(" ", "_", names(sero_r1))
names(sero_r1) = gsub("'", "_", names(sero_r1))

sero_r1 = sero_r1 %>%
  rename(index ='_index'
  ) %>%
  mutate(
    index = as.numeric(index)
  )

# Sero R2
names(sero_r2) = tolower(names(sero_r2))
names(sero_r2) = gsub(" ", "_", names(sero_r2))
names(sero_r2) = gsub("'", "_", names(sero_r2))

# Sero R3
names(sero_r3) = tolower(names(sero_r3))
names(sero_r3) = gsub(" ", "_", names(sero_r3))
names(sero_r3) = gsub("'", "_", names(sero_r3))

sero_r3 = sero_r3 %>% 
  rename(#id_individuel_hdss.y = 'id_individuel_hdss',
         gender = 'sexe',
         dob = 'date_de_naissance',
         religion_other = 'autre_religion',                                                                                           
         ethnie_other = 'ethnie_(_a_préciser)',                                                                                     
         education ='niveau_d_étude',                                                                                           
         occupation_other = 'occupation_(_à_préciser)'
  )

# Sero R4
names(sero_r4) = tolower(names(sero_r4))
names(sero_r4) = gsub(" ", "_", names(sero_r4))
names(sero_r4) = gsub("'", "_", names(sero_r4))

sero_r4 = sero_r4 %>% 
  rename(#id_individuel_hdss.y = 'id_individuel_hdss',
         gender = 'sexe',
         dob = 'date_de_naissance',
         religion_other = 'autre_religion',                                                                                           
         ethnie_other = 'ethnie_(_a_préciser)',                                                                                     
         education ='niveau_d_étude',                                                                                           
         occupation_other = 'occupation_(_à_préciser)'
  )

# Sero R5
names(sero_r5) = tolower(names(sero_r5))
names(sero_r5) = gsub(" ", "_", names(sero_r5))
names(sero_r5) = gsub("'", "_", names(sero_r5))

sero_r5 = sero_r5 %>% 
  rename(#id_individuel_hdss.y = 'id_individuel_hdss',
    gender = 'sexe',
    dob = 'date_de_naissance',
    religion_other = 'autre_religion',                                                                                           
    ethnie_other = 'ethnie_a_pr_ciser',                                                                                     
    education ='niveau_d_tude',                                                                                           
    occupation_other = 'occupation_pr_ciser'
  )

# Social mixing R1
names(mix_gen_r1) = tolower(names(mix_gen_r1))
names(mix_gen_r1) = gsub(" ", "_", names(mix_gen_r1))

mix_gen_r1 = mix_gen_r1 %>%
  rename(openhds_2_individual_id = 'id_serveur',
         index ='_index'
  ) %>%
  mutate(
    index = as.numeric(index)
  )

names(mix_gen_r1_org) = tolower(names(mix_gen_r1_org))
names(mix_gen_r1_org) = gsub(" ", "_", names(mix_gen_r1_org))

mix_gen_r1_org = mix_gen_r1_org %>%
  rename(id_individuel_hdss = 'id_individuelle',
         index ='_index'
  ) %>%
  mutate(
    index = as.numeric(index)
  )

# Social mixing R2
names(mix_gen_r2) = tolower(names(mix_gen_r2))
names(mix_gen_r2) = gsub(" ", "_", names(mix_gen_r2))
mix_gen_r2 = mix_gen_r2 %>%
  rename(index ='_index',
       id_hh_members_vol = '...7'
)

# Social mixing R4
names(mix_gen_r4) = tolower(names(mix_gen_r4))
names(mix_gen_r4) = gsub(" ", "_", names(mix_gen_r4))
mix_gen_r4 = mix_gen_r4 %>%
  rename(id_individuel_hdss = 'id_individuelle',
         index ='_index'
  ) %>%
  mutate(
    index = as.numeric(index)
  )

# Membre de menage
names(idmenage) = tolower(names(idmenage))
names(idmenage) = gsub(" ", "_", names(idmenage))

idmenage = idmenage  %>% rename(
  start='...1',
  end='...2'
) %>% select(id_individuel_hdss,vrai_id,round)   

dup = idmenage$id_individuel_hdss[which(duplicated(idmenage$id_individuel_hdss))]
#View(idmenage[idmenage$id_individuel_hdss%in%dup,])
idmenage = idmenage %>%filter(!duplicated(id_individuel_hdss)) %>%
  mutate(
    openhds_2_individual_id = NA
  )
table(idmenage$round)

###################################################################
# Clean variables
###################################################################
age_labels = c("0-5","5-9","10-14","15-19","20-25","25-29","30-35","35-39","40-44","45-49",
               "50-54","55-60","60-65","65-69","70-74","75-79","80+")

hdss_r0 = hdss_r0 %>%
  mutate(
    openhds_2_dob = as.Date(as.character(openhds_2_dob),format="%d/%m/%Y"),
    age = trunc(as.numeric(difftime(Sys.Date(),openhds_2_dob,units = "weeks"))/52.25),
    age_cat = as.character(cut(age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    age_cat = ifelse(is.na(age_cat), "80+", as.character(age_cat)),
    age_cat = ifelse(is.na(age), NA, as.character(age_cat)),
    age_cat = factor(age_cat, levels=
                       c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
                         "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
                         "(65,70]" ,"(70,75]","(75,80]", "80+")),
    openhds_2_gender = case_when(
      openhds_2_gender == 1 ~ "male",
      openhds_2_gender == 2 ~ "female",
      TRUE ~ NA_character_
    ),
    caract_migrant_ethnie = case_when(
      caract_migrant_ethnie == 1 ~ "ndibu",
      caract_migrant_ethnie == 2 ~ "manianga", # In Dynacovid this is Nianga, while in HDSS Manianga. Check with Djibril 
      caract_migrant_ethnie == 3 ~ "besingombe", # In Dynacovid this is ngombe, while in HDSS Besingombe. Check with Djibril 
      caract_migrant_ethnie == 4 ~ "ntandu",
      caract_migrant_ethnie == 5 ~ "yombe",
      caract_migrant_ethnie == 6 ~ "zombo",
      caract_migrant_ethnie == 7 ~ "other",
      caract_migrant_ethnie == 9 ~ "unknown",
      TRUE ~ as.character(caract_migrant_ethnie)
    ),
    caract_migrant_ethnie = ifelse(caract_migrant_ethnie == "unknown", NA, caract_migrant_ethnie),
    caract_migrant_religion = case_when(
      caract_migrant_religion == 1 ~ "islam",
      caract_migrant_religion == 2 ~ "christian",
      caract_migrant_religion == 3 ~ "animist",
      caract_migrant_religion == 4 ~ "buddism",
      caract_migrant_religion == 5 ~ "other",
      caract_migrant_religion == 6 ~ "none",
      TRUE ~ as.character(caract_migrant_religion)
    ), # Take those religions that are listed in sero_r3
    caract_migrant_religion = ifelse(caract_migrant_religion_other %in% c("B D K","B.D.K"), "bdk",
                                     ifelse(caract_migrant_religion_other %in% c("BDM"), "bdm",
                                            ifelse(caract_migrant_religion_other %in% c("KKB"), "kkb",
                                                   ifelse(caract_migrant_religion_other %in% c("KIBANGISTE", "KIBANGUISTE","KIMANGISTE","KIMBA'GISTE",
                                                                                                "KIMBA'NGISTE","KIMBAGUISME","KIMBAGUISTE","KIMBAKUITE",
                                                                                                "KIMBANGISME","KIMBANGISTE","KIMBANGITE","KIMBANGSTE",
                                                                                                "KIMBANGUISME","KIMBANGUISTE","KIMBANGUISTE DE NKAMBA",
                                                                                                "KIMBANGUSTE","KIMBUNGUISTE"), "kimbaguiste",
                                                          ifelse(caract_migrant_religion_other %in% c("MPEVE","MPE A NLONGO",
                                                                                      "MPEVE A  NLONGO","MPEVE A LONGO",
                                                                                      "MPEVE A N'LONGO","MPEVE A NLONGA",
                                                                                      "MPEVE A NLONGO","MPEVE Ã€ NLONGO","MPEVE ALONGO",
                                                                                      "MPEVE AN'LONGO","MPEVE ANLONGO","MPEVE YA NLONGO","MPEVEA NLONGO",
                                                                                      "MPVE A NLONGO","MSK","MSK (MPEVE A NLONGO)"), "mpeve a longo", 
                                                                 ifelse(caract_migrant_religion_other %in%c("MUSILMAN","MUSULMAN"), "islam", 
                                                                        ifelse(caract_migrant_religion_other %in% c("NEO","NEO APOSTOLIQUE"),"neoapostolique", caract_migrant_religion))))))),
    caract_migrant_education = case_when(
      caract_migrant_education == 0 ~ "none",
      caract_migrant_education == 1 ~ "primary",
      caract_migrant_education == 2 ~ "secondary",
      caract_migrant_education %in% c(3,4) ~ "higher(university/specialised)",
   #   caract_migrant_education == 4 ~ "specialised(technical qualification)",
      TRUE ~ NA_character_
      ),
    caract_migrant_current_schooling = case_when(
      caract_migrant_current_schooling == 0 ~ "never",
      caract_migrant_current_schooling == 1 ~ "yes(previously)",
      caract_migrant_current_schooling == 2 ~ "yes(currently)",
      TRUE ~ as.character(caract_migrant_current_schooling)
    ),
    caract_migrant_job = case_when(
      caract_migrant_job == 0 ~ "none",
      caract_migrant_job == 1 ~ "agriculture",
      caract_migrant_job == 2 ~ "farming",
      caract_migrant_job == 3 ~ "handicrafts",
      caract_migrant_job == 4 ~ "trade",
      caract_migrant_job == 5 ~ "housekeeping",
      caract_migrant_job == 6 ~ "schoolchild",
      caract_migrant_job == 7 ~ "student",
      caract_migrant_job == 8 ~ "civil servant/teacher",
      caract_migrant_job == 9 ~ "other",
      TRUE ~ NA_character_
    ),
   caract_migrant_job = ifelse(caract_migrant_job_other %in%c("MALADIVE"), "none",
                               ifelse(caract_migrant_job_other %in% c("AGRONOMIQUE"), "agriculture", caract_migrant_job)),
    #caract_migrant_religion_other = tolower(caract_migrant_religion_other),
    caract_migrant_job_other = tolower(caract_migrant_job_other),
    caract_migrant_autre_ethnie = tolower(caract_migrant_autre_ethnie)
    )

hdss_r0$caract_migrant_current_schooling = ifelse(hdss_r0$caract_migrant_current_schooling == "NULL", NA, hdss_r0$caract_migrant_current_schooling)
table(hdss_r0$caract_migrant_current_schooling,useNA= "always")

hdss_r1 = hdss_r1 %>%
  mutate(
    openhds_2_dob = as.Date(as.character(openhds_2_dob),format="%Y-%m-%d"),
    age = trunc(as.numeric(difftime(Sys.Date(),openhds_2_dob,units = "weeks"))/52.25),
    age_cat = as.character(cut(age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    age_cat = ifelse(is.na(age_cat), "80+", as.character(age_cat)),
    age_cat = ifelse(is.na(age), NA, as.character(age_cat)),
    age_cat = factor(age_cat, levels=
                       c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
                         "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
                         "(65,70]" ,"(70,75]","(75,80]", "80+"))
  )

hdss_r2 = hdss_r2 %>%
  mutate(
    openhds_2_dob = as.Date(as.character(openhds_2_dob),format="%Y-%m-%d"),
    age = trunc(as.numeric(difftime(Sys.Date(),openhds_2_dob,units = "weeks"))/52.25),
    age_cat = as.character(cut(age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    age_cat = ifelse(is.na(age_cat), "80+", as.character(age_cat)),
    age_cat = ifelse(is.na(age), NA, as.character(age_cat)),
    age_cat = factor(age_cat, levels=
                       c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
                         "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
                         "(65,70]" ,"(70,75]","(75,80]", "80+"))
  )

sero_r3 = sero_r3 %>%
  mutate(
    gender = case_when(
      gender == "F" ~ "female",
      gender == "M" ~ "male",
      TRUE ~ NA_character_
    ),
    religion = case_when(
      religion == "CATHOLIQUE" ~ "catholic",
      religion == "PROSTESTANTE" ~ "protestant",
      religion == "MUSULMAN" ~ "islam",
      religion == "AUTRES ( A Préciser )" ~ "other",
      TRUE ~ as.character(religion)
    ),
    religion = ifelse(religion_other %in% c("BDK"), "bdk",
                      ifelse(religion_other%in%c("Aucun", "Aucune","AUCUNE"), "none",
                             ifelse(religion_other%in%c("NEO", "Néo","NÉO","NEO APOSTOLIQUE","Néo apostolique",
                                    "Néo apostolique partant","NÉOAPOSTOLIQUE"), "Neoapostolique", religion))),
    # Religion in line with hdss (i.e. catholic and protestant together)
    religion2 = case_when(
      #religion %in% c("AUTRES ( A Préciser )") ~ "other",
      religion %in% c("catholic","protestant")  ~ "christian",
      TRUE ~ as.character(religion)
    ),
    religion = tolower(religion),
    religion2 = tolower(religion2),
    ethnie = case_when(
      ethnie == "Autres ( A préciser )" ~ "other",
      ethnie == "NGOMBE" ~ "besingombe", # Change for now to make aligned with HDSS, check with Djibril to confirm
      ethnie == "NIANGA" ~ "manianga", # Change for now to make aligned with HDSS, check with Djibril to confirm
      TRUE ~ as.character(ethnie)
    ),
    ethnie = tolower(ethnie),
    education = case_when(
      education == "PRIMAIRE" ~ "primary",
      education == "SECONDAIRE" ~ "secondary",
      education == "SANS INSTRUCTION" ~ "none",
      education == "UNIVERISITAIRE" ~ "higher(university/specialised)",
      TRUE ~ as.character(education)
    ),
    occupation = case_when(
      occupation == "Autres ( à préciser)" ~ "other",
      occupation == "Agriculteur (trice)" ~ "agriculture",
      occupation == "Enseignant(e)" | occupation_other == "Enseignemente" ~ "civil servant/teacher",
      occupation_other %in% c("ELEVE", "Élève") ~ "schoolchild",
      occupation_other %in% c("ÉTUDIANT", "ETUDIANTE") ~ "student",
      TRUE ~ NA_character_
    ),
    occupation = ifelse(occupation_other %in% c("ELEVE", "Élève"), "schoolchild",
                        ifelse(occupation_other %in% c("ÉTUDIANT", "ETUDIANTE"), "student",
                               ifelse(occupation_other %in% c("Commerce", "COMMERCENT", "COMMERCENTE"), "trade",
                                      ifelse(occupation_other %in% c("Aucun","Aucune","AUCUN", "Nul", "Chômeur","Maladif"),"none",
                                                                     ifelse(occupation_other %in% c("AGRONOME","AGronome"), "agriculture", occupation)))))
  )

sero_r1 = sero_r1 %>%
  mutate(
  sex = case_when(
    sex == "F" ~ "female",
    sex == "M" ~ "male")#,
    # age = trunc(as.numeric(difftime(Sys.Date(),dob,units = "weeks"))/52.25),
    # age_cat = as.character(cut(age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    # age_cat = ifelse(is.na(age_cat), "80+", as.character(age_cat)),
    # age_cat = ifelse(is.na(age), NA, as.character(age_cat)),
    # age_cat = factor(age_cat, levels=
    #                    c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
    #                      "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
    #                      "(65,70]" ,"(70,75]","(75,80]", "80+"))
  )
########################################################
# Select variables of interest and remove duplicates
########################################################


# HDSS DATA
#################################

hdss_r0_sel = hdss_r0 %>%
  filter(!duplicated(openhds_2_individual_id)) %>%
  select(x_creation_date,
         openhds_2_individual_id, 
         openhds_household_id,
         openhds_2_dob,
         age,
         age_cat,
         openhds_2_gender,
         caract_migrant_ethnie,
         caract_migrant_autre_ethnie,
         caract_migrant_religion,
         caract_migrant_religion_other,
         caract_migrant_education,
         caract_migrant_current_schooling,
         caract_migrant_job,
         caract_migrant_job_other
  )

# Select variables of interest and remove duplicates hdss_r1
hdss_r1_sel = hdss_r1 %>%
  filter(!duplicated(openhds_2_individual_id)) %>%
  select(x_creation_date,
         openhds_2_individual_id, 
         openhds_household_id,
         openhds_2_dob,
         age,
         age_cat,
         openhds_2_gender,
         present_ethnie,
         present_ethnie_unk,
         present_autre_ethnie,
         present_religion,
         present_religion_unk,
         present_autre_religion,
         present_educ_educ_ans_entete,
         present_educ_educ_unk_entete,
         present_job_job_change_date_chmtjob2019,
         present_job_job_unk_entete
  )

# Select variables of interest and remove duplicates hdss_r2
hdss_r2_sel = hdss_r2 %>%
  filter(!duplicated(openhds_2_individual_id)) %>%
  select(x_creation_date,
         openhds_2_individual_id, 
         openhds_household_id,
         openhds_2_dob,
         age,
         age_cat,
         openhds_2_gender,
         present_ethnie,
         present_ethnie_unk,
         present_autre_ethnie,
         present_religion,
         present_religion_unk,
         present_autre_religion,
         present_educ_educ_ans_entete,
         present_educ_educ_unk_entete,
         present_job_job_change_date_chmtjob2019,
         present_job_job_unk_entete
  )

# Individual characteristics as collected in R3 of seroprevalence
sero_r3_sel = sero_r3 %>%
  filter(!is.na(id_individuel_hdss)) %>%
  select(id_de_la_parcelle,
         id_individuel_hdss,
         religion,  
         religion2,
         religion_other,                                                                                           
         ethnie,                                                                                                   
         ethnie_other,                                                                                     
         education,                                                                                           
         occupation,                                                                                               
         occupation_other
  )



###################################################################
# Check for duplicates
###################################################################

# SOCIAL MIXING DATA
#################################

# How many duplicated R1
#################################
# soc mix with cleaned ID data
length(unique(mix_gen_r1$id_individuel_hdss)) # 1590 --> 29 dup

# soc mix original data
length(unique(mix_gen_r1_org$id_individuel_hdss)) # 1609 --> 11 dup

table(is.na(mix_gen_r1$id_individuel_hdss)) # No missing
table(is.na(mix_gen_r1_org$id_individuel_hdss)) # No missing

# Which are the duplicated
#################################

# soc mix with cleaned ID data
dup = mix_gen_r1$id_individuel_hdss[duplicated(mix_gen_r1$id_individuel_hdss)] # 29 duplicates
dup_openhdss = mix_gen_r1$openhds_2_individual_id[!is.na(mix_gen_r1$openhds_2_individual_id)&duplicated(mix_gen_r1$openhds_2_individual_id)] # 54 duplicates

# soc mix original data
dup_org = mix_gen_r1_org$id_individuel_hdss[duplicated(mix_gen_r1_org$id_individuel_hdss)] # 11 duplicates
## More duplicates in cleaned data for id_individual_hdss ##

# Check number of unique IDs in social mixing R1
num_unique_openhdss = length(unique(mix_gen_r1$openhds_2_individual_id[!is.na(mix_gen_r1$openhds_2_individual_id)])) # 1394
num_unique = length(unique(mix_gen_r1$id_individuel_hdss[!is.na(mix_gen_r1$openhds_2_individual_id)])) # 1422
## More duplicates when using the OpenHDSS ID vs id_individual_hdss ##


# Check where difference is coming from between orig and cleaned data
########################################################################
# Take from original data id_individuel, index, and id_parcel
org_index_id = mix_gen_r1_org %>% select("id_individuel_hdss","index", "id_parcelle")

# Link by index from both datasets
mix_gen_r1 = left_join(org_index_id, mix_gen_r1, by="index")

# Check when not same id_individuelle_hdss
mix_gen_r1$no_corresp = ifelse(mix_gen_r1$id_individuel_hdss.y!=mix_gen_r1$id_individuel_hdss.x, 
                               "yes","no")
table(mix_gen_r1$no_corresp) # 261 do no correspond

#View(mix_gen_r1 %>% filter(no_corresp=="yes") %>% 
#  select(index, id_parcelle.x,id_parcelle.y, id_individuel_hdss.x,
#                                                        id_individuel_hdss.y))

## The non-correspondance with original id has to do with data entry errors. So I think all good. ##

# Check difference openhds and id_individuel_hdss_id
########################################################################

w_dup = which(mix_gen_r1$id_individuel_hdss.y%in%dup)
w_dup_openhdss = which(mix_gen_r1$openhds_2_individual_id%in%dup_openhdss)

w_dup_openhdss_NOT_dup = w_dup_openhdss[which(!w_dup_openhdss %in%w_dup)]

mix_gen_r1$dup_openhds_NOT_hdss = "no"
mix_gen_r1$dup_openhds_NOT_hdss[w_dup_openhdss_NOT_dup] = "yes"
table(mix_gen_r1$dup_openhds_NOT_hdss)

#View(mix_gen_r1 %>% filter(openhds_2_individual_id%in%dup_openhdss) %>%
#       select(index, id_parcelle.x,id_parcelle.y, id_individuel_hdss.x,
#              id_individuel_hdss.y, openhds_2_individual_id, dup_openhds_NOT_hdss))

## These seem to largely concern volunteers, so I suppose these can be the same individuals ##

# Check if rest of observations for these individuals is similar
dat = mix_gen_r1 %>% filter(openhds_2_individual_id%in%dup_openhdss, dup_openhds_NOT_hdss=="yes") 


# number of voluntaires with missing ID
no_id = length(which(is.na(mix_gen_r1$openhds_2_individual_id)))
table(is.na(mix_gen_r1$openhds_2_individual_id))

length(dup_openhdss) + no_id + num_unique_openhdss

# Length of individuals with sero
table(is.na(mix_gen_r1$id_sero)) # 161 have missing id when also cross checked with sero
table((is.na(mix_gen_r1$id_sero) & is.na(mix_gen_r1$id_vol_ser0))) # 9 have soc mix but no seroprev

# Remove the original hdss id and change name back 
mix_gen_r1 = mix_gen_r1 %>% 
  select(-c(id_individuel_hdss.x,id_parcelle.x)) %>%
  rename(id_individuel_hdss = 'id_individuel_hdss.y',
         id_parcelle = 'id_parcelle.y')




#########################################################
# CHECK PRESENCE INDIVIDUALS ACROSS ROUNDS
#########################################################
# SERO R2
# NAs are those that were included in the cellulaire study so can be excluded
sero_r2 = sero_r2%>% filter(!is.na(id_individuel_hdss))
length(unique(sero_r2$id_individuel_hdss))
dup = sero_r2$id_individuel_hdss[which(duplicated(sero_r2$id_individuel_hdss))]
sero_r2 = sero_r2%>% filter(!duplicated(id_individuel_hdss))

# SERO R3
# NAs are those that were included in the cellulaire study so can be excluded
sero_r3 = sero_r3 %>% filter(!is.na(id_individuel_hdss))
length(unique(sero_r3$id_individuel_hdss))
dup = sero_r3$id_individuel_hdss[which(duplicated(sero_r3$id_individuel_hdss))]
#View(sero_r4[sero_r4$id_individuel_hdss%in%dup,])
sero_r3 =sero_r3 %>% filter(!duplicated(id_individuel_hdss))

# SERO R4
length(unique(sero_r4$id_individuel_hdss))
table(is.na(sero_r4$id_individuel_hdss))

# NAs are those that were included in the cellulaire study so can be excluded
sero_r4 = sero_r4 %>% filter(!is.na(id_individuel_hdss))
length(unique(sero_r4$id_individuel_hdss))
dup = sero_r4$id_individuel_hdss[which(duplicated(sero_r4$id_individuel_hdss))]
#View(sero_r4[sero_r4$id_individuel_hdss%in%dup,])
sero_r4 =sero_r4 %>% filter(!duplicated(id_individuel_hdss))

# SERO R5
# NAs are those that were included in the cellulaire study so can be excluded
table(is.na(sero_r5$id_individuel_hdss))
#View(sero_r5[which(is.na(sero_r5$id_individuel_hdss)),]) 
sero_r5 = sero_r5 %>% filter(!is.na(id_individuel_hdss))
length(unique(sero_r5$id_individuel_hdss))
dup = sero_r5$id_individuel_hdss[which(duplicated(sero_r5$id_individuel_hdss))]
#View(sero_r5[sero_r5$id_individuel_hdss%in%dup,]) 
sero_r5 =sero_r5 %>% filter(!duplicated(id_individuel_hdss))

# In round 5 a variable was included that kept track of the new household members
table(sero_r5$type_d_entree)
#sero_r5$round = ifelse(sero_r5$type_d_entree=='nouveau', 5,' ') 
#table(sero_r5$round)

# link with household ids which are new
sero_r2 = left_join(sero_r2, idmenage[idmenage$round==2,])
sero_r3 = left_join(sero_r3, idmenage[idmenage$round%in%c(2,3),])
sero_r4 = left_join(sero_r4, idmenage[idmenage$round%in%c(2,3,4),])
sero_r5 = left_join(sero_r5, idmenage)

# link with household ids which are new
mix_gen_r2 = left_join(mix_gen_r2, idmenage[idmenage$round==2,])
mix_gen_r4 = left_join(mix_gen_r4, idmenage[idmenage$round%in%c(2,3,4),])

# Check how many IDs of other rounds can be found in R1 social mixing

# R1 social mixing vs R2 social mixing
#######################################
# Who is volunteer
mix_gen_r2 = adjust_id_hdss_vol(mix_gen_r2)
table(mix_gen_r2$id_individuel_hdss_vol)

# Check if number that changed ID are same as number of volunteers with letter at the end
table(mix_gen_r2$id_adj_same_id_org) 
table(mix_gen_r2$id_individuel_hdss_vol_letter) # Yes the same

# # add variable that marks if individual was not in R1 (and thus household member)
id_mix_r1 = unique(mix_gen_r1$id_individuel_hdss)[!is.na(mix_gen_r1$id_individuel_hdss)]
id_mix_vrai_r1 = unique(mix_gen_r1$vrai_id)[!is.na(mix_gen_r1$vrai_id)]
id_vol_sero_r1 = unique(mix_gen_r1$id_vol_ser0)[!is.na(mix_gen_r1$id_vol_ser0)]
id_mix_adj = unique(mix_gen_r2$id_individuel_hdss_adj)[!is.na(mix_gen_r2$id_individuel_hdss_adj)]
id_sero_r1 = unique(sero_r1$id_individuel_hdss)[!is.na(sero_r1$id_individuel_hdss)]
id_sero_vrai_r1 = unique(sero_r1$vrai_id)[!is.na(sero_r1$vrai_id)]
id_sero_r2 = unique(sero_r2$id_individuel_hdss)[!is.na(sero_r2$id_individuel_hdss)]
id_sero_r3 = unique(sero_r3$id_individuel_hdss)[!is.na(sero_r3$id_individuel_hdss)]
id_sero_r4 = unique(sero_r4$id_individuel_hdss)[!is.na(sero_r4$id_individuel_hdss)]

#which(!id_sero_r3 %in% id_sero_r2)
#which(!id_vol_sero_r1 %in% id_sero_r1)

# Create variables to check where ids present
mix_gen_r2 = id_in_baseline_var(mix_gen_r2)

# Where most IDs found 
id_in_baseline(mix_gen_r2)

mix_gen_r2_c = mix_gen_r2
mix_gen_r2_c$id_individuel_hdss_adj_manuel = NA
mix_gen_r2_c$vrai.id = NA
mix_gen_r2_c$openhds_2_individual_id = NA

mix_gen_r2_c =mix_gen_r2_c %>%
  select(
    id_parcelle,                                                                                        
    id_individuel_hdss,
    id_individuel_hdss_adj_manuel,
    vrai.id,
    openhds_2_individual_id,
    round,                                                                                                    
    id_individuel_hdss_vol,                                                                                   
    id_individuel_hdss_last_letter,                                                                           
    id_individuel_hdss_vol_letter,                                                                           
    id_individuel_hdss_adj,                                                                                   
    id_adj_same_id_org,
    present_r1,
    present_vrai_r1,                                                                                          
    present_adj,                                                                                             
    present_r3_sero,                                                                                          
    present_r3_sero_or_adj,                                                                                  
    present_r1_or_adj,                                                                                   
    present_r1_sero_or_adj,                                                                                   
    present_r1_or_adj_or_vrai,                                                                               
    present_r1_sero_or_adj_or_sero_vrai,                                                                     
    present_r1_mix_sero_or_adj_or_mix_sero_vrai,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero)                                                 



# R1 social mixing vs R4 social mixing
#######################################

# Who is volunteer
mix_gen_r4 = adjust_id_hdss_vol(mix_gen_r4)
table(mix_gen_r4$id_individuel_hdss_vol)

# Check if number that changed ID are same as number of volunteers with letter at the end
table(mix_gen_r4$id_adj_same_id_org) 
table(mix_gen_r4$id_individuel_hdss_vol_letter) # Yes the same

# Create variables to check where ids present
mix_gen_r4 = id_in_baseline_var(mix_gen_r4)
# Where most IDs found 
id_in_baseline(mix_gen_r4)

mix_gen_r4_c = mix_gen_r4
mix_gen_r4_c$id_individuel_hdss_adj_manuel = NA
mix_gen_r4_c$vrai.id = NA
mix_gen_r4_c$openhds_2_individual_id = NA

mix_gen_r4_c =mix_gen_r4_c %>%
  select(
    id_parcelle,                                                                                        
    id_individuel_hdss,
    id_individuel_hdss_adj_manuel,
    vrai.id,
    openhds_2_individual_id,
    round,                                                                                                    
    id_individuel_hdss_vol,                                                                                   
    id_individuel_hdss_last_letter,                                                                           
    id_individuel_hdss_vol_letter,                                                                           
    id_individuel_hdss_adj,                                                                                   
    id_adj_same_id_org,
    present_r1,
    present_vrai_r1,                                                                                          
    present_adj,                                                                                             
    present_r3_sero,                                                                                          
    present_r3_sero_or_adj,                                                                                  
    present_r1_or_adj,                                                                                   
    present_r1_sero_or_adj,                                                                                   
    present_r1_or_adj_or_vrai,                                                                               
    present_r1_sero_or_adj_or_sero_vrai,                                                                     
    present_r1_mix_sero_or_adj_or_mix_sero_vrai,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero)                                                 


# R1 social mixing vs R1 sero
#######################################
# Who is volunteer
sero_r1 = adjust_id_hdss_vol(sero_r1)
table(sero_r1$id_individuel_hdss_vol)

# Check if number that changed ID are same as number of volunteers with letter at the end
table(sero_r1$id_adj_same_id_org) 
table(sero_r1$id_individuel_hdss_vol_letter) # Yes the same

# Create variables to check where ids present
sero_r1 = id_in_baseline_var(sero_r1)
# Where most IDs found 
id_in_baseline(sero_r1)


# R1 social mixing vs R2 sero
#######################################
# Who is volunteer
sero_r2 = adjust_id_hdss_vol(sero_r2)
table(sero_r2$id_individuel_hdss_vol)

# Check if number that changed ID are same as number of volunteers with letter at the end
table(sero_r2$id_adj_same_id_org) 
table(sero_r2$id_individuel_hdss_vol_letter) # Yes the same

# Create variables to check where ids present
sero_r2 = id_in_baseline_var(sero_r2)
# Where most IDs found 
id_in_baseline(sero_r2)

sero_r2_c = sero_r2
sero_r2_c$id_individuel_hdss_adj_manuel = NA
sero_r2_c$vrai.id = NA
sero_r2_c$openhds_2_individual_id = NA

sero_r2_c = sero_r2_c %>%
  select(
  id_de_la_parcelle,                                                                                        
  id_individuel_hdss,
  id_individuel_hdss_adj_manuel,
  vrai.id,
  openhds_2_individual_id,
  round,                                                                                                    
  id_individuel_hdss_vol,                                                                                   
  id_individuel_hdss_last_letter,                                                                           
  id_individuel_hdss_vol_letter,                                                                           
  id_individuel_hdss_adj,                                                                                   
  id_adj_same_id_org,
  present_r1,
  present_vrai_r1,                                                                                          
  present_adj,                                                                                             
  present_r3_sero,                                                                                          
  present_r3_sero_or_adj,                                                                                  
  present_r1_or_adj,                                                                                   
  present_r1_sero_or_adj,                                                                                   
  present_r1_or_adj_or_vrai,                                                                               
  present_r1_sero_or_adj_or_sero_vrai,                                                                     
  present_r1_mix_sero_or_adj_or_mix_sero_vrai,
  present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero,
  present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero,
  present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero,
  present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero)                                                 





# R1 social mixing vs R3 sero
#######################################
# Who is volunteer
sero_r3 = adjust_id_hdss_vol(sero_r3)
table(sero_r3$id_individuel_hdss_vol)

# Check if number that changed ID are same as number of volunteers with letter at the end
table(sero_r3$id_adj_same_id_org) 
table(sero_r3$id_individuel_hdss_vol_letter) # Yes the same

# Create variables to check where ids present
sero_r3 = id_in_baseline_var(sero_r3)
# Where most IDs found 
id_in_baseline(sero_r3)

sero_r3_c = sero_r3
sero_r3_c$id_individuel_hdss_adj_manuel = NA
sero_r3_c$vrai.id = NA
sero_r3_c$openhds_2_individual_id = NA

sero_r3_c = sero_r3_c %>%
  select(
    id_de_la_parcelle,                                                                                        
    id_individuel_hdss,
    id_individuel_hdss_adj_manuel,
    vrai.id,
    openhds_2_individual_id,
    round,                                                                                                    
    id_individuel_hdss_vol,                                                                                   
    id_individuel_hdss_last_letter,                                                                           
    id_individuel_hdss_vol_letter,                                                                           
    id_individuel_hdss_adj,                                                                                   
    id_adj_same_id_org,
    present_r1,
    present_vrai_r1,                                                                                          
    present_adj,                                                                                             
    present_r3_sero,                                                                                          
    present_r3_sero_or_adj,                                                                                  
    present_r1_or_adj,                                                                                   
    present_r1_sero_or_adj,                                                                                   
    present_r1_or_adj_or_vrai,                                                                               
    present_r1_sero_or_adj_or_sero_vrai,                                                                     
    present_r1_mix_sero_or_adj_or_mix_sero_vrai,                                                             
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero)                                                 



# R1 social mixing vs R4 sero
#######################################
# Who is volunteer
sero_r4 = adjust_id_hdss_vol(sero_r4)
table(sero_r4$id_individuel_hdss_vol)

# Check if number that changed ID are same as number of volunteers with letter at the end
table(sero_r4$id_adj_same_id_org) 
table(sero_r4$id_individuel_hdss_vol_letter) # Yes the same

# Create variables to check where ids present
sero_r4 = id_in_baseline_var(sero_r4)
# Where most IDs found 
id_in_baseline(sero_r4)

sero_r4_c = sero_r4
sero_r4_c$id_individuel_hdss_adj_manuel = NA
sero_r4_c$vrai.id = NA
sero_r4_c$openhds_2_individual_id = NA

sero_r4_c = sero_r4_c %>%
  select(
    id_de_la_parcelle,                                                                                        
    id_individuel_hdss,
    id_individuel_hdss_adj_manuel,
    vrai.id,
    openhds_2_individual_id,
    round,                                                                                                    
    id_individuel_hdss_vol,                                                                                   
    id_individuel_hdss_last_letter,                                                                           
    id_individuel_hdss_vol_letter,                                                                           
    id_individuel_hdss_adj,                                                                                   
    id_adj_same_id_org,
    present_r1,
    present_vrai_r1,                                                                                          
    present_adj,                                                                                             
    present_r3_sero,                                                                                          
    present_r3_sero_or_adj,                                                                                  
    present_r1_or_adj,                                                                                   
    present_r1_sero_or_adj,                                                                                   
    present_r1_or_adj_or_vrai,                                                                               
    present_r1_sero_or_adj_or_sero_vrai,                                                                     
    present_r1_mix_sero_or_adj_or_mix_sero_vrai,                                                             
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero)                                                 




# R1 social mixing vs R5 sero
#######################################
# Who is volunteer
sero_r5 = adjust_id_hdss_vol(sero_r5)
table(sero_r5$id_individuel_hdss_vol)

# Check if number that changed ID are same as number of volunteers with letter at the end
table(sero_r5$id_adj_same_id_org) 
table(sero_r5$id_individuel_hdss_vol_letter) # Yes the same

# Create variables to check where ids present
sero_r5 = id_in_baseline_var(sero_r5)
# Where most IDs found 
id_in_baseline(sero_r5)

sero_r5_c = sero_r5
sero_r5_c$id_individuel_hdss_adj_manuel = NA
sero_r5_c$vrai.id = NA
sero_r5_c$openhds_2_individual_id = NA

sero_r5_c = sero_r5_c %>%
  select(
    id_de_la_parcelle,                                                                                        
    id_individuel_hdss,
    id_individuel_hdss_adj_manuel,
    vrai.id,
    openhds_2_individual_id,
    round,                                                                                                    
    id_individuel_hdss_vol,                                                                                   
    id_individuel_hdss_last_letter,                                                                           
    id_individuel_hdss_vol_letter,                                                                           
    id_individuel_hdss_adj,                                                                                   
    id_adj_same_id_org,
    present_r1,
    present_vrai_r1,                                                                                          
    present_adj,                                                                                             
    present_r3_sero,                                                                                          
    present_r3_sero_or_adj,                                                                                  
    present_r1_or_adj,                                                                                   
    present_r1_sero_or_adj,                                                                                   
    present_r1_or_adj_or_vrai,                                                                               
    present_r1_sero_or_adj_or_sero_vrai,                                                                     
    present_r1_mix_sero_or_adj_or_mix_sero_vrai,                                                             
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero,
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero)                                                 



###################################################
# Link hdss with social mixing data
###################################################

# Select the IDS to be matched with (is used by id_hdss_sero_char() function)
ids_hdss0 = unique(hdss_r0$openhds_2_individual_id)
ids_hdss1 = unique(hdss_r1$openhds_2_individual_id)
ids_hdss2 = unique(hdss_r2$openhds_2_individual_id)
id_sero_r3 = unique(sero_r3_sel$id_individuel_hdss)


# Check which IDs are in which dataset by creating 3 new variables
mix_gen_r1 = id_hdss_sero_char(mix_gen_r1)

# Remove duplicates mix_gen_r1
mix_gen_r1_hdss = mix_gen_r1 %>% 
  filter(!duplicated(openhds_2_individual_id) & !is.na(openhds_2_individual_id))

table(mix_gen_r1_hdss$num_pres_hdss) # 135 individuals can not be found in either of the three databases
sum(table(mix_gen_r1_hdss$num_pres_hdss[!mix_gen_r1_hdss$num_pres_hdss==0])) # for 1259, the ID is found in either of the three databases

table(mix_gen_r1_hdss$which_press_hdss)

# Link mix_gen_r1 with hdss_0
mix_gen_r1_hdss0 =
  left_join(mix_gen_r1_hdss, hdss_r0_sel, by = "openhds_2_individual_id")

table(is.na(mix_gen_r1_hdss0$x_creation_date)) # 1170 individuals could be identified in hdss0
table(mix_gen_r1$which_press_hdss0_sero_r3) # We will find another 138 in R3 seroprevalence

# # Link mix_gen_r1 with hdss_1
# mix_gen_r1_hdss1 =
#   left_join(mix_gen_r1_hdss, hdss_r1_sel, by = "openhds_2_individual_id")
# 
# table(is.na(mix_gen_r1_hdss1$x_creation_date)) # 973 individuals could be identified in hdss1
# 
# # Link mix_gen_r1 with hdss_2
# mix_gen_r1_hdss2 =
#   left_join(mix_gen_r1_hdss, hdss_r2_sel, by = "openhds_2_individual_id")
# 
# table(is.na(mix_gen_r1_hdss2$x_creation_date)) # 864 individuals could be identified in hdss1

# Link R1 sero with HDSS 
################################################################

# Check which IDs are in which dataset by creating 3 new variables
sero_r1 = id_hdss_sero_char(sero_r1)

# Remove duplicates mix_gen_r1
sero_r1_hdss = sero_r1 %>% 
  filter(!duplicated(openhds_2_individual_id) & !is.na(openhds_2_individual_id))

table(sero_r1_hdss$num_pres_hdss) # 140 individuals can not be found in either of the three databases
sum(table(sero_r1_hdss$num_pres_hdss[!sero_r1_hdss$num_pres_hdss==0])) # for 1362, the ID is found in either of the three databases
 
table(sero_r1_hdss$which_press_hdss)

# Link sero_r1 with hdss_0
sero_r1_hdss0 =
  left_join(sero_r1_hdss, hdss_r0_sel, by = "openhds_2_individual_id")

table(sero_r1_hdss0$openhds_2_gender, useNA="always") # 234 IDs are not found in HDSS R0
table(sero_r1_hdss0$sex, useNA="always")

table(mix_gen_r1_hdss0$openhds_2_gender, useNA="always") # 224 IDs are not found in HDSS R0

# Link R1 Social mixing with characteristics R3 seroprevalence
################################################################
mix_gen_r1_dedup = mix_gen_r1 %>% 
  filter(!duplicated(id_individuel_hdss))
         
mix_gen_r1_sero_r3 = left_join(mix_gen_r1_dedup, sero_r3_sel, by = "id_individuel_hdss")
table(is.na(mix_gen_r1_sero_r3$religion)) # 673 individuals could be identified in R3 sero

# Link R1 Social mixing and HDSS with characteristics R3 seroprevalence
################################################################
# Select relevant variables of R1 hdss
mix_gen_r1_hdss0_sel = mix_gen_r1_hdss0%>%
  select(c(index, id_individuel_hdss, openhds_household_id,openhds_2_dob,age,age_cat,openhds_2_gender,
                            caract_migrant_ethnie, caract_migrant_autre_ethnie, caract_migrant_religion, caract_migrant_religion_other,  
                            caract_migrant_education,caract_migrant_current_schooling,caract_migrant_job,             
                            caract_migrant_job_other))


char_mix_gen_r1 = left_join(mix_gen_r1_sero_r3, mix_gen_r1_hdss0_sel, by = "id_individuel_hdss") %>%
  mutate(sex = NA,
         dob = NA) %>%
  select(id_individuel_hdss, 
         openhds_2_individual_id, 
         openhds_household_id,
         id_sero,
         vrai_id,
         openhds_2_dob,
         age,
         age_cat,
         openhds_2_gender,
         caract_migrant_ethnie, 
         caract_migrant_autre_ethnie, 
         caract_migrant_religion, 
         caract_migrant_religion_other,  
         caract_migrant_education,
         caract_migrant_current_schooling,
         caract_migrant_job,             
         caract_migrant_job_other,
         sex,
         dob,
         religion,
         religion2,
         religion_other,
         ethnie,
         ethnie_other,
         education,
         occupation,
         occupation_other)

# Link R1 sero with characteristics R3 seroprevalence  (will later add R4 check)
################################################################
sero_r1_dedup = sero_r1 %>% 
  filter(!duplicated(id_individuel_hdss))

sero_r1_sero_r3 = left_join(sero_r1_dedup, sero_r3_sel, by = "id_individuel_hdss")
table(is.na(sero_r1_sero_r3$religion)) # 745 individuals could be identified in R3 sero

# Link R1 sero and HDSS with characteristics R3 seroprevalence
################################################################
sero_r1_hdss0_sel = sero_r1_hdss0%>%
  select(c(index, id_individuel_hdss,openhds_household_id,openhds_2_dob,age,age_cat,openhds_2_gender,
           caract_migrant_ethnie, caract_migrant_autre_ethnie, caract_migrant_religion, caract_migrant_religion_other,  
           caract_migrant_education,caract_migrant_current_schooling,caract_migrant_job,             
           caract_migrant_job_other))

char_sero_r1 = left_join(sero_r1_sero_r3, sero_r1_hdss0_sel, by = "id_individuel_hdss") %>%
  mutate(id_sero = NA) %>%
  select(id_individuel_hdss, 
         openhds_2_individual_id, 
         openhds_household_id,
         id_sero,
         vrai_id,
         openhds_2_dob,
         age,
         age_cat,
         openhds_2_gender,
         caract_migrant_ethnie, 
         caract_migrant_autre_ethnie, 
         caract_migrant_religion, 
         caract_migrant_religion_other,  
         caract_migrant_education,
         caract_migrant_current_schooling,
         caract_migrant_job,             
         caract_migrant_job_other,
         dob,
         sex,
         religion,
         religion2,
         religion_other,
         ethnie,
         ethnie_other,
         education,
         occupation,
         occupation_other
         )

# Ids in sero vs mix
length(which(char_sero_r1$id_individuel_hdss %in% mix_gen_r1$id_individuel_hdss))
length(which(char_mix_gen_r1$id_individuel_hdss %in% sero_r1$id_individuel_hdss))

table(is.na(char_sero_r1$sex))
table(is.na(char_mix_gen_r1$openhds_2_gender))
table(is.na(char_sero_r1$openhds_2_gender))

# Create a FINAL dataset with characteristics from hdss R0 and sero R3 per different IDs
######################################################################
add_id = which(!char_mix_gen_r1$id_individuel_hdss %in% char_sero_r1$id_individuel_hdss)
d = char_mix_gen_r1[add_id,]
char_r1_final = rbind(char_sero_r1,d)

# Combine variables
# First fill all NA's of hdss, then use sero_r3 as latest observation. If hdss != sero_3, take sero_r3. When sero_r3 = NA, take hdss
data_char = char_r1_final %>%
  mutate(
    caract_migrant_ethnie_m = ifelse(is.na(caract_migrant_ethnie), ethnie, caract_migrant_ethnie),
    ethnie_m = ethnie,
    ethnie_m = ifelse(is.na(ethnie_m), caract_migrant_ethnie_m, ethnie_m), 
    caract_migrant_religion_m = ifelse(is.na(caract_migrant_religion), religion2, caract_migrant_religion),
    religion2_m = religion2,
    religion2_m = ifelse(is.na(religion2_m), caract_migrant_religion_m, religion2_m), 
    religion2_m = ifelse(religion2_m %in%c("bdk","kkb","animist","islam"), "other", religion2_m),
    caract_migrant_education_m = ifelse(is.na(caract_migrant_education), caract_migrant_education, caract_migrant_education),
    education_m = education,
    education_m = ifelse(is.na(education_m), caract_migrant_education_m, education_m),
    caract_migrant_job_m = ifelse(is.na(caract_migrant_job), occupation, caract_migrant_job),
    occupation_m = occupation,
    occupation_m = ifelse(is.na(occupation), caract_migrant_job_m, occupation),
    occupation_m = ifelse(occupation_m %in% c("student","handicrafts"), 'other', occupation_m),
    # merge age from hdss with age that has been manually added by Djibril to Sero_s1
    age2 =trunc(as.numeric(difftime(Sys.Date(),dob,units = "weeks"))/52.25),
    age_m =ifelse(is.na(age), age2, age),
    age2_m = ifelse(is.na(age2), age_m,age2), 
    age2_m_cat = as.character(cut(age2_m, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    age2_m_cat = ifelse(is.na(age2_m_cat), "80+", as.character(age2_m_cat)),
    age2_m_cat = ifelse(is.na(age2_m), NA, as.character(age2_m_cat)),
    age2_m_cat = factor(age2_m_cat, levels=
                       c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
                         "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
                         "(65,70]" ,"(70,75]","(75,80]", "80+"))
  ) %>%
  # remove less complete age
  select(-c(age, age_cat, age2, age_m)) %>%
  rename(age = "age2_m",
         age_cat = "age2_m_cat")

# Change names to clean format
names(data_char) = gsub("_", ".", names(data_char))
char = data_char %>%
  rename(
    dob.hdss = "openhds.2.dob",                   
    sex.hdss =  "openhds.2.gender",
    ethnie.hdss = "caract.migrant.ethnie",
    ethnie.other.hdss = "caract.migrant.autre.ethnie",
    religion.hdss = "caract.migrant.religion",
    religion.other.hdss = "caract.migrant.religion.other",
    education.hdss = "caract.migrant.education",
    current.school.hdss ="caract.migrant.current.schooling",
    occupation.hdss = "caract.migrant.job",
    occupation.other.hdss = "caract.migrant.job.other",
    dob.dc = "dob",                              
    sex.dc = "sex",
    religion.dc = "religion",
    religion2.dc= "religion2",
    religion.other.dc = "religion.other",
    ethnie.dc = "ethnie",
    ethnie.other.dc = "ethnie.other",
    education.dc = "education",
    occupation.dc = "occupation",                      
    occupation.other.dc = "occupation.other",
    ethnie.hdss.m = "caract.migrant.ethnie.m",
    religion.hdss.m = "caract.migrant.religion.m",
    education.hdss.m ="caract.migrant.education.m", 
    occupation.hdss.m = "caract.migrant.job.m",  
    ethnie.dc.m  = "ethnie.m",                        
    religion2.dc.m =  "religion2.m",                           
    education.dc.m = "education.m",
    occupation.dc.m = "occupation.m"
  ) %>% 
  select(
    id.individuel.hdss, 
    openhds.2.individual.id, 
    openhds.household.id,
    id.sero,
    vrai.id,
    age,
    age.cat,
    dob.hdss,                   
    dob.dc,                              
    sex.hdss,
    sex.dc,
    ethnie.hdss,
    ethnie.dc,
    ethnie.hdss.m,
    ethnie.dc.m,                        
    ethnie.other.hdss,
    ethnie.other.dc,
    religion.hdss,
    religion.dc,
    religion2.dc,
    religion.hdss.m,
    religion2.dc.m,                          
    religion.other.hdss,
    religion.other.dc,
    education.hdss,
    education.dc,
    education.hdss.m, 
    education.dc.m,
    current.school.hdss,
    occupation.hdss,
    occupation.dc,  
    occupation.hdss.m,  
    occupation.dc.m,
    occupation.other.hdss,
    occupation.other.dc
  ) %>%
  #remove duplicates
  filter(!duplicated(openhds.2.individual.id)) %>%
  mutate(
    age.cat2 = as.character(cut(age, c(0,17,50))),
    age.cat2 = ifelse(is.na(age.cat2), "50+", as.character(age.cat2)),
    age.cat2 = ifelse(is.na(age), NA, as.character(age.cat2))
  )


#########################################################
# COMPARE HDSS_R0 with HDSS_R1/HDSS_R2

# Schooling
table(mix_gen_r1_hdss0$caract_migrant_current_schooling)
#table(mix_gen_r1_hdss2$present_educ_educ_ans_entete)

# Ethnicity
table(mix_gen_r1_hdss0$caract_migrant_ethnie)
#table(mix_gen_r1_hdss2$present_ethnie)


########################################################
# Exploratory analyses HDSS

# Gender
g0_all = ex_plot(data=hdss_r0_sel,variable.name="openhds_2_gender",
                 data.name = "HDSS_0", plot.name="Gender", all="yes")

# g1_all = ex_plot(data=hdss_r1_sel,variable.name="openhds_2_gender",
#                  data.name = "HDSS_1", plot.name="Gender", all="yes")
# 
# g2_all = ex_plot(data=hdss_r2_sel,variable.name="openhds_2_gender",
#                  data.name = "HDSS_2", plot.name="Gender", all="yes")

g0_sm = ex_plot(data=mix_gen_r1_hdss0,variable.name="openhds_2_gender",
                data.name = "R1", plot.name="Gender", all="no")

g0_sm_m = ex_plot(data=data_char,variable.name="openhds_2_gender",
                data.name = "based on R1 and R3", plot.name="Gender", all="no")

# Education 
e0_all = ex_plot(data=hdss_r0_sel,variable.name="caract_migrant_ethnie",
                 data.name = "HDSS_0", plot.name="Ethnicity", all="yes")

# e1_all = ex_plot(data=hdss_r1_sel,variable.name="present_ethnie",
#                  data.name = "HDSS_1", plot.name="Ethnicity", all="yes")
# 
# e2_all = ex_plot(data=hdss_r2_sel,variable.name="present_ethnie",
#                  data.name = "HDSS_2", plot.name="Ethnicity", all="yes")

e0_sm = ex_plot(data=mix_gen_r1_hdss0,variable.name="caract_migrant_ethnie",
                 data.name = "R1", plot.name="Ethnicity", all="no")

e0_sm_m = ex_plot(data=data_char,variable.name="ethnie_m",
                data.name = "based on R1 and R3", plot.name="Ethnicity", all="no")

# Religion
r0_all = ex_plot(data=hdss_r0_sel,variable.name="caract_migrant_religion",
        data.name = "HDSS_0", plot.name="Religion", all="yes")

# r1_all = ex_plot(data=hdss_r1_sel,variable.name="present_religion",
#                  data.name = "HDSS_1", plot.name="Religion", all="yes")
# 
# r2_all = ex_plot(data=hdss_r2_sel,variable.name="present_religion",
#                  data.name = "HDSS_2", plot.name="Religion", all="yes")


r0_sm = ex_plot(data=mix_gen_r1_hdss0,variable.name="caract_migrant_religion",
        data.name = "R1", plot.name="Religion", all="no")

r0_sm_m = ex_plot(data=data_char,variable.name="religion2_m",
                data.name = "based on R1 and R3", plot.name="Religion", all="no")

# Education
ed0_all = ex_plot(data=hdss_r0_sel,variable.name="caract_migrant_education",
                 data.name = "HDSS_0", plot.name="Education", all="yes")

# ed1_all = ex_plot(data=hdss_r1_sel,variable.name="present_educ_educ_ans_entete",
#                   data.name = "HDSS_1", plot.name="Education", all="yes")
# 
# ed2_all = ex_plot(data=hdss_r2_sel,variable.name="present_educ_educ_ans_entete",
#                   data.name = "HDSS_2", plot.name="Education", all="yes")

ed0_sm = ex_plot(data=mix_gen_r1_hdss0,variable.name="caract_migrant_education",
                data.name = "R1", plot.name="Education", all="no")

ed0_sm_m = ex_plot(data=data_char,variable.name="education_m",
                 data.name = "based on R1 and R3", plot.name="Education", all="no")

# Job
j0_all = ex_plot(data=hdss_r0_sel,variable.name="caract_migrant_job",
        data.name = "HDSS_0", plot.name="Occupation", all="yes")

# j1_all = ex_plot(data=hdss_r1_sel,variable.name="present_job_job_change_date_chmtjob2019",
#                  data.name = "HDSS_1", plot.name="Occupation", all="yes")
# 
# j2_all = ex_plot(data=hdss_r2_sel,variable.name="present_job_job_change_date_chmtjob2019",
#                  data.name = "HDSS_2", plot.name="Occupation", all="yes")


j0_sm = ex_plot(data=mix_gen_r1_hdss0,variable.name="caract_migrant_job",
        data.name = "R1", plot.name="Occupation", all="no")

j0_sm_m = ex_plot(data=data_char,variable.name="occupation_m",
                data.name = "based on R1 and R2", plot.name="Occupation", all="no")

# Age category
ac0_all = ex_plot(data=hdss_r0_sel,variable.name="age_cat",
                 data.name = "HDSS_0", plot.name="Age category", all="yes")

# ac1_all = ex_plot(data=hdss_r1_sel,variable.name="age_cat",
#                   data.name = "HDSS_1", plot.name="Age category", all="yes")
# 
# ac2_all = ex_plot(data=hdss_r2_sel,variable.name="age_cat",
#                   data.name = "HDSS_2", plot.name="Age category", all="yes")

ac0_sm = ex_plot(data=mix_gen_r1_hdss0,variable.name="age_cat",
                data.name = "R1", plot.name="Age category", all="no")

ac0_sm_m = ex_plot(data=data_char,variable.name="age_cat",
                 data.name = "based on R1 and R3", plot.name="Age category", all="no")

# Age
a0_all = ggplot(hdss_r0_sel, aes(x=age)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="seagreen") +
  theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  labs(title = paste0("Age -  HDSS 0 (N = ", nrow(hdss_r0_sel),")"),
       x="Age", y= "Density") 

# a1_all = ggplot(hdss_r1_sel, aes(x=age)) +
#   geom_bar(aes(y = (..count..)/sum(..count..)),fill="seagreen") +
#   theme_bw() +
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=14,face="bold"))+
#   labs(title = paste0("Age -  HDSS 1 (N = ", nrow(hdss_r1_sel),")"),
#        x="Age", y= "Density") 
# 
# a2_all = ggplot(hdss_r2_sel, aes(x=age)) +
#   geom_bar(aes(y = (..count..)/sum(..count..)),fill="seagreen") +
#   theme_bw() +
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=14,face="bold"))+
#   labs(title = paste0("Age -  HDSS 2 (N = ", nrow(hdss_r2_sel),")"),
#        x="Age", y= "Density") 
# 
a0_sm = ggplot(mix_gen_r1_hdss0, aes(x=age)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="coral") +
  theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  labs(title = paste0("Age -  Social-mixing R1 (N = ", nrow(mix_gen_r1_hdss0),")"),
       x="Age", y= "Density") 

a0_sm_m = ggplot(data_char, aes(x=age)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="coral") +
  theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  labs(title = paste0("Age -  Social-mixing R1 and R3 (N = ", nrow(data_char),")"),
       x="Age", y= "Density") 

#multiplot(g0_all, g0_sm,cols=2)
#multiplot(a0_all, a0_sm,cols=2)
#multiplot(ac0_all, ac0_sm,cols=1)
#multiplot(e0_all, e0_sm,cols=2)
#multiplot(ed0_all, ed0_sm,cols=2)
#multiplot(r0_all, r0_sm, cols=2)
#multiplot(j0_all, j0_sm, cols =2)


# OUTPUT DATA FOR CHECKING
############################################
# Save new selection list
#write_xlsx(mix_gen_r1, paste0(OutputDirectoryDataCheck,"soc_mix_r1_check_dups.xlsx"))
#write_xlsx(mix_gen_r2, paste0(OutputDirectoryDataCheck,"soc_mix_r2_check_missing_in_R1.xlsx"))

save(char, file = paste0(OutputDirectoryData,"characteristics.RData"))
write_xlsx(char, paste0(OutputDirectoryData,"characteristics.xlsx"))
save(hdss_r0_sel, file = paste0(OutputDirectoryData,"hdss0.RData"))

write_xlsx(idmenage, paste0(OutputDirectoryDataCheck,"householdid.xlsx"))

# Write excel sheets for checking by Djibril & Anicet (10/05/2022)
write_xlsx(sero_r2_c, paste0(OutputDirectoryDataCheck,"sero2_check2.xlsx"))
write_xlsx(sero_r3_c, paste0(OutputDirectoryDataCheck,"sero3_check2.xlsx"))
write_xlsx(sero_r4_c, paste0(OutputDirectoryDataCheck,"sero4_check2.xlsx"))
write_xlsx(sero_r5_c, paste0(OutputDirectoryDataCheck,"sero5_check2.xlsx"))

write_xlsx(mix_gen_r2_c, paste0(OutputDirectoryDataCheck,"mix2_check2.xlsx"))
write_xlsx(mix_gen_r4_c, paste0(OutputDirectoryDataCheck,"mix4_check2.xlsx"))

# write_xlsx(sero_r2_c, paste0(OutputDirectoryDataCheck,"sero2_check.xlsx"))
# write_xlsx(sero_r3_c, paste0(OutputDirectoryDataCheck,"sero3_check.xlsx"))
# write_xlsx(sero_r4_c, paste0(OutputDirectoryDataCheck,"sero4_check.xlsx"))


# save mix_gen datasets to keep adj id variable
dat = mix_gen_r2[,c(1:36,40)] 
mix_gen_r2 = dat

dat = mix_gen_r4[,c(1:35,39)] 
mix_gen_r4 = dat

save(mix_gen_r2,mix_gen_r4, file = paste0(OutputDirectoryDataRaw,"mix_gen_r1_r2_volid_added.RData"))


# OUTPUT PLOTS
# All in one file
pdf(paste0(OutputDirectory,"Plots/Exploratory/hdss0_all_vs_sm.pdf"), width = 15, height=7)
print(multiplot(g0_all, g0_sm, g0_sm_m,cols=3))
print(multiplot(a0_all, a0_sm, a0_sm_m, cols=3))
print(multiplot(ac0_all, ac0_sm,ac0_sm_m,cols=1))
print(multiplot(e0_all, e0_sm,e0_sm_m,cols=3))
print(multiplot(ed0_all, ed0_sm,ed0_sm_m,cols=3))
print(multiplot(j0_all, j0_sm,j0_sm_m,cols=3))
print(multiplot(r0_all, r0_sm,r0_sm_m,cols=3))
dev.off()
# 
# # HDSS R0 vs R1 vs R2
# pdf(paste0(OutputDirectory,"Plots/Exploratory/hdss_0_hdss1_hdss2.pdf"), width = 15, height=10)
# print(multiplot(g0_all, g1_all, g2_all,cols=3))
# print(multiplot(ac0_all, ac1_all, ac2_all,cols=1))
# print(multiplot(e0_all, e1_all, e2_all,cols=3))
# print(multiplot(ed0_all, ed1_all, ed2_all,cols=3))
# print(multiplot(j0_all, j1_all, j2_all,cols=3))
# print(multiplot(r0_all, r1_all, r2_all,cols=3))
# dev.off()


# Seperate plots
pdf(paste0(OutputDirectory,"Plots/Exploratory/gender_all_vs_sm.pdf"), width = 15, height=7)
print(multiplot(g0_all, g0_sm,cols=2))
dev.off()

pdf(paste0(OutputDirectory,"Plots/Exploratory/age_all_vs_sm.pdf"), width = 15, height=7)
print(multiplot(a0_all, a0_sm,cols=2))
dev.off()

pdf(paste0(OutputDirectory,"Plots/Exploratory/age_cat_all_vs_sm.pdf"), width = 12, height=10)
print(multiplot(ac0_all, ac0_sm,cols=1))
dev.off()

pdf(paste0(OutputDirectory,"Plots/Exploratory/ethnicity_all_vs_sm.pdf"), width = 15, height=7)
print(multiplot(e0_all, e0_sm,cols=2))
dev.off()

pdf(paste0(OutputDirectory,"Plots/Exploratory/education_all_vs_sm.pdf"), width = 15, height=7)
print(multiplot(ed0_all, ed0_sm,cols=2))
dev.off()

pdf(paste0(OutputDirectory,"Plots/Exploratory/occupation_all_vs_sm.pdf"), width = 15, height=7)
print(multiplot(j0_all, j0_sm,cols=2))
dev.off()

pdf(paste0(OutputDirectory,"Plots/Exploratory/religion_all_vs_sm.pdf"), width = 15, height=7)
print(multiplot(r0_all, r0_sm,cols=2))
dev.off()
