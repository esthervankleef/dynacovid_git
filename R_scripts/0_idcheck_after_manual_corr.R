###################################################
# DATA CLEANING - AFTER MANUAL CHECKING IDs
###################################################

# Author: E van Kleef
# Date: 13 MAY 2022

rm(list = ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./Outputs/"
OutputDirectoryDataCheck <- "../Data/Raw/To_check/"
OutputDirectoryDataCheck <- "../Data/Raw/To_check/20220513 - checked/"
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
sero_r2_c = read_xlsx("../Data/Raw/To_check/20220513 - checked/sero2_check2_adj.xlsx", sheet=1)

# R3 Seroprevalence
sero_r3 = read_xlsx("../Data/Raw/BDD DYNA 07-10-2021.xlsx", sheet=5)
sero_r3_c = read_xlsx("../Data/Raw/To_check/20220513 - checked/sero3_check2_adj.xlsx", sheet=1)

# R4 Seroprevalence
sero_r4 = read_xlsx("../Data/Raw/R4_sero_kobo.xlsx", sheet=1)
sero_r4_c = read_xlsx("../Data/Raw/To_check/20220513 - checked/sero4_check2_adj.xlsx", sheet=1)

# R4 Seroprevalence
sero_r5 = read_xlsx("../Data/Raw/R5_sero_kobo.xlsx", sheet=1)
sero_r5_c = read_xlsx("../Data/Raw/To_check/20220513 - checked/sero5_check2_adj.xlsx", sheet=1)


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
View(idmenage[idmenage$id_individuel_hdss%in%dup,])
idmenage = idmenage %>%filter(!duplicated(id_individuel_hdss))
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

# REPLACE IDs WITH MANUALLY ADJUSTED IDs
#########################################################
# SERO R2
sero_r2 = left_join(sero_r2, sero_r2_c[,c('id_individuel_hdss','id_individuel_hdss_adj_manuel'),])
sero_r2 = sero_r2 %>% 
  mutate(
    id_individuel_hdss = ifelse(!is.na(id_individuel_hdss_adj_manuel),id_individuel_hdss_adj_manuel,id_individuel_hdss)
    )
table(duplicated(sero_r2$id_individuel_hdss))
table(is.na(sero_r2$id_individuel_hdss_adj_manuel))

# SERO R3
sero_r3 = left_join(sero_r3, sero_r3_c[,c('id_individuel_hdss','id_individuel_hdss_adj_manuel'),])
sero_r3 = sero_r3 %>% 
  mutate(
    id_individuel_hdss = ifelse(!is.na(id_individuel_hdss_adj_manuel),id_individuel_hdss_adj_manuel,id_individuel_hdss)
  )
table(duplicated(sero_r3$id_individuel_hdss))
table(is.na(sero_r3$id_individuel_hdss_adj_manuel))


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

sero_r2_cor = sero_r2

sero_r2_cor = sero_r2_cor %>%
  select(
    id_de_la_parcelle,                                                                                        
    id_individuel_hdss,
    id_individuel_hdss_adj_manuel,
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


# OUTPUT DATA FOR CHECKING
############################################
# Write excel sheets for checking by Djibril & Anicet (10/05/2022)
write_xlsx(sero_r2_cor, paste0(OutputDirectoryDataChecked,"sero2_check2_adj_cor.xlsx"))

