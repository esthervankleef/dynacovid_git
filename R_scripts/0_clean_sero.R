###################################################
# DATA CLEANING of SERO DATA
###################################################

# Author: E van Kleef
# Last updated: 23 November 2021

rm(list = ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./Outputs/"
OutputDirectoryDataCheck <- "../Data/Raw/To_check/"
OutputDirectoryData <- "../Data/Clean/"


# Source functions files
source("./R_scripts/functions.R")
source("./R_scripts/multiplot.R")

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, stringi,Hmisc)

###################################################################
# Load in data
###################################################################

char = load("../Data/Clean/characteristics.RData")

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

###################################################################
# Rename variables and select variables of interest
###################################################################
# R1 sero
names(sero_r1) = tolower(names(sero_r1))
names(sero_r1) = gsub(" ", "_", names(sero_r1))
names(sero_r1) = gsub("°c", "", names(sero_r1))
names(sero_r1)[14] = "fever"

sero_r1 = sero_r1 %>%
  rename(
    id.individuel.hdss= 'id_individuel_hdss',
    id.household = 'id_de_la_parcelle',
    gps.all = 'coordonnées_gps',
    gps.latitude = '_coordonnées_gps_latitude',
    gps.longitude = '_coordonnées_gps_longitude',
    gps.altitude = '_coordonnées_gps_altitude',
    gps.precision = '_coordonnées_gps_precision',
    contact_covid = "avez-vous_été_en_contact_avec_une_personne_suspectée_ou_confirmée_d'être_infectée_par_le_virus_covid-19_?",
    date_last_contact = "si_oui,_date_du_dernier_contact",                                                                          
    symptoms =  "au_cours_des_deux_derniers_mois,_avez-vous_eu_l'un_des_problèmes_suivants_:",                              
    #fever = "fièvre38",                                                                                             
    shivers= "frissons",                                                                                                 
    muscle_pain =  "douleurs_musculaires",                                                                                     
    sore_throat =  "mal_de_gorge",                                                                                          
    cough = "toux",                                                                                                     
    running_nose =  "nez_qui_coule",                                                                                            
    short_of_breath = "l'essoufflement",                                                                                          
    wheeze = "respiration_sifflante",                                                                                    
    chest_pain = "douleurs_thoraciques",                                                                                     
    other_respiratory_symptoms =  "autres_symptômes_respiratoires",                                                                           
    headache = "maux_de_tête",                                                                                             
    nausea_vomitting = "nausées/vomissements",                                                                                     
    abdominal_pain= "douleurs_abdominales",                                                                                   
    diarrhea =  "diarrhée",                                                                                                 
    doctor_needed_symp = "l'un_de_ces_symptômes_vous_a-t-il_obligé_à_consulter_un_médecin",                                          
    doctor_needed_symp_which = "si_oui,_quel_symptôme:...30",                                                                              
    no_work_symp = "l'un_de_ces_symptômes_vous_a-t-il_obligé_à_vous_absenter_du_travail_?",                                    
    no_work_symp_which = "si_oui,_quel_symptôme:...32",                                                                              
    hosp_symp = "l'un_de_ces_symptômes_a-t-il_nécessité_votre_hospitalisation_?",                                           
    hosp_symp_which ="si_oui,_quel_symptôme:...34",                                                                              
    body_temp= "mesurer_la_température_corporelle_():",
    openhds.2.individual.id ="openhds_2_individual_id",
    vrai.id ="vrai_id"
    
  ) %>%
  mutate(
    age = trunc(as.numeric(difftime(Sys.Date(),dob,units = "weeks"))/52.25),
    age.cat = as.character(cut(age, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    age.cat = ifelse(is.na(age.cat), "80+", as.character(age.cat)),
    age.cat = ifelse(is.na(age), NA, as.character(age.cat)),
    age.cat = factor(age.cat, levels=
                       c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
                         "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
                         "(65,70]" ,"(70,75]","(75,80]", "80+")),
    age.cat2 = as.character(cut(age, c(0,17,50))),
    age.cat2 = ifelse(is.na(age.cat2), "50+", as.character(age.cat2)),
    age.cat2 = ifelse(is.na(age), NA, as.character(age.cat2)),
    # age.cat2 = factor(age.cat2, levels=
    #                    c("(0,17]", "(17,50]","(50+]"))
  ) %>%
  select(
    id.individuel.hdss,
    id.household,
    openhds.2.individual.id,
    vrai.id,
    sex,
    dob,
    age,
    age.cat,
    age.cat2,
    gps.all,
    gps.latitude,
    gps.longitude,
    gps.altitude,
    gps.precision,
    contact_covid,
    date_last_contact,                                                                          
    symptoms,                              
    fever,                                                                                             
    shivers,                                                                                                 
    muscle_pain,                                                                                     
    sore_throat,                                                                                          
    cough,                                                                                                     
    running_nose,                                                                                            
    short_of_breath,                                                                                          
    wheeze,                                                                                    
    chest_pain ,                                                                                     
    other_respiratory_symptoms,                                                                           
    headache,                                                                                             
    nausea_vomitting ,                                                                                     
    abdominal_pain,                                                                                   
    diarrhea,                                                                                                 
    doctor_needed_symp,                                          
    doctor_needed_symp_which,                                                                              
    no_work_symp ,                                    
    no_work_symp_which ,                                                                              
    hosp_symp ,                                           
    hosp_symp_which,                                                                              
    body_temp
  )

# R2 sero
names(sero_r2) = tolower(names(sero_r2))
names(sero_r2) = gsub(" ", "_", names(sero_r2))
names(sero_r2) = gsub("°c", "", names(sero_r2))
names(sero_r2)[14] = "fever"

sero_r2 = sero_r2 %>%
  rename(
    id.individuel.hdss= 'id_individuel_hdss',
    id.household = 'id_de_la_parcelle',
    gps.all = 'coordonnées_gps',
    gps.latitude = '_coordonnées_gps_latitude',
    gps.longitude = '_coordonnées_gps_longitude',
    gps.altitude = '_coordonnées_gps_altitude',
    gps.precision = '_coordonnées_gps_precision',
    contact_covid = "avez-vous_été_en_contact_avec_une_personne_suspectée_ou_confirmée_d'être_infectée_par_le_virus_covid-19_?",
    date_last_contact = "si_oui,_date_du_dernier_contact",                                                                          
    symptoms =  "au_cours_des_deux_derniers_mois,_avez-vous_eu_l'un_des_problèmes_suivants_:",                              
    #fever = "fièvre38",                                                                                             
    shivers= "frissons",                                                                                                 
    muscle_pain =  "douleurs_musculaires",                                                                                     
    sore_throat =  "mal_de_gorge",                                                                                          
    cough = "toux",                                                                                                     
    running_nose =  "nez_qui_coule",                                                                                            
    short_of_breath = "l'essoufflement",                                                                                          
    wheeze = "respiration_sifflante",                                                                                    
    chest_pain = "douleurs_thoraciques",                                                                                     
    other_respiratory_symptoms =  "autres_symptômes_respiratoires",                                                                           
    headache = "maux_de_tête",                                                                                             
    nausea_vomitting = "nausées/vomissements",                                                                                     
    abdominal_pain= "douleurs_abdominales",                                                                                   
    diarrhea =  "diarrhée",                                                                                                 
    doctor_needed_symp = "l'un_de_ces_symptômes_vous_a-t-il_obligé_à_consulter_un_médecin",                                          
    doctor_needed_symp_which = "si_oui,_quel_symptôme:...30",                                                                              
    no_work_symp = "l'un_de_ces_symptômes_vous_a-t-il_obligé_à_vous_absenter_du_travail_?",                                    
    no_work_symp_which = "si_oui,_quel_symptôme:...32",                                                                              
    hosp_symp = "l'un_de_ces_symptômes_a-t-il_nécessité_votre_hospitalisation_?",                                           
    hosp_symp_which ="si_oui,_quel_symptôme:...34",                                                                              
    body_temp= "mesurer_la_température_corporelle_():" 
  ) %>%
  select(
    id.individuel.hdss,
    id.household,
    gps.all,
    gps.latitude,
    gps.longitude,
    gps.altitude,
    gps.precision,
    contact_covid,
    date_last_contact,                                                                          
    symptoms,                              
    fever,                                                                                             
    shivers,                                                                                                 
    muscle_pain,                                                                                     
    sore_throat,                                                                                          
    cough,                                                                                                     
    running_nose,                                                                                            
    short_of_breath,                                                                                          
    wheeze,                                                                                    
    chest_pain ,                                                                                     
    other_respiratory_symptoms,                                                                           
    headache,                                                                                             
    nausea_vomitting ,                                                                                     
    abdominal_pain,                                                                                   
    diarrhea,                                                                                                 
    doctor_needed_symp,                                          
    doctor_needed_symp_which,                                                                              
    no_work_symp ,                                    
    no_work_symp_which ,                                                                              
    hosp_symp ,                                           
    hosp_symp_which,                                                                              
    body_temp
  )

# R1 sero
names(sero_r3) = tolower(names(sero_r3))
names(sero_r3) = gsub(" ", "_", names(sero_r3))
names(sero_r3) = gsub("°c", "", names(sero_r3))
names(sero_r3)[29] = "fever"

sero_r3 = sero_r3 %>%
  rename(
    id.individuel.hdss= 'id_individuel_hdss',
    id.household = 'id_de_la_parcelle',
    gps.all = 'coordonnées_gps',
    gps.latitude = '_coordonnées_gps_latitude',
    gps.longitude = '_coordonnées_gps_longitude',
    gps.altitude = '_coordonnées_gps_altitude',
    gps.precision = '_coordonnées_gps_precision',
    contact_covid = "avez-vous_été_en_contact_avec_une_personne_suspectée_ou_confirmée_d'être_infectée_par_le_virus_covid-19_?",
    date_last_contact = "si_oui,_date_du_dernier_contact",                                                                          
    symptoms =  "au_cours_des_deux_derniers_mois,_avez-vous_eu_l'un_des_problèmes_suivants_:",                              
    #fever = "fièvre38",                                                                                             
    shivers= "frissons",                                                                                                 
    muscle_pain =  "douleurs_musculaires",                                                                                     
    sore_throat =  "mal_de_gorge",                                                                                          
    cough = "toux",                                                                                                     
    running_nose =  "nez_qui_coule",                                                                                            
    short_of_breath = "l'essoufflement",                                                                                          
    wheeze = "respiration_sifflante",                                                                                    
    chest_pain = "douleurs_thoraciques",                                                                                     
    other_respiratory_symptoms =  "autres_symptômes_respiratoires",                                                                           
    headache = "maux_de_tête",                                                                                             
    nausea_vomitting = "nausées/vomissements",                                                                                     
    abdominal_pain= "douleurs_abdominales",                                                                                   
    diarrhea =  "diarrhée",                                                                                                 
    doctor_needed_symp = "l'un_de_ces_symptômes_vous_a-t-il_obligé_à_consulter_un_médecin",                                          
    doctor_needed_symp_which = "si_oui,_quel_symptôme:...45",                                                                              
    no_work_symp = "l'un_de_ces_symptômes_vous_a-t-il_obligé_à_vous_absenter_du_travail_?",                                    
    no_work_symp_which = "si_oui,_quel_symptôme:...47",                                                                              
    hosp_symp = "l'un_de_ces_symptômes_a-t-il_nécessité_votre_hospitalisation_?",                                           
    hosp_symp_which ="si_oui,_quel_symptôme:...49",                                                                              
    body_temp= "mesurer_la_température_corporelle_():" 
  ) %>%
  select(
    id.individuel.hdss,
    id.household,
    gps.all,
    gps.latitude,
    gps.longitude,
    gps.altitude,
    gps.precision,
    contact_covid,
    date_last_contact,                                                                          
    symptoms,                              
    fever,                                                                                             
    shivers,                                                                                                 
    muscle_pain,                                                                                     
    sore_throat,                                                                                          
    cough,                                                                                                     
    running_nose,                                                                                            
    short_of_breath,                                                                                          
    wheeze,                                                                                    
    chest_pain ,                                                                                     
    other_respiratory_symptoms,                                                                           
    headache,                                                                                             
    nausea_vomitting ,                                                                                     
    abdominal_pain,                                                                                   
    diarrhea,                                                                                                 
    doctor_needed_symp,                                          
    doctor_needed_symp_which,                                                                              
    no_work_symp ,                                    
    no_work_symp_which ,                                                                              
    hosp_symp ,                                           
    hosp_symp_which,                                                                              
    body_temp
  )%>%
  filter(!is.na(id.household))

# R4 sero
names(sero_r4) = tolower(names(sero_r4))
names(sero_r4) = gsub(" ", "_", names(sero_r4))
names(sero_r4) = gsub("°c", "", names(sero_r4))
names(sero_r4)[29] = "fever"

sero_r4 = sero_r4 %>%
  rename(
    id.individuel.hdss= 'id_individuel_hdss',
    id.household = 'id_de_la_parcelle',
    gps.all = 'coordonnées_gps',
    gps.latitude = '_coordonnées_gps_latitude',
    gps.longitude = '_coordonnées_gps_longitude',
    gps.altitude = '_coordonnées_gps_altitude',
    gps.precision = '_coordonnées_gps_precision',
    contact_covid = "avez-vous_été_en_contact_avec_une_personne_suspectée_ou_confirmée_d'être_infectée_par_le_virus_covid-19_?",
    date_last_contact = "si_oui,_date_du_dernier_contact",                                                                          
    symptoms =  "au_cours_des_deux_derniers_mois,_avez-vous_eu_l'un_des_problèmes_suivants_:",                              
    #fever = "fièvre38",                                                                                             
    shivers= "frissons",                                                                                                 
    muscle_pain =  "douleurs_musculaires",                                                                                     
    sore_throat =  "mal_de_gorge",                                                                                          
    cough = "toux",                                                                                                     
    running_nose =  "nez_qui_coule",                                                                                            
    short_of_breath = "l'essoufflement",                                                                                          
    wheeze = "respiration_sifflante",                                                                                    
    chest_pain = "douleurs_thoraciques",                                                                                     
    other_respiratory_symptoms =  "autres_symptômes_respiratoires",                                                                           
    headache = "maux_de_tête",                                                                                             
    nausea_vomitting = "nausées/vomissements",                                                                                     
    abdominal_pain= "douleurs_abdominales",                                                                                   
    diarrhea =  "diarrhée",                                                                                                 
    doctor_needed_symp = "l'un_de_ces_symptômes_vous_a-t-il_obligé_à_consulter_un_médecin",                                          
    doctor_needed_symp_which = "si_oui,_quel_symptôme:...45",                                                                              
    no_work_symp = "l'un_de_ces_symptômes_vous_a-t-il_obligé_à_vous_absenter_du_travail_?",                                    
    no_work_symp_which = "si_oui,_quel_symptôme:...47",                                                                              
    hosp_symp = "l'un_de_ces_symptômes_a-t-il_nécessité_votre_hospitalisation_?",                                           
    hosp_symp_which ="si_oui,_quel_symptôme:...49",                                                                              
    body_temp= "mesurer_la_température_corporelle_():" 
  ) %>%
  select(
    id.individuel.hdss,
    id.household,
    gps.all,
    gps.latitude,
    gps.longitude,
    gps.altitude,
    gps.precision,
    contact_covid,
    date_last_contact,                                                                          
    symptoms,                              
    fever,                                                                                             
    shivers,                                                                                                 
    muscle_pain,                                                                                     
    sore_throat,                                                                                          
    cough,                                                                                                     
    running_nose,                                                                                            
    short_of_breath,                                                                                          
    wheeze,                                                                                    
    chest_pain ,                                                                                     
    other_respiratory_symptoms,                                                                           
    headache,                                                                                             
    nausea_vomitting ,                                                                                     
    abdominal_pain,                                                                                   
    diarrhea,                                                                                                 
    doctor_needed_symp,                                          
    doctor_needed_symp_which,                                                                              
    no_work_symp ,                                    
    no_work_symp_which ,                                                                              
    hosp_symp ,                                           
    hosp_symp_which,                                                                              
    body_temp
  ) %>%
  filter(!is.na(id.household))

############################################################
# Write datasets
save(sero_r1,sero_r2,sero_r3, sero_r4, file = paste0(OutputDirectoryData,"sero.RData"))

