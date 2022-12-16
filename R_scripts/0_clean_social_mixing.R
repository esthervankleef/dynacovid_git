###################################################
# DATA CLEANING of SOCIAL MIXING DATA
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

char = load("../Data/Clean/characteristics.RData")
###################################################################
# Load in data
###################################################################
# R1 Social mixing
mix_gen_r1 = read_xlsx("../Data/Raw/R1_mix_soc_adj.xlsx", sheet=1)
mix_lieu_r1 = read_xlsx("../Data/Raw/R1_mix_soc_adj.xlsx", sheet=2)
mix_contact_r1 = read_xlsx("../Data/Raw/R1_mix_soc_adj.xlsx", sheet=3)

# R2 social mixing
load("../Data/Raw/mix_gen_r1_r2_volid_added.RData")
#mix_gen_r2 = read_xlsx("../Data/Raw/R2_mix_soc_adj.xlsx", sheet=1)
mix_lieu_r2 = read_xlsx("../Data/Raw/R2_mix_soc_adj.xlsx", sheet=2)
mix_contact_r2 = read_xlsx("../Data/Raw/R2_mix_soc_adj.xlsx", sheet=3)

# R3 Social mixing (=R4 seroprevalence, thus call it R4)
#mix_gen_r4 = read_xlsx("../Data/Raw/R3_mix_soc_kobo.xlsx", sheet=1)
mix_lieu_r4 = read_xlsx("../Data/Raw/R3_mix_soc_kobo.xlsx", sheet=2)
mix_contact_r4 = read_xlsx("../Data/Raw/R3_mix_soc_kobo.xlsx", sheet=3)


###################################################################
# Rename variables and select variables of interest
###################################################################

#--------------------------------------------------------------------------#
#----------------------- General mixing data ------------------------------#
#--------------------------------------------------------------------------#

# Social mixing R1
names(mix_gen_r1) = tolower(names(mix_gen_r1))
names(mix_gen_r1) = gsub(" ", "_", names(mix_gen_r1))

mix_gen_r1 = mix_gen_r1 %>%
  rename(openhds_2_individual_id = 'id_serveur',
         id_household = 'id_parcelle',
         gps.all = 'coordonn_es_gps',
         gps.latitude = '_coordonn_es_gps_latitude',
         gps.longitude = '_coordonn_es_gps_longitude',
         gps.altitude = '_coordonn_es_gps_altitude',
         gps.precision = '_coordonn_es_gps_precision',
         date.interview = 'dateinterview',
         day.contact = 'jour_hier',
         casual.phys = 'nbre_contact',
         casual.nphys = 'contact_non_physique',
         mask.day.contact = 'utilisation_masque_facial',
         mask7.outside = 'exterieur_parcell',
         mask7.transp = 'transport_en_commun',
         mask7.work = "etre_au_travail",
         mask7.field  = "etre_au_champ",
         mask7.shop = "course_marche",
         present7.gathering20pers = "rassemblement_20persoones",
         mask7.gathering = 'participation_evenement',
         isolation.symptoms = "resterez_maison_si_toux",
         submission.id = '_id',
         index ='_index',
         ig.total.result = 'resultat_ig_total'
  ) %>%
  select(c(submission.id,
           index,
           id_individuel_hdss,
           id_household,
           openhds_2_individual_id,
           gps.all,
           gps.latitude,
           gps.longitude,
           gps.altitude,
           gps.precision,
           date.interview,
           day.contact,
           casual.phys,
           casual.nphys,
           mask.day.contact,
           mask7.outside,
           mask7.transp,
           mask7.work,
           mask7.field,
           mask7.shop,
           present7.gathering20pers,
           mask7.gathering,
           isolation.symptoms,
           ig.total.result)
         )  %>%
  mutate(
    date.interview = as.Date(date.interview, origin = "1899-12-30"),
    day.contact = case_when(
      day.contact =="lundi" ~ "monday",
      day.contact =="mardi" ~ "tuesday",
      day.contact =="mercredi" ~ "wednesday",
      day.contact =="jeudi" ~ "thursday",
      day.contact =="vendredi" ~ "friday",
      day.contact =="samedi" ~ "saturday",
      day.contact =="dimanche" ~ "sunday"),
    day.contact = factor(day.contact),
    casual.phys = case_when(
      casual.phys == "ind0_9" ~ "0-9",
      casual.phys == "ind10_19" ~ "10-19",
      casual.phys == "ind20_29" ~ "20-29",
      casual.phys == "ind30" ~ ">30",
      casual.phys == "nsp" ~ "unknown"),
    casual.phys = factor(casual.phys),
    casual.nphys = case_when(
      casual.nphys == "ind0_9" ~ "0-9",
      casual.nphys == "ind10_19" ~ "10-19",
      casual.nphys == "ind20_29" ~ "20-29",
      casual.nphys == "ind30" ~ ">30",
      casual.nphys == "untitled" ~ "unknown"),
    casual.nphys = factor(casual.nphys),
    mask.day.contact = case_when(
      mask.day.contact == "non" ~ "no",
      mask.day.contact == "oui" ~ "yes"),
    mask.day.contact = factor(mask.day.contact),
    mask7.outside = case_when(
      mask7.outside == "oui" ~"yes",
      mask7.outside == "non" ~"no"),
    mask7.outside = factor(mask7.outside),
    mask7.transp = case_when(
      mask7.transp == "oui" ~"yes",
      mask7.transp == "non" ~"no"),
    mask7.transp = factor(mask7.transp),
    mask7.work = case_when(
      mask7.work == "oui" ~"yes",
      mask7.work == "non" ~"no"),
    mask7.work = factor(mask7.work),
    mask7.field = case_when(
      mask7.field == "oui" ~"yes",
      mask7.field == "non" ~"no"),
    mask7.field = factor(mask7.field),
    mask7.shop = case_when(
      mask7.shop == "oui" ~"yes",
      mask7.shop == "non" ~"no"),
    mask7.shop = factor(mask7.shop),
    present7.gathering20pers = case_when(
      present7.gathering20pers == "non" ~"no",
      present7.gathering20pers == "oui20_50" ~"yes20_50",
      present7.gathering20pers == "oui50" ~"yes50"),
    present7.gathering20pers = factor(present7.gathering20pers),
    mask7.gathering = case_when(
      mask7.gathering == "oui" ~"yes",
      mask7.gathering == "non" ~"no"),
    mask7.gathering = factor(mask7.gathering),
    isolation.symptoms = case_when(
      isolation.symptoms == "oui" ~"yes",
      isolation.symptoms == "non" ~"no"),
    isolation.symptoms = factor(isolation.symptoms),
    submission.id = factor(submission.id),
    index = as.numeric(index)
  ) 
names(mix_gen_r1) = gsub("_", ".", names(mix_gen_r1))

# Social mixing R2
names(mix_gen_r2) = tolower(names(mix_gen_r2))
names(mix_gen_r2) = gsub(" ", "_", names(mix_gen_r2))
mix_gen_r2 = mix_gen_r2 %>%
  rename(id_household = 'id_parcelle',
         #id_hh_members_vol = '...7',
         gps.all = 'coordonn_es_gps',
         gps.latitude = '_coordonn_es_gps_latitude',
         gps.longitude = '_coordonn_es_gps_longitude',
         gps.altitude = '_coordonn_es_gps_altitude',
         gps.precision = '_coordonn_es_gps_precision',
         date.interview = 'dateinterview',
         day.contact = 'jour_hier',
         casual.phys = 'nbre_contact',
         casual.nphys = 'contact_non_physique',
         mask.day.contact = 'utilisation_masque_facial',
         mask7.outside = 'exterieur_parcell',
         mask7.transp = 'transport_en_commun',
         mask7.work = "etre_au_travail",
         mask7.field  = "etre_au_champ",
         mask7.shop = "course_marche",
         present7.gathering20pers = "rassemblement_20persoones",
         mask7.gathering = 'participation_evenement',
         isolation.symptoms = "resterez_maison_si_toux",
         submission.id = '_id'
         #index ='_index'
  ) %>%
  select(c(submission.id,
           index,
           id_individuel_hdss,
           id_individuel_hdss_adj,
           id_household,
           id_hh_members_vol,
           gps.all,
           gps.latitude,
           gps.longitude,
           gps.altitude,
           gps.precision,
           date.interview,
           day.contact,
           casual.phys,
           casual.nphys,
           mask.day.contact,
           mask7.outside,
           mask7.transp,
           mask7.work,
           mask7.field,
           mask7.shop,
           present7.gathering20pers,
           mask7.gathering,
           isolation.symptoms)
  )  %>%
  mutate(
    date.interview = as.Date(date.interview, origin = "1899-12-30"),
    day.contact = case_when(
      day.contact =="lundi" ~ "monday",
      day.contact =="mardi" ~ "tuesday",
      day.contact =="mercredi" ~ "wednesday",
      day.contact =="jeudi" ~ "thursday",
      day.contact =="vendredi" ~ "friday",
      day.contact =="samedi" ~ "saturday",
      day.contact =="dimanche" ~ "sunday"),
    day.contact = factor(day.contact),
    casual.phys = case_when(
      casual.phys == "ind0_9" ~ "0-9",
      casual.phys == "ind10_19" ~ "10-19",
      casual.phys == "ind20_29" ~ "20-29",
      casual.phys == "ind30" ~ ">30",
      casual.phys == "nsp" ~ "unknown"),
    casual.phys = factor(casual.phys),
    casual.nphys = case_when(
      casual.nphys == "ind0_9" ~ "0-9",
      casual.nphys == "ind10_19" ~ "10-19",
      casual.nphys == "ind20_29" ~ "20-29",
      casual.nphys == "ind30" ~ ">30",
      casual.nphys == "untitled" ~ "unknown"),
    casual.nphys = factor(casual.nphys),
    mask.day.contact = case_when(
      mask.day.contact == "non" ~ "no",
      mask.day.contact == "oui" ~ "yes"),
    mask.day.contact = factor(mask.day.contact),
    mask7.outside = case_when(
      mask7.outside == "oui" ~"yes",
      mask7.outside == "non" ~"no"),
    mask7.outside = factor(mask7.outside),
    mask7.transp = case_when(
      mask7.transp == "oui" ~"yes",
      mask7.transp == "non" ~"no"),
    mask7.transp = factor(mask7.transp),
    mask7.work = case_when(
      mask7.work == "oui" ~"yes",
      mask7.work == "non" ~"no"),
    mask7.work = factor(mask7.work),
    mask7.field = case_when(
      mask7.field == "oui" ~"yes",
      mask7.field == "non" ~"no"),
    mask7.field = factor(mask7.field),
    mask7.shop = case_when(
      mask7.shop == "oui" ~"yes",
      mask7.shop == "non" ~"no"),
    mask7.shop = factor(mask7.shop),
    present7.gathering20pers = case_when(
      present7.gathering20pers == "non" ~"no",
      present7.gathering20pers == "oui20_50" ~"yes20_50",
      present7.gathering20pers == "oui50" ~"yes50"),
    present7.gathering20pers = factor(present7.gathering20pers),
    mask7.gathering = case_when(
      mask7.gathering == "oui" ~"yes",
      mask7.gathering == "non" ~"no"),
    mask7.gathering = factor(mask7.gathering),
    isolation.symptoms = case_when(
      isolation.symptoms == "oui" ~"yes",
      isolation.symptoms == "non" ~"no"),
    isolation.symptoms = factor(isolation.symptoms),
    submission.id = factor(submission.id),
    index = as.numeric(index)
)

names(mix_gen_r2) = gsub("_", ".", names(mix_gen_r2))


# Social mixing R4
names(mix_gen_r4) = tolower(names(mix_gen_r4))
names(mix_gen_r4) = gsub(" ", "_", names(mix_gen_r4))
mix_gen_r4 = mix_gen_r4 %>%
  rename(#id_individuel_hdss = 'id_individuelle',
         id_household = 'id_parcelle',
         gps.all = 'coordonn_es_gps',
         gps.latitude = '_coordonn_es_gps_latitude',
         gps.longitude = '_coordonn_es_gps_longitude',
         gps.altitude = '_coordonn_es_gps_altitude',
         gps.precision = '_coordonn_es_gps_precision',
         date.interview = 'dateinterview',
         day.contact = 'jour_hier',
         casual.phys = 'nbre_contact',
         casual.nphys = 'contact_non_physique',
         mask.day.contact = 'utilisation_masque_facial',
         mask7.outside = 'exterieur_parcell',
         mask7.transp = 'transport_en_commun',
         mask7.work = "etre_au_travail",
         mask7.field  = "etre_au_champ",
         mask7.shop = "course_marche",
         present7.gathering20pers = "rassemblement_20persoones",
         mask7.gathering = 'participation_evenement',
         isolation.symptoms = "resterez_maison_si_toux",
         submission.id = '_id'
         #index ='_index'
  ) %>%
  select(c(submission.id,
           index,
           id_individuel_hdss,
           id_individuel_hdss_adj,
           id_household,
           gps.all,
           gps.latitude,
           gps.longitude,
           gps.altitude,
           gps.precision,
           date.interview,
           day.contact,
           casual.phys,
           casual.nphys,
           mask.day.contact,
           mask7.outside,
           mask7.transp,
           mask7.work,
           mask7.field,
           mask7.shop,
           present7.gathering20pers,
           mask7.gathering,
           isolation.symptoms)
  )  %>%
  mutate(
    date.interview = as.Date(date.interview, origin = "1899-12-30"),
    day.contact = case_when(
      day.contact =="lundi" ~ "monday",
      day.contact =="mardi" ~ "tuesday",
      day.contact =="mercredi" ~ "wednesday",
      day.contact =="jeudi" ~ "thursday",
      day.contact =="vendredi" ~ "friday",
      day.contact =="samedi" ~ "saturday",
      day.contact =="dimanche" ~ "sunday"),
    day.contact = factor(day.contact),
    casual.phys = case_when(
      casual.phys == "ind0_9" ~ "0-9",
      casual.phys == "ind10_19" ~ "10-19",
      casual.phys == "ind20_29" ~ "20-29",
      casual.phys == "ind30" ~ ">30",
      casual.phys == "nsp" ~ "unknown"),
    casual.phys = factor(casual.phys),
    casual.nphys = case_when(
      casual.nphys == "ind0_9" ~ "0-9",
      casual.nphys == "ind10_19" ~ "10-19",
      casual.nphys == "ind20_29" ~ "20-29",
      casual.nphys == "ind30" ~ ">30",
      casual.nphys == "untitled" ~ "unknown"),
    casual.nphys = factor(casual.nphys),
    mask.day.contact = case_when(
      mask.day.contact == "non" ~ "no",
      mask.day.contact == "oui" ~ "yes"),
    mask.day.contact = factor(mask.day.contact),
    mask7.outside = case_when(
      mask7.outside == "oui" ~"yes",
      mask7.outside == "non" ~"no"),
    mask7.outside = factor(mask7.outside),
    mask7.transp = case_when(
      mask7.transp == "oui" ~"yes",
      mask7.transp == "non" ~"no"),
    mask7.transp = factor(mask7.transp),
    mask7.work = case_when(
      mask7.work == "oui" ~"yes",
      mask7.work == "non" ~"no"),
    mask7.work = factor(mask7.work),
    mask7.field = case_when(
      mask7.field == "oui" ~"yes",
      mask7.field == "non" ~"no"),
    mask7.field = factor(mask7.field),
    mask7.shop = case_when(
      mask7.shop == "oui" ~"yes",
      mask7.shop == "non" ~"no"),
    mask7.shop = factor(mask7.shop),
    present7.gathering20pers = case_when(
      present7.gathering20pers == "non" ~"no",
      present7.gathering20pers == "oui20_50" ~"yes20_50",
      present7.gathering20pers == "oui50" ~"yes50"),
    present7.gathering20pers = factor(present7.gathering20pers),
    mask7.gathering = case_when(
      mask7.gathering == "oui" ~"yes",
      mask7.gathering == "non" ~"no"),
    mask7.gathering = factor(mask7.gathering),
    isolation.symptoms = case_when(
      isolation.symptoms == "oui" ~"yes",
      isolation.symptoms == "non" ~"no"),
    isolation.symptoms = factor(isolation.symptoms),
    submission.id = factor(submission.id),
    index = as.numeric(index)
  )
names(mix_gen_r4) = gsub("_", ".", names(mix_gen_r4))



#--------------------------------------------------------------------------#
#------------------------- Lieu mixing data -------------------------------#
#--------------------------------------------------------------------------#

# Social mixing R1
names(mix_lieu_r1) = tolower(names(mix_lieu_r1 ))
names(mix_lieu_r1) = gsub(" ", "_", names(mix_lieu_r1))

mix_lieu_r1 = mix_lieu_r1 %>%
  rename(loc.type = "type_lieu",
         type.oth = "pr_ciser_autre_lieu",
         vicinity = "lieu_iden",
         vicinity.vill.same.prov = "pr_ciser_autre_lieu_dans_la_province",
         vicinity.vill.same_as = "village_meme_aire_de_sante",
         vicinity.vill.other.as = "pr_cisez_aire_de_san_de_sant_de_kimpese_",
         vicinity.other.prov = "preciser_province",
         time.spend = 'temps_passe_endroit',
         n.contact = "nombre_de_contact",
         index.place = "_index",
         parent.index.place = "_parent_index",
         submission.id.place = "_submission__id") %>%
  mutate(
    type.oth = ifelse(type.oth %in% c("PUIS D'EAU","SOURCE  DEAU","POINT D EAU",
                                         "PUIT D'EAU","POINT DEAU","POINTS D'EAU",
                                         "RIVIERE","RIVIÈRE"), "Riviere", type.oth),
    loc.type = ifelse(type.oth %in% c("Riviere") ,"river", loc.type),
    loc.type = case_when(
      loc.type == "autres" ~ "other",
      loc.type == "maison" ~ "home",
      loc.type == "champs_t" ~ "field",
      loc.type == "ecole" ~ "school",
      loc.type == "lieu_culte" ~ "place_of_worship",
      loc.type == "transport_bus" ~ "transport_bus",
      loc.type == "transport_wewa" ~ "transport_wewa",
      loc.type == "autre_maison" ~ "other_household",
      loc.type == "travail" ~ "work",
      loc.type == "Lieu_de_loisir" ~ "place_of_leisure",
      loc.type == "magasin_marche" ~ "market",
      TRUE ~ as.character(loc.type)), 
    vicinity2 = ifelse(vicinity %in% c("aire_different","autre_village_meme_aire"), "outside village(ZS Kimpese)" ,
                                         ifelse(vicinity %in% c("autre_provincr",
                                                                "hors_zone_sante"), "outside village(outside ZS Kimpese)" ,
                                                "within village")),
    submission.id.place = factor(submission.id.place)
    ) %>%
      select(c(index.place,
               parent.index.place,
               submission.id.place,
               loc.type,
               type.oth,
               vicinity,
               vicinity2,
               vicinity.vill.same.prov,
               vicinity.vill.same_as,
               vicinity.vill.other.as,
               vicinity.other.prov,
               n.contact)
    )

# Social mixing R2
names(mix_lieu_r2) = tolower(names(mix_lieu_r2))
names(mix_lieu_r2) = gsub(" ", "_", names(mix_lieu_r2))

mix_lieu_r2 = mix_lieu_r2 %>%
  rename(loc.type = "type_lieu",
         type.oth = "pr_ciser_autre_lieu",
         vicinity = "lieu_iden",
         vicinity.vill.same.prov = "pr_ciser_autre_lieu_dans_la_province",
         vicinity.vill.same_as = "village_meme_aire_de_sante",
         vicinity.vill.other.as = "pr_cisez_aire_de_san_de_sant_de_kimpese_",
         vicinity.other.prov = "preciser_province",
         time.spend = 'temps_passe_endroit',
         n.contact = "nombre_de_contact",
         index.place = "_index",
         parent.index.place = "_parent_index",
         submission.id.place = "_submission__id") %>%
  mutate(
    type.oth = ifelse(type.oth %in% c("POINT D’EAU","SOURCE D’EAU",
                                      "RIVIERE","RIVIÈRE"), "Riviere", type.oth),
    loc.type = ifelse(type.oth %in% c("Riviere") ,"river", loc.type),
    loc.type = case_when(
      loc.type == "autres" ~ "other",
      loc.type == "maison" ~ "home",
      loc.type == "champs_t" ~ "field",
      loc.type == "ecole" ~ "school",
      loc.type == "lieu_culte" ~ "place_of_worship",
      loc.type == "transport_bus" ~ "transport_bus",
      loc.type == "transport_wewa" ~ "transport_wewa",
      loc.type == "autre_maison" ~ "other_household",
      loc.type == "travail" ~ "work",
      loc.type == "Lieu_de_loisir" ~ "place_of_leisure",
      loc.type == "magasin_marche" ~ "market",
      TRUE ~ as.character(loc.type)), 
    vicinity2 = ifelse(vicinity %in% c("aire_different","autre_village_meme_aire"), "outside village(ZS Kimpese)" ,
                       ifelse(vicinity %in% c("autre_provincr",
                                              "hors_zone_sante"), "outside village(outside ZS Kimpese)" ,
                              "within village")),
    submission.id.place = factor(submission.id.place)
  ) %>%
  select(c(
    index.place,
    parent.index.place,
    submission.id.place,
    loc.type,
    type.oth,
    vicinity,
    vicinity2,
    vicinity.vill.same.prov,
    vicinity.vill.same_as,
    vicinity.vill.other.as,
    vicinity.other.prov,
    n.contact)
)
 

# Social mixing R4
dat = mix_lieu_r4
names(mix_lieu_r4) = tolower(names(mix_lieu_r4))
names(mix_lieu_r4) = gsub(" ", "_", names(mix_lieu_r4))

mix_lieu_r4 = mix_lieu_r4 %>%
  rename(loc.type = "type_lieu",
         type.oth = "pr_ciser_autre_lieu",
         vicinity = "lieu_iden",
         vicinity.vill.same.prov = "pr_ciser_autre_lieu_dans_la_province",
         vicinity.vill.same_as = "village_meme_aire_de_sante",
         vicinity.vill.other.as = "pr_cisez_aire_de_san_de_sant_de_kimpese_",
         vicinity.other.prov = "preciser_province",
         time.spend = 'temps_passe_endroit',
         n.contact = "nombre_de_contact",
         index.place = "_index",
         parent.index.place = "_parent_index",
         submission.id.place = "_submission__id") %>%
  mutate(
    type.oth = ifelse(type.oth %in% c("RIVIERE","La RIVIÈRE"), "Riviere", type.oth),
    loc.type = ifelse(type.oth %in% c("Riviere") ,"river", loc.type),
    loc.type = case_when(
      loc.type == "autres" ~ "other",
      loc.type == "maison" ~ "home",
      loc.type == "champs_t" ~ "field",
      loc.type == "ecole" ~ "school",
      loc.type == "lieu_culte" ~ "place_of_worship",
      loc.type == "transport_bus" ~ "transport_bus",
      loc.type == "transport_wewa" ~ "transport_wewa",
      loc.type == "autre_maison" ~ "other_household",
      loc.type == "travail" ~ "work",
      loc.type == "Lieu_de_loisir" ~ "place_of_leisure",
      loc.type == "magasin_marche" ~ "market",
      TRUE ~ as.character(loc.type)), 
    vicinity2 = ifelse(vicinity %in% c("aire_different","autre_village_meme_aire"), "outside village(ZS Kimpese)" ,
                       ifelse(vicinity %in% c("autre_provincr",
                                              "hors_zone_sante"), "outside village(outside ZS Kimpese)" ,
                              "within village")),
    submission.id.place = factor(submission.id.place)
  ) %>%
  select(c(
    index.place,
    parent.index.place,
    submission.id.place,
    loc.type,
    type.oth,
    vicinity,
    vicinity2,
    vicinity.vill.same.prov,
    vicinity.vill.same_as,
    vicinity.vill.other.as,
    vicinity.other.prov,
    n.contact)
  )

#--------------------------------------------------------------------------#
#------------------------- Contact mixing data ----------------------------#
#--------------------------------------------------------------------------# 
# store to be able to make comparison
dat = mix_contact_r1 = read_xlsx("../Data/Raw/R1_mix_soc_adj.xlsx", sheet=3) 

# Social mixing R1
names(mix_contact_r1) = tolower(names(mix_contact_r1))
names(mix_contact_r1) = gsub(" ", "_", names(mix_contact_r1))


mix_contact_r1 = mix_contact_r1 %>%
  rename(
    sex.contact = "sex_contact", 
    age.contact = "age_du_contact", 
    rel.contact = "rela_contact",
    rel.contact.other = "autres_relation", 
    freq.contact = "frequence_contact", 
    n.place.contact = "nombre_endroit_personne", 
    type.contact = "type_de_contact", 
    duration.contact = "temps_endroit", 
    age.cat.contact = "cat_age",  
    index.contact = "_index",
    parent.index.contact = "_parent_index",
    submission.id.contact = "_submission__id"
  ) %>%
  mutate(
    age.cat.contact = ifelse(age.cat.contact == "A","(0,18]",
                               ifelse(age.cat.contact == "B","(18,50]",
                                      ifelse(age.cat.contact == "C","50+", age.cat.contact))),
    age.contact = as.numeric(age.contact),
    age.cat.contact2 = as.character(cut(age.contact, c(0,18,50))),
    age.cat.contact2 = ifelse(is.na(age.cat.contact2), "50+", as.character(age.cat.contact2)),
    age.cat.contact2 = ifelse(is.na(age.contact), NA, as.character(age.cat.contact2)),
    age.cat.contact.m = age.cat.contact,
    age.cat.contact.m = ifelse(is.na(age.cat.contact.m), age.cat.contact2, age.cat.contact.m),
    #age.cat.contact.m = factor(age.cat.contact.m, levels=
    #                            c("(0,18]", "(18,50]","50+")),
    age.cat.contact2 = as.character(cut(age.contact, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    age.cat.contact2 = ifelse(is.na(age.cat.contact2), "80+", as.character(age.cat.contact2)),
    age.cat.contact2 = ifelse(is.na(age.contact), NA, as.character(age.cat.contact2)),
    age.cat.contact2 = factor(age.cat.contact2, levels=
                       c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
                         "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
                         "(65,70]" ,"(70,75]","(75,80]", "80+")),
    rel.contact.other = ifelse(rel.contact.other %in% c("VOISINE","VOISIN"), "voisin", rel.contact.other),
    rel.contact = ifelse(rel.contact.other %in% "voisin", "voisin" ,rel.contact),
    sex.contact = case_when(
      sex.contact == "F" ~ "female",
      sex.contact == "M" ~ "male",
      TRUE ~ as.character(sex.contact)
    ),
    freq.contact = case_when(
      freq.contact == "tout_le_jour" ~ "daily",
      freq.contact == "Presque_tout_les_jours" ~ "almost_daily",
      freq.contact == "Une_fois_par_Semaine" ~ "weekly",
      freq.contact == "Au_moins_une_fois_par_mois" ~ "at_least_monthly",
      freq.contact == "Moins_dune_fois_par_mois" ~ "less_than_monthly",
      TRUE ~ as.character(freq.contact)
    ),
    freq.contact = factor(freq.contact, levels=c("daily", "almost_daily","weekly","at_least_monthly", "less_than_monthly")),
    duration.contact = case_when(
      duration.contact == "moins5min" ~ "<5m",
      duration.contact == "entre5et15min" ~ "5-15m",
      duration.contact == "entre15minet1h" ~ "15min-1h",
      duration.contact == "entre1et4h" ~ "1-4h",
      duration.contact == "plus4h" ~ ">4h",
      TRUE ~ as.character(duration.contact)
    ),
    duration.contact = factor(duration.contact, levels=c("<5m", "5-15m","15m-1h","1-4h", ">4h")),
    type.contact = case_when(
      type.contact == "physique" ~"physical",
      type.contact == "nonphysique" ~"non_physical",
      TRUE ~ as.character(type.contact)
    ),
    rel.contact = case_when(
      rel.contact == "ami" ~ "friend",
      rel.contact %in% c("petitamie", "Autre_relation") ~ "other",
      rel.contact == "collegue_ecole" ~ "schoolmate",
      rel.contact == "collegue_travail" ~ "colleague",
      rel.contact == "membre_menage" ~ "household_member",
      rel.contact == "autre_membre_famille" ~ "other_family_member",
      rel.contact == "voisin" ~ "neighbour",
    TRUE ~ as.character(rel.contact)
    )
    ) %>%
  select(c(
    index.contact,
    parent.index.contact,
    submission.id.contact,
    sex.contact,
    age.contact, 
   # age.cat.contact,
    age.cat.contact.m,
    age.cat.contact2,
    rel.contact,
    rel.contact.other,
    type.contact,
    duration.contact,
    freq.contact,
    n.place.contact)
  ) %>%
  rename(
    age.cat.contact = "age.cat.contact.m" 
  )

# Check datasets R1
sapply(dat%>% select(Sex_contact,Age_du_contact, rela_contact,Autres_relation,Frequence_contact,nombre_endroit_personne,type_de_contact,temps_endroit,cat_age), function(x) table(x))
sapply(mix_contact_r1 %>% select(sex.contact,age.contact, rel.contact,rel.contact.other,freq.contact,n.place.contact,type.contact,duration.contact,age.cat.contact), function(x) table(x))

# Social mixing R2
dat = mix_contact_r2
names(mix_contact_r2) = tolower(names(mix_contact_r2))
names(mix_contact_r2) = gsub(" ", "_", names(mix_contact_r2))

mix_contact_r2 = mix_contact_r2 %>%
  rename(
    sex.contact = "sex_contact", 
    age.contact = "age_du_contact", 
    rel.contact = "rela_contact",
    rel.contact.other = "autres_relation", 
    freq.contact = "frequence_contact", 
    n.place.contact = "nombre_endroit_personne", 
    type.contact = "type_de_contact", 
    duration.contact = "temps_endroit", 
    #age.cat.contact = "cat_age",  
    index.contact = "_index",
    parent.index.contact = "_parent_index",
    submission.id.contact = "_submission__id"
  ) %>%
   mutate(
  #   age.cat.contact = ifelse(age.cat.contact == "A","(0,18]",
  #                            ifelse(age.cat.contact == "B","(18, 50]",
  #                                   ifelse(age.cat.contact == "C","50+", age.cat.contact))),
    age.contact = as.numeric(age.contact),
    age.cat.contact = as.character(cut(age.contact, c(0,18,50))),
    age.cat.contact = ifelse(is.na(age.cat.contact), "50+", as.character(age.cat.contact)),
    age.cat.contact = ifelse(is.na(age.contact), NA, as.character(age.cat.contact)),
    # age.cat.contact.m = age.cat.contact,
    # age.cat.contact.m = ifelse(is.na(age.cat.contact.m), age.cat.contact2, age.cat.contact.m),
    #age.cat.contact.m = factor(age.cat.contact.m, levels=
    #                            c("(0,18]", "(18,50]","50+")),
    age.cat.contact2 = as.character(cut(age.contact, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    age.cat.contact2 = ifelse(is.na(age.cat.contact2), "80+", as.character(age.cat.contact2)),
    age.cat.contact2 = ifelse(is.na(age.contact), NA, as.character(age.cat.contact2)),
    age.cat.contact2 = factor(age.cat.contact2, levels=
                                c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
                                  "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
                                  "(65,70]" ,"(70,75]","(75,80]", "80+")),
    rel.contact.other = ifelse(rel.contact.other %in% c("VOISINE","VOISIN"), "voisin", rel.contact.other),
    rel.contact = ifelse(rel.contact.other %in% "voisin", "voisin" ,rel.contact),
    sex.contact = case_when(
      sex.contact == "F" ~ "female",
      sex.contact == "M" ~ "male"
    ),
    freq.contact = case_when(
      freq.contact == "tout_le_jour" ~ "daily",
      freq.contact == "Presque_tout_les_jours" ~ "almost_daily",
      freq.contact == "Une_fois_par_Semaine" ~ "weekly",
      freq.contact == "Au_moins_une_fois_par_mois" ~ "at_least_monthly",
      freq.contact == "Moins_dune_fois_par_mois" ~ "less_than_monthly",
      TRUE~as.character(freq.contact)
    ),
    freq.contact = factor(freq.contact, levels=c("daily", "almost_daily","weekly","at_least_monthly", "less_than_monthly")),
    duration.contact = case_when(
      duration.contact == "moins5min" ~ "<5m",
      duration.contact == "entre5et15min" ~ "5-15m",
      duration.contact == "entre15minet1h" ~ "15m-1h",
      duration.contact == "entre1et4h" ~ "1-4h",
      duration.contact == "plus4h" ~ ">4h"
    ),
    duration.contact = factor(duration.contact, levels=c("<5m", "5-15m","15m-1h","1-4h", ">4h")),
    type.contact = case_when(
      type.contact == "physique" ~"physical",
      type.contact == "nonphysique" ~"non_physical"
    ),
    rel.contact = case_when(
      rel.contact == "ami" ~ "friend",
      rel.contact %in% c("petitamie", "Autre_relation") ~ "other",
      rel.contact == "collegue_ecole" ~ "schoolmate",
      rel.contact == "collegue_travail" ~ "colleague",
      rel.contact == "membre_menage" ~ "household_member",
      rel.contact == "autre_membre_famille" ~ "other_family_member",
      rel.contact == "voisin" ~ "neighbour")
  ) %>%
  select(c(
    index.contact,
    parent.index.contact,
    submission.id.contact,
    sex.contact,
    age.contact, 
    age.cat.contact,
    age.cat.contact2,
    rel.contact,
    rel.contact.other,
    type.contact,
    duration.contact,
    freq.contact,
    n.place.contact)
  )

# Social mixing R4
dat = mix_contact_r4
names(mix_contact_r4) = tolower(names(mix_contact_r4))
names(mix_contact_r4) = gsub(" ", "_", names(mix_contact_r4))


mix_contact_r4 = mix_contact_r4 %>%
  rename(
    sex.contact = "sex_contact", 
    age.contact = "age_du_contact", 
    rel.contact = "rela_contact",
    rel.contact.other = "autres_relation", 
    freq.contact = "frequence_contact", 
    n.place.contact = "nombre_endroit_personne", 
    type.contact = "type_de_contact", 
    duration.contact = "temps_endroit", 
    #age.cat.contact = "cat_age",  
    index.contact = "_index",
    parent.index.contact = "_parent_index",
    submission.id.contact = "_submission__id"
  ) %>%
  mutate(
    #   age.cat.contact = ifelse(age.cat.contact == "A","(0,18]",
    #                            ifelse(age.cat.contact == "B","(18, 50]",
    #                                   ifelse(age.cat.contact == "C","50+", age.cat.contact))),
    age.contact = as.numeric(age.contact),
    age.cat.contact = as.character(cut(age.contact, c(0,18,50))),
    age.cat.contact = ifelse(is.na(age.cat.contact), "50+", as.character(age.cat.contact)),
    age.cat.contact = ifelse(is.na(age.contact), NA, as.character(age.cat.contact)),
    # age.cat.contact.m = age.cat.contact,
    # age.cat.contact.m = ifelse(is.na(age.cat.contact.m), age.cat.contact2, age.cat.contact.m),
    #age.cat.contact.m = factor(age.cat.contact.m, levels=
    #                            c("(0,18]", "(18,50]","50+")),
    age.cat.contact2 = as.character(cut(age.contact, c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))),
    age.cat.contact2 = ifelse(is.na(age.cat.contact2), "80+", as.character(age.cat.contact2)),
    age.cat.contact2 = ifelse(is.na(age.contact), NA, as.character(age.cat.contact2)),
    age.cat.contact2 = factor(age.cat.contact2, levels=
                                c("(0,5]", "(5,10]","(10,15]","(15,20]","(20,25]","(25,30]","(30,35]",
                                  "(35,40]", "(40,45]", "(45,50]", "(50,55]","(55,60]","(60,65]",
                                  "(65,70]" ,"(70,75]","(75,80]", "80+")),
    rel.contact.other = ifelse(rel.contact.other %in% c("VOISINE","VOISIN"), "voisin", rel.contact.other),
    rel.contact = ifelse(rel.contact.other %in% "voisin", "voisin" ,rel.contact),
    sex.contact = case_when(
      sex.contact == "F" ~ "female",
      sex.contact == "M" ~ "male"
    ),
    freq.contact = case_when(
      freq.contact == "tout_le_jour" ~ "daily",
      freq.contact == "Presque_tout_les_jours" ~ "almost_daily",
      freq.contact == "Une_fois_par_Semaine" ~ "weekly",
      freq.contact == "Au_moins_une_fois_par_mois" ~ "at_least_monthly",
      freq.contact == "Moins_dune_fois_par_mois" ~ "less_than_monthly"
    ),
    freq.contact = factor(freq.contact, levels=c("daily", "almost_daily","weekly","at_least_monthly", "less_than_monthly")),
    duration.contact = case_when(
      duration.contact == "moins5min" ~ "<5m",
      duration.contact == "entre5et15min" ~ "5-15m",
      duration.contact == "entre15minet1h" ~ "15m-1h",
      duration.contact == "entre1et4h" ~ "1-4h",
      duration.contact == "plus4h" ~ ">4h"
    ),
    duration.contact = factor(duration.contact, levels=c("<5m", "5-15m","15m-1h","1-4h", ">4h")),
    type.contact = case_when(
      type.contact == "physique" ~"physical",
      type.contact == "nonphysique" ~"non_physical"
    ),
    rel.contact = case_when(
      rel.contact == "ami" ~ "friend",
      rel.contact %in% c("petitamie", "Autre_relation") ~ "other",
      rel.contact == "collegue_ecole" ~ "schoolmate",
      rel.contact == "collegue_travail" ~ "colleague",
      rel.contact == "membre_menage" ~ "household_member",
      rel.contact == "autre_membre_famille" ~ "other_family_member",
      rel.contact == "voisin" ~ "neighbour")
  ) %>%
  select(c(
    index.contact,
    parent.index.contact,
    submission.id.contact,
    sex.contact,
    age.contact, 
    age.cat.contact,
    age.cat.contact2,
    rel.contact,
    rel.contact.other,
    type.contact,
    duration.contact,
    freq.contact,
    n.place.contact)
  )

############################################################
# REMOVE DUPLICATED IDS
############################################################
dat = mix_gen_r1

mix_gen_r1 = mix_gen_r1 %>% 
  filter(!duplicated(id.individuel.hdss) & !is.na(id.individuel.hdss))

dat = mix_gen_r2
mix_gen_r2 = mix_gen_r2 %>% 
  filter(!duplicated(id.individuel.hdss) & !is.na(id.individuel.hdss))

dat = mix_gen_r4
mix_gen_r4 = mix_gen_r4 %>% 
  filter(!duplicated(id.individuel.hdss) & !is.na(id.individuel.hdss))

############################################################
# Write datasets
save(mix_gen_r1,mix_gen_r2,mix_gen_r4,
          mix_lieu_r1,mix_lieu_r2,mix_lieu_r4,
          mix_contact_r1,mix_contact_r2, mix_contact_r4, file = paste0(OutputDirectoryData,"socialmix.RData"))

