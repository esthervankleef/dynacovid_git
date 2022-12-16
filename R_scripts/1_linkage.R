######################################################
# LINKAGE of SOCIAL MIXING DATA WITH CHARACTERISTICS
######################################################

# Author: E van Kleef
# Date: 23 November 2021

rm(list=ls())

# SET OUTPUT DIRECTORY
OutputDirectory <- "./Outputs/"
OutputDirectoryDataCheck <- "../Data/Raw/To_check/"
OutputDirectoryData <- "../Data/Clean/"


# Source functions files
source("./R_scripts/functions.R")
source("./R_scripts/multiplot.R")

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, stringi,Hmisc)

load("../Data/Clean/characteristics.RData")
load("../Data/Clean/socialmix.RData")
load("../Data/Clean/sero.RData")

pos_r2 = read_xlsx("../Data/Raw/R2_sero_id_pos.xlsx", sheet=1)
pos_r3 = read_xlsx("../Data/Raw/R3_sero_id_pos.xlsx", sheet=1)
pos_r4 = read_xlsx("../Data/Raw/R4_sero_id_pos.xlsx", sheet=1)

pos_r1 = mix_gen_r1 %>% select(
  id.individuel.hdss,
  ig.total.result) %>%
  filter(ig.total.result=="AC PRESENT") %>%
  mutate(
    ig.total.result = "positif"
  ) 


#-----------------------------------------------------------------------------------------------#
#---------Merging mix_gen and mix_lieu datasets by index and parent.index.place-----------------#
#-----------------------------------------------------------------------------------------------#

# R1 
#For not all places, individual data is listed. It seems these are for the 29 duplicated hdss individuel. I am now
# using the openHDS individuel id, so even more missing then. Just leave it for now.
# mix_gen_lieu_r1 <- left_join(mix_gen_r1, mix_lieu_r1,
#                           by = c("index"="parent.index.place"))
# 
# length(unique(mix_gen_lieu_r1$id.individuel.hdss))
# length(unique(mix_gen_r1$id.individuel.hdss))
# length(unique(mix_lieu_r1$parent.index.place))-length(unique(mix_gen_lieu_r1$index))
# 

# For now use data that can be merged
mix_gen_lieu_r1 <- merge(mix_gen_r1, mix_lieu_r1, 
                             by.x = "index", by.y = "parent.index.place", all=FALSE)

# R2
mix_gen_lieu_r2 <- merge(mix_gen_r2, mix_lieu_r2, 
                         by.x = "index", by.y = "parent.index.place", all=FALSE)

# R4
mix_gen_lieu_r4 <- merge(mix_gen_r4, mix_lieu_r4, 
                         by.x = "index", by.y = "parent.index.place", all=FALSE)

#-----------------------------------------------------------------------------------------------#
#----------Merging enquete_lieu with contacts data by index.place and parent.index.contact-------#
#-----------------------------------------------------------------------------------------------#

# R1
mix_all_r1 <- merge(mix_gen_lieu_r1, mix_contact_r1, 
                                by.x = "index.place" , by.y = "parent.index.contact" , all = FALSE)

##To count unique ids we remain with after merging three data sets
length(unique(mix_all_r1$index))
length(unique(mix_gen_r1$index)) - length(unique(mix_all_r1$index)) # Of 45 individuals we have no contacts listed. Does that mean they registered 0 contacts?

# R2
mix_all_r2 <- merge(mix_gen_lieu_r2, mix_contact_r2, 
                    by.x = "index.place" , by.y = "parent.index.contact" , all = FALSE)

##To count unique ids we remain with after merging three data sets
length(unique(mix_all_r2$index))
length(unique(mix_gen_r2$index)) - length(unique(mix_all_r2$index)) # Of 42 individuals we have no contacts listed. Does that mean they registered 0 contacts?

# R4
mix_all_r4 <- merge(mix_gen_lieu_r4, mix_contact_r4, 
                    by.x = "index.place" , by.y = "parent.index.contact" , all = FALSE)

##To count unique ids we remain with after merging three data sets
length(unique(mix_all_r4$index))
length(unique(mix_gen_r4$index)) - length(unique(mix_all_r4$index)) # Of 39 individuals we have no contacts listed. Does that mean they registered 0 contacts?


#-----------------------------------------------------------------------------------------------#
#-------------------Merging Hdss with full contacts dataset-------------------------------------#
#-----------------------------------------------------------------------------------------------#
# R1
# Join with openhds.2.individual.id
dat = char %>% filter(!is.na(id.individuel.hdss))

mix_all_hdss_r1 <- merge(dat, mix_all_r1, 
                                    by = "id.individuel.hdss",all = FALSE)

#mix_all_hdss_r1 <- mix_all_hdss_r1 %>%
#  select(-c(id.individuel.hdss.x)) %>%
#  rename(
#    id.individuel.hdss = "id.individuel.hdss.y"
#  )

# R2

# first join with id.individuel.hdss
dat1 <- merge(dat, mix_all_r2, 
                         by = "id.individuel.hdss",all = FALSE)

length(unique(dat1$id.individuel.hdss))
length(unique(mix_all_r2$id.individuel.hdss))

dat2 <- merge(dat, mix_all_r2, 
                         by.x = "vrai.id",by.y = "id.individuel.hdss",all = FALSE)
dat3 <- merge(dat, mix_all_r2, 
              by.x = "id.individuel.hdss",by.y = "id.individuel.hdss.adj",all = FALSE)
dat4 <- merge(dat, mix_all_r2, 
              by.x = "vrai.id",by.y = "id.individuel.hdss.adj",all = FALSE) 

dat2 = dat2 %>%
  select(names(dat1))

dat3 = dat3 %>% 
  rename(
    id.individuel.hdss.adj = "id.individuel.hdss",
    id.individuel.hdss = "id.individuel.hdss.y"
  )
dat3 = dat3 %>%
  select(names(dat1))

dat4 = dat4 %>% 
  rename(
    id.individuel.hdss = "id.individuel.hdss.x",
    id.individuel.hdss.adj = "vrai.id"
  ) %>%
  mutate(
    vrai.id = id.individuel.hdss.adj
  )

dat4 = dat4 %>%
  select(names(dat1))

mix_all_hdss_r2 = rbind(dat1,dat2,dat3,dat4)
mix_all_hdss_r2 = mix_all_hdss_r2 %>%
  filter(!duplicated(index.contact))

# How many missing
length(unique(mix_all_r2$id.individuel.hdss)) - length(unique(mix_all_hdss_r2$id.individuel.hdss)) # 168

# R4

# first join with id.individuel.hdss
dat1 <- merge(dat, mix_all_r4, 
              by = "id.individuel.hdss",all = FALSE)

length(unique(dat1$id.individuel.hdss))
length(unique(mix_all_r4$id.individuel.hdss))

dat2 <- merge(dat, mix_all_r4, 
              by.x = "vrai.id",by.y = "id.individuel.hdss",all = FALSE)
dat3 <- merge(dat, mix_all_r4, 
              by.x = "id.individuel.hdss",by.y = "id.individuel.hdss.adj",all = FALSE)
dat4 <- merge(dat, mix_all_r4, 
              by.x = "vrai.id",by.y = "id.individuel.hdss.adj",all = FALSE) 

dat2 = dat2 %>%
  select(names(dat1))

dat3 = dat3 %>% 
  rename(
    id.individuel.hdss.adj = "id.individuel.hdss",
    id.individuel.hdss = "id.individuel.hdss.y"
  )
dat3 = dat3 %>%
  select(names(dat1))

dat4 = dat4 %>% 
  rename(
    id.individuel.hdss = "id.individuel.hdss.x",
    id.individuel.hdss.adj = "vrai.id"
  ) %>%
  mutate(
    vrai.id = id.individuel.hdss.adj
  )

dat4 = dat4 %>%
  select(names(dat1))

mix_all_hdss_r4 = rbind(dat1,dat2,dat3,dat4)
mix_all_hdss_r4 = mix_all_hdss_r4 %>%
  filter(!duplicated(index.contact))

# How many missing
length(unique(mix_all_r4$id.individuel.hdss)) - length(unique(mix_all_hdss_r4$id.individuel.hdss)) # 265

length(which(unique(mix_all_hdss_r4$id.individuel.hdss) %in% unique(mix_all_r4$id.individuel.hdss)))

# 
# contact_number = as.data.frame(table(mix_all_hdss_r4$id.individuel.hdss))
# colnames(contact_number) = c("id.indiv.hdss","contact.nbr")
# hist(contact_number$contact.nbr)

# Link with seroprevalence results
sero_r1 = left_join(sero_r1, pos_r1)
sero_r2 = left_join(sero_r2, pos_r2)
sero_r3 = left_join(sero_r3, pos_r3)
sero_r4 = left_join(sero_r4, pos_r4)

table(sero_r2$ig.total.result)
table(sero_r3$ig.total.result)
table(sero_r4$ig.total.result)

length(which(!pos_r2$id.individuel.hdss%in%sero_r2$id.individuel.hdss))
length(which(!pos_r3$id.individuel.hdss%in%sero_r3$id.individuel.hdss))
length(which(!pos_r4$id.individuel.hdss%in%sero_r4$id.individuel.hdss))

miss = which(!pos_r2$id.individuel.hdss%in%sero_r2$id.individuel.hdss)
#View(pos_r2[miss,])

##############################################################################
# Save data
save(mix_all_hdss_r1, mix_all_hdss_r2, mix_all_hdss_r4,
     file = paste0(OutputDirectoryData,"socmix_linked.RData"))



