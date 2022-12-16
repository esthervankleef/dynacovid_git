######################################################
# ANALYSIS of SOCIAL MIXING DATA WITH CHARACTERISTICS
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
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, stringi,Hmisc, ggtext)

load("../Data/Clean/characteristics.RData")
load("../Data/Clean/socialmix.RData")
load("../Data/Clean/socmix_hdss_linked.RData")
load("../Data/Clean/sero_hdss_linked.RData")

sero_sum = read_xlsx("../Data/Clean/sero_summary.xlsx")

sero_sum = sero_sum %>%
  mutate(ig_pos_cohort_perc=round(as.numeric(ig_pos_cohort_perc), 2),
         ig_pos_hh_perc=round(as.numeric(ig_pos_hh_perc), 2),
         ig_neg_cohort = cohort_incl-ig_pos_cohort) 

n_contacts_r1 = n_contacts(mix_all_hdss_r1, char, round=1) 
n_contacts_r2 = n_contacts(mix_all_hdss_r2, char, round=2) 
n_contacts_r4 = n_contacts(mix_all_hdss_r4, char, round=4) 
n_contacts_all = rbind(n_contacts_r1, n_contacts_r2)

# mixing by age for plotting
groups=c(quo(age.cat2))
n_contacts_age = mix_age(list(n_contacts_r1,n_contacts_r2), var="age.cat2")

n_contacts_age = n_contacts_age %>% filter(!is.na(age.cat2))

# mixing by sex for plotting
groups=c(quo(sex.dc))
n_contacts_sex = mix_age(list(n_contacts_r1,n_contacts_r2), var="sex.dc")
n_contacts_sex = n_contacts_sex %>% filter(!is.na(sex.dc))

# mixing by education for plotting
groups=c(quo(education.dc.m))
n_contacts_education = mix_age(list(n_contacts_r1,n_contacts_r2), var="education.dc.m")
n_contacts_education = n_contacts_education %>% filter(!is.na(education.dc.m)) %>%
  mutate(education.dc.m = factor(education.dc.m, levels=c("none","primary", "secondary", "higher(university/specialised)",
                                                           "all"),
         labels=c("None","Primary", "Secondary", "Higher",
                  "All"))
         )

# mixing by ethnie for plotting
groups=c(quo(ethnie.dc.m))
n_contacts_ethnie = mix_age(list(n_contacts_r1,n_contacts_r2), var="ethnie.dc.m")
n_contacts_ethnie= n_contacts_ethnie %>% filter(!is.na(ethnie.dc.m), !is.na(sd))

# Serology datasets by age for plotting
sero_long <- gather(sero_sum %>% select(round, cohort_incl,ig_pos_cohort,ig_pos_cohort_perc),
                    cohort, n, cohort_incl:ig_pos_cohort, factor_key=TRUE)

sero_long$ig_pos_cohort_perc[1:4] = NA 

# Trend in seropositivity by age - 
pos_age = sero_age(list(sero_hdss_r1,sero_hdss_r2,sero_hdss_r3,sero_hdss_r4))
ci = simpasym(pos_age$n,pos_age$n.pos/pos_age$n)
pos_age = cbind(pos_age, ci)

# Trend in seropositivity by age - larger agegroups
pos_age2 = sero_age2(list(sero_hdss_r1,sero_hdss_r2,sero_hdss_r3,sero_hdss_r4))
ci = simpasym(pos_age2$n,pos_age2$n.pos/pos_age2$n)
pos_age2 = cbind(pos_age2, ci)

# Contact distribution
##################################################################################
 
p1 = pncontacts(mix_all_hdss_r1, round = "R1 social mixing")
p2 = pncontacts(mix_all_hdss_r2, round = "R2 social mixing")
p4 = pncontacts(mix_all_hdss_r4, round = "R4 social mixing")

multiplot(p1,p2,p4)

# Plot mixing data by age
p.nc1 = ggplot(n_contacts_age, aes(x=age.cat2,y=mean, col=factor(round), group=factor(round))) +geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.05, size=1, linetype=2)+
  scale_y_continuous(limits=c(0,6)) +
  labs(title=paste0("Number of close contacts by age group"), x = "",y="n contacts") +
  theme_bw()  +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA),
        legend.position = c(0.1, .15),
        axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        legend.text= element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(size=16))+
  scale_color_manual(name = "Wave", values = c("darkcyan", "coral", "yellow","darkslategray3"))
p.nc1
n_contacts_all %>%filter(!is.na(age.cat2)) %>%
ggplot(., aes(x=age.cat2,y=n.contact, fill=factor(round)))+geom_jitter(alpha=0.2) +geom_boxplot()+
  scale_y_continuous(limits=c(0,6)) +
  labs(title=paste0("Number of close contacts by age group"), x = "",y="n contacts") +
  theme_bw()  +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA),
        legend.position = c(0.1, .15),
        axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        legend.text= element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(size=16))+
  scale_fill_manual(name = "Wave", values = c("darkcyan", "coral", "yellow","darkslategray3"))


# Plot mixing data by sex
p.nc2 = ggplot(n_contacts_sex, aes(x=sex.dc,y=mean, col=factor(round), group=factor(round))) +geom_point(size=4)+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.05, size=1, linetype=2)+
  scale_y_continuous(limits=c(0,6)) +
  labs(title=paste0("Number of close contacts by sex"), x = "",y="n contacts") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA),
        legend.position = c(0.1, .15),
        axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        legend.text= element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(size=16))+
  scale_color_manual(name = "Wave", values = c("darkcyan", "coral", "yellow","darkslategray3"))
p.nc2

# Plot mixing data by education
p.nc3 = ggplot(n_contacts_education, aes(x=education.dc.m,y=mean, col=factor(round), group=factor(round))) +geom_point(size=4) +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.05, size=1, linetype=2)+
  scale_y_continuous(limits=c(0,6)) +
  labs(title=paste0("Number of close contacts by education"), x = "",y="n contacts") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA),
        legend.position = c(0.1, .15),
        axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        legend.text= element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(size=16))+
  scale_color_manual(name = "Wave", values = c("darkcyan", "coral", "yellow", "green","darkslategray3"))
p.nc3

# Plot mixing data by ethnie
p.nc4 = ggplot(n_contacts_ethnie, aes(x=ethnie.dc.m,y=mean, col=factor(round), group=factor(round))) +geom_point(size=4) + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.05, size=1, linetype=2)+
  labs(title=paste0("Number of close contacts by ethnicity"), x = "",y="n contacts") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA),
        legend.position = c(0.1, .15),
        axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        legend.text= element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(size=16))+
  scale_color_manual(name = "Wave", values = c("darkcyan", "coral", "yellow", "green","darkslategray3"))
p.nc4
multiplot(p.nc1,p.nc2, p.nc3,p.nc4, cols=2)

# Plot sero data
p5 = ggplot(sero_long, aes(x=round,y=n, fill=cohort))+geom_bar(stat="identity", position="dodge")+
  geom_text(aes(x = sero_long$round, y = sero_long$n, 
                label = sero_long$ig_pos_cohort_perc,
                hjust=-0.3, vjust=-0.5),size=4)+
  labs(title=paste0("Seropositivity over time"), x = "Wave") +
  theme(panel.background=element_rect(colour = "black", fill = "white"),
        legend.position = c(0.85, .85),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        legend.text= element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(size=16))+
  scale_fill_manual(name = "", values = c("darkcyan", "coral"), labels = c("Cohort", "IgG positive"))

p5  

# Trend in seropositivity by age - lines
ggplot(pos_age, aes(x=round,y=n.pos/n, col=age.cat)) + geom_line(size=1)+geom_point(size=2) + 
  labs(title=paste0("Age distribution seropositivity"), x = "Wave",y="% positive") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA))+
  geom_text(aes(x = pos_age$round, y = pos_age$n.pos/pos_age$n, 
                label = paste(pos_age$n),
                hjust=0.2, vjust=-0.5),size=4)



p7= ggplot(pos_age2, aes(x=round,y=n.pos/n, col=age.cat)) +geom_point(size=3) + geom_line(size=1.2)+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.05, size=1, linetype=2)+
  labs(title=paste0("Seropositivity by age"), x = "Wave",y="% positive") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA),
        legend.position = c(0.1, .85),
        axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        legend.text= element_text(size=12),
        legend.title= element_text(size=12),
        plot.title = element_text(size=16))+
  scale_color_manual(name = "", values = c("darkslategray3","darkcyan", "coral"))#+
p7
 # geom_text(aes(x = pos_age2$round, y = pos_age2$n.pos/pos_age2$n, 
#                label = paste(pos_age2$n),
#                hjust=0.2, vjust=-0.5),size=4)

p6 = ggplot(pos_age, aes(x=round,y=n.pos/n, col=age.cat)) + geom_line(size=1)+geom_point(size=2) + 
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0, size=1)+
  labs(title=paste0("Age distribution seropositivity"), x = "Round",y="% positive") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA))+
  geom_text(aes(x = pos_age$round, y = pos_age$n.pos/pos_age$n, 
                label = paste(pos_age$n),
                hjust=0.2, vjust=-0.5),size=4)+
  facet_wrap(~age.cat, nrow=4)+
  guides(color ="none")
p6

# Trend in seropositivity by age - stacked
ggplot(pos_age, aes(x=age.cat,y=n.pos/n, fill=factor(round))) + geom_bar(stat="identity",position="dodge")+ 
  labs(title=paste0("Age distribution seropositivity - ", " (n=", sum(d$n), ")"), x = "Age",y="% positive") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA))+
  geom_text(aes(x = pos_age$age.cat, y = pos_age$n.pos/pos_age$n, 
                label = pos_age$n,
                hjust=0, vjust=-0.5),size=4)
  

# Age distribution over time
ggplot(pos_age, aes(x=round,y=n.pos)) + geom_bar(stat="identity")+
  labs(title=paste0("Age distribution seropositivity - "), x = "Round",y="% positive") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA))+
  # geom_text(aes(x = pos_age$round, y = pos_age$n, 
  #               label = round(pos_age$n.pos/pos_age$n,2),
  #               hjust=0, vjust=-0.5,size=4))+
  facet_wrap(~age.cat, nrow=4)



# Save plots
pdf(paste0(OutputDirectory,"/Plots/ncontacts.pdf"), width=7,height=6)
multiplot(p1,p2,p4, cols=1)
dev.off()

pdf(paste0(OutputDirectory,"/Plots/sero_time.pdf"), width=8,height=6)
print(p5)
dev.off()

png(paste0(OutputDirectory,"/Plots/sero_time.png"), width=1800,height = 1500,res=300)
print(p5)
dev.off()

pdf(paste0(OutputDirectory,"/Plots/sero_age_time.pdf"), width=10,height=8)
print(p6)
dev.off()

pdf(paste0(OutputDirectory,"/Plots/mix_age2_time.pdf"), width=10,height=8)
print(p.nc1)
dev.off()

png(paste0(OutputDirectory,"/Plots/mix_age2_time.png"), width=1800,height = 1500,res=300)
print(p.nc1)
dev.off()

png(paste0(OutputDirectory,"/Plots/mix_sex_time.png"), width=1800,height = 1500,res=300)
print(p.nc2)
dev.off()

png(paste0(OutputDirectory,"/Plots/mix_education_time.png"), width=1800,height = 1500,res=300)
print(p.nc3)
dev.off()

png(paste0(OutputDirectory,"/Plots/sero_age2_time.png"), width=1800,height = 1500,res=300)
print(p7)
dev.off()

pdf(paste0(OutputDirectory,"/Plots/sero_age2_time.pdf"), width=15,height=10)
print(p7)
dev.off()


