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
pacman::p_load(readxl, writexl, dplyr,ggplot2, tidyverse, RColorBrewer,viridis, fields, gridExtra,
               lubridate)


####################################################################################################################################################
###############################                                           FIGURE 1                                   ###############################
####################################################################################################################################################
Sys.setlocale("LC_TIME", "C")
intervention_data <- read_excel(paste0(OutputDirectoryData,"drc_measures.xlsx"),sheet = "measureRDC")
time_data <- intervention_data %>% mutate(Start=ymd(Start_and_End)) 

### Code to remove the labels of either the x-axis or the y-axis-------
removeGrid <- function(x = TRUE, y = TRUE) {
  p <- ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  if (x) {
    p <- p +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }
  if (y) {
    p <- p +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }
  p
}

### For shedding colors of the waves -------------------------------
myColors <- brewer.pal(4,"RdYlBu")
mycolors <- c(myColors[1:4])#,"#2b8cbe")
### Data for the Waves -----------------------------------

df<-data.frame(xmin=as.Date(c("2020-09-24",'2021-01-22', '2021-04-24', '2021-08-14')),
               xmax=as.Date(c('2020-12-23','2021-02-24','2021-05-22', '2021-09-28')),
               ymin=c(0.5,0.5),
               ymax=c(10.5,10.5),
               Comix=c("Wave 1","Wave 2", "Wave 3 (sero only)", "Wave 4"))
### ------------------------------------------------------

dat <- time_data
dat$intervention_measure

plot1<- dat %>%
  ggplot(aes(x = Start, y = y_var, group=intervention_measure)) +
  geom_point(size=2) +
  geom_line()+
  annotate("point",
           dat$Start[c(14,20)],
           dat$y_var[c(14,20)],
           col="white")+
  geom_vline(xintercept = as.Date("2020-12-15"), lty=2, col="blue",size=1.5, alpha=0.4)+
  geom_vline(xintercept = as.Date("2021-06-15"), lty=2, col="blue",size=1.5,alpha=0.4)+
  #geom_text(aes(x = as.Date(dat$Start_and_End[1]), y = dat$y_var[1], label = dat$intervention_measure[1],
  #              hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[3]), y = dat$y_var[3], label = dat$intervention_measure[3],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[5]), y = dat$y_var[5], label = dat$intervention_measure[5],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[7]), y = dat$y_var[7], label=dat$intervention_measure[7],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[9]), y = dat$y_var[9]+0.3, label =dat$intervention_measure[9],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[11]), y = dat$y_var[11], label =dat$intervention_measure[11],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[13]), y = dat$y_var[13]+0.3, label =dat$intervention_measure[13],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[14]), y = dat$y_var[14], label =dat$intervention_measure[14],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[16]), y = dat$y_var[16], label =dat$intervention_measure[16],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[18]), y = dat$y_var[18], label =dat$intervention_measure[18],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[20]), y = dat$y_var[20], label =dat$intervention_measure[20],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[22]), y = dat$y_var[22], label =dat$intervention_measure[22],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[24]), y = dat$y_var[24], label =dat$intervention_measure[24],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[26]), y = dat$y_var[26]+0.3, label =dat$intervention_measure[26],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[28]), y = dat$y_var[28], label =dat$intervention_measure[28],
                hjust=-0.02, vjust=-0.5),size=4)+
 #geom_text(aes(x = as.Date(dat$Start_and_End[30]), y = dat$y_var[30], label =dat$intervention_measure[30],
#                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[30]), y = dat$y_var[31], label =dat$intervention_measure[30],
                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[32]), y = dat$y_var[32]+0.3, label =dat$intervention_measure[32],
                                hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[33]), y = dat$y_var[33], label =dat$intervention_measure[33],
                hjust=-0.02, vjust=-0.5),size=4)+
  # geom_text(aes(x = as.Date(dat$Start_and_End[34]), y = dat$y_var[34], label =dat$intervention_measure[34],
  #               hjust=-0.02, vjust=-0.5),size=4)+
  geom_text(aes(x = as.Date(dat$Start_and_End[35]), y = dat$y_var[35], label =dat$intervention_measure[35],
                hjust=-0.02, vjust=-0.5),size=4)

plot1 <- plot1 +  scale_x_date(date_breaks = "4 weeks",date_labels = "%b - %d")+ 
  scale_y_continuous(limits = c(0.5,10.5))+
  geom_rect(data=df,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=Comix),
            alpha=0.2,inherit.aes=FALSE) + scale_fill_manual(values=mycolors)

plot1 <- plot1 +ylab(NULL) + theme_minimal()+theme()
plot2 <- plot1 +  theme(axis.title.x=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),
                        axis.text.x = element_text(angle = -45, hjust = 0.0, size = 12,color = "gray9"))
plot2 <- plot2  + removeGrid(x = FALSE)
  

plot2

pdf(paste0(OutputDirectory,"/Plots/measures.pdf"), width=11,height=6)
plot2
dev.off()

png(paste0(OutputDirectory,"/Plots/measures.png"), width=3200,height=1800, res=300)
plot2
dev.off()

