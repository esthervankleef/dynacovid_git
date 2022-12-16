###########################################
# Functions for social mixing data
###########################################

# Esther van Kleef
# 24 October 2021


#Exploratory plots HDSS data
#####################################

ex_plot = function(data, variable.name,data.name, plot.name, all){
  colp = ifelse(all == "yes", "seagreen", "coral")
  title_add = ifelse(all =="yes", "all", "social-mixing")
  variable_place = which(names(data)==variable.name)
  if(all == "yes"){
  d = as.data.frame(data %>%
    group_by(data[,variable_place]) %>%
    summarise(count = n()) %>%
    mutate(countT = sum(count),
           perc=paste0(round(100*count/countT,2),'%')) %>%
    rename(
      variable.name = `data[, variable_place]`
    )) 
  }else{
    d = as.data.frame(data %>%
      group_by(data[,variable_place]) %>%
      summarise(count = n()) %>%
      mutate(countT = sum(count),
             perc=paste0(round(100*count/countT,2),'%'))) 
  }
    ggplot(d, aes(x=as.factor(d[,1]), y=count)) +
    geom_bar(stat = 'identity', fill=colp) +
    theme_bw() +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"),
          axis.text.x = element_text(angle = 90))+
    # scale_x_discrete("Ethnicity", breaks=factor(unique(data[,variable_place])), 
    #                  labels = factor(unique(data[,variable_place])),
    #                  limits = factor(unique(data[,variable_place])))+
    
    labs(title = paste0(plot.name," - ", title_add," ", data.name, " (N = ",nrow(data),")"),
         x=plot.name, y= "Count") +
    geom_text(aes(label = perc,vjust=-1)) 
}


# Substract last letter
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# Change id_individuel_hdss with letter removed for volunteers
adjust_id_hdss_vol <- function(data){
  # Who is volunteer
  data$id_individuel_hdss_vol = 0
  data$id_individuel_hdss_vol[grep("V", data$id_individuel_hdss)] = 1
  
  # Take last character of id to see if a letter
  data$id_individuel_hdss_last_letter = substrRight(data$id_individuel_hdss, 1)
  toMatch <- c("A", "B", "C","D")
  which_letter = grep(paste(toMatch,collapse="|"), 
                      data$id_individuel_hdss_last_letter)
  
  #Who of volunteers has letter to remove
  data$id_individuel_hdss_vol_letter = 0
  data$id_individuel_hdss_vol_letter[which_letter] = 1
  
  # Remove last letter among those volunteers
  data$id_individuel_hdss_adj = data$id_individuel_hdss
  data$id_individuel_hdss_adj[which_letter] = substring(data$id_individuel_hdss,1, 
                                                              nchar(data$id_individuel_hdss)-1)[which_letter]
  
  # Check if number that changed ID are same as number of volunteers with letter at the end
  data$id_adj_same_id_org = ifelse(data$id_individuel_hdss==data$id_individuel_hdss_adj,1,0)
  return(data)
}

# Check if IDs present in R1
id_in_baseline_var <- function(data){
  data = data %>%
  mutate(
    present_r1 = case_when(
      id_individuel_hdss %in% id_mix_r1 ~ 1,
      TRUE ~ 0
    ),
    present_vrai_r1 = case_when(
      id_individuel_hdss %in% id_mix_vrai_r1 ~ 1,
      TRUE ~ 0
    ),
    present_adj = case_when(
      id_individuel_hdss_adj %in% id_mix_r1 ~ 1,
      TRUE ~ 0
    ),
    present_r3_sero = case_when(
      id_individuel_hdss %in% id_sero_r3 ~ 1,
      TRUE ~ 0
    ),
    present_r3_sero_or_adj = case_when(
      id_individuel_hdss %in% id_sero_r3 |id_individuel_hdss_adj %in% id_sero_r3 ~ 1,
      TRUE ~ 0
    ),
    present_r1_or_adj = case_when(
      id_individuel_hdss_adj %in% id_mix_r1 | id_individuel_hdss %in% id_mix_r1  ~ 1,
      TRUE ~ 0
    ),
    present_r1_sero_or_adj = case_when(
      id_individuel_hdss_adj %in% id_sero_r1 | id_individuel_hdss %in% id_sero_r1  ~ 1,
      TRUE ~ 0
    ),
    present_r1_or_adj_or_vrai = case_when(
      id_individuel_hdss_adj %in% id_mix_r1 | id_individuel_hdss %in% id_mix_r1 |
        id_individuel_hdss %in% id_mix_vrai_r1 ~ 1,
      TRUE ~ 0
    ),
    present_r1_sero_or_adj_or_sero_vrai = case_when(
      id_individuel_hdss_adj %in% id_sero_r1 | id_individuel_hdss %in% id_sero_r1 |
        id_individuel_hdss %in% id_sero_vrai_r1 ~ 1,
      TRUE ~ 0
    ),
    present_r1_mix_sero_or_adj_or_mix_sero_vrai = case_when(
      id_individuel_hdss_adj %in% id_sero_r1 |  id_individuel_hdss_adj %in% id_mix_r1 |
        id_individuel_hdss %in% id_sero_r1 |
        id_individuel_hdss %in% id_sero_vrai_r1 | id_individuel_hdss %in% id_mix_r1 |
        id_individuel_hdss %in% id_mix_vrai_r1 ~ 1,
      TRUE ~ 0
    ),
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero = case_when(
      id_individuel_hdss_adj %in% id_sero_r1 |  id_individuel_hdss_adj %in% id_mix_r1 |
        id_individuel_hdss %in% id_sero_r1 |
        id_individuel_hdss %in% id_sero_vrai_r1 | id_individuel_hdss %in% id_mix_r1 |
        id_individuel_hdss %in% id_mix_vrai_r1|id_individuel_hdss %in% id_sero_r2 ~ 1,
      TRUE ~ 0
    ),
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero = case_when(
      id_individuel_hdss_adj %in% id_sero_r1 |  id_individuel_hdss_adj %in% id_mix_r1 |
        id_individuel_hdss %in% id_sero_r1 |
        id_individuel_hdss %in% id_sero_vrai_r1 | id_individuel_hdss %in% id_mix_r1 |
        id_individuel_hdss %in% id_mix_vrai_r1|id_individuel_hdss %in% id_sero_r3 ~ 1,
      TRUE ~ 0
    ),
    present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero = case_when(
      id_individuel_hdss_adj %in% id_sero_r1 |  id_individuel_hdss_adj %in% id_mix_r1 |
        id_individuel_hdss %in% id_sero_r1 |
        id_individuel_hdss %in% id_sero_vrai_r1 | id_individuel_hdss %in% id_mix_r1 |
        id_individuel_hdss %in% id_mix_vrai_r1|id_individuel_hdss %in% id_sero_r2|
        id_individuel_hdss %in% id_sero_r3 ~ 1,
      TRUE ~ 0
    ),
      present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero = case_when(
        id_individuel_hdss_adj %in% id_sero_r1 |  id_individuel_hdss_adj %in% id_mix_r1 |
          id_individuel_hdss %in% id_sero_r1 |
          id_individuel_hdss %in% id_sero_vrai_r1 | id_individuel_hdss %in% id_mix_r1 |
          id_individuel_hdss %in% id_mix_vrai_r1|id_individuel_hdss %in% id_sero_r2|
          id_individuel_hdss %in% id_sero_r3 | id_individuel_hdss %in% id_sero_r4 ~ 1,
        TRUE ~ 0
    )
  )
  return(data)
}

id_in_baseline = function(d){
  id_present_matrix = data.frame(matrix(nrow=11, ncol=2))
  names(id_present_matrix) = c("id_type", "no")
  id_present_matrix[1,] = c("present_r1", length(d$present_r1[d$present_r1==0]))
  id_present_matrix[2,] = c("present_vrai_r1", length(d$present_r1[d$present_vrai_r1==0]))
  id_present_matrix[3,] = c("present_r3_sero", length(d$present_r1[d$present_r3_sero==0]))
  id_present_matrix[4,] = c("present_r3_sero_or_adj", length(d$present_r1[d$present_r3_sero_or_adj==0]))
  id_present_matrix[5,] = c("present_adj", length(d$present_r1[d$present_adj==0]))
  id_present_matrix[6,] = c("present_r1_or_adj",length(d$present_r1[d$present_r1_or_adj==0]))
  id_present_matrix[7,] = c("present_r1_sero_or_adj", length(d$present_r1[d$present_r1_sero_or_adj==0]))
  id_present_matrix[8,] = c("present_r1_or_adj_or_vrai", length(d$present_r1[d$present_r1_or_adj_or_vrai==0]))
  id_present_matrix[9,] = c("present_r1_sero_or_adj_or_sero_vrai", length(d$present_r1[d$present_r1_sero_or_adj_or_sero_vrai==0]))
  id_present_matrix[10,] = c("present_r1_mix_sero_or_adj_or_mix_sero_vrai", length(d$present_r1[d$present_r1_mix_sero_or_adj_or_mix_sero_vrai==0]))
  id_present_matrix[11,] = c("present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero", length(d$present_r1[d$present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero==0]))
  id_present_matrix[12,] = c("present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero", length(d$present_r1[d$present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r3_sero==0]))
  id_present_matrix[13,] = c("present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero", length(d$present_r1[d$present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero==0]))
  id_present_matrix[14,] = c("present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero", length(d$present_r1[d$present_r1_mix_sero_or_adj_or_mix_sero_vrai_or_r2_sero_r3_sero_r4_sero==0]))
  id_present_matrix = id_present_matrix %>%
    mutate(
      no = as.numeric(no)
    )
  return(id_present_matrix)
}  


# Which IDS present in HDSS or R3 sero
id_hdss_sero_char = function(data){
  data = data %>%
  mutate(
    pres_hdss0 = case_when(
      openhds_2_individual_id %in% ids_hdss0 ~ 1,
      TRUE ~ 0),
    pres_hdss1 = case_when(
      openhds_2_individual_id %in% ids_hdss1 ~ 1,
      TRUE ~ 0),
    pres_hdss2 = case_when(
      openhds_2_individual_id %in% ids_hdss2 ~ 1,
      TRUE ~ 0),
    pres_sero_r3 = case_when(
      id_individuel_hdss %in% id_sero_r3 ~ 1,
      TRUE ~ 0),
    num_pres_hdss = pres_hdss0+pres_hdss1+pres_hdss2,
    which_press_hdss = case_when(
      pres_hdss0==1&pres_hdss1==0&pres_hdss2==0 ~ "onlyin0",
      pres_hdss0==0&pres_hdss1==1&pres_hdss2==0 ~ "onlyin1",
      pres_hdss0==0&pres_hdss1==0&pres_hdss2==1 ~ "onlyin2",
      pres_hdss0==1&pres_hdss1==1&pres_hdss2==0 ~ "0_1",
      pres_hdss0==1&pres_hdss1==0&pres_hdss2==1 ~ "0_2",
      pres_hdss0==0&pres_hdss1==1&pres_hdss2==1 ~ "1_2",
      pres_hdss0==1&pres_hdss1==1&pres_hdss2==1 ~ "0_1_2",
      TRUE ~ "none"
    ),
    which_press_hdss0_sero_r3 = case_when(
      pres_hdss0==0&pres_sero_r3==0 ~ "pas_r0_pas_sero",
      pres_hdss0==1&pres_sero_r3==0 ~ "in_r0_pas_sero",
      pres_hdss0==0&pres_sero_r3==1 ~ "in_sero_pas_r0",
      pres_hdss0==1&pres_sero_r3==1 ~ "in_sero_in_r0",
      TRUE ~ "NA")
  )
  return(data)
}

# Confidence interval proportion
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  return(as.data.frame(out))
}

n_contacts = function(data.contacts, data.char, round){
    d = as.data.frame(table(data.contacts$id.individuel.hdss)) 
    names(d) = c("id.individuel.hdss", "n.contact")
    data = left_join(d,data.char, by="id.individuel.hdss")
    data$round = round
    return(data)
}

mix_age = function(datas, var){
  dall = NULL
  cols = c("id.individuel.hdss","n.contact",var)
  i=1
  for(data in datas){
  d = data[,cols]
  d = d %>%
    group_by(!!groups[[1]]) %>% summarise(
      median = median(n.contact, na.rm=T),
      q1 = quantile(n.contact, probs = 0.25, na.rm=T),
      q3 = quantile(n.contact, probs = 0.75, na.rm=T),
      mean = mean(n.contact, na.rm=T),
      n.cases = length(unique(id.individuel.hdss)),
      sd =sd(n.contact),
      se =sd/sqrt(n.cases),
      lb = mean - qt(1 - (0.05 / 2), n.cases - 1) * se,
      lb = ifelse(lb<0,0,lb),
      ub = mean + qt(1 - (0.05 / 2), n.cases - 1) * se
      )
  d2 = data %>% summarise(
      median = median(n.contact, na.rm=T),
      q1 = quantile(n.contact, probs = 0.25, na.rm=T),
      q3 = quantile(n.contact, probs = 0.75, na.rm=T),
      mean = mean(n.contact, na.rm=T),
      n.cases = length(unique(id.individuel.hdss)),
      sd =sd(n.contact),
      se =sd/sqrt(n.cases),
      lb = mean - qt(1 - (0.05 / 2), n.cases - 1) * se,
      lb = ifelse(lb<0,0,lb),
      ub = mean + qt(1 - (0.05 / 2), n.cases - 1) * se
    )
  d2$var = "all"
  names(d2) = c(names(d2[1:9]), var)
  d$round = i
  d2$round = i
  d2 = d2[,names(d)]
  d3 = rbind(d,d2)
  dall = rbind(dall,d3)
  i = i+1
  }
  dall$round = ifelse(dall$round==3, 4,dall$round)
  return(dall)
}


# Create sero by age
sero_age= function(datas){
  dall = NULL
  i=1
  for(data in datas){
  d = as.data.frame(table(data$age.cat, data$ig.total.result)) %>%
  filter(Var2==1)
  d2 = as.data.frame(table(data$age.cat)) 
  d = merge(d,d2, by="Var1")
  names(d) = c("age.cat", "ig.total.result","n.pos","n")
  d$round = i
  dall = rbind(dall,d)
  i = i+1
  }
  return(dall)
}

sero_age2= function(datas){
  dall = NULL
  i=1
  for(data in datas){
    d = as.data.frame(table(data$age.cat2, data$ig.total.result)) %>%
      filter(Var2==1)
    d2 = as.data.frame(table(data$age.cat2)) 
    d = merge(d,d2, by="Var1")
    names(d) = c("age.cat", "ig.total.result","n.pos","n")
    d$round = i
    dall = rbind(dall,d)
    i = i+1
  }
  return(dall)
}

# Plot functions
pncontacts = function(data, round){
  d = as.data.frame(data %>% select(id.individuel.hdss) %>% table())
  n = length(unique(data$id.individuel.hdss))
  ggplot(d, aes(x=Freq)) +
  geom_bar(stat="count") +
  labs(title=paste0("Histogram of  contacts - ", round, " (n=", n, ")"), x = "Number of contacts") +
  theme_bw() +
  theme(panel.background=element_rect(colour = NA, fill = "white"),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = NA)) +
  geom_vline(aes(xintercept = mean(d$Freq),color="mean"), lty=2, size=1)+
  geom_vline(aes(xintercept = quantile(d$Freq, probs=c(0.05)), color="quantile"), lty=1, size=1, alpha=0.5)+
  geom_vline(aes(xintercept = quantile(d$Freq, probs=c(0.95)), color="quantile"), lty=1, size=1, alpha=0.5)+
  scale_color_manual(name = "", values = c(mean = "blue", quantile="red"))
}


