#------------------------------------------------------------------------------------------------------------
# Sankey  Relation plot/Flow
# https://medium.com/@ODSC/parallel-plots-for-visualizing-relationships-with-ggplot2-and-ggforce-eac1c462d9b8
# M.Dekker
# Aedes Benchmark
# 210119
#------------------------------------------------------------------------------------------------------------

library(ggforce)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(forcats)

#-----
# Load 
#-----

setwd("~/Trivire/R/Git/AedesBencmark/AedesBenchmark/Data")

df <- read.csv("Letter_per_jaar_per_corpo.csv", sep=";",header = T,stringsAsFactors = F, na.strings = "")
df <- df[,-c(13,17)] # 2020 onderhoud en bedrijfslasten lopen tot laatste boekjaar
df[is.na(df)] <- "geen score"
df <- df[,order(names(df))]
names(df) <- c("Corporatie",
  "J2017_bedrijfslasten",
"J2017_duurzaamheid",
"J2017_huurdersoordeel",
"J2017_onderhoud",
"J2018_bedrijfslasten", 
"J2018_duurzaamheid",
"J2018_huurdersoordeel",
"J2018_onderhoud",
"J2019_bedrijfslasten",
"J2019_duurzaamheid",
"J2019_huurdersoordeel",
"J2019_onderhoud",
"J2020_duurzaamheid",
"J2020_huurdersoordeel")

df <- df[,-1] #Drop Corporatie namen

#--------------
#Bedrijfslasten
#--------------

#requires data.frame containing the frequency of some sequence
Grouped_BL <- df %>% group_by(J2017_bedrijfslasten,J2018_bedrijfslasten,J2019_bedrijfslasten) %>% 
summarise(freq = n()) # %>% filter(freq > 10) # Tune for size of grouped to plot

# Long format:
Parallel_Set_Plot_BL <- gather_set_data(Grouped_BL, 1:length(Grouped_BL)-1) #Drop Freq


#Plot
ggplot(Parallel_Set_Plot_BL, aes(x, id = id, split = y, value = freq)) +
  geom_parallel_sets(aes(fill = J2019_bedrijfslasten), alpha = 0.3, axis.width = 0.2)+
  geom_parallel_sets_axes(axis.width = 0.2) +
  geom_parallel_sets_labels(colour = 'black',angle = 360,size = 3) + labs(fill = "Score") + xlab(" ") + ylab("Aantal Corporaties") +
  theme_bw()

#--------------
#Huurderoordeel
#--------------

#requires data.frame containing the frequency of some sequence
Grouped_HO <- df %>% group_by(J2017_huurdersoordeel,J2018_huurdersoordeel,J2019_huurdersoordeel,J2020_huurdersoordeel) %>% 
  summarise(freq = n()) # %>% filter(freq > 10) # Tune for size of grouped to plot

# Long format:
Parallel_Set_Plot_HO <- gather_set_data(Grouped_HO, 1:length(Grouped_HO)-1) #Drop Freq


#Plot
ggplot(Parallel_Set_Plot_HO, aes(x, id = id, split = y, value = freq)) +
  geom_parallel_sets(aes(fill = J2020_huurdersoordeel), alpha = 0.3, axis.width = 0.2)+
  geom_parallel_sets_axes(axis.width = 0.2) +
  geom_parallel_sets_labels(colour = 'black',angle = 360,size = 3) + labs(fill = "Score") + xlab(" ") + ylab("Aantal Corporaties") +
  theme_bw()

#------------
#Duurzaamheid
#------------

#requires data.frame containing the frequency of some sequence
Grouped_DU <- df %>% group_by(J2017_duurzaamheid,J2018_duurzaamheid,J2019_duurzaamheid,J2020_duurzaamheid) %>% 
  summarise(freq = n()) # %>% filter(freq > 10) # Tune for size of grouped to plot

# Long format:
Parallel_Set_Plot_DU <- gather_set_data(Grouped_DU, 1:length(Grouped_DU)-1) #Drop Freq

5,6,

#Plot
ggplot(Parallel_Set_Plot_DU, aes(x, id = id, split = y, value = freq)) +
  geom_parallel_sets(aes(fill = J2020_duurzaamheid), alpha = 0.3, axis.width = 0.2)+
  geom_parallel_sets_axes(axis.width = 0.2) +
  geom_parallel_sets_labels(colour = 'black',angle = 360,size = 3) + labs(fill = "Score") + xlab(" ") + ylab("Aantal Corporaties") +
  theme_bw()

#---------
#Onderhoud
#---------

#requires data.frame containing the frequency of some sequence
Grouped_OH <- df %>% group_by(J2017_onderhoud,J2018_onderhoud,J2019_onderhoud) %>% 
  summarise(freq = n()) # %>% filter(freq > 10) # Tune for size of grouped to plot

# Long format:
Parallel_Set_Plot_OH <- gather_set_data(Grouped_OH, 1:length(Grouped_OH)-1) #Drop Freq


#Plot
ggplot(Parallel_Set_Plot_OH, aes(x, id = id, split = y, value = freq)) +
  geom_parallel_sets(aes(fill = J2019_onderhoud), alpha = 0.3, axis.width = 0.2)+
  geom_parallel_sets_axes(axis.width = 0.2) +
  geom_parallel_sets_labels(colour = 'black',angle = 360,size = 3) + labs(fill = "Score") + xlab(" ") + ylab("Aantal Corporaties") +
  theme_bw()

#-----------------------------------
# Alle prestatievelden (Stacked)
#-----------------------------------

J2017 <- as.data.frame(paste("Bedr: ",df[,1],",","Duurz: ",df[,2],",","Huurd: ",df[,3],",","Onderh: ",df[,4]))
names(J2017) <- "J2017"
J2018 <- as.data.frame(paste("Bedr: ",df[,5],",","Duurz: ",df[,6],",","Huurd: ",df[,7],",","Onderh: ",df[,8]))
names(J2018) <- "J2018"
J2019 <- as.data.frame(paste("Bedr: ",df[,9],",","Duurz: ",df[,10],",","Huurd: ",df[,11],",","Onderh: ",df[,12]))
names(J2019) <- "J2019"
J2020 <- as.data.frame(paste("Bedr: ","nb",",","Duurz: ",df[,13],",","Huurd: ",df[,14],",","Onderh: ","nb"))
names(J2020) <- "J2020"
J_Total <- cbind(J2017,J2018,J2019,J2020)

#requires data.frame containing the frequency of some sequence
 Grouped_J <- J_Total %>% group_by(J2017,J2018,J2019,J2020) %>% 
  summarise(freq = n()) # %>% filter(freq > 10) # Tune for size of grouped to plot

# Long format:
Parallel_Set_Plot_J <- gather_set_data(Grouped_J, 1:length(Grouped_J)-1) #Drop Freq


#Plot
ggplot(Parallel_Set_Plot_J, aes(x, id = id, split = y, value = freq)) +
  geom_parallel_sets(aes(fill = J2017), alpha = 0.3, axis.width = 0.2)+
  geom_parallel_sets_axes(axis.width = 0.2) +
  geom_parallel_sets_labels(colour = 'black',angle = 360,size = 3) + labs(fill = "Score") + xlab(" ") + ylab("Aantal Corporaties")
  theme_bw()




