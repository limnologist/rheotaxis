rm(list=ls())
#'read data
library(readxl) # load library
library(tidyverse)

library(xlsx)
d <- read.xlsx("D:\\drifting5.xlsx",1)

species<-d$species
infection<-d$infection
position<-d$position


ml1<-lm(position~species*infection,data=d)
summary(ml1)

####graphics
#sp1<-
ggplot(d)+
  
  theme_classic()+
  geom_boxplot(aes(x=species,y=position,fill=infection), notch = T)+
  #geom_violin(aes(x=species,y=position,fill=infection), draw_quantiles = c(0.5), alpha = 0.2, col = "transparent", size = 2) +
  #geom_jitter(aes(x=species, y = position, col = infection, shape = infection), position = position_dodge(width = 1), size=2, width = 0.2)+
  scale_color_brewer(palette = "Dark2", direction = -1)+
  # sp1+ylim(-7,7)
  scale_y_continuous(limits=c(-7, 7),breaks = seq(-7, 7, 1))

###E. berilloni
position1<-position[species=="E. berilloni"]
infection1<-infection[species=="E. berilloni"]
model1<-lm(position1~infection1)
summary(model1)

###G. pulex
position1<-position[species=="G. pulex"]
infection1<-infection[species=="G. pulex"]
model1<-lm(position1~infection1)
summary(model1)

###G. fossarum 
position1<-position[species=="G. fossarum"]
infection1<-infection[species=="G. fossarum"]
model1<-lm(position1~infection1)
summary(model1)
















