library(gridExtra)
library(grid)
library(tidyverse)
library(lattice)
library(gridExtra)
library(cowplot)
library(ggplot2)
library(plotrix)
cbPalette <- c("#56B4E9", "#009E73", "#E69F00")

df <- read.csv("Howard_et_al_data.csv") %>% 
  select(better_location, Corg, mud) %>% 
  `colnames<-`(c("location", "Corg", "mud")) %>% 
  drop_na(mud) %>% 
  mutate(location = factor(location, c("Western FL Bay",
                                             "Eastern FL Bay",
                                             "FL keys")))


### top graph  - Corg

Corg_summary <- df %>% 
  group_by(location) %>% 
  summarise(mean = mean(Corg, na.rm = TRUE), 
            SE = std.error(Corg, na.rm = TRUE)) %>% 
  
  ggplot(aes(x=as.factor(location), y=mean, fill=location)) +
  geom_bar(stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2) +
  scale_y_continuous(expand = c(0, 0), breaks=c(0,1,2,3,4,5),limits = c(0,5))+
  labs(x = "",
       y = expression(paste("Soil C"[org] ~" content (% dry wt.)")))+
  annotate("text", x=3, y=4.5, label= "a)",size = 7)+ #####
annotate("text", x=1, y=.35, label= "a",size = 4)+ #####
annotate("text", x=2, y=.35, label= "b",size = 4)+ #####
annotate("text", x=3, y=.35, label= "b",size = 4)+
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank())+
  scale_fill_manual(values = cbPalette[c(3,1,2)], 
                    labels=c("Western FL Bay",
                             "Eastern FL Bay",
                             "FL keys"))


### bottom graph  - mud

mud_summary <- df %>% 
  group_by(location) %>% 
  summarise(mean = mean(mud, na.rm = TRUE), 
            SE = std.error(mud, na.rm = TRUE)) %>% 
  
  ggplot(aes(x=as.factor(location), y=mean, fill=location)) +
  geom_bar(stat="identity", color ="black") +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2, color = 'black') +
  scale_y_continuous(expand = c(0, 0), breaks=c(0,20,40,60,80),limits = c(0,60))+
  labs(x = "Sampling region",
       y = "Mud content (%)")+
  annotate("text", x=3, y=55, label= "b)",size = 7)+ #####
annotate("text", x=1, y=5, label= "a",size = 4)+ #####
annotate("text", x=2, y=5, label= "a",size = 4)+ #####
annotate("text", x=3, y=5, label= "b",size = 4)+
  theme(legend.position="none")+
  scale_fill_manual(values = cbPalette[c(3,1,2)], 
                    labels=c("Western FL Bay",
                             "Eastern FL Bay",
                             "FL keys"))






big_plot <- plot_grid(Corg_summary, mud_summary, ncol = 1, rel_heights = c(.9,1),
                      align = 'v', axis = 'l') # aligning vertically along the left axis


ggsave("fig2_regionbargraph.pdf",big_plot, width = 4.8, height = 6.6)

