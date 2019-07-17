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


ggsave("regional_bar.pdf",big_plot, width = 4.8, height = 6.6)



library(HH)
hov(sedtype$mud~sedtype$location)
#hovPlot(sedtype$mud~sedtype$location)
bartlett.test(sedtype$mud~sedtype$location)
model=aov(sedtype$mud~sedtype$location)
summary(model)
TukeyHSD(model)




#grid.arrange(densityplot, contentplot, Cdensityplot, ncol=1)
grid.text("Site location", x = unit(0.55, "npc"), just= "bottom",
          y = unit(0.012, "npc"))


dev.off()

######
####map with three zones
######

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata) # loads additional map databases (high-res)
library(maptools) # (loads foreign, sp, grid, and lattice)
library(rgeos) # required by maptools to work with GHHSG
gpclibPermit() # older alternative to work with GHHSG
library(PBSmapping)
library(ggplot2)
library(ggmap)
library(raster)
library(rworldxtra)    
data(countriesHigh) 
library(RColorBrewer)

###
setwd("~/Desktop/chapter_4/")
ramped=read.csv("chapter4.csv")


geo <- data.frame("long" = ramped$Longitude..dd.ddddd.,
                  "lat" = ramped$Latitude..dd.ddddd., 
           #       "mud" = ramped$mud,
                  "Corg" = ramped$Corg,
          #        "Cinorg" = ramped$Cinorg,
                  "location"= ramped$better_location)
#                "d13Corg" = df$d13Corg,
                #"d13Cinorg" = df$d13Cinorg)
geo=na.omit(geo)


xlim <- c(-82,-80)
ylim <- c(24.4, 25.6)

pdf("zonemap.pdf",width=6,height=4)
par(mfrow = c(1, 1))
par(cex = 0.7)
par(mar = c(0, .1, 0.1, 0), oma = c(.0, 4.7, 0.1, 0.4))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
par(las=1)
usa = getData("GADM",country="USA",level=2)
#map(regions = "usa", xlim=xlim, ylim=ylim,col="gray90", fill=TRUE)
map(usa, xlim=xlim, ylim=ylim,col="gray90", fill=TRUE)



zonecolor=c("blue","green3","red","purple")
palette(c(zonecolor))


grid(lty = 1)
box()

axis(1)
axis(2, at=c(24.4,24.8,25.2,25.6))

points(geo$long,geo$lat, pch = 20, cex=2, col = geo$location)

legend((-81.95),25.59, pch=20, legend=c("Eastern FL Bay", "Western FL Bay","FL Keys"),
       col=c("blue","red","green3"), cex=0.8)

dev.off()



