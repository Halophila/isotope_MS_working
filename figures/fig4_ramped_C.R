library(tidyverse)
library(plotrix)
library(cowplot)

ramped <- read.csv("Howard_et_al_data.csv")


#### top plot

x0 = 100*(ramped$Ccontentorg)
x160 = 100*(ramped$X160Wt.Cpost)/(ramped$X160remains)
x300 = 100*(ramped$X300Wt.Cpost)/(ramped$X300remains)        
x400 = 100*(ramped$X400Wt.Cpost)/(ramped$X400remains)
x500 = 100*(ramped$X500Wt.Cpost)/(ramped$X500remains)    
x550 = 100*(ramped$X550Wt.Cpost)/(ramped$X550remains)
x600 = 100*(ramped$X600Wt.Cpost)/(ramped$X600remains)

averageramped <- tibble(x0,x160,x300,x400,x500,x550,x600) %>% 
  gather("temp_id", "values")


summary <- averageramped %>% 
  group_by(temp_id) %>% 
  summarise(mean =  mean(values, na.rm = TRUE), 
            SE = std.error(values, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(temp = as.numeric(str_remove_all(temp_id, "x")))


###
top_plot <- summary %>% 
  ggplot(aes(temp,mean))+
  geom_point()+  
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE), width = 20) +
  geom_line(linetype = "dashed")+
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(from = 0, to = 600, by = 100),
                     limits = c(0,610))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks = c(9,10,11,12),
                     limits = c(9,12.2))+
  labs(x = "",
       y = "C content (% dry wt.)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

#####

#bottom plot
########
############

ramped$totalClost =  (ramped$X160Wt.Clost) + (ramped$X300Wt.Clost)+
  (ramped$X400Wt.Clost) + (ramped$X500Wt.Clost) + (ramped$X550Wt.Clost)

"x0" = (ramped$org_wt)-(ramped$org_wt)
"x160" = 100*(ramped$X160Wt.Clost)/(ramped$totalClost)
"x300" = 100*(ramped$X300Wt.Clost)/(ramped$totalClost)              
"x400" = 100*(ramped$X400Wt.Clost)/(ramped$totalClost)      
"x500" = 100*(ramped$X500Wt.Clost)/(ramped$totalClost)     
"x550" = 100*(ramped$X550Wt.Clost)/(ramped$totalClost)
"x600" = 100*(ramped$X600Wt.Clost)/(ramped$totalClost)
"total" = 100*(ramped$totalClost)/(ramped$totalClost)

averageramped <- tibble(x0,x160,x300,x400,x500,x550,x600) %>% 
  gather("temp_id", "values")


summary <- averageramped %>% 
  group_by(temp_id) %>% 
  summarise(mean =  mean(values, na.rm = TRUE), 
            SE = std.error(values, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(temp = as.numeric(str_remove_all(temp_id, "x")))


###
bottom_plot <- summary %>% 
  ggplot(aes(temp,mean))+
  geom_point()+  
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE), width = 20) +
  geom_line(linetype = "dashed")+
  scale_x_continuous(expand = c(0, 0), 
                     breaks = seq(from = 0, to = 600, by = 100),
                     limits = c(0,610))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks = c(0,10,20,30,40),
                     limits = c(0,40))+
  labs(x = "Temperature (°C)",
       y = "C loss (% of total)")



big_plot <- plot_grid(top_plot, bottom_plot, ncol = 1, rel_heights = c(.9,1),
                      align = 'v', axis = 'l') # aligning vertically along the left axis


ggsave("fig4_ramped_C.pdf", big_plot, width = 4.8, height = 6.6)

