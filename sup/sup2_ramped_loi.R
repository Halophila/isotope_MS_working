library(tidyverse)
library(plotrix)
library(cowplot)
ramped=read_csv("./Howard_et_al_data.csv")

x0 = 100*(ramped$org_wt)/(ramped$org_wt)
x160 = 100*(ramped$`160remains`)/(ramped$org_wt)
x300 = 100*(ramped$`300remains`)/(ramped$org_wt)              
x400 = 100*(ramped$`400remains`)/(ramped$org_wt)      
x500 = 100*(ramped$`500remains`)/(ramped$org_wt)     
x550 = 100*(ramped$`550remains`)/(ramped$org_wt)
x600 = 100*(ramped$`600remains`)/(ramped$org_wt)

averageramped <- tibble(x0,x160,x300,x400,x500,x550,x600) %>% 
  gather("temp_id", "values")


summary <- averageramped %>% 
  group_by(temp_id) %>% 
  summarise(mean= mean(values, na.rm = TRUE), 
            SE = std.error(values, na.rm = TRUE)) %>% 
  mutate(temp = as.numeric(str_remove_all(temp_id, "x")))

###
top_plot <- summary %>% 
  ggplot(aes(temp,mean))+
  geom_point()+  
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=20) +
  geom_line(linetype = "dashed")+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 600, by = 100),
                     limits = c(0,610))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(90,100,2),
                     limits = c(88,100.5))+
  labs(x = "",
       y = "Sample wt. (% of initial)")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())


#####

totalweightloss=ramped$`160off`+ramped$`300off`+ramped$`400off`+
  ramped$`500off`+ramped$`550off`+ramped$`600off`

"x0" = (ramped$org_wt)-(ramped$org_wt)
"x160" = 100*(ramped$`160off`)/(totalweightloss)
"x300" = 100*(ramped$`300off`)/(totalweightloss)              
"x400" = 100*(ramped$`400off`)/(totalweightloss)      
"x500" = 100*(ramped$`500off`)/(totalweightloss)     
"x550" = 100*(ramped$`550off`)/(totalweightloss)
"x600" = 100*(ramped$`600off`)/(totalweightloss)


averageramped <- tibble(x0,x160,x300,x400,x500,x550,x600) %>% 
  gather("temp_id", "values")


summary <- averageramped %>% 
  group_by(temp_id) %>% 
  summarise(mean= mean(values, na.rm = TRUE), 
            SE = std.error(values, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(temp = as.numeric(str_remove_all(temp_id, "x")))

bottom_plot <- summary %>% 
  ggplot(aes(temp,mean))+
  geom_point()+  
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=20) +
  geom_line(linetype = "dashed")+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 600, by = 100),
                     limits = c(0,610))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(0,30,5),
                     limits = c(0,30))+
  labs(x = "Temperature (C)",
       y = "Wt. loss (% of total)")


big_plot <- plot_grid(top_plot, bottom_plot, ncol = 1, rel_heights = c(.9,1),
                      align = 'v', axis = 'l') # aligning vertically along the left axis


ggsave("sup2_ramped_loi.pdf",big_plot, width = 6.8, height = 4.6)
