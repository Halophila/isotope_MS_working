library(gvlma)
library(tidyverse)
library(cowplot)
library(gridExtra)
library(quantreg)
library(personalFunctions)

ramped=read_csv("Howard_et_al_data.csv")
theme_set(theme_cowplot())

mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.5)),
  colhead = list(fg_params=list(cex = 0.6)),
  rowhead = list(fg_params=list(cex = 0.6)))



ratio <- tibble("Corg"= ramped$Corg,
                    "x160" =100*(ramped$`160Wt Clost`)/(ramped$org_wt),
                    "x300" =100*(ramped$`300Wt Clost`)/(ramped$org_wt),                    
                    "x400" = 100*(ramped$`400Wt Clost`)/(ramped$org_wt),  
                    "x500" = 100*(ramped$`500Wt Clost`)/(ramped$org_wt),
                    "x550"= 100*(ramped$`550Wt Clost`)/(ramped$org_wt),
                    "x600"= 100*(ramped$`600Wt Clost`)/(ramped$org_wt),
                    "mud"=ramped$mud,
                    "set"=ramped$set) %>% 
  drop_na() %>% 
  mutate(sum = x160 + x300 + x400 + x500 + x550,
         ratio = (x160 + x300 + x400)/(x500 + x550),
         Rpindex = (x500 + x550)/(x500 + x550 + x160 + x300 + x400))


ratio$sum=(ratio$x160+ratio$x300+ratio$x400)+(ratio$x500+ratio$x550)
ratio$ratio=(ratio$x160+ratio$x300+ratio$x400)/(ratio$x500+ratio$x550)
ratio$Rpindex=(ratio$x500+ratio$x550)/(ratio$x500+ratio$x550+ratio$x160+ratio$x300+ratio$x400)
## alt at 600 -- ratio$Rpindex=(ratio$x500+ratio$x550+ratio$x600)/(ratio$x500+ratio$x550+ratio$x600+ratio$x160+ratio$x300+ratio$x400)


line=lm(ratio$Rpindex~ratio$mud)
gvlma(line)
summary(line)



####
Model = c("y = 0.5600 - 0.0028")
`p value` <- c("p < 0.05; p = 0.00")
table1 = tibble(Model, `p value`)


Quantile <- c("95%", "50%","5%")
Model <- c("y = 0.75-0.05x", "y = 0.52-0.03x", "y = 0.35-0.01x")
`p value` <- c("p < 0.05; p = 0.00", "p > 0.05; p = 0.00", "p > 0.05; p = 0.14")
table2 = data_frame(Quantile, Model, `p value`)

####

mud_plot <- ratio %>% 
  ggplot(aes(mud, Rpindex))+
  geom_point()+
  labs (x = "Mud content (%)",
        y = "Recalcitrance index")+  
  geom_abline(slope = line$coefficients[2], intercept = line$coefficients[1], color ="grey40")+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 80, by = 20),
                     limits = c(0,80))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=c(0,0.2,0.4,0.6,0.8),
                     limits = c(0.2,0.9))+
  annotation_custom(tableGrob(table1, rows = NULL, theme = mytheme), 
                    xmin=40, xmax=80, ymin=0.84, ymax=0.87)

#######
####

alltau=c(0.95,.5,.05)
linewt=c(2,1,2)

lineweight <- data.frame("alltau" = alltau,
                         "linewt"= linewt)

model=tibble(slope = 0, int=0)

for (i in seq_along(alltau)){
  qr1_i <- rq(ratio$Rpindex~ratio$Corg, tau = alltau[i])
  intercept=coef(qr1_i)[1]
  slope=coef(qr1_i)[2]
  model[i,1] <- slope
  model[i,2] <- intercept
}



Corg_plot <- ratio %>% 
  ggplot(aes(Corg, Rpindex))+
  geom_point()+
  labs (x = expression(paste("Soil C"[org] ~" content (% dry weight)")),
        y = "")+  
  geom_abline(slope = model$slope[1], intercept = model$int[1], linetype = "dashed", color ="grey60") +
  geom_abline(slope = model$slope[2], intercept = model$int[2], color ="grey40") +
  geom_abline(slope = model$slope[3], intercept = model$int[3], linetype = "dashed", color ="grey60") +
  annotate("text", x = .4, y = model$int[1] + 0.02, label = paste0(alltau[1]*100,"%"), color = "grey40",size = 3) +
  annotate("text", x = .4, y = model$int[2] + 0.02, label = paste0(alltau[2]*100,"%"), color = "grey40",size = 3) +
  annotate("text", x = .4, y = model$int[3] + 0.02, label = paste0(alltau[3]*100,"%"), color = "grey40",size = 3) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 9, by = 2),
                     limits = c(0,9))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=c(0,0.2,0.4,0.6,0.8),
                     limits = c(0.2,0.9))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())+
  annotation_custom(tableGrob(table2, rows=NULL, theme = mytheme), 
                    xmin=4.5, xmax=8, ymin=0.75, ymax=0.87)

Corg_plot




big_plot <- plot_grid(mud_plot, Corg_plot, ncol = 2, rel_widths = c(1,0.9),
                      align = 'h', axis = 'l') # aligning vertically along the left axis


ggsave("Rindex.pdf",big_plot, width = 8.8, height = 4.3)



