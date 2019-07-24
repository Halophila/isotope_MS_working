library(tidyverse)
library(cowplot)
library(plotrix)
library(devtools)
library(personalFunctions)

theme_set(theme_cowplot())

point_colors <-  c("#e1e1e1","#0600e1")

df <- read.csv("Howard_et_al_data.csv")


inorg <- df %>% 
  select(Corg, d13Cinorg) %>% 
  mutate(type = "inorg") %>% 
  rename(d13 = d13Cinorg) %>% 
  drop_na()

model_inorg <- (lm(d13~Corg, data = inorg))

eqn1_Cinorg <- equationPrinter(model_inorg)[1]
eqn2_Cinorg <- equationPrinter(model_inorg)[2]


org <- df %>% 
  select(Corg, d13Corg) %>% 
  mutate(type = "org") %>% 
  rename(d13 = d13Corg) %>% 
  drop_na()

model_org <- lm(d13~Corg, data = org)

eqn1_Corg <- equationPrinter(model_org)[1]
eqn2_Corg <- equationPrinter(model_org)[2]

df_tall <- bind_rows(inorg,org)

###plots



isotope_Corg_plot <- df_tall %>% 
  ggplot(aes(Corg,d13, fill=type))+
  geom_point(shape=21, color = 'black')+
  labs(x = expression(paste("Soil C"[org],' content (% dry wt.)')),
       y = expression(paste(delta^{13},'C surface soil (\u2030)')))+
  geom_abline(slope = model_inorg$coefficients[2], intercept = model_inorg$coefficients[1], color = "grey50")+
  geom_abline(slope = model_org$coefficients[2], intercept = model_org$coefficients[1], color = point_colors[2])+
  scale_fill_manual(values = point_colors, 
                    labels = c(expression(paste('C'[inorg])), 
                               expression(paste('C'[org]))))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(-20,5,5),
                     limits = c(-20,5))+
  theme(legend.title=element_blank(),
        legend.position = c(0.68, .98),
        legend.direction = "horizontal")+
  annotate("text", label = eqn1_Cinorg, x = 7.5, y = 1.0,parse = TRUE)+
  annotate("text", label = eqn2_Cinorg, x = 7.5, y = 0.1,parse = TRUE)+
  annotate("text", label = eqn1_Corg, x = 7.5, y = -9.2,parse = TRUE)+
  annotate("text", label = eqn2_Corg, x = 7.5, y = -10.10,parse = TRUE)

cairo_pdf(filename = "fig7_isotope_Corg.pdf", width = 4.8, height = 4.6)
print(isotope_Corg_plot) # cairo_pdf only opens the graphics device, need to print the plot
dev.off() # and need to close the device


