library(tidyverse)
library(cowplot)
library(plotrix)
library(personalFunctions)

theme_set(theme_cowplot())
point_colors = c("#e1e1e1","#0600e1")
        
df <- read_csv("Howard_et_al_data.csv") %>% 
        filter(set=="fknms") 


inorg <- df %>% 
        select(d13seagrass, d13Cinorg) %>% 
        mutate(type = "inorg") %>% 
        rename(d13 = d13Cinorg) %>% 
        drop_na()

model_inorg <- (lm(d13~d13seagrass, data = inorg))

eqn1_Cinorg <- equationPrinter(model_inorg)[1]
eqn2_Cinorg <- equationPrinter(model_inorg)[2]


org <- df %>% 
        select(d13seagrass, d13Corg) %>% 
        mutate(type = "org") %>% 
        rename(d13 = d13Corg) %>% 
        drop_na()


df_tall <- bind_rows(inorg,org)

###plots



isotope_seagrass_plot <- df_tall %>% 
        ggplot(aes(d13seagrass,d13, fill=type))+
        geom_point(shape=21, color = 'black')+
        labs(x = expression(paste("Seagrass leaf tissue ", delta^{13}, 'C (\u2030)')),
             y = expression(paste(delta^{13},'C surface soil (\u2030)')))+
        geom_abline(slope = model_inorg$coefficients[2], intercept = model_inorg$coefficients[1], color = "grey50")+
       scale_fill_manual(values = point_colors, 
                            labels = c(expression(paste('C'[inorg])), 
                                       expression(paste('C'[org]))))+
        scale_y_continuous(expand = c(0, 0), 
                           breaks=seq(-20,5,5),
                           limits = c(-20,5))+
        theme(legend.title=element_blank(),
              legend.position = c(0.01, .98),
              legend.direction = "horizontal")+
        annotate("text", label = eqn1_Cinorg, x = -8, y = -3.95,parse = TRUE)+
        annotate("text", label = eqn2_Cinorg, x = -8, y = -5.0,parse = TRUE)



cairo_pdf(filename = "isotope_seagrass_plot.pdf", width = 4.8, height = 4.6) ##special charater requires cairo, cant use ggsave
print(isotope_seagrass_plot) # cairo_pdf only opens the graphics device, need to print the plot
dev.off() #  needs to close the device

