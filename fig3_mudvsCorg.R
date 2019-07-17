library(tidyverse)
library(cowplot)
library(personalFunctions)

cbPalette <- c("#56B4E9", "#009E73", "#E69F00")
df=read.csv("Howard_et_al_data.csv")


df$better_location = factor(df$better_location,c("Western FL Bay",
                                             "Eastern FL Bay",
                                             "FL keys"))

west_only <- df %>% filter(better_location =="Western FL Bay")
west_model <-lm(west_only$Corg~west_only$mud)
westEq <- equationPrinter(west_model, dec_num = 2)

keys_only <- df %>% filter(better_location =="FL keys")
keys_model <-lm(keys_only$Corg~keys_only$mud)
KeysEq <- equationPrinter(keys_model, dec_num = 2)


mudvsCorg_regional <- df %>% 
    ggplot(aes(mud,Corg))+
    geom_point(size =2, aes(color = better_location, shape = better_location))+    
    scale_x_continuous(expand = c(0, 0), breaks=c(0,20,40,60,80),limits = c(0,90))+
    scale_y_continuous(expand = c(0, 0), breaks=c(0,2,4,6,8),limits = c(0,7))+
    theme(legend.position = c(0.02, 0.9),
          legend.title = element_blank())+
    labs(x = "Mud content (%)",
         y = expression(paste("Soil C"[org] ~" content (% dry wt.)")))+
    scale_color_manual(values = cbPalette[c(3,1,2)], 
                                           labels=c("Western FL Bay",
                                                    "Eastern FL Bay",
                                                    "FL keys"))+
    scale_shape_manual(values=c(18,1,19))+
    

    geom_abline(slope = west_model$coefficients[2], 
                intercept = west_model$coefficients[1],
                color = "black", linetype = "solid", size = 1.5)+
    geom_abline(slope = west_model$coefficients[2], 
                intercept = west_model$coefficients[1],
                color = cbPalette[3], linetype = "solid", alpha = 1, size = 1)+
    
    geom_abline(slope = keys_model$coefficients[2], 
                intercept = keys_model$coefficients[1],
                color = "black", linetype = "solid", alpha = 1, size = 1.5)+
    geom_abline(slope = keys_model$coefficients[2], 
                intercept = keys_model$coefficients[1],
                color = cbPalette[2], linetype = "solid", alpha = 1, size = 1)+
    
    annotate("text", label = westEq[1], x = 35, y = 4.1,parse = TRUE, size = 3)+
    annotate("text", label = westEq[2], x = 35, y = 3.85, parse = TRUE, size = 3)+
    annotate("text", label = KeysEq[1], x = 80, y = 4.65,parse = TRUE, size = 3)+
    annotate("text", label = KeysEq[2], x = 80.5, y = 4.4,parse = TRUE, size = 3)    

mudvsCorg_regional

ggsave("mudvsCorg_regional.pdf",mudvsCorg_regional, width = 4.8, height = 4.8)




df_covar <- df %>% filter(better_location !="Eastern FL Bay")

fit1 <- aov(Corg~mud*better_location, data = df_covar)
fit2 <- aov(Corg~mud+better_location, data = df_covar)
