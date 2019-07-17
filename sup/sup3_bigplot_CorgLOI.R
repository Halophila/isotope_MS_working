library(gvlma)
library(tidyverse)
library(cowplot)
df=read.csv("Howard_et_al_sup_data.csv")


####160
#####
equation = equationPrinter(lm(df$X160Corg~df$X160LOI))

t160 <- df %>% 
  ggplot(aes(X160LOI,X160Corg))+
  geom_point()+
 # geom_smooth(method = "lm", se =FALSE, color="grey30")+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 10, by = 2),
                     limits = c(0,10))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=0:5,
                     limits = c(-0.3,5))+
  labs(x = "Loss on ignition (%)",
       y = expression(paste("Soil C"[org],' content (% dry wt.)')))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank())+
  annotate("text", x = 0.7, y = 4, label = "160°C", size = 7)


###300
###
equation = equationPrinter(lm(df$X300Corg~df$X300LOI))

model <- lm(df$X300Corg~df$X300LOI)
t300 <- df %>% 
  ggplot(aes(X300LOI,X300Corg))+
  geom_point()+
  geom_smooth(method = "lm", se =FALSE, color="grey30", fullrange = T)+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 10, by = 2),
                     limits = c(0,10))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=0:5,
                     limits = c(-0.3,5))+
  labs(x = "Loss on ignition (%)",
       y = expression(paste("Soil C"[org],' content (% dry wt.)')))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank())+
  annotate("text", x = 0.7, y = 4, label = "300°C", size = 7)+
  annotate("text", x = 2, y = 4.25, label = equation[1], size = 4, parse = TRUE)+
  annotate("text", x = 2, y = 3.75, label = equation[2], size = 4, parse = TRUE)



###400
###
equation = equationMAker(lm(df$X400Corg~df$X400LOI))

t400 <- df %>% 
  ggplot(aes(X400LOI,X400Corg))+
  geom_point()+
  geom_smooth(method = "lm", se =FALSE, color="grey30", fullrange = T)+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 10, by = 2),
                     limits = c(0,10))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=0:5,
                     limits = c(-0.3,5))+
  labs(x = "Loss on ignition (%)",
       y = expression(paste("Soil C"[org],' content (% dry wt.)')))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank())+
  annotate("text", x = 0.7, y = 4, label = "400°C", size = 7)+
  annotate("text", x = 2, y = 4.25, label = equation[1], size = 4, parse = TRUE)+
  annotate("text", x = 2, y = 3.75, label = equation[2], size = 4, parse = TRUE)



###500
###
equation = equationPrinter(lm(df$X500Corg~df$X500LOI))

t500 <- df %>% 
  ggplot(aes(X500LOI,X500Corg))+
  geom_point()+
  geom_smooth(method = "lm", se =FALSE, color="grey30", fullrange = T)+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 10, by = 2),
                     limits = c(0,10))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=0:5,
                     limits = c(-0.3,5))+
  labs(x = "Loss on ignition (%)",
       y = expression(paste("Soil C"[org],' content (% dry wt.)')))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank())+
  annotate("text", x = 0.7, y = 4, label = "500°C", size = 7)+
  annotate("text", x = 2, y = 4.25, label = equation[1], size = 4, parse = TRUE)+
  annotate("text", x = 2, y = 3.75, label = equation[2], size = 4, parse = TRUE)




###550
###
equation = equationPrinter(lm(df$X550Corg~df$X550LOI))

t550 <- df %>% 
  ggplot(aes(X550LOI,X550Corg))+
  geom_point()+
  geom_smooth(method = "lm", se =FALSE, color="grey30", fullrange = T)+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 10, by = 2),
                     limits = c(0,10))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=0:5,
                     limits = c(-0.3,5))+
  labs(x = "Loss on ignition (%)",
       y = expression(paste("Soil C"[org],' content (% dry wt.)')))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank())+
  annotate("text", x = 0.7, y = 4, label = "550°C", size = 7)+
  annotate("text", x = 2, y = 4.25, label = equation[1], size = 4, parse = TRUE)+
  annotate("text", x = 2, y = 3.75, label = equation[2], size = 4, parse = TRUE)




###600
###
equation = equationPrinter(lm(df$X600Corg~df$X600LOI))

t600 <- df %>% 
  ggplot(aes(X600LOI,X600Corg))+
  geom_point()+
  geom_smooth(method = "lm", se =FALSE, color="grey30", fullrange = T)+
  scale_x_continuous(expand = c(0, 0), 
                     breaks=seq(from = 0, to = 10, by = 2),
                     limits = c(0,10))+
  scale_y_continuous(expand = c(0, 0), 
                     breaks=0:5,
                     limits = c(-0.3,5))+
  labs(x = "Loss on ignition (%)",
       y = expression(paste("Soil C"[org],' content (% dry wt.)')))+
  theme(axis.title.y=element_blank())+
  annotate("text", x = 0.7, y = 4, label = "600°C", size = 7)+
  annotate("text", x = 2, y = 4.25, label = equation[1], size = 4, parse = TRUE)+
  annotate("text", x = 2, y = 3.75, label = equation[2], size = 4, parse = TRUE)



big_plot <- plot_grid(t160, t300, t400, t500, t550, t600, ncol = 1, 
                      rel_heights = c(.9,.9,.9,.9,.9,1.1),
                      align = 'v', axis = 'l', scale=0.92) +
  draw_label(expression(paste("Soil C"[org],' content lost (% dry wt.)')), x=  0, y=0.5, vjust= 1.4, 
             angle=90, size = 20)

ggsave("sup_bigplot.pdf",big_plot, width = 10.8, height = 12)

