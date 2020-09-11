library(gifski)
library(gganimate)
library(ggplot2)
colnames(eff_cluster)<-c("1","2","3","4","5","6","7","8",
                         "hub_gene","cluster","genes","count","names")
long_eff_clu<-eff_cluster%>%
  pivot_longer(cols=1:8, values_to="expression", names_to="time_points")

long_eff_clu$time_points<-ordered(long_eff_clu$time_points,
                                  levels=c("1","2","3","4","5","6","7","8"))
long_eff_clu$time_points<-as.numeric(long_eff_clu$time_points)
p<-ggplot(data = long_eff_clu,
       aes(x=long_eff_clu$time_points,
           y=long_eff_clu$expression,
           group=long_eff_clu$genes,
           color=long_eff_clu$names))+geom_point(size=0.5)+
  geom_line(aes(group=long_eff_clu$genes))+
  facet_wrap(~long_eff_clu$names, scales = "free")+
  theme_classic()+
  theme(strip.text = element_text(size=12,
                                  color="black", 
                                  family="Arial"))+
  ylab("relative transcript as TPMs")+ 
  xlab("Time points")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(color="black", size=20,  family="Arial"),
        axis.title.x = element_text(color="black", size=20, family="Arial"),
        axis.title.y = element_text(color="black", size=20, family="Arial"))+ 
  theme(axis.text.x = element_text(color="black", size=0.01,  family="Arial"),
        axis.text.y = element_text(color="black", size=12,  family="Arial"))+ 
  scale_colour_hue()+
  transition_reveal(long_eff_clu$time_points)

animate(p, height=600, width=800)

