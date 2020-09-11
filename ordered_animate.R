library(gifski)
library(gganimate)
library(ggplot2)

ordered_wide<-read.csv("ordered_cluster/ordered_cluster.csv")
colnames(ordered_wide)<-c("X","hub_gene","cluster","genes","count","names",
                         "1","2","3","4","5","6","7","8")


long_ordered<-ordered_wide%>%
  pivot_longer(cols=7:14,
               values_to="expression",
               names_to="time_points")

long_ordered$time_points<-ordered(long_ordered$time_points,
                                  levels=c("1","2","3","4","5","6","7","8"))

long_ordered$time_points<-as.numeric(long_ordered$time_points)

p<-ggplot(data = long_ordered,
          aes(x=long_ordered$time_points,
              y=long_ordered$expression,
              group=long_ordered$genes,
              color=long_ordered$names))+geom_point(size=0.5)+
  geom_line(aes(group=long_ordered$genes))+
  facet_wrap(~long_ordered$names, scales = "free")+
  theme_classic()+
  theme(strip.text = element_text(size=0.0001,
                                  color="white", 
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
  transition_reveal(long_ordered$time_points)

animate(p, height=600, width=1000)


