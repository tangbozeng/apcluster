ordered_wide<-read.csv("ordered_cluster/ordered_cluster.csv")

long_ordered<-ordered_wide%>%
  pivot_longer(cols=7:14,
               values_to="expression",
               names_to="time_points")

long_ordered$time_points<-
  ordered(long_ordered$time_points, 
          levels=c("t0","t8","t16","t24",
                   "t48","t72","t96","t144"))

ggplot(data = long_ordered,
       aes(x=long_ordered$time_points,
           y=long_ordered$expression,
           group=long_ordered$genes,
           color=long_ordered$names))+
  geom_line()+
  facet_wrap(~long_ordered$names, scales = "free")+
  theme_classic()+
  theme(strip.text = element_text(size=12,
                                  color="white", 
                                  family="Arial"))+
  ylab("relative transcripts as TPMs")+ 
  xlab("Time points")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(color="black", size=20,  family="Arial"),
        axis.title.x = element_text(color="black", size=20, family="Arial"),
        axis.title.y = element_text(color="black", size=20, family="Arial"))+ 
  theme(axis.text.x = element_text(color="black", size=8,  family="Arial"),
        axis.text.y = element_text(color="black", size=12,  family="Arial"))+ 
  scale_colour_hue()