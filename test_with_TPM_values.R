library(ggplot2)
library(dplyr)
library(tidyverse)
eff_ex<-read.csv("TPM_leafdrop/tpm_aver_efffector.csv", row.names = 1)
#eff_log2<-read.csv("tpm_aver_efffector_log2.csv", row.names = 1)

apres <- apcluster(negDistMat(r=0.25), eff_ex, details=TRUE)# more r, bigger size of each group
apres <- apcluster(s=apres@sim, q=0.2) # more q, more fargmentation.
show(apres)

eff_cluster<-eff_ex%>%
  mutate( hub_gene = labels(apres, type = "names") )%>%
  mutate( cluster = labels(apres, type = "enum"))%>%# type can be exemplars as well
  mutate(genes=rownames(eff_ex))%>%
  group_by(cluster)%>%mutate(count = n())%>%                                    # mutate new column with group number! nice!
  filter(count>9)%>%# remove clusters with less than 5 genes
  dplyr::mutate(names=paste(cluster,hub_gene, count,sep="_"))                   # create new labels
#colnames(eff_cluster)<-c("1","2","3","4","5","6","7","8","hub_gene","cluster","genes","count","names")

#colnames(eff_cluster)<-c("0","8","16","24","48","72","96","144","hub_gene","cluster","genes","count","names")
long_eff_clu<-eff_cluster%>%
  pivot_longer(cols=1:8, values_to="expression", names_to="time_points")
  

long_eff_clu$time_points<-
  ordered(long_eff_clu$time_points, 
          levels=c("t0","t8","t16","t24","t48","t72","t96","t144"))


#long_eff_clu$time_points<-as.numeric(long_eff_clu$time_points)
# plot clusters
ggplot(data = long_eff_clu,
       aes(x=long_eff_clu$time_points,
           y=long_eff_clu$expression,
           group=long_eff_clu$genes,
           color=long_eff_clu$names))+
  geom_line()+
  facet_wrap(~long_eff_clu$names, scales = "free")+
  theme_classic()+
  theme(strip.text = element_text(size=12,
                                  color="black", 
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

wide_clustered<-long_eff_clu%>%
  pivot_wider(names_from=time_points, values_from=expression)
write.csv(wide_clustered, "nt_wide.csv")

known<-read.csv("all_known_effectors.csv")

nt<-full_join(known, wide_clustered, by=c("gene"="genes"))
write.csv(nt, "final_nt.csv")