library(tidyverse)

basemeans_efe<-read.csv("TPM_leafdrop/leadrop_basemean.csv", row.names=1)

apres <- apcluster(negDistMat(r=0.5), basemeans_efe, details=TRUE)# more r, bigger size of each group
apres <- apcluster(s=apres@sim, q=0.1) # more q, more fargmentation.
show(apres)

eff_cluster<-basemeans_efe%>%
  mutate(cluster=labels(apres,type="enum"))%>%
  mutate(genes=rownames(basemeans_efe))
rownames(eff_cluster)<-rownames(basemeans_efe)
long_eff_clu<-eff_cluster%>%
  pivot_longer(cols=t0:t144, values_to="expression", names_to="time_points")
long_eff_clu$time_points<-
  ordered(long_eff_clu$time_points, 
          levels=c("t0","t8","t16","t24","t48","t72","t96","t144"))
ggplot(data = long_eff_clu,
       aes(x=long_eff_clu$time_points,
           y=log2(long_eff_clu$expression),
           group=long_eff_clu$genes,
           color=long_eff_clu$cluster))+
  geom_line()+
  facet_wrap(~long_eff_clu$cluster, scales = "free")