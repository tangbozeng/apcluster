eff_ex<-read.csv("tpm_aver_efffector.csv", row.names = 1)
eff_log2<-read.csv("tpm_aver_efffector_log2.csv", row.names = 1)

apres <- apcluster(negDistMat(r=0.5), eff_ex, details=TRUE)# more r, bigger size of each group
apres <- apcluster(s=apres@sim, q=0.1) # more q, more fargmentation.
show(apres)

eff_cluster<-eff_ex%>%
  mutate(hub_gene=labels(apres,type="exemplars"))%>%
  mutate(cluster=labels(apres, type="enum"))# type can be exemplars as well
  mutate(genes=rownames(eff_ex))%>%
  group_by(cluster)%>%mutate(count = n())%>% # mutate new column with group number! nice!
  filter(count>5)%>%# remove clusters with less than 5 genes
  nutate(names=paste(hub_gene, count,sep="_"))# create new labels


long_eff_clu<-eff_cluster%>%
  pivot_longer(cols=t0:t144, values_to="expression", names_to="time_points")
long_eff_clu$time_points<-
  ordered(long_eff_clu$time_points, 
          levels=c("t0","t8","t16","t24","t48","t72","t96","t144"))
ggplot(data = long_eff_clu,
       aes(x=long_eff_clu$time_points,
           y=log2(long_eff_clu$expression),
           group=long_eff_clu$genes,
           color=long_eff_clu$names))+
  geom_line()+
  facet_wrap(~long_eff_clu$cluster, scales = "free")


