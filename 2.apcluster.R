# cut off the zero values, we have 542.
#let us use the TPM average values.

library(apcluster)
eff_ex<-read.csv("tpm_aver_efffector.csv", row.names = 1)
eff_log2<-read.csv("tpm_aver_efffector_log2.csv", row.names = 1)

apres <- apcluster(negDistMat(r=0.5), eff_log2, details=TRUE)# more r, bigger size of each group
apres <- apcluster(s=apres@sim, q=0.2)#0.4) # more q, more fargmentation.
show(apres)
write.csv(labels(apres,type="enum"), "test.csv")


#apres <- apclusterK(negDistMat(r=2), count, K=20)

## show details of clustering results
show(apres)