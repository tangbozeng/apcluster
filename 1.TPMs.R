library(dplyr)
library(tidyverse)
library(ggplot2)

leaf_drop<-read.csv("TPM_leafdrop/Mo_leafdrop.csv")
gene_length<-read.csv("TPM_leafdrop/gene_length_mo.csv")
leaf_len<-inner_join(leaf_drop, gene_length, by=c("X"="Gene.stable.ID"))
write.csv(leaf_len, "12991_with_gene_id.csv") # output the one with id
leaf_fram<-leaf_len[,c(2:25)] # get pure count matrix
len<-leaf_len$length  # define the length

r_tpm <- function(leaf_drop,len)
{
  dfr1 <- sweep(leaf_drop,MARGIN=1,(len/10^4),`/`)
  scf <- colSums(dfr1)/(10^6)
  return(sweep(dfr1,2,scf,`/`))
}
test<-r_tpm(leaf_fram,len)%>%mutate(gene=leaf_len$X)
write.csv(test,"tpm_leafdrop.csv")

write.csv(leaf_len, "leaf_len.csv")

co_spray<-read.csv("TPMs_spray/co39_spray.csv")
moukoto_spray<-read.csv("TPMs_spray/moukoto.csv")

co_spray_len<-inner_join(co_spray, gene_length, by=c("Gene"="Gene.stable.ID"))
mou_spray_len<-inner_join(moukoto_spray, gene_length, by=c("X"="Gene.stable.ID"))

write.csv(co_spray_len, "co_spray_len.csv")
write.csv(mou_spray_len, "mou_spray_len.csv")
