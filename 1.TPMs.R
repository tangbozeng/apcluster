library(dplyr)
library(tidyverse)
library(ggplot2)

leaf_drop<-read.csv("TPM_leafdrop/Mo_leafdrop.csv")
gene_length<-read.csv("TPM_leafdrop/gene_length_mo.csv")
leaf_len<-inner_join(leaf_drop, gene_length, by=c("X"="Gene.stable.ID"))
write.csv(leaf_len, "leaf_len.csv")

co_spray<-read.csv("TPMs_spray/co39_spray.csv")
moukoto_spray<-read.csv("TPMs_spray/moukoto.csv")

co_spray_len<-inner_join(co_spray, gene_length, by=c("Gene"="Gene.stable.ID"))
mou_spray_len<-inner_join(moukoto_spray, gene_length, by=c("X"="Gene.stable.ID"))

write.csv(co_spray_len, "co_spray_len.csv")
write.csv(mou_spray_len, "mou_spray_len.csv")
