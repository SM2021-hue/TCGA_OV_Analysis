#imputing missing data in >20% (i.e., >2) samples 
cptac=read.xls("demo2.xlsx",sheet=7)
cptac=cptac[,c(2,7:17)]
cptac1=as.matrix(t(cptac[,2:12]))
colnames(cptac1)=cptac[,1]

library("mice")
tempData <- mice(cptac,m=5,meth='pmm') 
summary(tempData) 
res.cptac <- complete(tempData) 
anyNA(res.cptac) 
write.table(res.cptac,"cptac_mice.txt",quote=F,sep="\t")

########################### image- CPTAC correlation ################

cptac=read.xls("demo2.xlsx",sheet=1)
cptac=cptac[,c(2,7:17)]
cptac1=as.matrix(t(cptac[,2:12]))
colnames(cptac1)=cptac[,1]
image=read.xls("demo2.xlsx",sheet=2)

image1=as.matrix(image[,2:8])
rownames(image1)=image[,1]

####### Method1 #############
#image_cptac=cbind(image1[,1:7],cptac1[,1:6199])
#cor_image_cptac=cor(image_cptac,method="spearman")
#write.table(cor_image_cptac,"cor_image_cptac.txt",sep="\t")

######generate heatmap of correlation###########
#png("heatmap_50.png",height=1520,width=1520)
#heatmap.2(cor_image_cptac[1:50,1:50],trace="none",cexCol=1,cexRow = 1,density.info = "none",srtCol = 330,adjCol = c(0.1,0.3))
#dev.off()


############### Method2 ###########
library("psych", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.6")
ct_cptac_avg_image <- corr.test(image1, cptac1, method='spearman', adjust='none')
write.table(t(ct_cptac_avg_image$r),"cor_image_avg_cptac_method2.txt",sep="\t")
dim(ct_cptac_avg_image$p)

########################### image-RNAseq #################

rnaseq=read.xls("demo2.xlsx",sheet=5)
rnaseq=rnaseq[,1:12]
rnaseq1=as.matrix(t(rnaseq[,2:12]))
colnames(rnaseq1)=rnaseq[,1]

ct_rnaseq_avg_image <- corr.test(image1, rnaseq1, method='spearman', adjust='none')
write.table(t(ct_rnaseq_avg_image$r),"cor_image_avg_rnaseq_method2.txt",sep="\t")

