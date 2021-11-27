########################### image- RPPA correlation ################

rppa=read.xls("demo2.xlsx",sheet=8)
rppa=rppa[,c(1,4:14)]
rppa1=as.matrix(t(rppa[,2:12]))
colnames(rppa1)=rppa[,1]
image=read.xls("demo2.xlsx",sheet=2)

########################### average ############################
image1=as.matrix(image[,2:8])
rownames(image1)=image[,1]

library("psych", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.6")

ct_rppa_avg_image <- corr.test(image1, rppa1, method='spearman', adjust='none')
write.table(t(ct_rppa_avg_image$r),"cor_image_avg_rppa_method2.txt",sep="\t")
dim(ct_rppa_avg_image$p)


########################### variance ############################
image1=as.matrix(image[,9:15])
rownames(image1)=image[,1]

library("psych", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.6")

ct_rppa_var_image <- corr.test(image1, rppa1, method='spearman', adjust='none')
write.table(t(ct_rppa_var_image$r),"cor_image_var_rppa_method2.txt",sep="\t")
dim(ct_rppa_var_image$p)

########################### stdev ############################
image1=as.matrix(image[,16:22])
rownames(image1)=image[,1]

library("psych", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.6")

ct_rppa_std_image <- corr.test(image1, rppa1, method='spearman', adjust='none')
write.table(t(ct_rppa_std_image$r),"cor_image_std_rppa_method2.txt",sep="\t")
dim(ct_rppa_std_image$p)
