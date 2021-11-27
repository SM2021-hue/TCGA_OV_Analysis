#find common genes between image-cptac and image rnaseq

# Average of features

cptac_avg=read.xls("cor_image_avg_cptac_method2.xlsx",sheet=1)
rnaseq_avg=read.xls("cor_image_avg_rnaseq_method2.xlsx",sheet=1)
majoraxis_avg=intersect(subset(cptac_avg, Major.Axis <= -0.8 |  Major.Axis >= 0.8)[,1],subset(rnaseq_avg, Major.Axis <= -0.8 |  Major.Axis >= 0.8)[,1])
minoraxis_avg=intersect(subset(cptac_avg, Minor.Axis <= -0.8 |  Minor.Axis >= 0.8)[,1],subset(rnaseq_avg, Minor.Axis <= -0.8 |  Minor.Axis >= 0.8)[,1])
ratio_avg=intersect(subset(cptac_avg, Ratio <= -0.8 |  Ratio >= 0.8)[,1],subset(rnaseq_avg, Ratio <= -0.8 |  Ratio >= 0.8)[,1])
area_avg=intersect(subset(cptac_avg, Nuclear.Area <= -0.8 |  Nuclear.Area >= 0.8)[,1],subset(rnaseq_avg, Nuclear.Area <= -0.8 |  Nuclear.Area >= 0.8)[,1])
mindistance_avg=intersect(subset(cptac_avg, minimum.distance <= -0.8 |  minimum.distance >= 0.8)[,1],subset(rnaseq_avg, minimum.distance <= -0.8 |  minimum.distance >= 0.8)[,1])
maxdistance_avg=intersect(subset(cptac_avg, maximum.distance <= -0.8 |  maximum.distance >= 0.8)[,1],subset(rnaseq_avg, maximum.distance <= -0.8 |  maximum.distance >= 0.8)[,1])
meandistance_avg=intersect(subset(cptac_avg, mean.distance <= -0.8 |  mean.distance >= 0.8)[,1],subset(rnaseq_avg, mean.distance <= -0.8 |  mean.distance >= 0.8)[,1])

write.table(paste(majoraxis_avg,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_average.txt",append=T,col.names = F)
write.table(paste(minoraxis_avg,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_average.txt",append=T,col.names = F)
write.table(paste(ratio_avg,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_average.txt",append=T,col.names = F)
write.table(paste(area_avg,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_average.txt",append=T,col.names = F)
write.table(paste(mindistance_avg,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_average.txt",append=T,col.names = F)
write.table(paste(maxdistance_avg,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_average.txt",append=T,col.names = F)
write.table(paste(meandistance_avg,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_average.txt",append=T,col.names = F)

################### stdev of features

cptac_std=read.xls("cor_image_std_cptac_method2.xlsx",sheet=1)
rnaseq_std=read.xls("cor_image_std_rnaseq_method2.xlsx",sheet=1)
majoraxis_std=intersect(subset(cptac_std, Major.Axis <= -0.8 |  Major.Axis >= 0.8)[,1],subset(rnaseq_std, Major.Axis <= -0.8 |  Major.Axis >= 0.8)[,1])
minoraxis_std=intersect(subset(cptac_std, Minor.Axis <= -0.8 |  Minor.Axis >= 0.8)[,1],subset(rnaseq_std, Minor.Axis <= -0.8 |  Minor.Axis >= 0.8)[,1])
ratio_std=intersect(subset(cptac_std, Ratio <= -0.8 |  Ratio >= 0.8)[,1],subset(rnaseq_std, Ratio <= -0.8 |  Ratio >= 0.8)[,1])
area_std=intersect(subset(cptac_std, Nuclear.Area <= -0.8 |  Nuclear.Area >= 0.8)[,1],subset(rnaseq_std, Nuclear.Area <= -0.8 |  Nuclear.Area >= 0.8)[,1])
mindistance_std=intersect(subset(cptac_std, minimum.distance <= -0.8 |  minimum.distance >= 0.8)[,1],subset(rnaseq_std, minimum.distance <= -0.8 |  minimum.distance >= 0.8)[,1])
maxdistance_std=intersect(subset(cptac_std, maximum.distance <= -0.8 |  maximum.distance >= 0.8)[,1],subset(rnaseq_std, maximum.distance <= -0.8 |  maximum.distance >= 0.8)[,1])
meandistance_std=intersect(subset(cptac_std, mean.distance <= -0.8 |  mean.distance >= 0.8)[,1],subset(rnaseq_std, mean.distance <= -0.8 |  mean.distance >= 0.8)[,1])

write.table(paste(majoraxis_std,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_std.txt",append=T,col.names = F)
write.table(paste(minoraxis_std,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_std.txt",append=T,col.names = F)
write.table(paste(ratio_std,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_std.txt",append=T,col.names = F)
write.table(paste(area_std,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_std.txt",append=T,col.names = F)
write.table(paste(mindistance_std,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_std.txt",append=T,col.names = F)
write.table(paste(maxdistance_std,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_std.txt",append=T,col.names = F)
write.table(paste(meandistance_std,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_std.txt",append=T,col.names = F)


# variance of features

cptac_var=read.xls("cor_image_var_cptac_method2.xlsx",sheet=1)
rnaseq_var=read.xls("cor_image_var_rnaseq_method2.xlsx",sheet=1)
majoraxis_var=intersect(subset(cptac_var, Major.Axis <= -0.8 |  Major.Axis >= 0.8)[,1],subset(rnaseq_var, Major.Axis <= -0.8 |  Major.Axis >= 0.8)[,1])
minoraxis_var=intersect(subset(cptac_var, Minor.Axis <= -0.8 |  Minor.Axis >= 0.8)[,1],subset(rnaseq_var, Minor.Axis <= -0.8 |  Minor.Axis >= 0.8)[,1])
ratio_var=intersect(subset(cptac_var, Ratio <= -0.8 |  Ratio >= 0.8)[,1],subset(rnaseq_var, Ratio <= -0.8 |  Ratio >= 0.8)[,1])
area_var=intersect(subset(cptac_var, Nuclear.Area <= -0.8 |  Nuclear.Area >= 0.8)[,1],subset(rnaseq_var, Nuclear.Area <= -0.8 |  Nuclear.Area >= 0.8)[,1])
mindistance_var=intersect(subset(cptac_var, minimum.distance <= -0.8 |  minimum.distance >= 0.8)[,1],subset(rnaseq_var, minimum.distance <= -0.8 |  minimum.distance >= 0.8)[,1])
maxdistance_var=intersect(subset(cptac_var, maximum.distance <= -0.8 |  maximum.distance >= 0.8)[,1],subset(rnaseq_var, maximum.distance <= -0.8 |  maximum.distance >= 0.8)[,1])
meandistance_var=intersect(subset(cptac_var, mean.distance <= -0.8 |  mean.distance >= 0.8)[,1],subset(rnaseq_var, mean.distance <= -0.8 |  mean.distance >= 0.8)[,1])

write.table(paste(majoraxis_var,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_var.txt",append=T,col.names = F)
write.table(paste(minoraxis_var,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_var.txt",append=T,col.names = F)
write.table(paste(ratio_var,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_var.txt",append=T,col.names = F)
write.table(paste(area_var,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_var.txt",append=T,col.names = F)
write.table(paste(mindistance_var,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_var.txt",append=T,col.names = F)
write.table(paste(maxdistance_var,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_var.txt",append=T,col.names = F)
write.table(paste(meandistance_var,sep=" ",collapse = ","),"CPTAC-RNAseq-Image_var.txt",append=T,col.names = F)

