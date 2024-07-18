rm(list=ls())
data<-read.delim("C:/Users/Avell 5/Downloads/expression.txt",header=T, dec=',')
data
control<-data[which(data$Biogroup_sample == 'Untreated'),]
mean_control<-as.data.frame(tapply(control$Mean.Cq, control$Target, mean))
colnames(mean_control)<-c('meanCq')
for (rn in seq(1:nrow(data))){
  #print(rn)
  #print(data[rn,])
  #print(mean_control[data[rn,1],1])
  data[rn,'DeltaCq']<-mean_control[data[rn,1],1]-data[rn,'Mean.Cq']
  data[rn,'RQ']<-2^data[rn,'DeltaCq']
}
data
goi<-data[which(data$Type == 'GOI'),]

for (rn in seq(1:nrow(goi))){
  data[which(data$Type == 'Housekeeping' & data$Replicate == goi[rn,'Replicate'] & data$Biogroup_sample == goi[rn,'Biogroup_sample']),'RQ']
  goi[rn,'Norm.Factor']<-exp(mean(log(data[which(data$Type == 'Housekeeping' & data$Replicate == goi[rn,'Replicate'] & data$Biogroup_sample == goi[rn,'Biogroup_sample']),'RQ'])))
  goi[rn,'NormalizedExpression']<-goi[rn,'RQ']/goi[rn,'Norm.Factor']
  goi[rn,'Log2NormalizedExpression']<-log2(goi[rn,'NormalizedExpression'])
}
goi

pp<as.data.frame(matrix(nrow = 2, ncol=8))
colnames(pp)<-c('Target','Biogroup_sample','biogroupExpresionGeomean','','','','','','','')
for (goiid in unique(goi$Target)){
  print(goiid)
  for (biogroup in c('Treated','Untreated')){
    print(biogroup)
    biogroupExpresionGeomean<-exp(mean(log(goi[which(goi$Target == goiid & goi$Biogroup_sample == biogroup),'NormalizedExpression'])))
    print(biogroupExpresionGeomean)
  }
}

