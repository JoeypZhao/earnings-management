# earnings-management
research about earnings management

################################################################
data<-read.csv("E:\\earnings management\\EM.csv")
data.n<-na.omit(data) #delete NA
data.n<-data.n[data.n$indfmt=='INDL',] #choose the industrial format
data.n<-data.n[data.n$curcd=='USD',] #choose the USD currency

data1<-cbind(data.n$gvkey,data.n$fyear,data.n$act,data.n$lct,data.n$ch,data.n$revt)
colnames(data1)<-c('gvkey','date','act','lct','ch','revt')

data2<-cbind(data.n$gvkey,data.n$fyear,data.n$at)
colnames(data2)<-c('atgvkey','atyear','at')
data2<-data2[1:(length(data2[,1])-1),]

##calculate /Delta cash, current asset, current liability
d<-cbind(data.n[-1,c(1,3,12,14)],diff(data1[,c(1,3:6)]),data2)
d<-d[d[,5]==0,]
d<-d[,-5]

##calculate total accruals
ta<-as.data.frame(d$act-d$ch-d$lct-d$dp)
colnames(ta)<-c('ta')
d<-cbind(d,ta)

############################################################
##DeAngelo 1986########################################
abta<-cbind(d[-1,c(1:2)],diff(as.matrix(d[,c(1,12)])))
abta<-abta[abta[,3]==0,]
abta<-abta[,-3]

abta_firm<-abta$gvkey
datasize<-matrix(NA,length(abta$gvkey),1)

for(i in 1:length(abta_firm)){
  datasize[i]<-length(abta[abta$gvkey==abta_firm[i],1])
}
abta<-cbind(abta,datasize)

reg_abta<-abta[abta$datasize>5,]
reg_abta_firm<-unique(reg_abta$gvkey)
t_test<-matrix(NA,length(reg_abta_firm),3)
colnames(t_test)<-c('t-statistic','p-value','df')

for (i in 1:length(reg_abta_firm)) {
  t<-t.test(reg_abta$ta[reg_abta$gvkey==reg_abta_firm[i]],alternative = 'greater')
  t_test[i,1]<-t$statistic
  t_test[i,2]<-t$p.value
  t_test[i,3]<-t$parameter
}


##############################################################
##calculate ta/at, 1/at, revt/at, ppegt/at
e4<-cbind(d$ta/d$at,1/d$at,d$revt/d$at,d$ppegt/d$at)
colnames(e4)<-c('ta/at','1/at','revt/at','ppegt/at')
e<-cbind(d,e4)

##delete infinity value
e<-e[e$`1/at`!=Inf,]

##delete firms with zero value of revt/at and ppegt/at
zero<-matrix(1,1,2)
for (i in 1:length(e$gvkey)) {
  if(e$`revt/at`[i]==0){
    zero<-rbind(zero,c(e$gvkey[i],e$`revt/at`[i]))
  }
  else{
    next
  }
}
zero<-zero[-1,]
zero<-unique(zero)

zero2<-matrix(1,1,2)
for (i in 1:length(e$gvkey)) {
  if(e$`ppegt/at`[i]==0){
    zero2<-rbind(zero2,c(e$gvkey[i],e$`ppegt/at`[i]))
  }
  else{
    next
  }
}
zero2<-zero2[-1,]
zero2<-unique(zero2)

for (i in 1:length(zero)) {
  e<-e[e$gvkey!=zero[i],]
}

for (i in 1:length(zero2)) {
  e<-e[e$gvkey!=zero2[i],]
}



##choose year 2014 as prediction period and years prior to 2014 as estimation period
firm<-as.data.frame(e[e$fyear==2014,1])
colnames(firm)<-c('gvkey')
ne<-merge(e,firm,by='gvkey')
ne<-ne[ne$fyear<=2014,]

ne<-ne[order(ne$fyear),]
ne<-ne[order(ne$gvkey),]
ne<-unique(ne)

ne<-ne[ne$`ppegt/at`!=0,]
ne<-ne[ne$`revt/at`!=0,]

##create firm index for each firm
firmdex<-matrix(NA,length(ne$gvkey),1)
colnames(firmdex)<-c('firmdex')
ne<-cbind(ne,firmdex)
ne$firmdex[1]<-1

for(i in 2:length(ne$gvkey)){
  if(ne[i,1]==ne[i-1,1]){
    ne$firmdex[i]<-ne$firmdex[i-1]
  }
  else{
    ne$firmdex[i]<-ne$firmdex[i-1]+1
  }
}

##calculate data size of each firm
number<-ne$firmdex[length(ne$firmdex)]
firm1<-matrix(NA,number,1)
firm1<-cbind(c(1:number),firm1)

for(i in 1:number){
  firm1[i,2]<-length(ne[ne$firmdex==i,1])
}


##choose firms with estimation period greater than 10 years
firm2<-firm1[firm1[,2]>10,]
colnames(firm2)<-c('firmdex','firm_number')
final_e<-merge(ne,firm2,by='firmdex')

final_e<-final_e[order(final_e$fyear),]
final_e<-final_e[order(final_e$gvkey),]
final_e<-unique(final_e)


##create new firm index for each firms
for(i in 2:length(final_e$gvkey)){
  if(final_e$gvkey[i]==final_e$gvkey[i-1]){
    final_e$firmdex[i]<-final_e$firmdex[i-1]
  }
  else{
    final_e$firmdex[i]<-final_e$firmdex[i-1]+1
  }
}


##divide final data into prediction part and estimation part
est_e<-final_e[final_e$fyear<2014,]
pre_e<-final_e[final_e$fyear==2014,]

##run regression
coeff<-matrix(NA,est_e$firmdex[length(est_e$firmdex)],3)
for (i in 1:est_e$firmdex[length(est_e$firmdex)]) {
  temp_e<-est_e[est_e$firmdex==i,]
  reg<-lm(`ta/at`~ 0+`1/at`+`revt/at`+`ppegt/at`,temp_e)
  coeff[i,]<-reg$coefficients
}
coeff<-as.data.frame(coeff)
colnames(coeff)<-c('a','b1','b2')

##calculate u_ip
u<-pre_e$`ta/at`-(coeff$a*pre_e$`1/at`+coeff$b1*pre_e$`revt/at`+coeff$b2*pre_e$`ppegt/at`)
u<-as.data.frame(u)

##calculate \hat{\sigma}(u_ip)
sig<-matrix(NA,est_e$firmdex[length(est_e$firmdex)],1)
for (i in 1:est_e$firmdex[length(est_e$firmdex)]) {
  x<-as.matrix(est_e[est_e$firmdex==i,c(15:17)])
  xp<-as.matrix(pre_e[pre_e$firmdex==i,c(15:17)])
  y<-xp%*%solve(t(x)%*%x)%*%t(xp)
  sig[i]<-y+diag(1,1)
}
si<-sqrt(var(sig))

##calculate V_ip
v<-u/si
colnames(v)<-c('V_i2014')

write.csv(v,'E:\\earnings management\\V.csv')
