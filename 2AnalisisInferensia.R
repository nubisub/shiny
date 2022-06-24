Gejala <<- c()
pvalue <<- c()
proportiontest <- function(factor, data, nama) {
  hasil <- chisq.test(data, as.factor(factor))
  Gejala <<- append(Gejala, nama)
  pvalue <<- append(pvalue, round(hasil$p.value,3))
}

myrange <- 7:22
for (i in myrange){
  if((length(unique(df[,i])))<2){
    next
  }
  print(i)
  proportiontest( df$`Jenis Kelamin`, df[,i],colnames(df)[i])
}
x <- cbind(Gejala, pvalue)
as.data.frame(x)
write.csv(x, "DataProportionTest.csv")
a<-chisq.test(df[,7], factor(df$`Jenis Kelamin`))$observed
b<-chisq.test(df[,8], factor(df$`Jenis Kelamin`))$observed
c<-chisq.test(df[,10], factor(df$`Jenis Kelamin`))$observed
d<-chisq.test(df[,11], factor(df$`Jenis Kelamin`))$observed
e<-chisq.test(df[,12], factor(df$`Jenis Kelamin`))$observed
f<-chisq.test(df[,13], factor(df$`Jenis Kelamin`))$observed
g<-chisq.test(df[,14], factor(df$`Jenis Kelamin`))$observed
h<-chisq.test(df[,15], factor(df$`Jenis Kelamin`))$observed
i<-chisq.test(df[,16], factor(df$`Jenis Kelamin`))$observed
j<-chisq.test(df[,17], factor(df$`Jenis Kelamin`))$observed
k<-chisq.test(df[,18], factor(df$`Jenis Kelamin`))$observed
l<-chisq.test(df[,19], factor(df$`Jenis Kelamin`))$observed
m<-chisq.test(df[,20], factor(df$`Jenis Kelamin`))$observed
n<-chisq.test(df[,21], factor(df$`Jenis Kelamin`))$observed
o<-chisq.test(df[,22], factor(df$`Jenis Kelamin`))$observed
dg<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
as.data.frame(dg)
library(xlsx)
write.xlsx(dg,"contingenct2.xlsx")
