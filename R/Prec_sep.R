#M1 Test for independence
PreconeCP<-function(X,indT,min_step=3){
  X[,2]<-cumsum(X[,2])
  X[,1]<-X[,1]
  s1=numeric()
  s2=numeric()
  for (j in (min_step:(indT[1]-1))){
    sample1=X[1:j,2]
    sample2=X[j:indT[3],2]
    s1[j]=summary(lm(sample1~seq_along(sample1)))$coefficients[2]
    s2[j]=summary(lm(sample2~seq_along(sample2)))$coefficients[2]
    xwm=which.max(s2-s1)

  }

  P_M1=data.frame(x=X[c((xwm+1)),1])

  return(P_M1)
}


  #M2 Test3dim
PrectwoCP<-function(X,indT,s_p=4,min_step=3){
  X[,2]<-cumsum(X[,2])
  X[,1]<-as.Date(X[,1], format="%d.%m.%Y")
  Mat_3=matrix(data=NA,nrow=length(1:(indT[3]+s_p)),ncol=length(1:(indT[3]+s_p)))
  s1=numeric()
  s2=numeric()
  s3=numeric()

  #  for (j in (min_step:(indT[3]+s_p-(2*min_step)))){
  for (j in (min_step:(indT[1]-1))){
    for (k in ((j+(min_step-1)):(indT[3]+s_p-(min_step)))){
      sample1=X[1:j,2]
      sample2=X[j:k,2]
      sample3=X[k:(indT[3]+s_p),2]
      s1=summary(lm(sample1~seq_along(sample1)))$coefficients[2]
      s2=summary(lm(sample2~seq_along(sample2)))$coefficients[2]
      s3=summary(lm(sample3~seq_along(sample3)))$coefficients[2]
      #Mat_3[j,k]=abs(s2-s1)+abs(s2-s3)
      #Mat_3[j,k]=s2-s1+s2-s3
      Mat_3[j,k]=abs((s1+s3)/(2*s2))

    }
  }

  Mat_3[is.infinite(Mat_3)]=9999
  #Exp
  xwm=which(Mat_3 == min(Mat_3, na.rm=TRUE), arr.ind = TRUE)
  P_M2=data.frame(x=X[c((xwm[1]+1)),1])

  return(P_M2)
}
