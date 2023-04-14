######################################################################
# The R codes for reading the data are given in https://arxiv.org/abs/2012.08488v1
# 
######################################################################



T1=G1; T2=G2; T3=G3; T4=G4; 
N=length(T1)

RESCUYR <- numeric(); RESCU <- numeric()
h=1; r=4; s=0

for(h in 1:N){
  vp=c(T1[h], T2[h], T3[h], T4[h])
  if(rescuable(vp,r)==1){
    s=s+1
    RESCUYR[s]=h
  }
}

NRESCUYR=s

#edit(cbind(T1[RESCUYR], T2[RESCUYR], T3[RESCUYR],T4[RESCUYR]))

T1S=T1[RESCUYR]; T2S=T2[RESCUYR]; T3S=T3[RESCUYR]; T4S=T4[RESCUYR]
T1SC=T1S[(T1S<9)&(T2S<9)&(T3S<9)]
T2SC=T2S[(T1S<9)&(T2S<9)&(T3S<9)]
T3SC=T3S[(T1S<9)&(T2S<9)&(T3S<9)]
T4SC=T4S[(T1S<9)&(T2S<9)&(T3S<9)]

#edit(cbind(T1SC, T2SC, T3SC, T4SC))
index=seq(1,N)

V1=T1[(T1>0) & (T1<9) & (T2>0) & (T2<9) & (T3>0) & (T3<9) & (T4==-1)]
V2=T2[(T1>0) & (T1<9) & (T2>0) & (T2<9) & (T3>0) & (T3<9) & (T4==-1)]
V3=T3[(T1>0) & (T1<9) & (T2>0) & (T2<9) & (T3>0) & (T3<9) & (T4==-1)]
V4=T4[(T1>0) & (T1<9) & (T2>0) & (T2<9) & (T3>0) & (T3<9) & (T4==-1)]
Y1=year[(T1>0) & (T1<9) & (T2>0) & (T2<9) & (T3>0) & (T3<9) & (T4==-1)]
R1=region[(T1>0) & (T1<9) & (T2>0) & (T2<9) & (T3>0) & (T3<9) & (T4==-1)]
index1=index[(T1>0) & (T1<9) & (T2>0) & (T2<9) & (T3>0) & (T3<9) & (T4==-1)]

#edit(cbind(index1,Y1,R1,V1,V2,V3,V4))

yearOfStudy=2015
regionOfStudy=2
genderY=gender[year==yearOfStudy]
regionY=regionA[year==yearOfStudy]
G1Y=G1[year==yearOfStudy]
G2Y=G2[year==yearOfStudy]
G3Y=G3[year==yearOfStudy]
G4Y=G4[year==yearOfStudy]
#========================
genderR=gender[region==regionOfStudy]
G1R=G1[region==regionOfStudy]
G2R=G2[region==regionOfStudy]
G3R=G3[region==regionOfStudy]
G4R=G4[region==regionOfStudy]

# study of region and year

genderYR=gender[(region==regionOfStudy) & (year==yearOfStudy)]
G1YR=G1[(region==regionOfStudy) & (year==yearOfStudy)]
G2YR=G2[(region==regionOfStudy) & (year==yearOfStudy)]
G3YR=G3[(region==regionOfStudy) & (year==yearOfStudy)]
G4YR=G4[(region==regionOfStudy) & (year==yearOfStudy)]
#length(G4YR)

#c=c(1,2,3,4) # exclut G4
#c=c(1,2,4,3) # exclut G3
#c=c(1,3,4,2) # exclut G2
#c=c(2,4,3,1) # exclut G1

#T1=G1YR; T2=G2YR; T3=G3YR; T4=G4YR;
g=2
T1=G1YR[genderYR==g]; T2=G2YR[genderYR==g]; 
T3=G3YR[genderYR==g]; T4=G4YR[genderYR==g];
#T1=G1YR; T2=G2YR; T3=G3YR; T4=G4YR;

b=1
B=100
mpfB=0
mfpB=0
#B=1
for(b in 1:B){
  
  #length(T1)
  
  #####  Training - testing Data
  
  (N=length(T1))
  (n=ceiling((2*N)/3))
  (n=1.5*round((2*N)/3)+ 2000)
  (t=round(runif(n,1,N)))
  
  td<-numeric()
  k=1
  td[1] = t[1]
  for(h in 2:n){
    if(presenceTest(t[h],td,k)==0){
      k=k+1
      td[k] = t[h]
    }
  }
  #sort(td)
  N=length(T1)
  n=k
  (100*n/N)
  
  
  # Supervised Learning.
  #Traing Data.
  
  (TT1=T1[td])
  (TT2=T2[td])
  (TT3=T3[td])
  (TT4=T4[td])
  
  length(TT1)
  
  #Test data
  
  (TS1=T1[-td])
  (TS2=T2[-td])
  (TS3=T3[-td])
  (TS4=T4[-td])
  #length(TS1)
  #length(TT1)+length(TS)
  (ntrain=n)
  (ntest=N-n)
  
  lm1 = lm(TT4 ~ TT1 + TT2 + TT3)
  #summary(lm1)
  
  
  TSE=lm1$coef[1]+(lm1$coef[2]*TS1)+(lm1$coef[2]*TS2)+(lm1$coef[3]*TS3)
  
  #### Errors
  
  mpf=0
  mfp=0
  totalpass=0
  totalfail=0
  h=0
  
  for(h in 1:ntest){
    
    #if((TS4[h]<9) & (TS1[h]<9) & (TS2[h]<9) & (TS3[h]<9) ){
    if((TS4[h]<9) ){
      totalpass=totalpass+1
      
      if(TSE[h]>8){
        mpf=mpf+1
      }
      
    }
    else{
      totalfail=totalfail+1
      if(TSE[h]<0){
        mfp=mfp+1
      }
      
    }
  }
  #totalpass
  #totalfail
  #totalpass+totalfail
  mpfB=mpfB+ ((mpf/totalpass)/B)
  mfpB=mfpB+ ((mfp/totalfail)/B)
}
summary(lm1)
100*mpfB
100*mfpB

#caseT=80183
#caseT=77833
#caseT=77594
caseT=122915
VC=c(G1[caseT], G2[caseT], G3[caseT], G4[caseT])
#VC
###############################
yearOfStudy=2017
regionOfStudy=1

genderY=gender[year==yearOfStudy]
regionY=regionA[year==yearOfStudy]
G1Y=G1[year==yearOfStudy]
G2Y=G2[year==yearOfStudy]
G3Y=G3[year==yearOfStudy]
G4Y=G4[year==yearOfStudy]
#========================
genderR=gender[region==regionOfStudy]
G1R=G1[region==regionOfStudy]
G2R=G2[region==regionOfStudy]
G3R=G3[region==regionOfStudy]
G4R=G4[region==regionOfStudy]

# study of region and year
indexC=seq(1,length(G1))
length(indexC)
genderYR=gender[(region==regionOfStudy) & (year==yearOfStudy)]
G1YR=G1[(region==regionOfStudy) & (year==yearOfStudy)]
G2YR=G2[(region==regionOfStudy) & (year==yearOfStudy)]
G3YR=G3[(region==regionOfStudy) & (year==yearOfStudy)]
G4YR=G4[(region==regionOfStudy) & (year==yearOfStudy)]
indexCYR=indexC[(region==regionOfStudy) & (year==yearOfStudy)]
#length(G4YR)

#c=c(1,2,3,4) # exclut G4
#c=c(1,2,4,3) # exclut G3
#c=c(1,3,4,2) # exclut G2
#c=c(2,4,3,1) # exclut G1

#T1=G1YR; T2=G2YR; T3=G3YR; T4=G4YR;
g=2
T1=G1YR[genderYR==g]; T2=G2YR[genderYR==g]; 
T3=G3YR[genderYR==g]; T4=G4YR[genderYR==g];

T1=G1YR; T2=G2YR; T3=G3YR; T4=G4YR;
b=1
B=10
mpfB=0
mfpB=0
#B=1
grade4P=0
grade4F=0

for(b in 1:B){
  #####  Training - testing Data
  (N=length(T1))
  (n=ceiling((2*N)/3))
  (n=1.5*round((2*N)/3)+ 2000)
  (t=round(runif(n,1,N)))
  
  td<-numeric()
  k=1
  td[1] = t[1]
  for(h in 2:n){
    if(presenceTest(t[h],td,k)==0){
      k=k+1
      td[k] = t[h]
    }
  }
  #sort(td)
  N=length(T1)
  n=k
  (100*n/N)
  
  # Supervised Learning.
  #Traing Data.
  (TT1=T1[td])
  (TT2=T2[td])
  (TT3=T3[td])
  (TT4=T4[td])
  #length(TT1)
  
  #Test data
  (TS1=T1[-td])
  (TS2=T2[-td])
  (TS3=T3[-td])
  (TS4=T4[-td])
  #length(TS1)
  #length(TT1)+length(TS)
  (ntrain=n)
  (ntest=N-n)
  
  lm1 = lm(TT4 ~ TT1 + TT2 + TT3)
  #summary(lm1)
  
  VCE=lm1$coef[1]+(lm1$coef[2]*VC[1])+(lm1$coef[2]**VC[2])+(lm1$coef[3]**VC[3])
  if(VCE>8){
    grade4F=grade4F+(1/B)
  }
  else{
    grade4P=grade4P+(1/B)
  }
}
grade4P

yearOfStudy=2015
regionOfStudy=1


genderAY=genderA[yearA==yearOfStudy]
#length(genderAY)

regionAY=regionA[yearA==yearOfStudy]
#length(regionAY)

G1AY=G1A[yearA==yearOfStudy]
length(G1AY)

G2AY=G2A[yearA==yearOfStudy]
length(G2AY)

G3AY=G3A[yearA==yearOfStudy]
#length(G3AY)

G4AY=G4A[yearA==yearOfStudy]
#length(G4AY)

# study of region
genderAR=genderA[regionA==regionOfStudy]
G1AR=G1A[regionA==regionOfStudy]
G2AR=G2A[regionA==regionOfStudy]
G3AR=G3A[regionA==regionOfStudy]
G4AR=G4A[regionA==regionOfStudy]

# study of region and year

genderAYR=genderA[(regionA==regionOfStudy) & (yearA==yearOfStudy)]
G1AYR=G1A[(regionA==regionOfStudy) & (yearA==yearOfStudy)]
G2AYR=G2A[(regionA==regionOfStudy) & (yearA==yearOfStudy)]
G3AYR=G3A[(regionA==regionOfStudy) & (yearA==yearOfStudy)]
G4AYR=G4A[(regionA==regionOfStudy) & (yearA==yearOfStudy)]
#length(G4AYR)

#proceed to your analysis
# Choose type of data
W1 <- numeric()
W2 <- numeric()
W3 <- numeric()
W4 <- numeric()
V1 <- numeric()
V2 <- numeric()
V3 <- numeric()
V4 <- numeric()

W1=G1AYR
W2=G2AYR
W3=G3AYR
W4=G4AYR
(NT=length(W4))
#transform values credit 1 between 1 and 6, pass between 7 and 8, 3 fail for 9
for(j in 1:NT){
  if(W1[j]<7){
    V1[j]=1
  }
  else{
    if(W1[j]<9){
      V1[j]=2
    }
    else{
      V1[j]=3
    }
  }
  
  if(W2[j]<7){
    V2[j]=1
  }
  else{
    if(W2[j]<9){
      V2[j]=2
    }
    else{
      V2[j]=3
    }
  }
  
  if(W3[j]<7){
    V3[j]=1
  }
  else{
    if(W3[j]<9){
      V3[j]=2
    }
    else{
      V3[j]=3
    }
  }
  
  if(W4[j]<7){
    V4[j]=1
  }
  else{
    if(W4[j]<9){
      V4[j]=2
    }
    else{
      V4[j]=3
    }
  }
}
V1
V2
V3
V4
##   Choice of de X1, X2, X3, X4
X1=W1;X2=W2; X3=W3; X4=W4
#X1=V1;X2=V2; X3=V3; X4=V4
## ================================
## BIGGEST LOOP FOR B EXPERIENCES

B=100
b=1
mpfMG1=0
mfpMG1=0
mpf2MG1=0
mfp2MG1=0

mpfMG2=0
mfpMG2=0
mpf2MG2=0
mfp2MG2=0

for(b in 1:B){
  # grande boucle
  
  (N=length(X4))
  (n=ceiling((2*N)/3))
  (n=1.5*round((2*N)/3)+ 2000)
  (t=round(runif(n,1,N)))
  
  #length(X4)
  #n
  
  td<-numeric()
  k=1
  td[1] = t[1]
  for(h in 2:n){
    if(presenceTest(t[h],td,k)==0){
      k=k+1
      td[k] = t[h]
    }
  }
  #sort(td)
  k
  (n=k)
  (100*n/N)
  
  
  # Supervised Learning.
  #Traing Data.
  
  (XT1=X1[td])
  (XT2=X2[td])
  (XT3=X3[td])
  (XT4=X4[td])
  
  length(XT1)
  
  #Test data
  (XTD1=X1[-td])
  (XTD2=X2[-td])
  (XTD3=X3[-td])
  (XTD4=X4[-td])
  length(XTD1)
  length(XT1)+length(XTD1)
  ntrain=n
  ntest=N-n
  
  #Building the model
  #K=20
  # Estimation of X4 for the first testing data.
  c<- numeric()
  p<- numeric()
  D<-numeric()
  freq<- numeric()
  freqMFM <- numeric()
  distTestToTrain <- numeric()
  group<- numeric()
  F<- numeric()
  h=1
  size = 3
  q=0
  o=1
  gradex4=0
  sum=0
  v=1
  K=100 #nearest neighbors
  
  for(s in 1:ntest){
    for(u in 1:9){freq[u]=0}
    q=0
    gradex4=0
    c=c(XTD1[s], XTD2[s], XTD3[s])
    
    for(r in 1:ntrain){
      
      p=c(XT1[r], XT2[r], XT3[r])
      sum=0
      distTestToTrain[r]=distance(h,p,c,size)
      for(o in 1:size){
        if(p[o] != c[o]) sum=sum+1
      } 
      if(sum==0){
        q=q+1 
        gradex4=gradex4+XT4[r]
        for(u in 1:9){
          if(XT4[r]==u) freq[u]=freq[u]+1
        }
      }
      
    }
    if(q>K){
      group[s]=1
      D[s] = gradex4/q
      F[s]=freq[order(freq)][9]
    }
    else{
      group[s]=-1
      D[s]=lowerPartialMean(XT4[order(distTestToTrain)],K)
      F[s]=0
      
      for(u in 1:7){freqMFM[u]=0}
      for(v in 1:K){
        for(u in 1:9){
          if(XT4[order(distTestToTrain)][v]==u) freqMFM[u]=freqMFM[u]+1
        }
      }
      F[s]=freqMFM[order(freqMFM)][9]
      
      
    }
  }
  
  # misclassification
  g=1
  Z1=XTD4[group==g]
  Z2=D[group==g]
  Z3=F[group==1]
  
  #length(Z1)
  #length(Z3)
  nsg=length(Z1)
  mpf=0
  mfp=0
  mpf2=0
  mfp2=0
  
  for(r in 1:nsg){
    if(Z1[r]<9){
      if(Z2[r]>8){
        mpf=mpf+1
      }
      
      if(Z3[r]>8){
        mpf2=mpf2+1
      }
    }
    else{
      if(Z2[r]<9){
        mfp=mfp+1
      }
      
      if(Z3[r]<9){
        mfp2=mfp2+1
      }
    }
  }
  
  mpfMG1=mpfMG1+ ((mpf/nsg)/B)
  mfpMG1=mfpMG1+ ((mfp/nsg)/B)
  mpf2MG1=mpf2MG1+ ((mpf2/nsg)/B)
  mfp2MG1=mfp2MG1+ ((mfp2/nsg)/B)
  
  g=-1
  Z1=XTD4[group==g]
  Z2=D[group==g]
  Z3=F[group==1]
  
  #length(Z1)
  #length(Z3)
  nsg=length(Z1)
  mpf=0
  mfp=0
  mpf2=0
  mfp2=0
  
  for(r in 1:nsg){
    if(Z1[r]<9){
      if(Z2[r]>8){
        mpf=mpf+1
      }
      
      if(Z3[r]>8){
        mpf2=mpf2+1
      }
    }
    else{
      if(Z2[r]<9){
        mfp=mfp+1
      }
      
      if(Z3[r]<9){
        mfp2=mfp2+1
      }
    }
  }
  
  mpfMG2=mpfMG2+ ((mpf/nsg)/B)
  mfpMG2=mfpMG2+ ((mfp/nsg)/B)
  mpf2MG2=mpf2MG2+ ((mpf2/nsg)/B)
  mfp2MG2=mfp2MG2+ ((mfp2/nsg)/B)
  # end of the big loop
}

# Group +1
100*mpfMG1
100*mfpMG1
100*mpf2MG1
100*mfp2MG1

# Group -1
100*mpfMG2
100*mfpMG2
100*mpf2MG2
100*mfp2MG2

#Function Presence Test
L=c(0,2,5,8)
freq <- numeric()
test=0
xx=1
NN=1

presenceTest <- function(xx,L,NN){
  test=0
  for(h in 1:(NN)){
    if(L[h]==xx){
      test=1
    }
  }
  return(test)
}

c<- numeric()
p <- numeric()
size=1
dist=0
average=0
r=0
subsize=1
totsize=1

lowerPartialMean <- function(c,size){
  average=0
  for(r in 1:size){
    average=average+c[r]
  }
  average=average/size
  return(average)
}

upperPartialMean <- function(c,subsize,totsize){
  average=0
  for(r in 1:subsize){
    average=average+c[totsize-r+1]
  }
  average=average/subsize
  return(average)
}

c=c(1,0,3,6,10)
upperPartialMean(c,2,5)
c[4], c[5]

distance <- function(h,p,c,size){
  if(h==1){
    dist=0
    for(j in 1:size){
      dist=dist+(p[j]-c[j])^2
    }
    return(dist)
  }
  
  if(h==2){
    dist=0
    for(j in 1:size){
      if(abs(p[j]-c[j])>dist){
        dist=abs(p[j]-c[j])
      }
    }
    return(dist)
  }
}
#End of distance function
