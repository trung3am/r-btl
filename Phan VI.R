library(xlsx)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(tibble)


#Problem 2------------------

names(table(GKCDR.1922))

names(table(CKCDR.1922))

#Learning outcome that appeared in both mid/final exam is 12,23,31
#Learning outcome 23 is accounted for md 1922 midterm

TotalQFrameGKEX
t(TotalQFrameCK)


GLOW12
GLOW23
GLOW31

#learning outcome 12 has 19-20 questions in both exam according to chart we 
#see that students has one wrong answer is our target

#learning outcome 23 has 3 questions in both exam according to chart we 
#see that there is no student have right answer for all 3 questions
#our target is in made 1922 midterm with one right answer and one right answer in final

#learning outcome 31 has 9 questions in both exam according to chart we 
#see that students has no wrong answer is our target

#set param p to 1
p<-1

#set param q to 1
q<-1

#---------------------------------------------------------------

#reuse DiemGK & DiemCK

DiemGKCK.2<- cbind( DiemGK[1:361,],DiemCK)

#add combined made
DiemGKCK.2$mdCK_GK <- as.numeric(paste(DiemGKCK.2[,29] ,DiemGKCK.2[,62], sep = ""))
#rearrange
DiemGKCK.2 <- DiemGKCK.2[,c(63,1:28, 33:61)]

colnames(DiemGKCK.2)[c(5:29)] <- c(1:25)
colnames(DiemGKCK.2)[c(30:58)] <- c(26:54)


#function return learning outcome number of each md
getlonumgk <- function(md,lo){
  return(
    ncol(GKCDR[GKCDR[,1] == md, GKCDR[GKCDR[,1] == md,] == lo])
  )
}

getlonumck <- function(md,lo){
  return(
    ncol(CKCDR[CKCDR[,1] == md, CKCDR[CKCDR[,1] == md,] == lo])
  )
}

#create dataframe for each made

Diem2.1921.1921 <- apply(bind_rows(MD1921.1921,DiemGKCK.2[DiemGKCK.2[1] == 19211921,c(2,5:58)]),2,as.numeric)
Diem2.1921.1922 <- apply(bind_rows(MD1921.1922,DiemGKCK.2[DiemGKCK.2[1] == 19211922,c(2,5:58)]),2,as.numeric)
Diem2.1921.1923 <- apply(bind_rows(MD1921.1923,DiemGKCK.2[DiemGKCK.2[1] == 19211923,c(2,5:58)]),2,as.numeric)
Diem2.1921.1924 <- apply(bind_rows(MD1921.1924,DiemGKCK.2[DiemGKCK.2[1] == 19211924,c(2,5:58)]),2,as.numeric)
Diem2.1922.1921 <- apply(bind_rows(MD1922.1921,DiemGKCK.2[DiemGKCK.2[1] == 19221921,c(2,5:58)]),2,as.numeric)
Diem2.1922.1922 <- apply(bind_rows(MD1922.1922,DiemGKCK.2[DiemGKCK.2[1] == 19221922,c(2,5:58)]),2,as.numeric)
Diem2.1922.1923 <- apply(bind_rows(MD1922.1923,DiemGKCK.2[DiemGKCK.2[1] == 19221923,c(2,5:58)]),2,as.numeric)
Diem2.1922.1924 <- apply(bind_rows(MD1922.1924,DiemGKCK.2[DiemGKCK.2[1] == 19221924,c(2,5:58)]),2,as.numeric)
Diem2.1923.1921 <- apply(bind_rows(MD1923.1921,DiemGKCK.2[DiemGKCK.2[1] == 19231921,c(2,5:58)]),2,as.numeric)
Diem2.1923.1922 <- apply(bind_rows(MD1923.1922,DiemGKCK.2[DiemGKCK.2[1] == 19231922,c(2,5:58)]),2,as.numeric)
Diem2.1923.1923 <- apply(bind_rows(MD1923.1923,DiemGKCK.2[DiemGKCK.2[1] == 19231923,c(2,5:58)]),2,as.numeric)
Diem2.1923.1924 <- apply(bind_rows(MD1923.1924,DiemGKCK.2[DiemGKCK.2[1] == 19231924,c(2,5:58)]),2,as.numeric)
Diem2.1924.1921 <- apply(bind_rows(MD1924.1921,DiemGKCK.2[DiemGKCK.2[1] == 19241921,c(2,5:58)]),2,as.numeric)
Diem2.1924.1922 <- apply(bind_rows(MD1924.1922,DiemGKCK.2[DiemGKCK.2[1] == 19241922,c(2,5:58)]),2,as.numeric)
Diem2.1924.1923 <- apply(bind_rows(MD1924.1923,DiemGKCK.2[DiemGKCK.2[1] == 19241923,c(2,5:58)]),2,as.numeric)
Diem2.1924.1924 <- apply(bind_rows(MD1924.1924,DiemGKCK.2[DiemGKCK.2[1] == 19241924,c(2,5:58)]),2,as.numeric)

#create dataframe of each made for learning outcome 12

Diem2.LO12.1921.1921 <- as.data.frame(Diem2.1921.1921[,Diem2.1921.1921[1,] == 12 | is.na(Diem2.1921.1921[1,])]) 
Diem2.LO12.1921.1922 <- as.data.frame(Diem2.1921.1922[,Diem2.1921.1922[1,] == 12 | is.na(Diem2.1921.1922[1,])]) 
Diem2.LO12.1921.1923 <- as.data.frame(Diem2.1921.1923[,Diem2.1921.1923[1,] == 12 | is.na(Diem2.1921.1923[1,])]) 
Diem2.LO12.1921.1924 <- as.data.frame(Diem2.1921.1924[,Diem2.1921.1924[1,] == 12 | is.na(Diem2.1921.1924[1,])]) 
Diem2.LO12.1922.1921 <- as.data.frame(Diem2.1922.1921[,Diem2.1922.1921[1,] == 12 | is.na(Diem2.1922.1921[1,])]) 
Diem2.LO12.1922.1922 <- as.data.frame(Diem2.1922.1922[,Diem2.1922.1922[1,] == 12 | is.na(Diem2.1922.1922[1,])]) 
Diem2.LO12.1922.1923 <- as.data.frame(Diem2.1922.1923[,Diem2.1922.1923[1,] == 12 | is.na(Diem2.1922.1923[1,])]) 
Diem2.LO12.1922.1924 <- as.data.frame(Diem2.1922.1924[,Diem2.1922.1924[1,] == 12 | is.na(Diem2.1922.1924[1,])]) 
Diem2.LO12.1923.1921 <- as.data.frame(Diem2.1923.1921[,Diem2.1923.1921[1,] == 12 | is.na(Diem2.1923.1921[1,])]) 
Diem2.LO12.1923.1922 <- as.data.frame(Diem2.1923.1922[,Diem2.1923.1922[1,] == 12 | is.na(Diem2.1923.1922[1,])]) 
Diem2.LO12.1923.1923 <- as.data.frame(Diem2.1923.1923[,Diem2.1923.1923[1,] == 12 | is.na(Diem2.1923.1923[1,])]) 
Diem2.LO12.1923.1924 <- as.data.frame(Diem2.1923.1924[,Diem2.1923.1924[1,] == 12 | is.na(Diem2.1923.1924[1,])]) 
Diem2.LO12.1924.1921 <- as.data.frame(Diem2.1924.1921[,Diem2.1924.1921[1,] == 12 | is.na(Diem2.1924.1921[1,])]) 
Diem2.LO12.1924.1922 <- as.data.frame(Diem2.1924.1922[,Diem2.1924.1922[1,] == 12 | is.na(Diem2.1924.1922[1,])]) 
Diem2.LO12.1924.1923 <- as.data.frame(Diem2.1924.1923[,Diem2.1924.1923[1,] == 12 | is.na(Diem2.1924.1923[1,])]) 
Diem2.LO12.1924.1924 <- as.data.frame(Diem2.1924.1924[,Diem2.1924.1924[1,] == 12 | is.na(Diem2.1924.1924[1,])]) 


#added column number of right answer of midterm exam

Diem2.LO12.1921.1921$TLDGK <- apply(Diem2.LO12.1921.1921[,c(1:getlonumgk(1921,12))],1,sum) 
Diem2.LO12.1921.1922$TLDGK <- apply(Diem2.LO12.1921.1922[,c(1:getlonumgk(1921,12))],1,sum) 
Diem2.LO12.1921.1923$TLDGK <- apply(Diem2.LO12.1921.1923[,c(1:getlonumgk(1921,12))],1,sum) 
Diem2.LO12.1921.1924$TLDGK <- apply(Diem2.LO12.1921.1924[,c(1:getlonumgk(1921,12))],1,sum) 
Diem2.LO12.1922.1921$TLDGK <- apply(Diem2.LO12.1922.1921[,c(1:getlonumgk(1922,12))],1,sum) 
Diem2.LO12.1922.1922$TLDGK <- apply(Diem2.LO12.1922.1922[,c(1:getlonumgk(1922,12))],1,sum) 
Diem2.LO12.1922.1923$TLDGK <- apply(Diem2.LO12.1922.1923[,c(1:getlonumgk(1922,12))],1,sum) 
Diem2.LO12.1922.1924$TLDGK <- apply(Diem2.LO12.1922.1924[,c(1:getlonumgk(1922,12))],1,sum) 
Diem2.LO12.1923.1921$TLDGK <- apply(Diem2.LO12.1923.1921[,c(1:getlonumgk(1923,12))],1,sum) 
Diem2.LO12.1923.1922$TLDGK <- apply(Diem2.LO12.1923.1922[,c(1:getlonumgk(1923,12))],1,sum) 
Diem2.LO12.1923.1923$TLDGK <- apply(Diem2.LO12.1923.1923[,c(1:getlonumgk(1923,12))],1,sum) 
Diem2.LO12.1923.1924$TLDGK <- apply(Diem2.LO12.1923.1924[,c(1:getlonumgk(1923,12))],1,sum) 
Diem2.LO12.1924.1921$TLDGK <- apply(Diem2.LO12.1924.1921[,c(1:getlonumgk(1924,12))],1,sum) 
Diem2.LO12.1924.1922$TLDGK <- apply(Diem2.LO12.1924.1922[,c(1:getlonumgk(1924,12))],1,sum) 
Diem2.LO12.1924.1923$TLDGK <- apply(Diem2.LO12.1924.1923[,c(1:getlonumgk(1924,12))],1,sum) 
Diem2.LO12.1924.1924$TLDGK <- apply(Diem2.LO12.1924.1924[,c(1:getlonumgk(1924,12))],1,sum) 

#same for final

Diem2.LO12.1921.1921$TLDCK <- apply(Diem2.LO12.1921.1921[,c( (getlonumgk(1921,12) + 1) : (getlonumgk(1921,12) + getlonumck(1921,12)) )],1,sum) 
Diem2.LO12.1921.1922$TLDCK <- apply(Diem2.LO12.1921.1922[,c( (getlonumgk(1921,12) + 1) : (getlonumgk(1921,12) + getlonumck(1922,12)) )],1,sum) 
Diem2.LO12.1921.1923$TLDCK <- apply(Diem2.LO12.1921.1923[,c( (getlonumgk(1921,12) + 1) : (getlonumgk(1921,12) + getlonumck(1923,12)) )],1,sum) 
Diem2.LO12.1921.1924$TLDCK <- apply(Diem2.LO12.1921.1924[,c( (getlonumgk(1921,12) + 1) : (getlonumgk(1921,12) + getlonumck(1924,12)) )],1,sum) 
Diem2.LO12.1922.1921$TLDCK <- apply(Diem2.LO12.1922.1921[,c( (getlonumgk(1922,12) + 1) : (getlonumgk(1922,12) + getlonumck(1921,12)) )],1,sum) 
Diem2.LO12.1922.1922$TLDCK <- apply(Diem2.LO12.1922.1922[,c( (getlonumgk(1922,12) + 1) : (getlonumgk(1922,12) + getlonumck(1922,12)) )],1,sum) 
Diem2.LO12.1922.1923$TLDCK <- apply(Diem2.LO12.1922.1923[,c( (getlonumgk(1922,12) + 1) : (getlonumgk(1922,12) + getlonumck(1923,12)) )],1,sum) 
Diem2.LO12.1922.1924$TLDCK <- apply(Diem2.LO12.1922.1924[,c( (getlonumgk(1922,12) + 1) : (getlonumgk(1922,12) + getlonumck(1924,12)) )],1,sum) 
Diem2.LO12.1923.1921$TLDCK <- apply(Diem2.LO12.1923.1921[,c( (getlonumgk(1923,12) + 1) : (getlonumgk(1923,12) + getlonumck(1921,12)) )],1,sum) 
Diem2.LO12.1923.1922$TLDCK <- apply(Diem2.LO12.1923.1922[,c( (getlonumgk(1923,12) + 1) : (getlonumgk(1923,12) + getlonumck(1922,12)) )],1,sum) 
Diem2.LO12.1923.1923$TLDCK <- apply(Diem2.LO12.1923.1923[,c( (getlonumgk(1923,12) + 1) : (getlonumgk(1923,12) + getlonumck(1923,12)) )],1,sum) 
Diem2.LO12.1923.1924$TLDCK <- apply(Diem2.LO12.1923.1924[,c( (getlonumgk(1923,12) + 1) : (getlonumgk(1923,12) + getlonumck(1924,12)) )],1,sum) 
Diem2.LO12.1924.1921$TLDCK <- apply(Diem2.LO12.1924.1921[,c( (getlonumgk(1924,12) + 1) : (getlonumgk(1924,12) + getlonumck(1921,12)) )],1,sum) 
Diem2.LO12.1924.1922$TLDCK <- apply(Diem2.LO12.1924.1922[,c( (getlonumgk(1924,12) + 1) : (getlonumgk(1924,12) + getlonumck(1922,12)) )],1,sum) 
Diem2.LO12.1924.1923$TLDCK <- apply(Diem2.LO12.1924.1923[,c( (getlonumgk(1924,12) + 1) : (getlonumgk(1924,12) + getlonumck(1923,12)) )],1,sum) 
Diem2.LO12.1924.1924$TLDCK <- apply(Diem2.LO12.1924.1924[,c( (getlonumgk(1924,12) + 1) : (getlonumgk(1924,12) + getlonumck(1924,12)) )],1,sum)


#list of student meets learning outcome 12 result requirement
LO12SVList <- ("SV")

LO12SVList <- c(LO12SVList,Diem2.LO12.1921.1921[Diem2.LO12.1921.1921[,"TLDGK"] >= getlonumgk(1921,12) - q & Diem2.LO12.1921.1921[,"TLDCK"] >= getlonumck(1921,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1921.1922[Diem2.LO12.1921.1922[,"TLDGK"] >= getlonumgk(1921,12) - q & Diem2.LO12.1921.1922[,"TLDCK"] >= getlonumck(1922,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1921.1923[Diem2.LO12.1921.1923[,"TLDGK"] >= getlonumgk(1921,12) - q & Diem2.LO12.1921.1923[,"TLDCK"] >= getlonumck(1923,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1921.1924[Diem2.LO12.1921.1924[,"TLDGK"] >= getlonumgk(1921,12) - q & Diem2.LO12.1921.1924[,"TLDCK"] >= getlonumck(1924,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1922.1921[Diem2.LO12.1922.1921[,"TLDGK"] >= getlonumgk(1922,12) - q & Diem2.LO12.1922.1921[,"TLDCK"] >= getlonumck(1921,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1922.1922[Diem2.LO12.1922.1922[,"TLDGK"] >= getlonumgk(1922,12) - q & Diem2.LO12.1922.1922[,"TLDCK"] >= getlonumck(1922,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1922.1923[Diem2.LO12.1922.1923[,"TLDGK"] >= getlonumgk(1922,12) - q & Diem2.LO12.1922.1923[,"TLDCK"] >= getlonumck(1923,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1922.1924[Diem2.LO12.1922.1924[,"TLDGK"] >= getlonumgk(1922,12) - q & Diem2.LO12.1922.1924[,"TLDCK"] >= getlonumck(1924,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1923.1921[Diem2.LO12.1923.1921[,"TLDGK"] >= getlonumgk(1923,12) - q & Diem2.LO12.1923.1921[,"TLDCK"] >= getlonumck(1921,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1923.1922[Diem2.LO12.1923.1922[,"TLDGK"] >= getlonumgk(1923,12) - q & Diem2.LO12.1923.1922[,"TLDCK"] >= getlonumck(1922,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1923.1923[Diem2.LO12.1923.1923[,"TLDGK"] >= getlonumgk(1923,12) - q & Diem2.LO12.1923.1923[,"TLDCK"] >= getlonumck(1923,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1923.1924[Diem2.LO12.1923.1924[,"TLDGK"] >= getlonumgk(1923,12) - q & Diem2.LO12.1923.1924[,"TLDCK"] >= getlonumck(1924,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1924.1921[Diem2.LO12.1924.1921[,"TLDGK"] >= getlonumgk(1924,12) - q & Diem2.LO12.1924.1921[,"TLDCK"] >= getlonumck(1921,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1924.1922[Diem2.LO12.1924.1922[,"TLDGK"] >= getlonumgk(1924,12) - q & Diem2.LO12.1924.1922[,"TLDCK"] >= getlonumck(1922,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1924.1923[Diem2.LO12.1924.1923[,"TLDGK"] >= getlonumgk(1924,12) - q & Diem2.LO12.1924.1923[,"TLDCK"] >= getlonumck(1923,12) - p ,"No" ])  
LO12SVList <- c(LO12SVList,Diem2.LO12.1924.1924[Diem2.LO12.1924.1924[,"TLDGK"] >= getlonumgk(1924,12) - q & Diem2.LO12.1924.1924[,"TLDCK"] >= getlonumck(1924,12) - p ,"No" ])



#list of student meets learning outcome 12 result requirement
LO12SVList
na.omit(as.data.frame(LO12SVList))

#create dataframe of each made for learning outcome 31

Diem2.LO31.1921.1921 <- as.data.frame(Diem2.1921.1921[,Diem2.1921.1921[1,] == 31 | is.na(Diem2.1921.1921[1,])]) 
Diem2.LO31.1921.1922 <- as.data.frame(Diem2.1921.1922[,Diem2.1921.1922[1,] == 31 | is.na(Diem2.1921.1922[1,])]) 
Diem2.LO31.1921.1923 <- as.data.frame(Diem2.1921.1923[,Diem2.1921.1923[1,] == 31 | is.na(Diem2.1921.1923[1,])]) 
Diem2.LO31.1921.1924 <- as.data.frame(Diem2.1921.1924[,Diem2.1921.1924[1,] == 31 | is.na(Diem2.1921.1924[1,])]) 
Diem2.LO31.1922.1921 <- as.data.frame(Diem2.1922.1921[,Diem2.1922.1921[1,] == 31 | is.na(Diem2.1922.1921[1,])]) 
Diem2.LO31.1922.1922 <- as.data.frame(Diem2.1922.1922[,Diem2.1922.1922[1,] == 31 | is.na(Diem2.1922.1922[1,])]) 
Diem2.LO31.1922.1923 <- as.data.frame(Diem2.1922.1923[,Diem2.1922.1923[1,] == 31 | is.na(Diem2.1922.1923[1,])]) 
Diem2.LO31.1922.1924 <- as.data.frame(Diem2.1922.1924[,Diem2.1922.1924[1,] == 31 | is.na(Diem2.1922.1924[1,])]) 
Diem2.LO31.1923.1921 <- as.data.frame(Diem2.1923.1921[,Diem2.1923.1921[1,] == 31 | is.na(Diem2.1923.1921[1,])]) 
Diem2.LO31.1923.1922 <- as.data.frame(Diem2.1923.1922[,Diem2.1923.1922[1,] == 31 | is.na(Diem2.1923.1922[1,])]) 
Diem2.LO31.1923.1923 <- as.data.frame(Diem2.1923.1923[,Diem2.1923.1923[1,] == 31 | is.na(Diem2.1923.1923[1,])]) 
Diem2.LO31.1923.1924 <- as.data.frame(Diem2.1923.1924[,Diem2.1923.1924[1,] == 31 | is.na(Diem2.1923.1924[1,])]) 
Diem2.LO31.1924.1921 <- as.data.frame(Diem2.1924.1921[,Diem2.1924.1921[1,] == 31 | is.na(Diem2.1924.1921[1,])]) 
Diem2.LO31.1924.1922 <- as.data.frame(Diem2.1924.1922[,Diem2.1924.1922[1,] == 31 | is.na(Diem2.1924.1922[1,])]) 
Diem2.LO31.1924.1923 <- as.data.frame(Diem2.1924.1923[,Diem2.1924.1923[1,] == 31 | is.na(Diem2.1924.1923[1,])]) 
Diem2.LO31.1924.1924 <- as.data.frame(Diem2.1924.1924[,Diem2.1924.1924[1,] == 31 | is.na(Diem2.1924.1924[1,])]) 



#add column number of right answer of midterm exam

Diem2.LO31.1921.1921$TLDGK <- apply(Diem2.LO31.1921.1921[,c(1:getlonumgk(1921,31))],1,sum) 
Diem2.LO31.1921.1922$TLDGK <- apply(Diem2.LO31.1921.1922[,c(1:getlonumgk(1921,31))],1,sum) 
Diem2.LO31.1921.1923$TLDGK <- apply(Diem2.LO31.1921.1923[,c(1:getlonumgk(1921,31))],1,sum) 
Diem2.LO31.1921.1924$TLDGK <- apply(Diem2.LO31.1921.1924[,c(1:getlonumgk(1921,31))],1,sum) 
Diem2.LO31.1922.1921$TLDGK <- apply(Diem2.LO31.1922.1921[,c(1:getlonumgk(1922,31))],1,sum) 
Diem2.LO31.1922.1922$TLDGK <- apply(Diem2.LO31.1922.1922[,c(1:getlonumgk(1922,31))],1,sum) 
Diem2.LO31.1922.1923$TLDGK <- apply(Diem2.LO31.1922.1923[,c(1:getlonumgk(1922,31))],1,sum) 
Diem2.LO31.1922.1924$TLDGK <- apply(Diem2.LO31.1922.1924[,c(1:getlonumgk(1922,31))],1,sum) 
Diem2.LO31.1923.1921$TLDGK <- apply(Diem2.LO31.1923.1921[,c(1:getlonumgk(1923,31))],1,sum) 
Diem2.LO31.1923.1922$TLDGK <- apply(Diem2.LO31.1923.1922[,c(1:getlonumgk(1923,31))],1,sum) 
Diem2.LO31.1923.1923$TLDGK <- apply(Diem2.LO31.1923.1923[,c(1:getlonumgk(1923,31))],1,sum) 
Diem2.LO31.1923.1924$TLDGK <- apply(Diem2.LO31.1923.1924[,c(1:getlonumgk(1923,31))],1,sum) 
Diem2.LO31.1924.1921$TLDGK <- apply(Diem2.LO31.1924.1921[,c(1:getlonumgk(1924,31))],1,sum) 
Diem2.LO31.1924.1922$TLDGK <- apply(Diem2.LO31.1924.1922[,c(1:getlonumgk(1924,31))],1,sum) 
Diem2.LO31.1924.1923$TLDGK <- apply(Diem2.LO31.1924.1923[,c(1:getlonumgk(1924,31))],1,sum) 
Diem2.LO31.1924.1924$TLDGK <- apply(Diem2.LO31.1924.1924[,c(1:getlonumgk(1924,31))],1,sum) 

#same for final

Diem2.LO31.1921.1921$TLDCK <- apply(Diem2.LO31.1921.1921[,c( (getlonumgk(1921,31) + 1) : (getlonumgk(1921,31) + getlonumck(1921,31)) )],1,sum) 
Diem2.LO31.1921.1922$TLDCK <- apply(Diem2.LO31.1921.1922[,c( (getlonumgk(1921,31) + 1) : (getlonumgk(1921,31) + getlonumck(1922,31)) )],1,sum) 
Diem2.LO31.1921.1923$TLDCK <- apply(Diem2.LO31.1921.1923[,c( (getlonumgk(1921,31) + 1) : (getlonumgk(1921,31) + getlonumck(1923,31)) )],1,sum) 
Diem2.LO31.1921.1924$TLDCK <- apply(Diem2.LO31.1921.1924[,c( (getlonumgk(1921,31) + 1) : (getlonumgk(1921,31) + getlonumck(1924,31)) )],1,sum) 
Diem2.LO31.1922.1921$TLDCK <- apply(Diem2.LO31.1922.1921[,c( (getlonumgk(1922,31) + 1) : (getlonumgk(1922,31) + getlonumck(1921,31)) )],1,sum) 
Diem2.LO31.1922.1922$TLDCK <- apply(Diem2.LO31.1922.1922[,c( (getlonumgk(1922,31) + 1) : (getlonumgk(1922,31) + getlonumck(1922,31)) )],1,sum) 
Diem2.LO31.1922.1923$TLDCK <- apply(Diem2.LO31.1922.1923[,c( (getlonumgk(1922,31) + 1) : (getlonumgk(1922,31) + getlonumck(1923,31)) )],1,sum) 
Diem2.LO31.1922.1924$TLDCK <- apply(Diem2.LO31.1922.1924[,c( (getlonumgk(1922,31) + 1) : (getlonumgk(1922,31) + getlonumck(1924,31)) )],1,sum) 
Diem2.LO31.1923.1921$TLDCK <- apply(Diem2.LO31.1923.1921[,c( (getlonumgk(1923,31) + 1) : (getlonumgk(1923,31) + getlonumck(1921,31)) )],1,sum) 
Diem2.LO31.1923.1922$TLDCK <- apply(Diem2.LO31.1923.1922[,c( (getlonumgk(1923,31) + 1) : (getlonumgk(1923,31) + getlonumck(1922,31)) )],1,sum) 
Diem2.LO31.1923.1923$TLDCK <- apply(Diem2.LO31.1923.1923[,c( (getlonumgk(1923,31) + 1) : (getlonumgk(1923,31) + getlonumck(1923,31)) )],1,sum) 
Diem2.LO31.1923.1924$TLDCK <- apply(Diem2.LO31.1923.1924[,c( (getlonumgk(1923,31) + 1) : (getlonumgk(1923,31) + getlonumck(1924,31)) )],1,sum) 
Diem2.LO31.1924.1921$TLDCK <- apply(Diem2.LO31.1924.1921[,c( (getlonumgk(1924,31) + 1) : (getlonumgk(1924,31) + getlonumck(1921,31)) )],1,sum) 
Diem2.LO31.1924.1922$TLDCK <- apply(Diem2.LO31.1924.1922[,c( (getlonumgk(1924,31) + 1) : (getlonumgk(1924,31) + getlonumck(1922,31)) )],1,sum) 
Diem2.LO31.1924.1923$TLDCK <- apply(Diem2.LO31.1924.1923[,c( (getlonumgk(1924,31) + 1) : (getlonumgk(1924,31) + getlonumck(1923,31)) )],1,sum) 
Diem2.LO31.1924.1924$TLDCK <- apply(Diem2.LO31.1924.1924[,c( (getlonumgk(1924,31) + 1) : (getlonumgk(1924,31) + getlonumck(1924,31)) )],1,sum)


#list of student meets learning outcome 31 result requirement
LO31SVList <- ("SV")


LO31SVList <- c(LO31SVList,Diem2.LO31.1921.1921[Diem2.LO31.1921.1921[,"TLDGK"] >= getlonumgk(1921,31) - q & Diem2.LO31.1921.1921[,"TLDCK"] >= getlonumck(1921,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1921.1922[Diem2.LO31.1921.1922[,"TLDGK"] >= getlonumgk(1921,31) - q & Diem2.LO31.1921.1922[,"TLDCK"] >= getlonumck(1922,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1921.1923[Diem2.LO31.1921.1923[,"TLDGK"] >= getlonumgk(1921,31) - q & Diem2.LO31.1921.1923[,"TLDCK"] >= getlonumck(1923,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1921.1924[Diem2.LO31.1921.1924[,"TLDGK"] >= getlonumgk(1921,31) - q & Diem2.LO31.1921.1924[,"TLDCK"] >= getlonumck(1924,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1922.1921[Diem2.LO31.1922.1921[,"TLDGK"] >= getlonumgk(1922,31) - q & Diem2.LO31.1922.1921[,"TLDCK"] >= getlonumck(1921,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1922.1922[Diem2.LO31.1922.1922[,"TLDGK"] >= getlonumgk(1922,31) - q & Diem2.LO31.1922.1922[,"TLDCK"] >= getlonumck(1922,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1922.1923[Diem2.LO31.1922.1923[,"TLDGK"] >= getlonumgk(1922,31) - q & Diem2.LO31.1922.1923[,"TLDCK"] >= getlonumck(1923,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1922.1924[Diem2.LO31.1922.1924[,"TLDGK"] >= getlonumgk(1922,31) - q & Diem2.LO31.1922.1924[,"TLDCK"] >= getlonumck(1924,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1923.1921[Diem2.LO31.1923.1921[,"TLDGK"] >= getlonumgk(1923,31) - q & Diem2.LO31.1923.1921[,"TLDCK"] >= getlonumck(1921,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1923.1922[Diem2.LO31.1923.1922[,"TLDGK"] >= getlonumgk(1923,31) - q & Diem2.LO31.1923.1922[,"TLDCK"] >= getlonumck(1922,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1923.1923[Diem2.LO31.1923.1923[,"TLDGK"] >= getlonumgk(1923,31) - q & Diem2.LO31.1923.1923[,"TLDCK"] >= getlonumck(1923,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1923.1924[Diem2.LO31.1923.1924[,"TLDGK"] >= getlonumgk(1923,31) - q & Diem2.LO31.1923.1924[,"TLDCK"] >= getlonumck(1924,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1924.1921[Diem2.LO31.1924.1921[,"TLDGK"] >= getlonumgk(1924,31) - q & Diem2.LO31.1924.1921[,"TLDCK"] >= getlonumck(1921,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1924.1922[Diem2.LO31.1924.1922[,"TLDGK"] >= getlonumgk(1924,31) - q & Diem2.LO31.1924.1922[,"TLDCK"] >= getlonumck(1922,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1924.1923[Diem2.LO31.1924.1923[,"TLDGK"] >= getlonumgk(1924,31) - q & Diem2.LO31.1924.1923[,"TLDCK"] >= getlonumck(1923,31) - p ,"No" ])  
LO31SVList <- c(LO31SVList,Diem2.LO31.1924.1924[Diem2.LO31.1924.1924[,"TLDGK"] >= getlonumgk(1924,31) - q & Diem2.LO31.1924.1924[,"TLDCK"] >= getlonumck(1924,31) - p ,"No" ])

#list of student meets learning outcome 31 result requirement
LO31SVList
na.omit(as.data.frame(LO31SVList))



#Learning outcome 23 handling exception data of midterm MD1922
#Learning outcome 23 only have 1 question in midterm (MD1922) and 2 in final
TotalQFrameGKEX

#create dataframe of each made for learning outcome 23

Diem2.LO23.1922.1921 <- as.data.frame(Diem2.1922.1921[,Diem2.1922.1921[1,] == 23 | is.na(Diem2.1922.1921[1,])]) 
Diem2.LO23.1922.1922 <- as.data.frame(Diem2.1922.1922[,Diem2.1922.1922[1,] == 23 | is.na(Diem2.1922.1922[1,])]) 
Diem2.LO23.1922.1923 <- as.data.frame(Diem2.1922.1923[,Diem2.1922.1923[1,] == 23 | is.na(Diem2.1922.1923[1,])]) 
Diem2.LO23.1922.1924 <- as.data.frame(Diem2.1922.1924[,Diem2.1922.1924[1,] == 23 | is.na(Diem2.1922.1924[1,])]) 

#add column right answer of midterm exam

Diem2.LO23.1922.1921$TLDGK <- Diem2.LO23.1922.1921[1]
Diem2.LO23.1922.1922$TLDGK <- Diem2.LO23.1922.1922[1]
Diem2.LO23.1922.1923$TLDGK <- Diem2.LO23.1922.1923[1]
Diem2.LO23.1922.1924$TLDGK <- Diem2.LO23.1922.1924[1]

#add final

Diem2.LO23.1922.1921$TLDCK <- apply(Diem2.LO23.1922.1921[,c( 2 : (1 + getlonumck(1921,23)) )],1,sum) 
Diem2.LO23.1922.1922$TLDCK <- apply(Diem2.LO23.1922.1922[,c( 2 : (1 + getlonumck(1922,23)) )],1,sum) 
Diem2.LO23.1922.1923$TLDCK <- apply(Diem2.LO23.1922.1923[,c( 2 : (1 + getlonumck(1923,23)) )],1,sum) 
Diem2.LO23.1922.1924$TLDCK <- apply(Diem2.LO23.1922.1924[,c( 2 : (1 + getlonumck(1924,23)) )],1,sum) 

#list of student meets of 23 outcome
LO23SVList <- ("SV")

LO23SVList <- c(LO23SVList,Diem2.LO23.1922.1921[Diem2.LO23.1922.1921[,"TLDGK"] >= getlonumgk(1922,23) - q & Diem2.LO23.1922.1921[,"TLDCK"] >= getlonumck(1921,23) - p ,"No" ])  
LO23SVList <- c(LO23SVList,Diem2.LO23.1922.1922[Diem2.LO23.1922.1922[,"TLDGK"] >= getlonumgk(1922,23) - q & Diem2.LO23.1922.1922[,"TLDCK"] >= getlonumck(1922,23) - p ,"No" ])  
LO23SVList <- c(LO23SVList,Diem2.LO23.1922.1923[Diem2.LO23.1922.1923[,"TLDGK"] >= getlonumgk(1922,23) - q & Diem2.LO23.1922.1923[,"TLDCK"] >= getlonumck(1923,23) - p ,"No" ])  
LO23SVList <- c(LO23SVList,Diem2.LO23.1922.1924[Diem2.LO23.1922.1924[,"TLDGK"] >= getlonumgk(1922,23) - q & Diem2.LO23.1922.1924[,"TLDCK"] >= getlonumck(1924,23) - p ,"No" ])  

LO23SVList
na.omit(as.data.frame(LO23SVList))


#a conclusion : with q = = p = 1 
#(student only have one answer wrong in midterm and also in final on the same outcome )
# p,q is number of wrong answers permitted to be accepted at doing good at exam
#p,q can be set and reset.
#list student are below:
na.omit(as.data.frame(LO12SVList))
na.omit(as.data.frame(LO31SVList))
na.omit(as.data.frame(LO23SVList))
#Problem 3------------------------
# IDK
# 

GKMN<- DiemGK[,c(2,4:28)]

Nhom1<-GKMN[GKMN[,1] == "L01",]
Nhom2<-GKMN[GKMN[,1] == "L02",]
Nhom3<-GKMN[GKMN[,1] == "L03",]
Nhom1$TongDiem <-round(rowSums(apply(Nhom1[,c(2:26)],2,as.numeric))/25*10,1)
#average of each Nhom
mean(round(rowSums(apply(Nhom1[,c(2:26)],2,as.numeric))/25*10,1))
mean(round(rowSums(apply(Nhom2[,c(2:26)],2,as.numeric))/25*10,1))
mean(round(rowSums(apply(Nhom3[,c(2:26)],2,as.numeric))/25*10,1))
#median of each Nhom
median(round(rowSums(apply(Nhom1[,c(2:26)],2,as.numeric))/25*10,1))
median(round(rowSums(apply(Nhom2[,c(2:26)],2,as.numeric))/25*10,1))
median(round(rowSums(apply(Nhom3[,c(2:26)],2,as.numeric))/25*10,1))

#Frequency of each Nhom according to Diem

FNhom1 <-as.data.frame(table(round(rowSums(apply(Nhom1[,c(2:26)],2,as.numeric))/25*10,1)))
colnames(FNhom1) <- c("Diem", "Percentage")
FNhom1$Percentage <- round(FNhom1$Percentage/sum(FNhom1$Percentage)*100,2)

FNhom2 <-as.data.frame(table(round(rowSums(apply(Nhom2[,c(2:26)],2,as.numeric))/25*10,1)))
colnames(FNhom2) <- c("Diem", "Percentage")
FNhom2$Percentage <- round(FNhom2$Percentage/sum(FNhom2$Percentage)*100,2)

FNhom3 <-as.data.frame(table(round(rowSums(apply(Nhom3[,c(2:26)],2,as.numeric))/25*10,1)))
colnames(FNhom3) <- c("Diem", "Percentage")
FNhom3$Percentage <- round(FNhom3$Percentage/sum(FNhom3$Percentage)*100,2)

GNhom1 <- ggplot(FNhom1, aes(x = as.character(FNhom1[,1]), y = FNhom1[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=FNhom1[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Nhom 1", x = "Diem", y = "%SV cua nhom") + theme(panel.background = element_rect(fill = "#d557d5"))

GNhom2 <- ggplot(FNhom2, aes(x = as.character(FNhom2[,1]), y = FNhom2[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=FNhom2[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Nhom 2", x = "Diem", y = "%SV cua nhom") + theme(panel.background = element_rect(fill = "#d557d5"))

GNhom3 <- ggplot(FNhom3, aes(x = as.character(FNhom3[,1]), y = FNhom3[,2])) + geom_bar(stat = "identity") + geom_text(aes(label=FNhom3[,2]), vjust=-0.3, size=3.5, )+
  labs(title = "Nhom 3", x = "Diem", y = "%SV cua nhom") + theme(panel.background = element_rect(fill = "#d557d5"))

GNhom <- ggarrange(GNhom1,GNhom2,GNhom3, ncol = 3)

GNhom

#conclusion: 
#Nhom3 has high grade with highest average grade
#Nhom1 has low grade with lowest average grade
#Nhom2 has average grade slightly lower than Nhom3




#Problem 6-----------------------------------------------------------------------
#we trying to find question that only less than kp% of student have the right answer
#which kp is our threshold
#let's set kp = 10% (we could reset kp if we want)

kp <- 10


#OutCome----------------------------------------------------------------


GKOutcome <- read.xlsx2("192_CO1007.xlsx", sheetIndex = 3, startRow = 1, endRow = 16)

#select Learning outcome

GKOutcome <- GKOutcome[7:10, 1:26]
table(apply(GKOutcome[,-1],2,as.numeric))/4


CKOutcome <- read.xlsx2("192_CO1007.xlsx", sheetIndex = 5, startRow = 1, endRow = 16)
CKOutcome <- CKOutcome[7:10, 1:30]
#select Learning outcome
table(apply(CKOutcome[,-1],2,as.character))/4

#Outcome in midterm 1-7
#Outcome in final 8-11 & 6

#Building data frame

DiemGK.Outcome.1921 <- DiemGK.1921
DiemGK.Outcome.1922 <- DiemGK.1922
DiemGK.Outcome.1923 <- DiemGK.1923
DiemGK.Outcome.1924 <- DiemGK.1924

DiemCK.Outcome.1921 <- DiemCK.1921
DiemCK.Outcome.1922 <- DiemCK.1922
DiemCK.Outcome.1923 <- DiemCK.1923
DiemCK.Outcome.1924 <- DiemCK.1924


colnames(DiemGK.Outcome.1921) <- paste("Lo",GKOutcome[GKOutcome[,1]==1921,-1],"-Q",c(1:25), "-De21", sep  = "")
colnames(DiemGK.Outcome.1922) <- paste("Lo",GKOutcome[GKOutcome[,1]==1922,-1],"-Q",c(1:25), "-De22", sep  = "")
colnames(DiemGK.Outcome.1923) <- paste("Lo",GKOutcome[GKOutcome[,1]==1923,-1],"-Q",c(1:25), "-De23", sep  = "")
colnames(DiemGK.Outcome.1924) <- paste("Lo",GKOutcome[GKOutcome[,1]==1924,-1],"-Q",c(1:25), "-De24", sep  = "")

colnames(DiemCK.Outcome.1921) <- paste("Lo",CKOutcome[CKOutcome[,1]==1921,-1],"-Q",c(1:25), "-De21", sep  = "")
colnames(DiemCK.Outcome.1922) <- paste("Lo",CKOutcome[CKOutcome[,1]==1922,-1],"-Q",c(1:25), "-De22", sep  = "")
colnames(DiemCK.Outcome.1923) <- paste("Lo",CKOutcome[CKOutcome[,1]==1923,-1],"-Q",c(1:25), "-De23", sep  = "")
colnames(DiemCK.Outcome.1924) <- paste("Lo",CKOutcome[CKOutcome[,1]==1924,-1],"-Q",c(1:25), "-De24", sep  = "")



DiemGK.Outcome.1921 <-round(colSums(apply(DiemGK.Outcome.1921[-1,],2,as.numeric))/nrow(DiemGK.1921)*100,1)
DiemGK.Outcome.1922 <-round(colSums(apply(DiemGK.Outcome.1922[-1,],2,as.numeric))/nrow(DiemGK.1922)*100,1)
DiemGK.Outcome.1923 <-round(colSums(apply(DiemGK.Outcome.1923[-1,],2,as.numeric))/nrow(DiemGK.1923)*100,1)
DiemGK.Outcome.1924 <-round(colSums(apply(DiemGK.Outcome.1924[-1,],2,as.numeric))/nrow(DiemGK.1924)*100,1)

DiemCK.Outcome.1921 <-round(colSums(apply(DiemCK.Outcome.1921[-1,],2,as.numeric))/nrow(DiemCK.1921)*100,1)
DiemCK.Outcome.1922 <-round(colSums(apply(DiemCK.Outcome.1922[-1,],2,as.numeric))/nrow(DiemCK.1922)*100,1)
DiemCK.Outcome.1923 <-round(colSums(apply(DiemCK.Outcome.1923[-1,],2,as.numeric))/nrow(DiemCK.1923)*100,1)
DiemCK.Outcome.1924 <-round(colSums(apply(DiemCK.Outcome.1924[-1,],2,as.numeric))/nrow(DiemCK.1924)*100,1)


#Question and Outcome need to be revised listed as below
#Outcome : 11-21-32-23
DiemGK.Outcome.1921[DiemGK.Outcome.1921 < kp]
DiemGK.Outcome.1922[DiemGK.Outcome.1922 < kp]
DiemGK.Outcome.1923[DiemGK.Outcome.1923 < kp]
DiemGK.Outcome.1924[DiemGK.Outcome.1924 < kp]

DiemCK.Outcome.1921[DiemCK.Outcome.1921 < kp]
DiemCK.Outcome.1922[DiemCK.Outcome.1922 < kp]
DiemCK.Outcome.1923[DiemCK.Outcome.1923 < kp]
DiemCK.Outcome.1924[DiemCK.Outcome.1924 < kp]


#For Chapter ----

GKChapter <- read.xlsx2("192_CO1007.xlsx", sheetIndex = 3, startRow = 1, endRow = 16)

#select Learning outcome

GKChapter <- GKChapter[12:15, 1:26]
table(apply(GKChapter[,-1],2,as.numeric))/4


CKChapter <- read.xlsx2("192_CO1007.xlsx", sheetIndex = 5, startRow = 1, endRow = 16)
CKChapter <- CKChapter[12:15, 1:30]
#select Learning outcome
table(apply(CKChapter[,-1],2,as.character))/4

#Chapter in midterm 1-7
#Chapter in final 8-11 & 6

#Building data frame

DiemGK.Chapter.1921 <- DiemGK.1921
DiemGK.Chapter.1922 <- DiemGK.1922
DiemGK.Chapter.1923 <- DiemGK.1923
DiemGK.Chapter.1924 <- DiemGK.1924

DiemCK.Chapter.1921 <- DiemCK.1921
DiemCK.Chapter.1922 <- DiemCK.1922
DiemCK.Chapter.1923 <- DiemCK.1923
DiemCK.Chapter.1924 <- DiemCK.1924


colnames(DiemGK.Chapter.1921) <- paste("Ch",GKChapter[GKChapter[,1]==1921,-1],"-Q",c(1:25), "-De21", sep  = "")
colnames(DiemGK.Chapter.1922) <- paste("Ch",GKChapter[GKChapter[,1]==1922,-1],"-Q",c(1:25), "-De22", sep  = "")
colnames(DiemGK.Chapter.1923) <- paste("Ch",GKChapter[GKChapter[,1]==1923,-1],"-Q",c(1:25), "-De23", sep  = "")
colnames(DiemGK.Chapter.1924) <- paste("Ch",GKChapter[GKChapter[,1]==1924,-1],"-Q",c(1:25), "-De24", sep  = "")

colnames(DiemCK.Chapter.1921) <- paste("Ch",CKChapter[CKChapter[,1]==1921,-1],"-Q",c(1:25), "-De21", sep  = "")
colnames(DiemCK.Chapter.1922) <- paste("Ch",CKChapter[CKChapter[,1]==1922,-1],"-Q",c(1:25), "-De22", sep  = "")
colnames(DiemCK.Chapter.1923) <- paste("Ch",CKChapter[CKChapter[,1]==1923,-1],"-Q",c(1:25), "-De23", sep  = "")
colnames(DiemCK.Chapter.1924) <- paste("Ch",CKChapter[CKChapter[,1]==1924,-1],"-Q",c(1:25), "-De24", sep  = "")



DiemGK.Chapter.1921 <-round(colSums(apply(DiemGK.Chapter.1921[-1,],2,as.numeric))/nrow(DiemGK.1921)*100,1)
DiemGK.Chapter.1922 <-round(colSums(apply(DiemGK.Chapter.1922[-1,],2,as.numeric))/nrow(DiemGK.1922)*100,1)
DiemGK.Chapter.1923 <-round(colSums(apply(DiemGK.Chapter.1923[-1,],2,as.numeric))/nrow(DiemGK.1923)*100,1)
DiemGK.Chapter.1924 <-round(colSums(apply(DiemGK.Chapter.1924[-1,],2,as.numeric))/nrow(DiemGK.1924)*100,1)

DiemCK.Chapter.1921 <-round(colSums(apply(DiemCK.Chapter.1921[-1,],2,as.numeric))/nrow(DiemCK.1921)*100,1)
DiemCK.Chapter.1922 <-round(colSums(apply(DiemCK.Chapter.1922[-1,],2,as.numeric))/nrow(DiemCK.1922)*100,1)
DiemCK.Chapter.1923 <-round(colSums(apply(DiemCK.Chapter.1923[-1,],2,as.numeric))/nrow(DiemCK.1923)*100,1)
DiemCK.Chapter.1924 <-round(colSums(apply(DiemCK.Chapter.1924[-1,],2,as.numeric))/nrow(DiemCK.1924)*100,1)


#Question and Chapter need to be revised listed as below
#Chapter : 1-2-9-10-11
DiemGK.Chapter.1921[DiemGK.Chapter.1921 < kp]
DiemGK.Chapter.1922[DiemGK.Chapter.1922 < kp]
DiemGK.Chapter.1923[DiemGK.Chapter.1923 < kp]
DiemGK.Chapter.1924[DiemGK.Chapter.1924 < kp]

DiemCK.Chapter.1921[DiemCK.Chapter.1921 < kp]
DiemCK.Chapter.1922[DiemCK.Chapter.1922 < kp]
DiemCK.Chapter.1923[DiemCK.Chapter.1923 < kp]
DiemCK.Chapter.1924[DiemCK.Chapter.1924 < kp]

