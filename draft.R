#rename column
GKCDR.colrename <-GKCDR
CKCDR.colrename <-CKCDR
colnames(GKCDR.colrename)[2:26] <- c(1:25)
colnames(CKCDR.colrename)[2:30] <- c(26:54)


MD1921.1921 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1921,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1921,-1])
MD1921.1922 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1921,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1922,-1])
MD1921.1923 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1921,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1923,-1])
MD1921.1924 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1921,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1924,-1])
MD1922.1921 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1922,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1921,-1])
MD1922.1922 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1922,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1922,-1])
MD1922.1923 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1922,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1923,-1])
MD1922.1924 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1922,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1924,-1])
MD1923.1921 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1923,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1921,-1])
MD1923.1922 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1923,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1922,-1])
MD1923.1923 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1923,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1923,-1])
MD1923.1924 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1923,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1924,-1])
MD1924.1921 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1924,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1921,-1])
MD1924.1922 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1924,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1922,-1])
MD1924.1923 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1924,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1923,-1])
MD1924.1924 <- cbind(GKCDR.colrename[GKCDR.colrename[1] == 1924,-1],
                     CKCDR.colrename[CKCDR.colrename[1] == 1924,-1])

ncol(MD1924.1924)

Diem1921.1921<-DiemGKCK[DiemGKCK[1] == 19211921,]
Diem1921.1922<-DiemGKCK[DiemGKCK[1] == 19211922,]
Diem1921.1923<-DiemGKCK[DiemGKCK[1] == 19211923,]
Diem1921.1924<-DiemGKCK[DiemGKCK[1] == 19211924,]
Diem1922.1921<-DiemGKCK[DiemGKCK[1] == 19221921,]
Diem1922.1922<-DiemGKCK[DiemGKCK[1] == 19221922,]
Diem1922.1923<-DiemGKCK[DiemGKCK[1] == 19221923,]
Diem1922.1924<-DiemGKCK[DiemGKCK[1] == 19221924,]
Diem1923.1921<-DiemGKCK[DiemGKCK[1] == 19231921,]
Diem1923.1922<-DiemGKCK[DiemGKCK[1] == 19231922,]
Diem1923.1923<-DiemGKCK[DiemGKCK[1] == 19231923,]
Diem1923.1924<-DiemGKCK[DiemGKCK[1] == 19231924,]
Diem1924.1921<-DiemGKCK[DiemGKCK[1] == 19241921,]
Diem1924.1922<-DiemGKCK[DiemGKCK[1] == 19241922,]
Diem1924.1923<-DiemGKCK[DiemGKCK[1] == 19241923,]
Diem1924.1924<-DiemGKCK[DiemGKCK[1] == 19241924,]

Diem1921.1921 <- apply(bind_rows(MD1921.1921,Diem1921.1921[,c(3:56)]),2,as.numeric)
Diem1921.1922 <- apply(bind_rows(MD1921.1922,Diem1921.1922[,c(3:56)]),2,as.numeric)
Diem1921.1923 <- apply(bind_rows(MD1921.1923,Diem1921.1923[,c(3:56)]),2,as.numeric)
Diem1921.1924 <- apply(bind_rows(MD1921.1924,Diem1921.1924[,c(3:56)]),2,as.numeric)
Diem1922.1921 <- apply(bind_rows(MD1922.1921,Diem1922.1921[,c(3:56)]),2,as.numeric)
Diem1922.1922 <- apply(bind_rows(MD1922.1922,Diem1922.1922[,c(3:56)]),2,as.numeric)
Diem1922.1923 <- apply(bind_rows(MD1922.1923,Diem1922.1923[,c(3:56)]),2,as.numeric)
Diem1922.1924 <- apply(bind_rows(MD1922.1924,Diem1922.1924[,c(3:56)]),2,as.numeric)
Diem1923.1921 <- apply(bind_rows(MD1923.1921,Diem1923.1921[,c(3:56)]),2,as.numeric)
Diem1923.1922 <- apply(bind_rows(MD1923.1922,Diem1923.1922[,c(3:56)]),2,as.numeric)
Diem1923.1923 <- apply(bind_rows(MD1923.1923,Diem1923.1923[,c(3:56)]),2,as.numeric)
Diem1923.1924 <- apply(bind_rows(MD1923.1924,Diem1923.1924[,c(3:56)]),2,as.numeric)
Diem1924.1921 <- apply(bind_rows(MD1924.1921,Diem1924.1921[,c(3:56)]),2,as.numeric)
Diem1924.1922 <- apply(bind_rows(MD1924.1922,Diem1924.1922[,c(3:56)]),2,as.numeric)
Diem1924.1923 <- apply(bind_rows(MD1924.1923,Diem1924.1923[,c(3:56)]),2,as.numeric)
Diem1924.1924 <- apply(bind_rows(MD1924.1924,Diem1924.1924[,c(3:56)]),2,as.numeric)

#last 5 students did midterm exam but did not do final exam 
last5.1922 <- DiemGK[362:366,]
last5.1923 <- DiemGK[362:366,]
last5.1924 <- DiemGK[362:366,]
last5.1922 <- last5.1922 %>% filter(MADE==1922) %>% select(X1:X25) %>% apply(2,as.numeric)
last5.1923 <- last5.1923 %>% filter(MADE==1923) %>% select(X1:X25) %>% apply(2,as.numeric)
last5.1924 <- last5.1924 %>% filter(MADE==1924) %>% select(X1:X25) %>% apply(2,as.numeric)

last5.1922 <- as.data.frame(rbind(GKCDR.1922,last5.1922))
last5.1923 <- as.data.frame(rbind(GKCDR.1923,last5.1923))
last5.1924 <- as.data.frame(rbind(GKCDR.1924,last5.1924))


#Learning outcome 12

LO12.last5.1922 <- table(apply(last5.1922[-1,last5.1922[1,]==12],1,sum))
LO12.last5.1923 <- table(apply(last5.1923[-1,last5.1923[1,]==12],1,sum))
LO12.last5.1924 <- table(apply(last5.1924[-1,last5.1924[1,]==12],1,sum))

rownames(LO12.last5.1922) <- abs(as.numeric(rownames(LO12.last5.1922)) - ncol(GKCDR[GKCDR[,1]==1922,GKCDR[2,] == 12]))
rownames(LO12.last5.1923) <- abs(as.numeric(rownames(LO12.last5.1923)) - ncol(GKCDR[GKCDR[,1]==1923,GKCDR[2,] == 12]))
rownames(LO12.last5.1924) <- abs(as.numeric(rownames(LO12.last5.1924)) - ncol(GKCDR[GKCDR[,1]==1924,GKCDR[2,] == 12]))


LO12.Diem1921.1921 <- table(apply(Diem1921.1921[-1,Diem1921.1921[1,]==12],1,sum))
LO12.Diem1921.1922 <- table(apply(Diem1921.1922[-1,Diem1921.1922[1,]==12],1,sum))
LO12.Diem1921.1923 <- table(apply(Diem1921.1923[-1,Diem1921.1923[1,]==12],1,sum))
LO12.Diem1921.1924 <- table(apply(Diem1921.1924[-1,Diem1921.1924[1,]==12],1,sum))
LO12.Diem1922.1921 <- table(apply(Diem1922.1921[-1,Diem1922.1921[1,]==12],1,sum))
LO12.Diem1922.1922 <- table(apply(Diem1922.1922[-1,Diem1922.1922[1,]==12],1,sum))
LO12.Diem1922.1923 <- table(apply(Diem1922.1923[-1,Diem1922.1923[1,]==12],1,sum))
LO12.Diem1922.1924 <- table(apply(Diem1922.1924[-1,Diem1922.1924[1,]==12],1,sum))
LO12.Diem1923.1921 <- table(apply(Diem1923.1921[-1,Diem1923.1921[1,]==12],1,sum))
LO12.Diem1923.1922 <- table(apply(Diem1923.1922[-1,Diem1923.1922[1,]==12],1,sum))
LO12.Diem1923.1923 <- table(apply(Diem1923.1923[-1,Diem1923.1923[1,]==12],1,sum))
LO12.Diem1923.1924 <- table(apply(Diem1923.1924[-1,Diem1923.1924[1,]==12],1,sum))
LO12.Diem1924.1921 <- table(apply(Diem1924.1921[-1,Diem1924.1921[1,]==12],1,sum))
LO12.Diem1924.1922 <- table(apply(Diem1924.1922[-1,Diem1924.1922[1,]==12],1,sum))
LO12.Diem1924.1923 <- table(apply(Diem1924.1923[-1,Diem1924.1923[1,]==12],1,sum))
LO12.Diem1924.1924 <- table(apply(Diem1924.1924[-1,Diem1924.1924[1,]==12],1,sum))

#make count right answer -> wrong answer
rownames(LO12.Diem1921.1921) <- abs(as.numeric(rownames(LO12.Diem1921.1921)) - ncol(MD1921.1921[,MD1921.1921[1,]==12]))
rownames(LO12.Diem1921.1922) <- abs(as.numeric(rownames(LO12.Diem1921.1922)) - ncol(MD1921.1922[,MD1921.1922[1,]==12]))
rownames(LO12.Diem1921.1923) <- abs(as.numeric(rownames(LO12.Diem1921.1923)) - ncol(MD1921.1923[,MD1921.1923[1,]==12]))
rownames(LO12.Diem1921.1924) <- abs(as.numeric(rownames(LO12.Diem1921.1924)) - ncol(MD1921.1924[,MD1921.1924[1,]==12]))

#different is here
rownames(LO12.Diem1922.1921) <- abs(as.numeric(rownames(LO12.Diem1922.1921)) - ncol(MD1922.1921[,MD1922.1921[1,]==12]))
rownames(LO12.Diem1922.1922) <- abs(as.numeric(rownames(LO12.Diem1922.1922)) - ncol(MD1922.1922[,MD1922.1922[1,]==12]))
rownames(LO12.Diem1922.1923) <- abs(as.numeric(rownames(LO12.Diem1922.1923)) - ncol(MD1922.1923[,MD1922.1923[1,]==12]))
rownames(LO12.Diem1922.1924) <- abs(as.numeric(rownames(LO12.Diem1922.1924)) - ncol(MD1922.1924[,MD1922.1924[1,]==12]))


#-----------------

rownames(LO12.Diem1923.1921) <- abs(as.numeric(rownames(LO12.Diem1923.1921)) - ncol(MD1923.1921[,MD1923.1921[1,]==12]))
rownames(LO12.Diem1923.1922) <- abs(as.numeric(rownames(LO12.Diem1923.1922)) - ncol(MD1923.1922[,MD1923.1922[1,]==12]))
rownames(LO12.Diem1923.1923) <- abs(as.numeric(rownames(LO12.Diem1923.1923)) - ncol(MD1923.1923[,MD1923.1923[1,]==12]))
rownames(LO12.Diem1923.1924) <- abs(as.numeric(rownames(LO12.Diem1923.1924)) - ncol(MD1923.1924[,MD1923.1924[1,]==12]))
rownames(LO12.Diem1924.1921) <- abs(as.numeric(rownames(LO12.Diem1924.1921)) - ncol(MD1924.1921[,MD1924.1921[1,]==12]))
rownames(LO12.Diem1924.1922) <- abs(as.numeric(rownames(LO12.Diem1924.1922)) - ncol(MD1924.1922[,MD1924.1922[1,]==12]))
rownames(LO12.Diem1924.1923) <- abs(as.numeric(rownames(LO12.Diem1924.1923)) - ncol(MD1924.1923[,MD1924.1923[1,]==12]))
rownames(LO12.Diem1924.1924) <- abs(as.numeric(rownames(LO12.Diem1924.1924)) - ncol(MD1924.1924[,MD1924.1924[1,]==12]))




LOW12 <- as.data.frame(bind_rows(LO12.Diem1921.1921,
                                 LO12.Diem1921.1922,
                                 LO12.Diem1921.1923,
                                 LO12.Diem1921.1924,
                                 LO12.Diem1922.1921,
                                 LO12.Diem1922.1922,
                                 LO12.Diem1922.1923,
                                 LO12.Diem1922.1924,
                                 LO12.Diem1923.1921,
                                 LO12.Diem1923.1922,
                                 LO12.Diem1923.1923,
                                 LO12.Diem1923.1924,
                                 LO12.Diem1924.1921,
                                 LO12.Diem1924.1922,
                                 LO12.Diem1924.1923,
                                 LO12.Diem1924.1924,
                                 LO12.last5.1922,
                                 LO12.last5.1923,
                                 LO12.last5.1924
                                 ))

LOW12[is.na(LOW12)] <- 0

LOW12 <- as.data.frame(apply(LOW12,2,sum))
LOW12$SoCauTLSai <- as.numeric(row.names(LOW12))

LOW12$percent <- round(LOW12[,1]/sum(LOW12[,1]),3)

#graph----

GLOW12 <- ggplot(LOW12, aes(x = as.character(LOW12[,2]), y = LOW12[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW12[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 12", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))


#Learning outcome 23

#expected to be 1
LO23.last5.1922 <- table(last5.1922[-1,last5.1922[1,]==23])

rownames(LO23.last5.1922) <- abs(as.numeric(rownames(LO23.last5.1922)) - ncol(GKCDR[GKCDR[,1]==1922,GKCDR[2,] == 23]))

LO23.Diem1921.1921 <- table(apply(Diem1921.1921[-1,Diem1921.1921[1,]==23],1,sum))
LO23.Diem1921.1922 <- table(apply(Diem1921.1922[-1,Diem1921.1922[1,]==23],1,sum))
LO23.Diem1921.1923 <- table(apply(Diem1921.1923[-1,Diem1921.1923[1,]==23],1,sum))
LO23.Diem1921.1924 <- table(apply(Diem1921.1924[-1,Diem1921.1924[1,]==23],1,sum))
LO23.Diem1922.1921 <- table(apply(Diem1922.1921[-1,Diem1922.1921[1,]==23],1,sum))
LO23.Diem1922.1922 <- table(apply(Diem1922.1922[-1,Diem1922.1922[1,]==23],1,sum))
LO23.Diem1922.1923 <- table(apply(Diem1922.1923[-1,Diem1922.1923[1,]==23],1,sum))
LO23.Diem1922.1924 <- table(apply(Diem1922.1924[-1,Diem1922.1924[1,]==23],1,sum))
LO23.Diem1923.1921 <- table(apply(Diem1923.1921[-1,Diem1923.1921[1,]==23],1,sum))
LO23.Diem1923.1922 <- table(apply(Diem1923.1922[-1,Diem1923.1922[1,]==23],1,sum))
LO23.Diem1923.1923 <- table(apply(Diem1923.1923[-1,Diem1923.1923[1,]==23],1,sum))
LO23.Diem1923.1924 <- table(apply(Diem1923.1924[-1,Diem1923.1924[1,]==23],1,sum))
LO23.Diem1924.1921 <- table(apply(Diem1924.1921[-1,Diem1924.1921[1,]==23],1,sum))
LO23.Diem1924.1922 <- table(apply(Diem1924.1922[-1,Diem1924.1922[1,]==23],1,sum))
LO23.Diem1924.1923 <- table(apply(Diem1924.1923[-1,Diem1924.1923[1,]==23],1,sum))
LO23.Diem1924.1924 <- table(apply(Diem1924.1924[-1,Diem1924.1924[1,]==23],1,sum))

#make count right answer -> wrong answer
rownames(LO23.Diem1921.1921) <- abs(as.numeric(rownames(LO23.Diem1921.1921)) - ncol(MD1921.1921[,MD1921.1921[1,]==23]))
rownames(LO23.Diem1921.1922) <- abs(as.numeric(rownames(LO23.Diem1921.1922)) - ncol(MD1921.1922[,MD1921.1922[1,]==23]))
rownames(LO23.Diem1921.1923) <- abs(as.numeric(rownames(LO23.Diem1921.1923)) - ncol(MD1921.1923[,MD1921.1923[1,]==23]))
rownames(LO23.Diem1921.1924) <- abs(as.numeric(rownames(LO23.Diem1921.1924)) - ncol(MD1921.1924[,MD1921.1924[1,]==23]))

#different is here
rownames(LO23.Diem1922.1921) <- abs(as.numeric(rownames(LO23.Diem1922.1921)) - ncol(MD1922.1921[,MD1922.1921[1,]==23]))
rownames(LO23.Diem1922.1922) <- abs(as.numeric(rownames(LO23.Diem1922.1922)) - ncol(MD1922.1922[,MD1922.1922[1,]==23]))
rownames(LO23.Diem1922.1923) <- abs(as.numeric(rownames(LO23.Diem1922.1923)) - ncol(MD1922.1923[,MD1922.1923[1,]==23]))
rownames(LO23.Diem1922.1924) <- abs(as.numeric(rownames(LO23.Diem1922.1924)) - ncol(MD1922.1924[,MD1922.1924[1,]==23]))


#-----------------

rownames(LO23.Diem1923.1921) <- abs(as.numeric(rownames(LO23.Diem1923.1921)) - ncol(MD1923.1921[,MD1923.1921[1,]==23]))
rownames(LO23.Diem1923.1922) <- abs(as.numeric(rownames(LO23.Diem1923.1922)) - ncol(MD1923.1922[,MD1923.1922[1,]==23]))
rownames(LO23.Diem1923.1923) <- abs(as.numeric(rownames(LO23.Diem1923.1923)) - ncol(MD1923.1923[,MD1923.1923[1,]==23]))
rownames(LO23.Diem1923.1924) <- abs(as.numeric(rownames(LO23.Diem1923.1924)) - ncol(MD1923.1924[,MD1923.1924[1,]==23]))
rownames(LO23.Diem1924.1921) <- abs(as.numeric(rownames(LO23.Diem1924.1921)) - ncol(MD1924.1921[,MD1924.1921[1,]==23]))
rownames(LO23.Diem1924.1922) <- abs(as.numeric(rownames(LO23.Diem1924.1922)) - ncol(MD1924.1922[,MD1924.1922[1,]==23]))
rownames(LO23.Diem1924.1923) <- abs(as.numeric(rownames(LO23.Diem1924.1923)) - ncol(MD1924.1923[,MD1924.1923[1,]==23]))
rownames(LO23.Diem1924.1924) <- abs(as.numeric(rownames(LO23.Diem1924.1924)) - ncol(MD1924.1924[,MD1924.1924[1,]==23]))



LOW23 <- as.data.frame(bind_rows(LO23.Diem1921.1921,
                                 LO23.Diem1921.1922,
                                 LO23.Diem1921.1923,
                                 LO23.Diem1921.1924,
                                 LO23.Diem1922.1921,
                                 LO23.Diem1922.1922,
                                 LO23.Diem1922.1923,
                                 LO23.Diem1922.1924,
                                 LO23.Diem1923.1921,
                                 LO23.Diem1923.1922,
                                 LO23.Diem1923.1923,
                                 LO23.Diem1923.1924,
                                 LO23.Diem1924.1921,
                                 LO23.Diem1924.1922,
                                 LO23.Diem1924.1923,
                                 LO23.Diem1924.1924,
                                 LO23.last5.1922

))

LOW23[is.na(LOW23)] <- 0

LOW23 <- as.data.frame(apply(LOW23,2,sum))
LOW23$SoCauTLSai <- as.numeric(row.names(LOW23))

LOW23$percent <- round(LOW23[,1]/sum(LOW23[,1]),3)
#graph----
GLOW23 <- ggplot(LOW23, aes(x = as.character(LOW23[,2]), y = LOW23[,3])) + geom_bar(stat = "identity") + geom_text(aes(label=LOW23[,3]), vjust=-0.3, size=3.5, )+
  labs(title = "Learning Outcome 23", x = "So cau tra loi sai", y = "tan suat tuong duong") + theme(panel.background = element_rect(fill = "#d557d5"))

GW <- ggarrange(GLOW.GK11, GLOW12, GLOW.GK21, GLOW.GK22, GLOW23, GLOW.CK31, GLOW.CK32, ncol=3, nrow = 3)
GW