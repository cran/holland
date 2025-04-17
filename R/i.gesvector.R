### Gesamtvector nach Eder (1998) vgl. Anhang Seite 77--------------------------
# Eder, F. (1998). Differenziertheit der Interessen als Prädiktor der Interessenentwicklung. In J. Abel & C. Tarnai (Hrsg.), Pädagogisch-psychologische Interessenforschung in Studium und Beruf (S. 63–77). Münster: Waxmann.
# func. by: jhheine@googlemail.com 
# 'x' ist ein profil
# cos(rad(30)) = 0.8660254; sin(rad(30)) = 0.5

i.gesvector <- function(x){
  Koord1<-(c(-0.5,0.5,1,0.5,-0.5,-1)*x)
  Koord2<-(c(0.866,0.866,0,-0.866,-0.866,0)*x)
  ergdiff<-sqrt( ( (sum(Koord1,na.rm=T))^2 ) + ( (sum(Koord2,na.rm=T)^2)  )  ) # Differenziertheit
  # mean(x); range(x); max(x)-min(x)
  if( ergdiff==0) {return(c(NA,NA));stop}
  winkhv <- (asin((sum(Koord2,na.rm=T)/ergdiff))*360)/(2*3.141592654)# COMMENT Berechnung des Vektorwinkels.
  if((sum(Koord1,na.rm=T))==0 & (sum(Koord2,na.rm=T))>0) {erg<-0}
  if((sum(Koord1,na.rm=T))>0 & (sum(Koord2,na.rm=T))==0) {erg<-90}
  if((sum(Koord1,na.rm=T))==0 & (sum(Koord2,na.rm=T))<0) {erg<-180}
  if((sum(Koord1,na.rm=T))<0 & (sum(Koord2,na.rm=T))==0) {erg<-270}
  if((sum(Koord1,na.rm=T))>0 & (sum(Koord2,na.rm=T))>0) {erg<-90-winkhv}
  if((sum(Koord1,na.rm=T))>0 & (sum(Koord2,na.rm=T))<0) {erg<-90-winkhv}
  if((sum(Koord1,na.rm=T))<0 & (sum(Koord2,na.rm=T))<0) {erg<-270+winkhv}
  if((sum(Koord1,na.rm=T))<0 & (sum(Koord2,na.rm=T))>0) {erg<-270+winkhv}
  
  # erg
  
  return(erg)
}

