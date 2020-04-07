data_event <- read.csv("C:/Users/claey/Documents/Cours/cour ESIEA/5A 2017-2018 DTE/Analyse exploratoire/TD3/data/football-events/football-events/events_France.csv")
print(head(data_event))

list_col <- c("sort_order","time","season","fthg","ftag","odd_h","odd_d","odd_a")
list_col <- c("event_team","sort_order","time","fthg","odd_h")
#fthg full time home goals Numeric
#ftag full time away goals Numeric
#odd_h highest home win market odds Numeric
#odd_d highest draw market odds Numeric
#odd_a highest away market odds Numeric

data_event.x <- data_event[,list_col]

#Consolidate
data_event.c <- aggregate(.~event_team,data=data_event.x,FUN=mean)
rownames(data_event.c ) <- data_event.c$event_team

#We don't need event_team anymore
data_event.c$event_team <- NULL

pairs(data_event.c)

#Scale reduction of data
data_event.cr <- scale(data_event.c)


d.data_event <- dist(data_event.c)
cah.ward <- hclust(d.data_event,method="ward.D2")

par(cex=0.5)
plot(cah.ward, xlab="", ylab="", main="", sub="", axes=FALSE)
title(xlab="event_team", ylab="ylab", main="main")




groupes.cah <- cutree(cah.ward,k=6)
#dendrogramme avec matérialisation des groupes
rect.hclust(cah.ward,k=6)

library(ape)
# plot basic tree
#par(cex=0.5, mar=c(5, 8, 4, 1))
plot(as.phylo(cah.ward), cex = 0.9, label.offset = 1)



groupes.kmeans <- kmeans(data_event.c,centers=6,nstart=5)

#affichage des résultats
print(groupes.kmeans)

#correspondance avec les groupes de la CAH
print(table(groupes.cah,groupes.kmeans$cluster))

#(1)évaluer la proportion d'inertie expliquée
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(data_event.c,centers=k,nstart=5)  
  inertie.expl[k] <- clus$betweenss/clus$totss
}

par(cex=1)
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquee")

#Largeur moyenne de silhouette
#utilisation du package fpc 
library(fpc)
#évaluation des solutions
sol.kmeans <- kmeansruns(data_event.c,krange=2:10,criterion="ch")
#graphique
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")



#fonction de calcul des stats
stat.comp <- function(x,y){
  #nombre de groupes
  K <- length(unique(y))
  #nb. d'observations
  n <- length(x)
  #moyenne globale
  m <- mean(x)
  #variabilité totale
  TSS <- sum((x-m)^2)
  #effectifs conditionnels
  nk <- table(y)
  #moyennes conditionnelles
  mk <- tapply(x,y,mean)
  #variabilité expliquée
  BSS <- sum(nk * (mk - m)^2)
  #moyennes + prop. variance expliquée
  result <- c(mk,100.0*BSS/TSS)
  #nommer les élements du vecteur
  names(result) <- c(paste("G",1:K),"% epl.")
  #renvoyer le résultat
  return(result)
}


#appliquer stat.comp aux variables de
#la base originelle royau.x
#et non pas aux variables centrées et réduites
print(sapply(data_event.c,stat.comp,y=groupes.cah))


###
#ACP
acp <- princomp(data_event.c,cor=T,scores=T)

#screeplot - 2 axes retenus
plot(1:4,acp$sdev^2,type="b",xlab="Nb. de facteurs",ylab="Val. Propres")

#biplot
biplot(acp,cex=0.65)

#positionnement des groupes dans le plan factoriel avec etiquettes des points
plot(acp$scores[,1],acp$scores[,2],type="n")
text(acp$scores[,1],acp$scores[,2],col=c("red","green","blue","black","pink","darkgoldenrod")[groupes.cah],cex=0.65,labels=rownames(data_event.c))

