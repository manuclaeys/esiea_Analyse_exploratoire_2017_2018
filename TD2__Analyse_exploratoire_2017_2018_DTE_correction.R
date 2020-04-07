data_event <- read.csv("C:/Users/claey/Documents/Cours/cour ESIEA/5A 2017-2018 DTE/Analyse exploratoire/TD2/data/football-events/football-events/events_France.csv")

library(ade4)
library(adegraphics)
data_event  <- as.data.frame(data_event)

data_event$event_type <- as.factor(data_event$event_type)

list_col <- c("event_team","opponent","event_type")
data_event.x <- data_event[,list_col ]

summary(data_event.x)

acmtot <- dudi.acm(data_event.x,scannf=FALSE)
barplot(acmtot$eig)
score(acmtot,xax=1)
head(inertia.dudi(acmtot)$TOT)

scatter(acmtot)

#######partie 2

library(PCAmixdata)


pcamix.temp<- PCAmix(X.quanti = subset(data_event,select=c(5,33)) ,X.quali= subset(data_event,select=c(7,10,11)), rename.level=TRUE)

print(round(pcamix.temp$eig))

 #correlations
print(round(pcamix.temp$quanti.cor))

#coord. des modalites dudi.mix de ADE4
print(round(pcamix.temp$categ.coord))
print(round(pcamix.temp$categ.coord))

