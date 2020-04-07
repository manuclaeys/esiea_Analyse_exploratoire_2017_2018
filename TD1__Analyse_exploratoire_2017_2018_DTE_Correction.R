data_macdo <- read.csv("C:/Users/claey/Documents/Cours/cour ESIEA/5A 2017-2018 DTE/Analyse exploratoire/TD1/data/nutrition-facts-for-mcdonald-s-menu/menu.csv",sep=',')
data_macdo <- as.data.frame(data_macdo)
summary(data_macdo)


###rm neggets
data_macdo <- data_macdo[-(83),]

######Test 
chisq.test(data_macdo$Calories, data_macdo$Total.Fat) 


list <- c("Calories", "Total.Fat", "Cholesterol","Sodium","Sugars","Protein")
###need to test normality before###
lshap <- lapply(data_macdo[,list], shapiro.test)

#no exponential notation 
options("scipen"=100, "digits"=4)

  for(i in 1:length(lshap)) print(lshap[i])



round(cor(data_macdo[, list]),2)


## Representation 3D : Calories, Total.Fat, Cholesterol
library(rgl)
plot3d(data_macdo$Calories, data_macdo$Total.Fat,data_macdo$Cholesterol,type="s")

##### Centrage Réduction
list <- c("Calories", "Total.Fat", "Cholesterol")
data_macdo.cr <- scale(data_macdo[, list])
lims <- c(min(data_macdo.cr),max(data_macdo.cr))
plot3d(data_macdo.cr, type = "s", xlim = lims, ylim = lims,zlim = lims)


##### Corrélation
data_macdo.cr_df <- as.data.frame(data_macdo.cr)
plot3d(data_macdo.cr, type = "s", xlim = lims, ylim = lims,zlim = lims)
plot3d(ellipse3d(cor(cbind(data_macdo.cr_df$Calories, data_macdo.cr_df$Total.Fat,data_macdo.cr_df$Cholesterol))),
       col="grey",add=TRUE)


library(ade4)
list <- c("Calories", "Total.Fat", "Cholesterol")
acp <- dudi.pca(data_macdo[, list], center=TRUE, scale=TRUE, scannf = FALSE, nf = 3)
names(acp)
acp$cw
head(acp$lw)
head(acp$lw)*nrow(data_macdo)

pve <- 100*acp$eig/sum(acp$eig)
cumsum(pve)


inertie <-inertia.dudi(acp, col.inertia=TRUE)
inertie
# Coordonnees des attributs
round(acp$co,2)


s.corcircle(acp$co,xax=1,yax=2)

s.label(acp$li, xax = 1, yax = 2)

s.label(acp$li, xax = 1, yax = 2, label=as.character(data_macdo$Item))

gcol <- c("red1", "red4","orange")
s.class(dfxy = acp$li, fac = data_macdo$Category, col = gcol, xax = 1, yax = 2)


########

list <- c("Calories", "Total.Fat", "Cholesterol","Sodium","Sugars","Protein")
acp <- dudi.pca(data_macdo[, list], center=TRUE, scale=TRUE, scannf = FALSE, nf = 3)
names(acp)
acp$cw
head(acp$lw)
head(acp$lw)*nrow(data_macdo)

pve <- 100*acp$eig/sum(acp$eig)
cumsum(pve)


inertie <-inertia.dudi(acp, col.inertia=TRUE)
inertie


# Coordonnees des attributs
round(acp$co,2)


s.corcircle(acp$co,xax=1,yax=2)

s.label(acp$li, xax = 1, yax = 2)

s.label(acp$li, xax = 1, yax = 2, label=as.character(data_macdo$Item))

gcol <- c("red1", "red4","orange")
s.class(dfxy = acp$li, fac = data_macdo$Category, col = gcol, xax = 1, yax = 2)

