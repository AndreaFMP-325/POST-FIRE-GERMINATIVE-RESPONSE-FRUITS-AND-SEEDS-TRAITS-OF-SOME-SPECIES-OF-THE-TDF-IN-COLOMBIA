####Cargar datos

rasgos=read.csv("Rasgos2.1.csv", row.names = 1,sep=";")

str(rasgos)

library(labdsv)
library(vegan)


####Componentes principales

rasgos2=rasgos[, 2:11]

rasgos$RG=as.factor(rasgos$RG)

str(rasgos)

###Correlaciones###

cor(rasgos[,2:11])

####Ponerlas en la misma escala###
bosque.std=decostand(rasgos2, "standardize")
bosque.pca2=rda(bosque.std,scaling=2)

###Resumen de valores usados en el PCA

summary(bosque.pca2)

###Grafica de componentes principales c("#424242", "#6B6B6B", "#737373")
windows()
desc.sc2=scores(bosque.pca2, display="sp")
plot(bosque.pca2, display=c("sites"), type="n",xlim=c(-2,2),ylim = c(-1.5,1.5),lty=1)
text(bosque.pca2, display="sp", col="black", cex=1)
arrows(0, 0, desc.sc2[,1]*0.8, desc.sc2[,2]*0.8, length=0, col="black")
ordiellipse(bosque.pca2,rasgos$RG,label=T,cex=0.8,draw="polygon",lty=3,col="blue", alpha = 50,kind = c("sd"))
points(bosque.pca2,pch=17,col="red",cex=0.8)

with(rasgos, ordiellipse(bosque.pca2, rasgos$RG,cex=0.6,col="red", show.groups = "Estimulada ",
                         draw = "polygon",lty=1))

with(rasgos, ordiellipse(bosque.pca2, rasgos$RG,cex=0.6,col="blue", show.groups = "Tolerante ",
                         draw = "polygon",lty=1))

with(rasgos, ordiellipse(bosque.pca2, rasgos$RG,cex=0.6,col="yellow", show.groups = "Sensible ",
                         draw = "polygon"))

colvec=c("red", "blue", "yellow")

legend("bottomright", legend=c("Estimulada", "Tolerante","Sensible"), col = colvec
       ,pt.bg = colvec,pch=16,cex=0.8,box.lty = 0)

colvec=c("purple", "blue", "yellow")

colvec[grp]

points(bosque.pca2,pch=17,cex=0.8,display = "sp")

####Otra opcion

library(FactoMineR)
library(factoextra)

pca2 <- PCA(rasgos[,3:11], scale.unit = TRUE, ncp = 30, graph = F)
print(pca2)
get_eigenvalue(pca2)
View(pca2[["var"]][["cor"]])
View(pca2[["var"]][["contrib"]])
View(pca2[["var"]][["coord"]])
View(pca2[["var"]][["cos2"]])
fviz_eig(pca2, addlabels = T)
corrplot(pca2$var$cor, is.corr=FALSE)
fviz_cos2(pca2, choice = "var", axes = 1:2)

fviz_pca_ind(pca2, label="none")

p <- fviz_pca_ind(pca2, label="none", habillage=rasgos$RG,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)


#####Otra opcion 2 final----

library(ggbiplot)

pca <- prcomp(rasgos2, center = TRUE,scale. = TRUE)

summary(pca)
ggbiplot(pca)


ggbiplot(pca,ellipse=TRUE,  labels=rownames(rasgos),obs.scale = 1,
         var.scale = 1, groups=rasgos$RG)+theme_minimal()


windows()
ggbiplot(pca,ellipse=F,obs.scale = 1, var.scale = 1, groups=rasgos$RG) +
  scale_colour_manual(name="RG", values= c("forest green", "red3", "dark blue","pink","purple","brown"))+
  theme_minimal()+
  theme(legend.position = "bottom")

windows()
ggbiplot(pca,ellipse=TRUE,obs.scale = 1, var.scale = 1, groups=rasgos$RG) +
  scale_colour_manual(name="RG", values= c("forest green", "red3", "dark blue","pink","purple","brown"))+
  theme_minimal()+
  theme(legend.position = "bottom")


####modelo para seleccion de variables

rasgos=read.csv("Rasgos3.csv", row.names = 1,sep=";")

modelo1 <- glm(RG~TIF+LGF+WF+MED+ALM+DOR+MS+LGS+WS+GE, data = rasgos)

step(modelo,direction ="both")

####seleccion de varaibles
library(dplyr)
continuas=select(rasgos,GE,TIF,ALM,WF,MS)

pca <- prcomp(continuas, center = TRUE,scale. = TRUE)

cor(continuas)

summary(pca)
ggbiplot(pca,ellipse=TRUE,groups=rasgos$RG)
windows()
ggbiplot(pca,ellipse=TRUE,obs.scale = 1, var.scale = 1, groups=rasgos$RG) +
  scale_colour_manual(name="RG", values= c("forest green", "red3", "dark blue","pink","purple","brown"))+
  theme_minimal()+
  theme(legend.position = "bottom")

#####Semillas

continuas2=select(rasgos,DOR,ALM,MED,LGS,WS,MS)

pca <- prcomp(continuas2, center = TRUE,scale. = TRUE)

cor(continuas2)

summary(pca)
ggbiplot(pca,ellipse=TRUE,groups=rasgos$RG)
windows()

rasgos$RG=as.factor(rasgos$RG)
ggbiplot(pca,ellipse=TRUE,obs.scale = 1, var.scale = 1, groups=rasgos$RG) +
  scale_colour_manual(name="RG", values= c("forest green", "red3", "dark blue","pink","purple","brown"))+
  theme_minimal()+
  theme(legend.position = "bottom")


####Arbol de dicision

library(rpart)
library(rpart.plot)

rasgos=read.csv("Rasgos.csv", row.names = 1,sep=";")

rasgos$RG=as.factor(rasgos$RG)
rasgos$TIF=as.factor(ra1sgos$TIF)
rasgos$MED=as.factor(rasgos$MED)
rasgos$ALM=as.factor(rasgos$ALM)
rasgos$DOR=as.factor(rasgos$DOR)
rasgos$GE=as.factor(rasgos$GE)



library(party)


output.tree <- ctree(RG~ TIF+LGF+ WF+MED+ALM+DOR+MS+LGS+WS+GE, 
                     data = rasgos)

plot(output.tree)

###ejemplo titanic

input.dat <- clean_titanic[c(1:80),]

output.tree <- ctree(survived~ sex + age, 
                     data = input.dat)

plot(output.tree)

####opcion 2

####Limpiar la base

library(dplyr)

library(rpart.plot)

{create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample = 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}}


create_train_test(rasgos, size = 0.8, train = TRUE)

data_train <- create_train_test(rasgos, 0.8, train = TRUE)
data_test <- create_train_test(rasgos, 0.8, train = FALSE)
dim(data_train)

prop.table(table(data_train$RG))

prop.table(table(data_test$RG))


fit <- rpart(RG~.,data = rasgos, method = 'class')

windows()
rpart.plot(fit)

plot(fit)

######Otra opcion

library(caret)

index = createDataPartition(y=rasgos$RG, p=0.7, list=FALSE)

train.set = rasgos[index,]
test.set = rasgos[-index,]

iris.tree = train(RG~ ., 
                  data=train.set, 
                  method="rpart", 
                  trControl = trainControl(method = "cv"))

iris.tree

summary(iris.tree$finalModel)

plot(iris.tree$finalModel, uniform=TRUE,
     main="Classification Tree")
text(iris.tree$finalModel, use.n.=TRUE, all=TRUE, cex=.8)

####Analisis correspondencia multiple

matriz=read.csv("rasgos.csv",sep=";")

matriz$RG=as.factor(matriz$RG)

library("FactoMineR")
library("factoextra")

library("FactoMineR")

attach(matriz)

y=select(matriz,TIF,MED,ALM,DOR,GE)

library(FactoMineR)

res.famd <- FAMD(y, graph = FALSE)

print(res.famd)

library("factoextra")
eig.val <- get_eigenvalue(res.famd)
head(eig.val)

res.mca <- MCA(y, graph=FALSE)

fviz_mca_ind(res.mca, col.ind = "steelblue")

grp <- as.factor(matriz[, "RG"])
p <- fviz_mca_ind(res.mca, label="T", habillage=grp,
                  addEllipses=TRUE, ellipse.level=0.95,repel = TRUE)
print(p)


windows()
grp <- as.factor(matriz[, "RG"])
a=fviz_mca_biplot(res.mca, repel = T, col.var = "black",
                  habillage = grp, addEllipses = T, ellipse.level = 0.95)
a+theme_classic()


fviz_mca_biplot(res.mca, repel = TRUE)

fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


windows()
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())


####modelo generalizado de regresion logistica multimodal----

rasgos=read.csv("Frutos_total1603.csv",sep=";")

na.omit(rasgos)

rasgos$RG=as.factor(rasgos$RG)
rasgos$TIF=as.factor(rasgos$TIF)
rasgos$MED=as.factor(rasgos$MED)
rasgos$ALM=as.factor(rasgos$ALM)
rasgos$DOR=as.factor(rasgos$DOR)
rasgos$GE=as.factor(rasgos$GE)

library(car)
vif(test)


library(MASS)

library(nnet)

attach(rasgos)

####variable de referencia
rasgos$RG <- relevel(rasgos$RG, ref = "Tolerante")

names(rasgos)

###Regresion 
names(bosque)

test <- multinom(RG~WF+LGF+TIF+MED+ALM+DOR+GE+MS+LGS+WS
                 , data = rasgos)

summary(test)



# Modelo nuevo ------------------------------------------------------------

test2 <- multinom(RG~LGF+TIF+ALM+DOR+GE+WS+MED
                 , data = rasgos)

###Parametros de regresion 

summary(test2)

library(car)

###Anova para variables

Anova(test)

z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


### odds ratio
exp(coef(test))

head(pp <- fitted(test))

library(car)
Anova(test, type="II")  

###prediccion

library(stargazer)

stargazer(test2, type="text", out="multi2.html")

allmean <- data.frame(LGF=c(0.82,0.98,0.64),
                      WF=c(0.52,0.82,0.16),
                      TIF=c("Nuez","Baya","Legumbre"),
                      MED=c("Anemocoria","Zoocoria","Anemocoria"),
                      ALM=c("Ortodoxa","Recalcitrante","Ortodoxa"),
                      DOR=c("Dormante", "Dormante", "Dormante"),
                      GE=c("Heliofita","Heliofita","Heliofita"),
                      MS=c(0.94,1,0.5),
                      LGS=c(0.7,1.15,0.6),
                      WS=c(0.85,0.8,0.1))

allmean[, c("pred.prob")] <- predict(test, newdata=allmean, type="probs")

allmean[, c("pred.prob")] <- predict(test, newdata=allmean, type="class")

allmean

####pseudo R2

library("DescTools")

PseudoR2(test2, which = c("CoxSnell","Nagelkerke","McFadden"))


####modelo generalizado

rasgos=read.csv("rasgos3.csv",sep=";")

b=glm(RG~WF+LGF+TIF+MED+ALM+DOR+GE+MS+LGS+WS,gaussian	(	link = "identity"), 
      data=rasgos)

summary(b)

Anova(b)

#####cordenadas polares

library(ecodist)
library(vegan)
library(ape)

rasgos=read.csv("rasgos2.csv",sep=";")

rasgos2=rasgos[, 3:11]

mite.D <- vegdist(rasgos, "euclidean")
res <- pcoa(mite.D)
res$values
biplot(res)

rasgos$RG=as.factor(rasgos$RG)

biplot(res, plot.axes = c(1, 2))


####otra opcion

rasgos2=rasgos[, 3:11]

library(vegan)

especies.pa=decostand(rasgos2,"log")

especies.nms2d=metaMDS(especies.pa,"euclidean") 

plot(especies.nms2d, display="sites", type="t")

###usada para ver en que lugar se unen los genes 

###grafica mejorada

windows()
plot(especies.nms2d, type="n", xlim=c(-0.5,0.5))

text(especies.nms2d$species, labels=row.names(especies.nms2d$species), cex=0.7)


ordiellipse(especies.nms2d,rasgos$RG, scaling = "symmetric", col = "purple",
            draw="polygon", show.groups = "Estimulada ")

ordiellipse(especies.nms2d,rasgos$RG, scaling = "symmetric", col = "blue",
            draw="polygon", show.groups = "Tolerante ")

ordiellipse(especies.nms2d,rasgos$RG, scaling = "symmetric", col = "yellow",
            draw="polygon", show.groups = "Sensible ")



# Coordenadas principales -------------------------------------------------

rasgos=read.csv("Rasgos3.csv", row.names = 1,sep=";")

library(FD)
library(ape)


D <- gowdis(rasgos)
pcoa <- pcoa(D)

windows()
biplot(x = pcoa, Y = log(rasgos[, -1] + 1), plot.axes = c(1, 2))

loadings <- cor(rasgos[, -1], pcoa$vectors, method = "pearson",
                use = "complete.obs")
loadings

pcoa.all <- dudi.pco(sqrt(D), scannf = F, nf = 4)
scatter(pcoa.all)


windows()
a=scatter(dudi.pca(rasgos, scannf = F, nf = 4),posieig = "bottomright")


res.pca <- dudi.pca(rasgos[2:11],
                    scannf = FALSE,   # Hide scree plot
                    nf = 5            # Number of components kept in the results
)

rasgos$RG=as.factor(rasgos$RG)

res <- scatter(res.pca, clab.row = 0, posieig = "none")
s.class(res.pca$li, 
        fac = rasgos$RG,
        col = c("#00AFBB",  "#FC4E07"),
        add.plot = TRUE,         # Add onto the scatter plot
        cstar = 0,               # Remove stars
        cellipse = 0             # Remove ellipses
)




fviz_pca_ind(res.pca,
             col.ind = rasgos$RG, # color by groups
             palette = c("#00AFBB",  "#FC4E07","blue"),
             addEllipses = T, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Respuesta germinativa",
             repel = TRUE,
)



# opcion 4 ----------------------------------------------------------------

# Data for the supplementary individuals


windows()
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "black", # Variables color  # Individuals color
                col.ind = rasgos$RG,
                palette = c("#00AFBB",  "#FC4E07","blue"),
                legend.title = "Respuesta germinativa",
                title = "PCoA",
)


####http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/119-pca-in-r-using-ade4-quick-scripts/#compute-pca-using-dudi.pca