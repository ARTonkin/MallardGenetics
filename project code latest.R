
setwd("D:/Uni/780")

library("openxlsx")

genass = read.xlsx("Genetics_addiitonal data.xlsx", sheet = 1, startRow = 2)

gendat = read.xlsx("Genetics_addiitonal data.xlsx", sheet = 2, startRow = 2)

nestdat = read.xlsx("Genetics_addiitonal data.xlsx", sheet = 3, startRow = 2)

brooddat = read.xlsx("Genetics_addiitonal data.xlsx", sheet = 4, startRow = 2)


##################################
## Table of proportion band ids ##
##################################

# Making a table which shows which ids from a dataset can be found in the other datasets.
# This gives me a better idea as to how the data can be combined and transformed.

# \ 1  b1 c1 d1 \
# \ a1 1  c2 d2 \
# \ a2 b2 1  d3 \
# \ a3 b3 c3 1  \

a1 = round(sum(genass$SAMPLE.ID %in% gendat$Band) / nrow(genass), digits = 3)

a2 = round(sum(genass$SAMPLE.ID %in% nestdat$Band) / nrow(genass), digits = 3)

a3 = round(sum(genass$SAMPLE.ID %in% brooddat$Band) / nrow(genass), digits = 3)


b1 = round(sum(gendat$Band %in% genass$SAMPLE.ID) / nrow(gendat), digits = 3)

b2 = round(sum(gendat$Band %in% nestdat$Band) / nrow(gendat), digits = 3)

b3 = round(sum(gendat$Band %in% brooddat$Band) / nrow(gendat), digits = 3)


c1 = round(sum(nestdat$Band %in% genass$SAMPLE.ID) / nrow(nestdat), digits = 3)

c2 = round(sum(nestdat$Band %in% gendat$Band) / nrow(nestdat), digits = 3)

c3 = round(sum(nestdat$Band %in% brooddat$Band) / nrow(nestdat), digits = 3)


d1 = round(sum(brooddat$Band %in% genass$SAMPLE.ID) / nrow(brooddat), digits = 3)

d2 = round(sum(brooddat$Band %in% gendat$Band) / nrow(brooddat), digits = 3)

d3 = round(sum(brooddat$Band %in% nestdat$Band) / nrow(brooddat), digits = 3)


combining = matrix(c(1, a1, a2, a3,
                     b1, 1, b2, b3,
                     c1, c2, 1, c3,
                     d1, d2, d3, 1), nrow = 4, byrow = F)

combining
# You say: "67.5% of the sample ID's (column) in the GeneticAssignment dataset are in the GeneticData_2017 Band numbers (row)."
# 1: GeneticAssignment dataset
# 2: GeneticData_2017 dataset
# 3: NestData dataset
# 4: BroodData dataset



##################
## Merging Data ##
##################

# Merging the data into one dataset. Also simplifying the genetic data as suggested by the client.

genass$gen.simple = rep(NA, nrow(genass))

for(i in 1:nrow(genass)){
    if (genass$Genetic.Assignment[i] %in% c("PURE.MALL")) genass$gen.simple[i] = "MALL"
    if (genass$Genetic.Assignment[i] %in% c("PURE.GRDU")) genass$gen.simple[i] = "GREY"
    if (genass$Genetic.Assignment[i] %in% c("F1", "F2-GRDU", "F2-MALL", "F3-GRDU", "F3-MALL", 
                                            "F4-GRDU", "F4-MALL", "NZGR.BACKCROSS_SWARM", "MALL.BACKCROSS_SWARM")) 
        genass$gen.simple[i] = "HYBRID"
}    


full1 = merge(genass, gendat, by.x="SAMPLE.ID", by.y="Band", all.x=TRUE, all.y=FALSE)

full2 = merge(full1, nestdat, by.x="SAMPLE.ID", by.y="Band", all.x=TRUE, all.y=FALSE)

full = merge(full2, brooddat, by.x="SAMPLE.ID", by.y="Band", all.x=TRUE, all.y=FALSE)

rm(full1, full2, genass, gendat, nestdat, brooddat)

for(i in 1:nrow(full)){
  if (is.na(full$Study.Collected[i])) full$Study.Collected[i] = "other"
}

for(i in 1:nrow(full)){
  if (is.na(full$Sex[i])) full$Sex[i] = "F"
}



##################################
## Checking samples are similar ##
##################################

checkdat = full[!is.na(full$Species),]

table(full$Study.Collected, full$gen.simple)

prop.test(table(full$Study.Collected, full$gen.simple)[,2:3], correct=FALSE) 

# This proportion test yeilds no evidence that the proportions between the two Collection methods
# are different.



######################
## Staff Assignment ##
######################

# Creating tables to investigate how successful staff were at assigning duck species.

checkdat = full[!is.na(full$Species),]

table(checkdat$Species)
table(checkdat$Genetic.Assignment)
table(checkdat$gen.simple)

checkdat$Species == checkdat$gen.simple

guesstable = table(checkdat$Species, checkdat$gen.simple)
guesstable

mcheckdat = checkdat[checkdat$Sex == "M",]
fcheckdat = checkdat[checkdat$Sex == "F",]

guesstablem = table(mcheckdat$Species, mcheckdat$gen.simple)
guesstablem
outtablem = rbind(guesstablem, colSums(guesstablem))
outtablem = cbind(outtablem, rowSums(outtablem))
outtablem

guesstablef = table(fcheckdat$Species, fcheckdat$gen.simple)
guesstablef
outtablef = rbind(guesstablef, colSums(guesstablef))
outtablef = cbind(outtablef, rowSums(outtablef))
outtablef


correcttable = rbind(Correct=c(1,2,3), Incorrect=c(2,3,4))
colnames(correcttable) = c("Grey", "Hybrid", "Mallard")
correcttable

100*(round(guesstable / rep(colSums(guesstable), each=3), digits=3))

table(checkdat$Species, checkdat$Genetic.Assignment)


# Alluvial graph

library(ggplot2)
library(ggalluvial)

combos = data.frame(guess=rep(c("GREY", "HYBRID", "MALLARD"), 3), 
                    genetics=rep(c("GREY", "HYBRID", "MALLARD"), each=3), 
                    counts=c(guesstable), correct=as.logical(c(1,0,0,0,1,0,0,0,1)))

combos2 = data.frame(guess=rep(c("GREY", "HYBRID", "MALLARD"), 3), 
                    genetics=rep(c("GREY", "HYBRID", "MALLARD"), each=3), 
                    counts=rep(0,9), correct=as.logical(c(0,1,1,1,0,1,1,1,0)))

fullcombo = rbind(combos, combos2)

ggplot((fullcombo),
       aes(y = counts, axis1 = guess, axis2 = genetics)) +
    geom_alluvium(aes(fill = correct), width = 1/12) +
    geom_stratum(width = 1/12, fill = "black", color = "white") +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("Guessed Species", "Genetic Species"), expand = c(.05, .05)) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    ggtitle("Genetic classification of ducks, by guess and blood sample")


# 2nd Alluvial - gender breakdown

sexcombsm = data.frame(guess=rep(c("GREY", "HYBRID", "MALLARD"), 3), 
                      genetics=rep(c("GREY", "HYBRID", "MALLARD"), each=3), 
                      counts=c(guesstablem), sex=rep("M", 9))

sexcombsf = data.frame(guess=rep(c("GREY", "HYBRID", "MALLARD"), 3), 
                      genetics=rep(c("GREY", "HYBRID", "MALLARD"), each=3), 
                      counts=c(guesstablef), sex=rep("F", 9))

fullcombs = rbind(sexcombsm, sexcombsf)

ggplot((fullcombs),
       aes(y = counts, axis1 = genetics, axis2 = guess)) +
  geom_alluvium(aes(fill = sex), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "white") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Genetic Species", "Guessed Species"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Genetic classification of ducks, by guess and blood sample")


###########################
## Multivariate Approach ##
###########################

# Investigation into whether multivariate techniques can split the genetic groups.

## Making the PCA data

pcadat = full[,c("gen.simple", "Total.Mass.x", "Right.Wing.x", "Tarsus.x", "Keel.x", "Head.x", "Culmen.x", "Sex")]

pcadatnew = pcadat[!is.na(pcadat$Total.Mass.x),2:7]

pcadat1 = pcadatnew - rep(colMeans(pcadatnew), each=nrow(pcadatnew))

c = rep(0, ncol(pcadat1))

for (i in 1:ncol(pcadat1)){
    c[i] = sd(pcadat1[,i])
}

pcadat1 = pcadat1 / rep(c, each=nrow(pcadat1))


colours = rep(0, nrow(pcadat))

for (i in 1:length(colours)){
    if (pcadat$gen.simple[i] == "MALL") colours[i] = "green"
    if (pcadat$gen.simple[i] == "HYBRID") colours[i] = "royal blue"
    if (pcadat$gen.simple[i] == "GREY") colours[i] = "red"
}

symbols = rep(3, nrow(pcadat))

for (i in 1:length(colours)){
    if (pcadat$Sex[i] == "M") symbols[i] = 0
    if (pcadat$Sex[i] == "F") symbols[i] = 1
}

cols = colours[!is.na(pcadat$Total.Mass.x)]
syms = symbols[!is.na(pcadat$Total.Mass.x)]


## Checking data distributions

library("ggpubr")

ggdensity(pcadat1$Total.Mass.x)
ggdensity(pcadat1$Right.Wing.x)
ggdensity(pcadat1$Tarsus.x)
ggdensity(pcadat1$Keel.x)
ggdensity(pcadat1$Head.x)
ggdensity(pcadat1$Culmen.x)


## Performing PCA

library(MASS)

pca1 = princomp(pcadat1)

summary(pca1)

((pca1$sdev)^2) >= (sum((pca1$sdev)^2)/ncol(pcadat1))

screeplot(pca1, type="lines")

print(pca1$loadings[,1:4], cutoff=0, digits=2)

eqscplot(pca1$scores[,1:2], type="p", xlab="PC1", ylab="PC2", col=cols, pch=syms)

eqscplot(pca1$scores[,c(1,3)], type="p", xlab="PC1", ylab="PC3", col=cols, pch=syms)

eqscplot(pca1$scores[,c(2,3)], type="p", xlab="PC2", ylab="PC3", col=cols, pch=syms)


pcameansmat = matrix(c(colMeans(pca1$scores[cols == "red",1:3]),
                       colMeans(pca1$scores[cols == "royal blue",1:3]),
                       colMeans(pca1$scores[cols == "green",1:3])), nrow = 3)

colnames(pcameansmat) = c("GREY", "HYBRID", "MALLARD")
rownames(pcameansmat) = c("PC1", "PC2", "PC3")
pcameansmat



## testing difference

manovadat = data.frame(scores=as.numeric(pca1$scores[,1]),cols,syms)

manfit = aov(scores~cols*syms, data=manovadat)
manfit


## trellis pca plot
# splitting up the pca graph into 6 plots, for each combination of species and gender.

par(mar=c(0,0,0,0), mfrow=c(3,2))

plot(pcadatnew$Total.Mass.x[(cols=="red"&syms==0)],pcadatnew$Head.x[(cols=="red"&syms==0)], 
     type="p", xlab="PC1", ylab="PC2", xaxt='n', yaxt ='n')
plot(pcadatnew$Total.Mass.x[(cols=="red"&syms==1)],pcadatnew$Head.x[(cols=="red"&syms==1)], 
     type="p", xlab="PC1", ylab="PC2", xaxt='n', yaxt ='n')

plot(pcadatnew$Total.Mass.x[(cols=="royal blue"&syms==0)],pcadatnew$Head.x[(cols=="royal blue"&syms==0)], 
     type="p", xlab="PC1", ylab="PC2", xaxt='n', yaxt ='n')
plot(pcadatnew$Total.Mass.x[(cols=="royal blue"&syms==1)],pcadatnew$Head.x[(cols=="royal blue"&syms==1)], 
     type="p", xlab="PC1", ylab="PC2", xaxt='n', yaxt ='n')

plot(pcadatnew$Total.Mass.x[(cols=="green"&syms==0)],pcadatnew$Head.x[(cols=="green"&syms==0)], 
     type="p", xlab="PC1", ylab="PC2", xaxt='n', yaxt ='n')
plot(pcadatnew$Total.Mass.x[(cols=="green"&syms==1)],pcadatnew$Head.x[(cols=="green"&syms==1)], 
     type="p", xlab="PC1", ylab="PC2", xaxt='n', yaxt ='n')


plot(pca1$scores[(cols=="red"&syms==0),1:2], 
     type="p", xlab="PC1", ylab="PC2", xlim = c(-4, 5), ylim = c(-5,5), xaxt='n', yaxt ='n')
plot(pca1$scores[(cols=="red"&syms==1),1:2], 
     type="p", xlab="PC1", ylab="PC2", xlim = c(-4, 5), ylim = c(-5,5), xaxt='n', yaxt ='n')

plot(pca1$scores[(cols=="royal blue"&syms==0),1:2], 
     type="p", xlab="PC1", ylab="PC2", xlim = c(-4, 5), ylim = c(-5,5), xaxt='n', yaxt ='n')
plot(pca1$scores[(cols=="royal blue"&syms==1),1:2], 
     type="p", xlab="PC1", ylab="PC2", xlim = c(-4, 5), ylim = c(-5,5), xaxt='n', yaxt ='n')

plot(pca1$scores[(cols=="green"&syms==0),1:2], 
     type="p", xlab="PC1", ylab="PC2", xlim = c(-4, 5), ylim = c(-5,5), xaxt='n', yaxt ='n')
plot(pca1$scores[(cols=="green"&syms==1),1:2], 
     type="p", xlab="PC1", ylab="PC2", xlim = c(-4, 5), ylim = c(-5,5), xaxt='n', yaxt ='n')


dev.off()


## Machine Learning Decision Tree

mldat = pcadat[!is.na(pcadat$Total.Mass.x),1:8]

colnames(mldat) = c("gen.simple", "Total Mass", "Right Wing", "Tarsus",
                    "Keel", "Head", "Culmen", "Sex")

mldat$Sex = as.factor(mldat$Sex)
mldat$gen.simple = as.factor(mldat$gen.simple)
mldatm = mldat[mldat$Sex=="M",]
mldatf = mldat[mldat$Sex=="F",]


table(mldat$gen.simple)

library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

mldatz = mldatm[sample(1:nrow(mldatm)),]
mldaty = mldatm[sample(1:nrow(mldatf)),]

nrow(mldatz)

train1 = mldatz[1:90,]
test1 = mldatz[91:111,]

train2 = mldaty[1:70,]
test2 = mldaty[71:86,]

control = rpart.control(xval = 10)

mlfit1 = rpart(gen.simple~., data=mldatm, method="class")
rpart.plot(mlfit1)

mlfit2 = rpart(gen.simple~., data=mldatf, method="class")
rpart.plot(mlfit2)

printcp(mlfit1)

predmlfit1 = predict(mlfit1, test1, type="class")
testtable1 = table(test1$gen.simple, predmlfit1)
testtable1

confusionMatrix(predmlfit1, as.factor(test1$gen.simple))


predmlfit2 = predict(mlfit2, test2, type="class")
testtable2 = table(test2$gen.simple, predmlfit2)
testtable2

confusionMatrix(predmlfit2, as.factor(test2$gen.simple))

cvfunc = function(data, tries = 100){
  n = nrow(data)
  acc = 0
  acc2 = 0
  for(i in 1:tries){
    samp = sample(1:n, size = 10)
    train = data[samp,]
    test = data[-samp,]
    
    ml = rpart(gen.simple~., data=train, method="class")
    pred = predict(ml, test, type="class")
    testtable = table(test$gen.simple, pred)
    z = confusionMatrix(pred, as.factor(test$gen.simple))
    
    acc = acc + z$overall[1]
    acc2 = acc2 + z$overall[1]^2
    #print(acc)
  }
  return(c(acc / tries, (acc2/tries)-(acc / tries)^2))
}

cvfunc(mldatm, tries=10000)
cvfunc(mldatf, tries=10000)



## Random Forest

library(randomForest)
library(rfUtilities)

mldat$Sex = as.factor(mldat$Sex)
mldat$gen.simple = as.factor(mldat$gen.simple)
mldatm = mldat[mldat$Sex=="M",]
mldatf = mldat[mldat$Sex=="F",]

rf1 = randomForest(as.factor(gen.simple)~., data=mldatm, importance=T)
rf1

rf2 = randomForest(as.factor(gen.simple)~., data=mldat, ntree=500, mtry=7, importance=T)
rf2

plot(importance(rf2))

rfcv = rf.crossValidation(rf2, mldat, seed=0309)
rfcv


library(corrplot)

corrplot(cor(mldat[,2:7]), order="hclust")


###########################
## Reproduction Measures ##
###########################

head(full[,c("BSURV", "NumDucklingFledge", "INITAL.BroodSize")])

reprofull = full[full$Study.Collected=="other",c("BSURV", "NumDucklingFledge", "INITAL.BroodSize", "gen.simple")]

for (i in 1:nrow(reprofull)){
  for (j in 1:ncol(reprofull)){
    if (is.na(reprofull[i,j])) reprofull[i,j] = 0
  }
}

reprodat = full[!is.na(full$BSURV),c("BSURV", "NumDucklingFledge", "INITAL.BroodSize", "gen.simple")]



boxplot(NumDucklingFledge~gen.simple, data=reprodat)
boxplot(INITAL.BroodSize~gen.simple, data=reprodat)


library(ggplot2)
library(reshape2)

# Brood size data

a = melt(reprodat[,c("INITAL.BroodSize", "gen.simple")],
         id.vars = c("INITAL.BroodSize", "gen.simple"))

bardata = t(table(a))

barplot(bardata)

ggbardata = data.frame(counts=c(bardata[1,],bardata[2,]), brood_size=as.numeric(colnames(bardata)), 
                       genetics=c(rep("Hybrid",12),rep("Mallard",12)))

ggbardata$total_genetic_counts = c(rep(sum(ggbardata$counts[1:12]),12),rep(sum(ggbardata$counts[14:24]),12))

# Duckling fledge data

b = melt(reprodat[,c("NumDucklingFledge", "gen.simple")],
         id.vars = c("NumDucklingFledge", "gen.simple"))

bardatab = t(table(b))

barplot(bardatab)

ggbardatab = data.frame(counts=c(bardatab[1,],bardatab[2,]), ducklings_fledged=as.numeric(colnames(bardatab)), 
                       genetics=c(rep("Hybrid",8),rep("Mallard",8)))

ggbardatab$total_genetic_counts = c(rep(sum(ggbardata$counts[1:8]),8),rep(sum(ggbardata$counts[9:16]),8))

# Plotting

ggplot(ggbardata, aes(fill=genetics, y=counts, x=brood_size)) + 
  geom_bar(position = "dodge", stat="identity")

ggplot(ggbardata, aes(fill=genetics, y=counts / total_genetic_counts, x=brood_size)) + 
  geom_bar(position = "dodge", stat="identity") +
  xlab("Brood size") +
  ylab("Proportion of ducks")


ggplot(ggbardatab, aes(fill=genetics, y=counts, x=ducklings_fledged)) + 
  geom_bar(position = "dodge", stat="identity")

ggplot(ggbardatab, aes(fill=genetics, y=counts / total_genetic_counts, x=ducklings_fledged)) + 
  geom_bar(position = "dodge", stat="identity") +
  xlab("Number of ducklings fledged") +
  ylab("Proportion of ducks")



######################
## Decision surface ##
######################


which(pcadatnew$Head.x > 130)

which(syms == 3)

plotdat = pcadatnew[-c(12,178),]
colsplot = cols[-c(12,178)]
symsplot = syms[-c(12,178)]

plot(plotdat$Total.Mass.x, plotdat$Head.x, col=colsplot, pch=symsplot)


Species = pcadat$gen.simple[!is.na(pcadat$Total.Mass.x)]
Sex = pcadat$Sex[!is.na(pcadat$Total.Mass.x)]
Species = Species[-c(12,178)]
Sex = Sex[-c(12,178)]


library(ggplot2)

ggplot(plotdat, aes(x=Total.Mass.x, y=Head.x, shape=Sex, color=Species)) +
    geom_point()



plotsexm = plotdat[Sex == "M",]
plotsexf = plotdat[Sex == "F",]
Speciesm = Species[Sex == "M"]
Speciesf = Species[Sex == "F"]

ggplot(plotsexm, aes(x=Total.Mass.x, y=Head.x, color=Speciesm)) +
  geom_point() 

ggplot(plotsexf, aes(x=Total.Mass.x, y=Head.x, color=Speciesf)) +
  geom_point()



plotsexm$Prediction = rep(0, nrow(plotsexm))
plotsexf$Prediction = rep("HYBRID", nrow(plotsexf))

for(i in 1:nrow(plotsexm)){
  if(plotsexm$Total.Mass.x[i] >= 1168 & plotsexm$Total.Mass.x[i] < 1238) plotsexm$Prediction[i] = "MALL"
  if(plotsexm$Total.Mass.x[i] >= 1168 & plotsexm$Total.Mass.x[i] >= 1238) plotsexm$Prediction[i] = "HYBRID"
  if(plotsexm$Total.Mass.x[i] < 1168 & plotsexm$Head.x[i] < 107) plotsexm$Prediction[i] = "GREY"
  if(plotsexm$Total.Mass.x[i] < 1168 & plotsexm$Head.x[i] >= 107) plotsexm$Prediction[i] = "HYBRID"
}

for(i in 1:nrow(plotsexf)){
  if(plotsexf$Total.Mass.x[i] >= 973 & plotsexf$Head.x[i] >= 109) plotsexf$Prediction[i] = "MALL"
}

ggplot(plotsexm, aes(x=Total.Mass.x, y=Head.x, color=Prediction)) +
  geom_point() 

ggplot(plotsexf, aes(x=Total.Mass.x, y=Head.x, color=Prediction)) +
  geom_point()


Correct_m = plotsexm$Prediction == Speciesm
Correct_f = plotsexf$Prediction == Speciesf
  
ggplot(plotsexm, aes(x=Total.Mass.x, y=Head.x)) +
  geom_point(aes(color=Correct_m)) +
  geom_segment(aes(x=1168,y=125,xend=1168,yend=100)) +
  geom_segment(aes(x=700,y=107,xend=1168,yend=107)) +
  geom_segment(aes(x=1238,y=125,xend=1238,yend=100)) +
  geom_text(x=900, y=120, label="HYBRID") +
  geom_text(x=1350, y=107, label="HYBRID") +
  geom_text(x=900, y=102, label="GREY") +
  geom_text(x=1200, y=102, label="MALL") +
  labs(color = "Correct") +
  xlab("Total Mass") +
  ylab("Head")

ggplot(plotsexf, aes(x=Total.Mass.x, y=Head.x)) +
  geom_point(aes(color=Correct_f)) +
  geom_segment(aes(x=973,y=109,xend=1325,yend=109)) +
  geom_segment(aes(x=973,y=109,xend=973,yend=116)) +
  geom_text(x=1200, y=102, label="HYBRID") +
  geom_text(x=1200, y=115, label="MALL")  +
  labs(color = "Correct") +
  xlab("Total Mass") +
  ylab("Head")



decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}


knndatm = plotsexm[,c("Total.Mass.x", "Head.x")]
knndatm$Species = Speciesm
knndatf = plotsexf[,c("Total.Mass.x", "Head.x")]
knndatf$Species = Speciesf

library(caret)

knnmod1 = knn3(Species~Total.Mass.x*Head.x, data=knndatm, k=1)
decisionplot(knnmod1, knndatm, class="Species")

library(e1071)

bayesmod = lda(Species~., data=knndatm)
decisionplot(bayesmod, knndatm, class="Species")


bayesmodf = lda(Species~., data=knndatf)
decisionplot(bayesmodf, knndatf, class="Species")



rfsimp = randomForest(as.factor(Species)~Total.Mass.x+Head.x, data=knndatm, ntree=500, mtry=7, importance=T)
rfsimp

rfcvsimp = rf.crossValidation(rfsimp, knndatm, seed=0309)
rfcvsimp



rfsimpm = randomForest(as.factor(Species)~., data=knndatm, ntree=500, nodesize=5)

decisionplot(rfsimpm, knndatm, class="Species", xlab="Total Mass", ylab="Head")

rfsimpf= randomForest(as.factor(Species)~., data=knndatf, ntree=500, nodesize=5)

decisionplot(rfsimpf, knndatf, class="Species", xlab="Total Mass", ylab="Head")


####http://www.cmap.polytechnique.fr/~lepennec/R/Learning/Learning.html






###########################
## Dashboarding Practice ##
###########################


library(shiny)
library(shinydashboard)

mldat1 = mldat[,2:7]

## Pairs plot


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                selectInput("variable", "Choose a variable for the X axis:",
                            colnames(mldat1)
                ),
                selectInput("variable2", "Choose a variable for the Y axis:",
                            colnames(mldat1)
                ),
                selectInput("sex", "Choose a sex for the ducks:",
                            c("M", "F")
                ),
                selectInput("species", "Choose a species for the ducks:",
                            c("MALL", "HYBRID", "GREY")
                ),
                box(plotOutput("plot1", height = 250)),
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)


server <- function(input, output) {
  output$plot1 = renderPlot(plot(mldat1[as.logical((mldat$Sex == input$sex) * (mldat$gen.simple == input$species)),
                                        c(input$variable)],
                                 mldat1[as.logical((mldat$Sex == input$sex) * (mldat$gen.simple == input$species)),
                                        c(input$variable2)]))
}


shinyApp(ui, server)



library(rsconnect)
deployApp(appName = "Ducks", appFiles = NULL)







