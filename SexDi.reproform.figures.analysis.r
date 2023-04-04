#Analysis & Figures for Male + Female (Sexual Dimorphism)
#F. obscurus reproductive form project (i will have a separate file for female/sexual dimorphism)
#Starting on 7/8/22
library(scales)
library(car)
library(AICcmodavg)
library(lme4)
library(lmerTest)
library(nlme)
library(bbmle)
library(geomorph)
library(emmeans)

#I am going to first re-create all of the figures from the male repro form paper (except with females)
#Then I will try to do some comparisons based on reproductive form and sex

#Read in female data
femaledata <- read.table("claw.data.female.csv",fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")
female.table<-read.table("claw.data.female.csv",h=T,sep=',')

#Read in the .tps file
femaleobscurus<-readland.tps("Obscurus_claw_GMM_female.TPS",specID="ID")

#Creating the sliders for females
#slid.female.obscurus<-define.sliders(femaleobscurus[,,1],nsliders=16) 
#write.csv(slid.female.obscurus,"female.sliders.csv",row.names=F) 

#Reading in the female sliders file
slid.female.obscurus<-read.table("female.sliders.csv",sep=",",h=T)

#GPA on female claws
gpa.female.obscurus<-gpagen(femaleobscurus,curves=slid.female.obscurus,ProcD=T)

plot(gpa.female.obscurus) #just checking if everything is OK

#Appending the centroid csize to the female.table which is necessary for below
female.table$csize<-log(gpa.female.obscurus$Csize) 

#Use the code below to export the csize into an xls file that I can then add into my claw.data.male.file
library(xlsx)
write.xlsx(unlist(female.agdt$csize),"female.centroid.export.xlsx")

#Turning everything into a table to make it easier to work with for geomorph
female.agdt<-geomorph.data.frame(gpa.female.obscurus, side = female.table$claw.side, repro.form = female.table$repro.form, sex = female.table$sex,
                                 claw.length.mm=female.table$claw.length.mm, ind = female.table$crayfish.id,csize=female.table$csize, id = female.table$ID )
str(female.agdt)

#PCA on female data
femalePCA <- gm.prcomp(gpa.female.obscurus$coords)
summary(femalePCA)

plot(femalePCA, main = "PCA")

#png("Female_all_PCplot.png",res=600,w=300,h=240,units='mm')

colors <- c(alpha("black",0.75), alpha("brown",0.75))
colors <- colors[as.factor(male.avg.data$repro.form)]

plot(femalePCA, pch=shapes, cex=2,bty='l', 
     col=colors, xlab = "Principal component 1 (39.48%)",ylab = "Principal component 2 (16.00%)", cex.lab =1.3)

legend("topright", 
       legend=c(expression("F1"),
                expression("F2")),
       pch = c(21,21),
       pt.bg=c(alpha("black",0.75),alpha("darkolivegreen4",0.75)
               ,bty='n',cex=1.2))

#dev.off()

#Code to pick any single dot on the PCA plot and it will show you the shape
#picknplot.shape(plot(femalePCA), method = "vector")

femaleref1 <- mshape(gpa.female.obscurus$coords)
femalelinks1 <- define.links(femaleref1, ptsize=1 , links = NULL)

picknplot.shape(plot(femalePCA), method = "points", links = femalelinks1)


#Symmetry analysis for males; I am doing this to create an average shape of from the L+R claws for each female
claw.sym.female <- bilat.symmetry(A = gpa.female.obscurus, ind = ind, side = side,
                                  object.sym = FALSE, RRPP = TRUE, iter = 999, 
                                  data = female.agdt)

summary(claw.sym.female) #this is showing that female claws are symmetrical in shape but not size
plot(claw.sym.female, warpgrids = TRUE) 

#Using the symm.shape function in bilat.symmetry to calculate an average shape from the females
female_avg_shape <- claw.sym.female$symm.shape
str(female_avg_shape)
head(female_avg_shape)
#female_avg_shape is now the average shape for each female

#RUnning a new GPA on the average claw data (this is so I can later plot what the avreage shape of these females looks like)
gpa.female.avg.obscurus<-gpagen(female_avg_shape,curves=slid.female.obscurus,ProcD=T)
plot(gpa.female.avg.obscurus)

#Making a consensus shape for males; but the lines are connecting so now i am going to make define a links function to fix this 
#and plot a better consensus shape
consensus <- apply(gpa.female.avg.obscurus$coords, c(1,2), mean)
plot(consensus, asp=1, type = "n")
for (i in 1:length(gpa.female.avg.obscurus$coords[,,3]))points(gpa.female.avg.obscurus$coords[,,i])
points(consensus, col = "Red", cex = 2, pch = 20)
lines(consensus, col = "black", lwd = 2)

#Making a reference and then using define.links to make my outline
femaleref <- mshape(gpa.female.avg.obscurus$coords)
femalelinks <- define.links(femaleref, ptsize=1 , links = NULL)

#This is the mean shape for all female obscurus
plotAllSpecimens(gpa.female.avg.obscurus$coords, links = femalelinks, plot.param = list(pt.cex = 0, mean.cex = 0))

###==================================================###
#Creating consensus shapes for F1 females and F2 females (Fig X.)####
###==================================================###
femaledataf1 <- read.table("claw.data.femalef1.csv",fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")
femalef1.table<-read.table("claw.data.femalef1.csv",h=T,sep=',')
###
#F1 Females
###
femalef1obscurus<-readland.tps("Obscurus_claw_GMM_femalef1.TPS",specID="ID")

gpa.femalef1.obscurus<-gpagen(femalef1obscurus,curves=slid.female.obscurus,ProcD=T)
plot(gpa.malef1.obscurus)
#gpa.femalef1.obscurus <- rotate.coords(gpa.femalef1.obscurus, "flipY")
#gpa.femalef1.obscurus <- rotate.coords(gpa.femalef1.obscurus, "flipX")
plot(gpa.femalef1.obscurus) #much better


female.f1.agdt<-geomorph.data.frame(gpa.femalef1.obscurus, side = femalef1.table$claw.side, repro.form = femalef1.table$repro.form, sex = femalef1.table$sex,
                                  claw.length.mm=femalef1.table$claw.length.mm, ind =femalef1.table$crayfish.id, id = femalef1.table$ID )
str(female.f1.agdt)


#Symmetry analysis for f1 females; I am doing this to create an average shape of from the L+R claws for each male
claw.sym.femalef1 <- bilat.symmetry(A = gpa.femalef1.obscurus, ind = ind, side = side,
                                  object.sym = FALSE, RRPP = TRUE, iter = 999, 
                                  data = female.f1.agdt)

summary(claw.sym.femalef1) #this is showing that male claws are not symmetrical in shape or size
plot(claw.sym.femalef1, warpgrids = TRUE) 

#Using the symm.shape function in bilat.symmetry to calculate an average shape from the males
femalef1_avg_shape <- claw.sym.femalef1$symm.shape
str(femalef1_avg_shape)
head(femalef1_avg_shape)

gpa.femalef1.avg.obscurus<-gpagen(femalef1_avg_shape,curves=slid.female.obscurus,ProcD=T)
plot(gpa.femalef1.avg.obscurus)

consensusf1 <- apply(gpa.femalef1.avg.obscurus$coords, c(1,2), mean)
plot(consensusf1, asp=1, type = "n")
for (i in 1:length(gpa.femalef1.avg.obscurus$coords[,,3]))points(gpa.femalef1.avg.obscurus$coords[,,i])
points(consensusf1, col = "Red", cex = 2, pch = 20)
lines(consensusf1, col = "black", lwd = 2)

#Making a reference and then using define.links to make my outline
femalereff1 <- mshape(gpa.femalef1.avg.obscurus$coords)
femalef1links <- define.links(femalereff1, ptsize=1 , links = NULL)

#This is the mean shape for f1 female obscurus
#gpa.femalef1.avg.obscurus$coords <- rotate.coords(gpa.femalef1.avg.obscurus$coords, "flipY")
#gpa.femalef1.avg.obscurus$coords <- rotate.coords(gpa.femalef1.avg.obscurus$coords, "flipX")
plotAllSpecimens(gpa.femalef1.avg.obscurus$coords, links = femalef1links, plot.param = list(pt.cex = 0, mean.cex = 0))

###
#F2 females
###
femaledataf2 <- read.table("claw.data.femalef2.csv",fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")
femalef2.table<-read.table("claw.data.femalef2.csv",h=T,sep=',')
###
#F2 females
###
femalef2obscurus<-readland.tps("Obscurus_claw_GMM_femalef2.TPS",specID="ID")

gpa.femalef2.obscurus<-gpagen(femalef2obscurus,curves=slid.female.obscurus,ProcD=T)
plot(gpa.femalef2.obscurus)

female.f2.agdt<-geomorph.data.frame(gpa.femalef2.obscurus, side = femalef2.table$claw.side, repro.form = femalef2.table$repro.form, sex = femalef2.table$sex,
                                  claw.length.mm=femalef2.table$claw.length.mm, ind =femalef2.table$crayfish.id, id = femalef2.table$ID )
str(female.f2.agdt)

#Symmetry analysis for f1 males; I am doing this to create an average shape of from the L+R claws for each male
claw.sym.femalef2 <- bilat.symmetry(A = gpa.femalef2.obscurus, ind = ind, side = side,
                                  object.sym = FALSE, RRPP = TRUE, iter = 999, 
                                  data = female.f2.agdt)

summary(claw.sym.femalef2) #this is showing that male claws are not symmetrical in shape or size
plot(claw.sym.femalef2, warpgrids = TRUE) 

#Using the symm.shape function in bilat.symmetry to calculate an average shape from the males
femalef2_avg_shape <- claw.sym.femalef2$symm.shape
str(femalef2_avg_shape)
head(femalef2_avg_shape)

gpa.femalef2.avg.obscurus<-gpagen(femalef2_avg_shape,curves=slid.female.obscurus,ProcD=T)
plot(gpa.femalef2.avg.obscurus)

consensusf2 <- apply(gpa.femalef2.avg.obscurus$coords, c(1,2), mean)
plot(consensusf2, asp=1, type = "n")
for (i in 1:length(gpa.femalef2.avg.obscurus$coords[,,3]))points(gpa.femalef2.avg.obscurus$coords[,,i])
points(consensusf2, col = "Red", cex = 2, pch = 20)
lines(consensusf2, col = "black", lwd = 2)

#Making a reference and then using define.links to make my outline
femalereff2 <- mshape(gpa.femalef2.avg.obscurus$coords)
femalef2links <- define.links(femalereff2, ptsize=1 , links = NULL)

#This is the mean shape for f2 female obscurus
plotAllSpecimens(gpa.malef2.avg.obscurus$coords, links = femalef2links, plot.param = list(pt.cex = 0, mean.cex = 0, link.col = "brown", link.lty = 2, link.lwd = 4))

#Comparing the average shape of F1 to F2 claws
#F1
png("F1_female_avg_consensus.png",res=600,w=300,h=240,units='mm')
plotAllSpecimens(gpa.femalef1.avg.obscurus$coords, links = femalef1links, plot.param = list(pt.cex = 0, mean.cex = 0, link.lwd = 4))
dev.off()

#F2
png("F2_female_avg_consensus.png",res=600,w=300,h=240,units='mm')
plotAllSpecimens(gpa.femalef2.avg.obscurus$coords, links = femalef2links, plot.param = list(pt.cex = 0, mean.cex = 0, link.col = "brown", link.lty = 2, link.lwd = 4))
dev.off()


###=============================###
###Female Repro form GMM analysis####
###=============================###
model2<-procD.lm(gpa.female.obscurus$coords~csize*repro.form,data=female.agdt,RRPP=T,iter=9999)
summary(model2)

#Based on this, there are shpe differences between F1 and F2 males in claw shape 
#Analysis of Variance, using Residual Randomization
#Permutation procedure: Randomization of null model residuals 
#Number of permutations: 10000 
#Estimation method: Ordinary Least Squares 
#Sums of Squares and Cross-products: Type I 
#Effect sizes (Z) based on F distributions
#
#Df      SS        MS     Rsq      F       Z Pr(>F)    
#csize              1 0.01178 0.0117832 0.03655 5.0153  2.9997 0.0007 ***
#  repro.form         1 0.02206 0.0220615 0.06842 9.3900  3.7314 0.0001 ***
#  csize:repro.form   1 0.00195 0.0019482 0.00604 0.8292 -0.0463 0.5188    
#Residuals        122 0.28664 0.0023495 0.88899                          
#Total            125 0.32243                                         

###==================================================###        
#Testing the morphological disparity between F1 and F2 Male claws
###==================================================###  
mdisp<-morphol.disparity(f1=model2, groups = ~repro.form, data=female.agdt, iter = 9999)
mdisp

#No difference in morpho disparity between F1 and F2 males
#Procrustes variances for defined groups
#1           2 
#0.002119163 0.002446199 

#Pairwise absolute differences between variances
#1            2
#1 0.0000000000 0.0003270352
#2 0.0003270352 0.0000000000

#P-Values
#1     2
#1 1.000 0.212
#2 0.212 1.000

###========================###
###Female Figures and analysis####
###========================###
data <- read.table("claw.data.female.csv",fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")

data$sex <- as.factor(data$sex)
data$repro.form <- as.factor(data$repro.form)

#Female body length vs claw length (Fig 3.)####
m1 <- lmer(claw.length.mm ~ carapace.length.mm*repro.form + (1|crayfish.id), na.action="na.omit",data=data)
summary(m1)
confint(m1)

#Calculating slope contrasts based on repro.form
emtrends(m1, pairwise ~ repro.form, var = "carapace.length.mm")
#No difference in slope between F1 and F2 male claw length vs body size

#Calculating intercept contrasts based on repro.form
emmeans(m1, pairwise ~ repro.form*carapace.length.mm)
#Greater intercept in form 1 vs form 2 (p = 0.0001)


#png("Female_body_size_claw_length.png",res=600,w=300,h=240,units='mm')
par(mfrow=c(1,1))
shapes = c(19,19)
shapes <- shapes[as.factor(data$repro.form)]

colors <- c(alpha("black",0.75), alpha("brown",0.75))

colors <- colors[as.factor(data$repro.form)]

plot(data$carapace.length.mm,data$claw.length.mm, 
     pch=shapes, cex=2,bty='l', 
     col=colors, las=1, cex.lab= 1.3,
     xlab="Carapace length (mm)", ylab="Claw length (mm)")

curve(((0.1117) + (0.6126*x)), col=alpha('black',0.75),lwd=3,lty=2,to=36,from=21, add=T) #F1
curve(((0.1117 + 1.6024) + (0.6126 -0.1143)*x), col=alpha('brown',0.75), lwd=3,lty=2, add=T) #F2
#dev.off()

#Female body length vs claw centroid size (Fig 3.)####
m7 <- lmer(centroid.size ~ carapace.length.mm*repro.form + (1|crayfish.id), na.action="na.omit",data=data)
summary(m7)
confint(m7)

#Calculating slope contrasts based on repro.form
emtrends(m7, pairwise ~ repro.form, var = "carapace.length.mm")
#Slopes are not different

#Calculating intercept contrasts based on repro.form
emmeans(m7, pairwise ~ repro.form*carapace.length.mm)
#Intercepts are different

#png("Female_body_size_centroid_size.png",res=600,w=300,h=240,units='mm')
par(mfrow=c(1,1))

shapes = c(19,19)
shapes <- shapes[as.factor(data$repro.form)]

colors <- c(alpha("black",0.75), alpha("brown",0.75))

colors <- colors[as.factor(data$repro.form)]

plot(data$carapace.length.mm,data$centroid.size, 
     pch=shapes, cex=2,bty='l', 
     col=colors, las=1, cex.lab= 1.3,
     xlab="Carapace length (mm)", ylab="Claw centroid size")

curve(((0.188529 ) + (0.030806 *x)), col=alpha('black',0.75),lwd=3,lty=2,to=35, from=21, add=T) #F1
curve(((0.188529  - 0.190742) + ((0.030806 + 0.001916 )*x)), col=alpha('brown',0.75), lwd=3,lty=2, add=T) #F2
#dev.off()

#Female claw length vs claw strength (Fig 4.)####
m2 <- lmer(claw.strength.v ~ claw.length.mm*repro.form + (1|crayfish.id), na.action="na.omit", data=data)
summary(m2)
confint(m2)

#Calculating slope contrasts based on repro.form
emtrends(m2, pairwise ~ repro.form, var = "claw.length.mm")
#Slopes are not different

#Calculating intercept contrasts based on repro.form
emmeans(m2, pairwise ~ repro.form*claw.length.mm)
#Intercepts are differentt

#png("Female_claw_length_strength.png",res=600,w=300,h=240,units='mm')
par(mfrow=c(1,1))

shapes = c(19,19)
shapes <- shapes[as.factor(data$repro.form)]

colors <- c(alpha("black",0.75), alpha("brown",0.75))

colors <- colors[as.factor(data$repro.form)]

plot(data$claw.length.mm,data$claw.strength.v, 
     pch=shapes, cex=2,bty='l', 
     col=colors, las=1, cex.lab= 1.3,
     xlab="Claw length (mm)", ylab="Claw strength (N)")

curve(((-5.4384) + ( 0.7627*x)), col=alpha('black',0.75),lwd=3,lty=2,from = 12, add=T) #F1
curve(((-5.4384 + 1.1522) + (( 0.7627 -0.1746)*x)), col=alpha('brown',0.75), lwd=3,lty=2, to=23, add=T) #F2
#dev.off()

#Female centroid size vs claw strength (Fig 4.)####
m12 <- lmer(claw.strength.v ~ centroid.size*repro.form + (1|crayfish.id), na.action="na.omit",data=data)
summary(m12)
confint(m12)

#Calculating slope contrasts based on repro.form
emtrends(m12, pairwise ~ repro.form, var = "centroid.size")
#Slopes are  different

#Calculating intercept contrasts based on repro.form
emmeans(m12, pairwise ~ repro.form*centroid.size)
#Intercepts are not different

#png("Female_claw_centroid_strength.png",res=600,w=300,h=240,units='mm')
par(mfrow=c(1,1))

shapes = c(19,19)
shapes <- shapes[as.factor(data$repro.form)]

colors <- c(alpha("black",0.75), alpha("brown",0.75))

colors <- colors[as.factor(data$repro.form)]

plot(data$centroid.size,data$claw.strength.v, 
     pch=shapes, cex=2,bty='l', 
     col=colors, las=1, cex.lab=1.3,
     xlab="Claw centroid size", ylab="Claw strength (N)")

curve(( -9.064  + (15.952*x)), col=alpha('black',0.75),lwd=3,lty=2,from=0.7, add=T) #F1
curve((-9.064 + 5.972  ) + ((15.952 -7.202 )*x), col=alpha('brown',0.75), from=0.2, lwd=3,lty=2, add=T) #F2
#dev.off()


#Residual claw strength based on F1 and F2 (Fig X.)####
###Calcualte residuals for female data
m1 <- lm(data$claw.strength.v ~ data$claw.length.mm)
summary(m1)
resids <- resid(m1)
resids
plot(density(resid(m1)))
head(resids)
str(resids)

write.csv(resids, file= "femaleresids.csv")

##Making the density plot
dataf1 <- subset(data, subset = repro.form == "1")
dataf2 <- subset(data, subset = repro.form == "2")

#png("female_Form_residual_strength.png",res=600,w=300,h=240,units='mm')

plot(density(dataf1$resid.str),bty='l',col=NA,main="",ylim=c(0,0.25),xlim=c(-10,8),
     xlab="Residual claw strength based on claw length", cex.lab = 1.3)

polygon(density(dataf1$resid.str),col= alpha("black",0.85),border= "black") #f1
polygon(density(dataf2$resid.str),col= alpha("brown",0.85),border= "brown") #f2

#dev.off()


#Residual strength based on centroid size
m2 <- lm(data$claw.strength.v ~ data$centroid.size)
summary(m2)
resids2 <- resid(m2)
resids2
plot(density(resid(m2)))
head(resids2)
str(resids2)

write.csv(resids2, file= "femaleresids_centroid.csv")

dataf11 <- subset(data, subset = repro.form == "1")
dataf22 <- subset(data, subset = repro.form == "2")


#png("female_Form_residual_strength_centroid.png",res=600,w=300,h=240,units='mm')
plot(density(data$resid.str.centroid),bty='l',col=NA,main="",ylim=c(0,0.25),xlim=c(-10,11.3),
     xlab="Residual claw strength based on claw centroid size", cex.lab = 1.3)

polygon(density(dataf11$resid.str.centroid),col= alpha("black",0.85),border= "black") #f1
polygon(density(dataf22$resid.str.centroid),col= alpha("brown",0.85),border= "brown") #f2
#dev.off()








#Slope and Intercept Sexual Dimorphism Comparisons####
#Took this code from Alex
#Trying to make a slopes and intercept graph to compare males and females
maledata <- read.table("claw.data.male.csv",fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")
femaledata <- read.table("claw.data.female.csv",fileEncoding = 'UTF-8-BOM', header=TRUE, sep=",")

male1 <- lm(claw.length.mm ~ carapace.length.mm*repro.form, na.action="na.omit",data=maledata)
female1 <- lm(claw.length.mm ~ carapace.length.mm*repro.form, na.action="na.omit",data=femaledata)

coef.male1 <- coef(male1)
ci.male1<-confint(male1)

coef.female1<-coef(female1)
ci.female1<-confint(female1)

estimates2 = data.frame(rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8))
colnames(estimates2) = c("lower.95","estimate","upper.95","measure","groups")
estimates2$measure=sort(factor(rep(c("intercept","slope"),1)))
estimates2$groups=factor(c("female.f1","female.f1",
                           "female.f2","female.f2","male.f1","male.f1",
                           "male.f2","male.f2"))
estimates2$sex=factor(c("female","female",
                        "female","female","male","male",
                        "male","male"))
estimates2$form=factor(c("f1","f1",
                            "f2","f2","f1","f1",
                            "f2","f2"))

#intercepts
estimates2[1,2]=coef.female1[1]
estimates2[5,2]=coef.female1[1]+coef.female1[3]
estimates2[3,2]=coef.male1[1]
estimates2[7,2]=coef.male1[1]+coef.male1[3]

#slopes
estimates2[2,2]=coef.female1[2]
estimates2[6,2]=coef.female1[2]+coef.female1[4]
estimates2[4,2]=coef.male1[2]
estimates2[8,2]=coef.male1[2]+coef.male1[4]

#CI - female.f1
estimates2[1,1]=ci.female1[1,1]
estimates2[1,3]=ci.female1[1,2]
estimates2[2,1]=ci.female1[2,1]
estimates2[2,3]=ci.female1[2,2]

#CI - male.f1
estimates2[3,1]=ci.male1[1,1] #intercept
estimates2[3,3]=ci.male1[1,2]
estimates2[4,1]=ci.male1[2,1] #slope
estimates2[4,3]=ci.male1[2,2]

#CI - female.f2
estimates2[5,1]=ci.female1[1,1]+ci.female1[3,1] #intercept
estimates2[5,3]=ci.female1[1,2]+ci.female1[3,2]
estimates2[6,1]=ci.female1[2,1]+ci.female1[4,1] #slope (coef.m1[1]+coef.m1[6])
estimates2[6,3]=ci.female1[2,2]+ci.female1[4,2]

#CI - female.f1
estimates2[7,1]=ci.male1[1,1]+ci.male1[3,1] #intercept
estimates2[7,3]=ci.male1[1,2]+ci.male1[3,2]
estimates2[8,1]=ci.male1[2,1]+ci.male1[4,1] #slope (coef.m1[1]+coef.m1[5]+coef.m1[6]+coef.m1[8])
estimates2[8,3]=ci.male1[2,2]+ci.male1[4,2]

inters2=estimates2[estimates2$measure=="intercept",]
slopes2=estimates2[estimates2$measure=="slope",]

inters2<-inters2[order(inters2$form),]
slopes2<-slopes2[order(slopes2$form),]

inter2.f1<-inters2[inters2$form=='f1',]
inter2.f2<-inters2[inters2$form=='f2',]

slope2.f1<-slopes2[slopes2$form=='f1',]
slope2.f2<-slopes2[slopes2$form=='f2',]

#Body size vs. claw length intercept####
#png("Intercept_slope_bodysize_claw_length.png",res=600,w=400,h=240,units='mm')

par(mfrow=c(1,2),las=1,bty='l')

plot(inters2$estimate,cex=4,las=1,bty='l',pch=19,col=c(alpha("grey",0.75), alpha("black",0.75))[inters2$sex],
     ylab="Intercept",xlab="Body size vs. claw length",ylim=c(-30,20),xaxt='n',cex.axis=1.3,cex.lab=1.3)

axis(side=1, at=(1:4), labels=c("Female F1","Male F1","Female F2","Male F2"),cex.axis=1.3)

segments(x0=1:2,y0=inter2.f1$lower.95,y1=inter2.f1$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[inters2$sex],
         lwd=5)

segments(x0=3:4,y0=inter2.f2$lower.95,y1=inter2.f2$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[inters2$sex],
         lwd=5)


#Body size vs. claw length slope####
#par(mfrow=c(1,1),las=1,bty='l')

plot(slopes2$estimate,cex=4,las=1,bty='l',pch=19,col=c(alpha("grey",0.75), alpha("black",0.75))[slopes2$sex],
     ylab="Slope",xlab="Body size vs. claw length",ylim=c(-1,2),xaxt='n',cex.axis=1.3,cex.lab=1.3)

axis(side=1, at=(1:4), labels=c("Female F1","Male F1","Female F2","Male F2"),cex.axis=1.3)

segments(x0=1:2,y0=slope2.f1$lower.95,y1=slope2.f1$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[slopes2$sex],
         lwd=5)

segments(x0=3:4,y0=slope2.f2$lower.95,y1=slope2.f2$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[slopes2$sex],
         lwd=5)
#dev.off()


#Body size vs. claw centroid size intercept####
male2 <- lm(centroid.size ~ carapace.length.mm*repro.form, na.action="na.omit",data=maledata)
female2 <- lm(centroid.size ~ carapace.length.mm*repro.form, na.action="na.omit",data=femaledata)

coef.male2 <- coef(male2)
ci.male2<-confint(male2)

coef.female2<-coef(female2)
ci.female2<-confint(female2)

estimates3 = data.frame(rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8))
colnames(estimates3) = c("lower.95","estimate","upper.95","measure","groups")
estimates3$measure=sort(factor(rep(c("intercept","slope"),1)))
estimates3$groups=factor(c("female.f1","female.f1",
                           "female.f2","female.f2","male.f1","male.f1",
                           "male.f2","male.f2"))
estimates3$sex=factor(c("female","female",
                        "female","female","male","male",
                        "male","male"))
estimates3$form=factor(c("f1","f1",
                         "f2","f2","f1","f1",
                         "f2","f2"))

#intercepts
estimates3[1,2]=coef.female2[1]
estimates3[5,2]=coef.female2[1]+coef.female2[3]
estimates3[3,2]=coef.male2[1]
estimates3[7,2]=coef.male2[1]+coef.male2[3]

#slopes
estimates3[2,2]=coef.female2[2]
estimates3[6,2]=coef.female2[2]+coef.female2[4]
estimates3[4,2]=coef.male2[2]
estimates3[8,2]=coef.male2[2]+coef.male2[4]

#CI - female.f1
estimates3[1,1]=ci.female2[1,1]
estimates3[1,3]=ci.female2[1,2]
estimates3[2,1]=ci.female2[2,1]
estimates3[2,3]=ci.female2[2,2]

#CI - male.f1
estimates3[3,1]=ci.male2[1,1] #intercept
estimates3[3,3]=ci.male2[1,2]
estimates3[4,1]=ci.male2[2,1] #slope
estimates3[4,3]=ci.male2[2,2]

#CI - female.f2
estimates3[5,1]=ci.female2[1,1]+ci.female2[3,1] #intercept
estimates3[5,3]=ci.female2[1,2]+ci.female2[3,2]
estimates3[6,1]=ci.female2[2,1]+ci.female2[4,1] #slope (coef.m1[1]+coef.m1[6])
estimates3[6,3]=ci.female2[2,2]+ci.female2[4,2]

#CI - female.f1
estimates3[7,1]=ci.male2[1,1]+ci.male2[3,1] #intercept
estimates3[7,3]=ci.male2[1,2]+ci.male2[3,2]
estimates3[8,1]=ci.male2[2,1]+ci.male2[4,1] #slope (coef.m1[1]+coef.m1[5]+coef.m1[6]+coef.m1[8])
estimates3[8,3]=ci.male2[2,2]+ci.male2[4,2]

inters3=estimates3[estimates3$measure=="intercept",]
slopes3=estimates3[estimates3$measure=="slope",]

inters3<-inters3[order(inters3$form),]
slopes3<-slopes3[order(slopes3$form),]

inter3.f1<-inters3[inters3$form=='f1',]
inter3.f2<-inters3[inters3$form=='f2',]

slope3.f1<-slopes3[slopes3$form=='f1',]
slope3.f2<-slopes3[slopes3$form=='f2',]

#Body size vs. centroid size intercept####
#png("Intercept_slope_bodysize_centroid.png",res=600,w=400,h=240,units='mm')

par(mfrow=c(1,2),las=1,bty='l')

plot(inters3$estimate,cex=4,las=1,bty='l',pch=19,col=c(alpha("grey",0.75), alpha("black",0.75))[inters3$sex],
     ylab="Intercept",xlab="Body size vs. Claw centroid size",ylim=c(-5,5),xaxt='n',cex.axis=1.3,cex.lab=1.3)

axis(side=1, at=(1:4), labels=c("Female F1","Male F1","Female F2","Male F2"),cex.axis=1.3)

segments(x0=1:2,y0=inter3.f1$lower.95,y1=inter3.f1$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[inters3$sex],
         lwd=5)

segments(x0=3:4,y0=inter3.f2$lower.95,y1=inter3.f2$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[inters3$sex],
         lwd=5)

#Body size vs. claw length slope####
#par(mfrow=c(1,1),las=1,bty='l')

plot(slopes3$estimate,cex=4,las=1,bty='l',pch=19,col=c(alpha("grey",0.75), alpha("black",0.75))[slopes3$sex],
     ylab="Slope",xlab="Body size vs. Claw centroid size",ylim=c(-0.1,0.1),xaxt='n',cex.axis=1.3,cex.lab=1.3)

axis(side=1, at=(1:4), labels=c("Female F1","Male F1","Female F2","Male F2"),cex.axis=1.3)

segments(x0=1:2,y0=slope3.f1$lower.95,y1=slope3.f1$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[slopes3$sex],
         lwd=5)

segments(x0=3:4,y0=slope3.f2$lower.95,y1=slope3.f2$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[slopes3$sex],
         lwd=5)

#dev.off()

#Claw length vs. claw strength intercept####
male3 <- lm(claw.strength.v ~ claw.length.mm*repro.form, na.action="na.omit",data=maledata)
female3 <- lm(claw.strength.v ~ claw.length.mm*repro.form, na.action="na.omit",data=femaledata)

coef.male3 <- coef(male3)
ci.male3<-confint(male3)

coef.female3<-coef(female3)
ci.female3<-confint(female3)

estimates4 = data.frame(rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8))
colnames(estimates4) = c("lower.95","estimate","upper.95","measure","groups")
estimates4$measure=sort(factor(rep(c("intercept","slope"),1)))
estimates4$groups=factor(c("female.f1","female.f1",
                           "female.f2","female.f2","male.f1","male.f1",
                           "male.f2","male.f2"))
estimates4$sex=factor(c("female","female",
                        "female","female","male","male",
                        "male","male"))
estimates4$form=factor(c("f1","f1",
                         "f2","f2","f1","f1",
                         "f2","f2"))

#intercepts
estimates4[1,2]=coef.female3[1]
estimates4[5,2]=coef.female3[1]+coef.female3[3]
estimates4[3,2]=coef.male3[1]
estimates4[7,2]=coef.male3[1]+coef.male3[3]

#slopes
estimates4[2,2]=coef.female3[2]
estimates4[6,2]=coef.female3[2]+coef.female3[4]
estimates4[4,2]=coef.male3[2]
estimates4[8,2]=coef.male3[2]+coef.male3[4]

#CI - female.f1
estimates4[1,1]=ci.female3[1,1]
estimates4[1,3]=ci.female3[1,2]
estimates4[2,1]=ci.female3[2,1]
estimates4[2,3]=ci.female3[2,2]

#CI - male.f1
estimates4[3,1]=ci.male3[1,1] #intercept
estimates4[3,3]=ci.male3[1,2]
estimates4[4,1]=ci.male3[2,1] #slope
estimates4[4,3]=ci.male3[2,2]

#CI - female.f2
estimates4[5,1]=ci.female3[1,1]+ci.female3[3,1] #intercept
estimates4[5,3]=ci.female3[1,2]+ci.female3[3,2]
estimates4[6,1]=ci.female3[2,1]+ci.female3[4,1] #slope (coef.m1[1]+coef.m1[6])
estimates4[6,3]=ci.female3[2,2]+ci.female3[4,2]

#CI - female.f1
estimates4[7,1]=ci.male3[1,1]+ci.male3[3,1] #intercept
estimates4[7,3]=ci.male3[1,2]+ci.male3[3,2]
estimates4[8,1]=ci.male3[2,1]+ci.male3[4,1] #slope (coef.m1[1]+coef.m1[5]+coef.m1[6]+coef.m1[8])
estimates4[8,3]=ci.male3[2,2]+ci.male3[4,2]

inters4=estimates4[estimates4$measure=="intercept",]
slopes4=estimates4[estimates4$measure=="slope",]

inters4<-inters4[order(inters4$form),]
slopes4<-slopes4[order(slopes4$form),]

inter4.f1<-inters4[inters4$form=='f1',]
inter4.f2<-inters4[inters4$form=='f2',]

slope4.f1<-slopes4[slopes4$form=='f1',]
slope4.f2<-slopes4[slopes4$form=='f2',]

#Claw length vs claw strength intercept and slope####
png("Intercept_slope_clawlength_claw_strength.png",res=600,w=400,h=240,units='mm')

par(mfrow=c(1,2),las=1,bty='l')

plot(inters4$estimate,cex=4,las=1,bty='l',pch=19,col=c(alpha("grey",0.75), alpha("black",0.75))[inters4$sex],
     ylab="Intercept",xlab="Claw length vs. Claw Strength",ylim=c(-15,15),xaxt='n',cex.axis=1.3,cex.lab=1.3)

axis(side=1, at=(1:4), labels=c("Female F1","Male F1","Female F2","Male F2"),cex.axis=1.3)

segments(x0=1:2,y0=inter4.f1$lower.95,y1=inter4.f1$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[inters4$sex],
         lwd=5)

segments(x0=3:4,y0=inter4.f2$lower.95,y1=inter4.f2$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[inters3$sex],
         lwd=5)

#Body size vs. claw length slope####
#par(mfrow=c(1,1),las=1,bty='l')

plot(slopes4$estimate,cex=4,las=1,bty='l',pch=19,col=c(alpha("grey",0.75), alpha("black",0.75))[slopes4$sex],
     ylab="Slope",xlab="Claw length vs. Claw strength",ylim=c(-0.5,1.5),xaxt='n',cex.axis=1.3,cex.lab=1.3)

axis(side=1, at=(1:4), labels=c("Female F1","Male F1","Female F2","Male F2"),cex.axis=1.3)

segments(x0=1:2,y0=slope4.f1$lower.95,y1=slope4.f1$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[slopes4$sex],
         lwd=5)

segments(x0=3:4,y0=slope4.f2$lower.95,y1=slope4.f2$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[slopes4$sex],
         lwd=5)

dev.off()


#Claw centroid size vs. claw strength intercept####
male4 <- lm(claw.strength.v ~ centroid.size*repro.form, na.action="na.omit",data=maledata)
female4 <- lm(claw.strength.v ~ centroid.size*repro.form, na.action="na.omit",data=femaledata)

coef.male4 <- coef(male4)
ci.male4<-confint(male4)

coef.female4<-coef(female4)
ci.female4<-confint(female4)

estimates5 = data.frame(rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8),
                        rep(NA, 8))
colnames(estimates5) = c("lower.95","estimate","upper.95","measure","groups")
estimates5$measure=sort(factor(rep(c("intercept","slope"),1)))
estimates5$groups=factor(c("female.f1","female.f1",
                           "female.f2","female.f2","male.f1","male.f1",
                           "male.f2","male.f2"))
estimates5$sex=factor(c("female","female",
                        "female","female","male","male",
                        "male","male"))
estimates5$form=factor(c("f1","f1",
                         "f2","f2","f1","f1",
                         "f2","f2"))

#intercepts
estimates5[1,2]=coef.female4[1]
estimates5[5,2]=coef.female4[1]+coef.female4[3]
estimates5[3,2]=coef.male4[1]
estimates5[7,2]=coef.male4[1]+coef.male4[3]

#slopes
estimates5[2,2]=coef.female4[2]
estimates5[6,2]=coef.female4[2]+coef.female4[4]
estimates5[4,2]=coef.male4[2]
estimates5[8,2]=coef.male4[2]+coef.male4[4]

#CI - female.f1
estimates5[1,1]=ci.female4[1,1]
estimates5[1,3]=ci.female4[1,2]
estimates5[2,1]=ci.female4[2,1]
estimates5[2,3]=ci.female4[2,2]

#CI - male.f1
estimates5[3,1]=ci.male4[1,1] #intercept
estimates5[3,3]=ci.male4[1,2]
estimates5[4,1]=ci.male4[2,1] #slope
estimates5[4,3]=ci.male4[2,2]

#CI - female.f2
estimates5[5,1]=ci.female4[1,1]+ci.female4[3,1] #intercept
estimates5[5,3]=ci.female4[1,2]+ci.female4[3,2]
estimates5[6,1]=ci.female4[2,1]+ci.female4[4,1] #slope (coef.m1[1]+coef.m1[6])
estimates5[6,3]=ci.female4[2,2]+ci.female4[4,2]

#CI - female.f1
estimates5[7,1]=ci.male4[1,1]+ci.male4[3,1] #intercept
estimates5[7,3]=ci.male4[1,2]+ci.male4[3,2]
estimates5[8,1]=ci.male4[2,1]+ci.male4[4,1] #slope (coef.m1[1]+coef.m1[5]+coef.m1[6]+coef.m1[8])
estimates5[8,3]=ci.male4[2,2]+ci.male4[4,2]

inters5=estimates5[estimates5$measure=="intercept",]
slopes5=estimates5[estimates5$measure=="slope",]

inters5<-inters5[order(inters5$form),]
slopes5<-slopes5[order(slopes5$form),]

inter5.f1<-inters5[inters5$form=='f1',]
inter5.f2<-inters5[inters5$form=='f2',]

slope5.f1<-slopes5[slopes5$form=='f1',]
slope5.f2<-slopes5[slopes5$form=='f2',]

#Claw centrodi size vs claw strength intercept and slope####
png("Intercept_slope_centroid_size_claw_strength.png",res=600,w=400,h=240,units='mm')

par(mfrow=c(1,2),las=1,bty='l')

plot(inters5$estimate,cex=4,las=1,bty='l',pch=19,col=c(alpha("grey",0.75), alpha("black",0.75))[inters5$sex],
     ylab="Intercept",xlab="Claw centroid size vs. Claw Strength",ylim=c(-20,30),xaxt='n',cex.axis=1.3,cex.lab=1.3)

axis(side=1, at=(1:4), labels=c("Female F1","Male F1","Female F2","Male F2"),cex.axis=1.3)

segments(x0=1:2,y0=inter5.f1$lower.95,y1=inter5.f1$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[inters5$sex],
         lwd=5)

segments(x0=3:4,y0=inter5.f2$lower.95,y1=inter5.f2$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[inters5$sex],
         lwd=5)

#par(mfrow=c(1,1),las=1,bty='l')

#slope
plot(slopes5$estimate,cex=4,las=1,bty='l',pch=19,col=c(alpha("grey",0.75), alpha("black",0.75))[slopes5$sex],
     ylab="Slope",xlab="Claw centroid size vs. Claw strength",ylim=c(-15,25),xaxt='n',cex.axis=1.3,cex.lab=1.3)

axis(side=1, at=(1:4), labels=c("Female F1","Male F1","Female F2","Male F2"),cex.axis=1.3)

segments(x0=1:2,y0=slope5.f1$lower.95,y1=slope5.f1$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[slopes5$sex],
         lwd=5)

segments(x0=3:4,y0=slope5.f2$lower.95,y1=slope5.f2$upper.95,
         col=c(alpha("grey",0.7),alpha("black",0.7))[slopes5$sex],
         lwd=5)

dev.off()

#I need to do the emmeans stuff here but then I think I am done####





###
#Old Code,etc####
###
###Comparing average female to average male####
###
plotAllSpecimens(gpa.male.avg.obscurus$coords, links = malelinks, plot.param = list(pt.cex = 0, mean.cex = 0, link.lwd = 4)) #male
plotAllSpecimens(gpa.female.avg.obscurus$coords, links = femalelinks, plot.param = list(pt.cex = 0, mean.cex = 0, link.col = "blue", link.lty = 2, link.lwd = 4)) #female

malemsh <- mshape(gpa.male.avg.obscurus$coords)
plot(malemsh)
femalemsh <- mshape(gpa.female.avg.obscurus$coords)
plot(femalemsh)

plotRefToTarget(malemsh,femalemsh,method="TPS")
plotRefToTarget(malemsh,femalemsh,method="vector")





###++++++++++++++++++###
###Old Figures/Analysis###
###++++++++++++++++++###
#Male body length vs claw width###
m11 <- lmer(data$claw.width.mm ~ data$carapace.length.mm*data$repro.form + (1|data$crayfish.id), na.action="na.omit")
summary(m11)
confint(m11)

par(mfrow=c(1,1))

shapes = c(19,19)
shapes <- shapes[as.factor(data$repro.form)]

colors <- c(alpha("black",0.75), alpha("darkolivegreen4",0.75))

colors <- colors[as.factor(data$repro.form)]

plot(data$carapace.length.mm,data$claw.width.mm, 
     pch=shapes, cex=2,bty='l', 
     col=colors, las=1,
     xlab="Carapace Length (mm)", ylab="Claw Width (mm)")

curve(((-8.68801) + (0.74122*x)), col=alpha('black',0.75),lwd=3,lty=2, add=T) #F1
curve(((-8.68801 + 1.49994) + ((0.74122 -0.11763)*x)), col=alpha('darkolivegreen4',0.75), lwd=3,lty=2, add=T) #F2

