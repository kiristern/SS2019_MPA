library(vegan) # Community Ecology Package
library(asbio) # Collection Stats Tools Biologists
library(betapart) # Partition Beta Diversity
library(adespatial) # Multivariate Multiscale Spatial Analysis
library(MASS)
library(ade4)
library(lme4) # for fitting GLMMs
library(MuMIn) # model selection and allies
library(car) # for using vif (variance inflation factor)
library(modEvA) # for calculating explained deviance
library(gstat) # for fitting semivariograms 
library(sp)
library(coefplot)

#setwd("C:/Users/JAML/Desktop/Bios2Create/1_Ecological_Synthesis/SS2019_MPA")

################
# SPECIES DATA #
################

fish <- read.csv("all_data_table.csv", header = T)
dim(fish) # 3072 columns,  296 columns

# creating cells by species matrix for ordination
fishmatrix <- as.matrix(fish[,17:294])
dim(fishmatrix) # 3078 cells by 278 species
table(fishmatrix)

# set all values >=1 to 1
fishmatrix[which(fishmatrix > 1)] = 1
table(fishmatrix)

# removing cells with no ocean designation
# fish <- subset(fish, !fish$label==" ") DID IT BY HAND

# For any ordination and multivariate analysis
# Need data in matrix format
# But can't include 1 nor last column (spp. richness)
fishmatrix <- as.matrix(fish[,17:294])
dim(fishmatrix) # 3072 sites, 278 species

#AO
fishAO <- subset(fish, fish$label=="AO")
fishmatrixAO <- as.matrix(fishAO[,17:294])

#IO
fishIO <- subset(fish, fish$label=="IO")
fishmatrixIO <- as.matrix(fishIO[,17:294])

#NAO
fishNAO <- subset(fish, fish$label=="NAO")
fishmatrixNAO <- as.matrix(fishNAO[,17:294])

#NPO
fishNPO <- subset(fish, fish$label=="NPO")
fishmatrixNPO <- as.matrix(fishNPO[,17:294])

#SAO
fishSAO <- subset(fish, fish$label=="SAO")
fishmatrixSAO <- as.matrix(fishSAO[,17:294])

#SO
fishSO <- subset(fish, fish$label=="SO")
fishmatrixSO <- as.matrix(fishSO[,17:294])

#SPO
fishSPO <- subset(fish, fish$label=="SPO")
fishmatrixSPO <- as.matrix(fishSPO[,17:294])

basAO <- beta.div.comp(fishmatrixAO, "BS")


#create empty array to put in turnover and nestedness results from the different ocean basins
Turn_Nest <- data.frame(matrix(NA, nrow = 8, ncol = 2), stringsAsFactors = FALSE)






##############
# COVARIATES #
##############

# FACTORS
MPA <- as.factor(fish$DESIG)
summary(MPA)

Ocean <- as.factor(fish$label)
summary(Ocean)



# CONTINUOUS
# location
lat <- fish$Latitude
long <- fish$Longitude

# explanatory
prod <- sqrt(fish$PPROD_MN)
hist(prod)

temp <- fish$SST_MEAN
hist(temp)

oxi <- fish$DOXY_MN
hist(oxi)

coast <- sqrt(sqrt(fish$SUM_Length))
hist(coast)


# Species Richness #
####################

# Species richness exploration
rich <- fish$Richness
summary(rich)
hist(rich)

require(vcd)
# fitting discrete distributions to the abundance data
gf <- goodfit(rich,type= "nbinomial",method= "ML")
summary(gf); plot(gf) 


# richness per ocean, MPA
boxplot(rich~Ocean)
boxplot(rich~MPA)


# pairwise comparisons
pw <- pairw.anova(rich, x=MPA, method="tukey")
plot.pairw(pw, type = 1, las=1, ylab="Richness per MPA")


#############################
# richness against covariates

plot(rich~prod)
abline(lm(rich~prod))
lines(lowess(rich ~ prod), col="red")

plot(rich~temp)
abline(lm(rich~temp))
lines(lowess(rich ~ temp), col="red")

plot(rich~oxi)
abline(lm(rich~oxi))

plot(rich~coast)
abline(lm(rich~coast))

plot(rich~lat)
abline(lm(rich~lat))

plot(rich~long)
abline(lm(rich~long))


# To run Generalized Additive Models
library(mgcv)

AM1 <- gam(rich ~ s(prod)+s(temp)+s(coast)+s(lat)+s(long), family="nb")
summary(AM1)

gam.vcomp(AM1)
anova(AM1)
plot(AM1)
plot.gam(AM1, residuals=F)
gam.check(AM1)




####################
# Modelling Richness

# RANDOM EFFECTS MODEL
# negative binomial fijo y aleatorio
modnb0 <- glm.nb(rich~1)
exp(coefficients(modnb0))

modnb1 <- glm.nb(rich~prod+temp+coast+poly(lat,2)+long)
summary(modnb1)
stepAIC(modnb1)

coefplot(modnb1)
Dsquared(modnb1)
RsqGLM(modnb1)

sum.model <- summary(modnb1) # overdispersion?
(phi.model <- sum.model$deviance/sum.model$df.residual)  # no, underdisp actually!

coefplot(modnb1, title="Threatened Richness",
         xlab = "", ylab = "", color = "black", innerCI = 0,
         intercept=F, horizontal=F,
         sort="magnitude")



# spatial autocorrelation
fish$p.residraw <-  residuals(modnb1, type="pearson")

# with package gstat
E <- fish$p.residraw
mydata <- data.frame(E, long, lat)
coordinates(mydata) <- c("long", "lat")
bubble(mydata, "E", main="Model residuals", xlab="Long.", ylab="Lat.", col=c("1","8"))


# semivariogram
vario <- variogram(E~1,mydata)
plot(vario, main="Semivariogram", xlab="Distance", ylab="Residuals semivariance")













# response curves
# Effect of prod
########################
summary(scale(prod))

plot(rich~prod, 
     ylab="Predicted richness", xlab="Prod", cex=0.5, col=8)

newData <- data.frame(prod=seq(xx , xx, length.out = xx), 
                      temp=0,
                      oxi=0,           
                      lat=0,
                      long=0)

fishpred <- predict(rich0, type="response", newdata=newData, se=TRUE, appendData=TRUE)

plot(fishpred$fit ~ newData$prod)
lines((fishpred$fit+1.96*fishpred$se.fit) ~ newData$prod, type="l", col=gray(0.5))
lines((fishpred$fit-1.96*fishpred$se.fit) ~ newData$prod, type="l", col=gray(0.5))







##################################################
### Non-Metric Multidimentional Scaling (NMDS) ###

# performing NMDS on binary
#nmdsmam <- metaMDS(fishmatrix, k=4, trymax=300) # In two dimensions
#nmdsmam # stress 0.165
#plot(nmdsmam)

#ordiplot(nmdsmam,type="n")
#orditorp(nmdsmam,display="species",cex=0.6, col="red",air=0.1)
#orditorp(nmdsmam, display="sites",cex=0.3,col="black", air=0.1, label=T)

# protected areas
#ordihull(nmdsmam, groups=PA, cex=1.7, label=T)
#ordiellipse(nmdsmam, groups=PA, cex=1.7, label=T)

# states
#plot(nmdsmam)
#ordihull(nmdsmam, groups=state, cex=1.7, label=F)
#ordiellipse(nmdsmam, groups=state, cex=1.7, label=F)



#############################################
### Principal Coordinates Analysis (PCoA) ###

first <- decostand(fishmatrix, method="hellinger")
drel <- dist(first)

pcoarel <- cmdscale(drel)
ordiplot(pcoarel)

ordiellipse(pcoarel, groups=Ocean, cex=1.7, label=T)
ordihull(pcoarel1, groups=MPA, cex=1.7, label=T)




#### Mapping and Fitting environmental variables ####

# FITTING
fit <- envfit(pcoarel, cbind(rich=rich,
                             prod=prod,
                             temp=temp,
                             coast=coast,
                             lat=lat,
                             long=long,
                             Ocean=Ocean,
                             
                             perm=9999, na.rm=T))
fit
ordiplot(pcoarel)
plot(fit, p.=1, cex=0.7)







########################################################################
########################################################################
# Multivariate homogeneity of group dispersion to explore beta diversity
# first generate dissimilarity matrix
dissmamatrix <- vegdist(fishmatrix, method="bray")

# partition acroding to segment
mamdisp <- betadisper(dissmamatrix, group=PA, bias.adjust=T)
plot(mamdisp)

boxplot(mamdisp, xlab="PA")

# Tukey-Kramer honest significance difference
names(mamdisp)
y <- mamdisp$distances # define vector with distance to centroid
boxplot(y ~ PA) # same as boxplot(disp), it's magic!
pw <- pairw.anova(y, x=PA, method="tukey")
plot.pairw(pw, type = 1, las=3, ylab="Distance to centroid")






###########################################################################
############ BETA DIVERSITY PARTITIONING ##################################
###########################################################################

bas <- beta.div.comp(fishmatrix, "BS")
bas

repl <- as.vector(bas$repl)
rich <- as.vector(bas$rich)
diss <- as.vector(bas$D)
sim <- 1-diss

triangle.plot(
  as.data.frame(cbind(rich, sim, repl)),
  show = FALSE,
  labeltriangle = FALSE,
  addmean = TRUE,
  cpoint=1
)

text(-0.7, 0.5, "RichDiff", cex = 1.5)
text(0.4, 0.5, "Repl", cex = 1.5)
text(0, -0.6, "Sorensen similarity", cex = 1.5)

###################################################
# NOW will try using batapart package from Baselga!
# Now, we'll calculate the Sorensen index and its partitions of turnover and nestedness. 
# We can calculate Jaccard index instead by using the argument index.family="jaccard".

dist <- beta.pair(fishmatrix, index.family="sorensen")

# Yields 3 distance matrices accounting for the spatial turnover and the nestedness components 
# To get the turnover partition between communities, type: dist[[1]].
# To get nestedness partition, type: dist[[2]]. 
# To get all beta diversity: dist[[3]].

# If we want to compare the beta diversities of communities aggregated by the treatments
# we can use "betadisper" analysis.

# TOTAL BETADIVERSITY
bd_total <- betadisper(dist[[3]], PA) # Note this is all beta diversity.
plot(bd_total)
y_total <- bd_total$distances # define vector with distance to centroid

pwtotal <- pairw.anova(y_total, x=PA, method="tukey")
plot.pairw(pwtotal, type = 1, las=3, ylab="Distance to centroid")


# REPLACEMENT component
bd_repl <- betadisper(dist[[1]], PA) # Note this is all beta diversity.
plot(bd_repl)
y_repl <- bd_repl$distances # define vector with distance to centroid
boxplot(y_repl ~ PA) # same as boxplot(disp), it's magic!
pwrepl <- pairw.anova(y_repl, x=PA, method="tukey")
plot.pairw(pwrepl, type = 1, las=3, ylab="Distance to centroid")


# NESTEDNESS component
bd_nes <- betadisper(dist[[2]], PA, type = "centroid") # Note this is all beta diversity.
plot(bd_nes)
y_nes <- bd_nes$distances # define vector with distance to centroid
boxplot(y_nes ~ River) # same as boxplot(disp), it's magic!
pwnes <- pairw.anova(y_nes, x=PA, method="tukey")
plot.pairw(pwnes, type = 1, las=3, ylab="Distance to centroid")






######################################################################################
# REPLACEMENT RULES
repl.dbrda <- dbrda(dist[[2]] ~ prod+temp+coast+lat+long, distance="bray")
anova(repl.dbrda) # overall significance of the analysis
anova(repl.dbrda, by="margin", permu=200) ## test for sig. environ. variables
RsquareAdj(repl.dbrda)
plot(repl.dbrda)
########################################################################################




#############################################
### Ordinations with turnover!!! ############
pcoarel1 <- cmdscale(dist[[1]], eig = TRUE)
ordiplot(pcoarel1)





#### Mapping and Fitting environmental variables ####

fit <- envfit(pcoarel, cbind(rich=rich,
                             prod=prod,
                             temp=temp,
                             coast=coast,
                             lat=lat,
                             long=long,
                             Ocean=Ocean,
                             
                             perm=9999, na.rm=T))
fit
ordiplot(pcoarel)
plot(fit, p.=1, cex=0.7)

modperm <- adonis(dist[[1]]~ag25k+fc25k+pa25k+dcity+lat+long)
modperm
#######################################################################




#############################################
### Ordinations with nestedness!!! ############
pcoarel2 <- cmdscale(dist[[2]])
ordiplot(pcoarel2)

#### Mapping and Fitting environmental variables ####

# FITTING
fit <- envfit(pcoarel2, cbind(richness=richness,
                              lat=lat, long=long,
                              fc25k=fc25k,
                              pa25k=pa25k,
                              ag25k=ag25k,
                              elev25k=elev25k,
                              dcity=dcity),
              perm=9999, na.rm=T)
fit
plot(fit, p.=1, cex=0.7)



# distance decay functions
###################################
require(spatstat)

distances <- pairdist(long, lat)
dim(distances)

dissim <- as.matrix(dist[[1]])
dim(dissim)
sim <- 1-dissim

decay <- decay.model(y=sim, x=distances, model.type="exp", y.type="similarities", perm=100)

plot(sim~distances)
plot(decay)


######################################################
# Milton asked for scatterplots between beta diversity
# and differences between covariates

library(ecodist) # to perform mantel test
# and other spatial analyses

# beta diversity
beta_sor <- dist[[1]] # total dissimilarity
beta_sim <- dist[[1]] # turnover
beta_nes <- dist[[2]] # nestedness

# covariates
pa25k_diff <- vegdist(pa25k, method="euclidean", binary=F)
pa25k_diff <- as.dist(pa25k_diff)

fc25k_diff <- vegdist(fc25k, method="euclidean", binary=F)
fc25k_diff <- as.dist(fc25k_diff)

ag25k_diff <- vegdist(ag25k, method="euclidean", binary=F)
ag25k_diff <- as.dist(ag25k_diff)

dcity_diff <- vegdist(dcity, method="euclidean", binary=F)
dcity_diff <- as.dist(dcity_diff)

Eff <- vegdist(effort, method="euclidean", binary=F)
Eff <- as.dist(Eff)

distances <- pairdist(long, lat)
distances <- as.dist(distances)


# scatterplots
# turnover = beta_sim
plot(beta_sim~pa25k_diff, ylab="turnover", xlab="pa diff.", main="mantelr -0.25")
mantel(beta_sim~pa25k_diff)

plot(beta_sim~Eff, ylab="turnover", xlab="effort diff.", main="mantelr -0.30")
mantel(beta_sim~Eff)



# scatterplots
# nestedness = beta_nes
plot(beta_nes~pa25k_diff, ylab="nestedness", xlab="pa diff.", main="mantelr 0.22")
mantel(beta_nes~pa25k_diff)

plot(mgram(as.dist(dissim), as.dist(pa25k_diff), nclass=8))

dd <- dist.decay(gradient=fc25k, counts=mamatrix, coords=cbind(long,lat), nboots=1000, dis.fun = "vegdist", method = "bray", like.pairs=T)

x <- vegdist(fc25k, method = "euclidean")
y <- 1-vegdist(os[, -1:-7], method = "bray")
plot(x, y)
lines(dd$Predictions[, "x"], dd$Predictions[,"mean"], col="red", lwd=2)

