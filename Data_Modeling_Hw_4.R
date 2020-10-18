library(psych)
#5.27
Mouse <- read.csv("MouseBrain.csv")
##Looking at boxplot of relationship between mouse genotypes and number of times mouse come in contact with one another.
with(Mouse, boxplot(Contacts~Genotype))
#Summary statistics in the model between the different genotypes.
with(Mouse, describeBy(Contacts,Genotype))
#Anova of the model
summary(with(Mouse, aov(Contacts~Genotype)))
#5.37
anova(with(Mouse, lm(Contacts~Genotype)))
#5.45
##Looking at the significant differences among groups of genotypes in the dataset when looking at its affect on contact.
TukeyHSD(with(Mouse, aov(Contacts~Genotype)))

#5.28
Ricci <- read.csv("Ricci.csv")

##Graphical representation of seeing difference among race groups on average test scores.
with(Ricci, boxplot(Combine~Race))
##Checking descriptive statistics between race groups to see if conditions for anova are met. 
with(Ricci, describeBy(Combine,Race))
##5.39
##Anova
summary(with(Ricci, aov(Combine~Race)))
##5.47
##Looking at significant differences among groups.
TukeyHSD(with(Ricci, aov(Combine~Race)))

##5.31
Amyloid <- read.csv("Amyloid.csv")
##Descriptive Summary Statistics among groups of interest.
with(Amyloid, describeBy(Abeta,Group))
##Boxplot among groups and abeta.
with(Amyloid, boxplot(Abeta~Group))
##Anova
summary(with(Amyloid, aov(Abeta~Group)))
##Looking at the significant differences among groups of cognitive impairement in the dataset when looking at its affect on Abeta levels.
TukeyHSD(with(Amyloid, aov(Abeta~Group)))
##5.38
Meniscus <- read.csv("Meniscus.csv")
##Boxplot showing different meniscus repair methods and stiffness 
with(Meniscus, boxplot(Stiffness~Method))
##Descriptive summary statistics among the 3 methods of meniscus repairment. 
with(Meniscus, describeBy(Stiffness,Method))
##Anova
summary(with(Meniscus, aov(Stiffness~Method)))
##5.40
##Boxplot showing different mensicus repair methods and amount of displacement. 
with(Meniscus, boxplot(Displacement~Method))
##Descriptive summary statistics. 
with(Meniscus, describeBy(Displacement,Method))
##Anova
summary(with(Meniscus, aov(Displacement~Method)))
##Excluding group 3
clean=subset(Meniscus, Meniscus$Method %in% c("1","2"))
summary(with(clean, aov(Displacement~Method)))

