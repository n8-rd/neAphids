#load packages
library(phylopath)
library(ape)

#load tree
tree <- read.tree("Genus_tree_aUCE_timed.tre")
#load dataset
dat <- read.csv("aphid_genus_predictors.csv")
#inspect dataset
head(dat)
#assign rownames as genus names
rownames(dat) <- dat$Genus
#inspect tree labels
head(tree$tip.label)
#make tree genus names lowercase to match dataset
tree$tip.label <- tolower(tree$tip.label)
#remove rows in dataset and branches in tree that are not mutual
tree<-drop.tip(tree,tree$tip.label[which(tree$tip.label%in%dat$Genus==FALSE)])
check.species<-function(x) {any(x==tree$tip.label)}
dat <- dat[sapply(dat[,"Genus"],check.species),]
names(dat)

#env only model

#define model set. Response variable is written first, '~' represents the correlation between
#reponse and predictor variable, predictor variable is written last. Multiple predictor
#variables per response variable can be included.
models.1 <- define_model_set(
  b = c(Genus_diversity ~ Host_use_breadth,
        Host_use_breadth ~ Environmental_PC1 + Environmental_PC2 + Environmental_PC3)
  )
#run model
result.1 <- phylo_path(models.1, data = dat, tree= tree)
#examine model results. A p-value < 0.05 indicates there are correlations 
#between variables not included in the model. cCIC score determines model fit.
(s.1 <- summary(result.1))
#look at correlations between variables not included in model
(res <- result.1$d_sep$b)
#look at correlations between variables included in model. Low results for correlations
#may indicate removing the correlation might improve the model.
(b <- best(result.1))
#calculate the total effect of variables
gh <- b[4,5]
e3e <- b[1,4]*gh
e2e <- b[2,4]*gh
e1e <- b[3,4]*gh
env <- e1e + e2e + e3e
dat$epc <- e1e*dat$Environmental_PC1 + e2e*dat$Environmental_PC2 + e3e*dat$Environmental_PC3


#examine alternative models
models.2 <- define_model_set(
  b = c(Genus_diversity ~ Host_use_breadth + Environmental_PC1 + Environmental_PC2 + Environmental_PC3)
)

result.2 <- phylo_path(models.2, data = dat, tree= tree)
(s.2 <- summary(result.2))
(res <- result.2$d_sep$b)
(b <- best(result.2))

models.3 <- define_model_set(
  b = c(Genus_diversity ~ Environmental_PC1 + Environmental_PC2 + Environmental_PC3,
        Environmental_PC1 ~ Host_use_breadth,
        Environmental_PC2 ~ Host_use_breadth,
        Environmental_PC3 ~ Host_use_breadth)
)

result.3 <- phylo_path(models.3, data = dat, tree= tree)
(s.3 <- summary(result.3))
(res <- result.3$d_sep$b)
(b <- best(result.3))
