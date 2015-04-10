### R code from vignette source 'DART.Rnw'

###################################################
### code chunk number 1: setup
###################################################
library(DART)


###################################################
### code chunk number 2: loadBCdata
###################################################
library(Biobase)
library(breastCancerVDX) # Wang et al./Minn et al.
library(breastCancerMAINZ)

data(vdx)
data(mainz)


###################################################
### code chunk number 3: loadModelSig
###################################################
data(dataDART)
modelSig <- dataDART$sign


###################################################
### code chunk number 4: remapData
###################################################

# Order vdx by std dev across samples, decreasing
vdx.ord <- vdx[order(  apply(exprs(vdx), 1, sd,na.rm=T), decreasing=T ), ]
# Reduce vdx to retain a single (most variable) probeset per Entrez Gene ID
vdx.EntrezUniq <- vdx.ord[
	match(unique(fData(vdx.ord)$EntrezGene.ID), fData(vdx.ord)$EntrezGene.ID),]
# Get vdx data, labelled by Entrez Gene IDs
vdx.data <- exprs(vdx.EntrezUniq)
rownames(vdx.data) <- fData(vdx.EntrezUniq)$EntrezGene.ID[
	match(rownames(vdx.data), rownames(fData(vdx.EntrezUniq))) ]
colnames(vdx.data) <- sub('.CEL.gz$','',pData(vdx.EntrezUniq)$filename)
#
# Do the same for the Mainz data set
mainz.ord <- mainz[order(  apply(exprs(mainz), 1, sd,na.rm=T), decreasing=T ), ]
mainz.EntrezUniq <- mainz.ord[
	match(unique(fData(mainz.ord)$EntrezGene.ID),fData(mainz.ord)$EntrezGene.ID),]
mainz.data <- exprs(mainz.EntrezUniq)
rownames(mainz.data) <- fData(mainz.EntrezUniq)$EntrezGene.ID[
	match(rownames(mainz.data),rownames(fData(mainz.EntrezUniq))) ]
colnames(mainz.data) <- sub('.CEL.gz$','',pData(mainz.EntrezUniq)$filename)
#
# Inspect the gene expression matrices
dim(vdx.data)
dim(mainz.data)


###################################################
### code chunk number 5: buildRN
###################################################
rn.o <- BuildRN(vdx.data, modelSig, fdr=0.000001)

# How many nodes in the relevance network?
print(dim(rn.o$adj))
# How many edges in the relevance network? Look at the adjacency matrix.
print(sum(rn.o$adj == 1)/2)

# The nodes in the relevance network are the genes in the model signature
# which were found in the data matrix
print(length(rn.o$rep.idx))


###################################################
### code chunk number 6: buildRNfdrReduced
###################################################
rn.smallerFDR.o <- BuildRN(vdx.data, modelSig, fdr=0.0000001)
print(sum(rn.smallerFDR.o$adj == 1)/2) 


###################################################
### code chunk number 7: rnConsistency
###################################################
### Evaluate Consistency
evalNet.o <- EvalConsNet(rn.o);

print(evalNet.o$netcons['fconsE'])
### The consistency score (i.e fraction of consistent edges) is 0.72.

print(evalNet.o$netcons)
### The p-value of the consistency score is significant, so proceed.
### Note that P-values may appear as zero because of the finite number
### of randomisations performed.


###################################################
### code chunk number 8: rnPrune
###################################################
### Prune (i.e. denoise) the network
prNet.o <- PruneNet(evalNet.o)

### Print dimension of the maximally connected pruned network
print(dim(prNet.o$pradjMC))

### Print number of edges in maximally connected pruned network
print(sum(prNet.o$pradjMC)/2)


###################################################
### code chunk number 9: activityScore
###################################################
### Infer signature activation in the original data set
pred.o <- PredActScore(prNet.o, vdx.data)


###################################################
### code chunk number 10: phenoSubtype
###################################################
### Check that activation is higher in HER2+ compared to basals
pred.o.score.report <- pred.o$score[match(names(dataDART$pheno),names(pred.o$score))]

boxplot(pred.o.score.report ~ dataDART$pheno, ylab='activity score: ERBB2 perturb sig')
pv <- wilcox.test(pred.o.score.report ~ dataDART$pheno)$p.value
title(main=paste("DoDART - Wang data set: P=",signif(pv,2),sep=""))


###################################################
### code chunk number 11: inferIndept
###################################################
pred.mainz.o <- PredActScore(prNet.o, mainz.data)

# Check that activation is higher in HER2+ compared to basals
# in the smaller Mainz data set, after training on the larger Wang/Minn data set

pred.mainz.o.score.report <- pred.mainz.o$score[
	match(names(dataDART$phenoMAINZ),names(pred.mainz.o$score))]

boxplot(pred.mainz.o.score.report ~ dataDART$phenoMAINZ,
	ylab='activity score: ERBB2 perturb sig')
pv <- wilcox.test(pred.mainz.o.score.report ~ dataDART$phenoMAINZ)$p.value
title(main=paste("DoDART - Mainz data set: P=",signif(pv,2),sep=""))


###################################################
### code chunk number 12: wrapper
###################################################
res.vdx <- DoDART(vdx.data, modelSig, fdr=0.000001)

# View the activity scores for the first five Wang/Minn samples 
print(res.vdx$score[1:5])


###################################################
### code chunk number 13: wrapper2-1
###################################################
res2.vdx <- DoDARTCLQ(vdx.data, modelSig, fdr=0.000001)

# View the activity scores generated by DoDARTCLQ for the first five Wang/Minn samples
print(res2.vdx$pred[1:5])

### Check that activation generated by DoDARTCLQ is higher in HER2+ compared to basals
pred.o.score.report2 <- res2.vdx$pred[match(names(dataDART$pheno),names(pred.o$score))]

boxplot(pred.o.score.report2 ~ dataDART$pheno, ylab='activity score: ERBB2 perturb sig')
pv <- wilcox.test(pred.o.score.report ~ dataDART$pheno)$p.value
title(main=paste("DoDARTCLQ - Wang data set: P=",signif(pv,2),sep=""))

###################################################
### code chunk number 14: wrapper2-2
###################################################
# Check that activation is higher in HER2+ compared to basals
# in the smaller Mainz data set, after training on the larger Wang/Minn data 
# set by DoDARTCLQ

pred.mainz2.o <- PredActScore(res2.vdx$clq, mainz.data)

pred.mainz.o.score.report2 <- pred.mainz2.o$score[
    match(names(dataDART$phenoMAINZ),names(pred.mainz.o$score))]

boxplot(pred.mainz.o.score.report2 ~ dataDART$phenoMAINZ,
        ylab='activity score: ERBB2 perturb sig')
pv <- wilcox.test(pred.mainz.o.score.report ~ dataDART$phenoMAINZ)$p.value
title(main=paste("DoDARTCLQ - Mainz data set: P=",signif(pv,2),sep=""))

###################################################
### code chunk number 15: sessionInfo
###################################################
print(sessionInfo())


