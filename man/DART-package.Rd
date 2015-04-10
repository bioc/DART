\name{DART-package}
\alias{DART}
\docType{package}
\title{
Denoising Algorithm based on Relevance network Topology
}
\description{
Denoising Algorithm based on Relevance network Topology (DART) is an unsupervised algorithm which evaluates the consistency of model pathway/molecular signatures in independent molecular samples before estimating the activation status of the signature in these independent samples. DART was devised for application to cancer genomics problems. For instance, one may wish to infer the activation status of an in-vitro derived oncogenic perturbation gene expression signature in a primary tumour for which genome-wide expression data is available. Before estimating pathway activity in the tumour, DART will evaluate if the "model" in-vitro signature pattern of up and down regulation is consistent with the expression variation seen across an expression panel of primary tumours. If the consistency score is statistically significant, this justfies using the perturbation signature to infer the activation status of the oncogenic pathway in the independent tumour samples. However, in this case, DART will also prune/denoise the perturbation signature using a relevance network topology strategy. This denoising step implemented in DART has been shown to improve estimates of pathway activity in clinical tumour specimens. Other examples of model pathway signatures could be a pathway model of signal transduction, a curated list of genes predicted to be up or down regulated in response to pathway activation/inhibition, or predicted upregulated targets of a transcription factor from say ChIP-Chip/Seq experiments. Three internal functions implement the steps in DART and are provided as explicit functions to allow user flexibility.

\code{DoDARTCLQ} infers perturbation/pathway activity over a smaller and
more compact subnetwork compared to \code{DoDART}. Specifically, whereas
\code{DoDART} uses the whole pruned correlation network,
\code{DoDARTCLQ} infers all maximal cliques within the pruned
correlation network and then estimates activity using only genes in the
subnetwork obtained by the union of these maximal cliques.

\code{DoDART} and \code{DoDARTCLQ} are the main user functions which will automatically and sequentially run through the following internal functions:

(1) \code{BuildRN:} This function builds a relevance correlation network
of the model pathway signature genes in the data set in which the
pathway activity estimate is desired. Note that this step is totally
unsupervised and does not use any sample phenotype information. 

(2) \code{EvalConsNet:} This function evaluates the consistency of the
inferred relevance network with the prior information of the model pathway signature. The up/down regulatory pattern given by the model signature implies predictions about the directionality of the gene-gene correlations in the independent data set. For instance, if  gene "A" is upregulated and gene "B" is downregulated, then assuming that the model signature has any relevance in the independent data set, we would expect genes "A" and "B" to be anti-correlated. Thus, a consistency score can be computed. Only if the consistency score is higher than the score expected by random chance is it recommended that the model signature be used to infer pathway activity. 

(3) \code{PruneNet:} This function constructs the pruned, i.e consistent, network, in which any edge represents a significant correlation in gene expression whose directionality agrees with that predicted by the prior information. This is the denoising step of the algorithm. The function returns the whole pruned network and its maximally connected component.

(4) \code{PredActScore:} Given the adjacency matrix of the maximally
connected consistent (pruned) subnetwork and given the regulatory
weights of the corresponding model pathway signature, this function
estimates a pathway activation score in a given sample. This function
can also be used to infer pathway activity in samples from another
independent data set using the original inferred subnetwork.
}

\author{
Andrew E Teschendorff
}

\references{
{Jiao Y, Lawler K, Patel GS, Purushotham A, Jones AF, Grigoriadis A, Ng T, Teschendorff AE. (2011)  Denoising algorithm based on relevance network topology improves molecular pathway activity inference. BMC Bioinformatics 12:403.}

{Teschendorff AE, Gomez S, Arenas A, El-Ashry D, Schmidt M, et al. (2010) Improved prognostic classification of breast cancer defined by antagonistic activation patterns of immune response pathway modules. BMC Cancer 10:604.}

{Teschendorff AE, Li L, Yang Z. (2015) Denoising perturbation signatures reveals an actionable AKT-signaling gene module underlying a poor clinical outcome in endocrine treated ER+ breast cancer. Genome Biology 16:61.}
}


\keyword{pathway}
\keyword{gene expression}

\examples{

### Example
### load in example data:
data(dataDART);
### dataDART$data: mRNA expression data of 67 ER negative breast cancer samples.
### dataDART$pheno: 51 basals and 16 HER2+ (ERBB2+).
### dataDART$sign: perturbation signature of ERBB2 activation.

### Build Relevance Network
rn.o <- BuildRN(dataDART$data,dataDART$sign,fdr=0.000001);
### Evaluate Consistency
evalNet.o <- EvalConsNet(rn.o);
print(evalNet.o$netcons);
### The consistency score, i.e fraction of consistent edges is 0.94
### P-value is significant, so proceed:
### Prune i.e denoise the network
prNet.o <- PruneNet(evalNet.o);
### print dimension of the maximally connected pruned network
print(dim(prNet.o$pradjMC));
### infer signature activation in the original data set
pred.o <- PredActScore(prNet.o,dataDART$data);
### check that activation is higher in HER2+ compared to basals
boxplot(pred.o$score ~ dataDART$pheno);
pv <- wilcox.test(pred.o$score ~ dataDART$pheno)$p.value;
text(x=1.5,y=3.8,labels=paste("P=",pv,sep=""));

}
