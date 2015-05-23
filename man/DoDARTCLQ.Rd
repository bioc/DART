\name{DoDARTCLQ}
\alias{DoDARTCLQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Improved edition of DoDART
}
\description{
This function implements an improved version of DART. \code{DoDARTCLQ}
estimates perturbation/pathway activity using a smaller subnetwork
compared to \code{DoDART}. Specifically, whereas \code{DoDART} uses the whole pruned correlation network, \code{DoDARTCLQ} infers the largest cliques within the pruned correlation network and then estimates activity using only genes within a module obtained by merging the largest cliques together. Although the largest cliques may not be unique, we found that they generally exhibited very strong overlaps, justfying their merging, and resulting in approximate clique modules typically in the size of ~10 - 100 genes.

Given a data matrix and a model (pathway) signature it will construct the relevance correlation network for the genes in the signature over the data, evaluate the consistency of the correlative patterns with those predicted by the model signature, filter out the noise and finally obtain estimates of pathway activity for each individual sample. Specifically, it will call and run the following functions:

(1) \code{BuildRN:} This function builds a relevance correlation network
of the model pathway signature in the data set in which the pathway
activity estimate is desired. We point that this step is totally
unsupervised and does not use any sample phenotype information. 

(2) \code{EvalConsNet:} This function evaluates the consistency of the inferred network with the prior information of the model pathway signature. The up/down regulatory pattern given by the model signature implies predictions about the directionality of the gene-gene correlations in the independent data set. For instance, if  gene "A" is upregulated and gene "B" is downregulated, then assuming that the model signature has any relevance in the independent data set, we would expect genes "A" and "B" to be anti-correlated. Thus, a consistency score can be computed. Only if the consistency score is higher than the score expected by random chance is it recommended that the model signature be used to infer pathway activity. 

(3) \code{PruneNet:} This function obtains the pruned, i.e consistent, network, in which any edge represents a significant correlation in gene expression whose directionality agrees with that predicted by the prior information. This is the denoising step of the algorithm. The function returns the whole pruned network and its maximally connected component.

(4) \code{PredActScore:} Given the adjacency matrix of the maximally connected consistent subnetwork and given the regulatory weights of the corresponding model pathway signature, this function estimates a pathway activation score in each sample. This function can also be used to infer pathway activity in another independent data set using the inferred subnetwork.

Before performing the pruning step, \code{DoDARTCLQ} will check whether
the relevance correlation network is significantly consistent with the
predictions from the model signature. Significance is assessed by
first computing a consistency score (in effect, the fraction of edges in the
relevance network which are consistent with the model prediction) and
subsequently by 1000 random permutations to obtain an empirical null
distribution for the consistency score. Model signatures whose consistency scores have
empirical P-values less than 0.001 are deemed consistent. If the
consistency score is not significant, the function will issue a warning and it is not recommended to use the signature to predict pathway activity.
}
\usage{
DoDARTCLQ(data.m, sign.v, fdr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data.m}{Training normalized gene expression data matrix with rows labeling genes and columns labeling samples. Rownames of \code{data.m} should be annotated to the same gene identifier as used in names of \code{sign.v}.}
  \item{sign.v}{A perturbation expression signature vector, consisting of 1 and -1, indicating whether gene is significant overexpressed or underexpressed, respectively. names of \code{sign.v} should have a valid gene identifier e.g. entrez gene ID.}
  \item{fdr}{Desired false discovery rate (numeric) which determines the
  allowed number of false positives in the relevance network. The
  default value is 0.000001. Since typically model signatures may
  contain on the order of 100 genes, this amounts to constructing a
  relevance network on the order of 10000 edges (pairwise correlations).
  Using a Bonferroni correction, this leads to a P-value threshold of
  approx. 1e-6 to 1e-7. This parameter is tunable, so choosing a number
  of different thresholds may be advisable.}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  \item{pred}{A vector of predicted perturbation activity scores for all samples in \code{data.m}.}
  \item{clq}{A list containing information about the approximate clique gene module used to compute perturbation activity. List entries: clq$pradjMC: the adjacency matrix of the approx. clique gene module, clq$signMC: the corresponding perturbation signature for the genes making up the approx. clique module, clq$sizes: the sizes of all largest cliques in the pruned correlation network, clq$n: the number of largest cliques in the pruned correlation network.}
  \item{consist}{Module consistency result, output of \code{EvalConsNet}.}
}
\references{
{Jiao Y, Lawler K, Patel GS, Purushotham A, Jones AF, Grigoriadis A, Ng T, Teschendorff AE. (2011)  Denoising algorithm based on relevance network topology improves molecular pathway activity inference. BMC Bioinformatics 12:403.}

{Teschendorff AE, Gomez S, Arenas A, El-Ashry D, Schmidt M, et al. (2010) Improved prognostic classification of breast cancer defined by antagonistic activation patterns of immune response pathway modules. BMC Cancer 10:604.}

{Teschendorff AE, Li L, Yang Z. (2015) Denoising perturbation signatures reveals an actionable AKT-signaling gene module underlying a poor clinical outcome in endocrine treated ER+ breast cancer. Genome Biology 16:61.}
}
\author{
Andrew E Teschendorff
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
  
### Example
### load in example data:
data(dataDART);
### dataDART$data: mRNA expression data of 67 ER negative breast cancer samples.
### dataDART$pheno: 51 basals and 16 HER2+ (ERBB2+).
### dataDART$phenoMAINZ: 24 basals and 8 HER2+ (ERBB2+).
### dataDART$sign: perturbation signature of ERBB2 activation.


### Using DoDARTCLQ
dart.o <- DoDARTCLQ(dataDART$data,dataDART$sign, fdr=0.000001);
### check that activation is higher in HER2+ compared to basals
boxplot(dart.o$pred ~ dataDART$pheno);
pv <- wilcox.test(dart.o$pred ~ dataDART$pheno)$p.value;
text(x=1.5,y=3.5,labels=paste("P=",pv,sep=""));

}

\keyword{clique}
\keyword{network}
