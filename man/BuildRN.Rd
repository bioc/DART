\name{BuildRN}
\alias{BuildRN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Builds the relevance correlation network
}
\description{This function builds the relevance correlation network for the genes in the model pathway signature in the given data set.}
\usage{
BuildRN(data.m, sign.v, fdr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{data.m}{Data matrix (numeric). Rows label features (genes),
  columns label samples. It is assumed that the number of features is
  much larger than the number of samples. Rownames must be a valid gene or probe identifier.}
  \item{sign.v}{Model pathway signature vector (numeric). Elements correspond to the regulatory weights, i.e the sign indicates if up or downregulated. Names of sign.v must be a gene name (probe) identifier which must match the gene (probe) identifier used for the rows of \code{data.m}.}
  \item{fdr}{Desired false discovery rate (numeric) which determines the
  allowed number of false positives in the relevance network. The
  default value is 0.000001. Since typically model signatures may
  contain on the order of 100 genes, this amounts to constructing a
  relevance network on the order of 10000 edges (pairwise correlations).
  Using a Bonferroni correction, this leads to a P-value threshold of
  approx. 1e-6 to 1e-7. This parameter is tunable, so choosing a number
  of different thresholds may be advisable.}
}

\value{
  A list with following entries:
  \item{adj}{Adjacency matrix of inferred relevance network}
  \item{s}{Model signature vector in data}
  \item{sd}{Gene signature data matrix}
  \item{c}{Correlations between signature genes}
  \item{d}{Data matrix}
  \item{rep.idx}{Indices of the genes in signature which could be found in data matrix}

}
\references{{Jiao Y, Lawler K, Patel GS, Purushotham A, Jones AF, Grigoriadis A, Ng T, Teschendorff AE. (2011)  Denoising algorithm based on relevance network topology improves molecular pathway activity inference. BMC Bioinformatics 12:403.}

{Teschendorff AE, Gomez S, Arenas A, El-Ashry D, Schmidt M, et al. (2010) Improved prognostic classification of breast cancer defined by antagonistic activation patterns of immune response pathway modules. BMC Cancer 10:604.}


}
\author{Andrew E Teschendorff, Yan Jiao}



\examples{

data(dataDART)
rn.o <- BuildRN(dataDART$data, dataDART$sign, fdr=0.000001)
## See ?DoDART and vignette('DART') for further examples.
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
