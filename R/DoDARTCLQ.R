DoDARTCLQ <- function(data.m,sign.v,fdr=0.000001){
    
    rnet.o <- BuildRN(data.m,sign.v,fdr)
    cnet.o <- EvalConsNet(rnet.o)
    if ( cnet.o[[1]][5] < 0.001 ) {     ## see if P-vaule for consistency is significant or not; 0.001 were choosen for 1000 permutations in DART
        prnet.o <- PruneNet(cnet.o);
        gr.o <- graph.adjacency(prnet.o$pradjMC,mode="undirected");
        lclq.o <- largest.cliques(gr.o);
        clq.idx <- unique(unlist(lclq.o));## note that indices may start at 0 depending on igraph version!!!!!
        clqSizes.v <- lapply(lclq.o,length);
        nCLQ <- length(clqSizes.v);
        clq.l <- list(pradjMC=prnet.o$pradjMC[clq.idx,clq.idx],signMC=prnet.o$signMC[clq.idx],sizes=clqSizes.v,n=nCLQ);
        pred.v <- PredActScore(clq.l,data.m)$score;
        return(list(pred=pred.v,clq=clq.l, Consistency_result.o=cnet.o));
    } else {
        print("Warning: Module not consistent!");
        prnet.o <- PruneNet(cnet.o);
        gr.o <- graph.adjacency(prnet.o$pradjMC,mode="undirected");
        lclq.o <- largest.cliques(gr.o);
        clq.idx <- unique(unlist(lclq.o));## note that indices may start at 0 depending on igraph version!!!!!
        clqSizes.v <- lapply(lclq.o,length);
        nCLQ <- length(clqSizes.v);
        clq.l <- list(pradjMC=prnet.o$pradjMC[clq.idx,clq.idx],signMC=prnet.o$signMC[clq.idx],sizes=clqSizes.v,n=nCLQ);
        pred.v <- PredActScore(clq.l,data.m)$score;
        return(list(pred=pred.v,clq=clq.l, Consistency_result.o=cnet.o));
    }
}