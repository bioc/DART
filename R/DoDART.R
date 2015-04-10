DoDART <-
function(data.m,sign.v,fdr=0.000001){

  rn.o <- BuildRN(data.m,sign.v,fdr);
  evalNet.o <- EvalConsNet(rn.o);
  if ( evalNet.o[[1]][5] < 0.001 ) { ## see if P-vaule for consistency is significant or not; 0.001 were choosen for 1000 permutations
      prnet.o <- PruneNet(evalNet.o);
      pred.o <- PredActScore(prnet.o,data.m);   
      return(list(netcons=prnet.o$netcons,adj=pred.o$adj,sign=pred.o$sign,score=pred.o$score,degree=pred.o$degree,Consistency_result.o=evalNet.o));
  } else {
      print("Warning: Module not consistent!");
      prnet.o <- PruneNet(evalNet.o);
      pred.o <- PredActScore(prnet.o,data.m);   
      return(list(netcons=prnet.o$netcons,adj=pred.o$adj,sign=pred.o$sign,score=pred.o$score,degree=pred.o$degree,Consistency_result.o=evalNet.o));
  }
  
} ### END of FUNCTION

