#' Convert a phylo object to an inverse distance matrix that MCMCglmm understands
#'
#' @param tree tree of class phylo
#' @param MPL logical, should the function compute the mean path length?
#' @param inverseA logical, should the inverse matrix be computed?
#' @keywords MCMCglmm, ape, phylo, tree
#' @importFrom ape chronoMPL
#' @importFrom phytools force.ultrametric
#' @importFrom MCMCglmm inverseA
#' @export
#' @examples
#' tree <- ape::read.tree("./tree.newick") # newick format
#' ultm(tree, MPL = FALSE, inverseA = TRUE)

ultm<-function(tree, MPL = FALSE, inverseA = TRUE){
  
  if(attributes(tree)$class != "phylo"){
    stop("Tree object must be of class phylo")
  }
  
  if(MPL == TRUE){
    MPL <- chronoMPL(tree)
  } else MPL <- tree
  
  ult <- force.ultrametric(MPL)
  # remove zeroes in the distance matrix
  for(i in 1:length(ult$edge.length)){
    if(ult$edge.length[i] < 1e-16){
      ult$edge.length[i] <- 1e-10
    }
  }
  if(inverseA == TRUE){
    inverseult <- inverseA(ult, nodes = "ALL", scale = TRUE)
    Ainv <- as(as.matrix(inverseult$Ainv), "dgCMatrix")
    return(Ainv)
  } 
  return(ult)
}