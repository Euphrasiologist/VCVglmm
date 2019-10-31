#' Given a tree, return the average root to tip distance between a random pair of taxa
#' 
#' @param phylo object of class phylo
#' @param type at what level calculation should be at, between all species, at the level of genus or any combination within genus
#' @keywords variance, covariance, tree
#' @export
#' @examples
#' # requires ape
#' library(ape)
#' tree <- rtree(100, rooted = TRUE)
#' Bbar(tree, type = "Genus")

Bbar <- function(phylo, type = c("All", "Genus", "Species")){
  stopifnot(class(phylo) == "phylo")
  type <- match.arg(type)
  varcov <- vcv(phylo)
  n <- length(phylo$tip.label)
  
  # this is for all two species pairs in a phylogeny
  if(type == "All"){
    dbar <- mean(diag(varcov))
    obar <- (sum(varcov)-sum(diag(varcov)))/(n^2-n)
    Bbar <- 2*dbar + 4*obar
    return(Bbar)
  }
  # for each genus, match and extract the submatrix into a list
  submats <- list()
  for(name in gsub(pattern = " .*", replacement = "", colnames(varcov))){
    submats[[name]] <- varcov[grepl(pattern = name, x = rownames(varcov)),grepl(pattern = name, x = colnames(varcov))]
  }
  # for species pairs constrained to genus (not accounting for size of genus)
  
  # calculate dbar for each element of the list
  B <- matrix(sapply(X = submats, FUN = function(x) mean(diag(x))), dimnames = list(names(submats), "dbar"))
  # calculate obar for each element of the list
  B <- cbind(B, matrix(sapply(X = submats, FUN = function(x){
    n <- dim(x)[1]  
    sum(x)-sum(diag(x))/(n^2-n)
  }), dimnames = list(names(submats), "obar")))
  if(type == "Genus"){
    BbarG <- (2*sum(B[,"dbar"]) + 2*sum(B[,"obar"]))/dim(B)[1] # TODO are the scalar multipliers correct here?
    return(BbarG)
  }
  if(type == "Species"){
    # number of species per genus
    n <- matrix(sapply(X = submats, FUN = function(x) dim(x)[1]), dimnames = list(names(submats), "n"))
    # number of possible pairwise comparisons per genus
    nc <-  matrix(sapply(X = submats, FUN = function(x) (dim(x)[1]^2-dim(x)[1])/2), dimnames = list(names(submats), "nc"))
    B <- cbind(B, n, nc)
    BbarS <- (2*sum(B[,"dbar"], B[,"n"]) + 2*sum(B[,"obar"], B[,"nc"]))/sum(B[,"nc"]) # TODO are the scalar multipliers correct here?
    return(BbarS)
  }
}
