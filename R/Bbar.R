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

  if(class(phylo) != "phylo" || is.ultrametric(phylo)){
    stop("Object must be of class phylo and not ultrametric")
  }
  
  type <- match.arg(type)
  varcov <- vcv(phylo)
  n <- length(phylo$tip.label)
  
  if(type == "All"){
    dbar <- mean(diag(varcov))
    obar <- (sum(varcov)-sum(diag(varcov)))/(n^2-n)
    Bbar <- 2*dbar + 4*obar
    return(Bbar)
  }
  
  submats <- list()
  for(name in gsub(pattern = " .*", replacement = "", colnames(varcov))){
    submats[[name]] <- varcov[grepl(pattern = name, x = rownames(varcov)),grepl(pattern = name, x = colnames(varcov))]
  }
  
  B <- matrix(sapply(X = submats, FUN = function(x) mean(diag(x))), dimnames = list(names(submats), "dbar"))
  B <- cbind(B, matrix(sapply(X = submats, FUN = function(x){
    n <- dim(x)[1]  
    sum(x)-sum(diag(x))/(n^2-n)
  }), dimnames = list(names(submats), "obar")))
  
  if(type == "Genus"){
    BbarG <- (2*sum(B[,"dbar"]) + 2*sum(B[,"obar"]))/dim(B)[1] # TODO are the scalar multipliers correct here?
    return(BbarG)
  }
  if(type == "Species"){
    n <- matrix(sapply(X = submats, FUN = function(x) dim(x)[1]), dimnames = list(names(submats), "n"))
    nc <-  matrix(sapply(X = submats, FUN = function(x) (dim(x)[1]^2-dim(x)[1])/2), dimnames = list(names(submats), "nc"))
    B <- cbind(B, n, nc)
    BbarS <- (2*sum(B[,"dbar"], B[,"n"]) + 2*sum(B[,"obar"], B[,"nc"]))/sum(B[,"nc"]) # TODO are the scalar multipliers correct here?
    return(BbarS)
  }
}

