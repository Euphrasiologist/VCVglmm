#' Performing Wald tests on MCMCglmm fixed effects automatically
#'
#' Performs a joint Wald test (see \code{\link{Wald.test}}) for every term of the fixed effect
#' formula, e.g. all levels of a factor together.
#'
#' @param mod model of class MCMCglmm
#' @keywords MCMCglmm, fixed effects, p-values, Wald test
#' @export
#' @examples
#' \donttest{
#' library(MCMCglmm)
#' d <- data.frame(x = factor(rep(letters[1:4], 50)), z = rnorm(200))
#' d$y <- as.numeric(d$x) + rnorm(200)
#' mod <- MCMCglmm(y ~ x + z, data = d, verbose = FALSE)
#' Wald.test.auto(mod = mod)
#' }

Wald.test.auto <- function(mod){

  if(!inherits(mod, "MCMCglmm")){
    stop("Object not of class MCMCglmm")
  }

  tt <- stats::terms(mod$Fixed$formula)
  covars <- attr(tt, "term.labels")
  vars <- all.vars(stats::delete.response(tt))
  cn <- colnames(mod$Sol)[seq_len(mod$Fixed$nfl)]

  # assign each coefficient to a term: split on ":" and match each piece to the
  # longest variable name it starts with (so that x2 isn't attributed to x)
  owner <- function(piece){
    hit <- vars[startsWith(piece, vars)]
    if(length(hit)) hit[which.max(nchar(hit))] else NA
  }
  coef.term <- vapply(cn, function(nm){
    if(nm == "(Intercept)") return(NA_character_)
    v <- vapply(strsplit(nm, ":", fixed = TRUE)[[1]], owner, character(1))
    if(anyNA(v)) return(NA_character_)
    # find the term with exactly these variables
    m <- covars[vapply(covars, function(tl){
      tv <- vapply(strsplit(tl, ":", fixed = TRUE)[[1]], function(p) all.vars(parse(text = p)), character(1))
      setequal(tv, v) && length(tv) == length(v)
    }, logical(1))]
    if(length(m)) m[1] else NA_character_
  }, character(1))

  res <- list()
  for(i in seq_along(covars)){
    fixed <- which(coef.term == covars[i])
    if(length(fixed) == 0){
      warning("Could not find the coefficients of term ", covars[i])
      next
    }
    res[[covars[i]]] <- .wald(mod$Sol[, fixed, drop = FALSE])
  }

  datres <- data.frame(res, check.names = FALSE)
  class(datres) <- c("MCMCglmmWT", "data.frame")
  datres
}

#' @export

print.MCMCglmmWT <- function(x, signif = TRUE, ...){
  cat("Wald test of the joint effect of fixed covariates:\n\n")
  tx <- t(x)
  tx <- as.data.frame(tx)
  if(!signif){
    print(tx)
  } else {
    asterisks <- function(P) { # thanks https://gist.github.com/ewancarr/6674998
      if (P <= 0.001) {
        return("***")
      }
      if (P <= 0.01) {
        return(" **")
      }		
      if (P <= 0.05) {
        return("  *")
      }
      else {return("   ")
      }
    }
    
    stars <- vector()
    for(i in 1:dim(tx)[1]){
      stars[i] <- asterisks(tx[i,3])
    }
    tx$` ` <- stars
    print(tx)
  }
  
}
