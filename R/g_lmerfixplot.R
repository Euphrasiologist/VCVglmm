#' ggplot2 ouptut of Tukey comparisons for lmer models 
#' 
#' @param model model of class lmerMod
#' @param factor list of the form list(pairwise ~ your variable of interest)
#' @param reorder should the plot be reordered from high to low effect size?
#' @keywords lme4, fixed effects, plot, tukey
#' @export
#' @examples
#' mod <- lmer(y ~ x + (1 | z))
#' g_lmerfixplot(mod, list(pairwise ~ x), reorder = TRUE)

g_lmerfixplot <- function(model, factor = list(...), reorder = FALSE){
  
  if(class(model)[1] != "glmerMod" | class(model)[1] != "lmerMod"){
    stop("Functionality supported for lme4 models only")
  }
  rsum <-  emmeans(model, factor, adjust = "tukey")[[2]]
  rsum <- as.data.frame(rsum)
  
  rsum$ptext <- rep(NA, dim(rsum)[1])
  
  for(i in 1:dim(rsum)[1]){
    if(rsum$p.value[i] <= 0.05 & rsum$p.value[i] > 0.01){
      rsum$ptext[i] <- "*"
    } else if(rsum$p.value[i] <= 0.01 & rsum$p.value[i] > 0.001){
      rsum$ptext[i] <- "**"
    } else if(rsum$p.value[i] <= 0.001 & rsum$p.value[i] >= 0){
      rsum$ptext[i] <- "***"
    } else rsum$ptext[i] <- NA
  }
  
  rsum$enhance <- !is.na(as.character(rsum$ptext))
  
  if(reorder){
    ggplot(rsum,aes(x = reorder(contrast, -estimate) , y = estimate, alpha = enhance))+
      geom_pointrange(aes(ymin=estimate-2*SE,
                          ymax=estimate+2*SE))+
      geom_text(aes(label = ptext), nudge_x = 0.2) +
      coord_flip()+
      theme_bw()+
      labs(y = "Point estimates and confidence intervals", x = "Variables")+
      theme(legend.position = "none")
  } else
    ggplot(rsum,aes(x = contrast , y = estimate, alpha = enhance))+
    geom_pointrange(aes(ymin=estimate-2*SE,
                        ymax=estimate+2*SE))+
    geom_text(aes(label = ptext), nudge_x = 0.2) +
    coord_flip()+
    theme_bw()+
    labs(y = "Point estimates and confidence intervals", x = "Variables")+
    theme(legend.position = "none")
  
}
