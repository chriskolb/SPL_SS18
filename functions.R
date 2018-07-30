#### Function for ggsurvplot_combine #####

# store survplot object and choose functional argument (default is Survival Function) 

nonparametricKurves <- function(x,fun=NULL) {
  z <- ggsurvplot_combine(kmfh.all, data=dat, conf.int=T,
                          legend.labs=c("KM", "Fleming-Harrington"), legend.title="Model",  
                          fun=fun,
                          risk.table=F,
                          cumcensor=FALSE,
                          censor=FALSE,
                          linetype=c(1,1),
                          size = 0.3)
}

#### Function for Kaplan Meier Curves by Strata ####

### Storing survObject, labs= category description, Legend Title
kmGroupKurves <- function(x,labs,title,line=c(1,1),conf=T){
  x <- ggsurvplot(wide.fit, conf.int=conf,
                  legend.labs=labs, legend.title=title,  
                  censor=F,
                  palette = "strata",
                  risk.table = T,
                  pval=TRUE,
                  risk.table.height=.25,
                  ylim=c(0,1),
                  xlim=c(0,30),
                  surv.median.line="hv",
                  linetype=line,
                  size = 0.5)
  print(x)
}

