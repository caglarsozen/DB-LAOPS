# Run from the repository root.
run_component_study <- function(root=getwd()) {
 root<-normalizePath(root);rs<-file.path(R.home('bin'),'Rscript')
 if(!file.exists(rs))rs<-paste0(rs,'.exe')
 script<-file.path(root,'component_study','RUN_CONTRIBUTION.R')
 if(!file.exists(script))stop('Run this entry point from the repository root.')
 status<-system2(rs,shQuote(script))
 if(status!=0L)stop('Component study failed; inspect component_study/logs.')
 invisible(status)
}
run_component_study()
