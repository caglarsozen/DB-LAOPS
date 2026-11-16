# Reproducible exploratory contribution study. No journal submission is made.
sourced_file<-tryCatch(sys.frame(1L)$ofile,error=function(e)NULL)
arg0<-grep('^--file=',commandArgs(FALSE),value=TRUE)
study_file<-if(!is.null(sourced_file))sourced_file else if(length(arg0))sub('^--file=','',arg0[1]) else NULL
if(!is.null(study_file))setwd(dirname(normalizePath(study_file)))
root<-normalizePath('.')
cat('DB-LAOPS exploratory contribution study. Historical data were previously seen.\n')
e<-new.env(parent=globalenv());sys.source('engine/DB_LAOPS_v90.R',envir=e)
e$check_packages(install=identical(Sys.getenv('DB_LAOPS_INSTALL'),'TRUE'))
for(d in c('cache','logs','results'))dir.create(d,showWarnings=FALSE)
rs<-file.path(R.home('bin'),'Rscript');if(!file.exists(rs))rs<-paste0(rs,'.exe')
cores<-suppressWarnings(as.integer(Sys.getenv('DB_LAOPS_CORES','2')))
if(is.na(cores)||cores<1L)cores<-1L;cores<-min(cores,4L)
data_files<-sort(list.files('data/source_cache',pattern='[.]csv$',full.names=TRUE))
expected<-list(engine_md5=unname(tools::md5sum('engine/DB_LAOPS_v90.R')),
 data_md5=setNames(unname(tools::md5sum(data_files)),basename(data_files)),
 cache_builder_md5=unname(tools::md5sum('R/cache_core.R')))
jobs<-list()
for(cost in c(10,0,25))for(sample in c('main','no_crypto'))for(kind in c('context','uniform')) {
 tag<-paste(sample,cost,kind,sep='_');path<-file.path('cache',paste0(tag,'.rds'))
 if(identical(Sys.getenv('DB_LAOPS_VERIFY_ONLY'),'TRUE'))next
 if(file.exists(path)) {
  b<-readRDS(path)
  if(!identical(b$research_metadata,expected))stop('Stale cache, preserve it elsewhere before rerunning: ',path)
 } else jobs[[length(jobs)+1L]]<-list(script='cache_core.R',args=c(sample,as.character(cost),kind),tag=paste0('cache_',tag))
}
run_jobs<-function(jobs) {
 if(!length(jobs))return(invisible(NULL))
 f<-function(job,root,rs){setwd(root);log<-file.path('logs',paste0(job$tag,'.log'))
   status<-system2(rs,c(shQuote(file.path('R',job$script)),job$args),stdout=log,stderr=log)
   list(tag=job$tag,status=status,log=log)}
 if(cores==1L)res<-lapply(jobs,f,root=root,rs=rs) else {
  cl<-parallel::makeCluster(cores);on.exit(parallel::stopCluster(cl))
  res<-parallel::parLapply(cl,jobs,f,root=root,rs=rs)
 }
 failed<-vapply(res,function(x)x$status!=0L,logical(1))
 for(z in res)cat(z$tag,if(z$status==0L)'completed' else 'FAILED','\n')
 if(any(failed))stop('A job failed; inspect: ',paste(vapply(res[failed],`[[`,'','log'),collapse=', '))
 invisible(res)
}
if(!identical(Sys.getenv('DB_LAOPS_VERIFY_ONLY'),'TRUE'))run_jobs(jobs)
jobs<-list()
for(cost in c(10,0,25))for(sample in c('main','no_crypto'))for(kind in c('context','uniform')) {
 tag<-paste(sample,cost,kind,sep='_')
 jobs[[length(jobs)+1L]]<-list(script='replay_reductions.R',args=c(sample,as.character(cost),kind),tag=paste0('replay_',tag))
}
if(!identical(Sys.getenv('DB_LAOPS_VERIFY_ONLY'),'TRUE'))run_jobs(jobs)
run_jobs(lapply(if(identical(Sys.getenv('DB_LAOPS_VERIFY_ONLY'),'TRUE')) c('theory_checks.R','equivalence.R') else c('theory_checks.R','entropy_diagnostics.R','equivalence.R'),function(s)
 list(script=s,args=character(0),tag=sub('[.]R$','',s))))
run_jobs(list(list(script='make_figures.R',args=character(0),tag='make_figures'),
 list(script='validate_outputs.R',args=character(0),tag='validate_outputs')))
a<-read.csv('results/economic_equivalence.csv')
stopifnot(nrow(a)==12L)
writeLines(c('COMPLETED: 12 fixed core scenarios and exact raw-candidate replays.',
 '2 universes x 3 costs (0,10,25 bp) x 2 contextual probability rules.',
 '4,999 paired circular-block bootstrap draws; L=20.',
 'Primary equivalence margin: +/-25 annual CE5 bp; all comparisons retained.',
 'Exploratory retrospective evidence; no untouched holdout or general superiority claim.',
 paste0('25-bp equivalence, simultaneous 12-comparison intervals: ',sum(a$equiv_25bp_family12),' / 12'),
 paste0('10-bp sensitivity, simultaneous intervals: ',sum(a$equiv_10bp_family12),' / 12')),
 'results/RUN_SUMMARY.txt')
print(a[,c('sample','cost_bps','model','ce_difference_annual_bp','lower_family12_bp','upper_family12_bp','equiv_25bp_family12')],row.names=FALSE)
cat('Done. See results/RUN_SUMMARY.txt and the accompanying contribution report.\n')
