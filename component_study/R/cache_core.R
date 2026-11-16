# Research cache only; no publication files or manuscript are modified.
args <- commandArgs(trailingOnly=TRUE)
sample <- args[1]; cost <- as.numeric(args[2]); no_context <- identical(args[3],"uniform")
root <- normalizePath(".")
out <- file.path(root,"cache")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
label <- paste(sample,cost,if(no_context) "uniform" else "context",sep="_")
dest <- file.path(out,paste0(label,".rds"))
if(file.exists(dest)) stop("Refusing to overwrite completed cache: ",dest)
e <- new.env(parent=globalenv())
sys.source(file.path(root,"engine","DB_LAOPS_v90.R"),envir=e)
e$check_packages(FALSE)
# Independent external comparators do not feed any core rule. Omit their work;
# return no external allocations, and never publish a fabricated comparator path.
e$external_weights <- function(R,experts,candidates,eh,dh,pre,state,day,cost,context) {
  list(weights=list(),state=state,target_vol=NA_real_,matched_vol=NA_real_,
       attainable=NA,cvar_status=NA_integer_,cdar_status=NA_integer_)
}
m <- e$read_market(file.path(root,"data","source_cache"),download=FALSE)
t0 <- proc.time()[3]
message("START ",label," | ",Sys.time())
bt <- e$run_engine(m,sample=sample,cost_bps=cost,no_context=no_context,keep_weights=FALSE)
if(cost==10) {
  ref_file <- file.path(root,"reference",paste0(sample,if(no_context) "_uniform" else "_context",".csv"))
  ref <- read.csv(ref_file,stringsAsFactors=FALSE)
  a <- bt$returns
  k1 <- paste(as.character(a$date),a$model);k2 <- paste(ref$date,ref$model)
  ix <- match(k1,k2);ok <- !is.na(ix)
  if(!any(ok)) stop("No archived reference observations matched")
  err <- max(abs(a$net_return[ok]-ref$net_return[ix[ok]]))
  message("REFERENCE MATCH ",label," | n=",sum(ok)," | max_error=",format(err,digits=16))
  stopifnot(err<1e-10)
  bt$reference_validation <- list(n=sum(ok),max_error=err,reference=ref_file)
}
bt$research_cache_note <- "Public core engine; independent external comparators omitted, not imputed."
data_files<-sort(list.files(file.path(root,'data','source_cache'),pattern='\\.csv$',full.names=TRUE))
bt$research_metadata<-list(engine_md5=unname(tools::md5sum(file.path(root,'engine','DB_LAOPS_v90.R'))),
 data_md5=setNames(unname(tools::md5sum(data_files)),basename(data_files)),
 cache_builder_md5=unname(tools::md5sum(file.path(root,'R','cache_core.R'))))
saveRDS(bt,dest,compress=TRUE)
message("DONE ",label," | seconds=",round(proc.time()[3]-t0,2)," | ",Sys.time())
