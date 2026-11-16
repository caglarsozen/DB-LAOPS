args <- commandArgs(trailingOnly=TRUE)
sample <- args[1];cost <- as.numeric(args[2]);context_kind<-args[3]
root<-normalizePath('.');out<-file.path(root,'results')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
e<-new.env(parent=globalenv())
sys.source(file.path(root,'engine','DB_LAOPS_v90.R'),envir=e)
e$check_packages(FALSE)
m<-e$read_market(file.path(root,'data','source_cache'),download=FALSE)
bt<-readRDS(file.path(root,'cache',paste0(paste(sample,cost,context_kind,sep='_'),'.rds')))
R<-m$R[,colnames(bt$targets),drop=FALSE]
nm<-names(bt$candidate_weights[[1]]);N<-length(bt$candidate_weights);p<-NCOL(R)
H<-sapply(nm,function(k)bt$returns$net_return[bt$returns$model==k]);colnames(H)<-nm
db<-bt$returns[bt$returns$model=='DB_LAOPS',];stopifnot(nrow(db)==N)
e$ASSET_NAMES<-colnames(R)
modes<-if(context_kind=='context') c(OriginalReplay='main',NoDrawdownReplay='nodd',EqualTop3='nodd') else c(UniformOriginalReplay='main',UniformAdvice_EqualTop3='nodd')
histories<-lapply(modes,function(x)rep(1/p,p));names(histories)<-names(modes)
ret<-list();cert<-list();scores<-list();allstats<-vector('list',N)
for(t in seq_len(N)) {
  nh<-max(0L,t-1L-bt$settings$lag)
  dh<-if(nh) H[seq_len(nh),,drop=FALSE] else H[integer(0),,drop=FALSE]
  i<-match(bt$decision_dates[t],as.Date(index(R)))-bt$settings$lag
  ctx<-m$X[index(R)[i],]
  st<-e$candidate_history_stats(dh,126)
  if(!is.null(st)) {
    xi<-e$compute_stress_intensity(ctx);bc<-max(.014,.026-.004*xi);bd<-max(.18,.30-.04*xi)
    pc<-st$cvar<=bc;mx<-if(any(pc))max(-st$mdd[pc]) else NA_real_
    crit<-if(is.finite(mx)&&mx>.18) mx+.04*xi else 0
    score0<-st$mean_log-.05*pmax(st$ann_vol/max(median(st$ann_vol),1e-8)-1.5,0)/252-
      ifelse(nm%in%c('QQQ_BuyHold','SPY_QQQ_5050','QQQSoftGuard','DrawdownControlledQQQ'),xi*.00035,0)
    pool<-which(pc);top<-head(pool[order(-score0[pool],pool)],3)
    mxrank<-if(length(top)) max(-st$mdd[top]) else NA_real_
    rankcrit<-if(is.finite(mxrank)&&mxrank>.18) mxrank+.04*xi else 0
    cert[[length(cert)+1L]]<-data.frame(sample=sample,cost_bps=cost,context=context_kind,date=db$date[t],
       cvar_admissible=sum(pc),max_dd_given_cvar=mx,dd_budget=bd,stress=xi,
       margin=bd-mx,sufficient=(sum(pc)>=2&&all(-st$mdd[pc]<=bd)),critical_base_dd=crit,
       max_dd_of_selected=mxrank,rank_margin=bd-mxrank,rank_sufficient=(sum(pc)>=2&&all(-st$mdd[top]<=bd)),
       rank_critical_base_dd=rankcrit)
    allstats[[t]]<-list(stats=st,stress=xi)
  }
  for(k in names(modes)) {
    e$db_eta<-if(grepl('Replay$',k))28 else 0
    z<-e$make_db_selector_weights(dh,bt$candidate_weights[[t]],colnames(R),histories[[k]],ctx,mode=modes[[k]])
    w<-e$apply_turnover_control(z$w,histories[[k]],.015,.06)$w
    w<-e$project_assets(e$normalize_weights(w,cap=1),colnames(R),.35)
    rr<-as.numeric(R[as.character(db$date[t]),])
    tr<-e$execute_trade(w,histories[[k]],rr,cost/10000)
    ret[[length(ret)+1L]]<-data.frame(date=db$date[t],decision_date=bt$decision_dates[t],sample=sample,cost_bps=cost,
       model=k,net_return=tr$net,gross_return=tr$gross,turnover=tr$turnover,cost_fraction=tr$cost)
    histories[[k]]<-tr$drift
  }
}
ret<-do.call(rbind,ret);cer<-do.call(rbind,cert)
original_name<-names(modes)[1];chk<-ret[ret$model==original_name,]
err<-max(abs(chk$net_return-db$net_return));stopifnot(err<1e-10)
message('REPLAY IDENTITY ',sample,' ',cost,' ',context_kind,' max error=',format(err,digits=16))
write.csv(ret,file.path(out,paste0('reduction_returns_',sample,'_',cost,'_',context_kind,'.csv')),row.names=FALSE)
write.csv(cer,file.path(out,paste0('drawdown_certificate_',sample,'_',cost,'_',context_kind,'.csv')),row.names=FALSE)
check<-data.frame(sample=sample,cost_bps=cost,context=context_kind,n=N,max_error=err)
write.csv(check,file.path(out,paste0('replay_identity_',sample,'_',cost,'_',context_kind,'.csv')),row.names=FALSE)
if(context_kind=='context') {
 phase<-list()
 for(cb in c(.021,.026,.034)) {
  z<-list()
  for(t in seq_len(N)) {
   a<-allstats[[t]];if(is.null(a)||db$date[t]<as.Date('2020-01-01'))next
   bc<-max(.014,cb-.004*a$stress);pass<-a$stats$cvar<=bc
   maxdd<-if(any(pass))max(-a$stats$mdd[pass]) else NA_real_
   st<-a$stats;xi<-a$stress
   score0<-st$mean_log-.05*pmax(st$ann_vol/max(median(st$ann_vol),1e-8)-1.5,0)/252-
     ifelse(nm%in%c('QQQ_BuyHold','SPY_QQQ_5050','QQQSoftGuard','DrawdownControlledQQQ'),xi*.00035,0)
   pp<-which(pass);tt<-head(pp[order(-score0[pp],pp)],3)
   maxrank<-if(length(tt))max(-st$mdd[tt]) else NA_real_
   z[[length(z)+1L]]<-data.frame(npass=sum(pass),required_base=if(is.finite(maxdd)&&maxdd>.18)maxdd+.04*a$stress else 0,
      dd_at_cvar_pass=maxdd,required_rank_base=if(is.finite(maxrank)&&maxrank>.18)maxrank+.04*a$stress else 0)
  }
  zz<-do.call(rbind,z)
  phase[[length(phase)+1L]]<-data.frame(sample=sample,cost_bps=cost,base_cvar=cb,
    n=nrow(zz),min_cvar_admissible=min(zz$npass),all_minimum_count_met=all(zz$npass>=2),
    sufficient_critical_base_dd=max(zz$required_base),rank_sufficient_critical_base_dd=max(zz$required_rank_base),
    max_dd_of_cvar_admissible=max(zz$dd_at_cvar_pass))
 }
 write.csv(do.call(rbind,phase),file.path(out,paste0('budget_phase_',sample,'_',cost,'.csv')),row.names=FALSE)
}
e$db_eta<-28
perf<-e$perf_table(transform(ret,cost_bps=cost),m$rf,'2020-01-01','2025-12-31')
perf$cost_bps<-cost
write.csv(perf,file.path(out,paste0('reduction_performance_',sample,'_',cost,'_',context_kind,'.csv')),row.names=FALSE)
print(perf[,c('sample','model','cost_bps','ann_return','sharpe','mdd','ce5')],row.names=FALSE)
