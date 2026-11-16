# Paired fixed-rule historical equivalence, not validation on an untouched sample.
root<-normalizePath('.');out<-file.path(root,'results')
e<-new.env(parent=globalenv());sys.source(file.path(root,'engine','DB_LAOPS_v90.R'),envir=e)
rf<-read.csv(file.path(root,'reference','risk_free_carry.csv'))
if(!'rf'%in%names(rf)) stop('Check risk-free column name before use')
CE<-function(r,f)252*(mean(r-f)-2.5*var(r-f))
results<-list();B<-4999L;L<-20L
for(sample in c('main','no_crypto')) for(cost in c(10,0,25)) {
 f<-file.path(out,paste0('reduction_returns_',sample,'_',cost,'_context.csv'))
 g<-file.path(out,paste0('reduction_returns_',sample,'_',cost,'_uniform.csv'))
 if(!file.exists(f)||!file.exists(g))next
 a<-read.csv(f);b<-read.csv(g);a<-a[a$date>='2020-01-01',];b<-b[b$date>='2020-01-01',]
 ref<-a[a$model=='OriginalReplay',];fr<-rf$rf[match(ref$date,rf$date)]
 stopifnot(nrow(ref)==1508,!anyNA(fr))
 for(model in c('EqualTop3','UniformAdvice_EqualTop3')) {
  x<-if(model=='EqualTop3') a[a$model==model,] else b[b$model==model,]
  stopifnot(identical(ref$date,x$date))
  delta<-CE(x$net_return,fr)-CE(ref$net_return,fr)
  set.seed(20261115L+match(sample,c('main','no_crypto')))
  bs<-numeric(B)
  for(i in seq_len(B)) {
   ix<-e$circular_indices(nrow(ref),L)
   bs[i]<-CE(x$net_return[ix],fr[ix])-CE(ref$net_return[ix],fr[ix])
  }
  interval<-function(family) {
   tail<-.05/(2*family)
   as.numeric(2*delta-quantile(bs,c(1-tail,tail),names=FALSE))
  }
  ci<-interval(1);ci4<-interval(4);ci12<-interval(12)
  results[[length(results)+1L]]<-data.frame(sample=sample,cost_bps=cost,model=model,
    ce_difference=delta,ce_difference_annual_bp=1e4*delta,
    lower95_bp=1e4*ci[1],upper95_bp=1e4*ci[2],
    lower_family4_bp=1e4*ci4[1],upper_family4_bp=1e4*ci4[2],
    lower_family12_bp=1e4*ci12[1],upper_family12_bp=1e4*ci12[2],
    equiv_primary_25bp_family4=(ci4[1]>-.0025&&ci4[2]<.0025),
    equiv_25bp_family12=(ci12[1]>-.0025&&ci12[2]<.0025),
    equiv_10bp_family12=(ci12[1]>-.001&&ci12[2]<.001),
    equiv_50bp_family12=(ci12[1]>-.005&&ci12[2]<.005),
    max_daily_difference=max(abs(x$net_return-ref$net_return)),B=B,L=L,
    design='Exploratory, fixed historical rules; basic paired circular-block intervals')
 }
}
ans<-do.call(rbind,results)
write.csv(ans,file.path(out,'economic_equivalence.csv'),row.names=FALSE)
print(ans[,c('sample','cost_bps','model','ce_difference_annual_bp','lower_family12_bp','upper_family12_bp','equiv_25bp_family12')],row.names=FALSE)
