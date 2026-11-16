out<-'results';eq<-read.csv(file.path(out,'economic_equivalence.csv'))
stopifnot(nrow(eq)==12L,!anyDuplicated(eq[c('sample','cost_bps','model')]),all(eq$B==4999L),all(eq$L==20L))
rf<-read.csv('reference/risk_free_carry.csv');pub<-read.csv('reference/published_cost_sensitivity.csv')
errs<-numeric();ces<-numeric();dds<-numeric();rank_n<-0L
for(sample in c('main','no_crypto'))for(cost in c(0,10,25)) {
 p<-read.csv(file.path(out,paste0('reduction_performance_',sample,'_',cost,'_context.csv')))
 a<-p[p$model=='OriginalReplay',];b<-pub[pub$sample==sample&pub$tcost_bps==cost&pub$model=='DB_LAOPS',]
 fs<-c('ann_return','ann_vol','sharpe','sortino','mdd','cvar95','cvar99','cdar95','ulcer','calmar','log_growth','ce2','ce5','ce10','final_wealth','avg_turnover')
 stopifnot(nrow(a)==1L,nrow(b)==1L);errs<-c(errs,abs(unlist(a[fs])-unlist(b[fs])))
 for(kind in c('context','uniform')) {
  z<-read.csv(file.path(out,paste0('reduction_returns_',sample,'_',cost,'_',kind,'.csv')))
  if(kind=='context') {
   a<-z[z$model=='OriginalReplay',];b<-z[z$model=='NoDrawdownReplay',]
   stopifnot(identical(a$date,b$date));dds<-c(dds,abs(a$net_return-b$net_return))
  }
  c<-read.csv(file.path(out,paste0('drawdown_certificate_',sample,'_',cost,'_',kind,'.csv')))
  stopifnot(all(c$rank_sufficient),all(c$cvar_admissible>=2));rank_n<-rank_n+nrow(c)
  p<-read.csv(file.path(out,paste0('reduction_performance_',sample,'_',cost,'_',kind,'.csv')))
  z<-z[z$date>='2020-01-01',]
  for(model in unique(z$model)) {
   zz<-z[z$model==model,];f<-rf$rf[match(zz$date,rf$date)];stopifnot(length(f)==1508L,!anyNA(f))
   e<-zz$net_return-f;ces<-c(ces,abs(252*(mean(e)-2.5*var(e))-p$ce5[p$model==model]))
  }
  q<-read.csv(file.path(out,paste0('replay_identity_',sample,'_',cost,'_',kind,'.csv')))
  stopifnot(q$n==2513L,q$max_error<1e-10)
 }
}
t<-read.csv(file.path(out,'theory_checks.csv'));stopifnot(nrow(t)==6L,all(t$passed))
stopifnot(max(errs)<1e-10,max(ces)<1e-10,max(dds)<1e-12,all(eq$equiv_25bp_family12),all(eq$equiv_10bp_family12))
checks<-data.frame(check=c('Published cost metrics','Direct CE5 reconstruction','Exact drawdown-removal paths','Rank conditions and fallback guard','Analytical fixture groups','CE5 equivalence comparisons'),n=c(length(errs),length(ces),length(dds),rank_n,6,12),max_error=c(max(errs),max(ces),max(dds),0,0,0),passed=TRUE)
write.csv(checks,file.path(out,'REVISION_CHECKS.csv'),row.names=FALSE)
writeLines(c('PASS: revision output verification on existing computed paths.',capture.output(print(checks,row.names=FALSE)),'No general superiority, new-data success or future-loss guarantee is established.'),file.path(out,'REVISION_CHECKS.txt'))
print(checks,row.names=FALSE)
