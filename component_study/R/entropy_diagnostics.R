# Decision-time diagnostics from reference core caches; all empirical dates are
# selected by the evaluated outcome date, not by the preceding decision date.
out<-'results';dir.create(out,recursive=TRUE,showWarnings=FALSE)
ans<-list();scales<-list();clones<-list()
for(sample in c('main','no_crypto')) {
 bt<-readRDS(file.path('cache',paste0(sample,'_10_context.rds')))
 db<-bt$returns[bt$returns$model=='DB_LAOPS',]
 keep<-db$date>=as.Date('2020-01-01');N<-nrow(db)
 K<-length(unique(bt$probabilities$expert));stopifnot(K==17L)
 P<-matrix(bt$probabilities$prob_contextual,nrow=N,ncol=K,byrow=TRUE)[keep,,drop=FALSE]
 stopifnot(all(P>0),max(abs(rowSums(P)-1))<1e-12)
 P<-P/rowSums(P);lp<-log(P);D<-pmax(rowSums(P*log(K*P)),0)
 H<-1-D/log(K);g<-(D/log(K))^2
 R<-apply(lp,1,function(x)diff(range(x)))/24
 upperD<-24^2*R^2/8;upperG<-pmin(1,(upperD/log(K))^2)
 tr<-bt$tau[keep,];excess<-tr$tau-.05
 stopifnot(nrow(P)==1508,max(abs(H-tr$prob_entropy_contextual))<1e-12,
   all(D<=upperD+1e-12),all(excess<=.85*upperG+1e-13))
 ans[[length(ans)+1L]]<-data.frame(sample=sample,n=nrow(P),experts=K,
  mean_entropy=mean(H),min_entropy=min(H),max_expert_probability=max(P),
  mean_gate=mean(g),max_gate=max(g),mean_tau_excess=mean(excess),max_tau_excess=max(excess),
  median_eta_loss_range=median(24*R),max_eta_loss_range=max(24*R),
  max_tau_excess_bound=max(.85*upperG),median_gate_bound=median(upperG))
 vv<-apply(lp,1,var)*(K-1)/K
 for(alpha in c(.03125,.0625,.125,.25,.5,1,2,4,8,16,32,64,128)) {
  z<-alpha*(lp-rowMeans(lp));z<-z-apply(z,1,max);q<-exp(z);q<-q/rowSums(q)
  d<-pmax(rowSums(q*log(pmax(K*q,1e-300))),0);gg<-(d/log(K))^2
  leading<-(alpha^2*vv/(2*log(K)))^2
  scales[[length(scales)+1L]]<-data.frame(sample=sample,score_multiplier=alpha,eta=24*alpha,
    mean_gate=mean(gg),median_gate=median(gg),mean_fourth_order_approx=mean(leading),
    ratio_to_fourth_order=mean(gg)/mean(leading),fraction_gate_above_005=mean(gg>=.05))
 }
 for(m in c(1,2,5,10,100)) {
  Hm<-(H*log(K)+log(m))/log(m*K);gm<-pmax(1-Hm,0)^2
  predicted<-g*(log(K)/log(m*K))^2
  stopifnot(max(abs(gm-predicted))<1e-12)
  clones[[length(clones)+1L]]<-data.frame(sample=sample,replications=m,
    advice_weights_unchanged=TRUE,gate_ratio=(log(K)/log(m*K))^2,max_identity_error=max(abs(gm-predicted)))
 }
 write.csv(data.frame(decision_date=db$decision_date[keep],outcome_date=db$date[keep],sample=sample,
   KL=D,entropy=H,gate=g,loss_range=R,eta_loss_range=24*R,KL_upper_bound=upperD,
   gate_upper_bound=upperG,tau=tr$tau),file.path(out,paste0('entropy_certificate_',sample,'.csv')),row.names=FALSE)
}
write.csv(do.call(rbind,ans),file.path(out,'entropy_summary.csv'),row.names=FALSE)
write.csv(do.call(rbind,scales),file.path(out,'softmax_scale_experiment.csv'),row.names=FALSE)
write.csv(do.call(rbind,clones),file.path(out,'expert_replication_identity.csv'),row.names=FALSE)
print(do.call(rbind,ans),row.names=FALSE)
