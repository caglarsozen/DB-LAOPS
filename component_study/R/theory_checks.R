# Numerical falsification checks complement, and do not replace, the proofs.
set.seed(20261115L)
softmax<-function(x){z<-exp(x-max(x));z/sum(z)}
KL<-function(p)sum(p*log(length(p)*p))
gate<-function(p)(KL(p)/log(length(p)))^2
entropy_bound_cases<-5000L;max_bound_violation<-0;max_clone_error<-0
for(i in seq_len(entropy_bound_cases)) {
 K<-sample(2:40,1);ell<-runif(K,-.02,.02);eta<-10^runif(1,-2,3)
 p<-softmax(-eta*ell);D<-KL(p);bound<-eta^2*diff(range(ell))^2/8
 max_bound_violation<-max(max_bound_violation,D-bound)
 stopifnot(D<=bound+1e-12)
 for(m in c(2,5,10)) {
  q<-rep(p/m,each=m)
  predicted<-gate(p)*(log(K)/log(m*K))^2
  max_clone_error<-max(max_clone_error,abs(gate(q)-predicted))
  stopifnot(abs(gate(q)-predicted)<1e-12)
 }
}
# Exact rank-aware certificate: the selected CVaR-feasible top k survive DD.
rank_tests<-10000L;rank_certified<-0L
for(i in seq_len(rank_tests)) {
 M<-sample(4:20,1);k<-3L
 utility<-rnorm(M);dd<-runif(M);cv<-runif(M)
 bd<-runif(1);bc<-runif(1)
 C<-which(cv<=bc);J<-which(cv<=bc&dd<=bd)
 if(length(C)<2)next
 S<-head(C[order(-utility[C],C)],k)
 if(all(dd[S]<=bd)) {
  rank_certified<-rank_certified+1L
  S2<-head(J[order(-utility[J],J)],k)
  stopifnot(identical(S,S2),identical(softmax(utility[S]),softmax(utility[S2])))
 }
}
# Strong pool containment is not necessary: a low-ranked excluded asset is harmless.
utility<-c(4,3,2,1);dd<-c(.1,.1,.1,.4);bd<-.18
C<-1:4;J<-which(dd<=bd)
stopifnot(!all(dd[C]<=bd),identical(head(order(-utility),3),head(J[order(-utility[J])],3)))
# The fallback guard matters. A single CVaR-feasible candidate sends selection to
# the whole library, where DD excess penalties can change the top three.
base<-c(.001,.020,.018,.0175,.017)
cv<-c(.01,.04,.04,.04,.04);dd<-c(.1,.6,.1,.1,.1);bc<-.02;bd<-.20
score_no_dd<-base-1.5*pmax(cv-bc,0)/bc/252
score_joint<-score_no_dd-.8*pmax(dd-bd,0)/bd/252
stopifnot(sum(cv<=bc)==1L,
          !identical(head(order(-score_no_dd),3),head(order(-score_joint),3)))
# A small proposal perturbation need not imply a small trade perturbation at the
# hard no-trade boundary, so the entropy bound is not a terminal-wealth bound.
pre<-c(.5,.5);eps<-1e-10
a<-pre+c((.02-eps)/2,-(.02-eps)/2)
b<-pre+c((.02+eps)/2,-(.02+eps)/2)
trade_rule<-function(w){d<-sum(abs(w-pre));if(d<=.02)pre else pre+min(1,.04/d)*(w-pre)}
stopifnot(sum(abs(a-b))<1e-8,sum(abs(trade_rule(a)-trade_rule(b)))>.019)
res<-data.frame(check=c('finite_range_entropy_bound','uniform_replication_identity',
                        'rank_certificate_random_cases','fallback_counterexample','pool_not_necessary_counterexample',
                        'no_trade_discontinuity_counterexample'),
 cases=c(entropy_bound_cases,3*entropy_bound_cases,rank_certified,1,1,1),
 max_error=c(max_bound_violation,max_clone_error,0,0,0,0),passed=TRUE)
dir.create('results',recursive=TRUE,showWarnings=FALSE)
write.csv(res,'results/theory_checks.csv',row.names=FALSE)
print(res,row.names=FALSE)
