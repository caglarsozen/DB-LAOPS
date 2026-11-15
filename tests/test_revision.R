args<-commandArgs(trailingOnly=TRUE);root<-if(length(args))args[1] else "."
source(file.path(root,"DB_LAOPS_v90_MAJOR_REVISION.R"));check_packages()
passed<-character();check<-function(label,test) {if(!isTRUE(test))stop(label);passed<<-c(passed,label);cat("PASS",label,"\n")}
check("initial loss included in maximum drawdown",abs(max_drawdown_numeric(c(-.1,.05))+.1)<1e-12)
check("fractional empirical expected shortfall",abs(es_loss(1:10,.85)-(10+.5*9)/1.5)<1e-10)
check("expected shortfall handles ties",abs(es_loss(rep(.03,126))-.03)<1e-12)
set.seed(47)
for(k in 1:25) {v<-rnorm(10);u<-c(rep(.35,9),.1);w<-project_box(v,u)
 q<-quadprog::solve.QP(diag(10),v,cbind(rep(1,10),diag(10),-diag(10)),c(1,rep(0,10),-u),meq=1)$solution
 stopifnot(max(abs(w-q))<1e-8)}
check("box projection agrees with independent QP",TRUE)
p<-c(.6,.4);r<-c(.1,-.05);z<-execute_trade(p,p,r,.001)
check("no-trade accounting charges zero cost",abs(z$cost)<1e-12 && abs(z$net-.04)<1e-12)
z2<-execute_trade(z$drift,z$drift,c(-.04,.02),.001)
check("buy-and-hold has zero second-period turnover",z2$turnover<1e-12)
z3<-execute_trade(p,z$drift,c(-.04,.02),.001)
check("constant-mix rebalancing is charged after drift",z3$cost>0 && abs(z3$cost-.001*z3$turnover)<1e-12)
check("self financing equation",abs((1-z3$cost)*(1+z3$gross)-1-z3$net)<1e-12)
R<-matrix(rnorm(126*4,0,.01),126,4);colnames(R)<-c("SPY","QQQ","SHY","GLD");p<-rep(.25,4)
cv<-risk_lp(R,p,.001,"cvar");cd<-risk_lp(R,p,.001,"cdar");mv<-meanvar_cost(R,p,.001)
check("external solvers obey simplex and caps",all(vapply(list(cv$w,cd$w,mv),function(w)abs(sum(w)-1)<1e-8&&min(w)>-1e-8&&max(w)<.35+1e-8,logical(1))))
obj<-function(w) -sum(colMeans(R)*w)+5*es_loss(-as.numeric(R%*%w))+.001/21*sum(abs(w-p))
random<-replicate(200,obj(project_box(rexp(4)/4,rep(.35,4))))
check("CVaR LP improves on feasible random candidates",obj(cv$w)<=min(random)+1e-7)
check("risk matching solves covariance target",abs(vol_match(c(1,0,0,0),R,.14)$vol-.14)<1e-7)
check("circular block index bounds and sample size",{ix<-circular_indices(151,20);length(ix)==151&&all(ix>=1&ix<=151)})
a<-rnorm(150,.0003,.01);fr<-rep(.0001,150);ti<-paired_inference(a,a,fr,B=49,L=10)
check("paired identical-strategy null",all(abs(ti$difference)<1e-12)&&all(ti$p_value==1))
check("Sharpe uses excess returns",abs(performance(a,fr)["sharpe"]-sqrt(252)*mean(a-fr)/sd(a-fr))<1e-12)
# Nontrivial chronological prefix test: perturb future prices and context, not only function arguments.
m<-synthetic_market(630);ASSET_NAMES<-colnames(m$R);b1<-run_engine(m,"main",10,keep_weights=TRUE)
cut<-as.Date(index(m$R)[nrow(m$R)-15]);m2<-m;ix<-which(as.Date(index(m$R))>cut)
m2$R[ix,]<-m2$R[ix,]*2+.03;m2$X[ix,]<-m2$X[ix,]+.5
b2<-run_engine(m2,"main",10,keep_weights=TRUE)
a1<-b1$weights[b1$weights$date<=cut,];a2<-b2$weights[b2$weights$date<=cut,]
check("future perturbations cannot change earlier allocations",identical(a1,a2))
# Baseline target replay must reproduce every accounting event, including initial transition.
z<-replay_targets(b1$targets,b1$decision_dates,m$R,10,"DB_LAOPS","main")
orig<-b1$returns[b1$returns$model=="DB_LAOPS",]
check("independent target replay reproduces baseline",max(abs(z$returns$net_return-orig$net_return))<1e-10)
check("information strictly precedes execution and return",all(b1$returns$signal_date<b1$returns$decision_date)&all(b1$returns$decision_date<b1$returns$date))
writeLines(c(paste("Passed",length(passed),"checks"),passed),file.path(root,"tests","TEST_RESULTS.txt"))
# The individual screens must be capable of binding even when one is redundant in real data.
ns<-c('SPY','QQQ','IWM','EFA','EEM','GLD','TLT','SHY','UUP','BTC')
CW<-setNames(rep(list(rep(.1,10)),4),c('A','B','C','D'))
ctx<-xts::xts(matrix(c(log(18),0,0,0),1,dimnames=list(NULL,c('log_VIX','SPY_mom_63','regime_shift_score','SPY_drawdown_252'))),order.by=as.Date('2019-01-01'))
h<-matrix(.0001,126,4,dimnames=list(NULL,names(CW)));h[,1]<- -.003
q1<-make_db_selector_weights(h,CW,ns,rep(.1,10),ctx,'main')
q2<-make_db_selector_weights(h,CW,ns,rep(.1,10),ctx,'nodd')
check('drawdown screen can bind independently of CVaR',q1$pass_count==3 && q2$pass_count==4)
h[,1]<-.001;h[120:126,1]<- -.035
q1<-make_db_selector_weights(h,CW,ns,rep(.1,10),ctx,'main')
q2<-make_db_selector_weights(h,CW,ns,rep(.1,10),ctx,'nocvar')
check('CVaR screen can bind independently of drawdown',q1$pass_count==3 && q2$pass_count==4)
p<-rep(1/17,17);q<-p;p[1:2]<-p[1:2]+c(.0001,-.0001);q[1:2]<-q[1:2]+c(.0002,-.0002)
check('squared entropy gate has fourth-order local response',abs((1-entropy01(q))^2/(1-entropy01(p))^2-16)<.01)
writeLines(c(paste('Passed',length(passed),'checks'),passed),file.path(root,'tests','TEST_RESULTS.txt'))
