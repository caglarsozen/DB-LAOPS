out<-'results';x<-read.csv(file.path(out,'softmax_scale_experiment.csv'))
phase<-rbind(read.csv(file.path(out,'budget_phase_main_10.csv')),
             read.csv(file.path(out,'budget_phase_no_crypto_10.csv')))
device<-if(capabilities('cairo')) grDevices::cairo_pdf else grDevices::pdf
device(file.path(out,'mechanism_diagnostics.pdf'),width=10.8,height=4.5,
 family=if(capabilities('cairo')) 'sans' else 'Helvetica',pointsize=16)
par(mfrow=c(1,2),mar=c(5.3,4.8,2.7,1),cex=.9)
a<-x[x$sample=='main',];b<-x[x$sample=='no_crypto',]
plot(a$eta,a$mean_gate,log='xy',type='b',pch=16,col='#185C68',lwd=1.6,
 xlab='Inverse temperature (fixed historical scores)',ylab='Mean squared entropy gate',
 ylim=c(1e-16,.2),main='A. Fourth-order suppression')
lines(b$eta,b$mean_gate,type='b',pch=17,col='#B65824',lwd=1.6)
lines(a$eta,a$mean_fourth_order_approx,lty=2,col='#555555',lwd=1.2)
abline(v=24,lty=3,col='#555555')
legend('topleft',c('ETFs + Bitcoin','ETFs only','Fourth-order local approximation'),
 col=c('#185C68','#B65824','#555555'),pch=c(16,17,NA),lty=c(1,1,2),bty='n',cex=.78)
mtext('Score-scaling diagnostic; no trading performance is implied.',side=1,line=4,cex=.72)
pa<-phase[phase$sample=='main',];pb<-phase[phase$sample=='no_crypto',]
plot(100*pa$base_cvar,100*pa$sufficient_critical_base_dd,type='b',pch=16,lwd=1.6,
 col='#185C68',ylim=c(0,47),xlim=c(2,3.5),xlab='Base daily CVaR budget (%)',
 ylab='Required base drawdown (%)',main='B. Ranking changes the certificate')
lines(100*pb$base_cvar,100*pb$sufficient_critical_base_dd,type='b',pch=17,lwd=1.6,col='#B65824')
lines(100*pa$base_cvar,100*pa$rank_sufficient_critical_base_dd,type='b',pch=1,lwd=1.6,lty=2,col='#185C68')
lines(100*pb$base_cvar,100*pb$rank_sufficient_critical_base_dd,type='b',pch=2,lwd=1.6,lty=2,col='#B65824')
legend('topleft',c('Pool preservation: ETFs + BTC','Pool preservation: ETFs',
 'Top-three preservation: ETFs + BTC','Top-three preservation: ETFs'),
 col=c('#185C68','#B65824','#185C68','#B65824'),lty=c(1,1,2,2),pch=c(16,17,1,2),bty='n',cex=.72)
mtext('Zero means the built-in 18% floor already suffices.',side=1,line=4,cex=.72)
dev.off()
