library(survival)
tempos<-c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
cens<-c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)
dados <- data.frame(tempos=tempos, status=cens)
ekm <- survfit(Surv(tempos,status)~1, data=dados)
st <- ekm$surv
tempo <- ekm$time

library(flexsurv)
ajust1 <- flexsurvreg(Surv(tempos,status)~1,data=dados, dist='exponential')
ajust2 <- flexsurvreg(Surv(tempos,status)~1,data=dados, dist='weibull')
ajust3 <- flexsurvreg(Surv(tempos,status)~1,data=dados, dist='lognormal')
ajust4 <- flexsurvreg(Surv(tempos,status)~1,data=dados, dist='gengamma')

ajust1$loglik
ajust2$loglik
ajust3$loglik
ajust4$loglik

# Calculo do Teste de Razão de Verossimilhança
TRVe <- 2*(ajust4$loglik - ajust1$loglik)
1 - pchisq(TRVe,2)
TRVw <- 2*(ajust4$loglik - ajust2$loglik)
1 - pchisq(TRVw,1)
TRVlog <- 2*(ajust4$loglik - ajust3$loglik)
1 - pchisq(TRVlog,1)

plot(tempo, st, ylim=range(c(0.0,1)), xlim=range(c(0,50)), ylab="S(t)", xlab="Semanas")
lines(ajust1, col="blue", ci=FALSE)
lines(ajust2, col="green", ci=FALSE)
lines(ajust3, col="cyan", ci=FALSE)
lines(ajust4,col="red", ci=FALSE)
legend("topright", lty=c(1,1,1,1), lwd=c(2,2,2,2),
       col=c("blue", "green", "cyan","red"),
       c("Exponencial","Weibull", "Lognormal" , "Gama Generalizada"))


