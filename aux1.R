library(survival)
tempos<-c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
cens<-c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)
ekm <- survfit(Surv(tempos,cens)~1)

ajustExp <- survreg(Surv(tempos,cens)~1,dist='exponential')
ajustExp
alfa <- exp(ajustExp$coefficients[1])
alfa

ajustWei <- survreg(Surv(tempos,cens)~1,dist='weibull')
ajustWei
alfaw <- exp(ajustWei$coefficients[1])
alfaw
betaw <- 1/ajustWei$scale
betaw

ajustLog <- survreg(Surv(tempos,cens)~1,dist='lognorm')
ajustLog
mu <- ajustLog$icoef[1]
mu
sigma <- exp(ajustLog$icoef[2])
sigma

ajustLogl <- survreg(Surv(tempos,cens)~1,dist='loglogistic')
ajustLogl
mu1 <- ajustLogl$icoef[1]
mu1
sigma1 <- exp(ajustLogl$icoef[2])
sigma1

tempo <- ekm$time
st <- ekm$surv
ste <- exp(-tempo/alfa)
stw <- exp(-(tempo/alfaw)^betaw)
stln <- pnorm((-log(tempo) + mu)/sigma)
stll <- pnorm((-log(tempo) + mu1)/sigma1)
cbind(tempo,st,ste,stw,stln, stll)


par(mfrow=c(1,4))
plot(st,ste,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), 
     xlab = "S(t): Kaplan-Meier", ylab="S(t): exponencial")
lines(c(0,1), c(0,1), type="l", lty=1)
plot(st,stw,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), 
     xlab = "S(t): Kaplan-Meier", ylab="S(t): Weibull")
lines(c(0,1), c(0,1), type="l", lty=1)
plot(st,stln,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), 
     xlab = "S(t): Kaplan-Meier", ylab="S(t): lognormal")
lines(c(0,1), c(0,1), type="l", lty=1)
plot(st,stll,pch=16,ylim=range(c(0.0,1)), xlim=range(c(0,1)), 
     xlab = "S(t): Kaplan-Meier", ylab="S(t): loglogística")
lines(c(0,1), c(0,1), type="l", lty=1)


tab.np <- summary(ekm)

plot(log(tab.np$time), log(-log(tab.np$surv)),
     xlab="log(t)", ylab="log(-log(S(t)))", pch=20)
teste1 <- lm(log(tab.np$time) ~ log(-log(tab.np$surv)))
summary(teste1)
-log(1/alfa)

par(mfrow=c(1,3))
tab.np <- summary(ekm)
invst1 <- qnorm(tab.np$surv)
invst2 <- qlogis(tab.np$surv) 
plot(log(tab.np$time), log(-log(tab.np$surv)),
     xlab="log(t)", ylab="log(-log(S(t)))", pch=20)
plot(log(tab.np$time), invst1,
     xlab="log(t)", ylab=expression(Phi^-1*(S(t))), pch=20)
plot(log(tab.np$time), invst2,
     xlab="log(t)", ylab=expression(Phi^-1*Logis*(S(t))), pch=20)

tab.exp <- summary(ajustExp)
tab.wei <- summary(ajustWei)
tab.ln <- summary(ajustLog)
tab.ll <- summary(ajustLogl)
TRV <- 2*(tab.wei$loglik - tab.exp$loglik)
1 - pchisq(TRV,1)

aic.exp <- -2*tab.exp$loglik[1] + 2*1
aic.wei <- -2*tab.wei$loglik[1] + 2*2
aic.ln <- -2*tab.ln$loglik[1] + 2*2
aic.ll <- -2*tab.ll$loglik[1] + 2*2

aic <- c(aic.exp, aic.wei, aic.ln, aic.ll)
delta.aic <- aic - min(aic)
peso.aic <- exp(-0.5*delta.aic)/sum(exp(-0.5*delta.aic))
modelos <- data.frame(modelos=c("Exponencial", "Weibull",
                                "Lognormal", "Loglogistico"),
                      p_Akaike = peso.aic)
sum(peso.aic)
gt::gt(modelos)

library(flexsurv)
ajust1 <- flexsurvreg(Surv(survt,status)~1,data=dados, dist='exponential')
ajust2 <- flexsurvreg(Surv(survt,status)~1,data=dados, dist='weibull')
ajust3 <- flexsurvreg(Surv(survt,status)~1,data=dados, dist='lognormal')
ajust4 <- flexsurvreg(Surv(survt,status)~1,data=dados, dist='llogis')
ajust5 <- flexsurvreg(Surv(survt,status)~1,data=dados, dist='gengamma')

ajust1$loglik
ajust2$loglik
ajust3$loglik
ajust4$loglik
ajust5$loglik
