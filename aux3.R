library(survival)
tempos<-c(3,5,6,7,8,9,10,10,12,15,15,18,19,20,22,25,28,30,40,45)
cens<-c(1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0)
dados <- data.frame(tempos=tempos, status=cens)
ekm <- survfit(Surv(tempos,status)~1, data=dados)

library(flexsurv)
ajustLLog <- flexsurvreg(Surv(tempos, status)~1, data=dados, dist='loglogis')
#ajustLLog <- survreg(Surv(tempos, status)~1, data=dados, dist='loglogistic')
ajustLLog
betall<- ajustLLog$icoef[1]
betall
alfall <- ajustLLog$scale
alfall

llogisHaz <- function(x, alfa, beta) {
  haz <- beta/alfa*(x/alfa)^(beta-1)/(1+(x/alfa)^beta)  
}
curve(llogisHaz(x, alfa=alfall, beta=betall), from=0, to=45,
      ylab="h(t)", xlab="Tempo", col="red")

llogisHaz <- function(x, alfa, beta) {
  haz <- hllogis(x, shape = beta, scale = alfa, log = FALSE)
}
curve(llogisHaz(x, alfa=alfall, beta=betall), from=0, to=45,
      ylab="h(t)", xlab="Tempo", col="red")

library(car)
n.censurado <- dados$tempos[dados$status == 1]
qqPlot(n.censurado,
       dist = "llogis",
       shape = betall,
       scale = alfall,
       xlab = "Quantis Teoricos (LogLogistico)",
       ylab = "Quantis Empiricos")

ajustLog <- survreg(Surv(tempos, status)~1, data=dados, dist='lognorm')
ajustLog


library(survey)
dados_p <- svydesign(ids = ~1, data=dados)
dados_svykm <- svykm(Surv(tempos, status) ~ 1, dados_p, se=TRUE)
mediana.km <- quantile(dados_svykm, 0.5, ci=TRUE)
mediana.km
med.km <- c(mediana.km[1], attr(mediana.km, "ci")[1], attr(mediana.km, "ci")[2])
