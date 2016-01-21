dados = read.csv("Leprosy.csv",header = T,sep = ";")
attach(dados)
mean(count)
var(count)

#MODELO DE VEROSSIMILHANCA BINOMIAL NEGATIVO
library(COUNT)
fit_glm_NB = glm.nb(count ~ time + drug + time*drug, data=dados)
summary(fit_glm_NB)
nu = summary(fit_glm_NB)$theta
alpha = 1/nu
alpha

#MODELO EEG POISSON
library(geepack)
library(gee)
fit_gee_POI = geeglm(count~time+drug+time*drug, data=dados, id=id,family =poisson,corstr = "exchangeable")
summary(fit_gee_POI)

#MODELO DE CÓPULAS GAUSSIANAS POISSON
require("gcmr")
fit_cop_POI =gcmr(count~time+drug+time*drug, data=dados,marginal = poisson.marg ,id=id,
          cormat = cluster.cormat(id,type="exchangeable"))
summary(fit_cop_POI)


#MODELO DE CÓPULAS GAUSSIANAS BINOMIAL NEGATIVO
fit_cop_NB =gcmr(count~time+drug+time*drug, data=dados,marginal = negbin.marg ,id=id,
                  cormat = cluster.cormat(id,type="exchangeable"))
summary(fit_cop_NB)

fit_cop_NB =gcmr(count~time+drug+time*drug, data=dados,marginal = negbin.marg ,id=id,
                 cormat = cluster.cormat(id,type="independence"))
summary(fit_cop_NB)

fit_cop_NB =gcmr(count~time+drug+time*drug, data=dados,marginal = negbin.marg ,id=id,
                 cormat = cluster.cormat(id,type="ar1"))
summary(fit_cop_NB)

fit_cop_NB =gcmr(count~time+drug+time*drug, data=dados,marginal = negbin.marg ,id=id,
                 cormat = cluster.cormat(id,type="unstructured"))
summary(fit_cop_NB)

fit_cop_NB2 =gcmr(count~time, data=dados,marginal = negbin.marg ,id=id,
                 cormat = cluster.cormat(id,type="exchangeable"))
summary(fit_cop_NB2)


require(xtable)
xtable(head(dados,10))


