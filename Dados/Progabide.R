progabide = read.csv("Progabide.csv",header = T,sep = ";")
attach(progabide)
mean(seizures)
var(seizures)

#MODELO DE VEROSSIMILHANCA BINOMIAL NEGATIVO
library(COUNT)
fit_glm_NB = glm.nb(seizures~t + trat +t*trat,data=progabide)
summary(fit_glm_NB)
nu = summary(fit_glm_NB)$theta
alpha = 1/nu
alpha

#MODELO EEG POISSON
library(geepack)
library(gee)
fit_gee_POI = geeglm(seizures~t + trat +t*trat,data=progabide, id=id,family =poisson,corstr = "exchangeable")
summary(fit_gee_POI)

#MODELO DE CÓPULAS GAUSSIANAS POISSON
require("gcmr")
fit_cop_POI =gcmr(seizures~t + trat +t*trat,data=progabide,marginal = poisson.marg ,id=id,
                  cormat = cluster.cormat(id,type="exchangeable"))
summary(fit_cop_POI)


#MODELO DE CÓPULAS GAUSSIANAS BINOMIAL NEGATIVO
fit_cop_NB =gcmr(seizures~t + trat +t*trat,data=progabide,marginal = negbin.marg ,id=id,
                 cormat = cluster.cormat(id,type="exchangeable"))
summary(fit_cop_NB)

fit_cop_NB =gcmr(seizures~t + trat +t*trat,data=progabide,marginal = negbin.marg ,id=id,
                 cormat = cluster.cormat(id,type="independence"))
summary(fit_cop_NB)

fit_cop_NB =gcmr(seizures~t + trat +t*trat,data=progabide,marginal = negbin.marg ,id=id,
                 cormat = cluster.cormat(id,type="ar1"))
summary(fit_cop_NB)

fit_cop_NB =gcmr(seizures~t + trat +t*trat,data=progabide,marginal = negbin.marg ,id=id,
                 cormat = cluster.cormat(id,type="unstructured"))
summary(fit_cop_NB)

fit_cop_NB2 =gcmr(seizures~t,data=progabide,marginal = negbin.marg ,id=id,
                  cormat = cluster.cormat(id,type="exchangeable"))
summary(fit_cop_NB2)


require(xtable)
xtable(head(dados,10))


