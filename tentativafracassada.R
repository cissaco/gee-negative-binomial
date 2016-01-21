rm(list=ls())
library(COUNT)
library(geepack)
library(gee)
data(rwm5yr)
attach(rwm5yr)

tent1 = gee2(docvis ~ female + factor(edlevel), data=rwm5yr,family = negative.binomial(nu),id=id,corstr = "independence")

nbg <- glm.nb(docvis ~ female + factor(edlevel), data=rwm5yr)
summary(nbg) #Theta é o 1/alpha do livro ou seja o nu
nu = summary(nbg)$theta
alpha = 1/nu

c = coef(nbg)

#Mu)ij chapéu é calculado pelo fitted.values
mu = exp(c[1]+c[2]*female+c[3]*edlevel2+c[4]*edlevel3+c[5]*edlevel4)
head(fitted.values(nbg))
head(mu)

names(nbg)
names(summary(nbg))

#Residuos de pearson
r = residuals(nbg,type="pearson")
head(residuals(nbg,type="pearson"))


#para o calculo do phi
N = length(docvis)
p = length(c)
phi = 1/(sum(r^2)/(N-p))
phi

#para calculo do alpha

