rm(list=ls())
library(deSolve)
library(ggplot2)

seir <- function(t,y,parms){
  with(as.list(c(y, parms)),{
    
    #betat <- beta*(1-(a2/1+exp(-a1*(t-atao)))) #use a constant beta value
    
    dSdt = -beta*S*(Ia + Im)/N
    dEdt = beta*S*(Ia + Im)/N - v*E
    dIadt = r*v*E - Ph*taoh*Ia - (1-Ph)*Pu*taoh*Ia - (1-Ph)*(1-Pu)*gamma*Ia
    dImdt = (1-r)*v*E - gamma*Im
    dHdt = Ph*taoh*Ia - Pu*taou*H - (1-Pu)*taof*H
    dUdt = (1-Ph)*Pu*taoh*Ia + Pu*taou*H - taod*U
    dRdt = gamma*Im + (1-Ph)*(1-Pu)*gamma*Ia + (1-Pu)*taof*H + taod*U
    dXdt = r*g*taor*E
    list(c(dSdt, dEdt, dImdt, dIadt, dHdt, dUdt, dRdt, dXdt))
  })
}

# Kailahun district

parms <- c(N = 358190, 
           beta = (1/10.9)*11,
           a1 = 0.12,
           a2 = 0.94,
           atao = 1.5,
           v = 1/9.4,
           r = 0.6,
           Ph = 0.5,
           Pu = 0.5,
           taoh = 1/1.8,
           gamma = 1/10.9,
           taou = 1/2,
           taod = 1/(11.3 - 1/(1/1.8) - 1/(1/1.8)),
           taof = 1/(11.3 - 1/(1/1.8)),
           g = 1/4.5,
           taor = 1/4.5)

# initial values for 19 May to 25 May 2014 (Kailahun Province) # week 21

init <- c(S = 358190 - 3,
          E = 0,
          Ia = 3,
          Im = 0,
          H = 0,
          U = 0,
          R = 0,
          X = 0)

t <- seq(0,245,1) # 35 weeks from 1 june 2014 to 1 feb 2015

seir.out <- data.frame(lsoda(y = init,
                             times = t,
                             func = seir,
                             parms = parms))

head(seir.out)
# melt data
seir.out.melt <- reshape2::melt(seir.out, id.var = "time")

# plot
ggplot(data = seir.out.melt, aes(x= time, y = value, color = variable)) +
  geom_line()
