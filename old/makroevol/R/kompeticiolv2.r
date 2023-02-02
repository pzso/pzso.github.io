###############################################
## Folytonos logisztikus kompetíció dinamika ##
## FORRÁS:                                   ##
## M. Henry H. Stevens (2009)                ##
## Primer of Ecology with R                  ##
## 5. fejezet                                ##
###############################################

parms <- c(r1 = 1, r2 = 0.1,                 # Lotka-Volterra modell paraméterei
 a11 = 0.2, a21 = 0.1, a22 = 0.02, a12 = 0.01)
N0 <- c(2, 1)                                # kiindulási egyedszám

#--- Lotka-Volterra modell
lvcomp2 <- function(t, n, parms) {
 with(as.list(parms), {
   dn1dt <- r1 * n[1] * (1 - a11 * n[1] - a12 * n[2])
   dn2dt <- r2 * n[2] * (1 - a22 * n[2] - a21 * n[1])
   list(c(dn1dt, dn2dt))
 })
}

#---- MAIN
library(deSolve)                             # megoldás az ode függvénnyel (deSolve csomag)
out <- ode(y=N0, times = 1:100, func = lvcomp2, parms = parms)

#---- ÁBRÁZOLÁS
matplot(out[, 1], out[, -1], type = "l", col = c("black","blue"), lty=1, xlab="Idõ (t)",ylab="Egyedszám (N)")

