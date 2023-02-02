###############################################
## Folytonos logisztikus kompet�ci� dinamika ##
## FORR�S:                                   ##
## M. Henry H. Stevens (2009)                ##
## Primer of Ecology with R                  ##
## 5. fejezet                                ##
###############################################

parms <- c(r1 = 1, r2 = 0.1,                 # Lotka-Volterra modell param�terei
 a11 = 0.2, a21 = 0.1, a22 = 0.02, a12 = 0.01)
N0 <- c(2, 1)                                # kiindul�si egyedsz�m

#--- Lotka-Volterra modell
lvcomp2 <- function(t, n, parms) {
 with(as.list(parms), {
   dn1dt <- r1 * n[1] * (1 - a11 * n[1] - a12 * n[2])
   dn2dt <- r2 * n[2] * (1 - a22 * n[2] - a21 * n[1])
   list(c(dn1dt, dn2dt))
 })
}

#---- MAIN
library(deSolve)                             # megold�s az ode f�ggv�nnyel (deSolve csomag)
out <- ode(y=N0, times = 1:100, func = lvcomp2, parms = parms)

#---- �BR�ZOL�S
matplot(out[, 1], out[, -1], type = "l", col = c("black","blue"), lty=1, xlab="Id� (t)",ylab="Egyedsz�m (N)")

