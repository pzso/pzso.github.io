###############################################
## Diszkr�t logisztikus kompet�ci� dinamika  ##
## FORR�S:                                   ##
## M. Henry H. Stevens (2009)                ##
## Primer of Ecology with R                  ##
## 5. fejezet                                ##
###############################################

alphs <- matrix(c(0.01, 0.005, 0.008, 0.01),
 ncol = 2, byrow = TRUE)                     # Lotka-Volterra modell param�terei
maxt <- 20                                   # gener�ci�k sz�ma
N <- matrix(nrow = maxt + 1, ncol = 2)       # popul�ci�k m�rete
N[1, ] <- c(10, 10)                          # kezdeti egyedsz�m

#----- Lotka-Volterra modell, az egyedsz�m v�ltoz�sa (diszkr�t eset)
dlvcomp2 <- function(N, alpha, rd = c(1, 1)) {
  N1.t1 <- N[1] + rd[1] * N[1] * (1 - alpha[1, 1] * N[1] - alpha[1, 2] * N[2])
  N2.t1 <- N[2] + rd[2] * N[2] * (1 - alpha[2, 1] * N[1] - alpha[2, 2] * N[2])
  c(N1.t1, N2.t1)
}

#----- MAIN
for(i in 1:maxt)
 N[i+1,] <- dlvcomp2(N[i, ], alphs)           # dinamika

#----- �BR�ZOL�S
matplot(0:maxt, N, type = "l", col = c("black","blue"), lty=1, ylim = c(0, 110),
  xlab="Id� (t)",ylab="Egyedsz�m (N)")
abline(h = 1/alphs[1, 1], lty = 3)            #K �rt�ke
text(0, 1/alphs[1, 1], "K", adj = c(0, 0))
legend("bottomright", c("1. faj", "2. faj"), lty=1, col=c("black","blue"))
