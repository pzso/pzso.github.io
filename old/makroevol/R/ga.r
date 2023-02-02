############### GENETIKAI ALGORITMUS ##################
# Forr�s:                                             #
#  http://fishyoperations.com/r/genetic-algorithms-a-simple-r-example/
# P�lda: a h�tizs�k probl�ma megold�sa                #
# Egyedek most bin�ris kromosz�m�k                    #
# L�sd genalg csomag                                  #
# Param�tereK:                                        #
#  d: a tulajdons�gok le�r�sa a g�nek sorrendj�ben a  #
#     kromosz�m�n, egyik vektora a t�l�l�s (surv)     #
#     fitnesz: t�l�l�s �sszeg                         #
#  wlimit: s�ly fels� korl�t (optimaliz�ci�s          #
#     k�nyszer)                                       #
#  maxt: gener�ci�k sz�ma                             #
#######################################################

library(genalg)

d <- data.frame(
 item = LETTERS[1:7],
 surv = c(10, 20, 15, 2, 30, 10, 30), 
 weight = c(1, 5, 10, 1, 7, 5, 1)
)
wlimit <- 20
maxt <- 100

#---- P�lda: -----------------------------------------
# chromosome = c(1, 0, 0, 1, 1, 0, 0) #egy bin�ris kromosz�ma (egyed) p�lda
# d[chromosome == 1, ]                #mit visz�nk magunkkal a h�tizs�kban
# sum(chromosome * d$survivalpoints)  #a t�l�l�si �rt�k (fitnesz)
#------------------------------------------------------

#---- Kromosz�ma ki�rt�kel�s (fitnesz):
evalFunc <- function(x) {
 if (sum(x*d$weight) > wlimit) return(0)
 else return(-sum(x*d$surv))
}

#---- A modell:
GA <- rbga.bin(size = 7, popSize = 200, iters = maxt, 
               mutationChance = 0.01, elitism = 100,
               evalFunc = evalFunc)
cat(summary(GA))
solution = c(1, 1, 0, 1, 1, 1, 1)  # az optim�lis kromosz�ma
#d[solution == 1, ]
print(paste(sum(solution*d$surv),"/",sum(d$surv))) # optimum/k�nyszer n�lk�li

#---- �br�zol�sa (vagy l�sd monitorFunc param�ter, ?rbga.bin)
plot(1:maxt,-GA$mean[1:maxt],ylim=c(min(-GA$mean),max(-GA$mean)),
     xlab="Gener�ci�", ylab="Fitnesz",type="l",lwd=2);
lines(1:maxt,-GA$best[1:maxt],lwd=2,col="blue")
legend("bottomright",legend=c("popul�ci� �tlag","optim�lis"),
       lwd=2,col=c("black","blue"))
