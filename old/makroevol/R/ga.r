############### GENETIKAI ALGORITMUS ##################
# Forrás:                                             #
#  http://fishyoperations.com/r/genetic-algorithms-a-simple-r-example/
# Példa: a hátizsák probléma megoldása                #
# Egyedek most bináris kromoszómák                    #
# Lásd genalg csomag                                  #
# ParamétereK:                                        #
#  d: a tulajdonságok leírása a gének sorrendjében a  #
#     kromoszómán, egyik vektora a túlélés (surv)     #
#     fitnesz: túlélés összeg                         #
#  wlimit: súly felsõ korlát (optimalizációs          #
#     kényszer)                                       #
#  maxt: generációk száma                             #
#######################################################

library(genalg)

d <- data.frame(
 item = LETTERS[1:7],
 surv = c(10, 20, 15, 2, 30, 10, 30), 
 weight = c(1, 5, 10, 1, 7, 5, 1)
)
wlimit <- 20
maxt <- 100

#---- Példa: -----------------------------------------
# chromosome = c(1, 0, 0, 1, 1, 0, 0) #egy bináris kromoszóma (egyed) példa
# d[chromosome == 1, ]                #mit viszünk magunkkal a hátizsákban
# sum(chromosome * d$survivalpoints)  #a túlélési érték (fitnesz)
#------------------------------------------------------

#---- Kromoszóma kiértékelés (fitnesz):
evalFunc <- function(x) {
 if (sum(x*d$weight) > wlimit) return(0)
 else return(-sum(x*d$surv))
}

#---- A modell:
GA <- rbga.bin(size = 7, popSize = 200, iters = maxt, 
               mutationChance = 0.01, elitism = 100,
               evalFunc = evalFunc)
cat(summary(GA))
solution = c(1, 1, 0, 1, 1, 1, 1)  # az optimális kromoszóma
#d[solution == 1, ]
print(paste(sum(solution*d$surv),"/",sum(d$surv))) # optimum/kényszer nélküli

#---- Ábrázolása (vagy lásd monitorFunc paraméter, ?rbga.bin)
plot(1:maxt,-GA$mean[1:maxt],ylim=c(min(-GA$mean),max(-GA$mean)),
     xlab="Generáció", ylab="Fitnesz",type="l",lwd=2);
lines(1:maxt,-GA$best[1:maxt],lwd=2,col="blue")
legend("bottomright",legend=c("populáció átlag","optimális"),
       lwd=2,col=c("black","blue"))
