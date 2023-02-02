##################################################
## Forrás:                                      ##
## Soetaert and Herman (2008)                   ##
## A practical guide to ecological modelling    ##
## 10. fejezet, Dinamikus programozás           ##
## Folt szelekciós modell (Clark & Mangel 1999) ##
##################################################

#--- Paraméterek (állapot és idõ):
x_crit  <- 0                 # kritikus tömeg a túléléshez
x_max   <- 30                # maximális tömeg
x_rep   <- 4                 # kritikus tömeg a szaporodáshoz
x_cat   <- x_crit:x_max      # tömeg kategóriák (lehetséges állapotok)
nmass   <- length(x_cat)     # tömeg kategóriák száma
maxt    <- 20                # idõ (periódusok száma) 
times   <- 1:(maxt-1)        # periódusok

#--- Foltok:
npatch  <- 3                    # foltok száma (habitat)
psurvive <- c(0.99,0.95,0.98)   # túlélési valószínûség a foltban
pfood    <- c(0.2 ,0.5 ,0 )     # táplálkozási valószínûség
cost     <- c(1 ,1 ,1 )         # folt költség (metabolikus)
feedgain <- c(2 ,4 ,0 )         # nyereség (táplálkozás)
repr     <- c(0 ,0 ,4 )         # szaporodási nyereség

#--- Eredmények (változók):
f         <- matrix(nrow=maxt,ncol=nmass ,0)    # optimal fitnesz érték
bestpatch <- matrix(nrow=maxt-1,ncol=nmass-1,0) # optimális folt választás
V         <- vector(length=npatch)              # aktuális fitnesz a foltokra

#--- Az utolsó periódus végén (maxt, Mo)
fend      <- 60
kx        <- 0.25*x_max
f[maxt,]  <- fend*(x_cat-x_crit)/(x_cat-x_crit+kx)

#--- Fitnesz adott x és t értékre
fitness <- function(x,t) {
 xx <- pmin(x ,x_max) 
 xx <- pmax(xx,x_crit) 
 fitness <- f[t,xx-x_crit+1]
}

#--- Optimalizációs ciklus
for (t in rev(times)) {              # idõben visszafelé
 for (x in x_cat[-1]) {              # minden állapotra (kivéve x-crit)
  dfit <- pmax(0,pmin(x-x_rep,repr)) # fitnesz nyereség a szaporodásból
  expectgain <- psurvive*(pfood*fitness(x-cost+feedgain-dfit,t+1) + 
                          (1-pfood)*fitness(x-cost-dfit,t+1))
  V <- dfit + expectgain  
  V[expectgain == 0] <- 0           # elpusztul
  f[t,x-x_crit+1]    <- max(V)       # optimális fitnesz
  bestpatch[t,x] <- which.max(V)    # optimális folt
 }
}

#--- Ábrázolás
cols <- c("black","darkblue","lightblue","white")
image(x=times,y=x_cat[-1],z=bestpatch,ylab="Tömeg",xlab="Idõ",zlim=c(0,3),
      main="Optimális folt",col=cols)
box()
legend("topleft",fill=cols,legend=c("elpusztult","1","2","3"),cex=0.8)
contour(x=1:maxt,y=x_cat,z=f,add=TRUE)
legend("topright",legend="fitnesz",lty=1,cex=0.8)
