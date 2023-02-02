## Szekvencia evol�ci� szimul�ci�ja a JC69 modellel
## Forr�s: Sipos Botond, phylosim csomag dokument�ci� 
## (Example V2.1.1: Simulating substitutions under the JC69 model - "unrolled" example)
## https://github.com/sbotond/phylosim/blob/master/examples/example_V2.1.2.R

library("phylosim")

root.seq<-NucleotideSequence(length=30)     # gy�k�r objektum (30 bp hossz� szekvencia lesz)
p<-JC69()                                   # szubsztit�ci�s modell (folyamat objektum)
attachProcess(root.seq,p)                   # �sszekapcsoljuk a kett�t
sampleStates(root.seq)                      # gy�k�r szekvencia defini�l�sa (modellnek megfelel� egyens�lyi eloszl�s)
print(root.seq)
tree<-read.tree(                            # a filogenetikai fa (ape csomaggal)
  text="(A:0.9,(B:0.8,((C:0.5,((D:0.1,E:0.1):0.2,(F:0.1,G:0.1):0.3):0.4):0.2,(H:0.3,(I:0.1,J:0.1):0.3):0.6):0.1):0.2);
")
#tree<-read.tree("minta.nwk")
plot(tree); nodelabels()

sim<-PhyloSim()                             # a PhyloSim objektum l�trehoz�sa
sim$phylo<-tree
sim$rootSeq<-root.seq

Simulate(sim)                               # a szimul�ci�

data.frame(seq=                             # az eredm�ny: sim$alignment
  apply(sim$alignment,1,paste,collapse="")
)
plot(sim,num.pages=1,plot.ancestors=F)

