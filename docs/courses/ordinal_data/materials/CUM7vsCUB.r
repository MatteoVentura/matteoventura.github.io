# esplorazione relazione CUM5 vs CUB
# vedi file appunti

rm(list=ls())
setwd("G:\\.shortcut-targets-by-id\\1x4AvgNFShP3OtYW2ZuhN0heEzRb-tpu9\\CUB\\CUM\\CUM Ambra Matteo")

piB <- 16*3*(1579-15*sqrt(555))/107653
delta <- 64*(1579-15*sqrt(555))/107653 	
# se delta <- 64*(1579+15*sqrt(555))/107653	i valori accettabili di xiD e xiU non si sovrappongono
piM <- piB/delta
piM

# piB <- 0.8
# piM <- 0.8
# piM <- (1579*piB + 15*sqrt(555)*piB)/1408
# piM <- 0.699
# piM <- piB*(11/16)
# piM>piB -> no problemi

xi <- seq(0,1,0.001)
xiD <- (piB/piM*xi^6 + (piM-piB)/(7*piM))^(1/3)
xiU <- (piB/piM*(1-xi)^6 + (piM-piB)/(7*piM))^(1/3)
sel <- which((xiD+xiU)>1)
ximin <- xi[length(sel)/2]
ximax <- 1-ximin
xiD[sel] <- NA
xiU[sel] <- NA



#if(piB>piM){
#soglia1 <- ((piB-piM)/(7*piB))^(1/6)
#print(soglia1)
#print(paste("xi=>",soglia1,sep=""))
#ximin <- soglia1

#soglia2 <- -((piB-piM)/(7*piB))^(1/6)+1
#print(soglia2)
#print(paste("xi<=",soglia2,sep=""))
#ximin <- soglia2
#}


Pcub1 <- piB*xi^6 + (1-piB)/7
Pcum1 <- piM*xiD^3 + (1-piM)/7

plot(xi,Pcub1,type='l',ylim=c(0,0.4),col='orange',lty='dashed',lwd=2)
lines(xi,Pcum1,col='orange',lwd=2)

Pcub7 <- piB*(1-xi)^6 + (1-piB)/7
Pcum7 <- piM*xiU^3 + (1-piM)/7

lines(xi,Pcub7,col='grey',lty='dashed',lwd=2)
lines(xi,Pcum7,col='grey',lwd=2)

###############################################

Pcub2 <- 6*piB*xi^5*(1-xi) + (1-piB)/7
Pcum2 <- 3*piM*xiD^2*(1-xiD-xiU) + (1-piM)/7

lines(xi,Pcub2,type='l',col='red',lty='dashed',lwd=2)
lines(xi,Pcum2,col='red',lwd=2)

Pcub3 <- 15*piB*xi^4*(1-xi)^2 + (1-piB)/7
Pcum3 <- 3*piM*xiD*((1-xiD-xiU)^2 + xiD*xiU) + (1-piM)/7

lines(xi,Pcub3,col='green',lty='dashed',lwd=2)
lines(xi,Pcum3,col='green',lwd=2)

Pcub4 <- 20*piB*xi^3*(1-xi)^3 + (1-piB)/7
Pcum4 <- piM*(1-xiD-xiU)*((1-xiD-xiU)^2 + 6*xiD*xiU) + (1-piM)/7

lines(xi,Pcub4,col='blue',lty='dashed',lwd=2)
lines(xi,Pcum4,col='blue',lwd=2)


Pcub5 <- 15*piB*xi^2*(1-xi)^4 + (1-piB)/7
Pcum5 <- 3*piM*xiU*((1-xiD-xiU)^2 + xiD*xiU) + (1-piM)/7

lines(xi,Pcub5,col='violet',lty='dashed',lwd=2)
lines(xi,Pcum5,col='violet',lwd=2)

Pcub6 <- 6*piB*xi*(1-xi)^5 + (1-piB)/7
Pcum6 <- 3*piM*xiU^2*(1-xiD-xiU) + (1-piM)/7

lines(xi,Pcub6,type='l',col='brown',lty='dashed',lwd=2)
lines(xi,Pcum6,col='brown',lwd=2)


library(ggplot2)
library(latex2exp)
library(gridExtra)

X <- data.frame(xi,Pcub1,Pcub2,Pcub3,Pcub4,Pcub5,Pcub6,Pcub7,Pcum1,Pcum2,Pcum3,Pcum4,Pcum5,Pcum6,Pcum7)
X[X<0] <- NA

r2 <- ggplot(X, aes(x=xi,y=Pcub2))+
  geom_line(col = "red", size = 1) + 
geom_vline(xintercept = 0.5, linetype = "dashed", color='red')+
	#geom_vline(xintercept = ximin, linetype = "dashed")+ geom_vline(xintercept = ximax, linetype = "dashed")+
  geom_line(aes(y = Pcum2), colour = "red", linetype = "dashed", size = 1)+
  labs(
    title = TeX(r"( $r=2$ )"),
    x = TeX(r"( $\xi$ )"),
    y = TeX(r"( $P_{CUB}, P_{CUM}$ )") ) + theme_classic()

r3 <- ggplot(X, aes(x=xi,y=Pcub3))+
  geom_line(col = "green", size = 1) + 
geom_vline(xintercept = 0.5, linetype = "dashed", color='green')+
	#geom_vline(xintercept = ximin, linetype = "dashed")+ geom_vline(xintercept = ximax, linetype = "dashed")+
  geom_line(aes(y = Pcum3), colour = "green", linetype = "dashed", size = 1)+
  labs(
    title = TeX(r"( $r=3$ )"),
    x = TeX(r"( $\xi$ )"),
    y = TeX(r"( $P_{CUB}, P_{CUM}$ )") ) + theme_classic()


r4 <- ggplot(X, aes(x=xi,y=Pcub4))+
  geom_line(col = "blue", size = 1) + 
geom_vline(xintercept = 0.5, linetype = "dashed", color='blue')+
	#geom_vline(xintercept = ximin, linetype = "dashed")+ geom_vline(xintercept = ximax, linetype = "dashed")+
 geom_line(aes(y = Pcum4), colour = "blue", linetype = "dashed", size = 1)+
  labs(
    title = TeX(r"( $r=4$ )"),
    x = TeX(r"( $\xi$ )"),
    y = TeX(r"( $P_{CUB}, P_{CUM}$ )") ) + theme_classic()

r5 <- ggplot(X, aes(x=xi,y=Pcub5))+
  geom_line(col = "purple", size = 1) + 
geom_vline(xintercept = 0.5, linetype = "dashed", color='purple')+
	#geom_vline(xintercept = ximin, linetype = "dashed")+ geom_vline(xintercept = ximax, linetype = "dashed")+
 geom_line(aes(y = Pcum5), colour = "purple", linetype = "dashed", size = 1)+
  labs(
    title = TeX(r"( $r=5$ )"),
    x = TeX(r"( $\xi$ )"),
    y = TeX(r"( $P_{CUB}, P_{CUM}$ )") ) + theme_classic()

r6 <- ggplot(X, aes(x=xi,y=Pcub6))+
  geom_line(col = "cyan", size = 1) + 
geom_vline(xintercept = 0.5, linetype = "dashed", color='cyan')+
	#geom_vline(xintercept = ximin, linetype = "dashed")+ geom_vline(xintercept = ximax, linetype = "dashed")+
 geom_line(aes(y = Pcum6), colour = "cyan", linetype = "dashed", size = 1)+
  labs(
    title = TeX(r"( $r=6$ )"),
    x = TeX(r"( $\xi$ )"),
    y = TeX(r"( $P_{CUB}, P_{CUM}$ )") ) + theme_classic()

grid.arrange(r2,r3,r4,r5,r6,ncol=3)

pdf(file="fig5a.pdf",width=10,height=8,paper="special")
grid.arrange(r2,r3,r4,r5,r6,ncol=3)
dev.off()

# Fig 1a - delta = 0.7 - piB <- 0.1
# Fig 5a - delta = espressione 1 trovata da Matteo per risolvere equazioni 2 e 6 - piB <- 16*3*(1579-15*sqrt(555))/107653



