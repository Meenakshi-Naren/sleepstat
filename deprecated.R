intervalo_confianca <- function(x,confianca){ # Deprecated: mostra o grafico preenchendo a medida que a confianca aumenta.
    X = mean(x,na.rm=TRUE)
    E = (qnorm(confianca,mean=mean(x,na.rm=TRUE),sd=sd(x,na.rm=TRUE))*sd(x,na.rm=TRUE))/sqrt(length(x))
    esquerda = X-E
    direita = X+E    
    return(c(esquerda,direita))
}

graf_confianca <- function(values,confianca){ # Deprecated http://msenux.redwoods.edu/math/R/StandardNormal.php
    # curva
    x = seq(min(values,na.rm=TRUE),max(values,na.rm=TRUE),length=200)
    y = dnorm(x,mean=mean(values,na.rm=TRUE),sd=sd(values,na.rm=TRUE))
    plot(x,y,type="l",lwd=2,col="red")
    
    # sombra
    int = intervalo_confianca(values,confianca)
    a = int[1]
    b = int[2]
    x = seq(a,b,length=200)
    y = dnorm(x,mean=mean(values,na.rm=TRUE),sd=sd(values,na.rm=TRUE))
    polygon(c(a,x,b),c(0,y,0),col="gray")
    #return(pnorm(b,mean=mean(values,na.rm=TRUE),sd=sd(values,na.rm=TRUE))-pnorm(a,mean=mean(values,na.rm=TRUE),sd=sd(values,na.rm=TRUE)))
}