#Funcoes proprias que criamos para facilitar nosso projeto

moda <- function(x) {
  x  <- x[!is.na(x)] 
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

assimetria <- function(x){ #assimetria pelo coeficiente de Pearson (http://www.tc3.edu/instruct/sbrown/stat/shape.htm#SkewnessCompute)
    m3 <- mean((x-mean(x,na.rm=TRUE))^3,na.rm=TRUE)
    m2 <- mean((x-mean(x,na.rm=TRUE))^2,na.rm=TRUE)  
    y <- m3/((m2)^(3/2))
    return(y)
}

curtose <- function(x){ #curtose pelo coeficiente de Pearson
    m4 <- mean((x-mean(x,na.rm=TRUE))^4,na.rm=TRUE)
    m2 <- mean((x-mean(x,na.rm=TRUE))^2,na.rm=TRUE)  
    y <- m4/(m2)^2
    return(y)
}

poligono <- function(tmp){# Poligono de frequencia
    poligono = lines(c(min(tmp$breaks), tmp$mids, max(tmp$breaks)),c(0, tmp$counts, 0),type = "l",col = "royalblue")
    return(poligono)
}

histograma <- function(x,method="Sturges"){ # Monta o histograma e o poligono de frequencias de acordo com um metodo de divisao de classes
    histo = hist(x,breaks=method,border="gray", col="gray90")
    poli = (poligono(histo)) 
    return(c(histo,poli))
}

tabela_freq <- function(x,method="Sturges"){
    # Metodos(http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/hist.html)
    if(method == "Sturges"){
        md = nclass.Sturges(x)
    }else if(method == "Scott"){
        md = nclass.scott(na.omit(x))
    }else if(method == "FD"){
        md = nclass.FD(na.omit(x))   
    }
    int = cut(x, breaks = md, include.lowest = TRUE) # Intervalos
    return(data.frame(table(int))) #tabela de frequencias
}

int_conf <- function(x,conf){ # Intervalo de Confianca
    return(t.test(x,conf.level=conf)$conf.int[1:2])
}

graf_int_conf <- function(val,conf){ # Grafico do intervalo de confianca
    # Curva
    x = seq(min(val,na.rm=TRUE),max(val,na.rm=TRUE),length=200)
    y = dnorm(x,mean=mean(val,na.rm=TRUE),sd=sd(val,na.rm=TRUE))
    plot(x,y,type="l",lwd=2,col="red")
    
    # Sombreado
    int = int_conf(val,conf)
    x = seq(int[1],int[2],length=200)
    y = dnorm(x,mean=mean(val,na.rm=TRUE),sd=sd(val,na.rm=TRUE))
    polygon(c(int[1],x,int[2]),c(0,y,0),col="gray")
}

interpretacao_teste <- function(teste){ # Interpreta o resultado do teste através do p-value
    # One often "rejects the null hypothesis" when the p-value is less than the predetermined significance level 
    
    if(teste$p.value < (1- attr( t1$conf.int, "conf.level" ))){
        return("Rejeita a hipotese nula.")
    }else{
        return("Nao pode rejeitar a hipotese nula.")
    } 
}

graf_pie <- function(slices,name){
    lbls <- c("Pouco(1)","Não Pouco","Moderado(3)","Não Muito(4)","Muito(5)")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    pie(slices,labels = lbls, col=rainbow(length(lbls)),
         main=name)
}