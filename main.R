# Data Loader
    setwd("~/Dropbox/CIn/Estatistica/Projetos/ProjetoR/Scripts") # Diretorio de trabalho
    dataset <- read.table ("data/datafile.dat", header= TRUE) # le os dados do arquivo e aloca em uma tabela R
    
    dataset[dataset==-999.0] <- NA # Transforma os valores -999.0(valores ausentes do conjunto) em nao aplicaveis

# Variavel quantitativa escolhida: >>>>>>>> Total Sleep time <<<<<<<<<


    # Medidas de posição e de dispersão:
        media = mean(dataset$Total_sleep.hrs.day. , na.rm=TRUE) # media = 10.53276
        mediana = median(dataset$Total_sleep.hrs.day. , na.rm=TRUE) # mediana = 10.45

        # R nao tem uma funcao predefinida para a moda, por isso criamos a nossa propria em util.R
        source("util.R")
        moda = moda(dataset$Total_sleep.hrs.day.) # moda = 12.5

        variancia = var(dataset$Total_sleep.hrs.day.,na.rm=TRUE) # variancia = 21.22224
        desviopadrao = sd(dataset$Total_sleep.hrs.day.,na.rm=TRUE) # desviopadrao = 4.60676

        # R nao tem uma funcao predefinida para a assimetria nem curtose, por isso criamos as nossas em util.R
        assimetria = assimetria(dataset$Total_sleep.hrs.day.) # assimetria = 0.1960028
        curtose = curtose(dataset$Total_sleep.hrs.day.) # curtose = 2.43348

        pquartil = quantile(dataset$Total_sleep.hrs.day.,na.rm=TRUE,0.25) # primeiro_quartil = 8.05
        tquartil = quantile(dataset$Total_sleep.hrs.day.,na.rm=TRUE,0.75) # terceiro_quartil = 13.2

    # Histogramas com polígono de frequência e tabelas de frequência
        # Metodos disponiveis: Sturges,Scott,FD
        tabFreq1 = tabela_freq(dataset$Total_sleep.hrs.day.,method="Sturges") 
        tabFreq2 = tabela_freq(dataset$Total_sleep.hrs.day.,method="Scott")
        histograma1 = histograma(dataset$Total_sleep.hrs.day.,method="Sturges")
        histograma2 = histograma(dataset$Total_sleep.hrs.day.,method="Scott")

    # Analise da distribuição da variável
        points = qqnorm(dataset$Total_sleep.hrs.day.)
        line = qqline(dataset$Total_sleep.hrs.day.,col=2)
        normalTest = shapiro.test(dataset$Total_sleep.hrs.day.)
        # Resultado: os dados seguem uma distribuicao normal
    # Intervalos de Confianca
        intConfianca1 = int_conf(dataset$Total_sleep.hrs.day.,conf=0.95)
        intConfianca2 = int_conf(dataset$Total_sleep.hrs.day.,conf=0.80)
        grafConfianca1 = graf_int_conf(dataset$Total_sleep.hrs.day.,conf=0.95)
        grafConfianca2 = graf_int_conf(dataset$Total_sleep.hrs.day.,conf=0.80)

    # Testes de Hipotese
        # Sera que a maioria dos animais dormem, em media, mais que o homem típico(8hrs)? [TRUE] (http://super.abril.com.br/cotidiano/homem-unico-animal-dormir-tirada-so-437248.shtml)
            t1 = t.test(dataset$Total_sleep.hrs.day.,mu=8,alternative="greater",conf.level=0.95) # [TRUE]
        # Os animais com cerebro maior dormem mais tempo que os animais com o cerebro menor? [TRUE] (http://www.newscientist.com/article/dn14164-why-brainy-animals-need-more-rem-sleep-after-all.html)
            brainyless = dataset[with(dataset,order(Brain_Weight.g.)),]$Nondreaming_sleep.hrs.day.[1:32]
            brainy = dataset[with(dataset,order(Brain_Weight.g.)),]$Nondreaming_sleep.hrs.day.[33:62]
            t2 = t.test(brainy,brainyless,alternative="greater",conf.level=0.95)
        # Usaremos o package "visualizationTools" para possibilitar a visualizacao dos testes(http://www.inside-r.org/packages/cran/visualizationTools/docs/plot.htest)
            library(visualizationTools)
            graft1 = plot(t1)
            graft2 = plot(t2)
        # One often "rejects the null hypothesis" when the p-value is less than the predetermined significance level 
        interpretacao1 = interpretacao_teste(t1)
        interpretacao2 = interpretacao_teste(t2)

# Variavel qualitativa escolhida: >>>>>>>> Overall Danger <<<<<<<<<
    # Tabela de frequencia absoluta absoluta e relativa
        tabFreq3 = table(dataset$Overall_Danger_Index.1.5.)
        tabfreqrel = transform(tabFreq3, cumFreq = cumsum(Freq), relative = prop.table(Freq)) #Tabela de frequencia relativa

    # Graficos de Hastes
        grafFreq = plot(tabFreq3)
    # Grafico de Torta
        grafpie = graf_pie(tabFreq3,"Índice Global de Predação")
    # Grafico de Frequencia Relativa
        grafFreqRel = plot(tabfreqrel)

    # Analise do comportamento das variaveis qualitativas e quantitativas
        # Usaremos o package "SQL on DataFrames" para tornar mais fácil a analise comparativa
            library(sqldf)
        # Conclusão: quem tem medo dorme pouco!(indice de predacao influencia muito no tempo total de sono)    
            query = sqldf("SELECT Overall_Danger_Index_1_5_,AVG(Total_sleep_hrs_day_) FROM dataset GROUP BY Overall_Danger_Index_1_5_")






