# Instalando pacotes

if (!require(xaringan)) {
    install.packages('xaringan')
    library(xaringan)
} else{
    library(xaringan)
}


if (!require(svglite)) {
    install.packages('svglite')
    library(svglite)
} else{
    library(svglite)
}


if (!require(openxlsx)) {
    install.packages('openxlsx')
    library(openxlsx)
} else{
    library(openxlsx)
}


if (!require(readxl)) {
    install.packages('readxl')
    library(readxl)
} else{
    library(readxl)
}


if (!require(knitr)) {
    install.packages('kntir')
    library(knitr)
} else{
    library(knitr)
}
if (!require(zoo)) {
    install.packages('zoo')
    library(zoo)
} else{
    library(zoo)
}

if (!require(ggplot2)) {
    install.packages('ggplot2')
    library(ggplot2)
} else{
    library(ggplot2)
}

if (!require(plotly)) {
    install.packages('plotly')
    library(plotly)
} else{
    library(plotly)
}

if (!require(devtools)) {
    install.packages('devtools')
    library(devtools)
} else{
    library(devtools)
}

if (!require(rbcb)) {
    devtools::install_github('wilsonfreitas/rbcb')
    library(rbcb)
} else{
    library(rbcb)
}

if (!require(sidrar)) {
    install.packages('sidrar')
    library(sidrar)
} else{
    library(sidrar)
}

if (!require(imfr)) {
    install.packages('imfr')
    library(imfr)
} else{
    library(imfr)
}

if (!require(wbstats)) {
    install.packages('wbstats')
    library(wbstats)
} else{
    library(wbstats)
}

if (!require(xts)) {
    install.packages('xts')
    library(xts)
} else{
    library(xts)
}


if (!require(grid)) {
    install.packages('grid')
    library(grid)
} else{
    library(grid)
}


if (!require(ggfortify)) {
    install.packages('ggfortify')
    library(ggfortify)
} else{
    library(ggfortify)
}

if (!require(xfun)) {
    install.packages('xfun')
    library(xfun)
} else{
    library(xfun)
}

if (!require(highcharter)) {
    install.packages('highcharter')
    library(highcharter)
} else{
    library(highcharter)
}


if (!require(openssl)) {
    install.packages('openssl')
    library(openssl)
} else{
    library(openssl)
}

if (!require(stringr)) {
    install.packages('stringr')
    library(stringr)
} else{
    library(stringr)
}

if (!require(scales)) {
    install.packages('stringr')
    library(stringr)
} else{
    library(scales)
}



# Atualizando

Atualizacao <- FALSE

if (Atualizacao) {
    install.packages('xaringan')
    install.packages('svglite')
    install.packages('openxlsx')
    install.packages('readxl')
    install.packages('kntir')
    install.packages('zoo')
    install.packages('ggplot2')
    install.packages('plotly')
    install.packages('devtools')
    devtools::install_github('wilsonfreitas/rbcb')
    install.packages('sidrar')    
    install.packages('imfr')
    install.packages('wbstats')
    install.packages('xts')
    install.packages('grid')
    install.packages('ggfortify')
    install.packages('xfun')
    install.packages('highcharter')
    install.packages('openssl')
    install.packages('stringr')
    install.packages('scales')
}

Atualizacao <- NULL
