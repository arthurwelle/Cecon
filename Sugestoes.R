#' @title Faz Texto Acumulado Mes
#' @aliases FazTexto.AccMes
#' @author Arthur Welle
#' @export
#' @description Gera texto com taxa acumulada em m meses
#' @return Texto
#' @param x Serie a ser calculada a taxa
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param meses Numero de meses para acumular. Por padrao, 12 meses

FazTexto.AccMes <- function (x, digitos = 2, meses = 12)
{
    k <- 0
    for (i in 0:(meses - 1)) {
        k <- k + x[length(x) - i]
    }
    k <- format(round(k, digits = digitos), big.mark = ".", decimal.mark = ",")
    return(k)
}

#' @title Faz Texto Taxa Acumulada em M meses
#' @aliases FazTexto.TaxaAccMeses
#' @author Arthur Welle
#' @description Calcula taxa acumulada em m meses
#' @export
#' @param x Serie a ser calculada a taxa acumulada
#' @param meses Numero de meses para acumular. Por padrao, 12 meses
#' @param digitos Numero de casas decimais
#' @param texto Se TRUE retorna valor em caractere
#' @return Retorna texto da taxa acumulada em m meses

FazTexto.TaxaAccMeses <- function (x, meses = 12, digitos = 2, texto = TRUE)
{
    if (texto == TRUE) {
    m <- c(1:(length(x)))
    for (i in 1:(length(x) - (meses - 1))) {
        k <- 1
        for (j in 0:(meses - 1)) {
            k <- k * (1 + x[(i - j) + (meses - 1)]/100)
        }
        k <- (k - 1) * 100
        m[i + (meses - 1)] <- k
    }
    m[1:(meses - 1)] <- NA
    Taxa_Acum <- format(x = m[length(x)],
                        big.mark = ".",
                        decimal.mark = ",",
                        digits = digitos)
    return(Taxa_Acum)
    } else {
        m <- c(1:(length(x)))
        for (i in 1:(length(x) - (meses - 1))) {
            k <- 1
            for (j in 0:(meses - 1)) {
                k <- k * (1 + x[(i - j) + (meses - 1)]/100)
            }
            k <- (k - 1) * 100
            m[i + (meses - 1)] <- k
        }
        m[1:(meses - 1)] <- NA
        Taxa_Acum <- m[length(x)]
        return(Taxa_Acum)
    }
}

#' @title Faz Texto Taxa Anualizada
#' @aliases FazTexto.TaxaAnualizada
#' @author Arthur Welle
#' @description Cria texto para taxa anualizada
#' @export
#' @param x Serie para calcular taxa anualizada
#' @param digitos Numero de casas decimais
#' @param tempo Se mensal 12, trimestral 4. Se for xts, ts ou zoo, calcula automaticamente.
#' @param texto Se TRUE retorna caractere


FazTexto.TaxaAnualizada <- function (x, digitos = 2, texto = TRUE, tempo = 12)
{
    if (class(x) == "xts" | class(x) == "ts" | class(x) == "zoo") {
        if ((periodicity(x) %in% "monthly")[6] == TRUE) {
            k <- c(1:length(x))
            for (i in 0:length(x)) {
                k[i] <- (1 + x[i]/100)^12
            }
            k <- (k - 1) * 100
        } else if ((periodicity(x) %in% "quarterly")[6] == TRUE) {
            k <- c(1:length(x))
            for (i in 0:length(x)) {
                k[i] <- (1 + x[i]/100)^4
            }
            k <- (k - 1) * 100
        } else if ((periodicity(x) %in% "yearly")[6] == TRUE) {
            warning("Série já anual")
        }  else {
            warning("Rever Série")
        }
    } else {
    k <- c(1:length(x))
    for (i in 0:length(x)) {
        k[i] <- (1 + x[i]/100)^tempo
    }
    k <- (k - 1) * 100
    }
    if (texto == TRUE) {
    Taxa_Anuali <- format(k[length(x)],
                          big.mark = ".",
                          decimal.mark = ",",
                          digits = digitos)
    return(Taxa_Anuali)
    } else {
        if (class(x) == "xts" | class(x) == "ts" | class(x) == "zoo") {
        return(coredata(k[length(x)]))
        } else {
        return(k[length(x)])
        }
    }
}

#' @title Faz Texto Ultimo Valor
#' @aliases FazTexto.UltimoValor
#' @author Arthur Welle
#' @description Cria uma funcao que retorna o ultimo valor de uma serie x.
#' @export
#' @param x Serie a ser analisada
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param texto Se TRUE, retorna caractere
#' @return Texto



FazTexto.UltimoValor <- function (x, digitos = 2, texto = TRUE)
{
    if (texto == TRUE) {
    k <- format(round(x[length(x)], digits = digitos), big.mark = ".",
        decimal.mark = ",")
    return(k)
    } else {
        k <- x[length(x)]
        return(k)
    }
}

#' @title Faz Texto Valor Meses Antes
#' @aliases FazTexto.ValorMesAntes
#' @author Arthur Welle
#' @export
#' @description Retorna Valor de m meses anteriores
#' @param x Serie a ser analisada
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param meses Numero de meses a serem analisados
#' @param texto Se TRUE, retorna caractere
#' @return Texto

FazTexto.ValorMesAntes <- function (x, digitos = 2, meses = 12, texto = TRUE)
{
    if (texto == TRUE) {
    k <- format(round(x[length(x) - (meses - 1)], digits = digitos),
        big.mark = ".", decimal.mark = ",")
    return(k)
    } else {
        k <- x[length(x) - (meses - 1)]
        return(k)
    }
}

#' @title Faz Texto Variacao m meses percentual
#' @aliases FazTexto.VarMes.porc
#' @author Arthur Welle
#' @export
#' @description Cria uma funcao que retorna a variacao percentuais de uma serie nos ultimos m meses
#' @param x Serie a ser analisada
#' @param digitos Numero de casas decimias. Por padrao, 2
#' @param meses Numero de meses a serem calculados. Por padrao, 12
#' @param texto Se TRUE retorna caractere
#' @param porcentagem se TRUE, incui % ao numero
#' @return Texto



FazTexto.VarMes.porc <- function (x, digitos = 2, meses = 12, porcentagem = FALSE, texto = TRUE)
{
    if (texto == TRUE) {
    k <- format(round((((x[length(x)]/x[length(x) - (meses -
        1)]) - 1) * 100), digits = digitos), big.mark = ".",
        decimal.mark = ",")
    if (porcentagem == TRUE) {
    k <- paste0(k, "%")
    return(k)
    } else {
        return(k)
    }
    } else {
       k <- ((x[length(x)]/x[length(x) - (meses - 1)]) - 1)*100
        return(k)
    }
}


#' @title FazTexto.N_Indice
#' @author Arthur Welle
#' @details Corrigir: Alterar nome
#' @description Cria funcao para criar numero indice de uma serie mensal de inflação (para usar em deflacionamentos até para a ultima data)
#' @param x Serie temporal
#' @return Numero indice
#' @keywords Texto




FazTexto.N_Indice <- function(x)
{
    k <- c(1:length(x))
    k[1] <- (1 + x[1]/100)
    for (i in 2:length(x)) {
        k[i] <- (1 + x[i]/100) * k[i - 1]
    }
    k <- k[length(k)/k]
    return(k)
}

#' @title Faz Texto Variacao M meses absoluta
#' @aliases FazTexto.VarMes.Abs
#' @author Arthur Welle
#' @export
#' @description Cria uma funcao que retorna a variação absoluta (em pontos percentuais se ja for uma serie percental) de uma serie no ultimo ano
#' @param x Serie percentual
#' @param digitos Numero de casas decimais. Por padrao, 2
#' @param meses Numero de mesesa serem calculados.Por padrao, 12
#' @param texto Se TRUE, retorna valor em caractere
#' @return Texto

FazTexto.VarMes.Abs <- function (x, digitos = 2, meses = 12, texto = TRUE)
{
    if (texto == TRUE) {
    k <- format(round(x[length(x)] - x[length(x) - (meses - 1)],
        digits = digitos), big.mark = ".", decimal.mark = ",")
    return(k)
    } else {
        k <- x[length(x)] - x[length(x) - (meses - 1)]
        return(k)
    }
}

#' @title add_logo
#' @aliases add_logo
#' @author Arthur Welle
#' @description Adiciona logo do cecon aos graficos
#' @import ggplot2
#' @import plotly
#' @import dplyr
#' @param grafico Grafico gerado pelo ggplot. Por padrao, usa o ultimo grafico gerado
#' @return Um grafico interativo em html (plotly)
#' @export

add_logo <- function (grafico = last_plot())
{
    grafico %>% ggplotly() %>% layout(images = list(list(source = "http://i.imgur.com/2e3FQaz.png",
        xref = "paper", yref = "paper", x = 0.02, y = 1, sizex = 0.25,
        sizey = 0.25, opacity = 0.5))) %>% config(displayModeBar = TRUE)
}

#' @title ajuste_xts
#' @aliases ajuste_xts
#' @author Gabriel Petrini
#' @description Transforma os dados em xts para utilizar nas demais series do pacote
#' @import xts
#' @import dplyr
#' @import zoo
#' @param dados Serie a ser tratada
#' @param col_data Coluna da serie que possui as datas. Por padrao, usa-se a coluna 1
#' @param col_dados Coluna(s) dos dados. Por padrao, seleciona-se a segunda coluna
#' @param remover_NA Se TRUE, remove os linhas que contem NAs. Por padrao, remover_NA = FALSE
#' @return Serie em xts
#' @export

ajuste_xts <- function (dados, col_data = 1, col_dados = 2, remover_NA = FALSE)
{
    if ("xts" %in% class(dados) | "zoo" %in% class(dados)) {
        message("Serie esta em xts/zoo, funcao ajuste_xts nao necessaria")
        if (sum(is.na(dados)) != 0 & remover_NA == FALSE) {
            message("Series contem NAs, use remover_NA = TRUE",
                call. = FALSE)
        }
        if (remover_NA == FALSE) {
            xts(x = coredata(dados), order.by = as.Date(index(dados)))
        }
        else {
            sem_NA <- !is.na(coredata(dados))
            xts(x = coredata(dados[sem_NA]), order.by = as.Date(index(dados[sem_NA])))
        }
    }
    else if ("ts" %in% class(dados)) {
        dados <- as.xts(dados)
    }
    else {
        if (sum(is.na(dados)) != 0 & remover_NA == FALSE) {
            message("Series contem NAs, use remover_NA = TRUE",
                call. = FALSE)
        }
        if (remover_NA == TRUE) {
            teste_NA <- !(is.na(dados[, col_dados]))
            dados_sNA <- dados[teste_NA, ]
            proxy <- as.data.frame(dados_sNA)
            proxy_data <- as.Date(proxy[, col_data], origin = proxy[1,
                col_data])
            xts(x = proxy[, col_dados], order.by = proxy_data)
        }
        else {
            proxy <- as.data.frame(dados)
            proxy_data <- as.Date(proxy[, col_data], origin = proxy[1,
                col_data])
            xts(x = proxy[, col_dados], order.by = proxy_data)
        }
    }
}

#' @title Grafico CECON
#' @aliases grafico_cecon
#' @author Gabriel Petrini
#' @description Gera grafico interatico com logo do cecon
#' @import ggplot2
#' @import xts
#' @import plotly
#' @import dplyr
#' @param dado_xts Dado em xts
#' @param logo Se TRUE, inclui logo do CECON
#' @param FUN Tipo de função que gera gráfico. Padrao grafico_padrao
#' @param ... Argumentos adicionais para a funcao de grafico
#' @return Grafico interativo em html
#' @export
grafico_cecon <-  function (dado_xts, logo = TRUE, FUN = grafico_padrao, ...)
{
    grafico <- dado_xts %>% FUN(...)
    if (logo == TRUE) {
        grafico_logo <- grafico %>% add_logo()
        return(grafico_logo)
    }
    else {
        return(grafico)
    }
}

#' @title  Grafico Padrao
#' @aliases grafico_padrao
#' @author Gabriel Petrini
#' @description Gera um grafico com um template basico
#' @import xts
#' @import dplyr
#' @import ggplot2
#' @param dado_xts Objeto em xts ou que possa ser convertido para xts pela funcao ajuste_xts()
#' @param tipo_grafico Tipo de grafico a ser plotado. Por padrao, grafico de linha
#' @param titulo Caractere que especifica qual o titulo do grafico
#' @param fonte Caractere que espeficia a fonte. Nao e necessario escrever "Fonte: "
#' @param x_titulo Caractere com titulo do eixo x
#' @param y_titulo Caractere com titulo do eixo y
#' @param tema Tema do grafico. Por padrao, utiliza-se theme_classic()
#' @param quebra_data Periodo para seccionar o eixo x. Por padrao, e dividido por ano, isto e, "1 year"
#' @param label_data Formato em que e disponibilizada as datas do eixo x. Por padrao, e apresentada em anos, isto e, "%Y"
#' @param pontos Argumento para especificar se devem conter pontos no grafico. Por padrao, nao apresenta pontos, isto e, pontos = 0
#' @return Retorna um grafico ggplot
#' @export

grafico_padrao <- function (dado_xts, tipo_grafico = geom_line(size = 1), titulo = NULL,
          fonte = NULL, x_titulo = NULL, y_titulo = NULL, tema = theme_classic(),
          quebra_data = "1 year", label_data = "%Y", pontos = 0){
    dado_xts <- as.xts(dado_xts)
    ggplot(data = dado_xts,
           aes(x = as.Date(index(dado_xts)),
               y = coredata(dado_xts))) +
        tipo_grafico + labs(x = x_titulo,
                            y = y_titulo,
                            title = titulo,
                            caption = paste0("Fonte: ", fonte)) +
        tema + theme(panel.border = element_blank(),
                     axis.line = element_line(colour = "black", size = 0.7),
                     axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 14),
                     axis.text.y = element_text(angle = 0, hjust = 0, vjust = 0.5, size = 14),
                     text = element_text(size = 10, family = "TT Times New Roman")) +
        scale_x_date(date_breaks = quebra_data,
                     date_labels = label_data) +
        geom_point(size = pontos)
}

#' @title Grafico Rapido
#' @aliases grafico_rapido
#' @author Gabriel Petrini
#' @description Gera um grafico interativo com logo do CECON a partir de uma serie conversivel em xts
#' @import ggplot2
#' @import xts
#' @import dplyr
#' @export
#' @param dado Serie conversivel em xts
#' @param remover_NA Se TRUE, remove NAs
#' @return Grafico interativo com logo do CECON


grafico_rapido <- function (dado, remover_NA = FALSE)
{
    grafico <- dado %>% ajuste_xts(remover_NA = remover_NA) %>%
        grafico_padrao %>% add_logo
    return(grafico)
}
