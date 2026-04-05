# Como usar:
# Dentro da pasta abrir o terminal e abrir o R
# Digitar "source("analise_prova.R")"
# Digitar "analisar_prova("EstatisticaCAMB_Conjuntos.csv")"
# Na hora que for rodar o pdflatex, tem que rodar com "pdflatex --shell-escape arquivo.tex"

# Instalar install.packages("modeest")

analisar_prova <- function(arquivo_csv) {
    library(gridExtra)  # Para grid.table
    library(grid)       # Para manipular gráficos de tabela
    library(modeest) # Para calcular moda

    # Ler o CSV
    dados <- read.csv(arquivo_csv, stringsAsFactors = FALSE)
  
    # Separar colunas
    notas <- dados[1]
  
    # Criar pasta
    dir.create("graficos_prova", showWarnings = FALSE)
  
    colunas <- colnames(notas)
    resumo_geral <- data.frame()
    
    for (i in seq_along(colunas)) {
        titulo <- colunas[i]
        titulo_legivel <- gsub("\\.", " ", titulo)
        titulo_arquivo <- gsub("\\.", "", titulo)
        coluna <- notas[[i]]
    
        # --- HISTOGRAMA ---
        svg(filename = paste0("graficos_prova/histograma_", titulo_arquivo, ".svg"), width = 8, height = 6, bg = "transparent")
    
        # Quebras automáticas (ou use `breaks = seq(0, 10, by = 1)` por ex.)
        h <- hist(coluna,
                breaks = seq(0, 4, by = 0.5),  # classes de 0.5
                probability = TRUE,
                right = FALSE,
                col = "lightblue",
                border = "white",
                #main = paste("Histograma -", titulo_legivel),
                main = "",
                xlab = "Nota",
                ylab = "Densidade",
                ylim = c(0, max(density(coluna)$y) * 1.4),
                xlim = c(0, 4),
                xaxt = "n")
        
        axis(1, at = seq(0, 4, by = 0.5))
    
        # Frequência relativa em %
        rel_freq <- h$counts / sum(h$counts) * 100
        labels <- paste0(round(rel_freq, 1), "%")
    
        # Adiciona as porcentagens
        text(x = h$mids,
            y = h$density,
            labels = labels,
            pos = 3,
            cex = 0.9,
            col = "black")
    
        # Curva normal com média e desvio padrão dos dados
        media <- mean(coluna)
        desvio <- sd(coluna)
        x <- seq(min(coluna), max(coluna), length.out = 200)
        lines(x, dnorm(x, mean = media, sd = desvio), col = "red", lwd = 2)
        legend("topleft", legend = "Distribuição Normal", col = "red", lwd = 2, bty = "n")
    
        # Linha tracejada e valor da média
        abline(v = media, col = "darkred", lty = 2, lwd = 2)
        text(media, max(density(coluna)$y) * 1.4, 
            labels = paste0("Média = ", round(media, 2)), 
            pos = 2,  # à esquerda da linha
            col = "darkred", cex = 1.1)
       
        # Fecha PNG
        dev.off()
    
        # --- BOXPLOT ---
        svg(filename = paste0("graficos_prova/boxplot_", titulo_arquivo, ".svg"), width = 6, height = 8, bg = "transparent")
    
        quartis <- quantile(coluna, type = 1)
    
        boxplot(coluna,
                #main = paste("Boxplot -", titulo_legivel),
                ylab = "Nota",
                col = "salmon",
                border = "black",
                cex.axis = 1.2,
                cex.lab = 1.2,
                cex.main = 1.4)
    
        # Adicionando os valores dos quartis no gráfico
        # Min e Max à direita
        text(x = 1.2, y = quartis[1], labels = paste0("Mínimo = ", quartis[1]), pos = 4, col = "blue")
        text(x = 1.2, y = quartis[5], labels = paste0("Máximo = ", quartis[5]), pos = 4, col = "blue")

        # Q1, Mediana e Q3 à esquerda
        text(x = 0.8, y = quartis[2], labels = paste0("Q1 = ", quartis[2]), pos = 2, col = "blue")
        text(x = 0.8, y = quartis[3], labels = paste0("Mediana = ", quartis[3]), pos = 2, col = "blue")
        text(x = 0.8, y = quartis[4], labels = paste0("Q3 = ", quartis[4]), pos = 2, col = "blue")


        dev.off()
    
        # --- TABELA DESCRITIVA (summary) EM SVG ---
        resumo <- summary(coluna)
        tabela_resumo <- data.frame(
            Estatística = names(resumo),
            Valor = as.numeric(resumo),
            row.names = NULL
        )

        svg(filename = paste0("graficos_prova/tabela_summary_", titulo_arquivo, ".svg"), width = 6, height = 4, bg = "transparent")

        grid.table(tabela_resumo)

        dev.off()
        
        # --- ACUMULAR RESUMO GERAL ---
        moda <- mfv(coluna)
        cv <- desvio / media
        desvio_pop <- desvio * sqrt((length(coluna) - 1) / length(coluna))
        resumo_geral <- rbind(resumo_geral, data.frame(
            Questao = titulo_legivel,
            Media = round(media, 2),
            Mediana = round(median(coluna), 2),
            Moda = paste(moda, collapse = "/"),
            DesvioPadrao = round(desvio_pop, 2),
            Minimo = round(min(coluna), 2),
            Q1 = round(quartis[2], 2),
            Q3 = round(quartis[4], 2),
            Maximo = round(max(coluna), 2)
        ))
    }
    
    # --- SALVAR CSV COM DADOS RESUMIDOS ---
    write.csv(resumo_geral, "graficos_prova/resumo_estatisticas.csv", row.names = FALSE)

    # --- GERAR TABELA TEX ---
    arquivo_tex <- "graficos_prova/tabela_estatisticas.tex"
    con <- file(arquivo_tex, "w", encoding = "UTF-8")
    
    writeLines("\\begin{table}[H]", con)
    writeLines("\\centering", con)
    writeLines("\\renewcommand{\\arraystretch}{1.1}", con)
    writeLines("\\caption{Estatística descritiva da prova.}", con)
    writeLines("\\begin{tabularx}{\\textwidth}{Ycccccccc}", con)
    writeLines("\\toprule", con)
    writeLines("Questão & Média & Mediana & Moda & Desvio & Mínimo & Q1 & Q3 & Máximo \\\\", con)
    writeLines("\\midrule", con)

    for (i in 1:nrow(resumo_geral)) {
        linha <- resumo_geral[i, ]
        linha_tex <- paste(
            linha$Questao,
            linha$Media,
            linha$Mediana,
            linha$Moda,
            linha$DesvioPadrao,
            linha$Minimo,
            linha$Q1,
            linha$Q3,
            linha$Maximo,
            sep = " & "
        )
        writeLines(paste0(linha_tex, " \\\\"), con)
    }

    writeLines("\\bottomrule", con)
    writeLines("\\end{tabularx}", con)
    writeLines("\\end{table}", con)

    close(con)
}

