funcaototal <- function(){
    loading <- 0
    while (loading <= 100){
        if (loading != 100){
            cat(loading, '% - loading.\n')
            loading <- loading + 25
        } else {
            cat('LOADING CONCLUÍDO!\n')
            break
        }
    }

    nome <- readline(prompt = "\nNome do estudante: ")
    
    lista_notas <- numeric(3)
    for (i in 1:3) {
        lista_notas[i] <- as.numeric(readline(prompt = paste("Insira a nota", i, ": ")))
    }
    
    somatotal <- 0
    for (x in lista_notas){
        somatotal <- somatotal + x
    }
    mediafinal <- somatotal / length(lista_notas)
    
    if (is.numeric(mediafinal) & is.character(nome)) {
        if (mediafinal < 0 | mediafinal > 10) {
            cat('A nota', mediafinal, 'é inválida! Tente novamente.')
        } else {
            if (mediafinal >= 5) {
                resultado <- 'APROVADO'
            } else {
                resultado <- 'REPROVADO'
            }
            
            barplot(lista_notas, 
                    names.arg = c("Avaliação 1", "Avaliação 2", "Avaliação 3"), 
                    col = "lightblue", 
                    border = "black", 
                    main = paste(nome, "-", resultado),
                    ylab = "Notas")
            
            abline(h = mediafinal, col = "blue", lty = 2, lwd = 2)
            text(3, mediafinal, paste("Média:", round(mediafinal, 2)), col = "blue", pos = 3)
        }
    } else {
        cat('O formato do nome ou nota é inválido! Consulte o suporte.')
    }
}

funcaototal()
