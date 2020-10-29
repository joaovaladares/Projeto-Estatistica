#Projeto II
#Grupo:
# Andre Luis Vasconcelos (alpvj)
# Joao Vitor Valadares (jvvm)
# Marcos Vinicius Phryston (mvpn)

# Questao 1:
  got = read.csv("GOTFinal.csv", header = TRUE)#getwd()
  
  print(got)

# Questao 2:
  notas = got[, 3];
  #Pegando a media
    mediaNotas = mean(notas)
  #Pegando o desvio padrao
    desvio_padraoNotas = sd(notas)
  #Pegando a moda
    notas = sort(notas);
    resposta = c(0,0);
    auxNota = 0;
    auxQtd = 0;
    for (x in notas){
      if (auxNota != x){
          if (resposta[2] < auxQtd){
            resposta[1] = auxNota;
            resposta[2] = auxQtd;
          }
          auxNota = x;
          auxQtd = 1;
      }else{
        auxQtd = auxQtd + 1;
      }
    }
    modaNotas = resposta[1]
  #Printando a resposta
    print(sprintf("A media das notas e: %f", mediaNotas))
    print(sprintf("O desvio padrao das notas e: %f", desvio_padraoNotas))
    print(sprintf("A moda das notas e: %.1f", modaNotas))
    
#Questao 3:
  audiencia = got[, 5];
  #Pegando a media
    mediaAud = mean(audiencia)
  #Pegando o desvio padrao
    desvio_padraoAud = sd(audiencia)
  #Pegando a mediana
    medianaAud = median(audiencia)
  #Printando a resposta
    print(sprintf("A media das audiencias e: %f", mediaAud))
    print(sprintf("O desvio padrao das audiencias e: %f", desvio_padraoAud))
    print(sprintf("A mediana das audiencias e: %.2f", medianaAud))

#Questao 4:
      episodios = got[, 2]
      #Criando a funcao que printa os episodios com nota >= 9
      funcaoPrint = function(){
        index_x = 0 #um auxiliar para poder pegar o index de x em notas para poder printar
        for (x in notas){
          index_x = index_x + 1
          if (x >= 9)
            print(as.character(episodios[index_x]))
        }
      }
      funcaoPrint()
      
#Questao 5:
      #Funcao que pega o maior e o menor episodio de uma certa temporada (de acordo com a nota)
      funcaoEpiPorTemp = function(){
        TITULO = c()
        NOTA = c()
        TEMPORADA = c()
        for (temporada_atual in 1:8){
          indiceInit = 0
          indiceFinal = 0
          indexAtual = 0
          jaAchou = FALSE
          for (x in got$Temporada){
            indexAtual = indexAtual + 1
            if (x == temporada_atual)
              if (jaAchou == FALSE){
                indiceInit = indexAtual
                jaAchou = TRUE
              }
              else
                indiceFinal = indexAtual
          }
          arrayEp = episodios[indiceInit:indiceFinal]
          arrayNotas = got[, 3]
          arrayNotas = arrayNotas[indiceInit:indiceFinal]
          #BubbleSort
            indiceX = 0
            indiceJ = 0
            for (i in arrayNotas){
              indiceX = indiceX + 1
              indiceJ = 0
              for (j in arrayNotas){
                indiceJ = indiceJ + 1
                
                if (arrayNotas[indiceX] < arrayNotas[indiceJ]){
                  aux = arrayNotas[indiceX]
                  arrayNotas[indiceX] = arrayNotas[indiceJ]
                  arrayNotas[indiceJ] = aux
                  aux2 = arrayEp[indiceX]
                  arrayEp[indiceX] = arrayEp[indiceJ]
                  arrayEp[indiceJ] = aux2
                }
              }
            }
          #Adicionando nos arrays para criar o dataframe
            for(x in 1:length(arrayNotas)){
              if (arrayNotas[x] == arrayNotas[1]){
                TITULO[length(TITULO)+1] = as.character(arrayEp[x])
                NOTA[length(NOTA)+1] = arrayNotas[1]
                TEMPORADA[length(TEMPORADA)+1] = temporada_atual
              }
            }
            
            for(x in 1:length(arrayNotas)){
              if (arrayNotas[x] == arrayNotas[length(arrayNotas)]){
                TITULO[length(TITULO)+1] = as.character(arrayEp[x])
                NOTA[length(NOTA)+1] = arrayNotas[length(arrayNotas)]
                TEMPORADA[length(TEMPORADA)+1] = temporada_atual
              }
            }
        
        }
        
        df = data.frame(TITULO, NOTA, TEMPORADA)
        print(df)
      }
      
      funcaoEpiPorTemp()
      
#Questao 6:
      menorDP = function(){
        arrayDosDP = c()
        for (temporada_atual in 1:8){
          indiceInit = 0
          indiceFinal = 0
          indexAtual = 0
          jaAchou = FALSE
          for (x in got$Temporada){
            indexAtual = indexAtual + 1
            if (x == temporada_atual)
              if (jaAchou == FALSE){
                indiceInit = indexAtual
                jaAchou = TRUE
              }
            else
              indiceFinal = indexAtual
          }
          arrayAudiencia = got[, 5]
          arrayAudiencia = arrayAudiencia[indiceInit:indiceFinal]
          desvPad = sd(arrayAudiencia)
          arrayDosDP[temporada_atual] = desvPad
        }
        melhorTemp = 0
        indexAtual = 0
        menorDP = Inf
        for(x in arrayDosDP){
          indexAtual = indexAtual + 1
          if (x < menorDP){
            menorDP = x
            melhorTemp = indexAtual
          }
        }
        return (melhorTemp)
      }
      print(sprintf("A temporada com menor desvio padrao e: %d", menorDP()))
      
#Questao 7:
      arrayPers = got[, 4]
      arrayNotas = got[, 3]
      contador = 0
      somaNotas = 0
      indexAtual = 0
      for (x in arrayPers){
        indexAtual = indexAtual + 1
        if (grepl("Brienne of Tarth", as.character(x))){
          contador = contador + 1 
          somaNotas = somaNotas + arrayNotas[indexAtual]
        }
      }
      mediaBrienne = somaNotas/contador
      print(sprintf("A media das notas dos episodios em que Brienne of Tarth aparece e: %.1f",mediaBrienne))

#Questao 8:
      funcaoDeRepetidos4Temp  = function(){
        stringComTodoMundo = ""
        inicio4 = 0
        fim4 = 0
        for (x in got$Temporada){
          if (x == 4)
            (inicio4 == 0) ? inicio4 = x : fim4 = x
        }
        for (x in inicio4:fim4){
          stringComTodoMundo = paste0(stringComTodoMundo, as.character(arrayPers[x]))
          stringComTodoMundo = paste0(stringComTodoMundo, ",")
        }
        arrayTodosOsPersonagens = unlist(strsplit(stringComTodoMundo, ","))
        arrayPersonagens4 = arrayTodosOsPersonagens
        arrayTodosOsPersonagens = unique(arrayTodosOsPersonagens)
  
        arrayRepetido = c()
        for (x in 1:41){
          arrayRepetido[x] = 0
        }
        indexAtual = 0
        for (x in arrayTodosOsPersonagens){
          indexAtual = indexAtual + 1
          for (y in arrayPersonagens4){
            if (x == y){
              arrayRepetido[indexAtual] = arrayRepetido[indexAtual] + 1
            }
          }
        }
        arrayDoRetorno = c()
        contador = 0
        indexAtual = 0
        for (x in arrayRepetido){
          indexAtual = indexAtual + 1
          if (x == 1){
            contador = contador + 1
            arrayDoRetorno[contador] = arrayTodosOsPersonagens[indexAtual]
          }
        }
        return (arrayDoRetorno)
      }
      print(funcaoDeRepetidos4Temp())

#Questao 9:
      histogramaPersonagem = function(personagem){
        aparicoesPorTemp = c()
        indexAtual = 0
        for (x in got$Temporada){
          indexAtual = indexAtual +1
          if (grepl(personagem, as.character(got$Personagens[indexAtual]))){
            aparicoesPorTemp[length(aparicoesPorTemp) + 1] = x
          }
        }
        return (aparicoesPorTemp)
      }
      #nome = "Bran Stark(Isaac Hempstead)"
      #nome = "Tyrion Lannister(Peter Dinklage)"
      nome <- readline(prompt = "Digite o nome do personagem: ")
      localizador = nchar(nome)
      for (x in 1:nchar(nome)){
        if (substr(nome, x, x) == '('){
          localizador = x-1
        }
      }
      novoNome = substr(nome, 1, localizador)
      arrayParaHisto = histogramaPersonagem(novoNome)
      
      hist(arrayParaHisto,
           main = nome,
           ylab = "Ocorrencia",
           xlab = "Temporada",
           border = "black",
           col = "blue",
           breaks = c(0,1,2,3,4,5,6,7,8),
           xlim = c(0, 8),
           ylim = c(0,8)
      )