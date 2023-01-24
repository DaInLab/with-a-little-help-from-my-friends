# - ETAPA INICIAL
#--- Importação e preparação dos dados
# versão inicial em 05/01/2023.
# versão do projeto "Impacto da Covid 19 nos Estudantes de Ensino Superior" no GitHub

# Lendo o arquivo em .xlsx
if (!("readxl") %in% installed.packages()) install.packages("readxl")
library(readxl)
dbf.xlsx <- read_excel("./dados/COVID19IES.xlsx")

# Lendo o arquivo em .csv
dbf.csv <-read.csv("./dados/COVID19IES.csv", header = TRUE, sep = ";", quote = "\"", dec = ".")

# Lendo o arquivo em .ods
if (!("readODS") %in% installed.packages()) install.packages("readODS")
library(readODS)
dbf.ods <- read_ods("./dados/COVID19IES.ods")

# Utilizando o pacote smartEDA no dataframe
if(!("SmartEDA") %in% installed.packages()) install.packages("SmartEDA")
library(SmartEDA)
if(!("ISLR") %in% installed.packages()) install.packages("ISLR")
library("ISLR")

# Sumário do dataset
summary(dbf.xlsx)

# Função para gerar o dicionário de dados de um dataframe
ExpData(data=dbf.xlsx,type=1)

ExpData(data=dbf.xlsx,type=2)

# número total de observações no dataframe
total_casos <- nrow(dbf.xlsx)
total_casos  
#[1] 52

## Gráfico 1 Faixa Etária dos respondentes
casos_idade <- table(dbf.xlsx$idade)
casos_idade
#17 18 19 20 21 22 23 25 26 28 29 30 34 35 37 40 41 43 46 48 51 54 55 60 
# 1  1  3  4  2  3  4  4  3  1  1  1  1  2  1  3  3  4  1  2  2  2  2  1  

#Cálculo da porcentagem das faixas etárias
pct_idade <- paste0(round(unname(casos_idade) / sum(unname(casos_idade)) * 100,0), "%")
pct_idade  
#[1] "2%" "2%" "6%" "8%" "4%" "6%" "8%" "8%" "6%" "2%" "2%" "2%" "2%" "4%" "2%" "6%" "6%" "8%" "2%" "4%" "4%" "4%" "4%" "2%"

# Gráfico do tipo barra das faixas etárias
graph.idade <- barplot(casos_idade, 
                       main = "Gráfico 1: Faixa etária dos respondentes",
                       xlab = "Faixa Etária", 
                       ylab = "Respondentes",
                       col = "orange",
                       ylim = c(0,max(casos_idade) + 30))
text(x = graph.idade, y = casos_idade, label = unname(casos_idade), cex=1, pos=3)
axis(1, at=graph.idade, labels=paste("(", pct_idade, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)  
 
# Versão 2, utilizando a definição divisão das faixas etárias:
# 17 - 21
# 22 - 26
# 27 - 31
# 32 - 36
# 37 - 41
# 42 - 46 
# 47 - 51 
# 52 - 56
# 57 - 61

idade_concat <- data.frame(idade=dbf.xlsx$idade, faixa_etaria="")

for (k in 1:nrow(idade_concat)) {
  if(idade_concat$idade[k] <= 21) idade_concat$faixa_etaria[k] <- "de 17 a 21 anos"
  if(idade_concat$idade[k] >= 22 & idade_concat$idade[k] <= 26) idade_concat$faixa_etaria[k] <- "de 22 a 26 anos"
  if(idade_concat$idade[k] >= 27 & idade_concat$idade[k] <= 31) idade_concat$faixa_etaria[k] <- "de 27 a 31 anos"
  if(idade_concat$idade[k] >= 32 & idade_concat$idade[k] <= 36) idade_concat$faixa_etaria[k] <- "de 32 a 36 anos"
  if(idade_concat$idade[k] >= 37 & idade_concat$idade[k] <= 41) idade_concat$faixa_etaria[k] <- "de 37 a 41 anos"
  if(idade_concat$idade[k] >= 42 & idade_concat$idade[k] <= 46) idade_concat$faixa_etaria[k] <- "de 42 a 46 anos" 
  if(idade_concat$idade[k] >= 47 & idade_concat$idade[k] <= 51) idade_concat$faixa_etaria[k] <- "de 47 a 51 anos" 
  if(idade_concat$idade[k] >= 52 & idade_concat$idade[k] <= 56) idade_concat$faixa_etaria[k] <- "de 52 a 56 anos"
  if(idade_concat$idade[k] >= 57 & idade_concat$idade[k] <= 61) idade_concat$faixa_etaria[k] <- "de 57 a 61 anos"
  if(idade_concat$idade[k] > 61) idade_concat$faixa_etaria[k] <- "acima de 61 anos"
}

casos_idade_concat <- table(idade_concat$faixa_etaria)
casos_idade_concat
#de 17 a 21 anos de 22 a 26 anos de 27 a 31 anos de 32 a 36 anos de 37 a 41 anos de 42 a 46 anos de 47 a 51 anos 
#             11              14               3               3               7               5               4 
#de 52 a 56 anos de 57 a 61 anos 
#              4               1

#Cálculo da porcentagem das faixas etárias
pct_idade3 <- paste0(round(unname(casos_idade_concat) / sum(unname(casos_idade_concat)) * 100,0), "%")
pct_idade3
#[1] "21%" "27%" "6%"  "6%"  "13%" "10%" "8%"  "8%"  "2%"

# Gráfico do tipo barra das faixas etárias
graph.idade_concat <- barplot(casos_idade_concat, 
                               main = "Gráfico 1: Faixa etária dos respondentes redefinida",
                               xlab = "Faixa Etária", 
                               ylab = "Respondentes",
                               col = "orange",
                               horiz = F,
                               ylim = c(0,max(casos_idade_concat) + 5))
text(x = graph.idade_concat, y = casos_idade_concat, label = unname(casos_idade_concat), cex=1, pos=3)
axis(1, at=graph.idade_concat, labels=paste("(", pct_idade3, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 2
casos_genero <- table(dbf.xlsx$genero)
casos_genero
# Feminino              Homem gay              Masculino Transgênero/Transexual 
#       24                      1                     25                      2 
pct_genero <- paste(round(unname(casos_genero) / sum(unname(casos_genero)) * 100), "%")
pct_genero  
#[1] "46 %" "2 %"  "48 %" "4 %" 

#Gráfico 2: Quantidade de respondentes por sexo
# Gráfico tipo "pizza"
pie(casos_genero,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green"),
    labels = paste(names(casos_genero), "-", pct_genero),
    main = "Gráfico 2: Quantidade de respondentes por gênero")

# Dados do Gráfico 3
casos_conjugal <- table(dbf.xlsx$situacao_conjugal)
casos_conjugal
#  Casado(a)   Divorciado(a)/Separado(a)                 Solteiro(a) União Estável/Vivendo junto   Viúvo(a)
#         16                           2                          28                           5          1 

pct_conjugal <- paste(round(unname(casos_conjugal) / sum(unname(casos_conjugal)) * 100), "%")
pct_conjugal  
#[1] "46 %" "2 %"  "48 %" "4 %" 

#Gráfico 3: Quantidade de respondentes por sutuação conjugal
# Gráfico tipo "pizza"
pie(casos_conjugal,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(casos_conjugal), "-", pct_conjugal),
    main = "Gráfico 3: Quantidade de respondentes por sitação conjugal")

# Dados do Gráfico 4
casos_emprego <- table(dbf.xlsx$situacao_empregaticia)
casos_emprego
#                  Aposentada                                        Bolsista 
#                           1                                              10 
#         Corretor de Imóveis  Dependente (dos pais) com vínculo empregatício 
#                           1                                               1 
#Dependente (dos pais, etc.)                                  Desempregado(a) 
#                           7                                               2 
#                Empregado(a)                                      Empresária 
#                          24                                               1 
#               Estagiário(a)              Micro-empresário instituído por ME 
#                           2                                               1 
#           Servidora pública tatuador autônomo, mas conto com ajuda dos pais 
#                           1                                               1

# Rótulos muito grandes  em grande quantidade. Dificuldade para exibir nos gráficos
# Redução proposital dos textos
names(casos_emprego) <- c("Aposentada", "Bolsista","Corretor", "Dep./Empr.", "Dependente",
                          "Desemp.","Empregado","Empresária","Estagiário","Micro-emp.",
                          "Serv. púb.", "Autôn/Dep.")
casos_emprego
#Aposentada   Bolsista   Corretor Dep./Empr. Dependente    Desemp.  Empregado Empresária Estagiário Micro-emp. Serv. púb. Autôn/Dep. 
#         1         10          1          1          7          2         24          1          2          1          1          1 

pct_emprego <- paste(round(unname(casos_emprego) / sum(unname(casos_emprego)) * 100), "%")
pct_emprego
#[1] "2 %"  "19 %" "2 %"  "2 %"  "13 %" "4 %"  "46 %" "2 %"  "4 %"  "2 %"  "2 %"  "2 %" 

#Gráfico 4: Quantidade de respondentes por sutuação empregatícia
# Gráfico tipo "pizza"
pie(casos_emprego,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(casos_emprego), "-", pct_emprego),
    main = "Gráfico 4: Quantidade de respondentes por sitação empregatícia")

# Gráfico 4 do tipo barra sitação empregatícia
graph.casos_emprego <- barplot(casos_emprego, 
                              main = "Gráfico 4: Quantidade de respondentes por sitação empregatícia",
                              xlab = "Sitação Empregatícia", 
                              ylab = "Respondentes",
                              col = "orange",
                              ylim = c(0,max(casos_emprego) + 5))
text(x = graph.casos_emprego, y = casos_emprego, label = unname(casos_emprego), cex=1, pos=3)
axis(1, at=graph.casos_emprego, labels=paste("(", pct_emprego, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 5
casos_estado <- table(dbf.xlsx$estado_reside, exclude = NULL)
casos_estado
#  AM   SP <NA> 
#   1   47    4

pct_estado <- paste(round(unname(casos_estado) / sum(unname(casos_estado)) * 100), "%")
pct_estado  
#[1] "2 %"  "90 %" "8 %"
names(pct_estado) <-c("Amazonas", "São Paulo", "Não Respondeu")

#Gráfico 5: Quantidade de respondentes por estado
# Gráfico tipo "pizza"
pie(casos_estado,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black"),
    labels = paste(names(pct_estado), "-", pct_estado),
    main = "Gráfico 5: Quantidade de respondentes por estado")

# Dados do Gráfico 6
casos_ies <- table(dbf.xlsx$ies, exclude = NULL)
casos_ies

pct_ies <- paste(round(unname(casos_ies) / sum(unname(casos_ies)) * 100), "%")
pct_ies  
# [1] "2 %"  "4 %"  "2 %"  "2 %"  "2 %"  "2 %"  "2 %"  "4 %"  "2 %"  "75 %" "2 %"  "2 %" 

# Rótulos muito grandes e dispersos. Dificulta a exibição nos gráficos!
# Redução proposital no tamanho dos textos
names(casos_ies) <- c("ETEC", "FMJundiaí", "FJaú", "Fatec Catanduva", "FGV", "IMESB",
                          "SEBRAE", "UFSCar", "UNESP", "UNIARA", "UNOPAR")
                          
# Gráfico 6 do tipo barra por nome da Instituição de Ensino Superior
graph.casos_ies <- barplot(casos_ies,
                               horiz = F,
                               main = "Gráfico 6: Quantidade de respondentes por Instituição de Ensino Superior",
                               xlab = "IES", 
                               ylab = "Respondentes",
                               col = "orange",
                               ylim = c(0,max(casos_ies) + 10)
)
text(x = graph.casos_ies, y = casos_ies, label = casos_ies, cex=1, pos=3)
axis(1, at=graph.casos_ies, labels=paste("(", pct_ies , ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Rótulos muito grandes e dispersos. Dificulta a exibição nos gráficos!
# Redução proposital no tamanho dos textos
names(casos_ies) <- c("ETEC", "FMJundiaí", "FJaú", "Fatec Catanduva", "FGV", "IMESB",
                      "SEBRAE", "UFSCar", "UNESP", "UNIARA", "UNOPAR")

# Gráfico 6 do tipo barra respondentes por nome de IES
graph.casos_ies <- barplot(casos_ies,
                           horiz = F,
                           main = "Gráfico 6: Quantidade de respondentes por IES",
                           xlab = "IES", 
                           ylab = "Respondentes",
                           col = "orange",
                           ylim = c(0,max(casos_ies) + 5)
)
text(x = graph.casos_ies, y = casos_ies, label = unname(casos_ies), cex=1, pos=3)
axis(1, at=graph.casos_ies, labels=paste("(", pct_ies , ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)

# Dados do Gráfico 7
casos_ensino<- table(dbf.xlsx$nivel_ensino, exclude = NULL)
casos_ensino
# Doutorado     Ensino Técnico Especialização/MBA          Graduação           Mestrado      Pós-doutorado 
#        15                  1                  1                 21                 13                  1 

pct_ensino <- paste(round(unname(casos_ensino) / sum(unname(casos_ensino)) * 100), "%")
pct_ensino  
#[1] "29 %" "2 %"  "2 %"  "40 %" "25 %" "2 %" 

#Gráfico 7: Quantidade de respondentes por nível de ensino
# Gráfico tipo "pizza"
pie(casos_ensino,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("red", "orange", "yellow", "green", "black", "white"),
    labels = paste(names(casos_ensino), "-", pct_ensino),
    main = "Gráfico 7: Quantidade de respondentes por nível de ensino")

# Dados do Gráfico 8
casos_tipo_ies<- table(dbf.xlsx$tipo_ies, exclude = NULL)
casos_tipo_ies
# Autarquia municipal             Privada             Pública 
#                   1                   6                  45 
names(casos_tipo_ies) = c("Municipal", "Privada", "Pública")
pct_tipo_ies <- paste(round(unname(casos_tipo_ies) / sum(unname(casos_tipo_ies)) * 100), "%")
pct_tipo_ies  
#[1] "2 %"  "12 %" "87 %" 

#Gráfico 8: Quantidade de respondentes por tipo de mantenedora de instituição de ensino
# Gráfico tipo "pizza"
pie(casos_tipo_ies,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("red", "orange", "blue"),
    labels = paste(names(casos_tipo_ies), "-", pct_tipo_ies),
    main = "Gráfico 8: Respondentes por nível de ensino")

# Dados do Gráfico 9
casos_local_estudante <- table(dbf.xlsx$local_estudante, exclude = NULL)
casos_local_estudante
# Local Outra cidade 
#    18           34

pct_local_estudante <- paste(round(unname(casos_local_estudante) / 
                                     sum(unname(casos_local_estudante)) * 100), "%")
pct_local_estudante 
#[1] "35 %" "65 %" 

#Gráfico 9: Quantidade de respondentes por local de moradia
# Gráfico tipo "pizza"
pie(casos_local_estudante,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("red", "orange", "blue"),
    labels = paste(names(casos_local_estudante), "-", pct_local_estudante),
    main = "Gráfico 9: Respondentes por local de moradia")

# Dados do Gráfico 10
casos_migrou_virtual <- table(dbf.xlsx$migrou_virtual, exclude = NULL)
casos_migrou_virtual
# Não Sim 
#   1  51

pct_migrou_virtual <- paste(round(unname(casos_migrou_virtual) / 
                                     sum(unname(casos_migrou_virtual)) * 100), "%")
pct_migrou_virtual 
#[1] "2 %"  "98 %"" 

#Gráfico 10: IES migrou virtual
# Gráfico tipo "pizza"
pie(casos_migrou_virtual,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("black", "orange", "blue"),
    labels = paste(names(casos_migrou_virtual), "-", pct_migrou_virtual),
    main = "Gráfico 10: IES migrou para virual")

# Dados do Gráfico 11
casos_ies_fechou_dorm <- table(dbf.xlsx$ies_fechou_dorm, exclude = NULL)
casos_ies_fechou_dorm
#          Não Não se aplica       Não sei           Sim 
#            4             9            32             7

pct_fechou_dorm <- paste(round(unname(casos_ies_fechou_dorm) / 
                                    sum(unname(casos_ies_fechou_dorm)) * 100), "%")
pct_fechou_dorm 
#[1] "8 %"  "17 %" "62 %" "13 %" 

#Gráfico 11: IES fechou dorms
# Gráfico tipo "pizza"
pie(casos_ies_fechou_dorm,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("black", "orange", "blue", "red"),
    labels = paste(names(casos_ies_fechou_dorm), "-", pct_fechou_dorm),
    main = "Gráfico 11: IES fechou dormitórios")

# Gráfico 12: situacao_durante-pandemia
# Núvem de palavras

#Load the packages
if(!"wordcloud" %in% installed.packages()) install.packages("wordcloud")
library(wordcloud)
if(!"wordcloud2" %in% installed.packages()) install.packages("wordcloud2")
library(wordcloud2)
if(!"RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer")
library(RColorBrewer)
if(!"tm" %in% installed.packages()) install.packages("tm")
library(tm)

#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$situação_durante_pandemia)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
# Para utilizar o comando "pipe" (%>%) ou operador "tee pipe" (%T>%) , pode-se "carregar" o pacote magrittr
if(!"magrittr" %in% installed.packages()) install.packages("magrittr")
library(magrittr)
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
# Uma matriz de termos de documento é uma matriz matemática que descreve a frequência 
# dos termos que ocorrem em uma coleção de documentos.
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 3, # menor "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Dados do Gráfico 13
casos_aluno_reside <- table(dbf.xlsx$residencia_atual, exclude = NULL)
casos_aluno_reside
#Em outra cidade da instituição de ensino mas dentro do mesmo estado 
#                                                                 18 
#       Fora do campus, mas na mesma cidade da instituição de ensino 
#                                                                 33 
#                                                               <NA> 
#                                                                  1 
names(casos_aluno_reside) <- c("Outra cidade/mesmo Estado", "Fora do campus/mesma cidade", "NA")
pct_aluno_reside <- paste(round(unname(casos_aluno_reside) / 
                                 sum(unname(casos_aluno_reside)) * 100), "%")
pct_aluno_reside 
#[1] "35 %" "63 %" "2 %" 

#Gráfico 13: Residência atual do aluno
# Gráfico tipo "pizza"
pie(casos_aluno_reside,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("orange", "blue", "red","black"),
    labels = paste(names(casos_aluno_reside), "-", pct_aluno_reside),
    main = "Gráfico 13: Local de residência do aluno")

# Dados do Gráfico 14
casos_aluno_moradia <- table(dbf.xlsx$moradia_atual_permanente, exclude = NULL)
casos_aluno_moradia
#Não Sim 
#  5  47

pct_aluno_moradia <- paste(round(unname(casos_aluno_moradia) / 
                                   sum(unname(casos_aluno_moradia)) * 100), "%")
pct_aluno_moradia 
#[1] "10 %" "90 %" 

#Gráfico 14: Moradia permanente do aluno
# Gráfico tipo "pizza"
pie(casos_aluno_moradia,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("orange", "blue", "red","black"),
    labels = paste(names(casos_aluno_moradia), "-", pct_aluno_moradia),
    main = "Gráfico 14: Moradia atual é permanente do aluno")

# Dados do Gráfico 15
casos_aluno_mora_com <- table(dbf.xlsx$morando_com, exclude = NULL)
casos_aluno_mora_com
#Colega de quarto/República                    Família                 Sozinho(a) 
#                        3                         36                         13

pct_aluno_mora_com <- paste(round(unname(casos_aluno_mora_com) / 
                                   sum(unname(casos_aluno_mora_com)) * 100), "%")
pct_aluno_mora_com 
#[1] "6 %"  "69 %" "25 %" 

#Gráfico 15: Aluno mora com quem?
# Gráfico tipo "pizza"
pie(casos_aluno_mora_com,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("orange", "blue", "red","black"),
    labels = paste(names(casos_aluno_mora_com), "-", pct_aluno_mora_com),
    main = "Gráfico 15: Aluno mora com")

# Dados do Gráfico 16
casos_aluno_mora_risco <- table(dbf.xlsx$convive_risco_relevante, exclude = NULL)
casos_aluno_mora_risco
#Não Sim 
# 39  13      
pct_aluno_mora_risco <- paste(round(unname(casos_aluno_mora_risco) / 
                                    sum(unname(casos_aluno_mora_risco)) * 100), "%")
pct_aluno_mora_risco 
#[1] "75 %" "25 %" 

#Gráfico 16: Aluno mora com pessoas de risco
# Gráfico tipo "pizza"
pie(casos_aluno_mora_risco,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("orange", "blue", "red","black"),
    labels = paste(names(casos_aluno_mora_risco), "-", pct_aluno_mora_risco),
    main = "Gráfico 16: Aluno mora com pessoas de risco")

# Dados do Gráfico 17
casos_aluno_mora_quarentena <- table(dbf.xlsx$quarentena_imposta, exclude = NULL)
casos_aluno_mora_quarentena
#Não Sim 
# 19  33      
pct_aluno_mora_quarentena <- paste(round(unname(casos_aluno_mora_quarentena) / 
                                      sum(unname(casos_aluno_mora_quarentena)) * 100), "%")
pct_aluno_mora_quarentena
#[1] "75 %" "25 %" 

#Gráfico 17: Aluno reside pessoas em quarentena
# Gráfico tipo "pizza"
pie(casos_aluno_mora_risco,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("orange", "blue", "red","black"),
    labels = paste(names(casos_aluno_mora_quarentena), "-", pct_aluno_mora_quarentena),
    main = "Gráfico 17: Aluno reside com pessoas em quarentena")

# Dados do Gráfico 18
casos_aluno_vivenciou <- table(dbf.xlsx$vivenciou, exclude = NULL)
casos_aluno_vivenciou
#                                                                            Ajuda ou assistência de pessoas desconhecidas 
#                                                                                                                        4 
#Dificuldades devido a alterações em suas condições de vida, incluindo o fechamento de alojamentos, perda de emprego, etc. 
#                                                                                                                        9
#                                                                                     Dificuldades para viajar/se deslocar 
#                                                                                                                       19 
#                                                                                  Discriminação por pessoas desconhecidas 
#                                                                                                                        1 
#                                                                                 Não se aplica / Não sabe / Não se lembra 
#                                                                                                                       19 
names(casos_aluno_vivenciou) <- c("Ajuda/assistência pessoas desconhecidas", "Alterações nas condições de vida", 
                                  "Dificuldade viajar/se deslocar", "Discriminação por desconhecidos", "Não se aplica")
pct_aluno_vivenciou <- paste(round(unname(casos_aluno_vivenciou) / 
                                           sum(unname(casos_aluno_vivenciou)) * 100), "%")
pct_aluno_vivenciou
#[1] "8 %"  "17 %" "37 %" "2 %"  "37 %" 

#Gráfico 18: Dificuldades vividas pelo aluno
# Gráfico tipo "pizza"
pie(casos_aluno_vivenciou,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black"),
    labels = paste(names(casos_aluno_vivenciou), "-", pct_aluno_vivenciou),
    main = "Gráfico 18: Dificuldades vividas pelo aluno")

# Dados do Gráfico 19
casos_acesso_saude <- table(dbf.xlsx$acesso_servicos_saude, exclude = NULL)
casos_acesso_saude
# Melhor do que antes Muito pior do que antes         N/A ou Não sabe        O mesmo de antes       Pior do que antes 
#                   5                       5                       1                      28                      13

pct_acesso_saude <- paste(round(unname(casos_acesso_saude) / 
                                     sum(unname(casos_acesso_saude)) * 100), "%")
pct_acesso_saude
#[[1] "10 %" "10 %" "2 %"  "54 %" "25 %" 

#Gráfico 19: Acesso aos serviços de saude
# Gráfico tipo "pizza"
pie(casos_acesso_saude,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black"),
    labels = paste(names(casos_acesso_saude), "-", pct_acesso_saude),
    main = "Gráfico 19: Acesso aos serviços de saude")

# Dados do Gráfico 20
casos_acesso_internet <- table(dbf.xlsx$acesso_internet, exclude = NULL)
casos_acesso_internet
#Melhor do que antes Muito melhor do que antes      Muito pior que antes           N/A ou Não sabe         O mesmo que antes 
#                  9                         1                         2                         1                        34 
#Pior que antes 
#             5

pct_acesso_internet <- paste(round(unname(casos_acesso_internet) / 
                                  sum(unname(casos_acesso_internet)) * 100), "%")
pct_acesso_internet
#[1] "17 %" "2 %"  "4 %"  "2 %"  "65 %" "10 %" 

#Gráfico 20: Acesso aos serviços de internet
# Gráfico tipo "pizza"
pie(casos_acesso_internet,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black", "white"),
    labels = paste(names(casos_acesso_internet), "-", pct_acesso_internet),
    main = "Gráfico 20: Acesso aos serviços de internet")

# Dados do Gráfico 21
casos_prosseguir_estudos <- table(dbf.xlsx$capacidade_prosseguir_estudos, exclude = NULL)
casos_prosseguir_estudos
#Melhor do que antes Muito melhor do que antes      Muito pior que antes           N/A ou Não sabe         O mesmo que antes 
#                  5                         1                         4                         2                        22 
#Pior do que antes                      <NA>  
#               17                         1 
 
pct_prosseguir_estudos <- paste(round(unname(casos_prosseguir_estudos) / 
                                     sum(unname(casos_prosseguir_estudos)) * 100), "%")
pct_prosseguir_estudos
#[1] "10 %" "2 %"  "8 %"  "4 %"  "42 %" "33 %" "2 %" 

#Gráfico 21: Capacidade prosseguir estudos
# Gráfico tipo "pizza"
pie(casos_prosseguir_estudos,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black", "white", "green"),
    labels = paste(names(casos_prosseguir_estudos), "-", pct_prosseguir_estudos),
    main = "Gráfico 21: Capacidade para prosseguir ou concluir estudos")

# Dados do Gráfico 22
casos_socializacao <- table(dbf.xlsx$capacidade_socializacao, exclude = NULL)
casos_socializacao
#    Melhor do que antes Muito pior do que antes        O mesmo de antes       Pior do que antes 
#                      3                      10                      19                      20

pct_socializacao <- paste(round(unname(casos_socializacao) / 
                                        sum(unname(casos_socializacao)) * 100), "%")
pct_socializacao
#[1] "6 %"  "19 %" "37 %" "38 %" 

#Gráfico 22: Capacidade de socialização
# Gráfico tipo "pizza"
pie(casos_socializacao,
    edges = 200, radius = 0.8,
    clockwise = T,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red","black", "white", "green"),
    labels = paste(names(casos_socializacao), "-", pct_socializacao),
    main = "Gráfico 22: Capacidade de socialização")

# Dados do Gráfico 23
casos_bem_estar <- table(dbf.xlsx$bem_estar_psicologico, exclude = NULL)
casos_bem_estar
#      Melhor do que antes Muito melhor do que antes   Muito pior do que antes           N/A ou Não Sabe          O mesmo de antes 
#                        2                         2                         9                         1                        12 
#Pior do que antes 
#               26

pct_bem_estar <- paste(round(unname(casos_bem_estar) / 
                                  sum(unname(casos_bem_estar)) * 100), "%")
pct_bem_estar
#[1] "4 %"  "4 %"  "17 %" "2 %"  "23 %" "50 %" 

#Gráfico 23: Bem-estar psicológico
# Gráfico tipo "pizza"
pie(casos_bem_estar,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "white", "black", "red",  "green"),
    labels = paste(names(casos_bem_estar), "-", pct_bem_estar),
    main = "Gráfico 23: Bem-estar psicológico")

#Gráfico 24: Qualidade de vida
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$qualidade_de_vida)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 3, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Dados do Gráfico 25
casos_aulas <- table(dbf.xlsx$aulas_durante_pandemia, exclude = NULL)
casos_aulas
#Foram mais ou menos o mesmo                  Melhoraram             N/A ou Não sabe                    Pioraram 
#                         25                           1                           4                          22 

pct_aulas <- paste(round(unname(casos_aulas) / 
                               sum(unname(casos_aulas)) * 100), "%")
pct_aulas
#[1] "48 %" "2 %"  "8 %"  "42 %" 

#Gráfico 25: Forma de ministrar as aulas
# Gráfico tipo "pizza"
pie(casos_aulas,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red", "black", "white", "green"),
    labels = paste(names(casos_aulas), "-", pct_aulas),
    main = "Gráfico 25: Forma de ministrar as aulas na pandemia")

# Dados do Gráfico 26
casos_acesso_professores <- table(dbf.xlsx$acesso_professores, exclude = NULL)
casos_acesso_professores
#Foi mais ou menos o mesmo                  Melhorou           N/A ou Não sabe                    Piorou 
#                       13                         8                         4                        27

pct_acesso_professores <- paste(round(unname(casos_acesso_professores) / 
                           sum(unname(casos_acesso_professores)) * 100), "%")
pct_acesso_professores
# "25 %" "15 %" "8 %"  "52 %" 

#Gráfico 26: Acesso aos professores na pandemia
# Gráfico tipo "pizza"
pie(casos_acesso_professores,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red", "black", "white", "green"),
    labels = paste(names(casos_acesso_professores), "-", pct_acesso_professores),
    main = "Gráfico 26: Acesso aos professores na pandemia")

# Dados do Gráfico 27
casos_acesso_infra_ies <- table(dbf.xlsx$acesso_infra_ies, exclude = NULL)
casos_acesso_infra_ies
#Ficou mais ou menos o mesmo                    Melhorou             N/A ou Não sabe                    Pioraram 
#                         17                           7                           7                          21

pct_acesso_infra_ies <- paste(round(unname(casos_acesso_infra_ies) / 
                                        sum(unname(casos_acesso_infra_ies)) * 100), "%")
pct_acesso_infra_ies
#[1] "33 %" "13 %" "13 %" "40 %" 

#Gráfico 27: Acesso à infra-estrutura da IES
# Gráfico tipo "pizza"
pie(casos_acesso_infra_ies,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red", "black", "white", "green"),
    labels = paste(names(casos_acesso_infra_ies), "-", pct_acesso_infra_ies),
    main = "Gráfico 27: Acesso à infra-estrutura da IES")

# Dados do Gráfico 28
casos_espaco_fisico <- table(dbf.xlsx$espaco_físico, exclude = NULL)
casos_espaco_fisico
#Era mais ou menos o mesmo                  Melhorou           N/A ou Não sabe                    Piorou 
#                       25                         7                         5                        15

pct_espaco_fisico <- paste(round(unname(casos_espaco_fisico) / 
                                      sum(unname(casos_espaco_fisico)) * 100), "%")
pct_espaco_fisico
#[1] "48 %" "13 %" "10 %" "29 %" 

#Gráfico 28: Espaço físico utilizado para atividades escolares
# Gráfico tipo "pizza"
pie(casos_espaco_fisico,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "orange", "blue", "red", "black", "white", "green"),
    labels = paste(names(casos_espaco_fisico), "-", pct_espaco_fisico),
    main = "Gráfico 28: Espaço físico utilizado para atividades escolares")

# Dados do Gráfico 29
casos_disposicao <- table(dbf.xlsx$disposicao_atividades, exclude = NULL)
casos_disposicao
#   Aumentou                    Diminuiu Ficou mais ou menos a mesma             N/A ou Não sabe 
#         11                          26                          14                           1

pct_disposicao <- paste(round(unname(casos_disposicao) / 
                                   sum(unname(casos_disposicao)) * 100), "%")
pct_disposicao
#[1] "21 %" "50 %" "27 %" "2 %" 

#Gráfico 29: Disposição para participar das atividades escolares
# Gráfico tipo "pizza"
pie(casos_disposicao,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_disposicao), "-", pct_disposicao),
    main = "Gráfico 29: Disposição para participar das atividades escolares")

# Dados do Gráfico 30
casos_performance <- table(dbf.xlsx$desempenho_escolar, exclude = NULL)
casos_performance
#  Aumentou                  Diminuiu Foi mais ou menos o mesmo           N/A ou Não sabe 
#        14                        16                        20                         2

pct_performance <- paste(round(unname(casos_performance) / 
                                sum(unname(casos_performance)) * 100), "%")
pct_performance
#[1] "27 %" "31 %" "38 %" "4 %" 

#Gráfico 30: Desempenho escolar
# Gráfico tipo "pizza"
pie(casos_performance,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_performance), "-", pct_performance),
    main = "Gráfico 30: Desempenho escolar na pandemia")

# Dados do Gráfico 31
casos_ies_reinicio <- table(dbf.xlsx$ies_reinicio, exclude = NULL)
casos_ies_reinicio
#Em parte (apenas algumas atividades  presenciais retornaram)                                              N/A ou Não sabe 
#                                                           14                                                            5 
#         Não, ainda não retornou nenhuma atividade presencial               Sim (retornou todas as atividades presenciais) 
#                                                            4                                                           29
names(casos_ies_reinicio) <- c("Em parte", "N/A ou Não sabe", "Não retornou", "Retornou todas atividades")

pct_ies_reinicio <- paste(round(unname(casos_ies_reinicio) / 
                                 sum(unname(casos_ies_reinicio)) * 100), "%")
pct_ies_reinicio
#[1] "27 %" "10 %" "8 %"  "56 %"

#Gráfico 31: IES reiniciou atividades presenciais
# Gráfico tipo "pizza"
pie(casos_ies_reinicio,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_ies_reinicio), "-", pct_ies_reinicio),
    main = "Gráfico 31: IES reiniciou atividades presenciais")

# Dados do Gráfico 32
casos_vacinado <- table(dbf.xlsx$vacinado, exclude = NULL)
casos_vacinado
#                Sim vacinado com duas doses ou com vacina de dose única 
#                                                                      8 
#Sim vacinado com duas doses, com vacina de dose única e dose de reforço 
#                                                                     21 
#Sim, vacinado com duas doses ou com vacina de dose única e dose(s) de reforço 
#                                                                     19 
# <NA> 
#  4
names(casos_vacinado) <- c("Duas doses ou dose única", 
                               "Duas doses ou dose única e reforço", 
                               "Duas doses ou dose única e doses de reforço",
                               "N/A ou Não quero responder")

pct_vacinado <- paste(round(unname(casos_vacinado) / 
                                  sum(unname(casos_vacinado)) * 100), "%")
pct_vacinado
#[1] "15 %" "40 %" "37 %" "8 %"

#Gráfico 32: Está vacinado
# Gráfico tipo "pizza"
pie(casos_vacinado,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray",  "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_vacinado), "-", pct_vacinado),
    main = "Gráfico 32: Doses de vacinação")

#Gráfico 33: Dificuldades acadêmicas durante a pandelia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$dificuldades_academicas)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Dados do Gráfico 34
casos_despesas <- table(dbf.xlsx$despesas, exclude = NULL)
casos_despesas
#  Aumentaram                    Diminuiram Foram mais ou menos os mesmos 
#          17                            14                            21 

pct_despesas <- paste(round(unname(casos_despesas) / 
                              sum(unname(casos_despesas)) * 100), "%")
pct_despesas
#[1] "33 %" "27 %" "40 %"

#Gráfico 34: Despesas durante a pandemia
# Gráfico tipo "pizza"
pie(casos_despesas,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red","gray", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_despesas), "-", pct_despesas),
    main = "Gráfico 34: Despesas durante a pandemia")
 
# Dados do Gráfico 35
casos_renda <- table(dbf.xlsx$renda_financeira, exclude = NULL)
casos_renda
#Aumentou                   Diminuiu Está mais ou menos a mesma N/A ou Não quero responder 
#       3                         25                         23                          1 

pct_renda <- paste(round(unname(casos_renda) / 
                              sum(unname(casos_renda)) * 100), "%")
pct_renda
#[1] "6 %"  "48 %" "44 %" "2 %"

#Gráfico 35: Renda durante a pandemia
# Gráfico tipo "pizza"
pie(casos_renda,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("gray", "red", "blue","orange", "black", "white", "green"),
    labels = paste(names(casos_renda), "-", pct_renda),
    main = "Gráfico 35: Renda durante a pandemia")
  
# Dados do Gráfico 36
casos_ajuda <- table(dbf.xlsx$ajuda_financeira, exclude = NULL)
casos_ajuda
#Não Sim 
# 44   8

pct_ajuda <- paste(round(unname(casos_ajuda) / 
                           sum(unname(casos_ajuda)) * 100), "%")
pct_ajuda
#[1] "85 %" "15 %"

#Gráfico 36: Recebeu auxílio financeiro da IES
# Gráfico tipo "pizza"
pie(casos_ajuda,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red","blue", "gray","orange", "black", "white", "green"),
    labels = paste(names(casos_ajuda), "-", pct_ajuda),
    main = "Gráfico 36: Recebeu auxílio financeiro da IES")

# Dados do Gráfico 37
casos_endividamento <- table(dbf.xlsx$nivel_endividamento, exclude = NULL)
casos_endividamento
#Aumentaram                    Diminuiram Estão mais ou menos as mesmas               N/A ou Não sabe 
#         7                             6                            36                             3 
pct_endividamento <- paste(round(unname(casos_endividamento) / 
                           sum(unname(casos_endividamento)) * 100), "%")
pct_endividamento
#[1] "85 %" "15 %"

#Gráfico 37: Dívidas durante pandemia
# Gráfico tipo "pizza"
pie(casos_endividamento,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red","blue", "gray","orange", "black", "white", "green"),
    labels = paste(names(casos_endividamento), "-", pct_endividamento),
    main = "Gráfico 37: Dívidas durante a pandemia")

## Gráfico 38 
# Descobrindo as ocorrências
if(!("stringr") %in% installed.packages()) install.packages("stringr")
library(stringr)
perfil_despesas <- dbf.xlsx$despesas_cresceram # selecionando todos os casos da variável despesas cresceram

relacionadas_saude <- length(na.omit(str_match(perfil_despesas, "Relacionadas com saúde")))
viagens_deslocamentos <- length(na.omit(str_match(perfil_despesas, "Viagens/deslocamentos")))
transporte_urbano <- length(na.omit(str_match(perfil_despesas, "Transporte urbano")))
aluguel <- length(na.omit(str_match(perfil_despesas, "Aluguel")))
internet <- length(na.omit(str_match(perfil_despesas, "Internet")))
alimentacao <- length(na.omit(str_match(perfil_despesas, "Alimentação")))  
outras <- length(na.omit(str_match(perfil_despesas, "Outras")))

casos_perfil_despesas <- c(relacionadas_saude, viagens_deslocamentos, transporte_urbano, aluguel, internet, alimentacao, outras)
names(casos_perfil_despesas) <- c("Relac. saúde", "Deslocamento", "Transp. urbano", 
                                  "Aluguel", "Internet", "Alimentação", "Outras")
casos_perfil_despesas
#Relac. saúde  Deslocamentos Transp. urbano        Aluguel       Internet    Alimentação         Outras 
#          16             21             22             18             18             37             15

pct_perfil_despesas <- paste0(round(unname(casos_perfil_despesas) / sum(unname(casos_perfil_despesas)) * 100,0), "%")
pct_perfil_despesas  
#[1] "11%" "14%" "15%" "12%" "12%" "25%" "10%"

# Gráfico do tipo barras
graph.perfil.despesas <- barplot(casos_perfil_despesas, 
                       main = "Gráfico 38: Despesas que cresceram na pandemia",
                       xlab = "Despesas", 
                       ylab = "Quantidade",
                       col = "orange",
                       ylim = c(0,max(casos_perfil_despesas) + 30))
text(x = graph.perfil.despesas, y = casos_perfil_despesas, label = unname(casos_perfil_despesas), cex=1, pos=3)
axis(1, at=graph.perfil.despesas, labels=paste("(", pct_perfil_despesas, ")"), tick=F, las=1, line=-1.0, cex.axis= 1.1)      

#Gráfico 39: Dificuldades financeiras durante a pandelia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$dificuldades_financeiras)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Dados do Gráfico 40
casos_fechar_ies <- table(dbf.xlsx$decisao_fechar, exclude = NULL)
casos_fechar_ies
#De forma oportuna e prudente             Muito lentamente            Muito rapidamente 
#                          40                            7                            5

pct_fechar_ies <- paste(round(unname(casos_fechar_ies) / 
                                   sum(unname(casos_fechar_ies)) * 100), "%")
pct_fechar_ies
#[1] "77 %" "13 %" "10 %"

#Gráfico 40: Fechar IES e utilizar ferramentas online
# Gráfico tipo "pizza"
pie(casos_fechar_ies,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red","blue", "gray","orange", "black", "white", "green"),
    labels = paste(names(casos_fechar_ies), "-", pct_fechar_ies),
    main = "Gráfico 40: Decisão de fechar IES e utilizar ferramentas online")

#Gráfico 41: IES fez de positivo na pandemia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$ies_positivo)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

#Gráfico 42: IES poderia melhorar na pandemia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$ies_melhorar)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

#Gráfico 43: IES poderia melhor ajudar na pandemia
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$ies_ajudar)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 2, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))

# Dados do Gráfico 44
casos_ansiedade <- table(dbf.xlsx$nivel_ansiedade, exclude = NULL)
casos_ansiedade
#      Melhor do que antes Muito melhor do que antes   Muito pior do que antes           N/A ou Não Sabe          O mesmo de antes 
#                        2                         3                        10                         2                        17 
#Pior do que antes 
#               18

pct_aniedade <- paste(round(unname(casos_ansiedade) / 
                                sum(unname(casos_ansiedade)) * 100), "%")
pct_aniedade
#[1] "4 %"  "6 %"  "19 %" "4 %"  "33 %" "35 %"

#Gráfico 44: Nível de ansiedade para o futuro
# Gráfico tipo "pizza"
pie(casos_ansiedade,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("white","blue", "gray","orange", "black", "red", "green"),
    labels = paste(names(casos_ansiedade), "-", pct_aniedade),
    main = "Gráfico 44:  Nível de ansiedade quanto ao futuro")

# Dados do Gráfico 45
casos_ansiedade_planejamento <- table(dbf.xlsx$ansiedade_planejamento, exclude = NULL)
casos_ansiedade_planejamento
#Melhor do que antes Muito melhor do que antes   Muito pior do que antes          O mesmo de antes         Pior do que antes 
#                  7                         2                         8                        13                        22 

pct_ansiedade_planejamento <- paste(round(unname(casos_ansiedade_planejamento) / 
                              sum(unname(casos_ansiedade_planejamento)) * 100), "%")
pct_ansiedade_planejamento
#[1] "13 %" "4 %"  "15 %" "25 %" "42 %"

#Gráfico 45: Nível de ansiedade para planejamento pessoal
# Gráfico tipo "pizza"
pie(casos_ansiedade_planejamento,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("black","blue", "gray","orange", "red", "white", "green"),
    labels = paste(names(casos_ansiedade_planejamento), "-", pct_ansiedade_planejamento),
    main = "Gráfico 45: Nível de ansiedade para planejamento pessoal")

# Dados do Gráfico 46
casos_ansiedade_longo <- table(dbf.xlsx$ansiedade_longo_prazo, exclude = NULL)
casos_ansiedade_longo
# Melhor do que antes Muito melhor do que antes   Muito pior do que antes          O mesmo de antes         Pior do que antes 
#                   3                         2                        12                        19                        16                   7                         2                         8                        13                        22 

pct_ansiedade_longo <- paste(round(unname(casos_ansiedade_longo) / 
                                            sum(unname(casos_ansiedade_longo)) * 100), "%")
pct_ansiedade_longo
#[1] "6 %"  "4 %"  "23 %" "37 %" "31 %"

#Gráfico 46: Nível de ansiedade para planejamento a longo prazo
# Gráfico tipo "pizza"
pie(casos_ansiedade_longo,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("black","blue", "gray","orange", "red", "white", "green"),
    labels = paste(names(casos_ansiedade_longo), "-", pct_ansiedade_longo),
    main = "Gráfico 46: Nível de ansiedade para planejamento a longo prazo")

#Gráfico 47: Comentários finais
#Carregando o texto e eliminando os NA (missing data)
texto <- na.omit(dbf.xlsx$detalhes_finais)

# Criando um corpus  
docs <- Corpus(VectorSource(texto))

# Limpando o texto
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("portuguese"))

# Criar uma matrix de termos de documento
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# Gerar a núvem de palavras
set.seed(1234) # para reprodutibilidade 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, # maior "min.freq", maior/precisão, menor número/palavras     
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
