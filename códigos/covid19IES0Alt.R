# - ETAPA INICIAL
#--- Importação e preparação dos dados
# versão inicial em 05/01/2023.
# versão do projeto "Impacto da Covid 19 nos Estudantes de ENsino Superior" no GitHub

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
wordcloud(words = df$word, freq = df$freq, min.freq = 2,      
          max.words=200, random.order=FALSE, rot.per=0.35,       
          colors=brewer.pal(8, "Dark2"))
