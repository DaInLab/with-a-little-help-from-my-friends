

# Função para obter em qual ambiente (Sistema Operacional) o RStudio está sendo executado
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

if(!("SmartEDA") %in% installed.packages()) install.packages("SmartEDA")
library(SmartEDA)
if(!("ISLR") %in% installed.packages()) install.packages("ISLR")
library("ISLR")

Carseats <- ISLR::Carseats
ExpData(data=Carseats,type=1)
# saída

ExpNumStat(Carseats,by="A",gp=NULL,Qnt=NULL,MesofShape=2, Outlier=FALSE,round=2,Nlim=10)
#Saída- Resumo das variáveis numéricas dos dados Carseats

ExpCTable(Carseats)
#Output- Resumo das variáveis categóricas dos dados Carseats  

# Scatter plot
ExpNumViz(Carseats,target = "Price",nlim=4,fname=NULL, col=NULL,Page=NULL,sample=1)
# Density plot
ExpNumViz(Carseats,target=NULL,nlim=10,sample=1)
# Bar plot
ExpCatViz(Carseats,target=NULL,clim=5,margin=2,sample=1)
# Box plot
ExpNumViz(Carseats,target="US",type=2,nlim=10,sample=1)
# Normality plot
ExpOutQQ(Carseats,nlim=10,sample=1)
# Co-ordinate plots
ExpParcoord(Carseats,Group="ShelveLoc",Stsize=c(10,15,20),Nvar=
                 c("Price","Income","Advertising","Population","Age","Education"))
 