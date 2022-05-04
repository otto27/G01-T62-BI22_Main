#### -----
#### 
#### código para o relatório
####
#### -----


## instalação de bibliotecas ##
## executar em todas as sessões ##

bibliotecas <- c("knitr", "rmdformats", "readxl", "lubridate", "ggplot2", "DT",
                 "dplyr")
for (bibl in bibliotecas) {
    if (bibl %in% rownames(installed.packages()) == FALSE){
        install.packages(bibl, dependencies = TRUE)
        library(bibl, character.only = TRUE)    
    }
    else {
        library(bibl, character.only = TRUE)
    }
}
rm(bibl, bibliotecas)
# ----------------------------------------------------------------------------------------




## leitura dos dados e construção de data frames ##
## executar apenas uma vez! ##

library("readxl")
nomes_folhas <- excel_sheets("bidm.xlsx")
for(n in nomes_folhas) {
    assign(n, read_excel("bidm.xlsx", sheet = n))
    save(list = n, file = paste0(n, ".Rda"))
}
remove(n, nomes_folhas)
# -------------------------------------------------





## operações com dataframes ##
##  dá a estrutura e dimensão da sheet customers ##
str(customers)
dim(customers)

# -------------------------------------------------