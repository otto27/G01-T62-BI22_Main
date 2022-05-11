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
#remove(n, nomes_folhas)
# -------------------------------------------------





## operações com dataframes ##
##  dá a estrutura e dimensão da sheet customers ##
str(customers)
dim(customers)

#O calendário não é preciso considerar, pois existem funções que permitem extrair os dados

#carregar todos os dataframes para RAM

file_nomes <- list.files(getwd(), pattern = ".Rda")
dont_load <- c("calendar", "productCategory", "productSubCategory")

for (i in file_nomes) {
    load(i)
}
rm(i, file_nomes)

# -------------------------------------------------

#Feito na aula 11 maio

factSales$salesYear <- as.factor(year(factSales$OrderDate))
factSales$salesQuarter <- as.factor(quarters(factSales$OrderDate))
factSales$salesMonth <- as.factor(months(factSales$OrderDate, abbreviate = TRUE))
factSales$salesDay <- as.factor(weekdays(factSales$OrderDate, abbreviate = TRUE))

factSales$salesMonth <- ordered(factSales$salesMonth, 
                                levels = c("january", "february", "march", 
                                           "april", "may", "june", "july",
                                           "august", "september", "october",
                                           "november", "december"))


