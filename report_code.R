#### -----
#### 
#### Código para o relatório
####
#### -----


## ************ ROUTINES NEEEDED SO THE CODE CAN RUN SMOOTHLY *****************
## Run them at the start of every session

## Instalação de bibliotecas ##
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
# ------------------------------------------------------------------------------



## Leitura dos dados e construção de data frames ##
## Executar apenas uma vez! ##

library("readxl")
nomes_folhas <- excel_sheets("bidm.xlsx")
for(n in nomes_folhas) {
    assign(n, read_excel("bidm.xlsx", sheet = n))
    save(list = n, file = paste0(n, ".Rda"))
}
remove(n, nomes_folhas)
# ------------------------------------------------------------------------------

## *************        UTILS  
## Run in the beginning of every session


# Negate the %in% operator - nin significa Not In 
# https://www.rdocumentation.org/packages/functional/versions/0.6/topics/Negate
`%nin%` = Negate(`%in%`)

## ************             END OF ROUTINES                   *****************



# -------------          -------------------          ----------------   ------



## *****************  DATA FRAME MANIPULATION **********************************
##estrutura e dimensão da sheet customers ##
str(customers)
dim(customers)


###  Carregar todos os dataframes para RAM

file_nomes <- list.files(getwd(), pattern = ".Rda")
dont_load <- c("calendar.Rda", "productCategory.Rda", "productSubCategory.Rda")
#O calendário não é preciso considerar, pois existem funções que permitem 
#próprias do R para esse efeito

for (i in file_nomes) {
    if (i %nin% dont_load) {
        load(i)
    }
}
rm(i, file_nomes)

# -----------------------------------------------------------------------------

#Feito na aula 11 maio
#Determinação dos dados categorizados como Factor
factSales$salesYear <- as.factor(year(factSales$OrderDate))
factSales$salesQuarter <- as.factor(quarters(factSales$OrderDate))
factSales$salesMonth <- as.factor(months(factSales$OrderDate, abbreviate = TRUE))
factSales$salesDay <- as.factor(weekdays(factSales$OrderDate, abbreviate = TRUE))

factSales$salesMonth <- ordered(factSales$salesMonth, 
                                levels = c("january", "february", "march", 
                                           "april", "may", "june", "july",
                                           "august", "september", "october",
                                           "november", "december"))


## SALES PRODUCT ANALYSIS 

# Generate leaner dataframes
sales_product <- merge(factSales, products, by = "ProductKey")
sales_product <- subset(sales_product, select = -c(SalesOrderLineNumber))
save(sales_product, file = "sales_product.Rda")


##QUE CATEGORIA DE PRODUTO VENDE MAIS - quantity?
#By Product Category
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/table
sales_category_totals <- table(sales_product$Category)















