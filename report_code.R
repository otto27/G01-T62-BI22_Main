#### -----
#### 
#### Código para o relatório
####
#### -----


## ************ ROUTINES NEEEDED SO THE CODE CAN RUN SMOOTHLY ----
## Run them at the start of every session

## Instalação de bibliotecas ##
## executar em todas as sessões ##

bibliotecas <- c("knitr", "rmdformats", "readxl", "lubridate", "ggplot2", "DT",
                 "dplyr", "formattable", "plotly")
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


## Leitura dos dados e construção de data frames


library("readxl")
nomes_folhas <- excel_sheets("bidm.xlsx")
for(n in nomes_folhas) {
    assign(n, read_excel("bidm.xlsx", sheet = n))
    save(list = n, file = paste0(n, ".Rda"))
}
remove(n, nomes_folhas)





## *************        UTILS  ----
## Run in the beginning of every session


# Negate the %in% operator - nin significa Not In 
# https://www.rdocumentation.org/packages/functional/versions/0.6/topics/Negate
`%nin%` = Negate(`%in%`)

#função para fazer a estetica dos gráficos
graf_bar <- ggplot(df_agg2, aes(x = factor(Country), y = SalesAmount/1000)) +
    geom_col(fill = "lightblue") + 
    facet_wrap(~ salesYear, nrow = 4) +
    labs(title = "Sales Amount by Country",
         subtitle = " Years: 2016 to 2019",
         x = "Country",
         y = "Sales Amount (k dollars)")


#beautiful tables
formattable(df_agg1, align =c("c", "c"),
            list('SalesAmount' = color_bar("cyan")))


## ************             END OF ROUTINES                   *****************







## *****************  DATA FRAME MANIPULATION ----
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




## SALES PRODUCT ANALYSIS ----

# Generate leaner dataframes

# Products - for product analysis
sales_product <- merge(factSales, products, by = "ProductKey")
sales_product <- subset(sales_product, select = -c(SalesOrderLineNumber))
save(sales_product, file = "sales_product.Rda")

# Clients - for client analysis
clients_purchases <- merge(factSales, customers, by = "CustomerKey")
clients_purchases <- subset(clients_purchases, select = -c(SalesOrderNumber,
                                                           SalesOrderLineNumber,
                                                           OrderQuantity,
                                                           UnitPrice,
                                                           ProductStandardCost,
                                                           TaxAmt,
                                                           Freight))

##QUE CATEGORIA DE PRODUTO VENDE MAIS - quantity

##By Product Category

# Por faturação
accs_sales_amount <- aggregate(SalesAmount ~ Category + salesYear, data = sales_av, 
                                FUN = sum, na.rm = TRUE)

sales_amt_bar <- 
    ggplot(accs_sales_amount, aes(fill = Category, x = salesYear, y = SalesAmount/10^6)) +
    geom_bar(position = "fill", stat = "identity") +
    labs(title = "Sales Amount by Categories",
         subtitle = " Years: 2016 to 2019",
         x = "Categories",
         y = "Percentages of Sales") +
    scale_y_continuous(labels = scales::percent) 

## By product Subcategory

subs_sales_amount <- aggregate(SalesAmount ~ SubCategory + salesYear,
                             data = sales_av, FUN = sum, na.rm = TRUE)

subs_most_sales <- subs_sales_amount |> arrange(desc(SalesAmount)) |> head(5)


#Modelos por faturação
models_sales_amount <- aggregate(SalesAmount ~ ProductName + salesYear, 
                                data = sales_av, FUN = sum, na.rm = TRUE)

#Names of models with most sales
models_most_sales <- models_sales_amount |> arrange(desc(SalesAmount)) |> head(15)



#PLOTTING




##  REGIONS ANALYSIS ----
#Aula 18 May
## Aggregate
df_agg1 <- aggregate(SalesAmount ~ Country, data = sales_av, FUN = sum, na.rm = TRUE)
df_agg1 
# - littlemissdata

df_agg2 <- aggregate(SalesAmount ~ Country + Region + salesYear, data = sales_av, FUN = sum, na.rm = TRUE)
df_agg2 



##  CLIENTS ANALYSIS ----


#

























