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
         y = "Sales Amount ")


#beautiful tables
formattable(df_agg1, align =c("c", "c"),
            list('SalesAmount' = color_bar("cyan")))


## ************             END OF ROUTINES                   *****************







## *****************  DATA FRAME MANIPULATION ----
## estrutura e dimensão da sheet customers ##
str(customers)
dim(customers)

## TODO
## Adicionar uma coluna de idade ao sales_av através da subtração das colunas
## AsDate


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



## SALES ANALYSIS ----


## LEANER DATAFRAMES

#acrescentar uma coluna ao SALES_AV com a idade dos clientes

## ************** QUE CATEGORIA DE PRODUTO VENDE MAIS - quantity **************

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


##  ************************ END PRODUTO QUE VENDE MAIS 
#PLOTTING





## ************************* ANALISE DE VENDAS 
### POR QUARTER E ANO

sales_quarters_years <- aggregate(SalesAmount ~ salesQuarter + salesYear, 
                                  data = sales_av, FUN = sum, na.rm = TRUE)

salesAmount_quarter_year <- ggplot(sales_quarters_years, aes(x = factor(salesQuarter), y = SalesAmount/10^6)) +
    geom_col(fill = "lightblue") + 
    facet_wrap(~ salesYear, nrow = 4) +
    labs(title = "Sales Amount by Quarters",
         subtitle = " Years: 2016 to 2019",
         x = "Quarters",
         y = "Sales Amount")

### Por frequência de vendas de modo a saber períodos de tempo em que houve
### mais ativididade
sales_period <- aggregate(SalesAmount ~ salesMonth + salesYear + salesMonthday, 
                      data = sales_av, FUN = mean, na.rm = TRUE)

sales_period$salesMonth <- ordered(sales_period$salesMonth, levels = month_ord)

month_ord <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
               "Sep", "Oct", "Nov", "Dec")

sales_period_bar <- ggplot(sales_period, aes(x = as.integer(salesMonthday), 
                                  y = SalesAmount/1000)) + 
                                        geom_line (color = "lightblue") +
                                        geom_hline(yintercept = 1.5, 
                                                  linetype = "dashed",
                                                  color = "red",
                                                  size = 0.2) +
                                        geom_point(size = 0.3) + 
                                        scale_x_continuous(breaks = c(1, 7, 15, 22, 31)) + 
                                        facet_grid(salesMonth ~ salesYear) + 
                                        labs(title = "Average Daily Sales", 
                                             subtitle = "Years: 2016 to 2019", 
                                             x = "Weekdays", 
                                             y = "Sales Amount(thousand dollars)") +
                                        theme_light()



##  REGIONS ANALYSIS ----
#Aula 18 May

# - littlemissdata

sales_countries_region <- aggregate(SalesAmount ~ Country + Region + salesYear, data = sales_av, FUN = sum, na.rm = TRUE)
 



##  CLIENTS ANALYSIS ----

## Média de gastos dos clientes/mês


## Média de idades de clientes / territorio
clients_df <- aggregate(SalesAmount ~ CustomerKey + Region, data = sales_av,
                        FUN = sum, na.rm = TRUE) 


# ordered
clients_df <- clients_df[order(clients_df$SalesAmount, decreasing = TRUE),] 

# top 20 clients
clients_df_top20 <- head(clients_df, 50)




#

























