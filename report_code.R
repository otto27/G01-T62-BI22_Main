#### -----
#### 
#### Código para o relatório
####
#### -----


## ************ ROUTINES NEEEDED SO THE CODE CAN RUN SMOOTHLY ----
## Run them at the start of every session

## Instalação de bibliotecas ##
## executar em todas as sessões ##




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
sales_amt_bar
## By product Subcategory
subs_sales_amount <- aggregate(SalesAmount ~ SubCategory + salesYear,
                             data = sales_av, FUN = sum, na.rm = TRUE)

subs_most_sales <- subs_sales_amount |> arrange(desc(SalesAmount)) |> head(5)

subs_most_sales_bar <- ggplot(subs_most_sales, aes(x=salesYear, 
                                                   y=SalesAmount, size = pop)) +
                                geom_point(alpha=0.7)


#Modelos por faturação - por redundância, este DF é apenas informativo
models_sales_amount <- aggregate(SalesAmount ~ ProductName + salesYear, 
                                data = sales_av, FUN = sum, na.rm = TRUE)

#Names of models with most sales
models_most_sales <- models_sales_amount |> arrange(desc(SalesAmount)) |> head(15)

#Cores de produtos com mais procura para efeitos de stock

color_prod_df <- aggregate(SalesAmount ~ Color, data = sales_av, FUN = sum, na.rm = T)
table_color <- formattable(color_prod_df, align =c("c", "c"), 
                      list('SalesAmount' = color_bar("lightblue")))

table_color


##  ************************ END PRODUTO QUE VENDE MAIS 
#PLOTTING





## ************************* ANALISE DE VENDAS 
### POR QUARTER E ANO

sales_quarters_years <- aggregate(SalesAmount ~ salesQuarter + salesYear, 
                                  data = sales_av, FUN = sum, na.rm = TRUE)

salesAmount_quarter_year <- ggplot(sales_quarters_years, aes(x = factor(salesQuarter), y = SalesAmount/10^6)) +
    geom_bar(position=position_dodge(width = .2), stat="identity", size=.3,    colour="lightblue") +
    facet_wrap(~ salesYear, nrow = 4) +
    labs(title = "Sales Amount by Quarters",
         subtitle = " Years: 2016 to 2019",
         x = "Quarters",
         y = "Sales Amount")

salesAmount_quarter_year

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

sales_period_bar




##  REGIONS ANALYSIS ----

#Evolução de Vendas nos países
sales_countries_region <- aggregate(SalesAmount ~ Country + salesYear, data = sales_av, FUN = sum, na.rm = TRUE)

regions_sales <- ggplot(sales_countries_region, aes(fill=Country, y=SalesAmount/1000, x=salesYear)) + 
    geom_bar(position="dodge", stat="identity")
 
regions_sales


##  CLIENTS ANALYSIS ----

## Média de gastos dos clientes/mês
## Média de idades de clientes / territorio
clients_df <- aggregate(SalesAmount ~ CustomerKey + Region + YearlyIncome, data = sales_av,
                        FUN = sum, na.rm = TRUE) 

#Where our best 100 clients are located, by quantity and avg money spent
# ordered
clients_df <- clients_df[order(clients_df$SalesAmount, decreasing = TRUE),] 

# top 100 clients
clients_df_top100 <- head(clients_df, 100)

#***************Distribuição de Rendimentos dos nossos top 100 clientes
hist(clients_df_top100$YearlyIncome, xlab = "Rendimento top100 clientes", 
     main = "Histograma de Rendimentos Top 100 Clientes")

## Vector with unique region values
vec_regions <- sort(unique(clients_df_top100$Region))
avg_values <- list(country = vec_regions,
                   avg_spending = c(),
                   quantity = c())

for (region in avg_values$country) {
    countries <- subset(clients_df_top100, Region == region)
    counter <- as.integer(length(countries$CustomerKey))
    percentage_of_clients <- (counter / length(clients_df_top100$CustomerKey)) * 100
    avg_spending <- round(mean(countries$SalesAmount))
    #append to list
    avg_values$avg_spending <- c(avg_values$avg_spending, avg_spending)
    avg_values$quantity <- c(avg_values$quantity, percentage_of_clients)
}
remove(countries, counter, percentage_of_clients, avg_spending)

#Convert to dataframe
best_clients_df <- as.data.frame(avg_values)
best_clients_df$country <- as.factor(best_clients_df$country)


#Create Graph
best_clients_bar_colors <- c("#FEF9A7", "#37E2D5", "#C70A80", "#9BA3EB")
sizeRange <- c(2,12)
best_clients_bar <- ggplot(best_clients_df, aes(x = avg_spending, y = quantity,
                                                size = quantity, color=country)) + 
                    geom_point(alpha = 0.7) +
                    scale_size(range = sizeRange) +
                    labs(title = "Top Customers By Region", 
                        x = "Gasto médio de Cliente", 
                        y = "Concentração(em %)",
                        color = "Países") 
ggplotly(best_clients_bar)


# Distribuição de idades dos clientes
##Adicionar coluna nova de idade ao DF sales_av
Age <- as.integer(format(Sys.Date(), "%Y")) - (as.integer(format(sales_av$BirthDate, format = "%Y")))
sales_av <- add_column(sales_av, Age, .after = which(colnames(sales_av) == "BirthDate"))

##Build histogram
hist(sales_av$Age, xlab = "Idades", main = "Histograma de Idades")

#Distribuição do rendimentos
hist(sales_av$YearlyIncome, xlab = "Distribuição Rendimentos (em milhares)",
     breaks = 10, main = "Histograma de Rendimentos")


#PERFIL DE CLIENTES COM CARRO
car_clients_df <- aggregate(SalesAmount ~ NumberCarsOwned, 
                            data = sales_av, FUN = sum, na.rm = T)
car_clients_df <- car_clients_df[order(car_clients_df$SalesAmount, decreasing = T),]

#Perfil clientes com criancas
children_clients_df <- aggregate(SalesAmount ~ NumberChildrenAtHome, 
                            data = sales_av, FUN = sum, na.rm = T)
children_clients_df <- children_clients_df[order(children_clients_df$SalesAmount, decreasing = T),]
 














