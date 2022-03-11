# pof2017package

Pacote para ler e analisar dados da POF 2017-2018 (2008-2009 em breve)
## Qualquer coisa:
prubincosta@gmail.com

http://twitter.com/pedrorubincosta

http://github.com/pedrorubin

https://www.linkedin.com/in/pedro-rubin-78309597/

## Como instalar
devtools::install_github("pedrorubin/pof2017package")

## Como carregar
library(pof2017package)

## Pagina de ajuda (no R)
help(package = "pof2017package")

## Breve descrição das funções
### baixar_pof
Permite baixar os microdados da POF em uma pasta no computador. Se a pasta não existir, será criada

### ler_pof_geral
Toma como argumento o arquivo de microdados.txt e retorna um dataframe com todos os dados daquele registro

### ler_pof_despesa
Igual a ler_pof_geral, mas guarda apenas os dados relevantes para análise de despesa

### ler_pof_despesa_todas
Mesma coisa que a função acima, mas lê de uma vez todos os registros com dados de despesa

### ler_pof_rendimento
Igual a ler_pof_geral, mas guarda apenas os dados relevantes para análise de rendimento

### ler_pof_rendimento_todas
Mesma coisa que a função acima, mas lê de uma vez todos os registros com dados de rendimento

### calcular_despesa_mensal
Calcula a média mensal deflacionada dos gastos de despesa (por tipo). Para ver os tipos de despesa, consultar a tabela "indice_despesa". Similar à tabela 6715 do SIDRA. Em breve vou incluir subdivisões como UF e região.

### calcular_rendimento_mensal
Calcula a média mensal deflacionada dos gastos de rendimento. Para ver os tipos de rendimento, consultar a tabela "indice_rendimento". Similar à tabela 6974 do SIDRA. Em breve vou incluir subdivisões como UF e região.

### montar_tabela_despesa_uc
Retorna uma tabela com todos os dados das UC do registro "MORADOR" (referência chefe da uc) e mais o valor dos gastos de despesa (por tipo) daquela uc como colunas. Para ver os tipos de despesa, consultar a tabela "indice_despesa". 

### montar_tabela_rendimento_uc
Retorna uma tabela com todos os dados das UC do registro "MORADOR" (referência chefe da uc) e mais o valor dos gastos de rendimento (por tipo) daquela uc como colunas. Para ver os tipos de rendimento, consultar a tabela "indice_rendimento". 
