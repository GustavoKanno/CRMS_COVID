source('src/crms_prediction_function.R')

# COLOQUE AQUI O NOME DO ARQUIVO A SER LIDO
df = read.csv('src/data/df_test/df_test1.csv')

# nome do arquivo de sa?da
output_name = 'registers_crms_teste'

# tipo do arquivo
type = 'csv' 

# caso j? exista um arquivo com este nome e deseja-se adicionar as novas previs?es append = TRUE
# caso n?o exista o arquivo, append = FALSE
append = FALSE

# Caso esteja-se usando a decodifica?ao das bases de SRAG (0,1,2,9), decoding = TRUE
decoding = TRUE


crms_prediction_function(df = df,output_name = output_name,type = type,append = append,decoding = decoding)


