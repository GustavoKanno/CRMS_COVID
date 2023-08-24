# criando tabela para comportar os resultados do question?rio
df = data.frame(matrix(ncol = 17,nrow = 0))
colnames(df) = c('id','start','version', 'ageyrs','sexmf','stay_home','injury_acc','pos_test','fev_3dys',
                 'ext_fatig','cont_cough','loss_smell','diff_breat','cont_case','trav_exp','prob_covid','prob_other')

# salvando a tabela (vazia) em um formato .csv
write.table(df,'registers_crms.csv',sep = ',',row.names = FALSE)



