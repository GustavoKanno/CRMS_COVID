source('src/crms_function.R')

crms_prediction_function = function(df,output_name = 'registers_crms',type = 'csv',append = TRUE,decoding = FALSE){
  
  version = 'CRMS 1.0'
  
  if (decoding){
    df_predict_crms = decode(df)
  }
  else {
    df_predict_crms = df[,]
  }
  
  df_predictions = data.frame()
  
  for (k in 1:nrow(df_predict_crms)){
    
    id = df_predict_crms[k,'id']
    idade = df_predict_crms[k,'ageyrs']
    sexo = df_predict_crms[k,'sexmf']
    dist = df_predict_crms[k,'stay_home']
    acid = df_predict_crms[k,'injury_acc']
    comprov = df_predict_crms[k,'pos_test']
    febre = df_predict_crms[k,'fev_3dys']
    fadiga = df_predict_crms[k,'ext_fatig']
    tosse = df_predict_crms[k,'cont_cough']
    olfpal = df_predict_crms[k,'loss_smell']
    resp = df_predict_crms[k,'diff_breat']
    contato = df_predict_crms[k,'cont_case']
    area = df_predict_crms[k,'trav_exp']
    probs = crms(id,idade,sexo,dist,acid,comprov,febre,fadiga,tosse,olfpal,resp,contato,area)
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
    time = format(Sys.time(),"%D %X")
    
    df_predictions = rbind(df_predictions,c(id,time,version,idade,sexo,dist,acid,
                                            comprov,febre,fadiga,tosse,olfpal,resp,contato,area,
                                            prob_cov,prob_nao_cov))
  }
  
  
  colnames(df_predictions) = c('id','start','version', 'ageyrs','sexmf','stay_home','injury_acc','pos_test','fev_3dys',
  'ext_fatig','cont_cough','loss_smell','diff_breat','cont_case','trav_exp','prob_covid','prob_other')
  
  file_name = paste(output_name,'.',type,sep='')
  
  if (append){
    write.table(df_predictions,file_name,sep = ',',
                append=TRUE,col.names = FALSE, row.names = FALSE)
  }
  else {
    write.table(df_predictions,file_name, sep = ',',
                append=FALSE,col.names = TRUE, row.names = FALSE)
  }
  
}

# Orquestra a decodifica??o do banco de dados do formato SRAG para o
# o do CRMS

decode = function(df){
  df$ageyrs  = as.numeric(df$ageyrs) 
  df$sexmf = as.numeric(df$sexmf)
  df$stay_home = as.numeric(df$stay_home)
  df$injury_acc = as.numeric(df$injury_acc)
  df$pos_test = as.numeric(df$pos_test)
  df$fev_3dys  = as.numeric(df$fev_3dys)
  df$ext_fatig  = as.numeric(df$ext_fatig)
  df$cont_cough  = as.numeric(df$cont_cough)
  df$loss_smell  = as.numeric(df$loss_smell)
  df$diff_breat  = as.numeric(df$diff_breat)
  df$cont_case  = as.numeric(df$cont_case)
  df$trav_exp = as.numeric(df$trav_exp)
  
  df_predict_crms = data.frame('id' = df$id)
  df_predict_crms['ageyrs'] = df$ageyrs
  df_predict_crms['sexmf'] = transformer_sexo(df$sexmf)
  df_predict_crms['stay_home'] = transformer_geral(df$stay_home)
  df_predict_crms['injury_acc'] = transformer_geral(df$injury_acc)
  df_predict_crms['pos_test'] = transformer_geral(df$pos_test)
  df_predict_crms['fev_3dys'] = transformer_geral(df$fev_3dys)
  df_predict_crms['ext_fatig'] = transformer_geral(df$ext_fatig)
  df_predict_crms['cont_cough'] = transformer_geral(df$cont_cough)
  df_predict_crms['loss_smell'] = transformer_geral(df$loss_smell)
  df_predict_crms['diff_breat'] = transformer_geral(df$diff_breat)
  df_predict_crms['cont_case'] = transformer_geral(df$cont_case)
  df_predict_crms['trav_exp'] = transformer_geral(df$trav_exp)
  
  return(df_predict_crms)
}

# Transformador para casos em que est? sendo usada a codifica??o
# do Banco de Dados do SRAG

# Transformador para a vari?vel Sexo 

transformer_sexo = function(coluna){
  coluna = ifelse(coluna == 0, 'F',coluna)
  coluna = ifelse(coluna == 1,'M',coluna)
  coluna = ifelse(coluna == 9  | is.na(coluna),'D',coluna)
}

# Transformador para as outras vari?veis categ?ricas 

transformer_geral = function(coluna){
  coluna = ifelse(coluna == 1 | coluna ==0, 'Y',coluna)
  coluna = ifelse(coluna == 2,'N',coluna)
  coluna = ifelse(coluna == 9  | is.na(coluna) ,'D',coluna)
  
}