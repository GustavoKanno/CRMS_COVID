#diretorio
# setwd("C:/Users/Gustavo Kanno/Documents/IC/novo/final")

crms_questionaire = function(){
  
    # vers?o do programa
  version = 'CRMS 1.0'
  
  #resgatando prioris
  prioris_geral = read.csv('prioris_geral.csv')
  prioris_sexo = read.csv('prioris_sex.csv')
  
  # registrando o id do caso
  id = readline(prompt = 'Digite o ID do caso: ')
  
  #inicializando as vari?veis
  prob_cov = 0.5
  prob_nao_cov = 0.5
  
  #idade
  idade = readline(prompt = 'Qual era a idade do indiv?duo? ')
  idade = suppressWarnings(as.numeric(idade))
  while (suppressWarnings(is.na(idade)) | idade < 0){
    idade = readline(prompt = 'Por favor, insira um n?mero inteiro positivo: ')
    idade = suppressWarnings(as.numeric(idade))
  }
  prob_idade = func_idade(prob_cov,prob_nao_cov,idade)
  prob_cov = prob_idade[1]
  prob_nao_cov = prob_idade[2]
  
  #sexo
  sexo = readline(prompt = 'Qual era o sexo do indiv?duo? ')
  prob_sexo = func_sexo(prob_cov,prob_nao_cov,sexo,prioris_sexo)
  prob_cov = as.numeric(prob_sexo[1])
  prob_nao_cov = as.numeric(prob_sexo[2])
  sexo = prob_sexo[3]
  sexo = toupper(sexo)
  
  #distanciamento social
  dist = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo viveu em uma ?rea com dist?nciamento ou permaneceu em casa? ')
  if (dist != 'D' & dist != 'd'){
    priori = get_priori('dist',dist,prioris_geral)
    dist = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  dist = toupper(dist)
  
  #acidente
  acid = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo teve alguma les?o ou acidente que o levou a morte? ')
  if (acid != 'D' & acid != 'd'){
    priori = get_priori('acid',acid,prioris_geral)
    acid = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  acid = toupper(acid)
  
  #diagnostico
  comprov = readline(prompt = 'Durante as ?ltimas duas semanas, um profissional da sa?de confirmou que o indiv?duo havia contra?do a COVID-19? ')
  if (comprov != 'D' & comprov != 'd'){
    priori = get_priori('comprov',comprov,prioris_geral)
    comprov = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  comprov = toupper(comprov)
  
  #febre
  febre = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo teve febre por pelo menos 3 dias? ')
  if (febre != 'D' & febre != 'd'){
    priori = get_priori('febre',febre,prioris_geral)
    febre = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  febre = toupper(febre)
  
  #fadiga extrema
  fadiga = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo apresentou fadiga extrema? ')
  if (fadiga != 'D' & fadiga != 'd'){
    priori = get_priori('fadiga',fadiga,prioris_geral)
    fadiga = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  fadiga = toupper(fadiga)
  
  #tosse
  tosse = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo teve tosse? ')
  if (tosse != 'D' & tosse != 'd'){
    priori = get_priori('tosse',tosse,prioris_geral)
    tosse = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  tosse = toupper(tosse)
  
  # perda de olfato e paladar
  olfpal = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo teve perda, parcial ou total, do paladar ou do olfato? ')
  if (olfpal != 'D' & olfpal != 'd'){
    priori = get_priori('olfpal',olfpal,prioris_geral)
    olfpal = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  olfpal = toupper(olfpal)
  
  # dificuldade para respirar
  resp = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo teve dificuldade para respirar? ')
  if (resp != 'D' & resp != 'd'){
    priori = get_priori('resp',resp,prioris_geral)
    resp = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  resp = toupper(resp)
  
  # contato com outros indiv?duos doentes
  contato = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo viveu, visitou ou cuidou de algu?m com sintomas ou diagn?stico de  COVID 19? ')
  if (contato != 'D' & contato != 'd'){
    priori = get_priori('contato',contato,prioris_geral)
    contato = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  contato = toupper(contato)
  
  # viagem para ?rea de risco
  area = readline(prompt = 'Durante as ?ltimas duas semanas, o indiv?duo viajou ? uma ?rea em que o COVID 19 estava presente? ')
  if (area != 'D' & area != 'd'){
    priori = get_priori('area',area,prioris_geral)
    area = priori[3]
    probs = func_geral(prob_cov,prob_nao_cov,as.numeric(priori[1]),as.numeric(priori[2]))
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  area = toupper(area)
  
  time = format(Sys.time(),"%D %X")
  line_to_append = data.frame(id,time,version,idade,sexo,dist,acid,comprov,febre,fadiga,tosse,olfpal,resp,contato,area,round(prob_cov*100,0),round(prob_nao_cov*100,0))
  write.table(line_to_append,'registers_crms.csv',sep = ',',
              append=TRUE,col.names = FALSE, row.names = FALSE)
  
  print(paste('Caso: ',id))
  print(paste('Probabilidade de morte por COVID-19: ', round(prob_cov*100,0),'%'))
  print(paste('Probabilidade de morte por outras causas: ', round(prob_nao_cov*100,0),'%'))
  
}

get_priori = function(pergunta,resposta,prioris_geral){
  
  
  
  if (resposta == 'Y' | resposta == 'y'){
    return(c(as.numeric(prioris_geral[prioris_geral$pergunta == pergunta,c('priori_covid_Y','priori_nao_covid_Y')]),resposta))
  }
  else if (resposta == 'N' | resposta == 'n'){
    return(c(as.numeric(prioris_geral[prioris_geral$pergunta == pergunta,c('priori_covid_N','priori_nao_covid_N')]),resposta))
  }
  else if (resposta == 'D' | resposta == 'd'){
    return(c(0.5,0.5,resposta))
  }
  else {
    resposta = readline(prompt = 'Por favor, insira uma respota v?lida (Y/N/D): ')
    return(get_priori(pergunta,resposta,prioris_geral))
  }
}

func_idade = function(prob_cov,prob_nao_cov,idade){
  covid = 0.1 + (0.008*idade)
  nao_covid = 1 - covid
  prob_cov = (covid*prob_cov)/(covid*prob_cov + nao_covid*prob_nao_cov)
  prob_nao_cov = (nao_covid*prob_nao_cov)/(covid*prob_cov + nao_covid*prob_nao_cov)
  prob_cov = prob_cov/(prob_cov + prob_nao_cov)
  prob_nao_cov =  prob_nao_cov/(prob_cov + prob_nao_cov)   ##1 - prob_cov
  return(c(prob_cov,prob_nao_cov))
  
}

func_sexo = function(prob_cov,prob_nao_cov,sexo,prioris_sexo){
  if (sexo == 'M' | sexo == 'm'){
    priori_cov = prioris_sexo[prioris_sexo$pergunta == 'sexo','priori_covid_M']
    priori_nao_cov = prioris_sexo[prioris_sexo$pergunta == 'sexo','priori_other_M']
  } 
  else if (sexo == 'F' | sexo == 'f'){
    priori_cov = prioris_sexo[prioris_sexo$pergunta == 'sexo','priori_covid_F']
    priori_nao_cov = prioris_sexo[prioris_sexo$pergunta == 'sexo','priori_other_F']
  }
  else  if (sexo == 'D' | sexo == 'd'){
    priori_cov = prioris_sexo[prioris_sexo$pergunta == 'sexo','priori_covid_D']
    priori_nao_cov = prioris_sexo[prioris_sexo$pergunta == 'sexo','priori_other_D']
  }
  else{
    sexo = readline(prompt = 'Por favor, insira uma respota v?lida (M/F/D): ')
    return(func_sexo(prob_cov,prob_nao_cov,sexo,prioris_sexo))
  }
  return(c(func_geral(prob_cov,prob_nao_cov,priori_cov,priori_nao_cov),sexo))
}


func_geral = function(prob_cov, prob_nao_cov, priori_cov, priori_nao_cov){
  prob_cov = (priori_cov*prob_cov)/(priori_cov*prob_cov + priori_nao_cov*prob_nao_cov)
  prob_nao_cov = (priori_nao_cov*prob_nao_cov)/(priori_cov*prob_cov + priori_nao_cov*prob_nao_cov)
  prob_cov = prob_cov/(prob_cov + prob_nao_cov)
  prob_nao_cov =  prob_nao_cov/(prob_cov + prob_nao_cov)   ##1 - prob_cov
  return(c(prob_cov,prob_nao_cov))
  
}
