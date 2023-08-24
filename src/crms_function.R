crms = function(id,idade,sexo,dist,acid,comprov,febre,fadiga,tosse,olfpal,resp,contato,area){
  
  #resgatando prioris
  prioris_geral = read.csv('src/data/prioris/prioris_geral.csv')
  prioris_sexo = read.csv('src/data/prioris/prioris_sex.csv')

  #inicializando as vari?veis
  prob_cov = 0.5
  prob_nao_cov = 0.5
  
  #idade
  prob_idade = func_idade(prob_cov,prob_nao_cov,idade)
  prob_cov = prob_idade[1]
  prob_nao_cov = prob_idade[2]
  
  #sexo
  prob_sexo = func_sexo(prob_cov,prob_nao_cov,sexo,prioris_sexo)
  prob_cov = prob_sexo[1]
  prob_nao_cov = prob_sexo[2]
  
  #distanciamento social
  if (dist != 'D' & dist != 'd'){
    priori = get_priori('dist',dist,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  #acidente
  if (acid != 'D' & acid != 'd'){
    priori = get_priori('acid',acid,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  #diagnostico
  if (comprov != 'D' & comprov != 'd'){
    priori = get_priori('comprov',comprov,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  #febre
  if (febre != 'D' & febre != 'd'){
    priori = get_priori('febre',febre,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  #fadiga extrema
  if (fadiga != 'D' & fadiga != 'd'){
    priori = get_priori('fadiga',fadiga,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  #tosse
  if (tosse != 'D' & tosse != 'd'){
    priori = get_priori('tosse',tosse,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  # perda de olfato e paladar
  if (olfpal != 'D' & olfpal != 'd'){
    priori = get_priori('olfpal',olfpal,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  # dificuldade para respirar
  if (resp != 'D' & resp != 'd'){
    priori = get_priori('resp',resp,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  # contato com outros indiv?duos doentes
  if (contato != 'D' & contato != 'd'){
    priori = get_priori('contato',contato,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  # viagem para ?rea de risco
  if (area != 'D' & area != 'd'){
    priori = get_priori('area',area,prioris_geral)
    probs = func_geral(prob_cov,prob_nao_cov,priori[1],priori[2])
    prob_cov = probs[1]
    prob_nao_cov = probs[2]
  }
  
  return(c(round(prob_cov*100,0),round(100*prob_nao_cov,0)))

}

get_priori = function(pergunta,resposta,prioris_geral){
  if (resposta == 'Y' | resposta == 'y'){
    return(as.numeric(prioris_geral[prioris_geral$pergunta == pergunta,c('priori_covid_Y','priori_nao_covid_Y')]))
  }
  else if (resposta == 'N' | resposta == 'n'){
    return(as.numeric(prioris_geral[prioris_geral$pergunta == pergunta,c('priori_covid_N','priori_nao_covid_N')]))
  }
  else if (resposta == 'D' | resposta == 'd'){
    return(c(0.5,0.5))
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
  return(func_geral(prob_cov,prob_nao_cov,priori_cov,priori_nao_cov))
}

func_geral = function(prob_cov, prob_nao_cov, priori_cov, priori_nao_cov){
  prob_cov = (priori_cov*prob_cov)/(priori_cov*prob_cov + priori_nao_cov*prob_nao_cov)
  prob_nao_cov = (priori_nao_cov*prob_nao_cov)/(priori_cov*prob_cov + priori_nao_cov*prob_nao_cov)
  prob_cov = prob_cov/(prob_cov + prob_nao_cov)
  prob_nao_cov = prob_nao_cov/(prob_cov + prob_nao_cov) ##1 - prob_cov
  return(c(prob_cov,prob_nao_cov))
  
}