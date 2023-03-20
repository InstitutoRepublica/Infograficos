library(tidyverse)
library(survey)
library(PNADcIBGE)
library(splitstackshape)


load("arquivos_pnad.RData")

## Notação científica
options(scipen = 999)

### [mulheres na população] [pnadc] | [mulheres setor público][pnadc]

## sexo, tipo de cargo, setor, remuneração bruta, raça

vars = c("V2007", "V2010", "V4028", "V4014", "V4012","V4010","VD4016", "VD4010")

#"V2007" = sexo
#"V2010" = cor/raça
#"V4010" = Codigo da ocupação
#"V4012" = seto
#"V4014" = Nível 
#"V4028" = Nesse trabalho, ... era servidor público estatutário (federal, estadual ou municipal) ?
# VD4016 = remuneracao

################## Otimizado ###################

bases = c("dadosPNADc_22",
"dadosPNADc_21",
"dadosPNADc_20",
"dadosPNADc_19",
"dadosPNADc_18",
"dadosPNADc_17")




anos = c(2017:2021)

df_servidor = data.frame() 

for (i in anos) {
   
dadosPNADc <- get_pnadc(year=i, quarter=3, vars=vars)

dadosPNADc$variables <- transform(dadosPNADc$variables, flag_lideranca=ifelse(V4010 >= '1111' & V4010 <= '1439',1,0))
#servidor = svytotal(x=~interaction(V4014,V4012, V2007, V2010, flag_lideranca), design=subset(dadosPNADc,V4012=="Empregado do setor público (inclusive empresas de economia mista)"|V4012== "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" ), na.rm=TRUE)
#rm(dadosPNADc)
#gc()
#servidor = data.frame(servidor)

#servidor$ano = i

#df_servidor = rbind(df_servidor, servidor)


 }
 

df_servidor = rbind(df_servidor, servidor)


for (i in anos) {

   a=i

b = a-2000 }
 
 a=2022
 dadosPNADc_a-2000 = "x"
 
 #######################

 

dadosPNADc_22 <- get_pnadc(year=2022, quarter=3, vars=vars)
dadosPNADc_21 <- get_pnadc(year=2021, quarter=3, vars=vars)
dadosPNADc_20 <- get_pnadc(year=2020, quarter=3, vars=vars)
dadosPNADc_19 <- get_pnadc(year=2019, quarter=3, vars=vars)
dadosPNADc_18 <- get_pnadc(year=2018, quarter=3, vars=vars)
dadosPNADc_17 <- get_pnadc(year=2017, quarter=3, vars=vars)

save(dadosPNADc_22,
     dadosPNADc_21,
     dadosPNADc_20,
     dadosPNADc_19,
     dadosPNADc_18,
     dadosPNADc_17,df_servidor_22_1_civil_salario, file = "arquivos_pnad.RData")

### Tratando a base para apenas 1 ano (2022)

## transformando a variável antes de cruzar

dadosPNADc_22$variables <- transform(dadosPNADc_22$variables, flag_lideranca=ifelse(V4010 >= '1111' & V4010 <= '1439',1,0))



# Faz os cruzamentos e os filtros. 
servidor_22 = svytotal(x=~interaction(V4014,V4012, V2007, V2010, flag_lideranca), design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)"|V4012== "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" ), na.rm=TRUE)



# Transformar em dataframe os vinculos


df_servidor_22= data.frame(servidor_22)
row_nomes = data.frame(nome=rownames(df_servidor_22)) #Os parâmetros se transformam em nomes de linhas, essa função pega esses nomes
separa_nome=cSplit(cSplit(row_nomes,"nome", sep=".", type.convert=FALSE),"nome_1", sep=")", type.convert=FALSE)
labels = separa_nome %>%select("Esfera"=nome_1_2,"Tipologia_trabalho"=nome_2,"Sexo"=nome_3,"Cor"=nome_4, "flag_lideranca"=nome_5)

df_servidor_22_1 = cbind(df_servidor_22,labels)
df_servidor_22_1 = df_servidor_22_1 %>% select(Esfera, Tipologia_trabalho, Sexo, Cor,flag_lideranca,total, SE) 

### Chart 1

## mulheres no total

pop_genero =svytotal(x=~ V2007, design=dadosPNADc_22, na.rm=TRUE)
genero= data.frame(pop_genero)
homem = genero[[1,1]]
mulher = genero[[2,1]]

mulher/sum(genero$total)

# Mulheres no serviço civil
df_servidor_22_1_civil = df_servidor_22_1 %>% filter(Tipologia_trabalho=="Empregado do setor público (inclusive empresas de economia mista)" )

homens = df_servidor_22_1_civil %>%
  filter(Sexo=="Homem") %>%
  pull(total) %>%
  sum

mulheres = df_servidor_22_1_civil %>%
  filter(Sexo=="Mulher") %>%
  pull(total) %>%
  sum

mulheres_negras = df_servidor_22_1_civil %>%
  filter(Sexo=="Mulher" & (Cor=="Parda" | Cor=="Preta")) %>%
  pull(total) %>%
  sum

mulheres/sum(df_servidor_22_1_civil$total)
homens/sum(df_servidor_22_1_civil$total)
mulheres_negras/sum(df_servidor_22_1_civil$total)

# Mulheres no serviço civil e militar
df_servidor_22_1_todos = df_servidor_22_1 %>% filter(Tipologia_trabalho=="Empregado do setor público (inclusive empresas de economia mista)" | Tipologia_trabalho== "Militar do exército, da marinha, da aeronáutica, da polícia militar ou do corpo de bombeiros militar" )

homens = df_servidor_22_1_todos %>%
  filter(Sexo=="Homem") %>%
  pull(total) %>%
  sum

mulheres = df_servidor_22_1_todos %>%
  filter(Sexo=="Mulher") %>%
  pull(total) %>%
  sum

mulheres_negras = df_servidor_22_1_todos %>%
  filter(Sexo=="Mulher" & (Cor=="Parda" | Cor=="Preta")) %>%
  pull(total) %>%
  sum

mulheres/sum(df_servidor_22_1_civil$todos)
homens/sum(df_servidor_22_1_civil$todos)
mulheres_negras/sum(df_servidor_22_1_civil$todos)


## mulheres líderes no setor público civil

pop_genero_lid =svytotal(x=~ V2007, design=subset(dadosPNADc_22,flag_lideranca==1) , na.rm=TRUE)
genero_lid= data.frame(pop_genero_lid)
homem = genero_lid[[1,1]]
mulher = genero_lid[[2,1]]

mulher/sum(genero_lid$total)

### final para 2022

## Limpar para organizar espaço
#rm(dadosPNADc_22)
gc()


# Transformar em dataframe os salários




analise = df_servidor_22_1 %>% group_by(Cor, flag_lideranca,Sexo,Esfera) %>% summarise(sum(total))



#flag_lideranca = ifelse(V4010 >= '1111' & V4010 <= '1439',1,0)



## Link importante
## https://rpubs.com/gabriel-assuncao-ibge/pnadc


##### Remunerações - Civil



# mulheres do setor público

mediarenda_m <- svymean(x=~VD4016, design=subset(dadosPNADc_17,(V4012=="Empregado do setor público (inclusive empresas de economia mista)") & V2007== "Mulher" & VD4016>937 & VD4016<400000), na.rm=TRUE)

# homens do setor público

mediarenda_h <- svymean(x=~VD4016, design=subset(dadosPNADc_17,(V4012=="Empregado do setor público (inclusive empresas de economia mista)") & V2007== "Homem" & VD4016>937 & VD4016<400000), na.rm=TRUE)


mediarenda_m[[1]]/mediarenda_h[[1]]

(mediarenda_h-mediarenda_m)/mediarenda_m



## homem branco e mulher negra no setor público


mediarenda_h <- svymean(x=~VD4016, design=subset(dadosPNADc_17,(V4012=="Empregado do setor público (inclusive empresas de economia mista)") & V2007== "Homem" & VD4016>937 & VD4016<400000 & V2010=="Branca"), na.rm=TRUE)
mediarenda_m <- svymean(x=~VD4016, design=subset(dadosPNADc_17,(V4012=="Empregado do setor público (inclusive empresas de economia mista)") & V2007== "Mulher" & VD4016>937 & VD4016<400000 & (V2010=="Preta" |V2010=="Parda" )), na.rm=TRUE)

(mediarenda_h-mediarenda_m)/mediarenda_m




mediarenda_publico_civil <- svymean(x=~VD4016, design=subset(dadosPNADc_17,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>937 & VD4016<400000), na.rm=TRUE)
mediarenda_privado <- svymean(x=~VD4016, design=subset(dadosPNADc_17,V4012=="Empregado do setor privado" & VD4016>937 & VD4016<400000), na.rm=TRUE)

## Espaço de testes para análises

mediarenda_privado_h<- svyquantile(x=~VD4016,quantiles=0.5, design=subset(dadosPNADc_17,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>937 & VD4016<400000 & V2007== "Homem" & V4014=="Municipal" ), na.rm=TRUE)

mediarenda_privado_m<- svyquantile(x=~VD4016, quantiles=0.5,design=subset(dadosPNADc_17,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>937 & VD4016<400000 & V2007== "Mulher" & V4014=="Municipal"), na.rm=TRUE)

mediarenda_privado_m[[1]]/mediarenda_privado_h[[1]]


## Criar dataframe com remunerações médias e medianas dos seguintes cruzamentos:
## Observação: Precisa deflacionar
## Ver análise combinatória


### Criando dataframe com a renda 
media_salarial = c()

#Criei ID para me organizar
df_servidor_22_1_civil$ID=1:72

##Filtrei quem era total diferente de 0

df_servidor_22_1_civil=df_servidor_22_1_civil %>% filter(Cor!="Ignorado")

# For para criar vetor com salário
for (i in 1:60) {
  
mediarenda_publico_civil <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>937 & VD4016<400000 & V4014==df_servidor_22_1_civil[[i,1]]  &  V2007==df_servidor_22_1_civil[[i,3]] & V2010==df_servidor_22_1_civil[[i,4]] & flag_lideranca== df_servidor_22_1_civil[[i,5]] ), na.rm=TRUE)
  
media_salarial = c(media_salarial,mediarenda_publico_civil)
  
}


df_servidor_22_1_civil_salario = cbind(df_servidor_22_1_civil,media_salarial)

write.csv2(df_servidor_22_1_civil_salario, "base_2022.csv")


#------------------------

## Total de vinculos gerais

df_chart = df_servidor_22_1_civil %>% mutate(cor_2 = ifelse(Cor=="Parda"|Cor=="Preta", "Negra",ifelse(Cor=="Branca","Branca","Outra"))) %>% group_by(Esfera,Sexo,"Cor"=cor_2) %>% summarise(total=sum(total))

df_chart%>%
  filter(Esfera=="Federal") %>%
  pull(total) %>%
  sum
  
esferas = data.frame(Esfera=c("Municipal", "Estadual","Federal"),esfera_n=c(6957475,2946210,1402296))

df_chart =df_chart %>% left_join(esferas,by="Esfera") 
df_chart$prop = df_chart$total/df_chart$esfera_n

Homem = df_chart %>% filter(Sexo=="Homem") %>% select(Esfera, Cor, "Homem"=prop) %>% mutate(id=paste0(Cor,Esfera))


Mulher = df_chart %>% filter(Sexo=="Mulher") %>% select(Esfera, Cor, "Mulher"=prop) %>% mutate(id=paste0(Cor,Esfera)) %>% 
  left_join(Homem,"id") %>% select(Esfera.x, Cor.x, Homem, Mulher)

write.csv2(Mulher, "vinculos.csv")

## Total de vinculos lideranças

# Total de vinculos gerais

df_chart = df_servidor_22_1_civil %>% mutate(cor_2 = ifelse(Cor=="Parda"|Cor=="Preta", "Negra",ifelse(Cor=="Branca","Branca","Outra"))) %>% filter(flag_lideranca==1)%>% group_by(Esfera,Sexo,"Cor"=cor_2) %>% summarise(total=sum(total))

df_chart%>%
  filter(Esfera=="Federal") %>%
  pull(total) %>%
  sum

esferas = data.frame(Esfera=c("Municipal", "Estadual","Federal"),esfera_n=c(df_chart%>%
                                                                              filter(Esfera=="Municipal") %>%
                                                                              pull(total) %>%
                                                                              sum,df_chart%>%
                                                                              filter(Esfera=="Estadual") %>%
                                                                              pull(total) %>%
                                                                              sum,df_chart%>%
                                                                              filter(Esfera=="Federal") %>%
                                                                              pull(total) %>%
                                                                              sum))

df_chart =df_chart %>% left_join(esferas,by="Esfera") 
df_chart$prop = df_chart$total/df_chart$esfera_n

Homem = df_chart %>% filter(Sexo=="Homem") %>% select(Esfera, Cor, "Homem"=prop) %>% mutate(id=paste0(Cor,Esfera))


Mulher = df_chart %>% filter(Sexo=="Mulher") %>% select(Esfera, Cor, "Mulher"=prop) %>% mutate(id=paste0(Cor,Esfera)) %>% 
  left_join(Homem,"id") %>% select(Esfera.x, Cor.x, Homem, Mulher)

write.csv2(Mulher, "vinculos.csv")


# Razão de salários.Geral

mediarenda_municipal_Homem <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V4014=="Municipal"), na.rm=TRUE)
mediarenda_municipal_Mulher <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & V4014=="Municipal"), na.rm=TRUE)
mediarenda_municipal_Homem_branco <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V2010=="Branca" &V4014=="Municipal"), na.rm=TRUE)
mediarenda_municipal_Mulher_negra <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & (V2010=="Preta" | V2010=="Parda") & V4014=="Municipal"), na.rm=TRUE)
mediarenda_estadual_Homem <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V4014=="Estadual"), na.rm=TRUE)
mediarenda_estadual_Mulher <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & V4014=="Estadual"), na.rm=TRUE)
mediarenda_estadual_Homem_branco <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V2010=="Branca" &V4014=="Estadual"), na.rm=TRUE)
mediarenda_estadual_Mulher_negra <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & (V2010=="Preta" | V2010=="Parda") & V4014=="Estadual"), na.rm=TRUE)
mediarenda_federal_Homem <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V4014=="Federal"), na.rm=TRUE)
mediarenda_federal_Mulher <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & V4014=="Federal"), na.rm=TRUE)
mediarenda_federal_Homem_branco <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V2010=="Branca" &V4014=="Federal"), na.rm=TRUE)
mediarenda_federal_Mulher_negra <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & (V2010=="Preta" | V2010=="Parda") & V4014=="Federal"), na.rm=TRUE)





razao_municipal = mediarenda_municipal_Mulher[[1]]/mediarenda_municipal_Homem[[1]]
razao_estadual = mediarenda_estadual_Mulher[[1]]/mediarenda_estadual_Homem[[1]]
razao_federal = mediarenda_federal_Mulher[[1]]/mediarenda_federal_Homem[[1]]
razao_municipal_raca = mediarenda_municipal_Mulher_negra[[1]]/mediarenda_municipal_Homem_branco[[1]]
razao_estadual_raca = mediarenda_estadual_Mulher_negra[[1]]/mediarenda_estadual_Homem_branco[[1]]
razao_federal_raca = mediarenda_federal_Mulher_negra[[1]]/mediarenda_federal_Homem_branco[[1]]




razao_municipal
razao_estadual
razao_federal

razao_municipal_raca
razao_estadual_raca
razao_federal_raca



# Razão de salários.Liderança

mediarenda_municipal_Homem <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V4014=="Municipal" & flag_lideranca==1), na.rm=TRUE)
mediarenda_municipal_Mulher <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & V4014=="Municipal" & flag_lideranca==1), na.rm=TRUE)
mediarenda_municipal_Homem_branco <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V2010=="Branca" &V4014=="Municipal" & flag_lideranca==1), na.rm=TRUE)
mediarenda_municipal_Mulher_negra <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & (V2010=="Preta" | V2010=="Parda" & flag_lideranca==1) & V4014=="Municipal"), na.rm=TRUE)
mediarenda_estadual_Homem <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V4014=="Estadual" & flag_lideranca==1), na.rm=TRUE)
mediarenda_estadual_Mulher <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & V4014=="Estadual" & flag_lideranca==1), na.rm=TRUE)
mediarenda_estadual_Homem_branco <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V2010=="Branca" &V4014=="Estadual" & flag_lideranca==1), na.rm=TRUE)
mediarenda_estadual_Mulher_negra <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & (V2010=="Preta" | V2010=="Parda" & flag_lideranca==1) & V4014=="Estadual"), na.rm=TRUE)
mediarenda_federal_Homem <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V4014=="Federal" & flag_lideranca==1), na.rm=TRUE)
mediarenda_federal_Mulher <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & V4014=="Federal" & flag_lideranca==1), na.rm=TRUE)
mediarenda_federal_Homem_branco <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Homem" & V2010=="Branca" &V4014=="Federal" & flag_lideranca==1), na.rm=TRUE)
mediarenda_federal_Mulher_negra <- svymean(x=~VD4016, design=subset(dadosPNADc_22,V4012=="Empregado do setor público (inclusive empresas de economia mista)" & VD4016>1212 & VD4016<400000 & V2007=="Mulher" & (V2010=="Preta" | V2010=="Parda" & flag_lideranca==1) & V4014=="Federal"), na.rm=TRUE)





razao_municipal = mediarenda_municipal_Mulher[[1]]/mediarenda_municipal_Homem[[1]]
razao_estadual = mediarenda_estadual_Mulher[[1]]/mediarenda_estadual_Homem[[1]]
razao_federal = mediarenda_federal_Mulher[[1]]/mediarenda_federal_Homem[[1]]
razao_municipal_raca = mediarenda_municipal_Mulher_negra[[1]]/mediarenda_municipal_Homem_branco[[1]]
razao_estadual_raca = mediarenda_estadual_Mulher_negra[[1]]/mediarenda_estadual_Homem_branco[[1]]
razao_federal_raca = mediarenda_federal_Mulher_negra[[1]]/mediarenda_federal_Homem_branco[[1]]




razao_municipal
razao_estadual
razao_federal

razao_municipal_raca
razao_estadual_raca
razao_federal_raca

