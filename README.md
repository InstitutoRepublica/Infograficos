
## 07 - Dia Internacional das Mulheres

## 08 - Dia dos Povos Originários

Esse capítulo reúne dados trabalhados pela República.org sobre docentes autodeclarados indígenas pelo censo escolar. 

##### As tabelas 1, 2 e 3 foram tratadas no google sheets a partir do output feito com a query:

``` sql
SELECT  ano,COUNT ( DISTINCT id_docente), raca_cor as quantidade_docentes, sexo
 
 FROM `basedosdados-projetos.republica.total_professores_contratacao`  where rede!="privada"
 
 group by ano,raca_cor,sexo
```

[`Tabela 1: Quantidade de docentes indígenas ao longo do tempo por sexo.`](https://docs.google.com/spreadsheets/d/1LZRGyhKp5HfNhxwuxg83lUsRF3c1uF4-iSgq0bN2LtA/edit#gid=1141395208)

[`Tabela 2: Quantidade de docentes não indígenas ao longo do tempo por sexo.`](https://docs.google.com/spreadsheets/d/1LZRGyhKp5HfNhxwuxg83lUsRF3c1uF4-iSgq0bN2LtA/edit#gid=1141395208)

[`Tabela 3: Proporção e quantidade total de docentes por raça/cor`](https://docs.google.com/spreadsheets/d/1LZRGyhKp5HfNhxwuxg83lUsRF3c1uF4-iSgq0bN2LtA/edit#gid=1141395208)

As tabelas 1, 2 e 3 foram tratadas no google sheets a partir do output feito com a query:

``` sql

```

`Tabela 4: Quantidade de docentes de outras raça/cor ao longo do tempo por tipo de contratação.`

`Tabela 5: Dstribuição de docentes indígenas por UF`

`Tabela 6: Dstribuição de docentes indígenas por UF`

`Tabela 7: Distribuição de escolas com educação indígena que faz uso de língua indígena por região e ao longo do tempo.`

`Tabela 8: Quantidade de docentes indígenas que trabalham em escolas com Educação indígena.`

`Tabela 9: Docentes que trabalham em escolas com Educação indígena por tipo de contratação e cor/raça`
