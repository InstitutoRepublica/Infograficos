
Esse Repositório tem como objetivo reunir os códigos utilizados para elaboração de estudos e dados rápidos da República.org.  

<details>
<summary> 07 - Dia Internacional das Mulheres</summary>
</details>


<details>
<summary>08 - Dia dos Povos Originários</summary>

#### 08 - Dia dos Povos Originários

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

A tabela 4 foi tratada no google sheets a partir do output feito com a query:

``` sql

SELECT  sigla_uf,COUNT ( DISTINCT id_docente) as quantidade_docentes
 
 FROM `basedosdados-projetos.republica.total_professores_contratacao`  where rede!="privada"
 and ano=2020 and raca_cor='Indígena'
 group by sigla_uf order by sigla_uf


```

`Tabela 4: Distribuição de docentes indígenas por UF`

As tabelas 5 e 6 foram tratadas no google sheets a partir do output feito com a query:


``` sql
SELECT  ano,COUNT ( DISTINCT id_docente) as quantidade_docentes,raca_cor, tipo_contratacao
 
 FROM `basedosdados-projetos.republica.total_professores_contratacao`  where rede!="privada"

 group by ano, raca_cor, tipo_contratacao order by ano

```
`Tabela 5: Proporção de docentes indígenas por tipo de contratação`

`Tabela 6: Proporção de docentes não-indígenas por tipo de contratação`


</details>
