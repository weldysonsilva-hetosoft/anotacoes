### 1 Exiba ID_PESSOA, NOME e uma coluna StatusAtivo transformando INATIVO (0/1) em ATIVO/INATIVO. Sugestão: IIF.
```sql

SELECT
	ID_Pessoa,
	Nome ,
	IIF(1 = 1, 'É ATIVO', 'É INATIVO') AS StatusAtivo
FROM
	PESSOAS	
```

### 2 Exiba ID_PESSOA, CPF e uma coluna TemCPF indicando 1 se CPF preenchido e 0 se vazio/nulo. Sugestão: IIF + NULLIF.
```sql

SELECT  
    ID_Pessoa,
    CPF,
    IIF(NULLIF(CPF, '') IS NULL, 0, 1) AS TemCPF
FROM
    PESSOAS;
```
### 3 Exiba ID_PESSOA, CPF, CNPJ e uma coluna TipoPessoa indicando Física se CPF preenchido, senão Jurídica se CNPJ preenchido, senão Desconhecida. Sugestão: IIF ou CASE + COALESCE + NULLIF.
	
``` sql
SELECT  
ID_PESSOA,
  CPF, CNPJ,
  CASE
    WHEN NULLIF(LTRIM(RTRIM(CPF)), '') IS NOT NULL  THEN 'Física'
    WHEN NULLIF(LTRIM(RTRIM(CNPJ)), '') IS NOT NULL THEN 'Jurídica'
    ELSE 'Desconhecida'
  END AS TipoPessoa

FROM  PESSOAS;
```

### 4  Exiba ID_PESSOA e uma coluna Localidade indicando Exterior quando EXTERIOR=1, senão Brasil. Sugestão: IIF.
``` sql
SELECT
  ID_PESSOA,
  IIF(COALESCE(EXTERIOR, 0) = 1, 'Exterior', 'Brasil') AS Localidade
FROM PESSOAS;
```

### 5 Exiba ID_PESSOA, VL_LIMITE_CREDITO e TemLimite indicando 1 se o limite for maior que zero (tratando nulos como 0), senão 0. Sugestão: IIF + COALESCE.
``` sql
SELECT
  ID_PESSOA,
  VL_LIMITE_CREDITO,
  IIF(COALESCE(VL_LIMITE_CREDITO, 0) > 0, 1, 0) AS TemLimite
FROM PESSOAS;
```


### 6  Exiba ID_PESSOA, SEXO e SexoTexto mapeando 1=Masculino, 2=Feminino, outros=Não informado. Sugestão: IIF aninhado ou CASE.
``` sql
SELECT
  ID_PESSOA,
  SEXO,
  CASE COALESCE(SEXO, 0)
    WHEN 1 THEN 'Masculino'
    WHEN 2 THEN 'Feminino'
    ELSE 'Não informado'
  END AS SexoTexto
FROM PESSOAS;
```


### 7 Exiba ID_PESSOA, EMAIL e TemEmail indicando S se email preenchido (ignorando espaços), senão N. Sugestão: IIF + NULLIF.
``` sql
SELECT
  ID_PESSOA,
  EMAIL,
  IIF(NULLIF(LTRIM(RTRIM(EMAIL)), '') IS NULL, 'N', 'S') AS TemEmail
FROM PESSOAS;

```
### 8 Exiba ID_PESSOA e PapelComercial priorizando: Cliente se TPCLIENTE=1; senão Fornecedor se TPFORNECEDOR=1; senão Outro. Sugestão: IIF ou CASE.
``` sql
SELECT
  ID_PESSOA,
  CASE
    WHEN COALESCE(TPCLIENTE,0) = 1 THEN 'Cliente'
    WHEN COALESCE(TPFORNECEDOR,0) = 1 THEN 'Fornecedor'
    ELSE 'Outro'
  END AS PapelComercial
FROM PESSOAS;

```
### 9 Exiba ID_PESSOA e BloqueioGeral marcando Bloqueado se BLOQUEADO=1 ou BLOQUEADO_MOV=1 (tratando nulos), senão Liberado. Sugestão: IIF + COALESCE.
``` sql
SELECT
  ID_PESSOA,
  IIF(COALESCE(BLOQUEADO,0)=1 OR COALESCE(BLOQUEADO_MOV,0)=1, 'Bloqueado', 'Liberado') AS BloqueioGeral
FROM PESSOAS;
```

### 10 Exiba ID_PESSOA, SITE e TemSite indicando 1 se site preenchido (ignorando espaços), senão 0. Sugestão: IIF + NULLIF.
``` sql
SELECT
  ID_PESSOA,
  SITE,
  IIF(NULLIF(LTRIM(RTRIM(SITE)), '') IS NULL, 0, 1) AS TemSite
FROM PESSOAS;
```
### 11 Classifique o perfil do registro como Cliente, Fornecedor, Funcionário ou Outro com base em TPCLIENTE, TPFORNECEDOR e TPFUNCIONARIO. Sugestão: CASE.
``` sql
SELECT
  ID_PESSOA,
  CASE
    WHEN COALESCE(TPCLIENTE,0)=1       THEN 'Cliente'
    WHEN COALESCE(TPFORNECEDOR,0)=1    THEN 'Fornecedor'
    WHEN COALESCE(TPFUNCIONARIO,0)=1   THEN 'Funcionário'
    ELSE 'Outro'
  END AS Perfil
FROM PESSOAS;
```

### 12 Classifique VL_LIMITE_CREDITO em FaixaLimite (Alto, Médio, Baixo, Sem limite). Sugestão: CASE. 
``` sql
SELECT 
ID_PESSOA,
VL_LIMITE_CREDITO,
CASE
	WHEN COALESCE(VL_LIMITE_CREDITO,0) > 10000 THEN 'Alto'
	WHEN COALESCE(VL_LIMITE_CREDITO,0) >= 1000 THEN 'Médio'	
	WHEN COALESCE(VL_LIMITE_CREDITO,0) >= 0 THEN 'Baixo'
	ELSE 'Sem limite'	
END AS FaixaLimite
FROM PESSOAS;
```
### 13) Crie StatusCredito usando VL_DEBITO_EM_ABERTO e VL_LIMITE_DISPONIVEL (ex.: Crítico, Em débito, Ok, Indefinido). Sugestão: CASE.
``` sql
SELECT
  ID_PESSOA,
  VL_DEBITO_EM_ABERTO,
  VL_LIMITE_DISPONIVEL,
  CASE
    WHEN COALESCE(VL_DEBITO_EM_ABERTO,0) > 0 AND COALESCE(VL_LIMITE_DISPONIVEL,0) <= 0 THEN 'Crítico'
    WHEN COALESCE(VL_DEBITO_EM_ABERTO,0) > 0 AND COALESCE(VL_LIMITE_DISPONIVEL,0) >  0 THEN 'Em débito'
    WHEN COALESCE(VL_DEBITO_EM_ABERTO,0) = 0 AND COALESCE(VL_LIMITE_DISPONIVEL,0) >= 0 THEN 'Ok'
    ELSE 'Indefinido'
  END AS StatusCredito
FROM PESSOAS;
```


