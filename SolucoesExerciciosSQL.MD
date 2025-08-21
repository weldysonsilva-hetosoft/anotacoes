### 1 Exiba ID_PESSOA, NOME e uma coluna StatusAtivo transformando INATIVO (0/1) em ATIVO/INATIVO. Sugestão: IIF.
```sql

SELECT
	ID_Pessoa,
	Nome ,
	IIF(1 = 1, 'É ATIVO', 'É INATIVO') AS StatusAtivo
FROM PESSOAS	
```

### 2 Exiba ID_PESSOA, CPF e uma coluna TemCPF indicando 1 se CPF preenchido e 0 se vazio/nulo. Sugestão: IIF + NULLIF.
```sql

SELECT  
    ID_Pessoa,
    CPF,
    IIF(NULLIF(CPF, '') IS NULL, 0, 1) AS TemCPF
FROM  PESSOAS;
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

### 14) Mapeie TP_REGIME_TRIBUTACAO para nomes legíveis (ex.: Simples, Presumido, Real, Não informado). Sugestão: CASE.
``` sql
SELECT
	ID_PESSOA,
	TP_REGIME_TRIBUTACAO,
	CASE
		COALESCE(TP_REGIME_TRIBUTACAO, 0)
		WHEN 1 THEN 'Simples'
		WHEN 2 THEN 'Presumido'
		WHEN 3 THEN 'Real'
		ELSE 'Não informado'
	END AS RegimeTributacao
FROM PESSOAS;
```

### 15) Mapeie ESTADO_CIVIL para texto (Casado, Solteiro, Viúvo, Divorciado, Não informado). Sugestão: CASE.
``` sql
SELECT
	ID_PESSOA,
	ESTADO_CIVIL, 
CASE COALESCE(ESTADO_CIVIL, 0)
		WHEN 1 THEN 'CASADO'
		WHEN 2 THEN 'SOLTEIRO'
		WHEN 3 THEN 'VIÚVO'
		WHEN 4 THEN 'DIVORCIADO'
		ELSE 'NÃO INFORMADO'
	END AS EstadoCivil
FROM PESSOAS;
```

### 16) Defina CanalPreferido priorizando CELULAR, depois TELEFONE, depois EMAIL, senão Nenhum. Sugestão: CASE + NULLIF.
``` sql
SELECT 
ID_PESSOA,
CASE	
	WHEN NULLIF(LTRIM(RTRIM(CELULAR)), '')  IS NOT NULL THEN 'Celular'
	WHEN NULLIF(LTRIM(RTRIM(TELEFONE)), '')  IS NOT NULL THEN 'Telefone'
	WHEN NULLIF(LTRIM(RTRIM(EMAIL)), '')  IS NOT NULL THEN 'Email'	
	ELSE 'NENHUM'		
END AS CanalPreferido
FROM PESSOAS;
```
###  17) Monte DocumentoExibir indicando CPF: valor, CNPJ: valor ou Sem documento, conforme preenchimento. Sugestão: CASE + concatenação.
``` sql
SELECT 
ID_PESSOA,
CASE
	WHEN NULLIF(LTRIM(RTRIM(CPF)), '')  IS NOT NULL THEN CONCAT('CPF: ',  LTRIM(RTRIM(CPF)))
	WHEN NULLIF(LTRIM(RTRIM(CNPJ)), '')  IS NOT NULL THEN CONCAT('CNPJ: ',  LTRIM(RTRIM(CNPJ)))
ELSE 'SEM DOCUMENTO'	
END AS DocumentoExibir
FROM PESSOAS;
```

### 18) Classifique VL_RENDA em Baixa, Média, Alta ou Não informada (tratando nulos). Sugestão: CASE.
``` sql
SELECT
  ID_PESSOA,
  VL_RENDA,
  CASE
    WHEN VL_RENDA IS NULL                     THEN 'Não informada'
    WHEN VL_RENDA <= 2000                     THEN 'Baixa'
    WHEN VL_RENDA > 2000 AND VL_RENDA <= 8000 THEN 'Média'
    WHEN VL_RENDA > 8000                      THEN 'Alta'
    ELSE 'Não informada'
  END AS FaixaRenda
FROM PESSOAS;

```

### 19) Crie StatusCadastro priorizando REGISTRO_PADRAO_SISTEMA, depois INATIVO, senão Ativo. Sugestão: CASE.
``` sql
SELECT
  ID_PESSOA,
  CASE
    WHEN COALESCE(REGISTRO_PADRAO_SISTEMA,0) = 1 THEN 'Padrão do Sistema'
    WHEN COALESCE(INATIVO,0) = 1                 THEN 'Inativo'
    ELSE 'Ativo'
  END AS StatusCadastro
FROM PESSOAS;
```

### 20) Indique TipoTransportador como Transportador quando TPTRANSPORTADOR=1, senão Não transportador. Sugestão: CASE. 
``` sql
SELECT
  ID_PESSOA,
  CASE WHEN COALESCE(TPTRANSPORTADOR,0)=1 THEN 'Transportador'
       ELSE 'Não transportador' END AS TipoTransportador
FROM PESSOAS;
```

### 21) Monte TelefonePreferencial escolhendo primeiro CELULAR, depois TELEFONE, depois TELEFONE_TRABALHO, senão Sem telefone (ignorar vazios). Sugestão: COALESCE + NULLIF. 
``` sql
SELECT
	ID_PESSOA,
	COALESCE(
NULLIF(LTRIM(RTRIM(CELULAR)), ''), 	
NULLIF(LTRIM(RTRIM(TELEFONE)), ''), 	
NULLIF(LTRIM(RTRIM(TELEFONE_TRABALHO)), ''), 	
	 '(Sem telefone)'
) AS TelefonePreferencial
FROM PESSOAS;

```

###  22) Monte NomeExibicao priorizando NOME, depois DESCRICAO, senão (Sem nome) (ignorar vazios). Sugestão: COALESCE + NULLIF.
``` sql
SELECT
	ID_PESSOA,
	COALESCE(
NULLIF(LTRIM(RTRIM(NOME)), ''),
NULLIF(LTRIM(RTRIM(DESCRICAO)), ''),
'(SEM NOME)'
) AS NomeExibicao
FROM PESSOAS;
```

### 23) Normalize INSCRICAO_ESTADUAL para NULL quando for ISENTO ou vazia. Sugestão: COALESCE + NULLIF.
``` sql
SELECT
  ID_PESSOA,
  CASE
    WHEN NULLIF(UPPER(LTRIM(RTRIM(COALESCE(INSCRICAO_ESTADUAL, '')))), '') IN ('ISENTO', 'ISENTA') THEN NULL
    ELSE LTRIM(RTRIM(INSCRICAO_ESTADUAL))
  END AS INSCRICAO_ESTADUAL_NORMALIZADA
FROM PESSOAS;

```
### 24) Preencha EmailComDefault com EMAIL ou sem-email@example.com quando não houver email válido. Sugestão: COALESCE + NULLIF.
``` sql
SELECT
  ID_PESSOA,
  COALESCE(NULLIF(LTRIM(RTRIM(EMAIL)), ''), 'sem-email@example.com') 
  AS EmailComDefault
FROM PESSOAS;

```

### 25) Monte ContatoNomePreferido priorizando TELEFONECONTATO, depois CELULARCONTATO, depois EMAIL, senão Sem contato. Sugestão: COALESCE + NULLIF.
``` sql
SELECT
  ID_PESSOA,
  COALESCE(
    NULLIF(LTRIM(RTRIM(TELEFONECONTATO)), ''),
    NULLIF(LTRIM(RTRIM(CELULARCONTATO)), ''),
    NULLIF(LTRIM(RTRIM(EMAIL)), ''),
    'Sem contato'
  ) AS ContatoNomePreferido
FROM PESSOAS;
```

### 26)  Preencha UFNascimentoPadrao com UF_NASCIMENTO ou NA quando nulo. Sugestão: COALESCE.
``` sql
SELECT ID_PESSOA,
       COALESCE(UF_NASCIMENTO, 'NA') AS UFNascimentoPadrao
FROM PESSOAS;
```
