# FUNÇÕES QUE PODEM AJUDAR NA CONVERSÃO #


## SqlOrigem.Concat ##

  **Contexto:** 
No módulo SolNET_Conversao, ao configurar a conversão de dados usando o Builder fluente (ConversaoBuilder), é preciso garantir que comandos SQL 
sejam compatíveis com Firebird e SQL Server. Por isso, existe o helper SqlOrigem.Concat, que padroniza a forma de construir expressões de concatenação.


*ANTES*

 ```pascal
.AddCampo('Email', 'Email1 + '';'' + EmailNFE')
```
- Utiliza o operador + para concatenar campos SQL.
- Isso funciona apenas no SQL Server, pois no Firebird o operador de concatenação é ||.
- Problema: Usar 'Email1 + '';'' + EmailNFE' não é compatível com Firebird.

*DEPOIS*

```pascal
.AddCampo('Email', SqlOrigem.Concat(['Email1', 'EmailNFE'], ';'))
```

- Aqui usamos o helper **SqlOrigem.Concat**.
- Ele recebe um array de campos (`['Email1', 'EmailNFE']`) e o separador (`';'`).
- O método gera automaticamente o SQL correto para o banco de dados em uso:
  - SQL Server: `Email1 + ';' + EmailNFE`
  - Firebird: `Email1 || ';' || EmailNFE`
- Vantagem: O mesmo código funciona tanto em Firebird quanto em SQL Server, pois o helper adapta a sintaxe conforme o SGBD.



**Resumo Visual**

| SGBD | Sintaxe correta para concatenação | 
|----------|----------|
| SQL Server	  | campo1 + ';' + campo2| 
| Firebird  | 	`campo1  |

**Conclusão**
Sempre que precisar concatenar campos no SQL de conversão, use SqlOrigem.Concat, nunca operadores fixos (+ ou ||) diretamente no texto do campo.
Assim, seu código fica compatível, padronizado e mais seguro!

---
# 🟢 Padrão de Concatenção de Campos Complexos (Email1 + EmailNFE)

## Objetivo

Concatenar os campos `Email1` e `EmailNFE` utilizando `;` como separador apenas quando ambos existirem (não nulos e não vazios).  
Quando existir apenas um, deve retornar apenas esse valor (sem separador ou espaços extras).  
Quando ambos forem nulos ou vazios, o resultado deve ser vazio (`''`).

---

## SQL Padrão ISO (compatível com Firebird e SQL Server)

```sql
CASE 
    WHEN NULLIF(TRIM(Email1), '') IS NOT NULL AND NULLIF(TRIM(EmailNFE), '') IS NOT NULL
        THEN Email1 || ';' || EmailNFE     -- Firebird: usa ||
    WHEN NULLIF(TRIM(Email1), '') IS NOT NULL
        THEN Email1
    WHEN NULLIF(TRIM(EmailNFE), '') IS NOT NULL
        THEN EmailNFE
    ELSE ''
END AS Email
```

> **No SQL Server:** Troque `||` por `+`  
> **No Firebird:** Use `||` para concatenação

---

## Delphi/ConversaoBuilder

```pascal
.AddCampo('Email',
  'CASE ' +
    'WHEN NULLIF(TRIM(Email1), '''') IS NOT NULL AND NULLIF(TRIM(EmailNFE), '''') IS NOT NULL ' +
      'THEN Email1 || '';'' || EmailNFE ' +  // Use + para SQL Server
    'WHEN NULLIF(TRIM(Email1), '''') IS NOT NULL ' +
      'THEN Email1 ' +
    'WHEN NULLIF(TRIM(EmailNFE), '''') IS NOT NULL ' +
      'THEN EmailNFE ' +
    'ELSE '''' ' +
  'END'
)
```

### Para SQL Server (concatenação com `+`):

```pascal
.AddCampo('Email',
  'CASE ' +
    'WHEN NULLIF(TRIM(Email1), '''') IS NOT NULL AND NULLIF(TRIM(EmailNFE), '''') IS NOT NULL ' +
      'THEN Email1 + '';'' + EmailNFE ' +
    'WHEN NULLIF(TRIM(Email1), '''') IS NOT NULL ' +
      'THEN Email1 ' +
    'WHEN NULLIF(TRIM(EmailNFE), '''') IS NOT NULL ' +
      'THEN EmailNFE ' +
    'ELSE '''' ' +
  'END'
)
```

---

## Boas Práticas

- Use sempre `TRIM` para eliminar espaços antes de checar vazio.
- Utilize `NULLIF(..., '') IS NOT NULL` para garantir que campos vazios sejam tratados como nulos.
- Padronize a concatenação conforme SGBD (veja Documentacao Basica.md).
- Evite espaços extras no separador (`;`), exceto quando necessário para legibilidade.

---

## Aplicação

Use este padrão em qualquer conversão onde dois ou mais campos possam ser concatenados, mas só devem ser unidos se ambos existirem.  
Exemplo típico: e-mails, telefones, endereços complementares.

---

## Referência

- [Documentacao Basica.md](../Conversao/Documentacao Basica.md) – Guia prático de conversão
- SQL Server Docs: [CASE](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/case-transact-sql)
- Firebird Docs: [String concatenation](https://firebirdsql.org/refdocs/langrefupd25-concat.html)

---




# 📚 Padrão de Conversão: Transformação de Campo Varchar para Integer com Regra de Negócio

## Contexto

Durante a migração de dados de sistemas legados para o Sol.NET ERP, é comum que campos venham com tipos diferentes do modelo Sol.NET.  
Um caso recorrente ocorre no campo **TIPO_ITEM** da tabela de produtos, onde:

- **Origem (cliente):** campo `TipoItemSped` do tipo `varchar`
- **Destino (Sol.NET):** campo `TIPO_ITEM` do tipo `integer`
- **Regra de negócio:** valor `'99'` em `TipoItemSped` deve ser convertido para `11`, demais valores mantidos

## Solução Recomendada

A conversão deve ser feita utilizando o padrão **ISO SQL**, garantindo compatibilidade entre Firebird e SQL Server.  
A expressão deve ser encapsulada em um bloco CASE, seguido do CAST para integer.

### Exemplo de Implementação com ConversaoBuilder

```pascal
.AddCampo('TIPO_ITEM', 'CAST(CASE WHEN TipoItemSped = ''99'' THEN 11 ELSE TipoItemSped END AS INTEGER)')
```

#### Explicação

- **CASE WHEN**: Verifica se o valor de origem é `'99'`; se sim, retorna `11`; caso contrário, retorna o valor original.
- **CAST(... AS INTEGER)**: Converte o resultado para inteiro para garantir compatibilidade com o campo de destino.
- **Aspas simples**: Utilizadas para comparar valores varchar conforme padrão SQL.

### Exemplo Completo

```pascal
var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
  .SetTabelaConversao(TTabelaProduto.Create(Produto), 'PRODUTOS P')
  .AddPrimaryKey('P.CODIGO')
  .AddCampo('TIPO_ITEM', 'CAST(CASE WHEN P.TipoItemSped = ''99'' THEN 11 ELSE P.TipoItemSped END AS INTEGER)')
  // ... demais campos
  .Build;
```

## Checklist para Aplicação do Padrão

- [x] Certifique-se que o campo de origem é varchar e destino integer
- [x] Utilize CASE para aplicar regras de substituição de valores
- [x] Finalize a expressão com CAST para conversão de tipo
- [x] Teste a conversão em ambos os SGBDs (Firebird e SQL Server)
- [x] Documente a regra no código e na documentação técnica

## Observações

- Em casos onde os dados de origem possam conter valores inválidos para conversão, recomenda-se implementar uma validação via callback antes da conversão.
- Registre o padrão neste documento para referência futura em novos projetos de migração.

---
# 🟢 Recomendações para Conversão de Campos de Status – Padrão Clean Code Sol.NET_Conversao

## 1. Contexto

Ao migrar dados de sistemas legados, é comum encontrar campos de status representados por códigos, geralmente `'S'` (Sim) e `'N'` (Não), onde `'S'` indica que o registro está em um estado especial (excluído, cancelado, bloqueado etc).  
O campo convertido no Sol.NET normalmente é um inteiro, como `INATIVO`, onde `1` significa inativo e `0` significa ativo.

## 2. Exemplos de Expressão

### Expressão original (menos robusta)

```pascal
.AddCampo('INATIVO', 'IIF(StatusCampo = ''N'', 0, 1)')
```
- **Lógica:** Se `StatusCampo = 'N'`, então INATIVO = 0 (ativo); senão, INATIVO = 1 (inativo).
- **Problema:** Qualquer valor diferente de `'N'` (incluindo branco ou nulo) será considerado inativo.

### Expressão ajustada (recomendada)

```pascal
.AddCampo('INATIVO', 'IIF(StatusCampo = ''S'', 1, 0)')
```
- **Lógica:** Se `StatusCampo = 'S'`, INATIVO = 1 (inativo); senão, INATIVO = 0 (ativo).
- **Vantagem:** Apenas registros explicitamente marcados como especiais (excluídos/cancelados) serão inativos. Valores em branco/nulos serão considerados ativos.

### Expressão universal (padrão ISO SQL, recomendada para Firebird/SQL Server)

```pascal
.AddCampo('INATIVO', 'CASE WHEN StatusCampo = ''S'' THEN 1 ELSE 0 END')
```

## 3. Regras de Negócio

- `'S'` = Estado especial (excluído, cancelado, bloqueado, etc.)
- `'N'` = Estado normal
- Em Sol.NET, o campo convertido deve refletir:
  - Estado especial → inativo (1)
  - Estado normal ou indefinido → ativo (0)

## 4. Motivo da inversão

- Evita que registros com campo em branco/nulo sejam tratados como inativos por engano
- Garante que apenas registros explicitamente marcados como especiais sejam convertidos como inativos
- Segue o padrão Clean Code: “Prefira comparar sempre pelo valor que indica o estado especial (‘S’)”

## 5. Checklist para aplicação em outros campos

Sempre que encontrar campos de status com valores `'S'`/`'N'`, adote a lógica:

```pascal
.AddCampo('CAMPO_DESTINO', 'IIF(CAMPO_STATUS = ''S'', 1, 0)')
```
**Ou, preferencialmente para SQL universal:**
```pascal
.AddCampo('CAMPO_DESTINO', 'CASE WHEN CAMPO_STATUS = ''S'' THEN 1 ELSE 0 END')
```

## 6. Exemplos adicionais

### Campo Bloqueado

```pascal
.AddCampo('BLOQUEADO', 'CASE WHEN Bloqueado = ''S'' THEN 1 ELSE 0 END')
```

### Campo Cancelado

```pascal
.AddCampo('CANCELADO', 'IIF(Cancelado = ''S'', 1, 0)')
```

### Campo Suspenso

```pascal
.AddCampo('SUSPENSO', 'CASE WHEN Suspenso = ''S'' THEN 1 ELSE 0 END')
```

## 7. Observação técnica

- Para bancos que não suportam `IIF` (ex: SQL Server), use sempre o padrão `CASE WHEN`.
- Em Delphi, use o padrão de declaração inline para variáveis e siga a interface fluente do ConversaoBuilder.

## 8. Conclusão

Adotar a lógica invertida na comparação de campos de status garante maior segurança e integridade na conversão.  
Evita inconsistências e facilita futuras manutenções, seguindo o padrão Clean Code e as recomendações do Sol.NET_Conversao.

---

# 🟢 QUEBRA LINHA E CONCATENAÇÃO DE CAMPOS + CAST (FOI PEDIDO PELO ANALISTA SE PODERIA UNIR MAIS 2 CAMPOS (BITOLAMOLA E MEDIDAS JUNTO DO CARACTERISTICAS PARA VIR JUNTO DO OBS CAMPO DO SOLNET)

```sql
SELECT
	CodigoPeca,
	CONCAT_WS(char(13), '  Caracteristicas:  ' + Caracteristicas, '  BitolaMola:  ' + cast(BitolaMola as VARCHAR(10)), '  Medidas:  ' + MEDIDAS)
FROM
	tbProdutos
os espaços no codigo é para as informações não virem coladas no solnet 
```
---
# 🟢 Padrão de Extração do Primeiro Código em Agregações (`string_agg`)

## Objetivo

Garantir que, ao migrar dados legados onde múltiplos códigos de um produto ou referência estão agregados em uma string (separados por vírgula), **apenas o primeiro código** seja extraído e utilizado como chave/identificador no Sol.NET.  
Essa abordagem previne duplicidade e normaliza dados, especialmente em cenários onde o legado não distinguia referências únicas.

---

## Lógica e Funções Utilizadas

### 1. Agregação dos códigos (SQL Server)

```sql
string_agg(CodigoPeca, ',')
```
- **Função:** Concatena todos os valores do campo `CodigoPeca` em uma única string, separados por vírgula.
- **Exemplo:**  
  - Códigos: `A01, B02, C03`  
  - Resultado: `'A01,B02,C03'`

### 2. Localização da vírgula

```sql
CHARINDEX(',', string_agg(CodigoPeca, ','))
```
- **Função:** Retorna a posição da primeira vírgula na string agregada.
- **Exemplo:** `'A01,B02,C03'` → `4` (posição da primeira vírgula)

### 3. Cálculo da extensão do primeiro código

```sql
COALESCE(NULLIF(CHARINDEX(',', string_agg(CodigoPeca, ',')) - 1, -1), LEN(string_agg(CodigoPeca, ',')))
```
- **Função:**  
  - `CHARINDEX(...) - 1`: Pega a posição anterior à primeira vírgula.
  - `NULLIF(..., -1)`: Se não encontrar vírgula, retorna NULL.
  - `COALESCE(..., LEN(...))`: Se for NULL (apenas um código), retorna o tamanho total da string.
- **Propósito:** Garante que se houver apenas um código, ele seja extraído por completo.

### 4. Extração do primeiro código

```sql
SUBSTRING(string_agg(CodigoPeca, ','), 1, ...)
```
- **Função:** Extrai apenas o primeiro código da lista agregada.
- **Exemplo:**  
  - `'A01,B02,C03'` → `SUBSTRING(..., 1, 3)` → `'A01'`  
  - `'A99'` → `SUBSTRING(..., 1, 3)` → `'A99'`

### 5. Eliminação de duplicidade

```sql
DISTINCT ...
```
- **Função:** Remove linhas totalmente idênticas no resultado da query.

### 6. Constante de tipo

```sql
1 AS TP_CODIGO
```
- **Função:** Atribui o valor constante `1` à coluna `TP_CODIGO`, indicando o tipo do código conforme padrão Sol.NET.

---

## Exemplo Completo

```sql
SELECT DISTINCT
  SUBSTRING(
    string_agg(CodigoPeca, ','),
    1,
    COALESCE(NULLIF(CHARINDEX(',', string_agg(CodigoPeca, ',')) - 1, -1), LEN(string_agg(CodigoPeca, ',')))
  ) AS ID_PRODUTO,
  1 AS TP_CODIGO
FROM TabelaPecas
GROUP BY <AgrupamentoNecessario>
```

---

## Aplicação no Sol.NET_Conversao

- **Uso:** Ao migrar produtos/referências onde o legado pode ter múltiplos códigos agregados, garante que **apenas o primeiro** seja migrado como identificador único.
- **Benefício:** Previne duplicidade e normaliza referências, facilitando integrações e buscas no ERP.
- **Compatibilidade:**  
  - SQL Server: Padrão conforme acima.  
  - Firebird: Adaptar usando funções equivalentes (`LIST()` + manipulação de string).

---

## Observações

- **ISO SQL:** O padrão acima segue as recomendações de compatibilidade entre SGBDs e Clean Code do projeto.
- **Adaptação para Firebird:** Utilize `LIST(CodigoPeca, ',')` em vez de `string_agg` e funções de substring/posição conforme suporte do SGBD.
- **Exemplo no Builder:**  
  ```pascal
  .AddCampo('ID_PRODUTO', 'SUBSTRING(string_agg(CodigoPeca, '',''), 1, COALESCE(NULLIF(CHARINDEX('','', string_agg(CodigoPeca, '','')) - 1, -1), LEN(string_agg(CodigoPeca, '',''))))')
  .AddCampo('TP_CODIGO', '1')
  ```

---

## Referências

- Documentação Técnica – SolNET_Conversao
- [Documentacao Basica.md](../Documentacao Basica.md)
- SQL Server Docs: [string_agg](https://learn.microsoft.com/en-us/sql/t-sql/functions/string-agg-transact-sql)
- Firebird Docs: [LIST aggregate function](https://firebirdsql.org/refdocs/langrefupd25-list.html)

---

# 🟢 Correção de Erros de Foreign Key na Conversão – Padrão Sol.NET_Conversao

## Contexto

Durante a conversão de dados legados para o Sol.NET, um erro comum é a violação de restrição de **Foreign Key** (FK), especialmente quando os registros da tabela de destino referenciam chaves inexistentes ou inválidas (exemplo: valor `-1`, nulo, vazio ou apenas espaços).  
O erro típico reportado é:

```
Foreign key reference target does not exist Problematic key value is ("ID_ESTOQUE_LOCALIZACAO" = -1)
[FireDAC][Phys][FB] violation of FOREIGN KEY constraint "FK_PRODUTO_LOCALIZACAO_EL" on table "PRODUTO_LOCALIZACAO"
```

## Causa

- O campo do relacionamento (ex: `ID_ESTOQUE_LOCALIZACAO`) recebe um valor que não existe na tabela referenciada (exemplo: `-1`, nulo, string vazia, apenas espaços).
- Dados legados podem conter registros sem valor real para o campo de localização, levando a tentativas de vínculo com chave inexistente.

## Solução Recomendada

### 1. **Filtrar Registros com WHERE**
Adicione uma cláusula `WHERE` que só permita registros com valor real no campo de referência.  
Utilize `COALESCE` e `TRIM` para garantir que não sejam nulos, vazios ou apenas espaços:

```pascal
.AddWhere('COALESCE(Ecommerce_localizacao, TRIM(Ecommerce_localizacao)) <> ''''')
```

**Explicação:**  
- `COALESCE(Ecommerce_localizacao, TRIM(Ecommerce_localizacao))` retorna o valor do campo ou o valor sem espaços; se ambos forem vazios/nulos, a condição é falsa.
- Apenas registros com valor real entram na conversão.

### 2. **Exemplo Completo no ConversaoBuilder**

```pascal
var ParametroConversao: TParametrosConversao := TConversaoBuilder.Create
  .SetTabelaConversao(TTabelaEstoqueLocalizacao.Create, 'tbProdutos')
  .AddPrimaryKey('Ecommerce_localizacao')
  .AddCampo('DESCRICAO', 'Ecommerce_localizacao')
  .AddCampo('ID_EMPRESA', '0')
  .AddWhere('COALESCE(Ecommerce_localizacao, TRIM(Ecommerce_localizacao)) <> ''''')
  .Build;

ConversaoEstoqueLocalizacao(ParametroConversao);
```

### 3. **Vantagens da Abordagem**

- Evita conversão de registros inválidos que gerariam erro de FK
- Garante integridade referencial: apenas valores existentes e consistentes são migrados
- Segue padrão Clean Code e boas práticas de SQL universal (compatível com Firebird e SQL Server)

### 4. **Checklist de Correção de FK**

- [x] Verificar campos referenciais na origem
- [x] Filtrar registros sem valor real (nulo, vazio ou espaços)
- [x] Validar se todos os valores referenciados existem na tabela destino
- [x] Adicionar WHERE usando `COALESCE` e `TRIM`
- [x] Testar migração para garantir ausência de violação de FK

### 5. **Alternativas Adicionais**

- Se for necessário criar registros de localização faltantes, implemente sub‑conversão para gerar localizações antes de migrar dependentes.
- Para casos onde valor `-1` representa ausência, adapte o filtro para excluir também esse valor:

```pascal
.AddWhere('COALESCE(Ecommerce_localizacao, TRIM(Ecommerce_localizacao)) <> '''' AND Ecommerce_localizacao <> -1')
```

---

## Referência

- [Documentacao Basica.md](../Conversao/Documentacao Basica.md) – Guia prático de conversão
- [FireDAC][Phys][FB] – Mensagens de erro de Foreign Key

---

# 🟢 Padrão de Uso de CAST e UNION para Unificação de Códigos

## Objetivo

Unificar diferentes tipos de códigos de produtos (código interno, fabricante, referência, original) em uma única saída, utilizando a cláusula `UNION` e conversões de tipos via `CAST`.  
Este padrão é essencial para normalizar dados legados e garantir compatibilidade de tipos entre os SELECTs, conforme exigido pelo SQL universal (Firebird/SQL Server).

---

## 🌐 Regra do UNION

> **Todas as colunas correspondentes entre os SELECTs precisam ser do mesmo tipo de dado ou, pelo menos, implicitamente conversíveis.**

Se os tipos não forem compatíveis, o SQL retorna erro:  
`Conversion failed when converting the varchar value ... to data type int.`

---

## 🔧 Padrão de CAST

- Sempre **CAST** campos para o tipo mais abrangente necessário (normalmente `VARCHAR` para códigos).
- Garante que, ao combinar select de diferentes fontes, não há conflitos de tipo.
- Permite manipulação posterior (ex: agrupamentos, JOINs, ordenação).

---

## 💡 Exemplo Prático

```sql
SELECT
	DISTINCT CAST(CodigoPeca AS VARCHAR(50)) ID_PRODUTO,
	1 TP_CODIGO,
	CAST(CodigoPeca AS VARCHAR(50)) CODIGO,
	-1 ID_PESSOA,
	1 PADRAO,
	0 TP_BARRA,
	1 TP_BALANCA
FROM
	tbProdutos

UNION

SELECT
	DISTINCT CodigoPeca ID_PRODUTO,
	2 TP_CODIGO,
	NumFabricante CODIGO,
	CodigoFabricante ID_PESSOA,
	1 PADRAO,
	1 TP_BARRA,
	1 TP_BALANCA
FROM
	tbProdutos

UNION

SELECT
	DISTINCT SUBSTRING(string_agg(CodigoPeca, ','), 1, COALESCE(NULLIF(CHARINDEX(',', string_agg(CodigoPeca, ',')) - 1, -1), LEN(string_agg(CodigoPeca, ',')))) ID_PRODUTO,
	1 TP_CODIGO,
	Referencia CODIGO,
	-1 ID_PESSOA,
	0 PADRAO,
	1 TP_BARRA,
	0 TP_BALANCA
FROM
	tblCAPRSIMI
WHERE
	(CodigoRefLinha not in(71, 28, 15, 16, 31, 8, 9, 119, 74))
GROUP BY
	Referencia

UNION

SELECT
	DISTINCT CodigoPeca ID_PRODUTO,
	1 TP_CODIGO,
	NumOriginal CODIGO,
	-1 ID_PESSOA,
	0 PADRAO,
	0 TP_BARRA,
	0 TP_BALANCA
FROM
	tbProdutos
WHERE
	(NumOriginal <> '')
```

---

## 📝 Explicação dos Principais Pontos

- **CAST(CodigoPeca AS VARCHAR(50))**: Garante que `CodigoPeca`, que pode ser inteiro ou string, será tratado como texto em todas as linhas do UNION.
- **SUBSTRING + string_agg**: Extrai o primeiro código agregado para garantir unicidade e compatibilidade de tipos.
- **TP_CODIGO, PADRAO, TP_BARRA, TP_BALANCA**: Colunas fixas para padronização conforme regra Sol.NET.
- **ID_PESSOA**: Ajustado para -1 ou valor real conforme contexto do código.
- **WHERE**: Filtra apenas códigos relevantes, excluindo grupos indesejados.

---

## 🚦 Boas Práticas

- **Sempre CAST para o tipo mais abrangente!**  
  - Se um SELECT traz inteiro e outro varchar, use `CAST(... AS VARCHAR)` em todos.
- **Evite misturar tipos implícitos no UNION.**
- **Documente cada campo fixo (TP_CODIGO, PADRAO, etc) para facilitar entendimento futuro.**
- **Para Firebird, adapte CAST para padrão correspondente (`VARCHAR(N)` disponível normalmente).**
- **No ConversaoBuilder:**  
  ```pascal
  .AddCampo('ID_PRODUTO', 'CAST(CodigoPeca AS VARCHAR(50))')
  ```

---

## 💬 Aplicação no Sol.NET_Conversao

- Usado para migrar e normalizar códigos de produtos vindos de múltiplas fontes (fabricante, referência, original).
- Garante que o ERP Sol.NET tenha chave única e padronizada para busca e integração.
- Permite fácil expansão para novos tipos de código (basta adicionar novo SELECT ao UNION com o CAST).

---

## 📋 Referência

Consulte o [Guia Prático: Criando uma Nova Conversão](Documentacao Basica.md) para exemplos de uso do ConversaoBuilder e padrões de SQL universal.

---


# 🟢 Ajuste no Campo SUBCONVERSAO – Evitar Criação Desnecessária de Tabela Auxiliar

## Contexto

Em conversões envolvendo SUBCONVERSÕES (relacionamentos 1:N, como endereços de clientes), um erro comum é a **criação inadvertida de tabela auxiliar** para campos de vínculo (ex: IDs), especialmente ao passar o parâmetro `TTabelaPessoa.Create(Cliente)` no método `AddCampo`.  
Isso pode causar **lentidão extrema** durante o processo de conversão, já que o sistema pode tentar criar/consultar tabelas auxiliares para cada vínculo, multiplicando o volume de operações.

---

## ⚠️ Problema

### Antes (com tabela auxiliar desnecessária)

```pascal
var ParametrosEndereco: TParametrosSubConversao := TSubConversaoBuilder.Create
  .SetTabelaConversao(TTabelaEndereco.Create(), 'tbClientes')
  .AddCampo('ID_VINCULO', 'CodigoCliente', TTabelaPessoa.Create(Cliente))
  .AddCampo('ID_ESTADO', 'CodigoUF')
  .AddCampo('ID_CIDADE', 'Municipio')
  .AddCampo('LOGRADOURO', 'Endereco')
```

- **Impacto:** Para cada campo de vínculo, uma tabela auxiliar pode ser criada, gerando consultas e escrituras redundantes.
- **Sintoma:** Conversão demorada, sobrecarga de consultas, aumento no tempo total de processamento.

---

## ✅ Solução – Remover Tabela Auxiliar do Campo de Vínculo

### Depois (correto, sem tabela auxiliar)

```pascal
var ParametrosEndereco: TParametrosSubConversao := TSubConversaoBuilder.Create
  .SetTabelaConversao(TTabelaEndereco.Create(), 'tbClientes')
  .AddCampo('ID_VINCULO', 'CodigoCliente')
  .AddCampo('ID_ESTADO', 'CodigoUF')
  .AddCampo('ID_CIDADE', 'Municipio')
  .AddCampo('LOGRADOURO', 'Endereco')
```

- **Resultado:** Os campos de vínculo são mapeados diretamente, sem consulta/criação de tabela auxiliar desnecessária.
- **Benefício:** Conversão muito mais rápida e eficiente, sem sobrecarga de busca de de/para.

---

## 🔍 Como Identificar

- Se o parâmetro de tabela auxiliar (`TTabelaPessoa.Create(...)`) está sendo passado para campos de vínculo (como IDs), revise e remova.
- Use o parâmetro de tabela auxiliar **apenas** em campos que realmente precisam de lookup de de/para (ex: busca de código alternativo, relacionamento externo).
- Campos simples de vínculo (IDs diretos) **não devem** receber tabela auxiliar.

---

## 📋 Checklist de Ajuste

- [x] Revisar todos os métodos `AddCampo` nos botões de sub‑conversão.
- [x] Remover tabela auxiliar de campos de vínculo.
- [x] Manter tabela auxiliar apenas em campos que exigem de/para.
- [x] Testar tempo de conversão antes/depois do ajuste.
- [x] Documentar no código o motivo do ajuste para futuras manutenções.

---

## 👨‍💻 Boas Práticas

- **Sempre revise campos de vínculo em sub-conversões**
- **Só utilize tabela auxiliar quando necessário para relacionamento externo**
- **Documente ajustes para consulta futura**

---
## **📌REMOVENDO PONTOS E CARACTERES E DEIXANDO SO NUMEROS!  `[TFuncoes.SoNumeros]` no ConversaoBuilder**

### **O que é?**

A função `SoNumeros` é um dos valores do enum `TFuncoes` disponível para uso nos mapeamentos do **ConversaoBuilder**.

Ela serve para **limpar** o valor de um campo, removendo todos os caracteres não numéricos (como pontos, traços, espaços, letras etc.), deixando apenas os dígitos no resultado.

---

### **Como usar no ConversaoBuilder?**

Ao configurar um campo no Builder, basta adicionar `[TFuncoes.SoNumeros]` no último parâmetro do método `.AddCampo`.

Exemplo prático:

Pascal

`.AddCampo('CEST', 'CEST', nil, '', [TFuncoes.SoNumeros])`

### **Explicação dos parâmetros:**

- `'CEST'` → nome do campo de destino no Sol.NET
- `'CEST'` → nome do campo de origem (na tabela do cliente)
- `nil` → não utiliza tabela auxiliar para lookup
- `''` → sem busca dinâmica
- `[TFuncoes.SoNumeros]` → **aplica a função para filtrar somente números**

---

### **Quando usar?**

Utilize `[TFuncoes.SoNumeros]` sempre que precisar garantir que o valor migrado para o Sol.NET contenha **apenas números**.

Exemplos de campos típicos:

- CEST (código de situação tributária)
- NCM (classificação fiscal)
- CPF/CNPJ
- Código de barras
- Telefone

---

### **Benefícios**

- Evita problemas de validação ou duplicidade causados por caracteres extras.
- Garante compatibilidade com regras de negócio do Sol.NET, que exigem apenas números em certos campos.
- Facilita futuras integrações e cruzamento de dados.

---

### **Exemplo de aplicação:**

Pascal

`.AddCampo('CPF', 'CPF_CLIENTE', nil, '', [TFuncoes.SoNumeros])
.AddCampo('CNPJ', 'CNPJ_CLIENTE', nil, '', [TFuncoes.SoNumeros])
.AddCampo('CEST', 'CEST', nil, '', [TFuncoes.SoNumeros])
.AddCampo('NCM', 'ClassificacaoFiscal', nil, '', [TFuncoes.SoNumeros])`

---

### **Resumo**

- `[TFuncoes.SoNumeros]` remove pontos, traços, espaços e tudo que não for número do campo selecionado.
- Use no último parâmetro de `.AddCampo` do ConversaoBuilder.
- Ideal para garantir conformidade e integridade dos dados migrados.


# AddPrimaryKey(SQLOrigem.CONCAT(['''F''', 'CodigoFuncionario']))

### **que está sendo feito?**

Você está definindo a chave primária (PK) para a tabela de destino usando uma **concatenação** de uma letra fixa (`'F'`) com o campo `CodigoFuncionario`, formando algo como:

- **PK destino:** `'F' + CodigoFuncionario`
- Exemplo: Se `CodigoFuncionario = 123`, então o PK será `'F123'`

### **Motivo da concatenação**

No Sol.NET, especialmente na conversão de entidades diferentes (Funcionário, Mecanico, Representante, Vendedor), **cada tipo pode ter códigos numéricos que se repetem entre tabelas**. Para garantir que, ao migrar para o novo banco, **cada registro seja único**, é comum "prefixar" o código com uma letra identificadora do tipo de entidade:

| **Tipo** | **Prefixo** | **Exemplo PK** |
| --- | --- | --- |
| Funcionário | F | F123 |
| Mecânico | M | M123 |
| Representante | R | R123 |
| Vendedor | V | V123 |

**Sem esse prefixo**, você poderia ter um funcionário e um vendedor ambos com código `123`, causando conflito de chave primária.

### **Por que usar o helper `SQLOrigem.CONCAT`?**

O helper `SQLOrigem.CONCAT` é usado **para gerar SQL compatível** tanto com Firebird quanto com SQL Server. Ele monta a expressão de concatenação correta para cada banco:

- **Firebird:** `'F' || CodigoFuncionario`
- **SQL Server:** `'F' + CodigoFuncionario`

Isso garante que o campo PK será corretamente calculado em qualquer SGBD suportado pelo projeto.

### **Vantagens**

- **Unicidade garantida:** Evita colisão de PK entre diferentes tipos de entidade.
- **Migração segura:** Facilita conversão de sistemas legados com códigos sobrepostos.
- **Padronização:** Segue padrão adotado no projeto para entidades multi-tipo.
- **Compatibilidade total:** Funciona em Firebird e SQL Server sem ajuste manual.

---

## **Resumo**

Você usa `.AddPrimaryKey(SQLOrigem.CONCAT(['''F''', 'CodigoFuncionario']))` para:

- Garantir que cada funcionário tenha uma chave primária única e distinta de outros tipos de entidades (mesmo que os números se repitam).
- Permitir migração segura, evitando conflitos no banco destino.
- Seguir o padrão do projeto Sol.NET de prefixar PKs conforme o tipo de entidade.
- Gerar SQL universal, compatível com Firebird e SQL Server.
