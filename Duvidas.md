# 🧩 Conversões com Builder e Sintaxe Tradicional — Delphi Pascal

## 📅 06/08/25 – Passagem do botão para forma Builder (erro persistente)

### 🔘 **Botão Status Pessoa (sintaxe tradicional)**

```pascal
pascal
CopiarEditar
procedure TFrmSDX.BotaoStatusPessoa;
begin
  var ParametroConversao: TParametrosConversao := TParametrosConversao.Create(nil);
  ParametroConversao.Tabelas.Create(TTabelaPessoaStatus.Create);

  var ParametroSql: TParametroSQL := TParametroSQL.Create('SITUACAOEMP');
  ParametroSql.AdicionarCamposPk('ID_PESSOA_TP_STATUS', 'IDREG');

  with ParametroSql.ListaCampos do
  begin
    AddCampo('DESCRICAO', 'NOME');
  end;

  ParametroConversao.AddParametro(ParametroSql);

  ConversaoTpStatusPessoas(ParametroConversao);
end;

```

---

### 🔨 **Mesma lógica usando Builder**

```pascal
pascal
CopiarEditar
procedure TFrmSDX.BotaoStatusPessoa;
begin
  var ParametroConversao: TParametrosConversao := TConversaoBuilder.Create
    .SetTabelaConversao(TTabelaPessoaStatus.Create, 'SITUACAOEMP')
    .AddPrimaryKey('IDREG')
    .AddCampo('DESCRICAO', 'NOME')
    .Build;

  ConversaoTpStatusPessoas(ParametroConversao);
end;

```

---

## 📅 07/08/25 – Uso de `IIF` para conversão de `var` para `int`

Exemplo:

```pascal
pascal
CopiarEditar
.AddCampo('TPFABRICANTE', 'IIF(e.FABRICANTE = ''S'', 1, 0)')

```

Use essa forma quando precisar **converter valores condicionais (ex: 'S' para 1 e outro valor para 0)** no campo convertido.

---

## 🧾  – Modelo Tradicional (Botão Pessoas)

```pascal
pascal
CopiarEditar
procedure TFrmSDX.BotaoPessoas;
begin
  var ParametrosConversao: TParametrosConversao := TParametrosConversao.Create(nil);
  ParametrosConversao.Tabelas.Create(TTabelaPessoa.Create(Pessoa));

  var ParametrosEndereco: TParametrosSubConversao := TParametrosSubConversao.Create(nil);
  ParametrosEndereco.Tabelas.Create(TTabelaEndereco.Create);
  ParametrosEndereco.CampoFK := 'ID_VINCULO';

  var ParametroSql: TParametroSQL := TParametroSQL.Create('EMPRESAS');
  ParametroSql.AdicionarCamposPk('ID_PESSOA', 'IDREG');

  with ParametroSql.ListaCampos do
  begin
    AddCampo('CODIGO', 'CODEMP');
    AddCampo('NOME', 'RAZAO');
    AddCampo('DESCRICAO', 'IIF(FANTASIA <> '''', FANTASIA, RAZAO)');
    AddCampo('TELEFONE', 'DDD + FONE1');
    AddCampo('TELEFONECONTATO', 'CONTATO');
    AddCampo('INSCRICAO_ESTADUAL', 'INSCREST');
    AddCampo('CPF', 'CPF');
    AddCampo('EMAIL', 'EMAIL', nil, '', '', [Nenhum]);
    AddCampo('VL_LIMITE_CREDITO', 'LIMCRED');
    AddCampo('TPCLIENTE', 'IIF (CLIENTE = ''S'', 1, 0)');
    AddCampo('TPFORNECEDOR', 'IIF (FORNECEDOR = ''S'', 1, 0)');
    AddCampo('TPFABRICANTE', 'IIF (FABRICANTE = ''S'', 1, 0)');
    AddCampo('BLOQUEADO', '0');
    // AddCampo('ID_PESSOA_TP_STATUS', 'VARATAC', TTabelaPessoaStatus.Create);
    // AddCampo('ID_PESSOA_TP_CF', SQLOrigem.CONCAT(['CONDCOMERC', '1'], '#'), TTabelaTipoClienteForne.Create);
    // AddCampo('ID_PESSOA_RAMO_ATIVIDADE', 'CODCLASSE', TTabelaTpRamoAtividade.Create);
    AddCampo('INATIVO', '0');
  end;

```

---

## 🧱 Builder (Botão Pessoas com JOIN)

```pascal
pascal
CopiarEditar
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
    .SetTabelaConversao(TTabelaPessoa.Create(Pessoa), 'EMPRESAS e')
    .AddPrimaryKey('e.IDREG')
    .AddCampo('CODIGO', 'e.CODEMP')
    .AddCampo('NOME', 'e.RAZAO')
    .AddCampo('DESCRICAO', 'IIF(e.FANTASIA <> '''', e.FANTASIA, e.RAZAO)')
    .AddCampo('TELEFONE', 'e.DDD + e.FONE1')
    .AddCampo('TELEFONECONTATO', 'e.CONTATO')
    .AddCampo('CELULAR', 'e.DDDCEL + E.CELULAR')
    .AddCampo('INSCRICAO_ESTADUAL', 'e.INSCREST')
    .AddCampo('CPF', 'e.CPF')
    .AddCampo('CNPJ', 'e.CNPJ')
    .AddCampo('EMAIL', 'e.EMAIL', nil, '', '', [Nenhum])
    .AddCampo('VL_LIMITE_CREDITO', 'c.LIMCRED')
    .AddCampo('TPCLIENTE', 'IIF(e.CLIENTE = ''S'', 1, 0)')
    .AddCampo('TPFORNECEDOR', 'IIF(e.FORNECEDOR = ''S'', 1, 0)')
    .AddCampo('TPFABRICANTE', 'IIF(e.FABRICANTE = ''S'', 1, 0)')
    .AddCampo('BLOQUEADO', 'IIF(e.BloqPagto = ''S'', 1, 0)')
    .AddCampo('BLOQUEADO_MOV', 'IIF(e.BloqTpMovStk = ''S'', 1, 0)')
    .AddCampo('ID_PESSOA_TP_STATUS', 'VARATAC', TTabelaPessoaStatus.Create)
    .AddCampo('ID_PESSOA_TP_CF', SQLOrigem.CONCAT(['CONDCOMERC', '1'], '#'), TTabelaTipoClienteForne.Create)
    .AddCampo('ID_PESSOA_RAMO_ATIVIDADE', 'CODCLASSE', TTabelaTpRamoAtividade.Create)
    .AddCampo('INATIVO', '0')
    .AddJoin('LEFT JOIN Credito c ON e.Idreg = c._IdMst')
    .Build;

  var ParametroSql: TParametroSQL := TParametroSQL.Create('EMPRESAS');
  ParametroSql.AdicionarCamposPk('ID_ENDERECO');

  with ParametroSql.ListaCampos do
  begin
    AddCampo('ID_VINCULO', 'IDREG', TTabelaPessoa.Create(Pessoa));
    AddCampo('ID_ESTADO', '_IdUF');
    AddCampo('ID_CIDADE', 'Cidade');
    AddCampo('CEP', 'CEP');
    AddCampo('BAIRRO', 'BAIRRO');
    AddCampo('LOGRADOURO', 'LOGRA');
    AddCampo('NUMERO', 'NUMEND');
    AddCampo('COMPLEMENTO', 'COMPLEND');
    AddCampo('TIPO', '1');
    AddCampo('TIPO_VINCULO', '0');
  end;

  var ParametrosEndereco: TParametrosSubConversao := TParametrosSubConversao.Create(nil);
  ParametrosEndereco.Tabelas.Create(TTabelaEndereco.Create);
  ParametrosEndereco.CampoFK := 'ID_VINCULO';

  ParametrosEndereco.AddParametro(ParametroSql);

  ConversaoPessoas(ParametrosConversao, ParametrosEndereco);
end;

```

---

## 📅 08/08/25 –🟢 Migração: BotaoMovimentos: Tradicional → ConversaoBuilder



## **1. Criação dos Objetos de Conversão**

### **Antes (Tradicional):**

- Instancia as classes manualmente:
    
    Pascal
    
    `var ParametroMovimento: TParametrosConversao := TParametrosConversao.Create(nil);
    ParametroMovimento.Tabelas.Create(TTabelaMovimentos.Create(VPedido));`
    

### **Depois (Builder):**

- Usa o método fluente do Builder:
    
    Pascal
    
    `var ParametroMovimento: TParametrosConversao := 
      TConversaoBuilder.Create
        .SetTabelaConversao(TTabelaMovimentos.Create(VPedido), 'STKDOC MV')
        //...
        .Build;`
    

**✔️ Troca:**

Substitui a criação manual e o método `.Tabelas.Create` por `.SetTabelaConversao` no Builder.

---

## **2. Configuração do SQL de Origem**

### **Antes:**

- Instancia manualmente o objeto SQL:
    
    Pascal
    
    `var ParametroSQLMovimento: TParametroSQL := TParametroSQL.Create('STKDOC MV');
    ParametroSQLMovimento.AdicionarCamposPk('ID_MOVIMENTO', 'M.ID_MOVIMENTO');`
    

### **Depois:**

- Usa método fluente no Builder:
    
    Pascal
    
    `.SetTabelaConversao(TTabelaMovimentos.Create(VPedido), 'STKDOC MV')
    .AddPrimaryKey('M.ID_MOVIMENTO')`
    

**✔️ Troca:**

Substitui criação manual do SQL e chamada de chave primária por método fluente.

---

## **3. Adição dos Campos**

### **Antes:**

- Bloco `with ... do` para adicionar campos:
    
    Pascal
    
    `with ParametroSQLMovimento.ListaCampos do
    begin
      AddCampo('NUMERO_DOCUMENTO', 'NUMDOC');
      AddCampo(...);
      // ...
    end;`
    

### **Depois:**

- Encadeia `.AddCampo` diretamente no Builder:
    
    Pascal
    
    `.AddCampo('NUMERO_DOCUMENTO', 'NUMDOC')
    .AddCampo(...)
    // ...`
    

**✔️ Troca:**

Remove o `with`, substitui por encadeamento direto no Builder.

---

## **4. Adição dos JOINs**

### **Antes:**

- Adiciona via lista manual:
    
    Pascal
    
    `ParametroSQLMovimento.SqlJoin.Add('LEFT JOIN REQSRVNFE NF ON ...');`
    

### **Depois:**

- Encadeia `.AddJoin` no Builder:
    
    Pascal
    
    `.AddJoin('LEFT JOIN REQSRVNFE NF ON ...')`
    

**✔️ Troca:**

Remove `SqlJoin.Add`, usa encadeamento.

---

## **5. Adição dos Parâmetros SQL ao Conversor**

### **Antes:**

- Adiciona manualmente ao objeto de parâmetros:
    
    Pascal
    
    `ParametroMovimento.AddParametro(ParametroSQLMovimento);`
    

### **Depois:**

- Não precisa, o Builder já gerencia e retorna tudo pronto no `.Build`.

**✔️ Troca:**

Remove necessidade de adicionar manualmente.

---

## **6. Configuração dos Itens do Movimento**

### **Antes:**

- Criação manual dos objetos e campos dos itens:
    
    Pascal
    
    `var ParametroMovimentoItem: TParametrosSubConversao := TParametrosSubConversao.Create(nil);
    ParametroMovimentoItem.Tabelas.Create('MOVIMENTOS_ITENS');
    var ParametroSQLMovimentoItem: TParametroSQL := TParametroSQL.Create('STKPRD MVI');
    ParametroSQLMovimentoItem.AdicionarCamposPk('ID_MOVIMENTO_ITEM');
    ParametroMovimentoItem.CampoFK := 'ID_MOVIMENTO';
    
    with ParametroSQLMovimentoItem.ListaCampos do
    begin
      AddCampo(...);
      // ...
    end;
    ParametroMovimentoItem.AddParametro(ParametroSQLMovimentoItem);`
    

### **Depois:**

- Usa o Builder para criar todos os campos e joins de forma fluente:
    
    Pascal
    
    `var ParametroSQLMovimentoItem: TParametrosConversao :=
      TConversaoBuilder.Create
        .SetTabelaConversao(TTabelaMovimentos.Create(VPedido), 'STKPRD MV')
        .AddPrimaryKey('ID_MOVIMENTO_ITEM')
        .AddCampo(...)
        .AddJoin(...)
        .Build;
    ParametroMovimentoItem.AddParametro(ParametroSQLMovimentoItem);`
    

**✔️ Troca:**

Toda a configuração dos itens passa a ser feita pelo Builder, eliminando o bloco `with` e inicializações manuais.

---

## **7. Execução da Conversão**

### **Antes e Depois:**

- A chamada final de execução permanece igual:
    
    Pascal
    
    `ConversaoMovimentos(ParametroMovimento, ParametroMovimentoItem);`
    

---

# **Resumo das Trocas**

| **Tradicional** | **Builder** |
| --- | --- |
| Criação manual de objetos | Builder fluente com `.Create` e `.Build` |
| `.Tabelas.Create(...)` | `.SetTabelaConversao(...)` |
| `TParametroSQL.Create(...)` | `.SetTabelaConversao(..., 'Alias')` |
| `AdicionarCamposPk(...)` | `.AddPrimaryKey(...)` |
| Bloco `with ... do` para campos | Encadeamento `.AddCampo(...)` |
| `SqlJoin.Add(...)` | Encadeamento `.AddJoin(...)` |
| Adição manual de parâmetros | Builder já retorna objeto pronto |
| Bloco manual para sub-conversão de itens | Builder para itens, igual ao principal |
| Mais linhas e inicializações | Menos linhas, mais legível, menos erro |

---

## **Benefícios da Troca**

- **Código mais limpo e curto**
- **Menos propenso a erro**
- **Melhor legibilidade**
- **Facilidade de manutenção**
- **Padronização para todas as rotinas futuras**

## 📅 11/08/25 –🟢 Migração: BotaoProdutos: Tradicional → ConversaoBuilder

### Pascal Procedure: Migração para ConversãoBuilder



#### Depois (Utilizando ConversaoBuilder):

```pascal
procedure TFrmSDX.BotaoProduto;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaProduto.Create(Produto), 'ITENS P')
      .AddPrimaryKey('P.IDREG')
      .AddCampo('DESCRICAO', 'P.Descricao')
    // .AddCampo('ID_FORNECEDOR', 'P.CODFAB', TTabelaPessoa.Create(Pessoa));
      .AddCampo('ID_UNIDADE', 'P.IDUNIDMED', TTabelaUnidade.Create)
      .AddCampo('ID_FAMILIA_PRODUTO', 'P._IDGRUPO', TTabelaFamilia.Create)
      .AddCampo('ID_NCM', 'P._IDNCM', TTabelaNcm.Create)
      .AddCampo('PRECO_VENDA_1', 'PRECO.PRCVDAPADRAO')
      .AddCampo('CUSTO_INICIAL', 'PRECO.CUSTOINFPADRAO')
      .AddCampo('CUSTO_MEDIO_UNITARIO', 'PRECO.CUSTOINFPADRAO')
      .AddCampo('APLICACAO', 'P.TXTAPLIC')
      .AddJoin('LEFT JOIN LSTPRECOITENS AS PRECO ON (PRECO.IDREG = ESTL._IDLSTPRECOITENSPADRAO)')
      .build;
  ConversaoProduto(ParametrosConversao);
end;
```

#### Pontos importantes da migração

- Utilização do padrão **Builder** para organizar e encapsular os parâmetros de conversão.
- Os campos obrigatórios e joins agora são definidos fluentemente.
- Facilita manutenção, extensão e testes.
- O método `ConversaoProduto` recebe agora um objeto de parâmetros estruturado.
- Comentários indicam campos opcionais ou em análise.

#### Campos convertidos:

| Campo                 | Origem               | Tipo de Conversão             |
|-----------------------|----------------------|-------------------------------|
| DESCRICAO             | P.Descricao          | Campo direto                   |
| ID_UNIDADE            | P.IDUNIDMED          | Tabela relacionada (Unidade)   |
| ID_FAMILIA_PRODUTO    | P._IDGRUPO           | Tabela relacionada (Familia)   |
| ID_NCM                | P._IDNCM             | Tabela relacionada (NCM)       |
| PRECO_VENDA_1         | PRECO.PRCVDAPADRAO   | Join com tabela de preços      |
| CUSTO_INICIAL         | PRECO.CUSTOINFPADRAO | Join com tabela de preços      |
| CUSTO_MEDIO_UNITARIO  | PRECO.CUSTOINFPADRAO | Join com tabela de preços      |
| APLICACAO             | P.TXTAPLIC           | Campo direto                   |

#### Observação

- O campo `ID_FORNECEDOR` está comentado, indicando análise ou futura implementação.
- O join principal é realizado com a tabela `LSTPRECOITENS` para obter preços e custos.

#### Resumo

A migração do método tradicional para o uso do ConversaoBuilder traz maior clareza, modularidade e facilidade para futuras expansões, seguindo boas práticas de design de software.

###UPDATE 
ADICIONADO UM INTEM .AddCampo('TIPO_ITEM','IIF(P.TipoItem = ''S'', 1, 0)'). UTILIZANDO O IIF
Sobre a expressão IIF
IIF(condição, valor_se_verdadeiro, valor_se_falso)
É aceito tanto em Firebird quanto em SQL Server (compatível com o padrão do projeto).

P.TipoItem é CHAR na origem, então a comparação 'P.TipoItem = ''S''' está correta.

Valores retornados:

Se TipoItem = 'S' → retorna 1
Senão → retorna 0
Isso converte perfeitamente para INT no destino.
3. Observações do projeto
O campo de destino (TIPO_ITEM) é INT — ok!
O campo de origem (TipoItem) é CHAR — ok!
Não é necessário informar tabela auxiliar, pois não há lookup de ID (é apenas uma conversão de valor).
4. Sintaxe correta
A sintaxe está correta para o padrão do ConversaoBuilder e para a estrutura do Sol.NET.
Você está convertendo um campo CHAR para INT usando uma expressão SQL condicional.

5. Exemplo completo
Pascal
.AddCampo('TIPO_ITEM', 'IIF(P.TipoItem = ''S'', 1, 0)')
6. Dicas
Se você usar Firebird, pode usar IIF.
Se for SQL Server, o IIF também é aceito (desde SQL Server 2012).
Se quiser garantir compatibilidade máxima, pode usar CASE WHEN, mas o projeto aceita IIF.
Resumo
Sua sintaxe está correta!
O ConversaoBuilder vai gerar o campo TIPO_ITEM como INT conforme esperado.


# Duvidas

pelo que vi quase não será usado o callback. ja o auxiliar e o join será usado em todas conversoes. eu ainda não entendi a diferenã dos dois

### **Copilot said: Ótima observação! Vou esclarecer a diferença entre**

Ótima observação! Vou esclarecer a diferença entre **auxiliar** e **join** no contexto do Sol.NET_Conversao, conforme a documentação oficial do projeto.

---

## **JOIN**

O **JOIN** é um comando SQL utilizado para **relacionar duas ou mais tabelas** diretamente na consulta de origem.

- É usado quando você precisa que o SELECT de origem já traga campos de outra tabela relacionada.
- O relacionamento é feito via chave estrangeira ou campo comum.

**Exemplo:**

Pascal

`.AddJoin('LEFT JOIN tb_fabricante F ON F.Id_fabricante = P.Id_fabricante')
.AddCampo('NumFabricante', 'F.NumFabricante')`

Nesse caso, você está dizendo explicitamente que quer buscar o campo `NumFabricante` da tabela `tb_fabricante` (apelidada como F), usando o relacionamento.

---

## **Auxiliar (Lookup via Framework)**

O **auxiliar** é um recurso do framework de conversão que permite, no mapeamento de campo, **informar uma tabela auxiliar** para buscar o valor desejado.

- Você não precisa escrever o SQL do JOIN manualmente; o framework entende que, para preencher aquele campo, deve buscar na tabela auxiliar usando a chave estrangeira disponível.
- O relacionamento é resolvido automaticamente pela engine de conversão.

**Exemplo:**

Pascal

`.AddCampo('ID_FABRICANTE', 'NumFabricante', TTabelaPessoa.Create(Fab))`

Aqui, você informa ao Builder que o campo `ID_FABRICANTE` deve ser preenchido pegando o valor `NumFabricante` na tabela auxiliar do fabricante (`TTabelaPessoa.Create(Fab)`), que será localizado via o campo de referência presente na tabela de origem (`Id_fabricante`).

---

## **Resumo das Diferenças**

| **JOIN (SQL)** | **Auxiliar (Framework/Lookup)** |
| --- | --- |
| Você escreve o comando SQL JOIN manualmente | Você só informa a tabela auxiliar no campo |
| O SELECT já traz todos os dados relacionados | O framework faz o relacionamento automático |
| Mais flexível, mas exige mais código | Mais simples, mas depende da estrutura do framework |
| Usado para campos de múltiplas tabelas | Usado para campos lookup (1:1) |
| Exemplo: AddJoin + AddCampo com alias | Exemplo: AddCampo com TabelaAuxBuscar |

---

### **Quando usar cada um?**

- **JOIN:**
    - Quando precisa trazer vários campos de uma tabela auxiliar
    - Quando o relacionamento é complexo ou envolve múltiplas condições
    - Quando quer controlar o SQL e filtros manualmente
- **Auxiliar:**
    - Quando só precisa buscar um campo simples via chave estrangeira
    - Quando quer reduzir código e usar o poder do framework
    - Quando o relacionamento é direto (1:1 ou lookup simples)

---

**Na prática:**

- O framework do Sol.NET_Conversao permite que você use ambos, mas o **auxiliar** torna o mapeamento mais rápido e limpo para casos comuns.
- O **JOIN** ainda é necessário para situações mais complexas ou múltiplos campos da tabela relacionada.



## 📅 25/08/25 –🟢 AJUSTE DE Classificacao fiscal do cliente pega do campo ''TipoCliente'' da tabela cliente.
 
      
TINHA ESSA CONVERSÃO ONDE O ANALISTA INDENTIFICOU QUE O CAMPO FORMAAPLICACAO ERA O ERRADO E PEDIU PARA TRAZER AS INFORMAÇÕES DO CAMPO TIPOCLIENTE. DENTRO DESSE CAMPO TIPOCLIENTE TINHA
ESSAS 4 INFORMAÇÕES:
CONSUMIDOR
CONSUMIDOR NC
REVENDA
REVENDATARE
ONDE HÁ SEMELHANÇA ENTRE ELAS, TIVE QUE USAR O LIKE PARA AJUSATAR ISSO QUE FICOU 

 ```sql

SELECT  CodigoCliente,   
    CASE 
        WHEN TipoCliente LIKE 'CONSUMIDOR%' THEN 1
        WHEN TipoCliente LIKE '%REVENDA%' THEN 2
        ELSE 0
    END AS CategoriaNome
FROM TBCLIENTES;

OPÇÃO NORMAL, GERALMENTE É ESSA QUE É PEDIDA, O TRIM E O UPPER FOI PQ EU COLOQUEI.

------------ 
SELECT
  CODIGOCLIENTE,
  CASE
    WHEN UPPER(TRIM(TipoCliente)) LIKE 'CONSUMIDOR%' THEN 1   
    WHEN UPPER(TRIM(TipoCliente)) LIKE 'REVENDA%' THEN 2     
    ELSE 0
  END AS TipoClienteCodigo
FROM TBCLIENTES
WHERE UPPER(TRIM(TipoCliente)) LIKE 'CONSUMIDOR%' 
   OR UPPER(TRIM(TipoCliente)) LIKE 'REVENDA%';

-- sintaxe com like OFERECIDA PELO COPILOT  /// AQUI PEDI A SINTAXE NO COPILOT DO CASE + LIKE E ELE ME SURGERIU ISSO, ACRESCENTEI O TRIM E UPPER MAS AINDA NÃO SEI SE IREI CONTINUAR COM ELES.
SELECT 
    Nome,
    CASE 
        WHEN Nome LIKE 'Ana%' THEN 'Começa com Ana'
        WHEN Nome LIKE '%Silva%' THEN 'Contém Silva'
        ELSE 'Outro'
    END AS CategoriaNome
FROM Clientes;

 ```

```pascal
.AddCampo('TP_ATIVIDADE_COMERCIAL', 'CASE WHEN TipoCliente LIKE ''CONSUMIDOR%'' THEN 1 WHEN TipoCliente LIKE ''%REVENDA%'' THEN 2 ELSE 0 END') /// ACABOU FICANDO ESSE!

.AddCampo('TP_ATIVIDADE_COMERCIAL', 'CASE WHEN UPPER(TRIM(TIPOCLIENTE)) LIKE ''CONSUMIDOR%'' THEN 1 WHEN UPPER(TRIM(TIPOCLIENTE)) LIKE ''REVENDA%'' THEN 2 ELSE 0 END')

/// codigo completo so para duvidas futuras

procedure TFrmABMolas.BotaoClientes;
begin
  var ParametroConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaPessoa.Create(Cliente), 'tbClientes')
      .AddPrimaryKey('CodigoCliente')
      .AddCampo('TPCLIENTE', '1')
      .AddCampo('CODIGO', 'CodigoCliente')
      .AddCampo('NOME', 'RazaoSocial')
      .AddCampo('DESCRICAO', 'NomeFantasia')
      .AddCampo('CPF', 'CnpjCpf')
      .AddCampo('RG', 'IeImRG')
      .AddCampo('INDICADOR_ISC_EST_PES', 'IIF(RegimeApuracao = ''CONTRIBUINTE'', 1, 9)')
      .AddCampo('VL_LIMITE_CREDITO', 'LimiteCredito')
      .AddCampo('Email', 'Email1 + '';'' + EmailNFE')
      .AddCampo('CELULAR', 'Celular1')
      .AddCampo('BLOQUEADO', 'IIF(Status = 0, 0, 1)')
      .AddCampo('TELEFONECONTATO', 'CobTelefone')
      .AddCampo('TP_ATIVIDADE_COMERCIAL', 'CASE WHEN UPPER(TRIM(TIPOCLIENTE)) LIKE ''CONSUMIDOR%'' THEN 1 WHEN UPPER(TRIM(TIPOCLIENTE)) LIKE ''REVENDA%'' THEN 2 ELSE 0 END')
      .AddCampo('DT_CADASTRO', 'DataCadastro')
      .AddCampo('TELEFONE', 'Telefone1')
      .AddCampo('ID_PESSOA_RAMO_ATIVIDADE', 'CodigoRamo', TTabelaTpRamoAtividade.Create)
      .AddCampo('OBS', 'CAST(Observacoes AS VARCHAR(MAX))')
      .AddCampo('ID_PESSOA_REGIAO', 'CodigoRegiao', TTabelaRegioes.Create)
      .Build;
 ```


## 📅 28/08/25 –🟢 AJUSTE no Campo *SUBCONVERSAO* 

CASO A CONVERSÃO ESTEJA DEMORANDO MUITO, VERIFICAR SE NOS BOTÕES SUBCONVERSÃO ESTA CRIANDO TABELAS AUXILIARES NOS CAMPOS ID 
---
**ANTES** 

```pascal
  var ParametrosEndereco: TParametrosSubConversao := TSubConversaoBuilder.Create
      .SetTabelaConversao(TTabelaEndereco.Create(), 'tbClientes')
      .AddCampo('ID_VINCULO', 'CodigoCliente', TTabelaPessoa.Create(Cliente))
      .AddCampo('ID_ESTADO', 'CodigoUF')
      .AddCampo('ID_CIDADE', 'Municipio')
      .AddCampo('LOGRADOURO', 'Endereco')

```

**DEPOIS**
```pascal
  var ParametrosEndereco: TParametrosSubConversao := TSubConversaoBuilder.Create
      .SetTabelaConversao(TTabelaEndereco.Create(), 'tbClientes')
      .AddCampo('ID_VINCULO', 'CodigoCliente')
      .AddCampo('ID_ESTADO', 'CodigoUF')
      .AddCampo('ID_CIDADE', 'Municipio')
      .AddCampo('LOGRADOURO', 'Endereco')

```

## 📅 28/08/25 –🟢 Concatenação de campos complexos 

```sql
Concatenação do Email1 com o EmailNFE usando ; como separador somente quando ambos existirem.
Quando existir apenas um deles, deve mostrar só esse (sem ; nem espaços).
Quando não houver nenhum, deve retornar vazio (NULL tratado).


SELECT
    DISTINCT CodigoCliente ID_PESSOA,
    1 TPCLIENTE,
    CodigoCliente CODIGO,
    RazaoSocial NOME,
    NomeFantasia DESCRICAO,
    CnpjCpf CPF,
    IeImRG RG,
    IIF(RegimeApuracao = 'CONTRIBUINTE', 1, 9) INDICADOR_ISC_EST_PES,
    LimiteCredito VL_LIMITE_CREDITO,

    -- 👇 Ajuste no campo Email
    CASE 
        WHEN NULLIF(LTRIM(RTRIM(Email1)), '') IS NOT NULL 
             AND NULLIF(LTRIM(RTRIM(EmailNFE)), '') IS NOT NULL
            THEN Email1 + '; ' + EmailNFE
        WHEN NULLIF(LTRIM(RTRIM(Email1)), '') IS NOT NULL
            THEN Email1
        WHEN NULLIF(LTRIM(RTRIM(EmailNFE)), '') IS NOT NULL
            THEN EmailNFE
        ELSE ''
    END AS Email,

    Telefone1 TELEFONE,
    Contato1 TELEFONECONTATO,
    Celular1 CELULAR,
    CobTelefone CELULARCONTATO,
    IIF(Status = 0, 0, 1) BLOQUEADO,
    1 PERMITIR_PORTADOR,
    CASE
        WHEN TipoCliente LIKE '%REVENDA%' THEN 1
        ELSE 0
    END TP_ATIVIDADE_COMERCIAL,
    DataCadastro DT_CADASTRO,
    CodigoRamo ID_PESSOA_RAMO_ATIVIDADE,
    CAST(Observacoes AS VARCHAR(MAX)) OBS,
    CodigoRegiao ID_PESSOA_REGIAO
FROM tbClientes         /// substitui os ltrim e rtrim por trim
```

```pascal
.AddCampo('Email', 'CASE ' + 'WHEN NULLIF(TRIM(Email1), '''') IS NOT NULL AND NULLIF(TRIM(EmailNFE), '''') IS NOT NULL ' +
      'THEN Email1 + ''; '' + EmailNFE ' + 'WHEN NULLIF(TRIM(Email1), '''') IS NOT NULL ' + 'THEN Email1 ' +
      'WHEN NULLIF(TRIM(EmailNFE), '''') IS NOT NULL ' + 'THEN EmailNFE ' +
      'ELSE '''' ' + 'END')      
```

## 📅 28/08/25 –🟢 CAST E UNION

junta várias formas de códigos (código interno, código do fabricante, referência, código original) numa mesma saída unificada usando UNION.
se não fizesse o cast do codigopeca passando ele para varchar não daria pra fazer o union.
No UNION existe uma regra:

👉 Todas as colunas correspondentes entre os SELECTs precisam ser do mesmo tipo de dado ou, pelo menos, implicitamente conversíveis.

```pascal
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
### **CORREÇÃO DE ERROS DESSE TIPO:** 
Foreign key reference target does not exist
Problematic key value is ("ID_ESTOQUE_LOCALIZACAO" = -1)
Id Reg.: 0
Registro: 947
[FireDAC][Phys][FB] violation of FOREIGN KEY constraint "FK_PRODUTO_LOCALIZACAO_EL" on table "PRODUTO_LOCALIZACAO"

Foreign key reference target does not exist
Problematic key value is ("ID_ESTOQUE_LOCALIZACAO" = -1)
Id Reg.: 0
Registro: 948
[FireDAC][Phys][FB] violation of FOREIGN KEY constraint "FK_PRODUTO_LOCALIZACAO_EL" on table "PRODUTO_LOCALIZACAO"


      .AddPrimaryKey('Ecommerce_localizacao')
      .AddCampo('DESCRICAO', 'Ecommerce_localizacao')
      .AddCampo('ID_EMPRESA', '0')
	  
      .Build;

  ConversaoEstoqueLocalizacao(ParametroConversao);

       .AddPrimaryKey('Ecommerce_localizacao')
      .AddCampo('DESCRICAO', 'Ecommerce_localizacao')
      .AddCampo('ID_EMPRESA', '0')
      .AddWhere('COALESCE(Ecommerce_localizacao, TRIM(Ecommerce_localizacao)) <> ''''')
      .Build;

  ConversaoEstoqueLocalizacao(ParametroConversao);

ADICIONADO O WHERE: Ou seja, só entram na conversão os registros que têm um valor real em Ecommerce_localizacao (nem nulo, nem vazio, nem só espaços). 

## 📅 29/08/25 –🟢 QUEBRA LINHA E CONCATENAÇÃO DE CAMPOS + CAST (FOI PEDIDO PELO ANALISTA SE PODERIA UNIR MAIS 2 CAMPOS (BITOLAMOLA E MEDIDAS JUNTO DO CARACTERISTICAS PARA VIR JUNTO DO OBS CAMPO DO SOLNET)

```sql
SELECT
	CodigoPeca,
	CONCAT_WS(char(13), '  Caracteristicas:  ' + Caracteristicas, '  BitolaMola:  ' + cast(BitolaMola as VARCHAR(10)), '  Medidas:  ' + MEDIDAS)
FROM
	tbProdutos
--- os espaços no codigo é para as informações não virem coladas no solnet 
```
## 📅 01/09/25 –🟢 string_agg(CodigoPeca, ',')

1. string_agg(CodigoPeca, ',')
Função: Une (concatena) todos os valores do campo CodigoPeca em uma única string, separados por vírgula.
Exemplo:
Se existirem os códigos: A01, B02, C03
Resultado: 'A01,B02,C03'
2. CHARINDEX(',', string_agg(...))
Função: Procura a primeira ocorrência da vírgula na string agregada.
Exemplo:
'A01,B02,C03' → Retorna 4 (posição do primeiro ,)
3. COALESCE(NULLIF(CHARINDEX(',') - 1, -1), LEN(string_agg(...)))
Função:
CHARINDEX(',') - 1 → Pega a posição antes da primeira vírgula.
Se não encontrar vírgula, retorna -1.
NULLIF(..., -1) → Se for -1, vira NULL.
COALESCE(..., LEN(...)) → Se for NULL, pega o tamanho total da string.
Propósito:
Garante que se não houver vírgula (apenas um código), pega a string inteira.
4. SUBSTRING(string_agg(...), 1, ...)
Função:
Usa os cálculos acima para pegar só o primeiro código da lista agregada.
Exemplo:
'A01,B02,C03' → SUBSTRING(..., 1, 3) → 'A01'
'A99' → SUBSTRING(..., 1, 3) → 'A99'
5. DISTINCT ...
Elimina linhas totalmente idênticas desse resultado.
6. 1 TP_CODIGO
Apenas atribui o valor constante 1 na coluna TP_CODIGO.
Resumo
Esse SELECT retorna apenas o primeiro código de peça de cada agregação, ignorando os demais.
É útil quando você quer garantir que, para cada grupo, só o primeiro valor (por exemplo, para referencia única) seja migrado como ID_PRODUTO.
O valor TP_CODIGO é fixo, normalmente para indicar o tipo do código no padrão do Sol.NET.
Aplicações no Sol.NET_Conversao
Usado para garantir unicidade de produtos ao migrar dados, evitando duplicidade ou agrupando por referência.
Ajuda na normalização de dados legados, onde múltiplos códigos podem estar agregados.
Segue padrão ISO SQL, compatível com SQL Server (se precisar para Firebird, pode adaptar).

## 📅 01/09/25 –🟢 INVERTENDO A LOGICA DO IIF 

Ótima pergunta!
Vamos analisar tecnicamente a diferença entre os dois trechos, usando o contexto do Sol.NET_Conversao e padrões SQL/Delphi do projeto.
**Trecho 1 (original):**Pascal

`.AddCampo('INATIVO', 'IIF(CadastroExcluido = ''N'', 0, 1)')`

• **Lógica:**Se `CadastroExcluido = 'N'`, então `INATIVO = 0`Senão, `INATIVO = 1`
**Trecho 2 (após ajuste do supervisor):**Pascal

`.AddCampo('INATIVO', 'IIF(CadastroExcluido = ''S'', 1, 0)')`

• **Lógica:**Se `CadastroExcluido = 'S'`, então `INATIVO = 1`Senão, `INATIVO = 0`
**Por que o supervisor trocou?
1. Significado dos valores**
• Geralmente, em bancos de dados, o campo `CadastroExcluido` indica se o registro foi **excluído**:
    ◦ `'S'` = Sim, foi excluído
    ◦ `'N'` = Não, não foi excluído
**2. Regra de Negócio**
• **Registro excluído** deve ser **inativo** (`INATIVO = 1`)
• **Registro não excluído** deve ser **ativo** (`INATIVO = 0`)
**3. Problema do original**
• O primeiro trecho (`CadastroExcluido = 'N' → INATIVO = 0`) parece correto à primeira vista.
• Porém, se o campo está em branco (`''`) ou nulo, a expressão original (`CadastroExcluido = 'N'`) resultará em **falso**.
• Portanto, **qualquer valor diferente de `'N'`** (incluindo branco, nulo, etc.) será convertido como **INATIVO = 1** (inativo).
• Isso pode acabar marcando registros "em branco" como inativos, gerando registros faltando ou errados.
**4. Lógica do supervisor**
• Ao inverter para `CadastroExcluido = 'S'`, só registros marcados explicitamente como excluídos (`'S'`) serão convertidos como inativos.
• Qualquer outro valor (`'N'`, branco, nulo...) será considerado como ativo (`INATIVO = 0`).
• **É mais seguro**, pois evita que registros com valor em branco sejam tratados como excluídos/inativos por engano.
**Resumo prático**
• **Primeira expressão:**Qualquer valor diferente de `'N'` (inclusive branco ou nulo) = inativo.
• **Expressão ajustada:**Só `'S'` = inativo, todos os outros = ativo.
**Boa prática**
No contexto do Sol.NET_Conversao e Clean Code:
• Sempre prefira comparar pelo valor que indica **exclusão explícita** (`'S'`).
• Expressão do supervisor é **mais robusta** e evita erros causados por campos em branco ou nulos.
**Sugestão extra**
Se quiser garantir ainda mais (caso campo venha nulo ou branco por descuido do legado), pode usar:SQL

`IIF(CadastroExcluido = 'S', 1, 0)`

Ou, para SQL universal, usando CASE:SQL

`CASE WHEN CadastroExcluido = 'S' THEN 1 ELSE 0 END`

**Conclusão:**
O supervisor **corrigiu** para evitar que registros em branco ou nulos fossem marcados como inativos por engano.
Isso segue o padrão Clean Code e evita inconsistência na conversão!
Se quiser exemplos para outros campos ou recomendações de validação, só pedir!

****

- • **Lógica:**Se `CadastroExcluido = 'N'`, então `INATIVO = 0`Senão, `INATIVO = 1`
- • **Lógica:**Se `CadastroExcluido = 'S'`, então `INATIVO = 1`Senão, `INATIVO = 0`
- • Geralmente, em bancos de dados, o campo `CadastroExcluido` indica se o registro foi **excluído**:
    - ◦ `'S'` = Sim, foi excluído
    - ◦ `'N'` = Não, não foi excluído
- 
- 
- • **Registro excluído** deve ser **inativo** (`INATIVO = 1`)
- • **Registro não excluído** deve ser **ativo** (`INATIVO = 0`)
- • O primeiro trecho (`CadastroExcluido = 'N' → INATIVO = 0`) parece correto à primeira vista.
- • Porém, se o campo está em branco (`''`) ou nulo, a expressão original (`CadastroExcluido = 'N'`) resultará em **falso**.
- • Portanto, **qualquer valor diferente de `'N'`** (incluindo branco, nulo, etc.) será convertido como **INATIVO = 1** (inativo).
- • Isso pode acabar marcando registros "em branco" como inativos, gerando registros faltando ou errados.
- • Ao inverter para `CadastroExcluido = 'S'`, só registros marcados explicitamente como excluídos (`'S'`) serão convertidos como inativos.
- • Qualquer outro valor (`'N'`, branco, nulo...) será considerado como ativo (`INATIVO = 0`).
- • **É mais seguro**, pois evita que registros com valor em branco sejam tratados como excluídos/inativos por engano.
- • **Primeira expressão:**Qualquer valor diferente de `'N'` (inclusive branco ou nulo) = inativo.
- • **Expressão ajustada:**Só `'S'` = inativo, todos os outros = ativo.
- • Sempre prefira comparar pelo valor que indica **exclusão explícita** (`'S'`).
- • Expressão do supervisor é **mais robusta** e evita erros causados por campos em branco ou nulos.
