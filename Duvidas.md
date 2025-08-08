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

