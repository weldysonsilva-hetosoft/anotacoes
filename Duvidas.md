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
