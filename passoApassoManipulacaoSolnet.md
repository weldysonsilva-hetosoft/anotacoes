# 📚 DOCUMENTAÇÃO COMPLETA: Implementação de "Confirmar Divergentes" na Conciliação Bancária

## 🎯 Visão Geral da Issue #7351

**Título:** [SOL.NET](http://sol.net/) - Conciliação Bancária - Aceitar Divergências PIX/Cartão

**Problema:** Valores de recebimentos via PIX e Cartão apresentam pequenas diferenças (arredondamentos) entre o sistema e o extrato bancário, impedindo a conciliação automática.

**Solução Implementada:** Botão "Confirmar Divergentes" que permite aceitar essas pequenas diferenças e ajustar automaticamente o valor no lado do sistema.

---

## 📖 Glossário de Termos Técnicos

| Termo | Significado |
| --- | --- |
| **DDL (Data Definition Language)** | Linguagem de definição de dados - usada para criar/alterar estruturas de banco de dados |
| **Builder** | Padrão de projeto que constrói objetos complexos passo a passo (no caso, comandos SQL) |
| **CDS (TClientDataSet)** | Componente Delphi que armazena dados em memória, como uma tabela temporária |
| **DAL (Data Access Layer)** | Camada de Acesso a Dados - responsável por comunicação com o banco |
| **Form** | Janela/Tela da aplicação (Interface do Usuário) |
| **Grid (DBGrid)** | Componente visual que exibe dados em formato de tabela |
| **DataSource** | Componente que conecta um CDS a componentes visuais |
| **Query/SQL** | Consulta ao banco de dados |
| **Transaction** | Conjunto de operações que devem ser executadas como uma unidade (tudo ou nada) |
| **Commit** | Confirmar/Salvar alterações no banco |
| **Rollback** | Desfazer alterações no banco |
| **Auditoria** | Registro de quem fez o quê e quando no sistema |

---

## 🏗️ PASSO 1: Criar Campo no Banco de Dados

### 📍 Arquivo: `uProcessosAtualizacaoPrincipal.pas`

### O que é o TDDLBuilder?

O **TDDLBuilder** é uma classe auxiliar que facilita a criação de comandos SQL para alterar estruturas de banco de dados. Em vez de escrever SQL manualmente, você usa métodos encadeados.

### Código Adicionado

```
builder := TDDLBuilder.Create(ddlAlter, 'CONCILIACAO_BANCARIA')
  .AddCampo('VL_FINAL_ORIGINAL', tcNumeric, 0, 15, 5);
Executar(TGUID.Create('{079011FA-9216-4F75-88F7-F52C4F129531}'), builder.ToString);
TryFreeAndNil(builder);

```

### Explicação Detalhada

### `TDDLBuilder.Create(ddlAlter, 'CONCILIACAO_BANCARIA')`

- **ddlAlter**: Tipo de operação (ALTER TABLE - modificar tabela existente)
- **'CONCILIACAO_BANCARIA'**: Nome da tabela que será alterada

### `.AddCampo('VL_FINAL_ORIGINAL', tcNumeric, 0, 15, 5)`

Adiciona um novo campo (coluna) na tabela:

| Parâmetro | Valor | Significado |
| --- | --- | --- |
| **Nome do Campo** | `VL_FINAL_ORIGINAL` | Nome da coluna que será criada |
| **Tipo** | `tcNumeric` | Tipo numérico (aceita casas decimais) |
| **Parâmetro 1** | `0` | Valor padrão inicial (zero) |
| **Parâmetro 2** | `15` | Precisão total (15 dígitos no total) |
| **Parâmetro 3** | `5` | Escala (5 casas decimais) |

**Exemplo de valor:** `9999999999.99999` (10 dígitos antes da vírgula + 5 depois)

### `Executar(TGUID.Create(...), builder.ToString)`

- **TGUID**: Identificador único para esta atualização
- Se este GUID já foi executado antes, o sistema pula (evita duplicação)
- `builder.ToString`: Converte o builder em comando SQL real

### SQL Gerado (aproximadamente)

```sql
ALTER TABLE CONCILIACAO_BANCARIA
ADD VL_FINAL_ORIGINAL NUMERIC(15,5) DEFAULT 0;

```

### Por que criar este campo?

Este campo armazenará o **valor original** do Controle Financeiro antes de ser alterado pela confirmação de divergência. Assim, quando desfizer a conciliação, o sistema pode restaurar o valor correto.

---

## 🔍 PASSO 2: Modificar a Consulta SQL (DAL)

### 📍 Arquivo: `uDalCaixaGeral.pas`

### O que é a DAL?

A **DAL (Data Access Layer)** é a camada responsável por todas as comunicações com o banco de dados. Ela contém funções que:

- Buscam dados (SELECT)
- Atualizam dados (UPDATE)
- Inserem dados (INSERT)
- Deletam dados (DELETE)

### Função Modificada: `SqlBuscarConciliacaoOFX`

### Antes (com comentários)

```
strSql.Append('SELECT                         ' + BR);
strSql.Append('  CF.*,                        ' + BR);
// strSql.Append('  0 AS OPCAO,                ' + BR);
// strSql.Append('  0 AS SEL_AUT               ' + BR);
// Modified by hetos 26/03/2020 10:43:06 SQLSERVER
strSql.Append(' CAST(0 AS INTEGER) AS OPCAO,  ' + BR);
strSql.Append(' CAST(0 AS INTEGER) AS SEL_AUT ' + BR);

```

### Depois (limpo)

```
strSql.Append('SELECT                         ' + BR);
strSql.Append('  CF.*,                        ' + BR);

strSql.Append(' CAST(0 AS INTEGER) AS OPCAO,  ' + BR);
strSql.Append(' CAST(0 AS INTEGER) AS SEL_AUT ' + BR);

```

### Por que Remover Comentários?

**Boa Prática - Clean Code:**

- Comentários desatualizados confundem mais do que ajudam
- Histórico de mudanças já está no Git/GitHub
- Código deve ser autoexplicativo
- Comentários devem explicar "por quê", não "o quê"

### Outras Limpezas na Mesma Função

```
// ❌ ANTES - Comentários desnecessários
// Entrada ou Saida
// if (numTpEntradaSaida < 2) then
// begin
//   strSql.Append('AND (CF.TP_ENTRADA_SAIDA = ' + numTpEntradaSaida.ToString + ') ');
// end;

// Modified by hetos 01/08/2019 10:01:05
if OculsarPrevisao = 1 then
  strSql.Append('AND (CF.TP_PREVISAO = 0 OR CF.TP_PREVISAO IS NULL) ');

// ✅ DEPOIS - Código limpo
if OculsarPrevisao = 1 then
  strSql.Append('AND (CF.TP_PREVISAO = 0 OR CF.TP_PREVISAO IS NULL) ');

```

### Correção de Indentação

### Antes (indentação inconsistente)

```
var cdsTaxa: TClientDataSet := TClientDataSet.Create(nil);
try
   try
    strSql.Clear;
    cdsTaxa.Data := Dados.QryOpenOle(...);
  except
    on E: Exception do
    begin
      raise Exception.Create(...);
    end;
  end;
finally
  TryFreeAndNil(cdsTaxa);
end;

```

### Depois (indentação correta)

```
var cdsTaxa: TClientDataSet := TClientDataSet.Create(nil);
try
  try
    strSql.Clear;
    cdsTaxa.Data := Dados.QryOpenOle(...);
  except
    on E: Exception do
    begin
      raise Exception.Create(...);
    end;
  end;
finally
  TryFreeAndNil(cdsTaxa);
end;

```

**Por quê?** Código alinhado é mais fácil de ler e entender a estrutura lógica.

---

## 🎨 PASSO 3: Criar o Botão na Interface (Form)

### 📍 Arquivo: `uFrmCaixaGeral.dfm` (Design do Form)

### O que foi adicionado?

```
object btnCBConfirmarDiv: TBitBtn
  AlignWithMargins = True
  Left = 171
  Top = 2
  Width = 134
  Height = 37
  Cursor = crHandPoint
  Caption = 'Confirmar Divergentes'
  TabOrder = 3
  OnClick = btnCBConfirmarDivClick
end

```

### Propriedades do Botão

| Propriedade | Valor | Significado |
| --- | --- | --- |
| **Name** | `btnCBConfirmarDiv` | Identificador único do componente |
| **Caption** | `'Confirmar Divergentes'` | Texto exibido no botão |
| **Cursor** | `crHandPoint` | Cursor vira "mãozinha" ao passar por cima |
| **OnClick** | `btnCBConfirmarDivClick` | Método executado ao clicar |
| **TabOrder** | `3` | Ordem de navegação pelo Tab |

### Prefixos de Nomenclatura

| Prefixo | Tipo de Componente |
| --- | --- |
| **btn** | Botão (Button/BitBtn) |
| **txt** | Campo de texto (Edit) |
| **cds** | ClientDataSet |
| **dbg** | DBGrid (Grade de dados) |
| **pnl** | Panel (Painel) |
| **lbl** | Label (Rótulo) |

---

## 💻 PASSO 4: Implementar a Lógica do Botão

### 📍 Arquivo: `uFrmCaixaGeral.pas`

### Declaração do Evento

```
type
  TFrmCaixaGeral = class(TFrmHeranca)
    // ... outros componentes ...
    btnCBConfirmarDiv: TBitBtn;  // ← Declaração do botão

    // ... outros procedures ...
    procedure btnCBConfirmarDivClick(Sender: TObject);  // ← Declaração do evento
  end;

```

### Implementação Completa com Comentários Explicativos

```
procedure TFrmCaixaGeral.btnCBConfirmarDivClick(Sender: TObject);
begin
  inherited; // ← Chama método da classe pai (se existir)

  // ═══════════════════════════════════════════════════════════
  // VALIDAÇÃO 1: Verificar se há dados para processar
  // ═══════════════════════════════════════════════════════════
  if (cdsOFX.EstaVazio) or (cdsOP.EstaVazio) then
    Exit; // Sai do procedimento se não houver dados

  // ═══════════════════════════════════════════════════════════
  // CRIAÇÃO DE DATASETS TEMPORÁRIOS
  // ═══════════════════════════════════════════════════════════
  // Criamos cópias temporárias para não mexer nos dados originais
  var cdsRegBancario: TClientDataSet := TClientDataSet.Create(Self);
  var cdsRegCaixaGeral: TClientDataSet := TClientDataSet.Create(Self);
  try
    // Copiar dados do extrato bancário (OFX)
    cdsRegBancario.Data := cdsOFX.Data;
    cdsRegBancario.Filtrar('OPCAO = 1'); // ← Filtra apenas registros selecionados

    // Copiar dados do sistema (Controle Financeiro)
    cdsRegCaixaGeral.Data := cdsOP.Data;
    cdsRegCaixaGeral.Filtrar('OPCAO = 1'); // ← Filtra apenas registros selecionados

    // ═══════════════════════════════════════════════════════════
    // VALIDAÇÃO 2: Permitir apenas 1 registro de cada lado
    // ═══════════════════════════════════════════════════════════
    if (cdsRegBancario.RecordCount > 1) or (cdsRegCaixaGeral.RecordCount > 1) then
    begin
      Geral.Men('Não é possível confirmar registros múltiplos com divergência.' + BR +
                'Selecione apenas 1 registo em cada lado!');
      Exit;
    end;

    // ═══════════════════════════════════════════════════════════
    // VALIDAÇÃO 3: Verificar se registros já estão conciliados
    // ═══════════════════════════════════════════════════════════
    if (cdsRegCaixaGeral.FieldByName('TP_CONCILIADO').AsInteger = 1) then
    begin
      Geral.Men('Não Perminito Selecionar um Registro Já "Conciliado" - Caixa Geal');
      Exit;
    end;

    // ═══════════════════════════════════════════════════════════
    // VALIDAÇÃO 4: Verificar se pagamento foi compensado
    // ═══════════════════════════════════════════════════════════
    if not(cdsRegCaixaGeral.FieldByName('TP_STATUS').AsInteger > 0) then
    begin
      Geral.Men('Não Perminito Selecionar um Registro "Não Compensado" - Caixa Geal');
      Exit;
    end;

    // ═══════════════════════════════════════════════════════════
    // VALIDAÇÃO 5: Verificar registro bancário
    // ═══════════════════════════════════════════════════════════
    if (cdsRegBancario.FieldByName('TP_CONCILIADO').AsInteger = 1) then
    begin
      Geral.Men('Não Perminito Selecionar um Registro Já "Conciliado" - Banco');
      Exit;
    end;

    if (cdsRegBancario.FieldByName('TP_PREVISAO').AsInteger = 1) then
    begin
      Geral.Men('Não Perminito Selecionar um Registro "Previsão" - Banco');
      Exit;
    end;

    // ═══════════════════════════════════════════════════════════
    // CAPTURAR VALORES E CALCULAR DIFERENÇA
    // ═══════════════════════════════════════════════════════════
    var IdControleFinanceiro: Double := cdsRegCaixaGeral.FieldByName('ID_CONTROLE_FINANCEIRO').AsFloat;
    var IdConciliacaoBancaria: Double := cdsRegBancario.FieldByName('ID_CONCILIACAO_BANCARIA').AsFloat;
    var ValorRegBancario: Double := cdsRegBancario.FieldByName('VALOR').AsFloat;
    var ValorRegCaixa: Double := cdsRegCaixaGeral.FieldByName('VALOR').AsFloat;
    var DiferencaValor: Double := (ValorRegCaixa - ValorRegBancario);

    // ═══════════════════════════════════════════════════════════
    // CONFIRMAÇÃO DO USUÁRIO
    // ═══════════════════════════════════════════════════════════
    if not Geral.MenConfirmar(
      'Os registro tem uma diferença de Valor (' +
      Geral.FormatarValor(DiferencaValor, 2, False, True) + ')!' + BR +
      'Deseja confirmar divergência?'
    ) then
    begin
      Exit;
    end;

    // ═══════════════════════════════════════════════════════════
    // INICIAR TRANSAÇÃO NO BANCO DE DADOS
    // ═══════════════════════════════════════════════════════════
    Dados.InicairTransaction;
    try
      // Gerar ID único para vincular os dois registros
      var idVinculoInd: Double := Dados.IdGeral('CONCILIACAO_BANCARIA_VINC');

      // ═══════════════════════════════════════════════════════════
      // ATUALIZAR CONTROLE FINANCEIRO (Sistema)
      // ═══════════════════════════════════════════════════════════
      Dados.QryExecutar(
        'UPDATE CONTROLE_FINANCEIRO ' +
        'SET TP_CONCILIADO = 2, ' +          // ← Marca como conciliado com divergência
        '    ID_VINCULO_CB = :ID_VINCULO_CB, ' +
        '    VALOR = :VALOR ' +               // ← ALTERA VALOR PARA O DO BANCO
        'WHERE ID_CONTROLE_FINANCEIRO = :ID_CONTROLE_FINANCEIRO',
        varArrayOf([idVinculoInd, ValorRegBancario, IdControleFinanceiro])
      );

      // ═══════════════════════════════════════════════════════════
      // ATUALIZAR CONCILIAÇÃO BANCÁRIA
      // ═══════════════════════════════════════════════════════════
      Dados.QryExecutar(
        'UPDATE CONCILIACAO_BANCARIA ' +
        'SET TP_CONCILIADO = 2, ' +
        '    ID_VINCULO_CB = :ID_VINCULO_CB, ' +
        '    VL_FINAL_ORIGINAL = :VL_FINAL_ORIGINAL ' +  // ← GUARDA VALOR ORIGINAL
        'WHERE ID_CONCILIACAO_BANCARIA = :ID_CONCILIACAO_BANCARIA',
        varArrayOf([idVinculoInd, ValorRegCaixa, IdConciliacaoBancaria])
      );

      // ═══════════════════════════════════════════════════════════
      // GRAVAR AUDITORIA
      // ═══════════════════════════════════════════════════════════
      varDadosOriginalAuditoria.Clear;
      varDadosModificadosAuditoria.Clear;

      varDadosOriginalAuditoria.Append('Conciliação bancaria com divergência' + BR);
      varDadosOriginalAuditoria.Append('Valor : ' + Geral.ConverteParaString(ValorRegCaixa) + BR);

      varDadosModificadosAuditoria.Append('Conciliação bancaria com divergência' + BR);
      varDadosModificadosAuditoria.Append('Valor : ' + Geral.ConverteParaString(ValorRegBancario) + BR);

      GravarAuditoria(Self, nil, 'CONTROLE_FINANCEIRO', 'E', IdControleFinanceiro);

      // ═══════════════════════════════════════════════════════════
      // CONFIRMAR ALTERAÇÕES NO BANCO
      // ═══════════════════════════════════════════════════════════
      Dados.Commit;

      // ═══════════════════════════════════════════════════════════
      // ATUALIZAR VISUAL (CDS em memória)
      // ═══════════════════════════════════════════════════════════
      cdsOP.Localizar('ID_CONTROLE_FINANCEIRO', IdControleFinanceiro);
      cdsOP.Edit;
      cdsOP.FieldByName('VALOR').AsFloat := ValorRegBancario;
      cdsOP.FieldByName('TP_CONCILIADO').AsFloat := 1;
      cdsOP.Post;

      cdsOFX.Localizar('ID_CONCILIACAO_BANCARIA', IdConciliacaoBancaria);
      cdsOFX.Edit;
      cdsOFX.FieldByName('TP_CONCILIADO').AsFloat := 1;
      cdsOFX.Post;

    except
      // ═══════════════════════════════════════════════════════════
      // EM CASO DE ERRO: DESFAZER TUDO
      // ═══════════════════════════════════════════════════════════
      Dados.Rollback;
    end;

  finally
    // ═══════════════════════════════════════════════════════════
    // LIBERAR MEMÓRIA DOS DATASETS TEMPORÁRIOS
    // ═══════════════════════════════════════════════════════════
    TryFreeAndNil(cdsRegBancario, cdsRegCaixaGeral);
  end;
end;

```

---

## 🔄 Como Descobrir qual CDS Usar?

### Método 1: Verificar o DataSource do Grid

1. **Clique no Grid** (dbgOFX ou dbgOP)
2. **Procure a propriedade `DataSource`**
3. O DataSource aponta para um CDS

```
dbgOFX → DataSource: dsOFX → Dataset: cdsOFX

```

### Método 2: Buscar por `.Data` no Código

Procure no código onde o CDS recebe dados:

```
// Exemplo encontrado
cdsOFX.Data := DalCaixaGeral.SqlBuscarConciliacaoOFX(
  parametrosOFX,
  cbxVisCampoPesquisadoConBanc,
  cbxVisCondicaoConBanc.AsStringValor,
  txtVisBuscarConBanc
);

```

**Ao clicar** em `SqlBuscarConciliacaoOFX`, você é levado à consulta SQL!

### Método 3: Verificar Declaração do CDS

No início do Form:

```
private
  cdsOP: TClientDataSet;   // ← Dados do sistema
  cdsOFX: TClientDataSet;  // ← Dados do banco

```

---

## 📊 Fluxo Completo da Funcionalidade

```
┌─────────────────────────────────────────────────────────────┐
│ 1. USUÁRIO SELECIONA REGISTROS DIVERGENTES                 │
│    ✓ 1 registro do sistema (cdsOP)                         │
│    ✓ 1 registro do banco (cdsOFX)                          │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. CLICA EM "CONFIRMAR DIVERGENTES"                        │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. VALIDAÇÕES                                               │
│    ✓ Há dados?                                              │
│    ✓ Só 1 registro de cada lado?                            │
│    ✓ Não estão conciliados?                                 │
│    ✓ Pagamento compensado?                                  │
│    ✓ Não é previsão?                                        │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. CALCULAR DIFERENÇA E CONFIRMAR COM USUÁRIO               │
│    "Diferença de R$ 0,05. Confirmar?"                       │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. ATUALIZAR BANCO DE DADOS (Transaction)                   │
│    ┌─────────────────────────────────────────────┐          │
│    │ CONTROLE_FINANCEIRO:                        │          │
│    │  - TP_CONCILIADO = 2 (divergência)         │          │
│    │  - VALOR = valor do banco (ajustado)        │          │
│    │  - ID_VINCULO_CB = ID único gerado          │          │
│    └─────────────────────────────────────────────┘          │
│    ┌─────────────────────────────────────────────┐          │
│    │ CONCILIACAO_BANCARIA:                       │          │
│    │  - TP_CONCILIADO = 2                        │          │
│    │  - ID_VINCULO_CB = mesmo ID                 │          │
│    │  - VL_FINAL_ORIGINAL = valor original ★     │          │
│    └─────────────────────────────────────────────┘          │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 6. GRAVAR AUDITORIA                                         │
│    Antes: R$ 100,05                                         │
│    Depois: R$ 100,00                                        │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 7. COMMIT (Confirmar alterações)                            │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 8. ATUALIZAR VISUAL (CDS em memória)                        │
│    Grid mostra registros como conciliados                   │
└─────────────────────────────────────────────────────────────┘

```

---

## 🔑 Conceitos Importantes

### 1. Por que usar `try...finally`?

```
var cds: TClientDataSet := TClientDataSet.Create(nil);
try
  // ... usar cds ...
finally
  TryFreeAndNil(cds); // ← SEMPRE EXECUTA, mesmo com erro
end;

```

**Motivo:** Evitar **vazamento de memória** (memory leak). Se não liberar, o objeto fica na memória para sempre.

### 2. Por que usar `try...except` dentro de Transaction?

```
Dados.InicairTransaction;
try
  // Operações no banco
  Dados.Commit;
except
  Dados.Rollback; // ← Desfaz TUDO em caso de erro
end;

```

**Motivo:** **Atomicidade** - ou todas as operações funcionam, ou nenhuma funciona.

### 3. O que são os `varArrayOf`?

```
Dados.QryExecutar(
  'UPDATE TABELA SET CAMPO = :VALOR WHERE ID = :ID',
  varArrayOf([123.45, 789])  // ← Substitui :VALOR e :ID
);

```

**Motivo:** Evitar **SQL Injection** e facilitar passagem de parâmetros.

### 4. Por que atualizar CDS após Commit?

```
Dados.Commit; // ← Salva no banco

cdsOP.Edit;   // ← Atualiza visual
cdsOP.FieldByName('VALOR').AsFloat := NovoValor;
cdsOP.Post;

```

**Motivo:** O CDS é uma **cópia em memória** dos dados. Se não atualizar, o usuário vê dados desatualizados.

---

## ⚠️ Próximos Passos (Issue #7105)

Como mencionado anteriormente, **falta implementar a restauração do valor** no botão "Desfazer":

```
// NO BOTÃO DESFAZER:
// 1. Buscar VL_FINAL_ORIGINAL da CONCILIACAO_BANCARIA
// 2. Restaurar VALOR original no CONTROLE_FINANCEIRO
// 3. Limpar VL_FINAL_ORIGINAL (deixar NULL)

```

---

## 📝 Checklist de Boas Práticas Aplicadas

- ✅ **Campo criado com DDLBuilder** (evita SQL manual)
- ✅ **GUID único** para controle de versão do banco
- ✅ **Código limpo** (comentários antigos removidos)
- ✅ **Indentação consistente**
- ✅ **Validações robustas** (múltiplas verificações)
- ✅ **Transações atômicas** (commit/rollback)
- ✅ **Auditoria completa** (antes/depois registrados)
- ✅ **Liberação de memória** (TryFreeAndNil)
- ✅ **SQL parametrizado** (segurança contra injection)
- ✅ **Atualização visual** (CDS sincronizado)

---

## 🎓 Resumo para Iniciantes

### O que a Issue Pedia?

"Permitir confirmar conciliações bancárias mesmo quando há pequenas diferenças de valor (arredondamentos do PIX/Cartão)"

### O que foi Feito?

1. **Criado campo `VL_FINAL_ORIGINAL`** → Guarda valor antes de alterar
2. **Limpado código SQL** → Removidos comentários antigos
3. **Criado botão visual** → Interface para usuário
4. **Implementada lógica completa** → Validações + Banco + Auditoria

### Fluxo Resumido

```
Usuário → Seleciona 2 registros com diferença pequena
       ↓
Sistema → Valida tudo
       ↓
Usuário → Confirma diferença
       ↓
Sistema → Ajusta valor do sistema = valor do banco
       ↓
Sistema → Guarda valor original para poder desfazer depois
       ↓
Concluído → Registros aparecem conciliados na tela

```

---

**Documentação criada em:** 28/01/2025

**Autor:** Copilot

**Baseado na Issue:** #7351

**Pull Request:** #7419

**Versão [Sol.NET](http://sol.net/):** 12.2
