# 📋 GUIA DE IMPLEMENTAÇÃO - Issue #7563

**Issue:** 246926: SOL.NET - ERROS - LANCAMENTO RH AO INSERIR UM DÉBITO COM PORTADOR 'A VISTA' AVISO DE 'NÃO PERMITIDO'

**Branch:** `7563-246926-solnet---erros---lancamento-rhao-inserir-um-debito-com-portador-a-vista-aviso-de-n`

---

## 🎯 OBJETIVO

Filtrar os comboboxes de **Portadores** e **Tipos de Documentos** baseado no **Tipo de Operação** selecionado (Crédito/Débito/Saldo), tanto no **Cadastro RH** quanto no **Lançamento RH**.

**Problema:** O sistema estava listando TODOS os portadores e tipos de documentos, independente da operação selecionada. Isso permitia que o usuário selecionasse um portador incompatível (ex: Boleto - Entrada quando a operação era Débito - Saída), gerando erro "Não Permitido" na validação.

**Solução:** Filtrar dinamicamente os comboboxes para mostrar apenas portadores e documentos compatíveis com a operação selecionada.

---

## 📂 ARQUIVO 1: `uFrmCadastroRH.pas`

### **PASSO 1: Adicionar declaração dos métodos (Seção Protected)**

📍 **Localização:** Por volta da linha 280, antes de `public`

**Encontre:**
```pascal
    procedure QuantidadeRegistroMassa;
    function LancamentoEmMassaRH: Boolean;
    // FRIMFORMULARIO
  public
    { Public declarations }
```

**Substitua por:**
```pascal
    procedure QuantidadeRegistroMassa;
    function LancamentoEmMassaRH: Boolean;
    
    procedure PreencherPortadoresRH;
    procedure PreencherTiposDocumentosRH;
    // FRIMFORMULARIO
  public
    { Public declarations }
```

---

### **PASSO 2: Modificar o CarregarCombos**

📍 **Localização:** Por volta da linha 1733

**ANTES:**
```pascal
procedure TFrmCadastroRH.CarregarCombos;
begin
  inherited;

  cds.Limpar(cdsAux1);
  cdsAux1.Data := Dados.SqlBuscarEmpresas;
  cbxVisIdEmpresaCad.Preencher(cdsAux1, [0, 1], -1, True);
  cbxVisIdEmpresaCad2.Preencher(cdsAux1, [0, 1], -1, True);
  cbxEmpresaRH.Preencher(cdsAux1, [0, 1]);

  cds.Limpar(cdsSistema);
  cdsSistema.Data := Dados.BuscarGeral('TIPOS_DOCUMENTOS', 'DESCRICAO');
  cbxTipoDocumentoRH.Preencher(cdsSistema, ['ID_TIPO_DOCUMENTO', 'DESCRICAO']);

  cds.Limpar(cdsSistema);
  cdsSistema.Data := Dados.BuscarGeral('PORTADORES', 'DESCRICAO');
  cbxPortadorRH.Preencher(cdsSistema, ['ID_PORTADOR', 'DESCRICAO']);
end;
```

**DEPOIS:**
```pascal
procedure TFrmCadastroRH.CarregarCombos;
begin
  inherited;

  cds.Limpar(cdsAux1);
  cdsAux1.Data := Dados.SqlBuscarEmpresas;
  cbxVisIdEmpresaCad.Preencher(cdsAux1, [0, 1], -1, True);
  cbxVisIdEmpresaCad2.Preencher(cdsAux1, [0, 1], -1, True);
  cbxEmpresaRH.Preencher(cdsAux1, [0, 1]);

  PreencherTiposDocumentosRH;
  PreencherPortadoresRH;
  cbxTipoOperacaoRH.OnChange := cbxTipoOperacaoRHChange;
end;
```

---

### **PASSO 3: Adicionar os NOVOS métodos**

📍 **Localização:** Logo após o método `CarregarCombos`

**Adicione estes 3 métodos:**

```pascal
procedure TFrmCadastroRH.PreencherTiposDocumentosRH;
var TipoES: Integer;
begin
  TipoES := 2; // default all
  if cbxTipoOperacaoRH.AsInteger = 0 then
    TipoES := 0 // Entrada (Crédito)
  else if cbxTipoOperacaoRH.AsInteger = 1 then
    TipoES := 1 // Saida (Débito)
  else if cbxTipoOperacaoRH.AsInteger = 2 then
    TipoES := 1; // Saldo (Débito)
  
  cds.Limpar(cdsSistema);
  if TipoES = 2 then
    cdsSistema.Data := Dados.BuscarGeral('TIPOS_DOCUMENTOS', 'DESCRICAO')
  else
  begin
    cdsSistema.Data := Dados.QryOpenOle(
      'SELECT * FROM TIPOS_DOCUMENTOS WHERE (TP_ENTRADA_SAIDA = ' + TipoES.ToString + ' OR TP_ENTRADA_SAIDA = 2) ORDER BY DESCRICAO');
  end;
  cbxTipoDocumentoRH.Preencher(cdsSistema, ['ID_TIPO_DOCUMENTO', 'DESCRICAO']);
end;

procedure TFrmCadastroRH.PreencherPortadoresRH;
var TipoES: Integer;
begin
  TipoES := 2; // default all
  if cbxTipoOperacaoRH.AsInteger = 0 then
    TipoES := 0 // Entrada (Crédito)
  else if cbxTipoOperacaoRH.AsInteger = 1 then
    TipoES := 1 // Saida (Débito)
  else if cbxTipoOperacaoRH.AsInteger = 2 then
    TipoES := 1; // Saldo (Débito)
  
  cds.Limpar(cdsSistema);
  cdsSistema.Data := DalDiversos.SqlBuscarPortadoresComboBox(TipoES, cbxEmpresaRH.AsFloat);
  cbxPortadorRH.Preencher(cdsSistema, ['ID_PORTADOR', 'DESCRICAO']);
end;

procedure TFrmCadastroRH.cbxTipoOperacaoRHChange(Sender: TObject);
begin
  PreencherTiposDocumentosRH;
  PreencherPortadoresRH;
end;
```

---

### **PASSO 4: Atualizar cbxTipoOperacaoRHCloseUp**

📍 **Localização:** Por volta da linha 1483

**ANTES:**
```pascal
procedure TFrmCadastroRH.cbxTipoOperacaoRHCloseUp(Sender: TObject);
begin
  inherited;

  if cbxTipoOperacaoRH.AsInteger = 2 then
  begin
    txtValorRH.AsFloat := 0;
    cbxFechamentoRH.MostrarValor(1);
    ckbCriarRH.MostrarValor(1);
  end;

end;
```

**DEPOIS:**
```pascal
procedure TFrmCadastroRH.cbxTipoOperacaoRHCloseUp(Sender: TObject);
begin
  inherited;

  if cbxTipoOperacaoRH.AsInteger = 2 then
  begin
    txtValorRH.AsFloat := 0;
    cbxFechamentoRH.MostrarValor(1);
    ckbCriarRH.MostrarValor(1);
  end;
  
  cbxTipoOperacaoRHChange(Sender);
end;
```

---

### **PASSO 5: Atualizar cbxTipoOperacaoRHKeyUp**

📍 **Localização:** Por volta da linha 1496

**ANTES:**
```pascal
procedure TFrmCadastroRH.cbxTipoOperacaoRHKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if cbxTipoOperacaoRH.AsInteger = 2 then
  begin
    txtValorRH.AsFloat := 0;
    cbxFechamentoRH.MostrarValor(1);
    ckbCriarRH.MostrarValor(1);
  end;
end;
```

**DEPOIS:**
```pascal
procedure TFrmCadastroRH.cbxTipoOperacaoRHKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if cbxTipoOperacaoRH.AsInteger = 2 then
  begin
    txtValorRH.AsFloat := 0;
    cbxFechamentoRH.MostrarValor(1);
    ckbCriarRH.MostrarValor(1);
  end;
  
  cbxTipoOperacaoRHChange(Sender);
end;
```

---

## 📂 ARQUIVO 2: `uFrmCadastroLancamentoRH.pas`

### **PASSO 1: Adicionar declaração dos métodos (Seção Protected)**

📍 **Localização:** Por volta da linha 368, antes de `public`

**Encontre:**
```pascal
    procedure LojaContaModificar;
    procedure QuantidadeRegistroMassa;
    function LancamentoEmMassaRH: Boolean;
  public
    { Public declarations }
```

**Substitua por:**
```pascal
    procedure LojaContaModificar;
    procedure QuantidadeRegistroMassa;
    function LancamentoEmMassaRH: Boolean;
    
    procedure PreencherPortadoresRH;
    procedure PreencherTiposDocumentosRH;
  public
    { Public declarations }
```

---

### **PASSO 2: Modificar o CarregarCombos**

📍 **Localização:** Por volta da linha 1957

**ANTES:**
```pascal
procedure TFrmCadastroLancamentoRH.CarregarCombos;
begin
  inherited;
  // cdsAux1.Data := Dados.BuscarGeral('TABELA', 'DESCRICAO');
  // cbxVisEmpresas.Preencher(cdsAux1, [0, 1]);

  // Buscar campos antes de fazer a buscar
  cds.Limpar(cdsAux1);
  cdsAux1.Data := Dados.SqlBuscarEmpresas;
  cbxVisIdEmpresaCad.Preencher(cdsAux1, [0, 1], -1, True);
  cbxVisIdEmpresaCad2.Preencher(cdsAux1, [0, 1], -1, True);
  cbxEmpresaRH.Preencher(cdsAux1, [0, 1]);

  cds.Limpar(cdsSistema);
  cdsSistema.Data := Dados.BuscarGeral('TIPOS_DOCUMENTOS', 'DESCRICAO');
  cbxTipoDocumentoRH.Preencher(cdsSistema, ['ID_TIPO_DOCUMENTO', 'DESCRICAO']);

  cds.Limpar(cdsSistema);
  cdsSistema.Data := Dados.BuscarGeral('PORTADORES', 'DESCRICAO');
  cbxPortadorRH.Preencher(cdsSistema, ['ID_PORTADOR', 'DESCRICAO']);

end;
```

**DEPOIS:**
```pascal
procedure TFrmCadastroLancamentoRH.CarregarCombos;
begin
  inherited;
  // cdsAux1.Data := Dados.BuscarGeral('TABELA', 'DESCRICAO');
  // cbxVisEmpresas.Preencher(cdsAux1, [0, 1]);

  // Buscar campos antes de fazer a buscar
  cds.Limpar(cdsAux1);
  cdsAux1.Data := Dados.SqlBuscarEmpresas;
  cbxVisIdEmpresaCad.Preencher(cdsAux1, [0, 1], -1, True);
  cbxVisIdEmpresaCad2.Preencher(cdsAux1, [0, 1], -1, True);
  cbxEmpresaRH.Preencher(cdsAux1, [0, 1]);

  PreencherTiposDocumentosRH;
  PreencherPortadoresRH;
  cbxTipoOperacaoRH.OnChange := cbxTipoOperacaoRHChange;
end;
```

---

### **PASSO 3: Adicionar os NOVOS métodos**

📍 **Localização:** Logo após o método `CarregarCombos`

**Adicione estes 3 métodos:**

```pascal
procedure TFrmCadastroLancamentoRH.PreencherTiposDocumentosRH;
var TipoES: Integer;
begin
  TipoES := 2; // default all
  if cbxTipoOperacaoRH.AsInteger = 0 then
    TipoES := 0 // Entrada (Crédito)
  else if cbxTipoOperacaoRH.AsInteger = 1 then
    TipoES := 1 // Saida (Débito)
  else if cbxTipoOperacaoRH.AsInteger = 2 then
    TipoES := 1; // Saldo (Débito)
  
  cds.Limpar(cdsSistema);
  if TipoES = 2 then
    cdsSistema.Data := Dados.BuscarGeral('TIPOS_DOCUMENTOS', 'DESCRICAO')
  else
  begin
    cdsSistema.Data := Dados.QryOpenOle(
      'SELECT * FROM TIPOS_DOCUMENTOS WHERE (TP_ENTRADA_SAIDA = ' + TipoES.ToString + ' OR TP_ENTRADA_SAIDA = 2) ORDER BY DESCRICAO');
  end;
  cbxTipoDocumentoRH.Preencher(cdsSistema, ['ID_TIPO_DOCUMENTO', 'DESCRICAO']);
end;

procedure TFrmCadastroLancamentoRH.PreencherPortadoresRH;
var TipoES: Integer;
begin
  TipoES := 2; // default all
  if cbxTipoOperacaoRH.AsInteger = 0 then
    TipoES := 0 // Entrada (Crédito)
  else if cbxTipoOperacaoRH.AsInteger = 1 then
    TipoES := 1 // Saida (Débito)
  else if cbxTipoOperacaoRH.AsInteger = 2 then
    TipoES := 1; // Saldo (Débito)
  
  cds.Limpar(cdsSistema);
  cdsSistema.Data := DalDiversos.SqlBuscarPortadoresComboBox(TipoES, cbxEmpresaRH.AsFloat);
  cbxPortadorRH.Preencher(cdsSistema, ['ID_PORTADOR', 'DESCRICAO']);
end;

procedure TFrmCadastroLancamentoRH.cbxTipoOperacaoRHChange(Sender: TObject);
begin
  PreencherTiposDocumentosRH;
  PreencherPortadoresRH;
end;
```

---

## ✅ CHECKLIST DOS TÓPICOS DO SUPERVISOR

| Tópico | Status | Onde está implementado |
|--------|--------|----------------------|
| **CarregarCombos** | ✅ | Modificado para chamar os novos métodos de preenchimento |
| **WHERE no SELECT de portadores** | ✅ | Usa `DalDiversos.SqlBuscarPortadoresComboBox(TipoES, ...)` que já tem o WHERE correto |
| **cbxOperacaoOnExit → operação** | ✅ | Implementado via `cbxTipoOperacaoRHChange` chamado no CloseUp e KeyUp |
| **Corrigir combobox de portadores** | ✅ | Método `PreencherPortadoresRH` filtra por tipo de operação |
| **Corrigir combobox de tipo de documentos** | ✅ | Método `PreencherTiposDocumentosRH` filtra por tipo de operação |

---

## 🔍 LÓGICA DE FUNCIONAMENTO

### Mapeamento de Operações:

| Operação Selecionada | AsInteger | TipoES | Filtro Aplicado |
|---------------------|-----------|--------|-----------------|
| *(vazio)* | -1 | 2 | Mostra TODOS |
| **+ CRÉDITO** | 0 | 0 | Mostra ENTRADA ou AMBOS |
| **- DÉBITO** | 1 | 1 | Mostra SAÍDA ou AMBOS |
| **- SALDO** | 2 | 1 | Mostra SAÍDA ou AMBOS |

### Valores no Banco de Dados:

**Tabela PORTADORES / TIPOS_DOCUMENTOS - Campo TP_ENTRADA_SAIDA:**
- `0` = Entrada
- `1` = Saída  
- `2` = Ambos

### Exemplos Práticos:

**Cenário 1:** Usuário seleciona **"+ CRÉDITO"**
- TipoES = 0 (Entrada)
- SQL: `WHERE (TP_ENTRADA_SAIDA = 0 OR TP_ENTRADA_SAIDA = 2)`
- Resultado: Mostra apenas portadores marcados como "Entrada" ou "Ambos"
- ✅ Não mostrará "Compra" (que é Saída)

**Cenário 2:** Usuário seleciona **"- DÉBITO"**
- TipoES = 1 (Saída)
- SQL: `WHERE (TP_ENTRADA_SAIDA = 1 OR TP_ENTRADA_SAIDA = 2)`
- Resultado: Mostra apenas portadores marcados como "Saída" ou "Ambos"
- ✅ Não mostrará "Boleto" (que é Entrada)

**Cenário 3:** Usuário não selecionou nada
- TipoES = 2 (Todos)
- SQL: Sem WHERE
- Resultado: Mostra TODOS os portadores/documentos

---

## 🎬 FLUXO DE EXECUÇÃO

1. **Ao abrir o formulário:**
   - `CarregarCombos` é chamado
   - `PreencherTiposDocumentosRH` executa com TipoES = 2 (mostra todos)
   - `PreencherPortadoresRH` executa com TipoES = 2 (mostra todos)
   - Atribui evento `OnChange` ao combo de operação

2. **Quando usuário altera a operação:**
   - `cbxTipoOperacaoRHCloseUp` ou `cbxTipoOperacaoRHKeyUp` é disparado
   - Chama `cbxTipoOperacaoRHChange`
   - `PreencherTiposDocumentosRH` recarrega com filtro
   - `PreencherPortadoresRH` recarrega com filtro
   - Usuário vê apenas opções compatíveis

3. **Ao salvar:**
   - Validação existente continua verificando compatibilidade
   - Mas agora o usuário NÃO consegue selecionar opções incompatíveis
   - ✅ Problema resolvido preventivamente!

---

## 💡 OBSERVAÇÕES IMPORTANTES

### Por que usar `DalDiversos.SqlBuscarPortadoresComboBox`?

Esta função JÁ EXISTE no sistema e foi criada especificamente para filtrar portadores por tipo de entrada/saída. Ela:

1. ✅ Já tem o WHERE correto implementado
2. ✅ Já considera o filtro por empresa
3. ✅ Já é usada no módulo ContasPR (que funciona corretamente)
4. ✅ Retorna os portadores ordenados por descrição

### Tipos de Documentos

Para tipos de documentos, fazemos query direta pois é mais simples:
```sql
SELECT * FROM TIPOS_DOCUMENTOS 
WHERE (TP_ENTRADA_SAIDA = [TipoES] OR TP_ENTRADA_SAIDA = 2) 
ORDER BY DESCRICAO
```

### Evento OnChange vs OnExit

Usamos **OnChange** porque:
- É disparado assim que o valor muda
- Funciona tanto com mouse (CloseUp) quanto teclado (KeyUp)
- Garante sincronização imediata dos combos dependentes

---

## 🧪 COMO TESTAR

### Teste 1: Cadastro RH - Operação Crédito
1. Abra **Cadastro RH** (Pessoa_RH)
2. Clique em "Novo"
3. Selecione Operação: **+ CRÉDITO**
4. ✅ Verificar: Combo Portador mostra apenas Entrada/Ambos
5. ✅ Verificar: Combo Tipo Documento mostra apenas Entrada/Ambos
6. ❌ Não deve aparecer: Portadores de Saída (ex: Compra)

### Teste 2: Cadastro RH - Operação Débito
1. Abra **Cadastro RH**
2. Clique em "Novo"
3. Selecione Operação: **- DÉBITO**
4. ✅ Verificar: Combo Portador mostra apenas Saída/Ambos
5. ✅ Verificar: Combo Tipo Documento mostra apenas Saída/Ambos
6. ❌ Não deve aparecer: Portadores de Entrada (ex: Boleto)

### Teste 3: Lançamento RH - Mudança de Operação
1. Abra **Lançamento RH**
2. Clique em "Novo"
3. Selecione Operação: **+ CRÉDITO**
4. Observe os portadores disponíveis
5. Mude para: **- DÉBITO**
6. ✅ Verificar: Lista de portadores foi atualizada automaticamente
7. ✅ Verificar: Portadores incompatíveis sumiram da lista

### Teste 4: Operação Saldo
1. Abra **Cadastro RH**
2. Selecione Operação: **- SALDO**
3. ✅ Verificar: Comportamento igual ao DÉBITO (TipoES = 1)

---

## 🐛 TROUBLESHOOTING

### Problema: "Access Violation" ao mudar operação
**Causa:** Evento OnChange disparando antes do combo estar pronto  
**Solução:** Verificar se cbxTipoOperacaoRH está criado antes de atribuir OnChange

### Problema: Combos não atualizam ao mudar operação
**Causa:** Evento OnChange não foi atribuído  
**Solução:** Verificar se linha `cbxTipoOperacaoRH.OnChange := cbxTipoOperacaoRHChange;` está no CarregarCombos

### Problema: Erro de compilação "Undeclared identifier"
**Causa:** Métodos não declarados na seção protected  
**Solução:** Adicionar declarações no PASSO 1 de cada arquivo

---

## 📝 RESUMO DAS ALTERAÇÕES

### Arquivos Modificados:
- ✏️ `Sol.NET\Form\uFrmCadastroRH.pas`
- ✏️ `Sol.NET\Form\uFrmCadastroLancamentoRH.pas`

### Métodos Adicionados (em cada arquivo):
- `PreencherPortadoresRH` - Filtra portadores por operação
- `PreencherTiposDocumentosRH` - Filtra tipos de documentos por operação  
- `cbxTipoOperacaoRHChange` - Evento de mudança de operação

### Métodos Modificados (em cada arquivo):
- `CarregarCombos` - Chama novos métodos e atribui evento
- `cbxTipoOperacaoRHCloseUp` - Adiciona chamada ao Change
- `cbxTipoOperacaoRHKeyUp` - Adiciona chamada ao Change

---

## ✨ BENEFÍCIOS DA SOLUÇÃO

1. ✅ **Prevenção de Erro:** Usuário não consegue mais selecionar opções incompatíveis
2. ✅ **UX Melhorada:** Interface mais intuitiva, mostra apenas opções válidas
3. ✅ **Consistência:** Mesmo comportamento de ContasPR
4. ✅ **Manutenibilidade:** Usa funções existentes e testadas
5. ✅ **Performance:** Filtra no banco, não no cliente
6. ✅ **Escalável:** Fácil adicionar novos tipos de operação se necessário

---

**Desenvolvido por:** GitHub Copilot  
**Data:** 25/11/2025  
**Versão do Guia:** 1.0
