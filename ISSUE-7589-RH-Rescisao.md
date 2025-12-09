[ISSUE-7589-RH-Rescisao.md](https://github.com/user-attachments/files/24057177/ISSUE-7589-RH-Rescisao.md)
# Issue #7589 - Adicionar "Rescisão" em Lançamento de RH

## 📋 Resumo
Implementação da opção "Rescisão" como tipo de registro em Lançamento de RH e Configuração de RH, permitindo cadastrar, filtrar e visualizar lançamentos do tipo Rescisão.

---

## 🎯 Solicitação Original
1. ✅ Adicionar "Rescisão" no ComboBox de Registro em **Lançamento de RH** (Cadastro)
2. ✅ Adicionar "Rescisão" no ComboBox de Registro em **Lançamento de RH** (Pesquisa/Filtro)
3. ✅ Adicionar "Rescisão" no ComboBox de Registro em **Configuração de RH** (Pesquisa/Filtro)
4. ✅ Garantir que o sistema salve e exiba corretamente os registros tipo Rescisão

---

## 🔧 Implementação

### 1️⃣ **Arquivos Modificados**

| Arquivo | Alteração | Telas Afetadas |
|---------|-----------|----------------|
| `uFrmCadastroLancamentoRH.dfm` | Adicionados items "Rescisão" e ID "3" nos combos | Lançamento de RH |
| `uFrmCadastroRH.dfm` | Adicionado combo `cbxVisRegistroRH` com filtro | Configuração de RH |
| `uFrmCadastroRH.pas` | Declaração do combo e atualização das chamadas DAL | Configuração de RH |
| `uDalPessoa.pas` | Parâmetro `objRegistroRH` nas funções de busca | Configuração de RH |
| `uDalGetText.pas` | Adicionado case 3 no método `GetTextCds_RHFerias` | Ambas |

---

### 2️⃣ **Detalhamento das Alterações**

#### **A) Lançamento de RH - ComboBox de Cadastro (`cbxRegistroRH`)**
**Arquivo:** `Sol.NET\Form\uFrmCadastroLancamentoRH.dfm` (linha ~2186)

**Alteração:**
```delphi
AHS_ItemsID.Strings = (
  '-1'
  '0'
  '1'
  '2'
  '3')  // ← ADICIONADO

Items.Strings = (
  ''
  'Normal'
  'Férias'
  'Décimo Terceiro'
  'Rescisão')  // ← ADICIONADO
```

**Observação:** O combo já estava vinculado ao campo `FERIAS` da tabela `LANCAMENTO_RH` via propriedade `AHS_ClientDataSetCampo`.

---

#### **B) Lançamento de RH - ComboBox de Filtro (`cbxRegistroRHVis`)**
**Arquivo:** `Sol.NET\Form\uFrmCadastroLancamentoRH.dfm` (linha ~329)

**Alteração:** Idêntica ao combo de cadastro (mesmos items e IDs).

---

#### **C) Configuração de RH - ComboBox de Filtro (`cbxVisRegistroRH`)**
**Arquivo:** `Sol.NET\Form\uFrmCadastroRH.dfm` (linha ~176)

**Novo componente criado:**
```delphi
object cbxVisRegistroRH: TComboBoxPlus
  Left = 526
  Top = 53
  Width = 114
  Height = 22
  Cursor = crHandPoint
  EditLabel.Width = 40
  EditLabel.Height = 13
  EditLabel.Caption = 'Registro'
  LabelSpacing = 0
  AHS_ItemsID.Strings = (
    '-1'
    '0'
    '1'
    '2'
    '3')
  AHS_CampoObrigatorio = False
  AHS_ClientDataSetCampo = 'FERIAS'
  AHS_ClientDataSetCampoAgrupar = False
  AHS_ClientDataSetNaoGravar = False
  AHS_NaoUsarEnterForm = False
  AHS_Caption = 'Registro'
  AHS_TipoComboBox = tbInteger
  AHS_Auditoria = False
  Style = csOwnerDrawFixed
  Color = clWhite
  TabOrder = 6
  Items.Strings = (
    ''
    'Normal'
    'Férias'
    'Décimo Terceiro'
    'Rescisão')
end
```

**Posicionamento:** TabSheet "Pesquisar por", ao lado dos outros filtros de busca.

---

#### **D) Configuração de RH - Declaração e Chamadas**
**Arquivo:** `Sol.NET\Form\uFrmCadastroRH.pas`

**Declaração do componente (linha ~96):**
```delphi
cbxVisRegistroRH: TComboBoxPlus;
```

**Atualização de todas as chamadas (6 locais):**
```delphi
// Exemplo da chamada principal (linha ~369):
cdsBuscar.Data := DalPessoa.SqlBuscarConfiguracaoRH(-1, cbxVisCampoPesquisado, 
  cbxVisCondicao, txtVisBuscar, cbxVisIdEmpresa, cbxVisIdEmpresa2,
  txtPlanoConta, txtCentroCusto, cbxVisRegistroRH);  // ← Parâmetro adicionado
```

---

#### **E) DAL - Funções de Busca**
**Arquivo:** `Sol.NET\Dal\uDalPessoa.pas`

**Assinatura das funções atualizadas:**
```delphi
// Função principal (linha ~129):
function SqlBuscarConfiguracaoRH(vlIdTabela: Double; objCampoAPesquisar1: TComboBoxPlus; 
  objCondicao1: TComboBoxPlus; objTextoOuIdPesquisar1: TGenEditBtn; 
  idEmpresa: TCheckedComboBoxPlus; idEmpresa2: TCheckedComboBoxPlus;
  objPlanoContas: TGenEditBtn; objCentroCusto: TGenEditBtn; 
  objRegistroRH: TComboBoxPlus; IdPessoa: Double = 0): OleVariant;  // ← Parâmetro adicionado

// Função resumo (linha ~133):
function SqlBuscarConfiguracaoRH_Resumo(objCampoAPesquisar1: TComboBoxPlus; 
  objCondicao1: TComboBoxPlus; objTextoOuIdPesquisar1: TGenEditBtn; 
  idEmpresa: TCheckedComboBoxPlus; idEmpresa2: TCheckedComboBoxPlus;
  objPlanoContas: TGenEditBtn; objCentroCusto: TGenEditBtn; 
  objRegistroRH: TComboBoxPlus): OleVariant;  // ← Parâmetro adicionado
```

**Lógica de filtro SQL implementada (linhas ~1811 e ~1894):**
```delphi
if objRegistroRH.AsInteger > -1 then
begin
  strSql.Append('AND (RH.FERIAS = ' + objRegistroRH.AsStringValor + ') ');
end;
```

**Comportamento:**
- Quando `cbxVisRegistroRH` está vazio (valor -1): mostra todos os registros
- Quando um valor é selecionado (0, 1, 2 ou 3): filtra apenas registros daquele tipo

---

#### **F) Método de Conversão de Texto (`GetTextCds_RHFerias`)**
**Arquivo:** `Framework\Dal\uDalGetText.pas` (linha ~1994)

**COMO CHEGAMOS AQUI:**

1. Identificamos que o campo no banco se chama `FERIAS` (via `AHS_ClientDataSetCampo`)
2. Buscamos no código onde esse campo é usado para exibição:
   ```pascal
   cdsBuscar.FieldByName('FERIAS').OnGetText := DalGetText.GetTextCds_RHFerias;
   ```
3. Localizamos o método `GetTextCds_RHFerias` no `uDalGetText.pas`
4. Identificamos que faltava o case para valor `3`

**Alteração:**
```delphi
procedure TDalGetText.GetTextCds_RHFerias(Sender: TField; var Text: string; DisplayText: Boolean);
begin
  if Sender.DataSet.FieldByName(Sender.FieldName).AsString <> '' then
  begin
    if DisplayText then
    begin
      case Sender.DataSet.FieldByName(Sender.FieldName).AsInteger of
        1:
          Text := 'FÉRIAS';
        2:
          Text := 'DÉCIMO TERCEIRO';
        3:
          Text := 'RESCISÃO';  // ← ADICIONADO
      else
        Text := 'NORMAL';
      end;
    end;
  end;
end;
```

**Por que essa alteração é necessária:**
- Sem o case 3, quando o grid exibe um registro com `FERIAS = 3`, ele cai no `else` e mostra "NORMAL"
- Com o case 3, o grid exibe corretamente "RESCISÃO"

---

## 🗄️ Banco de Dados

### **Tabelas Afetadas**

#### **1. LANCAMENTO_RH**
- **Campo:** `FERIAS` (tipo `SMALLINT`)
- **Uso:** Armazena o tipo de registro do lançamento
- **Tela:** Lançamento de RH

#### **2. PESSOA_RH**
- **Campo:** `FERIAS` (tipo `SMALLINT`)
- **Uso:** Armazena o tipo de registro da configuração
- **Tela:** Configuração de RH

### **Valores Padronizados**
| Valor | Descrição |
|-------|-----------|
| `0` | Normal |
| `1` | Férias |
| `2` | Décimo Terceiro |
| `3` | Rescisão (NOVO) |

**Nenhuma alteração de estrutura foi necessária** - os campos já aceitam valores numéricos.

---

## ✅ Fluxo de Funcionamento

### **1. Lançamento de RH - Cadastro**
```
Usuário seleciona "Rescisão" → cbxRegistroRH retorna ID 3 → 
Sistema salva FERIAS = 3 na tabela LANCAMENTO_RH
```

### **2. Lançamento de RH - Filtro/Pesquisa**
```
Usuário seleciona "Rescisão" no filtro → cbxRegistroRHVis passa valor 3 → 
SQL busca registros WHERE FERIAS = 3 na tabela LANCAMENTO_RH
```

### **3. Configuração de RH - Filtro/Pesquisa**
```
Usuário seleciona "Rescisão" no filtro → cbxVisRegistroRH passa valor 3 → 
SQL busca registros WHERE RH.FERIAS = 3 na tabela PESSOA_RH
```

### **4. Visualização Grid (ambas as telas)**
```
Grid carrega registro com FERIAS = 3 → 
Evento OnGetText chama GetTextCds_RHFerias → 
Case 3 retorna "RESCISÃO" → Grid exibe "RESCISÃO"
```

---

## 📝 Validações Realizadas

✅ Não há validações no código limitando valores de 0 a 2  
✅ Não há CASE statements em SQL que precisem atualização  
✅ Não há constraints CHECK no banco bloqueando valor 3  
✅ Campos `FERIAS` são do tipo numérico (aceitam valor 3)  
✅ Todas as 6 chamadas à `SqlBuscarConfiguracaoRH` foram atualizadas  
✅ Ambas as funções DAL (`SqlBuscarConfiguracaoRH` e `SqlBuscarConfiguracaoRH_Resumo`) incluem o filtro  

---

## 🎓 Observações Técnicas

### Nomenclatura Legada
- O nome do campo `FERIAS` é legado - originalmente só armazenava se era Férias (1) ou Normal (0)
- O campo foi evoluído para armazenar: Décimo (2) e agora Rescisão (3)
- Nome mantido por compatibilidade com código existente e evitar refatoração massiva

### Padrão de Implementação
- **Lançamento de RH**: Componente já existia, apenas adicionados novos valores
- **Configuração de RH**: Componente criado do zero, replicando padrão do Lançamento de RH
- **Framework**: Método de conversão compartilhado entre ambas as telas

### Compatibilidade
- **Firebird 3.0/5.0**: ✅ Compatível
- **SQL Server**: ✅ Compatível
- **Código ISO SQL**: ✅ Utilizado nas queries

---

## 🧪 Testes Recomendados

### Teste Manual 1: Lançamento de RH
1. Abrir tela "Lançamento de RH"
2. Criar novo registro com Registro = "Rescisão"
3. Salvar e verificar que foi gravado com `FERIAS = 3`
4. Filtrar por "Rescisão" na aba de pesquisa
5. Verificar que apenas registros tipo Rescisão são exibidos
6. Verificar que o grid mostra "RESCISÃO" na coluna Registro

### Teste Manual 2: Configuração de RH
1. Abrir tela "Configuração de RH"
2. Selecionar filtro Registro = "Rescisão"
3. Verificar que apenas configurações tipo Rescisão são exibidas
4. Verificar que o grid mostra "RESCISÃO" na coluna Registro

### Teste Manual 3: Integração
1. Criar lançamento com tipo Rescisão
2. Verificar que aparece corretamente na Configuração de RH
3. Testar com ambos os SGBDs (Firebird e SQL Server)

---

## 📊 Review de Código

**Status:** ✅ APROVADO

**Comentários da Revisão:**
> "Code Review: No defects detected in the new `cbxVisRegistroRH` filter flow (`uFrmCadastroRH.dfm/.pas`, `uDalPessoa.pas`). The component is properly declared, wired into every `SqlBuscarConfiguracaoRH`/`_Resumo` call, and the DAL now applies the expected `RH.FERIAS` predicate when a value is chosen."

**Riscos Residuais:**
- Testar manualmente com Firebird e SQL Server
- Confirmar comportamento do filtro vazio (mostrar todos)
- Verificar exibição correta em todos os grids

---

**Implementado por:** Weldyson Azevedo  
**Data Inicial:** 03/12/2025  
**Data Conclusão:** 09/12/2025  
**Issue:** #7589  
**Branch:** `7589-247472-solnet---rh-melhorias-incluir-rescisao-em-registro-na-tela-lancamento-de-rh-ima`

---

## 📚 Referências

- Issue Original: GitHub #7589
- Tabelas: `LANCAMENTO_RH`, `PESSOA_RH`
- Manual Sol.NET: https://hetosoft.com.br/Arquivos/Manual/index.htm
