# Issue #7589 - Adicionar "Rescisão" em Lançamento de RH

## 📋 Resumo
Implementação da opção "Rescisão" como tipo de registro em Lançamento de RH, permitindo cadastrar, filtrar e visualizar lançamentos do tipo Rescisão.

---

## 🎯 Solicitação
- Adicionar "Rescisão" no ComboBox de Registro (Cadastro)
- Adicionar "Rescisão" no ComboBox de Registro (Pesquisa/Filtro)
- Garantir que o sistema salve e exiba corretamente os registros tipo Rescisão

---

## 🔧 Implementação

### 1️⃣ **Arquivos Modificados**

| Arquivo | Alteração |
|---------|-----------|
| `uFrmCadastroLancamentoRH.dfm` | Adicionados items "Rescisão" e ID "3" nos combos |
| `uDalGetText.pas` | Adicionado case 3 no método `GetTextCds_RHFerias` |

---

### 2️⃣ **Detalhamento das Alterações**

#### **A) ComboBox de Cadastro (`cbxRegistroRH`)**
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

#### **B) ComboBox de Filtro (`cbxRegistroRHVis`)**
**Arquivo:** `Sol.NET\Form\uFrmCadastroLancamentoRH.dfm` (linha ~329)

**Alteração:** Idêntica ao combo de cadastro (mesmos items e IDs).

---

#### **C) Método de Conversão de Texto (`GetTextCds_RHFerias`)**
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

### **Campo Utilizado**
- **Tabela:** `LANCAMENTO_RH`
- **Campo:** `FERIAS` (tipo `SMALLINT`)
- **Valores:**
  - `0` = Normal
  - `1` = Férias
  - `2` = Décimo Terceiro
  - `3` = Rescisão (NOVO)

**Nenhuma alteração de estrutura foi necessária** - o campo já aceita valores numéricos.

---

## ✅ Fluxo de Funcionamento

### **1. Cadastro**
```
Usuário seleciona "Rescisão" → cbxRegistroRH retorna ID 3 → 
Sistema salva FERIAS = 3 no banco
```

### **2. Filtro/Pesquisa**
```
Usuário seleciona "Rescisão" no filtro → cbxRegistroRHVis passa valor 3 → 
SQL busca registros WHERE FERIAS = 3
```

### **3. Visualização Grid**
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
✅ Campo `FERIAS` é do tipo numérico (aceita valor 3)  

---

## 🎓 Observações Técnicas

- O nome do campo `FERIAS` é legado - originalmente só armazenava se era Férias (1) ou Normal (0)
- O campo foi evoluído para armazenar: Décimo (2) e agora Rescisão (3)
- Manter o nome por compatibilidade com código existente


# 🧪 Plano de Testes - Issue #7589

## Objetivo
Validar a funcionalidade "Rescisão" em Lançamento de RH (cadastro, filtro e exibição).

---

## ⚙️ Preparação

1. **Compilar o projeto:**
   - Certifique-se de que os arquivos foram salvos
   - Compile o `Sol.NET` em modo Debug (x64)

2. **Abrir o Sol.NET:**
   - Execute a aplicação
   - Entre com usuário e senha
   - Selecione a empresa de testes

---

## 📋 Testes a Executar

### **Teste 1: Cadastro de Novo Lançamento RH com Rescisão**

| Passo | Ação | Resultado Esperado |
|-------|------|-------------------|
| 1 | Acesse o menu de **Lançamento de RH** | Formulário de cadastro abre |
| 2 | Clique em **Novo** | Formulário limpo pronto para cadastro |
| 3 | Abra o ComboBox **Registro** | Deve aparecer: Normal, Férias, Décimo Terceiro, **Rescisão** |
| 4 | Selecione **Rescisão** | Campo fica com valor "Rescisão" |
| 5 | Preencha os demais campos obrigatórios | Campos preenchidos |
| 6 | Clique em **Salvar** | Mensagem de sucesso, registro salvo |
| 7 | Localize o registro recém-criado no grid | Registro aparece no grid |
| 8 | Verifique a coluna "Registro" no grid | Deve exibir **"RESCISÃO"** (não "NORMAL") |

**✅ Critério de Sucesso:**
- ComboBox mostra "Rescisão"
- Sistema salva sem erro
- Grid exibe "RESCISÃO" corretamente

---

### **Teste 2: Edição de Lançamento RH Existente**

| Passo | Ação | Resultado Esperado |
|-------|------|-------------------|
| 1 | Localize o registro criado no Teste 1 | Registro está no grid com "RESCISÃO" |
| 2 | Dê duplo clique para editar | Formulário abre com dados carregados |
| 3 | Verifique o ComboBox **Registro** | Deve estar selecionado **"Rescisão"** |
| 4 | Altere algum outro campo (ex: observação) | Campo alterado |
| 5 | Clique em **Salvar** | Registro salvo sem erro |
| 6 | Verifique o grid novamente | Continua exibindo "RESCISÃO" |

**✅ Critério de Sucesso:**
- Combo carrega "Rescisão" corretamente ao editar
- Alterações são salvas mantendo o tipo Rescisão

---

### **Teste 3: Filtro por Rescisão**

| Passo | Ação | Resultado Esperado |
|-------|------|-------------------|
| 1 | Na tela de Lançamento RH, localize o filtro | Filtro visível no topo da tela |
| 2 | Abra o ComboBox **Registro** do filtro | Deve aparecer: (vazio), Normal, Férias, Décimo Terceiro, **Rescisão** |
| 3 | Selecione **Rescisão** | Filtro ativado |
| 4 | Clique em **Pesquisar/Filtrar** | Grid atualiza |
| 5 | Verifique os registros no grid | Apenas registros com tipo "RESCISÃO" aparecem |
| 6 | Limpe o filtro (selecione opção vazia) | Todos os registros voltam a aparecer |

**✅ Critério de Sucesso:**
- Filtro mostra opção "Rescisão"
- Grid filtra corretamente apenas Rescisões
- Limpar filtro restaura todos os registros

---

### **Teste 4: Validação de Banco de Dados**

**Este teste é opcional - use apenas se tiver acesso direto ao banco:**

| Passo | Ação | Resultado Esperado |
|-------|------|-------------------|
| 1 | Conecte-se ao banco Firebird/SQL Server | Conexão estabelecida |
| 2 | Execute a query: `SELECT * FROM LANCAMENTO_RH WHERE FERIAS = 3` | Retorna os registros criados nos testes 1 e 2 |
| 3 | Verifique o campo `FERIAS` | Deve conter valor **3** |

**✅ Critério de Sucesso:**
- Campo `FERIAS` armazena corretamente valor 3

---

## 🔍 Checklist Geral

- [ ] Compilação sem erros
- [ ] Cadastro com "Rescisão" funciona
- [ ] Grid exibe "RESCISÃO" (não "NORMAL")
- [ ] Edição mantém tipo "Rescisão"
- [ ] Filtro encontra apenas Rescisões
- [ ] Banco armazena valor 3 no campo FERIAS

---

## 🐛 Em Caso de Erro

### **Se o grid exibir "NORMAL" ao invés de "RESCISÃO":**
- Verifique se `uDalGetText.pas` foi compilado com o case 3
- Verifique se o projeto foi completamente recompilado (Build All)

### **Se não aparecer "Rescisão" no ComboBox:**
- Confirme que `uFrmCadastroLancamentoRH.dfm` foi salvo com as alterações
- Confirme que ambos combos (`cbxRegistroRH` e `cbxRegistroRHVis`) foram alterados

### **Se houver erro ao salvar:**
- Verifique se há validações personalizadas no código do formulário
- Verifique se o banco suporta valor 3 no campo FERIAS

---

## 📊 Relatório de Testes

Após executar os testes, preencha:

| Teste | Status | Observações |
|-------|--------|-------------|
| Teste 1: Cadastro | ⬜ Pass / ⬜ Fail | |
| Teste 2: Edição | ⬜ Pass / ⬜ Fail | |
| Teste 3: Filtro | ⬜ Pass / ⬜ Fail | |
| Teste 4: BD | ⬜ Pass / ⬜ Fail / ⬜ Não executado | |

---

**Testado por:** _______________  
**Data:** _______________  
**Versão:** _______________



---

**Implementado por:** Weldyson Azevedo  
**Data:** 03/12/2025  
**Issue:** #7589  
**Branch:** `7563-246926-solnet---erros---lancamento-rhao-inserir-um-debito-com-portador-a-vista-aviso-de-n`
