# Mapeamento Completo - Issue #7303
## Adicionar Campo para Pesquisar Código Similar por Descrição

---

## 📋 **RESUMO DA NECESSIDADE**

Atualmente é possível pesquisar produtos por **Similar** usando o combo `cbxSimilarVis`, que filtra pelo **ID do Similar**.

**O que precisa ser implementado:**
- Adicionar opção de pesquisar produtos pela **DESCRIÇÃO DO SIMILAR** no campo de pesquisa principal (`cbxVisCampoPesquisado`)
- Quando o usuário selecionar "Similar (Descrição)" e digitar um texto, o sistema deve buscar todos os produtos vinculados a similares cuja descrição contenha o texto digitado

---

## 🎯 **ESPELHAR-SE EM:**

A implementação já existe de forma análoga com os seguintes recursos:
1. **Pesquisa por Similar (ID)**: usando `cbxSimilarVis` (combo separado)
2. **Pesquisa por Sugestão (ID)**: usando `cbxSugestaoVis` (combo separado)
3. **Pesquisa por outros campos com lookup**: como "Marca/Fabricante", "Fornecedor Padrão", etc.

---

## 📍 **LOCAIS EXATOS DE ALTERAÇÃO**

### **1. ARQUIVO: `Sol.NET\Dal\uDalProduto.pas`**

#### **📌 LOCAL 1.1: Modificar a função `SqlBuscarProduto`**

**Linha aproximada:** 1048-1049

**O que fazer:**
- Adicionar um novo parâmetro `objSimilarDescVis: TComboBoxPlus = nil` (ou usar `objTextoSimilarVis: TGenEditBtn = nil`)

**ANTES:**
```pascal
function TDalProduto.SqlBuscarProduto(TodosRegistros: Integer; objCampoAPesquisar1: TComboBoxPlus; objCondicao1: TComboBoxPlus;
  objTextoOuIdPesquisar1: TGenEditBtn; objCampoAPesquisar2: TComboBoxPlus; objCondicao2: TComboBoxPlus; objTextoOuIdPesquisar2: TGenEditBtn;
  TipoPesquisa: TTipoPesquisar; vlId: Double = 0; varStatusReg: TComboBoxPlus = nil; varLista: Double = 0; varConsultaOpcoes: Integer = -1;
  varConsultaOpcoes2: Integer = -1; varEmpresaOpcoes2: string = ''; varIdMoeda: Double = 0; objTipoData: Integer = -1; objDataInicial: TDate = 0;
  objDataFinal: TDate = 0; obsTipoItem: Integer = -1; objErro: Integer = -1; objEstoque: Integer = -1; varIdsLimitador: string = '';
  varIdsPromocao: string = ''; objCondicaoE: TComboBoxPlus = nil; objTextoOuIdPesquisarE: TGenEditBtn = nil; objProdEmpresa: TComboBoxPlus = nil;
  vlIdEmpresaMov: Double = 0; objProdEmpInd: Integer = 0; objTipoMov: Integer = -1; objDataInicialMov: TDate = 0; objDataFinalMov: TDate = 0;
  objEmpresaMov: Double = 0; objSituacaoEstoque: Double = 0; objLocalEstoque: Double = 0; objHoraInicial: TTime = 0; objHoraFinal: TTime = 0;
  objStatusImagem: TComboBoxPlus = nil; objHetoStatusImagem: TComboBoxPlus = nil; objSimilarVis: TComboBoxPlus = nil;
  objSugestaoVis: TComboBoxPlus = nil; objImgCodCliente: TComboBoxPlus = nil; tpEstatistica: Integer = 0): OleVariant;
```

**DEPOIS:**
```pascal
function TDalProduto.SqlBuscarProduto(TodosRegistros: Integer; objCampoAPesquisar1: TComboBoxPlus; objCondicao1: TComboBoxPlus;
  objTextoOuIdPesquisar1: TGenEditBtn; objCampoAPesquisar2: TComboBoxPlus; objCondicao2: TComboBoxPlus; objTextoOuIdPesquisar2: TGenEditBtn;
  TipoPesquisa: TTipoPesquisar; vlId: Double = 0; varStatusReg: TComboBoxPlus = nil; varLista: Double = 0; varConsultaOpcoes: Integer = -1;
  varConsultaOpcoes2: Integer = -1; varEmpresaOpcoes2: string = ''; varIdMoeda: Double = 0; objTipoData: Integer = -1; objDataInicial: TDate = 0;
  objDataFinal: TDate = 0; obsTipoItem: Integer = -1; objErro: Integer = -1; objEstoque: Integer = -1; varIdsLimitador: string = '';
  varIdsPromocao: string = ''; objCondicaoE: TComboBoxPlus = nil; objTextoOuIdPesquisarE: TGenEditBtn = nil; objProdEmpresa: TComboBoxPlus = nil;
  vlIdEmpresaMov: Double = 0; objProdEmpInd: Integer = 0; objTipoMov: Integer = -1; objDataInicialMov: TDate = 0; objDataFinalMov: TDate = 0;
  objEmpresaMov: Double = 0; objSituacaoEstoque: Double = 0; objLocalEstoque: Double = 0; objHoraInicial: TTime = 0; objHoraFinal: TTime = 0;
  objStatusImagem: TComboBoxPlus = nil; objHetoStatusImagem: TComboBoxPlus = nil; objSimilarVis: TComboBoxPlus = nil;
  objSugestaoVis: TComboBoxPlus = nil; objImgCodCliente: TComboBoxPlus = nil; tpEstatistica: Integer = 0;
  objSimilarDescVis: TGenEditBtn = nil): OleVariant;  // <-- NOVO PARÂMETRO AQUI
```

#### **📌 LOCAL 1.2: Na declaração do cabeçalho (interface)**

**Linha aproximada:** 99-108

**Fazer a mesma alteração** na declaração da função na seção `public` da classe `TDalProduto`.

#### **📌 LOCAL 1.3: Implementar a lógica de busca**

**Linha aproximada:** 2347-2366 (logo após o bloco de Similar por ID)

**ESPELHAR-SE EM:** O código existente de Similar e Sugestão

**ADICIONAR APÓS O BLOCO DE SUGESTÃO:**
```pascal
      // Sugestao
      if (objSugestaoVis <> nil) and (objSugestaoVis.AsFloat > 0) then
      begin
        strAux.Clear;
        strAux.Append(' SELECT SUGG.ID_PRODUTO                                             ' + BR);
        strAux.Append(' FROM PRODUTO_SUGESTAO SUGG  ' + SQL.WithNoLock + '                ' + BR);
        strAux.Append(' WHERE SUGG.ID_SUGESTAO = ' + objSugestaoVis.AsFloat.ToString + '  ' + BR);

        strSql.Append(' AND PROD.ID_PRODUTO IN (' + strAux.ToString + ') ');
      end;

      // *** NOVO CÓDIGO AQUI ***
      // Similar por Descrição
      if (objSimilarDescVis <> nil) and (Trim(objSimilarDescVis.Text) <> '') then
      begin
        strAux.Clear;
        strAux.Append(' SELECT PS.ID_PRODUTO                                               ' + BR);
        strAux.Append(' FROM PRODUTO_SIMILARES PS  ' + SQL.WithNoLock + '                  ' + BR);
        strAux.Append(' INNER JOIN SIMILARES SIM ON SIM.ID_SIMILAR = PS.ID_SIMILAR        ' + BR);
        strAux.Append(' WHERE UPPER(SIM.DESCRICAO) LIKE UPPER(''' + '%' + Trim(objSimilarDescVis.Text) + '%' + ''') ' + BR);

        strSql.Append(' AND PROD.ID_PRODUTO IN (' + strAux.ToString + ') ');
      end;
      // *** FIM DO NOVO CÓDIGO ***

      if (objImgCodCliente <> nil) and (objImgCodCliente.AsFloat > 0) then
      begin
        // ... código existente
```

---

### **2. ARQUIVO: `Sol.NET\FormEspecias\uFrmCadastroProdutos.dfm`**

#### **📌 LOCAL 2.1: Adicionar nova opção no combo `cbxVisCampoPesquisado`**

**Linha aproximada:** 150-247

**ESPELHAR-SE EM:** As outras opções já existentes no combo

**O que fazer:**
- Adicionar `'Similar (Descrição)'` na propriedade `Items.Strings`
- Adicionar `'SIM.DESCRICAO'` na propriedade `AHS_ItemsID.Strings`

**LOCALIZAR:**
```pascal
              Items.Strings = (
                'Automático'
                'Automático Condição'
                'Código Produto/Descrição'
                'Descrição'
                ...
                'Margem Real'
                'Margem Real 2')
```

**ADICIONAR (em ordem alfabética ou no final):**
```pascal
              Items.Strings = (
                'Automático'
                'Automático Condição'
                'Código Produto/Descrição'
                'Descrição'
                ...
                'Margem Real'
                'Margem Real 2'
                'Similar (Descrição)')  // <-- NOVA OPÇÃO
```

**E TAMBÉM ADICIONAR em `AHS_ItemsID.Strings`:**
```pascal
              AHS_ItemsID.Strings = (
                'DESCRICAO_CODIGO'
                'DESCRICAO_CODIGO_EXATO'
                ...
                'PROD.PC_MARGEM_LUCRO_2REAL'
                'SIM.DESCRICAO')  // <-- NOVO ID
```

---

### **3. ARQUIVO: `Sol.NET\FormEspecias\uFrmCadastroProdutos.pas`**

#### **📌 LOCAL 3.1: Passar o novo parâmetro na chamada da função**

**Linha aproximada:** 2956-2962

**ESPELHAR-SE EM:** Como os outros parâmetros são passados

**ANTES:**
```pascal
        cdsBuscar.Data := DalProduto.SqlBuscarProduto(cbxTodosRegistros.AsInteger, cbxVisCampoPesquisado, cbxCondicao, txtVisBuscar,
          cbxVisCampoPesquisado2, cbxCondicao2, txtVisBuscar2, TipoConsulta, 0, cbxStatusReg, cbxListaVis.AsFloat, cbxConsultasOpcoes.AsInteger,
          cbxConsultasOpcoes2.AsInteger, txtVisEmpresas.IdLista, cbxMoedaVis.AsFloat, cbxVisData.AsInteger, txtVisDtInicial.AsDateTime,
          txtVisDtFinal.AsDateTime, cbxTipoItemVis.AsInteger, cbxErrosVis.AsInteger, cbxEstoque.AsInteger, varIdsLimitador, vIdsPromocoes,
          cbxCondicaoE, txtVisBuscarE, cbxProdEmpresa, varIdEmpresa, cbxProdInd.AsInteger, cbxVisDataMov.AsInteger, txtVisDtInicialMov.AsDateTime,
          txtVisDtFinalMov.AsDateTime, cbxEmpresaVisMov.AsFloat, cbxSitSituacaoEstoque3.AsFloat, cbxSitLocalEstoque3.AsFloat,
          txtHoraInicial.AsDateTime, txtHoraFinal.AsDateTime, cbxStatusImagem, cbxHStatusImagem, cbxSimilarVis, cbxSugestaoVis, cbxImgCodCliente,
          cbxEstatistica.AsInteger);
```

**DEPOIS:**
```pascal
        cdsBuscar.Data := DalProduto.SqlBuscarProduto(cbxTodosRegistros.AsInteger, cbxVisCampoPesquisado, cbxCondicao, txtVisBuscar,
          cbxVisCampoPesquisado2, cbxCondicao2, txtVisBuscar2, TipoConsulta, 0, cbxStatusReg, cbxListaVis.AsFloat, cbxConsultasOpcoes.AsInteger,
          cbxConsultasOpcoes2.AsInteger, txtVisEmpresas.IdLista, cbxMoedaVis.AsFloat, cbxVisData.AsInteger, txtVisDtInicial.AsDateTime,
          txtVisDtFinal.AsDateTime, cbxTipoItemVis.AsInteger, cbxErrosVis.AsInteger, cbxEstoque.AsInteger, varIdsLimitador, vIdsPromocoes,
          cbxCondicaoE, txtVisBuscarE, cbxProdEmpresa, varIdEmpresa, cbxProdInd.AsInteger, cbxVisDataMov.AsInteger, txtVisDtInicialMov.AsDateTime,
          txtVisDtFinalMov.AsDateTime, cbxEmpresaVisMov.AsFloat, cbxSitSituacaoEstoque3.AsFloat, cbxSitLocalEstoque3.AsFloat,
          txtHoraInicial.AsDateTime, txtHoraFinal.AsDateTime, cbxStatusImagem, cbxHStatusImagem, cbxSimilarVis, cbxSugestaoVis, cbxImgCodCliente,
          cbxEstatistica.AsInteger, 
          txtVisBuscar);  // <-- NOVO PARÂMETRO (reutilizando o txtVisBuscar quando campo selecionado for "Similar (Descrição)")
```

**IMPORTANTE:** A lógica deve verificar se `cbxVisCampoPesquisado.Text = 'Similar (Descrição)'`. Se sim, passa `txtVisBuscar`, senão passa `nil`.

**SUGESTÃO DE IMPLEMENTAÇÃO CORRETA:**
```pascal
var
  objSimilarDesc: TGenEditBtn;
begin
  // Determinar se deve passar o campo de busca por descrição
  if cbxVisCampoPesquisado.Text = 'Similar (Descrição)' then
    objSimilarDesc := txtVisBuscar
  else
    objSimilarDesc := nil;

  cdsBuscar.Data := DalProduto.SqlBuscarProduto(cbxTodosRegistros.AsInteger, cbxVisCampoPesquisado, cbxCondicao, txtVisBuscar,
    cbxVisCampoPesquisado2, cbxCondicao2, txtVisBuscar2, TipoConsulta, 0, cbxStatusReg, cbxListaVis.AsFloat, cbxConsultasOpcoes.AsInteger,
    cbxConsultasOpcoes2.AsInteger, txtVisEmpresas.IdLista, cbxMoedaVis.AsFloat, cbxVisData.AsInteger, txtVisDtInicial.AsDateTime,
    txtVisDtFinal.AsDateTime, cbxTipoItemVis.AsInteger, cbxErrosVis.AsInteger, cbxEstoque.AsInteger, varIdsLimitador, vIdsPromocoes,
    cbxCondicaoE, txtVisBuscarE, cbxProdEmpresa, varIdEmpresa, cbxProdInd.AsInteger, cbxVisDataMov.AsInteger, txtVisDtInicialMov.AsDateTime,
    txtVisDtFinalMov.AsDateTime, cbxEmpresaVisMov.AsFloat, cbxSitSituacaoEstoque3.AsFloat, cbxSitLocalEstoque3.AsFloat,
    txtHoraInicial.AsDateTime, txtHoraFinal.AsDateTime, cbxStatusImagem, cbxHStatusImagem, cbxSimilarVis, cbxSugestaoVis, cbxImgCodCliente,
    cbxEstatistica.AsInteger, objSimilarDesc);
```

#### **📌 LOCAL 3.2: Ajustar comportamento do campo de pesquisa**

**Linha aproximada:** 25618-25650 (dentro da procedure `MudaStatusDaConsulta`)

**ESPELHAR-SE EM:** Como funciona para "Descrição", "Código Fabricante", etc.

**ADICIONAR:**
```pascal
  // Com Texto
  if (cbxVisCampoPesquisado.Text = 'Automático') or (cbxVisCampoPesquisado.Text = 'Automático Condição') or
    (cbxVisCampoPesquisado.Text = 'Código Produto/Descrição') or (cbxVisCampoPesquisado.Text = 'Código Barras/Descrição') or
    (cbxVisCampoPesquisado.Text = 'Descrição') or (cbxVisCampoPesquisado.Text = 'Descrição/Códigos') or
    (cbxVisCampoPesquisado.Text = 'Similar (Descrição)') or  // <-- ADICIONAR AQUI
    (varListaCamposCCC_Texto.IndexOf(cbxVisCampoPesquisado.Text) > -1) or (cbxVisCampoPesquisado.Text = 'Código Fabricante') or
    (cbxVisCampoPesquisado.Text = 'Código Fornecedor') or (cbxVisCampoPesquisado.Text = 'Código Barra') or (cbxVisCampoPesquisado.Text = 'CST ICMS')
  then
  begin
    txtVisBuscar.Hint := '';
    txtVisBuscar.TextHint := 'Coloque o conteúdo';
    txtVisBuscar.AHS_TipoEdit := teString;
    txtVisBuscar.AHS_TrocarPontoVirg := False;
    txtVisBuscar.AHS_ReadOnly2 := False;
  end;
```

---

## 🔄 **FLUXO COMPLETO DA IMPLEMENTAÇÃO**

### **Etapa 1: Modificar uDalProduto.pas**
1. Adicionar parâmetro `objSimilarDescVis: TGenEditBtn = nil` na assinatura da função (interface + implementation)
2. Implementar a lógica SQL para buscar por descrição do similar

### **Etapa 2: Modificar uFrmCadastroProdutos.dfm**
1. Adicionar "Similar (Descrição)" nos `Items.Strings` do `cbxVisCampoPesquisado`
2. Adicionar "SIM.DESCRICAO" nos `AHS_ItemsID.Strings`

### **Etapa 3: Modificar uFrmCadastroProdutos.pas**
1. Na procedure que chama `SqlBuscarProduto`, adicionar lógica condicional para passar `txtVisBuscar` quando campo for "Similar (Descrição)"
2. Na procedure `MudaStatusDaConsulta`, adicionar "Similar (Descrição)" no bloco de campos de texto

---

## ✅ **VALIDAÇÃO**

Após implementar, testar:
1. Selecionar "Similar (Descrição)" no combo de pesquisa
2. Digitar parte da descrição de um similar existente
3. Verificar se retorna todos os produtos vinculados a similares com essa descrição
4. Testar com diferentes condições: CONTÉM, IGUAL, INICIA COM, etc.

---

## 📚 **REFERÊNCIAS DE CÓDIGO EXISTENTE**

### **Similar por ID (cbxSimilarVis):**
- **Linha 2347 em uDalProduto.pas:** Implementação da busca
- **Linha 1001 em uFrmCadastroProdutos.dfm:** Componente visual
- **Linha 15794 em uFrmCadastroProdutos.pas:** Preenchimento do combo

### **Menu de contexto "Localizar Similares":**
- **Linha 23089 em uFrmCadastroProdutos.pas:** Implementação do `mnLocalizarSimilaresClick`
- Pode servir de inspiração para lógica de busca

---

## 🎯 **RESULTADO ESPERADO**

Usuário poderá:
1. Selecionar "Similar (Descrição)" no campo de pesquisa
2. Digitar texto (ex: "PARAFUSO")
3. Sistema buscará na tabela SIMILARES todos os registros cuja descrição contenha "PARAFUSO"
4. Retornará todos os produtos da tabela PRODUTO_SIMILARES vinculados a esses similares
5. Exibirá a lista de produtos na grid de visualização

---

# 🗺️ Mapeamento de Consultas e Eventos - Issue #7303
## Pesquisa de Produtos por Similar

---

## 📍 **COMPONENTES VISUAIS PRINCIPAIS**

### **Formulário:** `uFrmCadastroProdutos`

#### **1. ComboBox de Campo a Pesquisar**
- **Componente:** `cbxVisCampoPesquisado` (TComboBoxPlus)
- **Localização DFM:** Linha ~150-247
- **Função:** Permite selecionar qual campo usar para buscar produtos
- **Eventos:**
  - `OnChange`: `cbxVisCampoPesquisadoChange` - linha ~1687
  - `OnCloseUp`: `cbxVisCampoPesquisadoCloseUp` - linha ~1352

#### **2. Campo de Texto para Pesquisa**
- **Componente:** `txtVisBuscar` (TGenEditBtn)
- **Função:** Campo onde o usuário digita o conteúdo da pesquisa
- **Eventos:**
  - `OnDblClick`: `txtVisBuscarDblClick`
  - `OnLeftButtonClick`: `txtVisBuscarLeftButtonClick`
  - `OnRightButtonClick`: `txtVisBuscarDblClick`

#### **3. ComboBox de Similar (Filtro por ID)**
- **Componente:** `cbxSimilarVis` (TComboBoxPlus)
- **Localização DFM:** Linha ~1001
- **Localização PAS:** Linha ~1049
- **Função:** Filtra produtos por ID do Similar (já existe)
- **Preenchimento:** Linha ~15794

#### **4. Botão Buscar**
- **Componente:** `btnBuscar1`, `btnBuscar2`, etc.
- **Evento:** `OnClick` → `btnBuscar1Click`
- **Chamadas:** Linhas 16211, 16220

---

## 🔍 **FLUXO DE CONSULTA PRINCIPAL**

### **1. Evento de Busca (Botão F5)**

```
Usuário clica em "Buscar" (F5)
    ↓
btnBuscar1Click (ou similar)
    ↓
Chama procedure Buscar (herdada)
    ↓
Linha ~2955: DalProduto.SqlBuscarProduto(...)
```

### **2. Procedure Buscar - Linha ~2950-3000**

**Arquivo:** `Sol.NET\FormEspecias\uFrmCadastroProdutos.pas`

```pascal
// Linha 2950-2962
if varBuscarTipo = tbIdTabela then
  cdsGeral.Data := DalProduto.SqlBuscarProdutoId(cdsBuscar.FieldByName(varBuscarIdTabela).AsFloat)
else
begin
  cdsBuscar.Data := DalProduto.SqlBuscarProduto(
    cbxTodosRegistros.AsInteger,     // Todos registros ou limitado
    cbxVisCampoPesquisado,           // Campo selecionado para pesquisar
    cbxCondicao,                      // Condição (Contém, Inicia com, etc)
    txtVisBuscar,                     // Texto digitado pelo usuário
    cbxVisCampoPesquisado2,          // Campo 2 (pesquisa avançada)
    cbxCondicao2,                     // Condição 2
    txtVisBuscar2,                    // Texto 2
    TipoConsulta,                     // Tipo: Produto ou Serviço
    0,                                // vlId
    cbxStatusReg,                     // Status (Ativo/Inativo)
    cbxListaVis.AsFloat,             // Lista de produtos
    cbxConsultasOpcoes.AsInteger,    // Consultas especiais
    cbxConsultasOpcoes2.AsInteger,   // Consultas de estoque
    txtVisEmpresas.IdLista,          // Empresas
    cbxMoedaVis.AsFloat,            // Moeda
    cbxVisData.AsInteger,            // Tipo de data
    txtVisDtInicial.AsDateTime,      // Data inicial
    txtVisDtFinal.AsDateTime,        // Data final
    cbxTipoItemVis.AsInteger,        // Tipo de item
    cbxErrosVis.AsInteger,           // Divergências
    cbxEstoque.AsInteger,            // Estoque
    varIdsLimitador,                 // IDs limitadores
    vIdsPromocoes,                   // IDs de promoções
    cbxCondicaoE,                    // Condição E
    txtVisBuscarE,                   // Busca E (localização)
    cbxProdEmpresa,                  // Produto por empresa
    varIdEmpresa,                    // ID empresa
    cbxProdInd.AsInteger,            // Produto individual
    cbxVisDataMov.AsInteger,         // Data movimento
    txtVisDtInicialMov.AsDateTime,   // Data inicial movimento
    txtVisDtFinalMov.AsDateTime,     // Data final movimento
    cbxEmpresaVisMov.AsFloat,        // Empresa movimento
    cbxSitSituacaoEstoque3.AsFloat,  // Situação estoque
    cbxSitLocalEstoque3.AsFloat,     // Local estoque
    txtHoraInicial.AsDateTime,       // Hora inicial
    txtHoraFinal.AsDateTime,         // Hora final
    cbxStatusImagem,                 // Status imagem
    cbxHStatusImagem,                // Hetosoft status imagem
    cbxSimilarVis,                   // ⭐ SIMILAR POR ID (já existe)
    cbxSugestaoVis,                  // Sugestão por ID
    cbxImgCodCliente,                // Imagem código cliente
    cbxEstatistica.AsInteger         // Estatística
  );

  Dados.Salvar_SQL;
  Dados.Mostrar_SQL;
  GetPromocaoVinculos;
end;
```

---

## 🎯 **FUNÇÃO DAL DE CONSULTA**

### **Arquivo:** `Sol.NET\Dal\uDalProduto.pas`

#### **1. Declaração da Função (Interface)**
- **Linha:** ~99-108
- **Função:** `SqlBuscarProduto`

#### **2. Implementação da Função**
- **Linha:** ~1048-2500
- **Onde monta a consulta SQL completa**

#### **3. Bloco de Filtro por Similar (ID) - Já Existente**
**Linha:** ~2347-2355

```pascal
// Similar por ID
if (objSimilarVis <> nil) and (objSimilarVis.AsFloat > 0) then
begin
  strAux.Clear;
  strAux.Append(' SELECT SIMM.ID_PRODUTO                                           ' + BR);
  strAux.Append(' FROM PRODUTO_SIMILARES SIMM  ' + SQL.WithNoLock + '             ' + BR);
  strAux.Append(' WHERE SIMM.ID_SIMILAR = ' + objSimilarVis.AsFloat.ToString + '  ' + BR);

  strSql.Append(' AND PROD.ID_PRODUTO IN (' + strAux.ToString + ') ');
end;
```

#### **4. Bloco de Filtro por Sugestão (Similar)**
**Linha:** ~2358-2366

```pascal
// Sugestao por ID
if (objSugestaoVis <> nil) and (objSugestaoVis.AsFloat > 0) then
begin
  strAux.Clear;
  strAux.Append(' SELECT SUGG.ID_PRODUTO                                             ' + BR);
  strAux.Append(' FROM PRODUTO_SUGESTAO SUGG  ' + SQL.WithNoLock + '                ' + BR);
  strAux.Append(' WHERE SUGG.ID_SUGESTAO = ' + objSugestaoVis.AsFloat.ToString + '  ' + BR);

  strSql.Append(' AND PROD.ID_PRODUTO IN (' + strAux.ToString + ') ');
end;
```

**📍 É APÓS ESTE BLOCO QUE VOCÊ ADICIONARÁ O FILTRO POR DESCRIÇÃO DO SIMILAR**

---

## ⚙️ **EVENTOS RELACIONADOS**

### **1. Mudança de Campo Pesquisado**
**Arquivo:** `uFrmCadastroProdutos.pas`
**Linha:** ~1687

```pascal
procedure TFrmCadastroProdutos.cbxVisCampoPesquisadoChange(Sender: TObject);
```

**Função:** Executado quando o usuário muda o campo a pesquisar no combo.

### **2. Ajuste de Comportamento do Campo de Busca**
**Arquivo:** `uFrmCadastroProdutos.pas`
**Linha:** ~25600-25750

```pascal
procedure TFrmCadastroProdutos.MudaStatusDaConsulta(Limpar: Boolean);
```

**Função:** Define o comportamento do campo `txtVisBuscar` de acordo com o campo selecionado:
- Se for lookup (ID): abre tela de consulta
- Se for texto: permite digitação livre
- Se for número: valida como número

**Exemplo de bloco relevante (linha ~25647):**
```pascal
// Com Texto
if (cbxVisCampoPesquisado.Text = 'Automático') or 
   (cbxVisCampoPesquisado.Text = 'Automático Condição') or
   (cbxVisCampoPesquisado.Text = 'Código Produto/Descrição') or 
   (cbxVisCampoPesquisado.Text = 'Descrição') or 
   (cbxVisCampoPesquisado.Text = 'Descrição/Códigos') or
   (varListaCamposCCC_Texto.IndexOf(cbxVisCampoPesquisado.Text) > -1) or 
   (cbxVisCampoPesquisado.Text = 'Código Fabricante') or
   (cbxVisCampoPesquisado.Text = 'Código Fornecedor') or 
   (cbxVisCampoPesquisado.Text = 'Código Barra') or 
   (cbxVisCampoPesquisado.Text = 'CST ICMS')
then
begin
  txtVisBuscar.Hint := '';
  txtVisBuscar.TextHint := 'Coloque o conteúdo';
  txtVisBuscar.AHS_TipoEdit := teString;
  txtVisBuscar.AHS_TrocarPontoVirg := False;
  txtVisBuscar.AHS_ReadOnly2 := False;
end;
```

### **3. Menu de Contexto - Localizar Similares**
**Arquivo:** `uFrmCadastroProdutos.pas`
**Linha:** ~23089

```pascal
procedure TFrmCadastroProdutos.mnLocalizarSimilaresClick(Sender: TObject);
```

**Função:** Menu de contexto que localiza todos os produtos com o mesmo similar do produto selecionado.

**Como funciona:**
1. Pega o produto selecionado na grid
2. Busca o ID_SIMILAR desse produto
3. Busca todos os produtos que têm o mesmo ID_SIMILAR
4. Exibe na grid de visualização

---

## 📊 **ESTRUTURA DE DADOS**

### **Tabelas Envolvidas:**

```sql
-- Tabela de Similares (mestre)
SIMILARES
├─ ID_SIMILAR (PK)
└─ DESCRICAO

-- Tabela de Vínculo Produto-Similar
PRODUTO_SIMILARES
├─ ID_PRODUTO_SIMILAR (PK)
├─ ID_PRODUTO (FK → PRODUTOS)
└─ ID_SIMILAR (FK → SIMILARES)

-- Tabela de Produtos
PRODUTOS
├─ ID_PRODUTO (PK)
├─ DESCRICAO
├─ PRODUTO_SIMILAR (flag: 0/1)
└─ ... outros campos
```

---

## 🔄 **FLUXO COMPLETO DE PESQUISA**

```
1. FormShow (Inicialização)
   ↓
   Linha ~2400-2500: Preenche combos e configurações
   ↓
   Linha ~15794: cbxSimilarVis.Preencher(cdsSistema, [0, 1])

2. Usuário seleciona campo no cbxVisCampoPesquisado
   ↓
   cbxVisCampoPesquisadoChange (linha ~1687)
   ↓
   MudaStatusDaConsulta (linha ~25600)
   ↓
   Define comportamento do txtVisBuscar

3. Usuário digita no txtVisBuscar

4. Usuário clica em Buscar (F5)
   ↓
   btnBuscar1Click
   ↓
   Procedure Buscar (linha ~2950)
   ↓
   DalProduto.SqlBuscarProduto (linha ~2955)
   ↓
   Monta SQL com todos os filtros (uDalProduto.pas linha ~1048-2500)
   ↓
   Linha ~2347: Aplica filtro de Similar por ID (se selecionado)
   ↓
   Executa consulta e retorna dados
   ↓
   Exibe na grid dbgBuscar
```

---

## 🎯 **ONDE ADICIONAR O FILTRO POR DESCRIÇÃO**

### **Para implementar a busca por descrição do Similar:**

#### **1. No arquivo `uDalProduto.pas`:**
- **Linha ~108:** Adicionar parâmetro na declaração
- **Linha ~1049:** Adicionar parâmetro na implementação
- **Linha ~2366:** Adicionar bloco SQL (após objSugestaoVis)

#### **2. No arquivo `uFrmCadastroProdutos.pas`:**
- **Linha ~2961:** Passar novo parâmetro na chamada
- **Linha ~25647:** Adicionar tratamento em MudaStatusDaConsulta

#### **3. No arquivo `uFrmCadastroProdutos.dfm`:**
- **Linha ~195:** Adicionar em AHS_ItemsID.Strings
- **Linha ~247:** Adicionar em Items.Strings

---

## 📚 **FUNÇÕES DE APOIO**

### **Preenchimento de Combos**
- **Linha ~15794:** `cbxSimilarVis.Preencher(cdsSistema, [0, 1])`
- Busca dados da tabela SIMILARES e preenche o combo

### **Salvamento de Configurações**
- `BuscarCampoGenerico` - Carrega configuração salva
- `SalvarCampoGenerico` - Salva seleção do usuário

### **Montagem de SQL**
- **uDalProduto.pas linha ~1100-2500:** Monta SQL complexo com múltiplos filtros
- Usa `strSql` (StringBuilder) para concatenar partes do SQL
- Usa `strAux` (StringBuilder) para subconsultas

---

## 💡 **DICA IMPORTANTE**

A lógica já está toda pronta para campos de texto! Você só precisa:

1. **Adicionar a opção no combo** (dfm)
2. **Passar o parâmetro correto** (quando "Similar (Descrição)" estiver selecionado)
3. **Implementar o SQL de busca** (espelhando-se no bloco de Sugestão)

O sistema já sabe:
- ✅ Como tratar campos de texto
- ✅ Como montar subconsultas com IN
- ✅ Como fazer LIKE com UPPER para case-insensitive
- ✅ Como combinar múltiplos filtros

---

**Data do Mapeamento:** 30/10/2025
**Issue:** #7303
**Desenvolvedor:** Copilot

