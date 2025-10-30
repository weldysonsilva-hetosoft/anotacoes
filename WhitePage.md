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

**Data do Mapeamento:** 30/10/2025
**Issue:** #7303
**Branch:** 7303-243174-solnet---adicionar-campo-a-pesquisar-codigo-similar-no-cadastro-de-produtos-onde-traz-todo
