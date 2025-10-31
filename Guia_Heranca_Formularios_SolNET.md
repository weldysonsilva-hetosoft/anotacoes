# 📘 Guia Completo: Herança de Formulários e Ciclo de Vida no Sol.NET

## 🎯 Objetivo

Este guia foi desenvolvido para ajudar desenvolvedores **iniciantes** no Sol.Net a entender como funcionam os formulários na arquitetura Sol.NET, com foco em:
- Eventos de ciclo de vida dos formulários
- Herança e polimorfismo aplicados
- Reutilização de código através de POO (Programação Orientada a Objetos)
- Uso prático do formulário base `TFrmHeranca`

---

## 📚 Índice

1. [Conceitos Fundamentais](#1-conceitos-fundamentais)
2. [Ciclo de Vida dos Formulários](#2-ciclo-de-vida-dos-formulários)
3. [Herança de Formulários no Sol.NET](#3-herança-de-formulários-no-solnet)
4. [O Formulário Base: TFrmHeranca](#4-o-formulário-base-tfrmheranca)
5. [Eventos Virtuais e Override](#5-eventos-virtuais-e-override)
6. [TFrmHerancaCopiar - Formulário Especializado](#6-tfrmherancacopiar---formulário-especializado)
7. [Exemplos Práticos](#7-exemplos-práticos)
8. [Boas Práticas](#8-boas-práticas)

---

## 1. Conceitos Fundamentais

### 1.1 O que é Herança?

**Herança** é um dos pilares da POO que permite criar uma nova classe baseada em uma classe existente, herdando suas características (propriedades e métodos) e podendo adicionar ou modificar funcionalidades.

**Analogia:** Imagine que você tem uma receita básica de bolo. Você pode criar variações (bolo de chocolate, bolo de cenoura) partindo dessa receita base, adicionando ou modificando ingredientes específicos.

### 1.2 Polimorfismo

**Polimorfismo** permite que classes filhas implementem comportamentos diferentes para o mesmo método herdado.

**No Sol.NET:** Quando um formulário filho sobrescreve (`override`) um método do formulário pai, ele está usando polimorfismo.

### 1.3 Reutilização de Código

A arquitetura do Sol.NET utiliza herança para evitar repetição de código:

```
TFrmHeranca (base)
    ↓
    ├─ TFrmHerancaCopiar
    ├─ TFrmCadastroProdutos
    ├─ TFrmCadastroClientes
    └─ ... outros formulários
```

Todos os formulários compartilham funcionalidades comuns (buscar, gravar, excluir) definidas em `TFrmHeranca`.

---

## 2. Ciclo de Vida dos Formulários

### 2.1 Eventos Nativos do Delphi

Os formulários Delphi possuem eventos nativos que são executados em momentos específicos:

#### **📌 FormCreate**
- **Quando ocorre:** Ao criar a instância do formulário (primeira vez)
- **Uso:** Inicializar variáveis, criar objetos, configurar estado inicial
- **Exemplo:**
```pascal
procedure TFrmHeranca.FormCreate(Sender: TObject);
begin
  FrmCriar; // Chama método virtual personalizado
end;
```

#### **📌 FormShow**
- **Quando ocorre:** Antes do formulário ser exibido na tela
- **Uso:** Carregar dados, atualizar interface
- **No Sol.NET:** Chama `FrmMostrar`

#### **📌 FormActivate**
- **Quando ocorre:** Quando o formulário recebe foco
- **Uso:** Atualizar dados que podem ter mudado
- **No Sol.NET:** Chama `FrmAtivar`

#### **📌 FormPaint**
- **Quando ocorre:** Quando o formulário precisa ser redesenhado
- **Uso:** Operações visuais customizadas
- **No Sol.NET:** Chama `FrmPintar`

#### **📌 FormDeactivate**
- **Quando ocorre:** Quando o formulário perde o foco
- **Uso:** Salvar estado, pausar operações

#### **📌 FormClose**
- **Quando ocorre:** Ao fechar o formulário
- **Uso:** Liberar recursos, salvar configurações
- **No Sol.NET:** Chama `FrmFechar`

#### **📌 FormDestroy**
- **Quando ocorre:** Ao destruir a instância do formulário
- **Uso:** Liberar memória, destruir objetos criados
- **No Sol.NET:** Chama `FrmDestroi`

### 2.2 Ordem de Execução

```
1. FormCreate    → FrmCriar
2. FormShow      → FrmMostrar
3. FormPaint     → FrmPintar
4. FormActivate  → FrmAtivar
   ↓
   (formulário em uso)
   ↓
5. FormDeactivate
6. FormClose     → FrmFechar
7. FormDestroy   → FrmDestroi
```

---

## 3. Herança de Formulários no Sol.NET

### 3.1 Arquitetura em Camadas

O Sol.NET usa uma estrutura de herança bem definida:

```
┌─────────────────────────────────────┐
│         TForm (Delphi)              │
│         (Classe Base VCL)           │
└─────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────┐
│        TFrmHeranca                  │
│   (Framework/uFrmHeranca.pas)       │
│                                     │
│  - Funcionalidades comuns           │
│  - CRUD básico                      │
│  - Gerenciamento de estado          │
│  - Permissões                       │
│  - Auditoria                        │
└─────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────┐
│     TFrmHerancaCopiar               │
│  (Framework/uFrmHerancaCopiar.pas)  │
│                                     │
│  - Herda TFrmHeranca                │
│  - Adiciona grid de busca (dbgBuscar)│
│  - Eventos de duplo clique          │
│  - Campos código/descrição          │
└─────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────┐
│   Formulários Específicos           │
│   (Sol.NET/FormEspecias/...)        │
│                                     │
│  - TFrmCadastroProdutos             │
│  - TFrmCadastroClientes             │
│  - TFrmCadastroPessoas              │
│  - etc.                             │
└─────────────────────────────────────┘
```

### 3.2 Vantagens da Herança

✅ **Reutilização:** Código escrito uma vez, usado em vários lugares
✅ **Manutenção:** Correção em um local afeta todos os filhos
✅ **Consistência:** Comportamento uniforme em toda aplicação
✅ **Produtividade:** Desenvolvimento mais rápido de novos formulários
✅ **Organização:** Código estruturado e fácil de entender

---

## 4. O Formulário Base: TFrmHeranca

### 4.1 Localização

**Arquivo:** `Framework/uFrmHeranca.pas`

### 4.2 Responsabilidades

O `TFrmHeranca` é o **coração** de todos os formulários do Sol.NET. Ele fornece:

#### **🔹 Gerenciamento de Estado**
```pascal
FEstado: string;  // Estados: 'B' (Browse), 'I' (Insert), 'E' (Edit)
```

Estados possíveis:
- **'B'** (Browse): Navegando/visualizando registros
- **'I'** (Insert): Inserindo novo registro
- **'E'** (Edit): Editando registro existente

#### **🔹 CRUD Completo**
- **SqlBuscar:** Busca registros do banco
- **SqlGravar:** Salva novo registro ou alteração
- **SqlExcluir:** Exclui registro
- **SqlBuscarDetalhes:** Busca detalhes/itens relacionados

#### **🔹 Componentes Visuais Padrão**
```pascal
pagCadastro: TPageControl;     // Abas principal
  ├─ tabVisualizar             // Aba de visualização/busca
  └─ tabCadastro               // Aba de cadastro/edição

btnNovo: TBitBtn;              // Botão Novo
btnAlterar: TBitBtn;           // Botão Alterar
btnGravar: TBitBtn;            // Botão Gravar
btnExcluir: TBitBtn;           // Botão Excluir
btnDesfazer: TBitBtn;          // Botão Cancelar
btnSair: TBitBtn;              // Botão Sair

cdsBuscar: TClientDataSet;     // Dataset de busca
cdsGeral: TClientDataSet;      // Dataset principal
dsBuscar: TDataSource;         // DataSource de busca
dsGeral: TDataSource;          // DataSource principal
```

#### **🔹 Funcionalidades Adicionais**
- ✅ Controle de permissões
- ✅ Auditoria de alterações
- ✅ Registro em uso (evitar edição simultânea)
- ✅ Validação de dados
- ✅ Gerenciamento de imagens
- ✅ Campos complementares
- ✅ Exportação de dados
- ✅ Totalizador de registros
- ✅ Memorização de pesquisas

### 4.3 Variáveis Importantes

```pascal
// Configuração de busca
varBuscarTabela: string;           // Nome da tabela no banco
varBuscarIdTabela: string;         // Campo ID (chave primária)
varBuscarCampoCodigo: string;      // Campo código
varBuscarCampoDescricao: string;   // Campo descrição
varBuscarOrdem: string;            // Ordenação SQL

// Modos de operação
varModoPesquisar: Boolean;         // Modo pesquisa (para seleção)
varShowModal: Boolean;             // Aberto como modal
varClonarRegistro: Boolean;        // Está clonando um registro

// Comportamento
varUsarEnter: Boolean;             // Enter funciona como Tab
varModoPesquisarConsulta: Boolean; // Pesquisa rápida
```

---

## 5. Eventos Virtuais e Override

### 5.1 O que são Métodos Virtuais?

Métodos **virtuais** podem ser sobrescritos (`override`) pelas classes filhas.

```pascal
// Na classe pai (TFrmHeranca)
procedure FrmCriar; virtual;
```

```pascal
// Na classe filha (TFrmCadastroProdutos)
procedure FrmCriar; override;
```

### 5.2 Principais Métodos Virtuais

#### **📌 FrmCriar**
```pascal
procedure TFrmHeranca.FrmCriar;
```
**Quando:** Criação do formulário (FormCreate)
**Uso:** 
- Configurar variáveis de busca
- Criar objetos DAL
- Definir estrutura do banco
- Carregar configurações iniciais

**Exemplo:**
```pascal
procedure TFrmCadastroProdutos.FrmCriar;
begin
  inherited; // SEMPRE chame inherited primeiro!
  
  // Configurar busca
  varBuscarTabela := 'PRODUTOS';
  varBuscarIdTabela := 'ID_PRODUTO';
  varBuscarCampoDescricao := 'DESCRICAO';
  varBuscarCampoCodigo := 'CODIGO';
  
  // Criar DAL específico
  DalProduto := TDalProduto.Create(Self);
  
  // Executar busca inicial
  SqlBuscar;
end;
```

#### **📌 FrmMostrar**
```pascal
procedure TFrmHeranca.FrmMostrar;
```
**Quando:** Exibição do formulário (FormShow)
**Uso:**
- Carregar combos
- Atualizar dados
- Ajustar layout

#### **📌 FrmPintar**
```pascal
procedure TFrmHeranca.FrmPintar;
```
**Quando:** Redesenho do formulário (FormPaint)
**Uso:** Ajustes visuais, cores, estilos

#### **📌 FrmAtivar**
```pascal
procedure TFrmHeranca.FrmAtivar;
```
**Quando:** Formulário recebe foco (FormActivate)
**Uso:** Atualizar dados que podem ter mudado

#### **📌 FrmFechar**
```pascal
procedure TFrmHeranca.FrmFechar;
```
**Quando:** Fechamento do formulário (FormClose)
**Uso:**
- Salvar configurações
- Liberar recursos
- Verificar alterações não salvas

#### **📌 FrmDestroi**
```pascal
procedure TFrmHeranca.FrmDestroi;
```
**Quando:** Destruição do formulário (FormDestroy)
**Uso:**
- Destruir objetos criados
- Liberar memória
- Limpar registros temporários

### 5.3 Métodos de Dados (CRUD)

#### **📌 SqlBuscar**
```pascal
procedure TFrmHeranca.SqlBuscar; virtual;
```

**Responsabilidade:** Buscar registros do banco de dados

**Implementação Base:**
```pascal
procedure TFrmHeranca.SqlBuscar;
begin
  ProcessarTempo; // Medir tempo de execução
  
  try
    // Busca por código ou descrição
    if varBuscarTipo = tbDescricaoCodigo then
    begin
      if Geral.SoTemNumero(txtVisBuscar.Text) then
        cdsBuscar.Data := Dados.BuscarGeralAtivo(
          varBuscarTabela, 
          varBuscarCampoCodigo, 
          txtVisBuscar.Text, 
          varBuscarOrdem, 
          cbxStatusReg
        )
      else
        cdsBuscar.Data := Dados.BuscarGeralAtivo(
          varBuscarTabela, 
          varBuscarCampoDescricao, 
          txtVisBuscar.Text, 
          varBuscarOrdem, 
          cbxStatusReg
        );
    end;
    
    // Atualiza botões e totalizador
    Botoes(dsBuscar, btnNovo, btnAlterar, btnGravar, btnExcluir, btnDesfazer);
    Totalizador(dsBuscar);
    
  finally
    varBuscarTipo := varBuscarTipoDefault;
  end;
end;
```

**Override em Filho:**
```pascal
procedure TFrmCadastroProdutos.SqlBuscar; 
begin
  // NÃO chame inherited se quiser substituir completamente
  
  try
    // Busca customizada com filtros específicos
    cdsBuscar.Data := DalProduto.SqlBuscarProduto(
      cbxTodosRegistros.AsInteger,
      cbxVisCampoPesquisado,
      cbxCondicao,
      txtVisBuscar,
      // ... muitos outros parâmetros
    );
    
    Botoes(dsBuscar, btnNovo, btnAlterar, btnGravar, btnExcluir, btnDesfazer);
    Totalizador(dsBuscar);
    
  finally
    varBuscarTipo := varBuscarTipoDefault;
  end;
end;
```

#### **📌 SqlGravar**
```pascal
function TFrmHeranca.SqlGravar: Boolean; virtual;
```

**Responsabilidade:** Gravar dados no banco (INSERT ou UPDATE)

**Retorno:** `True` se gravou com sucesso, `False` se houve erro

**Uso Típico:**
```pascal
function TFrmMeuForm.SqlGravar: Boolean;
begin
  Result := False;
  
  try
    Dados.IniciarTransaction;
    
    // Gravar tabela principal
    Dados.GravarCds(cdsGeral, 'MINHA_TABELA', True, True);
    
    // Gravar detalhes se houver
    if not cds.EstaVazio(cdsDetalhes) then
      Dados.GravarCds(cdsDetalhes, 'MINHA_TABELA_ITENS', True, True);
    
    Dados.CommitTransaction;
    Result := True;
    
  except
    on E: Exception do
    begin
      Dados.RollbackTransaction;
      Geral.MenErro('Erro ao gravar: ' + E.Message);
    end;
  end;
end;
```

#### **📌 SqlExcluir**
```pascal
procedure TFrmHeranca.SqlExcluir; virtual;
```

**Responsabilidade:** Excluir registro do banco

#### **📌 SqlBuscarDetalhes**
```pascal
procedure TFrmHeranca.SqlBuscarDetalhes; virtual;
```

**Responsabilidade:** Buscar itens relacionados (detalhes)

**Exemplo:** Ao selecionar um produto, buscar seus códigos de barra, preços, etc.

### 5.4 Métodos de Validação e Transporte

#### **📌 Validar**
```pascal
function TFrmHeranca.Validar: Boolean; virtual;
```

**Responsabilidade:** Validar dados antes de gravar

**Retorno:** `True` se validação passou, `False` se há erros

**Exemplo:**
```pascal
function TFrmCadastroProdutos.Validar: Boolean;
begin
  Result := False;
  
  // Validação obrigatória
  if Trim(cdsGeral.FieldByName('DESCRICAO').AsString) = '' then
  begin
    Geral.Men('Descrição é obrigatória!');
    txtDescricao.SetFocus;
    Exit;
  end;
  
  // Validação de valor
  if cdsGeral.FieldByName('PRECO_VENDA').AsFloat <= 0 then
  begin
    Geral.Men('Preço deve ser maior que zero!');
    txtPreco.SetFocus;
    Exit;
  end;
  
  Result := True;
end;
```

#### **📌 Transportar**
```pascal
procedure TFrmHeranca.Transportar; virtual;
```

**Responsabilidade:** Transportar dados da tela para o dataset (cdsGeral)

**Quando:** Antes de gravar, move valores dos campos visuais para o ClientDataSet

**Exemplo:**
```pascal
procedure TFrmCadastroProdutos.Transportar;
begin
  inherited; // Chama transporte padrão
  
  // Transportes customizados
  cdsGeral.FieldByName('DESCRICAO').AsString := txtDescricao.Text;
  cdsGeral.FieldByName('PRECO_VENDA').AsFloat := txtPreco.AsFloat;
  cdsGeral.FieldByName('ID_FABRICANTE').AsFloat := txtFabricante.Id;
  
  // Campos calculados
  cdsGeral.FieldByName('MARGEM_LUCRO').AsFloat := CalcularMargem;
end;
```

### 5.5 Métodos de Interface

#### **📌 TabVisualizarMostrar**
```pascal
procedure TFrmHeranca.TabVisualizarMostrar; virtual;
```

**Quando:** Ao exibir aba de visualização
**Uso:** Atualizar grid, carregar dados

#### **📌 TabCadastroMostrar**
```pascal
procedure TFrmHeranca.TabCadastroMostrar; virtual;
```

**Quando:** Ao exibir aba de cadastro
**Uso:** Preparar campos para edição

---

## 6. TFrmHerancaCopiar - Formulário Especializado

### 6.1 Propósito

`TFrmHerancaCopiar` é uma **especialização** de `TFrmHeranca` que adiciona:

✅ Grid de busca (`dbgBuscar: TDBGridPlus`)
✅ Campos de código e descrição
✅ Eventos de duplo clique no grid
✅ Pesquisa rápida via teclado

### 6.2 Estrutura

**Arquivo:** `Framework/uFrmHerancaCopiar.pas`

```pascal
type
  TFrmHerancaCopiar = class(TFrmHeranca)
    dbgBuscar: TDBGridPlus;        // Grid de busca
    txtCodigo: TGenEdit;           // Campo código
    txtDescricao: TGenEdit;        // Campo descrição
    
  protected
    procedure FrmCriar; override;
    procedure SqlBuscar; override;
    // ... outros overrides
  end;
```

### 6.3 Funcionalidades Adicionais

#### **🔹 Duplo Clique no Grid**
```pascal
procedure TFrmHerancaCopiar.dbgBuscarDblClick(Sender: TObject);
```
Permite selecionar registro com duplo clique

#### **🔹 Navegação por Teclado**
```pascal
procedure TFrmHerancaCopiar.dbgBuscarKeyDown(Sender: TObject; 
  var Key: Word; Shift: TShiftState);
```
- **Enter:** Seleciona registro
- **Esc:** Fecha formulário
- **F2:** Abre cadastro

### 6.4 Quando Usar

Use `TFrmHerancaCopiar` como base quando seu formulário precisa de:
- ✅ Grid de busca simples
- ✅ Pesquisa por código ou descrição
- ✅ Seleção rápida de registros
- ✅ Cadastros auxiliares (Marcas, Unidades, etc.)

**Exemplo:** Formulário de Cadastro de Unidades, Marcas, Grupos, etc.

---

## 7. Exemplos Práticos

### 7.1 Criar um Formulário Simples

**Cenário:** Criar formulário de cadastro de Departamentos

#### **Passo 1: Criar a Unit**
```pascal
unit uFrmCadastroDepartamentos;

interface

uses
  uFrmHerancaCopiar, // Herda de HerancaCopiar
  System.Classes, Vcl.Forms;

type
  TFrmCadastroDepartamentos = class(TFrmHerancaCopiar)
  protected
    procedure FrmCriar; override;
    procedure SqlBuscar; override;
    function Validar: Boolean; override;
  end;

var
  FrmCadastroDepartamentos: TFrmCadastroDepartamentos;

implementation

{$R *.dfm}

uses uDalConexao, uFuncoesGeral;

procedure TFrmCadastroDepartamentos.FrmCriar;
begin
  inherited; // SEMPRE!
  
  // Configurar variáveis de busca
  varBuscarTabela := 'DEPARTAMENTOS';
  varBuscarIdTabela := 'ID_DEPARTAMENTO';
  varBuscarCampoDescricao := 'DESCRICAO';
  varBuscarCampoCodigo := 'CODIGO';
  varBuscarOrdem := 'DESCRICAO';
  varCampoMessagemParaExclusao := 'DESCRICAO';
  
  // Executar busca inicial
  SqlBuscar;
end;

procedure TFrmCadastroDepartamentos.SqlBuscar;
begin
  inherited; // Usa busca padrão do HerancaCopiar
end;

function TFrmCadastroDepartamentos.Validar: Boolean;
begin
  Result := False;
  
  // Validar descrição
  if Trim(cdsGeral.FieldByName('DESCRICAO').AsString) = '' then
  begin
    Geral.Men('Descrição é obrigatória!');
    txtDescricao.SetFocus;
    Exit;
  end;
  
  Result := True;
end;

end.
```

#### **Passo 2: No DFM (Visual)**
- Herda visualmente de `TFrmHerancaCopiar`
- Já possui grid, botões, abas
- Adicione apenas campos específicos na aba de cadastro

### 7.2 Sobrescrever SqlBuscar Customizado

**Cenário:** Busca com filtros avançados

```pascal
procedure TFrmCadastroProdutos.SqlBuscar;
begin
  // NÃO chame inherited - implementação completamente nova
  
  try
    ProcessarTempo;
    
    if varBuscarTipo = tbIdTabela then
    begin
      // Buscar por ID específico
      cdsGeral.Data := DalProduto.SqlBuscarProdutoId(
        cdsBuscar.FieldByName(varBuscarIdTabela).AsFloat
      );
    end
    else
    begin
      // Busca avançada com múltiplos filtros
      cdsBuscar.Data := DalProduto.SqlBuscarProduto(
        cbxTodosRegistros.AsInteger,
        cbxVisCampoPesquisado,
        cbxCondicao,
        txtVisBuscar,
        cbxVisCampoPesquisado2,
        cbxCondicao2,
        txtVisBuscar2,
        TipoConsulta,
        0,
        cbxStatusReg,
        cbxListaVis.AsFloat,
        // ... outros filtros
      );
    end;
    
    Botoes(dsBuscar, btnNovo, btnAlterar, btnGravar, btnExcluir, btnDesfazer);
    Totalizador(dsBuscar);
    
  finally
    varBuscarTipo := varBuscarTipoDefault;
  end;
end;
```

### 7.3 Adicionar Funcionalidade Específica

**Cenário:** Adicionar botão de importação

```pascal
type
  TFrmMeuCadastro = class(TFrmHerancaCopiar)
    btnImportar: TBitBtn;
    procedure btnImportarClick(Sender: TObject);
  protected
    procedure FrmCriar; override;
    procedure Funcoes; override; // Sobrescrever menu Funções
  end;

implementation

procedure TFrmMeuCadastro.FrmCriar;
begin
  inherited;
  
  // Configurações...
  
  // Mostrar botão Funções
  pnlSeparador.Visible := True;
  btnFuncoes.Visible := True;
end;

procedure TFrmMeuCadastro.Funcoes;
begin
  inherited; // Mantém funcionalidades padrão
  
  // Adicionar menu customizado
  if Geral.MenConfirmar('Deseja importar dados?') then
    ImportarDados;
end;

procedure TFrmMeuCadastro.btnImportarClick(Sender: TObject);
begin
  // Lógica de importação
  if odGeral.Execute then
  begin
    ImportarArquivo(odGeral.FileName);
    SqlBuscar; // Atualizar grid
  end;
end;
```

### 7.4 Trabalhar com Detalhes (Mestre-Detalhe)

**Cenário:** Produto com itens (códigos de barra)

```pascal
procedure TFrmCadastroProdutos.SqlBuscarDetalhes;
begin
  inherited;
  
  // Buscar códigos de barra do produto
  cdsCodigosBarra.Data := DalProduto.SqlBuscarCodigoBarra(
    cdsGeral.FieldByName('ID_PRODUTO').AsFloat
  );
  
  // Buscar preços progressivos
  cdsPrecosProgressivos.Data := DalProduto.SqlBuscarPrecosProgressivos(
    cdsGeral.FieldByName('ID_PRODUTO').AsFloat
  );
end;

function TFrmCadastroProdutos.SqlGravar: Boolean;
begin
  Result := False;
  
  try
    Dados.IniciarTransaction;
    
    // Gravar produto (tabela principal)
    Dados.GravarCds(cdsGeral, 'PRODUTOS', True, True);
    
    // Gravar códigos de barra (detalhe 1)
    if not cds.EstaVazio(cdsCodigosBarra) then
      Dados.GravarCds(cdsCodigosBarra, 'PRODUTO_CODIGOS', True, True);
    
    // Gravar preços (detalhe 2)
    if not cds.EstaVazio(cdsPrecosProgressivos) then
      Dados.GravarCds(cdsPrecosProgressivos, 'PRODUTO_PRECOS', True, True);
    
    Dados.CommitTransaction;
    Result := True;
    
  except
    on E: Exception do
    begin
      Dados.RollbackTransaction;
      Geral.MenErro('Erro ao gravar: ' + E.Message);
    end;
  end;
end;
```

---

## 8. Boas Práticas

### 8.1 Sempre Chame `inherited`

```pascal
// ✅ CORRETO
procedure TFrmMeuForm.FrmCriar;
begin
  inherited; // Executa lógica da classe pai primeiro
  
  // Seu código aqui
end;

// ❌ ERRADO
procedure TFrmMeuForm.FrmCriar;
begin
  // inherited; // Esqueceu de chamar!
  
  // Seu código aqui
end;
```

**Exceção:** Quando você quer **substituir completamente** o comportamento (ex: `SqlBuscar` customizado)

### 8.2 Configure Variáveis em FrmCriar

```pascal
procedure TFrmMeuForm.FrmCriar;
begin
  inherited;
  
  // ✅ Configure TODAS as variáveis necessárias
  varBuscarTabela := 'MINHA_TABELA';
  varBuscarIdTabela := 'ID_TABELA';
  varBuscarCampoDescricao := 'DESCRICAO';
  varBuscarOrdem := 'DESCRICAO';
  
  // Execute busca inicial
  SqlBuscar;
end;
```

### 8.3 Validação Completa

```pascal
function TFrmMeuForm.Validar: Boolean;
begin
  Result := False; // Assume falha
  
  // Validar campos obrigatórios
  if Trim(cdsGeral.FieldByName('CAMPO1').AsString) = '' then
  begin
    Geral.Men('Campo 1 é obrigatório!');
    txtCampo1.SetFocus;
    Exit;
  end;
  
  // Validar valores
  if cdsGeral.FieldByName('VALOR').AsFloat < 0 then
  begin
    Geral.Men('Valor não pode ser negativo!');
    txtValor.SetFocus;
    Exit;
  end;
  
  // Validar duplicidade
  if ExisteRegistroDuplicado then
  begin
    Geral.Men('Registro já existe!');
    Exit;
  end;
  
  Result := True; // Passou em todas as validações
end;
```

### 8.4 Tratamento de Erros

```pascal
procedure TFrmMeuForm.FrmCriar;
begin
  inherited;
  
  try
    // Configurações
    varBuscarTabela := 'MINHA_TABELA';
    
    // Busca inicial
    SqlBuscar;
    
  except
    on E: Exception do
    begin
      Geral.MenErro('Erro ao inicializar: ' + E.Message);
      Close; // Fechar formulário se houver erro crítico
    end;
  end;
end;
```

### 8.5 Liberar Recursos em FrmDestroi

```pascal
procedure TFrmMeuForm.FrmDestroi;
begin
  // Destruir objetos criados
  FreeAndNil(MeuObjeto);
  FreeAndNil(MinhaLista);
  
  inherited; // Chamar no final para liberar recursos do pai
end;
```

### 8.6 Use Try-Finally em Transações

```pascal
function TFrmMeuForm.SqlGravar: Boolean;
begin
  Result := False;
  
  Dados.IniciarTransaction;
  try
    try
      // Operações de gravação
      Dados.GravarCds(cdsGeral, 'TABELA', True, True);
      
      Dados.CommitTransaction;
      Result := True;
      
    except
      on E: Exception do
      begin
        Dados.RollbackTransaction;
        Geral.MenErro('Erro: ' + E.Message);
      end;
    end;
  finally
    // Limpeza adicional se necessário
  end;
end;
```

### 8.7 Documentação de Código

```pascal
/// <summary>
/// Busca produtos com filtros avançados
/// </summary>
/// <remarks>
/// Permite buscar por código, descrição, fabricante,
/// grupo, e diversos outros filtros
/// </remarks>
procedure TFrmCadastroProdutos.SqlBuscar;
begin
  // Implementação
end;
```

---

## 9. Fluxograma de Operações

### 9.1 Fluxo de Inclusão

```
Usuário clica em "Novo"
    ↓
btnNovoClick
    ↓
Novo(dsGeral)
    ↓
- Muda estado para 'I' (Insert)
- cdsGeral.Insert
- Habilita campos
- Desabilita botões (exceto Gravar/Cancelar)
    ↓
Usuário preenche dados
    ↓
Usuário clica em "Gravar"
    ↓
btnGravarClick
    ↓
Gravar
    ↓
- Validar: Boolean
    ↓ (se True)
- Transportar
    ↓
- SqlGravar: Boolean
    ↓ (se True)
- cdsGeral.Post
- Auditoria
- Retorna ao modo 'B' (Browse)
- Atualiza grid
```

### 9.2 Fluxo de Alteração

```
Usuário seleciona registro no grid
    ↓
Usuário clica em "Alterar"
    ↓
btnAlterarClick
    ↓
Alterar(dsGeral)
    ↓
- Verifica se registro está em uso
- Muda estado para 'E' (Edit)
- cdsGeral.Edit
- Habilita campos
- Bloqueia registro para outros usuários
    ↓
Usuário modifica dados
    ↓
Usuário clica em "Gravar"
    ↓
(mesmo fluxo de inclusão a partir daqui)
```

### 9.3 Fluxo de Exclusão

```
Usuário seleciona registro no grid
    ↓
Usuário clica em "Excluir"
    ↓
btnExcluirClick
    ↓
- Verifica permissão
- Confirma com usuário
    ↓ (se confirmado)
SqlExcluir
    ↓
- Executa DELETE no banco
- Auditoria
- Atualiza grid
```

---

## 10. Perguntas Frequentes (FAQ)

### ❓ Quando devo chamar `inherited`?

**R:** Na maioria dos casos, SEMPRE! Exceto quando você quer substituir completamente o comportamento do método pai.

### ❓ Qual a diferença entre `TFrmHeranca` e `TFrmHerancaCopiar`?

**R:** 
- `TFrmHeranca`: Base pura, sem grid de busca
- `TFrmHerancaCopiar`: Adiciona grid e funcionalidades de pesquisa rápida

### ❓ Por que meu `SqlBuscar` não está funcionando?

**R:** Verifique se:
1. Você configurou `varBuscarTabela` e outras variáveis em `FrmCriar`
2. Chamou `inherited` se estiver usando busca padrão
3. Não há erros de sintaxe SQL

### ❓ Como adicionar um novo campo ao formulário?

**R:**
1. Adicione o campo visual no DFM (TextBox, ComboBox, etc.)
2. Em `Transportar`, transporte o valor para o cdsGeral
3. Em `Validar`, adicione validação se necessário
4. Em `TabCadastroMostrar`, carregue dados se for lookup

### ❓ O que é `Estado` e quais valores pode ter?

**R:** 
- **'B'** (Browse): Navegando
- **'I'** (Insert): Inserindo
- **'E'** (Edit): Editando

### ❓ Como trabalhar com tabelas relacionadas (mestre-detalhe)?

**R:** Use `SqlBuscarDetalhes` para carregar itens relacionados e grave todos os datasets em `SqlGravar` dentro de uma transação.

### ❓ Posso criar um formulário que não herda de nada?

**R:** Tecnicamente sim, mas você perderá TODAS as funcionalidades do framework (CRUD, permissões, auditoria, etc.). Não é recomendado.

---

## 11. Glossário

| Termo | Significado |
|-------|-------------|
| **CRUD** | Create, Read, Update, Delete - operações básicas de banco |
| **Override** | Sobrescrever método da classe pai |
| **Virtual** | Método que pode ser sobrescrito |
| **Inherited** | Chama implementação da classe pai |
| **ClientDataSet** | Dataset em memória (Delphi) |
| **DataSource** | Ligação entre dataset e componentes visuais |
| **DAL** | Data Access Layer - camada de acesso a dados |
| **Transação** | Conjunto de operações tratadas como unidade |
| **Polimorfismo** | Mesma interface, comportamentos diferentes |
| **Herança** | Classe filha herda características da pai |

---

## 12. Recursos Adicionais

### 📚 Arquivos de Referência

- `Framework/uFrmHeranca.pas` - Formulário base
- `Framework/uFrmHerancaCopiar.pas` - Formulário com grid
- `Sol.NET/FormEspecias/uFrmCadastroProdutos.pas` - Exemplo complexo

### 🔗 Documentação Relacionada

- Manual do Sol.NET: https://hetosoft.com.br/Arquivos/Manual/index.htm
- Documentação de Conversão: `/Documentacao/Conversao/`
- Documentação de Integrações: `/Documentacao/Integracoes/`

### 💡 Dicas Finais

1. **Estude exemplos:** Abra formulários existentes e veja como foram implementados
2. **Comece simples:** Faça formulários auxiliares antes dos complexos
3. **Use comentários:** Documente código difícil de entender
4. **Teste sempre:** Teste inclusão, alteração, exclusão e busca
5. **Peça ajuda:** Consulte desenvolvedores seniores quando em dúvida

---

## 13. Exemplo Completo Comentado

```pascal
unit uFrmCadastroMarcas;

interface

uses
  uFrmHerancaCopiar,
  System.Classes, Vcl.Forms, Vcl.Controls;

type
  /// <summary>
  /// Formulário de cadastro de marcas/fabricantes
  /// Herda de TFrmHerancaCopiar para aproveitar grid de busca
  /// </summary>
  TFrmCadastroMarcas = class(TFrmHerancaCopiar)
  protected
    // Eventos do ciclo de vida
    procedure FrmCriar; override;
    procedure FrmMostrar; override;
    procedure FrmFechar; override;
    
    // Métodos de dados
    procedure SqlBuscar; override;
    function Validar: Boolean; override;
  end;

var
  FrmCadastroMarcas: TFrmCadastroMarcas;

implementation

{$R *.dfm}

uses 
  uDalConexao, 
  uFuncoesGeral, 
  uFuncoesCds,
  uVariaveisGlobais;

{ TFrmCadastroMarcas }

procedure TFrmCadastroMarcas.FrmCriar;
begin
  // SEMPRE chame inherited primeiro
  inherited;
  
  // Configurar variáveis de busca
  varBuscarTabela := 'FABRICANTES';           // Tabela no banco
  varBuscarIdTabela := 'ID_FABRICANTE';       // Chave primária
  varBuscarCampoDescricao := 'DESCRICAO';     // Campo para busca por texto
  varBuscarCampoCodigo := 'CODIGO';           // Campo para busca por código
  varBuscarOrdem := 'DESCRICAO';              // Ordenação padrão
  varCampoMessagemParaExclusao := 'DESCRICAO'; // Campo mostrado ao excluir
  
  // Definir tipo de busca padrão
  varBuscarTipo := tbDescricaoCodigo;         // Aceita código OU descrição
  varBuscarTipoDefault := tbDescricaoCodigo;
  
  // Executar busca inicial
  try
    SqlBuscar;
  except
    on E: Exception do
    begin
      Geral.MenErro('Erro ao carregar marcas: ' + E.Message);
      Close;
    end;
  end;
end;

procedure TFrmCadastroMarcas.FrmMostrar;
begin
  inherited;
  
  // Ajustar caption do formulário
  Self.Caption := 'Cadastro de Marcas';
  
  // Configurações visuais adicionais podem ir aqui
end;

procedure TFrmCadastroMarcas.FrmFechar;
begin
  // Liberar recursos específicos deste formulário
  // (se houver objetos criados)
  
  inherited; // Libera recursos do pai
end;

procedure TFrmCadastroMarcas.SqlBuscar;
begin
  // Usa a busca padrão do TFrmHerancaCopiar
  inherited;
  
  // Se precisasse customizar, faria aqui
  // Mas para este caso simples, inherited é suficiente
end;

function TFrmCadastroMarcas.Validar: Boolean;
begin
  // Assume falha até provar o contrário
  Result := False;
  
  // Validar descrição obrigatória
  if Trim(cdsGeral.FieldByName('DESCRICAO').AsString) = '' then
  begin
    Geral.Men('Descrição da marca é obrigatória!');
    txtDescricao.SetFocus;
    Exit;
  end;
  
  // Validar tamanho mínimo
  if Length(Trim(cdsGeral.FieldByName('DESCRICAO').AsString)) < 3 then
  begin
    Geral.Men('Descrição deve ter no mínimo 3 caracteres!');
    txtDescricao.SetFocus;
    Exit;
  end;
  
  // Validar duplicidade
  cds.Limpar(cdsAux1);
  cdsAux1.Data := Dados.QryOpenOle(
    'SELECT * FROM FABRICANTES WHERE ' +
    'UPPER(DESCRICAO) = ' + QuotedStr(UpperCase(Trim(txtDescricao.Text))) +
    ' AND ID_FABRICANTE <> ' + cdsGeral.FieldByName('ID_FABRICANTE').AsString
  );
  
  if not cds.EstaVazio(cdsAux1) then
  begin
    Geral.Men('Já existe uma marca com esta descrição!');
    txtDescricao.SetFocus;
    Exit;
  end;
  
  // Passou em todas as validações
  Result := True;
end;

end.
```

---

## 🎓 Conclusão

Este guia apresentou os conceitos fundamentais de herança de formulários no Sol.NET:

✅ **Ciclo de vida:** Entendeu quando cada evento ocorre
✅ **Herança:** Aprendeu como reutilizar código
✅ **Polimorfismo:** Viu como customizar comportamentos
✅ **TFrmHeranca:** Conheceu o formulário base
✅ **TFrmHerancaCopiar:** Viu especialização com grid
✅ **Boas práticas:** Aprendeu padrões de desenvolvimento

### Próximos Passos

1. **Pratique:** Crie formulários simples
2. **Estude:** Analise formulários existentes
3. **Experimente:** Sobrescreva métodos e veja o resultado
4. **Aprofunde:** Estude DAL e banco de dados

---

**Desenvolvido para:** Projeto Sol.NET ERP
**Data:** Outubro de 2025
**Versão:** 1.0
**Autor:** Copilot AI Assistant

---

💡 **Lembre-se:** A melhor forma de aprender é praticando! Comece com formulários simples e vá evoluindo gradualmente.

🚀 **Bom desenvolvimento!**

[Documentação Básica de Programação Sol.NET.pdf](https://github.com/user-attachments/files/23264362/Documentacao.Basica.de.Programacao.Sol.NET.pdf)

## **📚 Respostas às Questões Fundamentais**

### **1️⃣ O que é uma variável?**

Uma variável é um espaço nomeado na memória que armazena um valor que pode ser alterado durante a execução do programa.

**No padrão Sol.NET (Delphi 12.2):**

delphi

`// Declaração inline (padrão do projeto)
var MinhaVariavel: Integer := 0;
var NomeCliente: string := 'João Silva';
var PrecoUnitario: Double := 10.50;
var DataCadastro: TDateTime := Now;`

### **2️⃣ O que é uma classe?**

Uma classe é um modelo/template que define a estrutura e comportamento de objetos. É como uma "planta" que determina:

- **Propriedades** (características)
- **Métodos** (ações/comportamentos)
- **Eventos** (reações a acontecimentos)

**Exemplo no Sol.NET:**

delphi

`type
  TIntegracaoBase = class(TInterfacedObject, IIntegracaoBase)
  private
    FNomeIntegracao: string;  // Campo privado
    FDados: TDados;
  protected
    procedure LogAdd(Msg: string); virtual;  // Método protegido
  public
    constructor Create(Owner: TForm; Dados: TDados);
    property NomeIntegracao: string read FNomeIntegracao write FNomeIntegracao;
  end;`

### **3️⃣ O que é um objeto?**

Um objeto é uma **instância concreta** de uma classe. É a classe "materializada" na memória.

**Exemplo:**

delphi

`// TClientDataSet é a CLASSE
// cdsBuscar é o OBJETO (instância da classe)
var cdsBuscar: TClientDataSet := TClientDataSet.Create(Self);

// Outro exemplo
var MinhaIntegracao: TIntegracaoBase := TIntegracaoBase.Create(Self, Dados);`

### **4️⃣ O que é um método?**

Um método é uma **função ou procedimento** que pertence a uma classe e define um comportamento/ação que o objeto pode executar.

**Exemplo no contexto de formulários:**

delphi

`type
  TFrmProdutos = class(TFrmHeranca)
  private
    procedure ConfigurarGrid;  // Método privado
  protected
    procedure FrmMostrar; override;  // Método protegido (sobrescrito)
  public
    procedure BuscarProdutos;  // Método público
    function ValidarCodigoBarras(Codigo: string): Boolean;  // Função
  end;

implementation

procedure TFrmProdutos.BuscarProdutos;
begin
  // Implementação
  cdsBuscar.Close;
  cdsBuscar.CommandText := 'SELECT * FROM PRODUTOS';
  cdsBuscar.Open;
end;

function TFrmProdutos.ValidarCodigoBarras(Codigo: string): Boolean;
begin
  Result := Length(Codigo) >= 8;
end;`

### **5️⃣ Quais os tipos de método?**

**a) Procedure (Procedimento):**

- Não retorna valor
- Executa ações

delphi

`procedure TFrmProdutos.LimparCampos;
begin
  txtDescricao.Clear;
  txtPreco.Clear;
end;`

**b) Function (Função):**

- Retorna um valor
- Executa ações E retorna resultado

delphi

`function TFrmProdutos.CalcularPrecoVenda(PrecoCusto: Double): Double;
begin
  Result := PrecoCusto * 1.3; // Margem de 30%
end;`

**c) Constructor:**

- Método especial para criar/inicializar objetos

delphi

`constructor TIntegracaoBase.Create(Owner: TForm; Dados: TDados);
begin
  inherited Create;
  FOwner := Owner;
  FDados := Dados;
end;`

**d) Destructor:**

- Método especial para destruir/liberar objetos

delphi

`destructor TIntegracaoBase.Destroy;
begin
  FDados.Free;
  inherited;
end;`

### **6️⃣ O que é uma propriedade?**

Uma propriedade é uma interface pública para acessar/modificar dados privados de uma classe de forma controlada.

**Sintaxe:**

delphi

`property NomePropriedade: Tipo read GetMethod write SetMethod;`

**Exemplo completo:**

delphi

`type
  TProduto = class
  private
    FDescricao: string;
    FPreco: Double;
    procedure SetPreco(const Value: Double);  // Validação ao definir
  public
    property Descricao: string read FDescricao write FDescricao;
    property Preco: Double read FPreco write SetPreco;
  end;

implementation

procedure TProduto.SetPreco(const Value: Double);
begin
  if Value < 0 then
    raise Exception.Create('Preço não pode ser negativo');
  FPreco := Value;
end;`

### **7️⃣ Qual a diferença entre "variável x propriedade"?**

| **Aspecto** | **Variável** | **Propriedade** |
| --- | --- | --- |
| **Escopo** | Local ou global | Membro de classe |
| **Acesso** | Direto | Controlado (via read/write) |
| **Validação** | Não tem | Pode ter lógica de validação |
| **Encapsulamento** | Não se aplica | Protege campo privado |
| **Visibilidade** | Definida por seção | Sempre public |

**Exemplo prático:**



`// VARIÁVEL - Acesso direto
var Contador: Integer := 0;
Contador := Contador + 1;  // Sem validação

// PROPRIEDADE - Acesso controlado
type
  TContador = class
  private
    FValor: Integer;
    procedure SetValor(const Value: Integer);
  public
    property Valor: Integer read FValor write SetValor;
  end;

procedure TContador.SetValor(const Value: Integer);
begin
  if Value < 0 then
    FValor := 0  // Validação!
  else
    FValor := Value;
end;

// Uso
var MeuContador: TContador := TContador.Create;
MeuContador.Valor := -5;  // Será convertido para 0`

### **8️⃣ O que é um Evento?**

Um evento é um **método especial** que é executado automaticamente quando algo específico acontece (ex: clique do mouse, tecla pressionada, formulário sendo exibido).

**Eventos no ciclo de vida dos formulários Sol.NET:**

```pascal

`type
  TFrmProdutos = class(TFrmHeranca)
  private
    procedure FrmCriar(Sender: TObject);     // OnCreate
    procedure FrmMostrar(Sender: TObject);   // OnShow
    procedure FrmPintar(Sender: TObject);    // OnPaint
    procedure FrmFechar(Sender: TObject; var Action: TCloseAction); // OnClose
    procedure FrmDestroi(Sender: TObject);   // OnDestroy
    
    procedure btnSalvarClick(Sender: TObject);  // OnClick do botão
    procedure txtPrecoKeyPress(Sender: TObject; var Key: Char); // OnKeyPress
  end;`

```
**Ordem de execução (Nascimento → Vida → Morte):**

Code

`NASCIMENTO:
OnCreate → OnShow → OnActivate → OnPaint

VIDA:
Eventos dos componentes (Click, KeyPress, Change, etc.)

MORTE:
OnClose → OnDeactivate → OnHide → OnDestroy`

### **9️⃣ Como mostrar dados de um TClientDataSet em um DBGrid?**

**Passo a passo:**



`// 1. Configurar componentes (normalmente feito visualmente no IDE)
cdsBuscar: TClientDataSet;  // Dataset
dsBuscar: TDataSource;      // Ponte
DBGridBuscar: TDBGrid;      // Grid visual

// 2. Conectar os componentes
dsBuscar.DataSet := cdsBuscar;      // DataSource aponta para ClientDataSet
DBGridBuscar.DataSource := dsBuscar; // Grid aponta para DataSource

// 3. Definir a consulta SQL
cdsBuscar.Close;
cdsBuscar.CommandText := 'SELECT ID_PRODUTO, DESCRICAO, PRECO FROM PRODUTOS WHERE ATIVO = 1';

// 4. Abrir o dataset
cdsBuscar.Open;

// PRONTO! Os dados aparecerão automaticamente no grid`

**Exemplo completo em um formulário:**

```pascal

`procedure TFrmProdutos.BuscarProdutos;
begin
  cdsBuscar.Close;
  
  var SQL: string := 
    'SELECT ' +
    '  P.ID_PRODUTO, ' +
    '  P.DESCRICAO, ' +
    '  P.CODIGO_BARRA, ' +
    '  P.PRECO_VENDA, ' +
    '  P.ESTOQUE_ATUAL ' +
    'FROM PRODUTOS P ' +
    'WHERE P.DESCRICAO LIKE ' + QuotedStr('%' + txtPesquisa.Text + '%');
  
  cdsBuscar.CommandText := SQL;
  cdsBuscar.Open;
  
  // Opcional: Configurar colunas do grid
  DBGridBuscar.Columns[0].Title.Caption := 'Código';
  DBGridBuscar.Columns[1].Title.Caption := 'Descrição';
  DBGridBuscar.Columns[2].Width := 120;
end;`
```
---

## **🎯 Resumo Visual**

Code

`CLASSE (Molde)          →  OBJETO (Instância)
TClientDataSet          →  cdsBuscar: TClientDataSet

VARIÁVEL (Dado solto)   →  var Total: Double := 100.50;
PROPRIEDADE (Dado encapsulado) → property Preco: Double read FPreco write SetPreco;

MÉTODO (Ação)           →  procedure Salvar;
EVENTO (Reação)         →  procedure btnSalvarClick(Sender: TObject);

FUNCTION (Retorna)      →  function Calcular: Double;
PROCEDURE (Não retorna) →  procedure Limpar;`



