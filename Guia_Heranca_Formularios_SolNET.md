[Guia_Heranca_Formularios_SolNET.md](https://github.com/user-attachments/files/23267307/Guia_Heranca_Formularios_SolNET.md)
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

### 2.1 Conceito: Nascimento, Vida e Morte

O ciclo de vida de um formulário no Sol.NET pode ser dividido em **três fases principais**:

```
┌─────────────────────────────────────────────────────────┐
│                    🎬 NASCIMENTO                        │
│  OnCreate → OnShow → OnActivate → OnPaint              │
│  (FrmCriar → FrmMostrar → FrmAtivar → FrmPintar)       │
└─────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────┐
│                      💼 VIDA                            │
│  Formulário em estado de ociosidade                     │
│  Aguardando ações do usuário ou eventos de componentes  │
└─────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────┐
│                     ⚰️ MORTE                            │
│  OnClose → OnDeactivate → OnHide → OnDestroy           │
│  (FrmFechar → FrmDestroi)                               │
└─────────────────────────────────────────────────────────┘
```

⚠️ **Importante:** Todas as telas são **criadas no momento em que são chamadas** e **destruídas quando fechadas**. Se você acessar o Cadastro de Produtos 5x após o login, o formulário será criado e destruído 5 vezes (há exceção quando o form é usado como variável).

### 2.1 Eventos Nativos do Delphi

Os formulários Delphi possuem eventos nativos que são executados em momentos específicos:

#### **📌 FormCreate** (Nascimento)
- **Quando ocorre:** Ao criar a instância do formulário (primeira vez)
- **Uso:** Inicializar variáveis, criar objetos, configurar estado inicial
- **No Sol.NET:** Chama `FrmCriar`
- **Exemplo:**
```pascal
procedure TFrmHeranca.FormCreate(Sender: TObject);
begin
  FrmCriar; // Chama método virtual personalizado
end;
```

#### **📌 FormShow** (Nascimento)
- **Quando ocorre:** Antes do formulário ser exibido na tela
- **Uso:** Carregar dados, atualizar interface
- **No Sol.NET:** Chama `FrmMostrar`

#### **📌 FormActivate** (Nascimento)
- **Quando ocorre:** Quando o formulário recebe foco pela primeira vez
- **Uso:** Atualizar dados que podem ter mudado
- **No Sol.NET:** Chama `FrmAtivar`

#### **📌 FormPaint** (Nascimento)
- **Quando ocorre:** Quando o formulário precisa ser redesenhado
- **⚠️ ATENÇÃO:** É disparado também toda vez que a janela é **redimensionada**!
- **Uso:** Operações visuais customizadas, processos semi-automatizados
- **No Sol.NET:** Chama `FrmPintar`

#### **📌 FormDeactivate** (Morte)
- **Quando ocorre:** Quando o formulário perde o foco
- **Uso:** Salvar estado, pausar operações

#### **📌 FormHide** (Morte)
- **Quando ocorre:** Quando o formulário é ocultado
- **Uso:** Operações antes de esconder o formulário

#### **📌 FormClose** (Morte)
- **Quando ocorre:** Ao fechar o formulário
- **Uso:** Salvar configurações do usuário (campos de pesquisa, filtros, etc.)
- **No Sol.NET:** Chama `FrmFechar`

#### **📌 FormDestroy** (Morte)
- **Quando ocorre:** Ao destruir a instância do formulário
- **Uso:** Liberar memória, destruir objetos criados no FrmCriar
- **No Sol.NET:** Chama `FrmDestroi`

### 2.2 Ordem de Execução Completa

```
🎬 NASCIMENTO
1. FormCreate    → FrmCriar
2. FormShow      → FrmMostrar
3. FormActivate  → FrmAtivar
4. FormPaint     → FrmPintar
   ↓
💼 VIDA
   (formulário em uso - aguardando ações do usuário)
   (eventos de componentes: OnClick, OnChange, etc.)
   (FormPaint pode ser chamado ao redimensionar)
   ↓
⚰️ MORTE
5. FormClose     → FrmFechar
6. FormDeactivate
7. FormHide
8. FormDestroy   → FrmDestroi
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
- **Criar objetos NÃO-VISUAIS** (DAL, listas, etc.)
- Definir estrutura do banco de dados
- Carregar configurações iniciais
- **NÃO manipule componentes visuais aqui** (use FrmMostrar para isso)

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
- **Manipular componentes visuais** (combos, grids, campos)
- Carregar listas de seleção (combos)
- Atualizar dados na tela
- Ajustar layout e configurações visuais

**💡 Dica:** **Se estiver em dúvida sobre onde colocar um código, USE ESTE!** É o mais versátil e seguro.

**Exemplo:**
```pascal
procedure TFrmCadastroProdutos.FrmMostrar;
begin
  inherited; // SEMPRE chame inherited primeiro!
  
  // Carregar combos
  CarregarComboUnidades(cbxUnidade);
  CarregarComboMarcas(cbxMarca);
  
  // Configurar visibilidade
  pnlEstoque.Visible := UsuarioPossuiPermissao('ESTOQUE_AVANCADO');
end;
```

#### **📌 FrmPintar**
```pascal
procedure TFrmHeranca.FrmPintar;
```
**Quando:** Redesenho do formulário (FormPaint)

**Uso:** 
- Ajustes visuais (cores, estilos)
- Ajustes de tamanho de componentes
- **⚠️ ATENÇÃO:** Evite redimensionar componentes aqui! Prefira FrmMostrar ou TabCadastroMostrar

**⚠️ Aviso Importante:**
O `FrmPintar` é executado **múltiplas vezes** durante a vida do formulário (sempre que há redesenho). **Nunca** coloque lógica pesada ou redimensionamento de componentes aqui, pois pode causar problemas de performance e comportamento inesperado.

**Exemplo seguro:**
```pascal
procedure TFrmCadastroProdutos.FrmPintar;
begin
  inherited;
  
  // Apenas ajustes de cor/estilo - nada de redimensionamento!
  if cdsGeral.FieldByName('ATIVO').AsString = 'N' then
    pnlPrincipal.Color := clGray
  else
    pnlPrincipal.Color := clWhite;
end;

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

## 14. 🔄 Processo de Varredura Automática de Componentes

### 14.1 O que é a Varredura Automática?

O `TFrmHeranca` possui um mecanismo **automático** que faz a **sincronização bidirecional** entre os componentes visuais da tela e o `cdsGeral` (ClientDataSet principal).

### 14.2 Como Funciona?

#### **📥 Varredura: cdsGeral → Componentes** (ao abrir para editar)

Quando você clica em **"Alterar"** em um registro:

```
1. Sistema busca registro no BD → armazena em cdsGeral
2. Sistema varre TODOS os componentes da tela
3. Para cada componente "ligado" ao dsGeral:
   - Localiza o campo correspondente no cdsGeral
   - Copia valor do cdsGeral para o componente
```

**Exemplo automático:**
```pascal
// Isso acontece AUTOMATICAMENTE no TFrmHeranca.TabCadastroMostrar
txtCodigoBarra.AsString := cdsGeral.FieldByName('CODIGO_BARRA').AsString;
txtDescricao.Text := cdsGeral.FieldByName('DESCRICAO').AsString;
txtPreco.AsFloat := cdsGeral.FieldByName('PRECO_VENDA').AsFloat;
// ... e assim para TODOS os componentes ligados ao dsGeral
```

#### **📤 Varredura: Componentes → cdsGeral** (ao salvar)

Quando você clica em **"Gravar"**:

```
1. Sistema varre TODOS os componentes da tela
2. Para cada componente "ligado" ao dsGeral:
   - Copia valor do componente para o campo do cdsGeral
3. Sistema executa SqlGravar (INSERT/UPDATE no BD)
```

**Exemplo automático:**
```pascal
// Isso acontece AUTOMATICAMENTE antes de chamar SqlGravar
cdsGeral.FieldByName('CODIGO_BARRA').AsString := txtCodigoBarra.AsString;
cdsGeral.FieldByName('DESCRICAO').AsString := txtDescricao.Text;
cdsGeral.FieldByName('PRECO_VENDA').AsFloat := txtPreco.AsFloat;
// ... e assim para TODOS os componentes ligados ao dsGeral
```

### 14.3 Como "Ligar" um Componente ao dsGeral?

No Delphi, configure as propriedades do componente:

```pascal
// Para um TDBEdit (exemplo):
txtDescricao.DataSource := dsGeral;
txtDescricao.DataField := 'DESCRICAO';

// Para um TGenEdit (componente customizado Sol.NET):
txtProduto.NomeCampo := 'ID_PRODUTO';
txtProduto.DataSource := dsGeral;
```

### 14.4 Validação de Registro em Uso

Quando um usuário tenta **editar** um registro, o sistema valida **automaticamente** se outro usuário já está editando:

```pascal
// Isso acontece em TabCadastroMostrar quando Estado = 'E'
if RegistroEmUso(IdRegistro, NomeTabela) then
begin
  Geral.Men('Registro está sendo editado por ' + NomeUsuario);
  pagCadastro.ActivePage := tabVisualizar; // Volta para busca
  Exit;
end;

// Se não está em uso, bloqueia para este usuário
BloquearRegistro(IdRegistro, NomeTabela, UsuarioAtual);
```

### 14.5 Personalização no TabCadastroMostrar

Embora a varredura seja **automática**, você pode adicionar lógica específica no evento `TabCadastroMostrar`:

**Exemplo: Carregar Padrões na Inserção**
```pascal
procedure TFrmContasPR.TabCadastroMostrar;
begin
  inherited; // Executa varredura automática
  
  // Lógica específica para INSERÇÃO
  if Estado = 'I' then
  begin
    // Carregar padrões diferentes para Pagar vs Receber
    if TipoTela = 'PAGAR' then
    begin
      cdsGeral.FieldByName('ID_TIPO_DOC').AsFloat := PadraoTipoDocPagar;
      cdsGeral.FieldByName('ID_PORTADOR').AsFloat := PadraoPortadorPagar;
    end
    else // RECEBER
    begin
      cdsGeral.FieldByName('ID_TIPO_DOC').AsFloat := PadraoTipoDocReceber;
      cdsGeral.FieldByName('ID_PORTADOR').AsFloat := PadraoPortadorReceber;
    end;
    
    // Data padrão = hoje
    cdsGeral.FieldByName('DATA_EMISSAO').AsDateTime := Date;
  end;
  
  // Lógica para EDIÇÃO E INSERÇÃO
  ConfigurarVisibilidadeCampos;
end;
```

---

## 15. 📋 Conceitos Fundamentais (Questões de Necessidade Básica)

### 15.1 O que é uma variável?

**Variável** é um espaço na memória que armazena um valor que pode ser alterado durante a execução do programa.

```pascal
var
  Nome: string;        // Variável que armazena texto
  Idade: Integer;      // Variável que armazena número inteiro
  Preco: Double;       // Variável que armazena número decimal
  Ativo: Boolean;      // Variável que armazena verdadeiro/falso
begin
  Nome := 'João';
  Idade := 25;
  Preco := 10.50;
  Ativo := True;
end;
```

### 15.2 O que é uma classe?

**Classe** é um "molde" ou "projeto" que define características (propriedades) e comportamentos (métodos) de um objeto.

```pascal
type
  TPessoa = class
  private
    FNome: string;
    FIdade: Integer;
  public
    property Nome: string read FNome write FNome;
    property Idade: Integer read FIdade write FIdade;
    
    procedure Apresentar;
  end;

procedure TPessoa.Apresentar;
begin
  ShowMessage('Meu nome é ' + FNome + ' e tenho ' + IntToStr(FIdade) + ' anos');
end;
```

### 15.3 O que é um objeto?

**Objeto** é uma **instância** de uma classe. É a "materialização" do molde.

```pascal
var
  Pessoa1, Pessoa2: TPessoa;
begin
  // Criando objetos (instâncias da classe TPessoa)
  Pessoa1 := TPessoa.Create;
  Pessoa2 := TPessoa.Create;
  
  // Cada objeto tem suas próprias propriedades
  Pessoa1.Nome := 'João';
  Pessoa1.Idade := 25;
  
  Pessoa2.Nome := 'Maria';
  Pessoa2.Idade := 30;
  
  Pessoa1.Apresentar; // "Meu nome é João e tenho 25 anos"
  Pessoa2.Apresentar; // "Meu nome é Maria e tenho 30 anos"
  
  // Liberar memória
  Pessoa1.Free;
  Pessoa2.Free;
end;
```

### 15.4 O que é um método?

**Método** é uma função ou procedimento que pertence a uma classe e define um **comportamento** do objeto.

```pascal
type
  TCalculadora = class
  public
    function Somar(A, B: Integer): Integer;      // Método que retorna valor
    procedure MostrarResultado(Valor: Integer);  // Método que não retorna valor
  end;

function TCalculadora.Somar(A, B: Integer): Integer;
begin
  Result := A + B;
end;

procedure TCalculadora.MostrarResultado(Valor: Integer);
begin
  ShowMessage('Resultado: ' + IntToStr(Valor));
end;
```

### 15.5 Quais os tipos de método?

#### **🔹 Função (Function)**
Retorna um valor:
```pascal
function Somar(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// Uso:
Total := Somar(10, 20); // Total = 30
```

#### **🔹 Procedimento (Procedure)**
Não retorna valor:
```pascal
procedure Exibir(Mensagem: string);
begin
  ShowMessage(Mensagem);
end;

// Uso:
Exibir('Olá Mundo!');
```

#### **🔹 Constructor**
Cria uma instância do objeto:
```pascal
constructor TMinhaClasse.Create;
begin
  inherited Create;
  // Inicializações
end;
```

#### **🔹 Destructor**
Destrói uma instância do objeto:
```pascal
destructor TMinhaClasse.Destroy;
begin
  // Liberar recursos
  inherited Destroy;
end;
```

### 15.6 O que é uma propriedade?

**Propriedade** é um meio controlado de acessar ou modificar valores privados de uma classe.

```pascal
type
  TProduto = class
  private
    FPreco: Double;
    procedure SetPreco(Value: Double);  // Setter
  public
    property Preco: Double read FPreco write SetPreco;
  end;

procedure TProduto.SetPreco(Value: Double);
begin
  if Value < 0 then
    raise Exception.Create('Preço não pode ser negativo');
  FPreco := Value;
end;

// Uso:
Produto.Preco := 10.50;  // Chama SetPreco internamente
Total := Produto.Preco;  // Acessa FPreco diretamente
```

### 15.7 Qual a diferença entre variável e propriedade?

| Variável | Propriedade |
|----------|-------------|
| Espaço direto na memória | Acesso controlado via getter/setter |
| Sem validação automática | Pode ter validação no setter |
| Acesso direto | Pode executar código ao ler/escrever |
| `Idade := 25;` | `Pessoa.Idade := 25;` (pode validar) |

**Exemplo:**
```pascal
// Variável simples:
var Idade: Integer;
Idade := -5; // ❌ Aceita valor inválido

// Propriedade com validação:
property Idade: Integer read FIdade write SetIdade;

procedure SetIdade(Value: Integer);
begin
  if Value < 0 then
    raise Exception.Create('Idade inválida');
  FIdade := Value;
end;

Pessoa.Idade := -5; // ✅ Lança exceção!
```

### 15.8 O que é um evento?

**Evento** é uma ação que ocorre em resposta a algo (clique, mudança de valor, etc.).

```pascal
// Declaração:
procedure TFrmMeuForm.btnSalvarClick(Sender: TObject);
begin
  // Código executado quando o botão é clicado
  Salvar;
end;

procedure TFrmMeuForm.txtNomeChange(Sender: TObject);
begin
  // Código executado quando o texto muda
  lblContador.Caption := IntToStr(Length(txtNome.Text));
end;
```

### 15.9 Como mostrar dados de um TClientDataSet em um DBGrid?

```pascal
// 1. Criar os componentes (design time ou runtime)
cdsLista := TClientDataSet.Create(Self);
dsLista := TDataSource.Create(Self);
dbgLista := TDBGrid.Create(Self);

// 2. Ligar os componentes
dsLista.DataSet := cdsLista;      // DataSource aponta para ClientDataSet
dbgLista.DataSource := dsLista;   // DBGrid aponta para DataSource

// 3. Estruturar o ClientDataSet
cdsLista.FieldDefs.Clear;
cdsLista.FieldDefs.Add('CODIGO', ftInteger);
cdsLista.FieldDefs.Add('NOME', ftString, 100);
cdsLista.FieldDefs.Add('PRECO', ftFloat);
cdsLista.CreateDataSet;

// 4. Inserir dados
cdsLista.Append;
cdsLista.FieldByName('CODIGO').AsInteger := 1;
cdsLista.FieldByName('NOME').AsString := 'Produto A';
cdsLista.FieldByName('PRECO').AsFloat := 10.50;
cdsLista.Post;

// 5. O DBGrid mostra automaticamente!
```

---

## 16. 📚 Objetos Importantes para Estudo

### 16.1 TClientDataSet

**O que é:** Dataset em memória que armazena dados.

**Principais propriedades:**
- `FieldDefs`: Define campos
- `Data`: Dados em formato binário
- `Active`: Ativa/desativa o dataset

**Principais métodos:**
- `CreateDataSet`: Cria estrutura
- `Append`: Adiciona novo registro
- `Edit`: Edita registro atual
- `Post`: Confirma alterações
- `Cancel`: Cancela alterações
- `Delete`: Exclui registro
- `First`, `Last`, `Next`, `Prior`: Navegação
- `Locate`: Localiza registro
- `FieldByName('CAMPO')`: Acessa campo

**Documentação:**
- [TClientDataSet - Embarcadero](https://docwiki.embarcadero.com/Libraries/Alexandria/en/Datasnap.DBClient.TClientDataSet)

### 16.2 TStringList

**O que é:** Lista de strings com funcionalidades extras.

**Principais propriedades:**
- `Count`: Quantidade de itens
- `Strings[Index]`: Acessa item por índice
- `Text`: Todo conteúdo como texto

**Principais métodos:**
- `Add`: Adiciona item
- `Clear`: Limpa lista
- `Delete`: Remove item
- `IndexOf`: Busca item
- `LoadFromFile/SaveToFile`: Salvar/carregar arquivo

**Exemplo:**
```pascal
var
  Lista: TStringList;
begin
  Lista := TStringList.Create;
  try
    Lista.Add('Item 1');
    Lista.Add('Item 2');
    ShowMessage(Lista[0]); // "Item 1"
  finally
    Lista.Free;
  end;
end;
```

**Documentação:**
- [TStringList - Embarcadero](https://docwiki.embarcadero.com/Libraries/Alexandria/en/System.Classes.TStringList)

### 16.3 TEdit

**O que é:** Caixa de texto para entrada de dados.

**Principais propriedades:**
- `Text`: Texto digitado
- `ReadOnly`: Somente leitura
- `Enabled`: Habilitado/desabilitado
- `MaxLength`: Tamanho máximo

**Principais eventos:**
- `OnChange`: Ao mudar texto
- `OnEnter`: Ao receber foco
- `OnExit`: Ao perder foco
- `OnKeyPress`: Ao pressionar tecla

**Documentação:**
- [TEdit - Embarcadero](https://docwiki.embarcadero.com/Libraries/Alexandria/en/Vcl.StdCtrls.TEdit)

### 16.4 TButton

**O que é:** Botão clicável.

**Principais propriedades:**
- `Caption`: Texto do botão
- `Enabled`: Habilitado/desabilitado
- `Visible`: Visível/oculto

**Principais eventos:**
- `OnClick`: Ao clicar

**Documentação:**
- [TButton - Embarcadero](https://docwiki.embarcadero.com/Libraries/Alexandria/en/Vcl.StdCtrls.TButton)

### 16.5 TComboBox

**O que é:** Lista suspensa para seleção.

**Principais propriedades:**
- `Items`: Lista de opções
- `ItemIndex`: Índice selecionado
- `Text`: Texto selecionado
- `Style`: Estilo (csDropDown, csDropDownList)

**Principais eventos:**
- `OnChange`: Ao mudar seleção

**Exemplo:**
```pascal
// Adicionar itens
cbxStatus.Items.Clear;
cbxStatus.Items.Add('Ativo');
cbxStatus.Items.Add('Inativo');
cbxStatus.ItemIndex := 0; // Seleciona primeiro

// Ler seleção
if cbxStatus.ItemIndex = 0 then
  ShowMessage('Ativo selecionado');
```

**Documentação:**
- [TComboBox - Embarcadero](https://docwiki.embarcadero.com/Libraries/Alexandria/en/Vcl.StdCtrls.TComboBox)

---

## 17. 🔗 Links de Documentação Oficial

### 17.1 Documentação Técnica Oficial (Embarcadero)

**RAD Studio Documentation:**
- 🌐 [Getting Started with RAD Studio](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Getting_Started_with_RAD_Studio)
- Use a caixa de pesquisa para encontrar componentes específicos

### 17.2 Documentação Técnica Não-Oficial

**Delphi Basics:**
- 🌐 [Delphi Basics](http://www.delphibasics.co.uk/index.html)
- Excelente recurso com exemplos práticos

### 17.3 Recomendação de Estudo

1. **Sempre comece com a documentação técnica oficial** (Embarcadero)
2. **Em caso de dúvidas, procure explicações mais "vulgares"** (fóruns, Stack Overflow)
3. **Pratique com exemplos pequenos** antes de aplicar em projetos grandes

---

**Desenvolvido para:** Projeto Sol.NET ERP  
**Data:** Outubro de 2025  
**Versão:** 2.0 (Atualizado com documentação do coordenador)  
**Autor:** Copilot AI Assistant

---

💡 **Lembre-se:** A melhor forma de aprender é praticando! Comece com formulários simples e vá evoluindo gradualmente.

🚀 **Bom desenvolvimento!**

