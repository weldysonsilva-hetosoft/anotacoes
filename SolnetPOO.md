# **Documentação: Programação Orientada a Objetos (POO) no Sol.NET**

## **🎯 Visão Geral**

Este documento estabelece os padrões, convenções e melhores práticas para desenvolvimento orientado a objetos no projeto Sol.NET. A aplicação utiliza Delphi 12.2 e segue princípios modernos de Clean Code, com foco em manutenibilidade, extensibilidade e reutilização de código.

### **Objetivos desta Documentação:**

- ✅ Estabelecer padrões consistentes de POO
- ✅ Documentar convenções específicas do Sol.NET
- ✅ Fornecer exemplos práticos e aplicáveis
- ✅ Facilitar onboarding de novos desenvolvedores
- ✅ Garantir qualidade e manutenibilidade do código

---

## **🏗️ Princípios Fundamentais**

O Sol.NET adota os seguintes princípios de POO:

### **1. Encapsulamento**

Proteção do estado interno dos objetos, expondo apenas interfaces públicas necessárias.

### **2. Herança**

Reutilização de código através de hierarquias bem definidas.

### **3. Polimorfismo**

Flexibilidade através de interfaces e métodos virtuais/abstratos.

### **4. Abstração**

Separação entre interface e implementação.

---

## **📐 Convenções de Nomenclatura**

### **Classes**

delphi

`// ✅ Correto - Prefixo T + Pascal Case
type
  TPessoa = class
  TContaBancaria = class
  TNotaFiscalEletronica = class

// ❌ Evitar
type
  Pessoa = class              // Sem prefixo
  tPessoa = class             // Case incorreto
  T_Pessoa = class            // Underscore desnecessário`

### **Interfaces**

delphi

`// ✅ Correto - Prefixo I + Pascal Case + GUID
type
  IRepositorioPessoa = interface
    ['{GUID-AQUI}']
  end;

  IServicoPagamento = interface
    ['{GUID-AQUI}']
  end;

// ❌ Evitar
type
  RepositorioPessoa = interface  // Sem prefixo I
  IPessoa_Repository = interface // Underscore/inglês misturado`

### **Campos Privados**

delphi

`type
  TPessoa = class
  private
    FNome: string;              // ✅ Prefixo F + Pascal Case
    FIdade: Integer;
    FDataNascimento: TDateTime;
    
    // ❌ Evitar
    Nome: string;               // Sem prefixo
    fNome: string;              // Case incorreto
    _Nome: string;              // Prefixo errado
  end;`

### **Propriedades**

delphi

`type
  TPessoa = class
  private
    FNome: string;
  public
    property Nome: string read FNome write FNome;  // ✅ Sem prefixo, Pascal Case
  end;`

### **Métodos**

delphi

`type
  TPessoa = class
  public
    // ✅ Verbos descritivos em Pascal Case
    procedure CalcularIdade;
    function ObterNomeCompleto: string;
    function ValidarCPF(const ACPF: string): Boolean;
    
    // ❌ Evitar
    procedure calcular_idade;   // Case e underscore incorretos
    function nome: string;      // Não descreve ação
  end;`

---

## **🎨 Estrutura de Classes**

### **Anatomia de uma Classe Padrão**

delphi

`unit uPessoa;

interface

uses
  System.SysUtils, System.Classes;

type
  {$REGION 'Tipos Auxiliares'}
  TTipoPessoa = (tpFisica, tpJuridica);
  
  TEnderecoPessoa = record
    Logradouro: string;
    Numero: string;
    Cidade: string;
    UF: string;
    CEP: string;
  end;
  {$ENDREGION}

  {$REGION 'TPessoa - Classe Principal'}
  /// <summary>
  /// Representa uma pessoa (física ou jurídica) no sistema Sol.NET
  /// </summary>
  TPessoa = class
  strict private
    FIdPessoa: Integer;
    FNome: string;
    FDocumento: string;
    FTipoPessoa: TTipoPessoa;
    FEndereco: TEnderecoPessoa;
    FAtivo: Boolean;
    
    procedure SetNome(const Value: string);
    function GetNomeFormatado: string;
    
  private
    // Métodos auxiliares internos
    function ValidarDocumento: Boolean;
    
  protected
    // Métodos que podem ser sobrescritos por classes filhas
    procedure ValidarDados; virtual;
    function ObterDescricaoTipo: string; virtual;
    
  public
    constructor Create; overload;
    constructor Create(AIdPessoa: Integer; const ANome: string); overload;
    destructor Destroy; override;
    
    // Métodos de negócio
    procedure Salvar;
    procedure Excluir;
    function ObterIdade: Integer;
    
    // Propriedades
    property IdPessoa: Integer read FIdPessoa write FIdPessoa;
    property Nome: string read FNome write SetNome;
    property NomeFormatado: string read GetNomeFormatado;
    property Documento: string read FDocumento write FDocumento;
    property TipoPessoa: TTipoPessoa read FTipoPessoa write FTipoPessoa;
    property Ativo: Boolean read FAtivo write FAtivo;
  end;
  {$ENDREGION}

implementation

{$REGION 'TPessoa'}

constructor TPessoa.Create;
begin
  inherited Create;
  FAtivo := True;
  FTipoPessoa := tpFisica;
end;

constructor TPessoa.Create(AIdPessoa: Integer; const ANome: string);
begin
  Create;
  FIdPessoa := AIdPessoa;
  FNome := ANome;
end;

destructor TPessoa.Destroy;
begin
  // Liberar recursos se necessário
  inherited;
end;

procedure TPessoa.SetNome(const Value: string);
begin
  if Trim(Value) = '' then
    raise Exception.Create('Nome não pode ser vazio');
    
  FNome := UpperCase(Value);
end;

function TPessoa.GetNomeFormatado: string;
begin
  Result := Format('%s (%s)', [FNome, ObterDescricaoTipo]);
end;

function TPessoa.ValidarDocumento: Boolean;
begin
  case FTipoPessoa of
    tpFisica: Result := ValidarCPF(FDocumento);
    tpJuridica: Result := ValidarCNPJ(FDocumento);
  else
    Result := False;
  end;
end;

procedure TPessoa.ValidarDados;
begin
  if Trim(FNome) = '' then
    raise Exception.Create('Nome obrigatório');
    
  if not ValidarDocumento then
    raise Exception.Create('Documento inválido');
end;

function TPessoa.ObterDescricaoTipo: string;
begin
  case FTipoPessoa of
    tpFisica: Result := 'Pessoa Física';
    tpJuridica: Result := 'Pessoa Jurídica';
  else
    Result := 'Tipo Desconhecido';
  end;
end;

procedure TPessoa.Salvar;
begin
  ValidarDados;
  
  if FIdPessoa = 0 then
    // INSERT
  else
    // UPDATE
end;

procedure TPessoa.Excluir;
begin
  if FIdPessoa = 0 then
    raise Exception.Create('Pessoa não foi salva ainda');
    
  // Lógica de exclusão
  FAtivo := False;
end;

function TPessoa.ObterIdade: Integer;
begin
  // Implementação do cálculo de idade
  Result := 0;
end;

{$ENDREGION}

end.`

---

## **🔒 Níveis de Visibilidade**

### **Strict Private**

Acesso **apenas** pela própria classe, não visível para descendentes.

delphi

`type
  TContaBancaria = class
  strict private
    FSenha: string;              // Não acessível por classes filhas
    FChaveEncriptacao: string;   // Dados ultra-sensíveis
  end;`

### **Private**

Acesso pela classe e por outras classes **na mesma unit**.

delphi

`type
  TPessoa = class
  private
    FNome: string;               // Acessível na mesma unit
    procedure MetodoInterno;     // Helpers internos
  end;`

### **Protected**

Acesso pela classe e por todas as **classes descendentes**.

delphi

`type
  TEntidadeBase = class
  protected
    FIdEntidade: Integer;
    procedure ValidarEntidade; virtual;  // Pode ser sobrescrito
  end;
  
  TPessoa = class(TEntidadeBase)
  protected
    procedure ValidarEntidade; override; // Sobrescreve comportamento
  end;`

### **Public**

Acesso **irrestrito** de qualquer lugar.

delphi

`type
  TPessoa = class
  public
    property Nome: string read FNome write FNome;
    procedure Salvar;
    function ObterIdade: Integer;
  end;`

### **Published**

Acesso público + **serialização RTTI** + visível no Object Inspector (componentes visuais).

delphi

`type
  TMeuComponente = class(TComponent)
  published
    property CorFundo: TColor read FCorFundo write FCorFundo;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;`

---

## **🧬 Herança e Polimorfismo**

### **Hierarquia de Classes**

delphi

`type
  {$REGION 'Classe Base'}
  TEntidadeBase = class
  strict private
    FIdEntidade: Integer;
    FDataCriacao: TDateTime;
    FDataAlteracao: TDateTime;
    
  protected
    procedure ValidarEntidade; virtual; abstract;
    procedure AntesDeGravar; virtual;
    procedure DepoisDeGravar; virtual;
    
  public
    constructor Create; virtual;
    procedure Salvar;
    procedure Excluir; virtual;
    
    property IdEntidade: Integer read FIdEntidade write FIdEntidade;
    property DataCriacao: TDateTime read FDataCriacao;
  end;
  {$ENDREGION}

  {$REGION 'Classe Descendente'}
  TPessoa = class(TEntidadeBase)
  strict private
    FNome: string;
    FCPF: string;
    
  protected
    procedure ValidarEntidade; override;  // Implementa método abstrato
    procedure AntesDeGravar; override;    // Estende comportamento
    
  public
    constructor Create; override;
    
    property Nome: string read FNome write FNome;
    property CPF: string read FCPF write FCPF;
  end;
  {$ENDREGION}

  {$REGION 'Especialização'}
  TFuncionario = class(TPessoa)
  strict private
    FMatricula: string;
    FSalario: Double;
    
  protected
    procedure ValidarEntidade; override;  // Adiciona validações específicas
    
  public
    property Matricula: string read FMatricula write FMatricula;
    property Salario: Double read FSalario write FSalario;
  end;
  {$ENDREGION}`

### **Implementação da Herança**

delphi

`{$REGION 'TEntidadeBase'}

constructor TEntidadeBase.Create;
begin
  inherited Create;
  FDataCriacao := Now;
end;

procedure TEntidadeBase.AntesDeGravar;
begin
  FDataAlteracao := Now;
  ValidarEntidade;
end;

procedure TEntidadeBase.DepoisDeGravar;
begin
  // Hook para processamentos pós-gravação
end;

procedure TEntidadeBase.Salvar;
begin
  AntesDeGravar;
  try
    // Lógica de gravação no banco
  finally
    DepoisDeGravar;
  end;
end;

procedure TEntidadeBase.Excluir;
begin
  if FIdEntidade = 0 then
    raise Exception.Create('Entidade não persistida');
    
  // Lógica de exclusão
end;

{$ENDREGION}

{$REGION 'TPessoa'}

constructor TPessoa.Create;
begin
  inherited Create;
  // Inicializações específicas de TPessoa
end;

procedure TPessoa.ValidarEntidade;
begin
  if Trim(FNome) = '' then
    raise Exception.Create('Nome obrigatório');
    
  if not ValidarCPF(FCPF) then
    raise Exception.Create('CPF inválido');
end;

procedure TPessoa.AntesDeGravar;
begin
  FNome := UpperCase(Trim(FNome));
  inherited AntesDeGravar;  // Chama validações da classe base
end;

{$ENDREGION}

{$REGION 'TFuncionario'}

procedure TFuncionario.ValidarEntidade;
begin
  inherited ValidarEntidade;  // Valida dados de TPessoa
  
  if Trim(FMatricula) = '' then
    raise Exception.Create('Matrícula obrigatória');
    
  if FSalario <= 0 then
    raise Exception.Create('Salário deve ser maior que zero');
end;

{$ENDREGION}`

---

## **🔌 Interfaces no Sol.NET**

### **Definição de Interface**

delphi

`type
  {$REGION 'Interface de Repositório'}
  /// <summary>
  /// Define contrato para persistência de pessoas
  /// </summary>
  IRepositorioPessoa = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    
    function Buscar(AIdPessoa: Integer): TPessoa;
    function BuscarPorCPF(const ACPF: string): TPessoa;
    function ListarTodos: TList<TPessoa>;
    procedure Salvar(APessoa: TPessoa);
    procedure Excluir(AIdPessoa: Integer);
  end;
  {$ENDREGION}

  {$REGION 'Implementação do Repositório'}
  TRepositorioPessoaSQL = class(TInterfacedObject, IRepositorioPessoa)
  private
    FDados: TDados;
    
  public
    constructor Create(ADados: TDados);
    
    // Implementação da interface
    function Buscar(AIdPessoa: Integer): TPessoa;
    function BuscarPorCPF(const ACPF: string): TPessoa;
    function ListarTodos: TList<TPessoa>;
    procedure Salvar(APessoa: TPessoa);
    procedure Excluir(AIdPessoa: Integer);
  end;
  {$ENDREGION}`

### **Uso de Interfaces (Injeção de Dependência)**

delphi

`type
  TControlePessoa = class
  private
    FRepositorio: IRepositorioPessoa;  // Dependência via interface
    
  public
    constructor Create(ARepositorio: IRepositorioPessoa);
    
    procedure CadastrarNovaPessoa(const ANome, ACPF: string);
    function BuscarPessoaPorCPF(const ACPF: string): TPessoa;
  end;

implementation

constructor TControlePessoa.Create(ARepositorio: IRepositorioPessoa);
begin
  inherited Create;
  FRepositorio := ARepositorio;  // Injeção via construtor
end;

procedure TControlePessoa.CadastrarNovaPessoa(const ANome, ACPF: string);
begin
  var Pessoa: TPessoa := TPessoa.Create;
  try
    Pessoa.Nome := ANome;
    Pessoa.CPF := ACPF;
    
    FRepositorio.Salvar(Pessoa);  // Usa interface, não implementação concreta
  finally
    Pessoa.Free;
  end;
end;

function TControlePessoa.BuscarPessoaPorCPF(const ACPF: string): TPessoa;
begin
  Result := FRepositorio.BuscarPorCPF(ACPF);
end;

// Uso no código
var Repositorio: IRepositorioPessoa := TRepositorioPessoaSQL.Create(DadosConexao);
var Controle: TControlePessoa := TControlePessoa.Create(Repositorio);`

---

## **📦 Propriedades**

### **Propriedades Simples**

delphi

`type
  TProduto = class
  private
    FDescricao: string;
    FPreco: Double;
    
  public
    // Leitura/escrita direta
    property Descricao: string read FDescricao write FDescricao;
    property Preco: Double read FPreco write FPreco;
  end;`

### **Propriedades com Validação**

delphi

`type
  TProduto = class
  private
    FDescricao: string;
    FPreco: Double;
    
    procedure SetDescricao(const Value: string);
    procedure SetPreco(const Value: Double);
    
  public
    property Descricao: string read FDescricao write SetDescricao;
    property Preco: Double read FPreco write SetPreco;
  end;

implementation

procedure TProduto.SetDescricao(const Value: string);
begin
  if Trim(Value) = '' then
    raise Exception.Create('Descrição não pode ser vazia');
    
  FDescricao := UpperCase(Value);
end;

procedure TProduto.SetPreco(const Value: Double);
begin
  if Value < 0 then
    raise Exception.Create('Preço não pode ser negativo');
    
  FPreco := Value;
end;`

### **Propriedades Somente Leitura**

delphi

`type
  TProduto = class
  private
    FDescricao: string;
    FPreco: Double;
    FCustoMedio: Double;
    
    function GetMargemLucro: Double;
    
  public
    property Descricao: string read FDescricao write FDescricao;
    property Preco: Double read FPreco write FPreco;
    property CustoMedio: Double read FCustoMedio write FCustoMedio;
    property MargemLucro: Double read GetMargemLucro;  // Somente leitura
  end;

implementation

function TProduto.GetMargemLucro: Double;
begin
  if FCustoMedio = 0 then
    Result := 0
  else
    Result := ((FPreco - FCustoMedio) / FCustoMedio) * 100;
end;`

### **Propriedades Indexadas**

delphi

`type
  TListaProdutos = class
  private
    FLista: TList<TProduto>;
    
    function GetProduto(Index: Integer): TProduto;
    procedure SetProduto(Index: Integer; const Value: TProduto);
    function GetCount: Integer;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    property Produtos[Index: Integer]: TProduto read GetProduto write SetProduto; default;
    property Count: Integer read GetCount;
  end;

implementation

function TListaProdutos.GetProduto(Index: Integer): TProduto;
begin
  Result := FLista[Index];
end;

procedure TListaProdutos.SetProduto(Index: Integer; const Value: TProduto);
begin
  FLista[Index] := Value;
end;

// Uso
var Lista: TListaProdutos := TListaProdutos.Create;
var Produto: TProduto := Lista[0];  // Usa propriedade default`

---

## **🏭 Construtores e Destrutores**

### **Múltiplos Construtores**

delphi

`type
  TPessoa = class
  private
    FIdPessoa: Integer;
    FNome: string;
    FCPF: string;
    
  public
    // Construtor padrão
    constructor Create; overload;
    
    // Construtor com parâmetros
    constructor Create(const ANome, ACPF: string); overload;
    
    // Construtor para carregar do banco
    constructor Create(AIdPessoa: Integer); overload;
    
    destructor Destroy; override;
  end;

implementation

constructor TPessoa.Create;
begin
  inherited Create;
  // Inicializações padrão
end;

constructor TPessoa.Create(const ANome, ACPF: string);
begin
  Create;  // Chama construtor padrão
  FNome := ANome;
  FCPF := ACPF;
end;

constructor TPessoa.Create(AIdPessoa: Integer);
begin
  Create;  // Chama construtor padrão
  FIdPessoa := AIdPessoa;
  // Carregar dados do banco
end;

destructor TPessoa.Destroy;
begin
  // Liberar recursos alocados
  inherited;
end;`

### **Gerenciamento de Memória**

delphi

`type
  TControlePessoa = class
  private
    FPessoa: TPessoa;
    FEndereco: TEndereco;
    FLista: TObjectList<TProduto>;
    
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TControlePessoa.Create;
begin
  inherited Create;
  FPessoa := TPessoa.Create;
  FEndereco := TEndereco.Create;
  FLista := TObjectList<TProduto>.Create(True);  // OwnsObjects = True
end;

destructor TControlePessoa.Destroy;
begin
  FreeAndNil(FPessoa);
  FreeAndNil(FEndereco);
  FreeAndNil(FLista);  // Libera lista e objetos contidos
  inherited;
end;`

---

## **🎭 Métodos Virtuais e Abstratos**

### **Métodos Virtuais**

delphi

`type
  TRelatorioBase = class
  protected
    procedure GerarCabecalho; virtual;
    procedure GerarCorpo; virtual;
    procedure GerarRodape; virtual;
    
  public
    procedure Gerar;
  end;

  TRelatorioVendas = class(TRelatorioBase)
  protected
    procedure GerarCabecalho; override;  // Sobrescreve comportamento
    procedure GerarCorpo; override;
  end;

implementation

{$REGION 'TRelatorioBase'}

procedure TRelatorioBase.Gerar;
begin
  GerarCabecalho;
  GerarCorpo;
  GerarRodape;
end;

procedure TRelatorioBase.GerarCabecalho;
begin
  // Implementação padrão
end;

procedure TRelatorioBase.GerarCorpo;
begin
  // Implementação padrão
end;

procedure TRelatorioBase.GerarRodape;
begin
  // Implementação padrão
end;

{$ENDREGION}

{$REGION 'TRelatorioVendas'}

procedure TRelatorioVendas.GerarCabecalho;
begin
  inherited;  // Chama implementação da classe base
  // Adiciona comportamento específico
end;

procedure TRelatorioVendas.GerarCorpo;
begin
  // Implementação completamente nova
end;

{$ENDREGION}`

### **Métodos Abstratos**

delphi

`type
  TExportadorBase = class
  protected
    procedure ConfigurarExportacao; virtual; abstract;
    procedure ExportarDados; virtual; abstract;
    procedure FinalizarExportacao; virtual; abstract;
    
  public
    procedure Executar;
  end;

  TExportadorExcel = class(TExportadorBase)
  protected
    procedure ConfigurarExportacao; override;
    procedure ExportarDados; override;
    procedure FinalizarExportacao; override;
  end;

implementation

procedure TExportadorBase.Executar;
begin
  ConfigurarExportacao;
  ExportarDados;
  FinalizarExportacao;
end;

procedure TExportadorExcel.ConfigurarExportacao;
begin
  // Configurações específicas do Excel
end;

procedure TExportadorExcel.ExportarDados;
begin
  // Lógica de exportação para Excel
end;

procedure TExportadorExcel.FinalizarExportacao;
begin
  // Finalização da exportação Excel
end;`

---

## **🔧 Class Helpers e Record Helpers**

### **Class Helper**

delphi

`type
  TStringHelper = class helper for string
  public
    function SomenteNumeros: string;
    function FormatarCPF: string;
    function FormatarCNPJ: string;
    function EhNumerico: Boolean;
  end;

implementation

function TStringHelper.SomenteNumeros: string;
begin
  var I: Integer := 0;
  Result := '';
  
  for I := 1 to Length(Self) do
  begin
    if CharInSet(Self[I], ['0'..'9']) then
      Result := Result + Self[I];
  end;
end;

function TStringHelper.FormatarCPF: string;
begin
  var Numeros: string := SomenteNumeros;
  
  if Length(Numeros) <> 11 then
    Result := Self
  else
    Result := Format('%s.%s.%s-%s', [
      Copy(Numeros, 1, 3),
      Copy(Numeros, 4, 3),
      Copy(Numeros, 7, 3),
      Copy(Numeros, 10, 2)
    ]);
end;

function TStringHelper.EhNumerico: Boolean;
begin
  var Valor: Double;
  Result := TryStrToFloat(Self, Valor);
end;

// Uso
var CPF: string := '12345678901';
ShowMessage(CPF.FormatarCPF);  // 123.456.789-01`

### **Record Helper**

delphi

`type
  TDateTimeHelper = record helper for TDateTime
  public
    function PrimeiroDiaMes: TDateTime;
    function UltimoDiaMes: TDateTime;
    function AdicionarDiasUteis(Dias: Integer): TDateTime;
    function EhDiaUtil: Boolean;
  end;

implementation

function TDateTimeHelper.PrimeiroDiaMes: TDateTime;
begin
  Result := EncodeDate(YearOf(Self), MonthOf(Self), 1);
end;

function TDateTimeHelper.UltimoDiaMes: TDateTime;
begin
  Result := EndOfTheMonth(Self);
end;

function TDateTimeHelper.EhDiaUtil: Boolean;
begin
  Result := not (DayOfWeek(Self) in [1, 7]);  // Não é sábado ou domingo
end;

// Uso
var Data: TDateTime := Now;
var PrimeiroDia: TDateTime := Data.PrimeiroDiaMes;`

---

## **🎯 Padrões de Design Aplicados no Sol.NET**

### **1. Repository Pattern**

delphi

`type
  IRepositorio<T: class> = interface
    ['{GUID}']
    function Buscar(AId: Integer): T;
    function ListarTodos: TList<T>;
    procedure Salvar(AEntidade: T);
    procedure Excluir(AId: Integer);
  end;

  TRepositorioBase<T: class> = class(TInterfacedObject, IRepositorio<T>)
  protected
    FDados: TDados;
    
  public
    constructor Create(ADados: TDados);
    
    function Buscar(AId: Integer): T; virtual; abstract;
    function ListarTodos: TList<T>; virtual; abstract;
    procedure Salvar(AEntidade: T); virtual; abstract;
    procedure Excluir(AId: Integer); virtual; abstract;
  end;`

### **2. Factory Pattern**

delphi

`type
  TIntegracaoFactory = class
  public
    class function CriarIntegracao(ATipo: TTipoIntegracao): IIntegracaoBase;
  end;

implementation

class function TIntegracaoFactory.CriarIntegracao(ATipo: TTipoIntegracao): IIntegracaoBase;
begin
  case ATipo of
    tiIFood: Result := TIntegracaoIFood.Create;
    tiWBuy: Result := TIntegracaoWBuy.Create;
    tiScanntech: Result := TIntegracaoScanntech.Create;
  else
    raise Exception.Create('Tipo de integração não suportado');
  end;
end;`

### **3. Strategy Pattern**

delphi

`type
  ICalculoPreco = interface
    ['{GUID}']
    function CalcularPrecoFinal(APrecoBase: Double): Double;
  end;

  TCalculoPrecoVarejo = class(TInterfacedObject, ICalculoPreco)
    function CalcularPrecoFinal(APrecoBase: Double): Double;
  end;

  TCalculoPrecoAtacado = class(TInterfacedObject, ICalculoPreco)
    function CalcularPrecoFinal(APrecoBase: Double): Double;
  end;

  TProduto = class
  private
    FPrecoBase: Double;
    FEstrategiaPreco: ICalculoPreco;
    
  public
    constructor Create(AEstrategiaPreco: ICalculoPreco);
    
    function ObterPrecoFinal: Double;
    property PrecoBase: Double read FPrecoBase write FPrecoBase;
  end;`

### **4. Singleton Pattern**

delphi

`type
  TConfiguracoesGlobais = class
  strict private
    class var FInstancia: TConfiguracoesGlobais;
    
    constructor CreatePrivate;
    
  public
    class function Instancia: TConfiguracoesGlobais;
    class destructor Destroy;
    
    // Propriedades de configuração
    property VersaoSistema: string;
    property CaminhoInstalacao: string;
  end;

implementation

var
  FInstanciaLock: TObject;

constructor TConfiguracoesGlobais.CreatePrivate;
begin
  inherited Create;
end;

class function TConfiguracoesGlobais.Instancia: TConfiguracoesGlobais;
begin
  if not Assigned(FInstancia) then
  begin
    TMonitor.Enter(FInstanciaLock);
    try
      if not Assigned(FInstancia) then
        FInstancia := TConfiguracoesGlobais.CreatePrivate;
    finally
      TMonitor.Exit(FInstanciaLock);
    end;
  end;
  
  Result := FInstancia;
end;

class destructor TConfiguracoesGlobais.Destroy;
begin
  FreeAndNil(FInstancia);
end;

initialization
  FInstanciaLock := TObject.Create;

finalization
  FreeAndNil(FInstanciaLock);`

---

## **✅ Boas Práticas Específicas do Sol.NET**

### **1. Declaração de Variáveis Inline**

delphi

`// ✅ Padrão Sol.NET - Declaração inline com inicialização
procedure ProcessarPedido;
begin
  var Pedido: TPedido := TPedido.Create;
  try
    var Total: Double := 0.0;
    var I: Integer := 0;
    
    for I := 0 to Pedido.Itens.Count - 1 do
      Total := Total + Pedido.Itens[I].ValorTotal;
      
    Pedido.ValorTotal := Total;
  finally
    Pedido.Free;
  end;
end;

// ❌ Evitar - Declaração tradicional (exceto quando necessário)
procedure ProcessarPedido;
var
  Pedido: TPedido;
  Total: Double;
  I: Integer;
begin
  // ...
end;`

### **2. Uso de Try-Finally para Liberação de Recursos**

delphi

`// ✅ Correto - Sempre proteger recursos alocados
procedure ConsultarCliente(AIdCliente: Integer);
begin
  var Cliente: TCliente := TCliente.Create;
  try
    Cliente.Carregar(AIdCliente);
    // Usar cliente
  finally
    Cliente.Free;
  end;
end;

// ❌ Evitar - Sem proteção
procedure ConsultarCliente(AIdCliente: Integer);
begin
  var Cliente: TCliente := TCliente.Create;
  Cliente.Carregar(AIdCliente);
  Cliente.Free;  // Não será executado se houver exceção
end;`

### **3. Métodos Pequenos e Coesos**

delphi

`// ✅ Correto - Métodos pequenos com responsabilidade única
procedure TControlePedido.ProcessarPedido(APedido: TPedido);
begin
  ValidarPedido(APedido);
  CalcularTotais(APedido);
  AplicarDescontos(APedido);
  GerarFinanceiro(APedido);
  AtualizarEstoque(APedido);
  EnviarNotificacao(APedido);
end;

procedure TControlePedido.ValidarPedido(APedido: TPedido);
begin
  if APedido.Itens.Count = 0 then
    raise Exception.Create('Pedido sem itens');
    
  if APedido.Cliente.IdPessoa = 0 then
    raise Exception.Create('Cliente não informado');
end;

procedure TControlePedido.CalcularTotais(APedido: TPedido);
begin
  var Total: Double := 0.0;
  
  for var I := 0 to APedido.Itens.Count - 1 do
    Total := Total + APedido.Itens[I].ValorTotal;
    
  APedido.ValorTotal := Total;
end;

// ❌ Evitar - Método grande com múltiplas responsabilidades
procedure TControlePedido.ProcessarPedido(APedido: TPedido);
begin
  // Validações
  // Cálculos
  // Descontos
  // Financeiro
  // Estoque
  // Notificações
  // ... 200+ linhas
end;`

### **4. Nomenclatura Descritiva**

delphi

`// ✅ Correto - Nomes descritivos e auto-explicativos
function TControlePedido.ObterPedidosPendentesDeAprovacao: TList<TPedido>;
procedure TControlePedido.AprovarPedido(AIdPedido: Integer);
function TCalculoTributos.CalcularICMSPorUF(AValor: Double; AUF: string): Double;

// ❌ Evitar - Nomes genéricos ou abreviados
function TControlePedido.GetPed: TList<TPedido>;
procedure TControlePedido.Proc(Id: Integer);
function TCalculoTributos.CalcIcms(V: Double; U: string): Double;`

### **5. Comentários Significativos**

delphi

`/// <summary>
/// Calcula o preço de venda baseado no custo médio e margem de lucro
/// </summary>
/// <param name="ACustoMedio">Custo médio do produto</param>
/// <param name="AMargemLucro">Margem de lucro desejada (percentual)</param>
/// <returns>Preço de venda calculado</returns>
/// <remarks>
/// A fórmula aplicada é: PrecoVenda = CustoMedio * (1 + MargemLucro / 100)
/// </remarks>
function TCalculoPreco.CalcularPrecoVenda(ACustoMedio, AMargemLucro: Double): Double;
begin
  if ACustoMedio <= 0 then
    raise Exception.Create('Custo médio deve ser maior que zero');
    
  Result := ACustoMedio * (1 + AMargemLucro / 100);
end;`

---

## **🔍 Checklist de Qualidade POO**

### **Design de Classes**

- [ ]  Classe tem responsabilidade única e bem definida
- [ ]  Nome da classe é descritivo e segue convenção (prefixo T)
- [ ]  Herança é usada apropriadamente (relação "é um")
- [ ]  Composição é preferida quando adequado (relação "tem um")
- [ ]  Métodos são pequenos e coesos (máximo 50 linhas)
- [ ]  Campos privados usam prefixo F
- [ ]  Propriedades expõem interface pública sem prefixo

### **Encapsulamento**

- [ ]  Campos são sempre privados ou protected
- [ ]  Acesso a campos é feito via propriedades
- [ ]  Validações são implementadas em setters quando necessário
- [ ]  Estado interno é protegido de acesso externo indevido

### **Herança e Polimorfismo**

- [ ]  Métodos virtuais são usados quando extensibilidade é necessária
- [ ]  Métodos abstratos definem contratos para classes derivadas
- [ ]  Override é usado corretamente em métodos sobrescritos
- [ ]  Inherited é chamado quando necessário preservar comportamento base

### **Interfaces**

- [ ]  Interfaces definem contratos claros
- [ ]  Interfaces são preferidas para injeção de dependência
- [ ]  Implementações concretas dependem de abstrações (interfaces)
- [ ]  GUIDs são únicos e bem formados

### **Gerenciamento de Recursos**

- [ ]  Construtores alocam recursos necessários
- [ ]  Destrutores liberam todos os recursos alocados
- [ ]  Try-finally protege liberação de recursos
- [ ]  TObjectList com OwnsObjects gerencia objetos filhos

---

## **📚 Referências e Recursos Adicionais**

### **Documentação Oficial Delphi**

- [Object Pascal Style Guide](https://docwiki.embarcadero.com/RADStudio/en/Object_Pascal_Style_Guide)
- [Classes and Objects](https://docwiki.embarcadero.com/RADStudio/en/Classes_and_Objects)

### **Princípios SOLID**

- **S**ingle Responsibility Principle
- **O**pen/Closed Principle
- **L**iskov Substitution Principle
- **I**nterface Segregation Principle
- **D**ependency Inversion Principle

### **Livros Recomendados**

- "Clean Code" - Robert C. Martin
- "Design Patterns" - Gang of Four
- "Refactoring" - Martin Fowler

---

## **📝 Histórico de Versões**

| **Versão** | **Data** | **Descrição** |
| --- | --- | --- |
| 1.0.0 | 2025-10-16 | Versão inicial da documentação POO Sol.NET |

---

**Última atualização**: 16 de outubro de 2025

**Versão**: 1.0.0

**Responsável**: Equipe de Desenvolvimento Sol.NET

**Elaborado por**: Copilot Assistant

*Esta documentação estabelece os padrões fundamentais de POO para o projeto Sol.NET, garantindo código consistente, manutenível e de alta qualidade.*
