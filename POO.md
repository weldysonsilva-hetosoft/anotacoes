# Documentação Completa - Programação Orientada a Objetos em Delphi

## Índice

1. [Introdução à POO](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
2. [Classes e Objetos](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
3. [Encapsulamento](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
4. [Herança](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
5. [Polimorfismo](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
6. [Interfaces](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
7. [Classes Abstratas](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
8. [Properties (Propriedades)](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
9. [Construtores e Destrutores](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
10. [Métodos de Classe (Class Methods)](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
11. [Delegates e Events](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
12. [Generics](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
13. [RTTI (Run-Time Type Information)](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)
14. [Boas Práticas](https://www.notion.so/POO-2796d2e107ee8079a812c45df545b2b3?pvs=21)

---

## Introdução à POO

A Programação Orientada a Objetos (POO) é um paradigma de programação que organiza o código em torno de objetos que contêm dados (atributos) e comportamentos (métodos).

### Pilares da POO

- **Encapsulamento**: Ocultar detalhes de implementação
- **Herança**: Reutilizar código através de hierarquias
- **Polimorfismo**: Usar interfaces comuns para diferentes implementações
- **Abstração**: Modelar conceitos do mundo real

---

## Classes e Objetos

### Definindo uma Classe

```pascal

`type
  TPessoa = class
  private
    FNome: string;
    FIdade: Integer;
  public
    procedure SetNome(const Value: string);
    function GetNome: string;
    procedure Apresentar;
  end;

implementation

procedure TPessoa.SetNome(const Value: string);
begin
  FNome := Value;
end;

function TPessoa.GetNome: string;
begin
  Result := FNome;
end;

procedure TPessoa.Apresentar;
begin
  ShowMessage('Olá, meu nome é ' + FNome);
end;`

### Criando e Usando Objetos

delphi

`var
  Pessoa: TPessoa;
begin
  Pessoa := TPessoa.Create;
  try
    Pessoa.SetNome('João');
    Pessoa.Apresentar;
  finally
    Pessoa.Free;
  end;
end;`
```

**Importante**: Sempre libere a memória com `Free` ou use `FreeAndNil`.

---

## Encapsulamento

O encapsulamento protege os dados internos da classe usando modificadores de acesso.

### Modificadores de Acesso

delphi

`type
  TContaBancaria = class
  strict private
    FSenha: string;  // Apenas a própria classe acessa
  private
    FSaldo: Currency;  // Mesmo unit pode acessar
  protected
    FTitular: string;  // Classe e descendentes acessam
  public
    procedure Depositar(Valor: Currency);  // Todos podem acessar
    function ConsultarSaldo: Currency;
  published
    property Titular: string read FTitular write FTitular;
  end;

procedure TContaBancaria.Depositar(Valor: Currency);
begin
  if Valor > 0 then
    FSaldo := FSaldo + Valor;
end;

function TContaBancaria.ConsultarSaldo: Currency;
begin
  Result := FSaldo;
end;`

---

## Herança

Permite criar classes baseadas em outras, reutilizando e estendendo funcionalidades.

### Exemplo Básico

delphi

`type
  TAnimal = class
  protected
    FNome: string;
  public
    procedure EmitirSom; virtual; abstract;
    property Nome: string read FNome write FNome;
  end;

  TCachorro = class(TAnimal)
  public
    procedure EmitirSom; override;
  end;

  TGato = class(TAnimal)
  public
    procedure EmitirSom; override;
  end;

implementation

procedure TCachorro.EmitirSom;
begin
  ShowMessage(FNome + ' faz: Au au!');
end;

procedure TGato.EmitirSom;
begin
  ShowMessage(FNome + ' faz: Miau!');
end;`

### Usando Herança

delphi

`var
  Animal: TAnimal;
begin
  Animal := TCachorro.Create;
  try
    Animal.Nome := 'Rex';
    Animal.EmitirSom;  // Chama a versão do TCachorro
  finally
    Animal.Free;
  end;
end;`

---

## Polimorfismo

Permite que objetos de diferentes classes sejam tratados através de uma interface comum.

### Tipos de Polimorfismo

### 1. Polimorfismo de Sobrescrita (Override)

delphi

`type
  TForma = class
  public
    function CalcularArea: Double; virtual;
  end;

  TRetangulo = class(TForma)
  private
    FLargura, FAltura: Double;
  public
    constructor Create(ALargura, AAltura: Double);
    function CalcularArea: Double; override;
  end;

  TCirculo = class(TForma)
  private
    FRaio: Double;
  public
    constructor Create(ARaio: Double);
    function CalcularArea: Double; override;
  end;

implementation

constructor TRetangulo.Create(ALargura, AAltura: Double);
begin
  inherited Create;
  FLargura := ALargura;
  FAltura := AAltura;
end;

function TRetangulo.CalcularArea: Double;
begin
  Result := FLargura * FAltura;
end;

constructor TCirculo.Create(ARaio: Double);
begin
  inherited Create;
  FRaio := ARaio;
end;

function TCirculo.CalcularArea: Double;
begin
  Result := Pi * FRaio * FRaio;
end;`

### 2. Polimorfismo com Interfaces

delphi

`type
  IImprimivel = interface
    ['{12345678-1234-1234-1234-123456789012}']
    procedure Imprimir;
  end;

  TRelatorio = class(TInterfacedObject, IImprimivel)
  public
    procedure Imprimir;
  end;

  TNotaFiscal = class(TInterfacedObject, IImprimivel)
  public
    procedure Imprimir;
  end;

procedure ImprimirDocumento(Documento: IImprimivel);
begin
  Documento.Imprimir;
end;`

---

## Interfaces

Interfaces definem contratos que as classes devem implementar.

### Definindo e Implementando Interfaces

delphi

`type
  IRepositorio = interface
    ['{GUID-AQUI}']
    procedure Inserir(const Dados: string);
    procedure Atualizar(const Dados: string);
    procedure Deletar(ID: Integer);
    function Buscar(ID: Integer): string;
  end;

  TRepositorioSQL = class(TInterfacedObject, IRepositorio)
  public
    procedure Inserir(const Dados: string);
    procedure Atualizar(const Dados: string);
    procedure Deletar(ID: Integer);
    function Buscar(ID: Integer): string;
  end;

  TRepositorioNoSQL = class(TInterfacedObject, IRepositorio)
  public
    procedure Inserir(const Dados: string);
    procedure Atualizar(const Dados: string);
    procedure Deletar(ID: Integer);
    function Buscar(ID: Integer): string;
  end;`

### Vantagens das Interfaces

- Desacoplamento entre classes
- Facilita testes (mocks)
- Permite herança múltipla de comportamento
- Reference counting automático

---

## Classes Abstratas

Classes que não podem ser instanciadas diretamente e servem como base para outras.

delphi

`type
  TProcessador = class abstract
  public
    procedure Processar; virtual; abstract;
    function Validar: Boolean; virtual;
  end;

  TProcessadorPagamento = class(TProcessador)
  public
    procedure Processar; override;
  end;

  TProcessadorEmail = class(TProcessador)
  public
    procedure Processar; override;
    function Validar: Boolean; override;
  end;

implementation

function TProcessador.Validar: Boolean;
begin
  Result := True;  // Implementação padrão
end;

procedure TProcessadorPagamento.Processar;
begin
  if Validar then
    ShowMessage('Processando pagamento...');
end;

procedure TProcessadorEmail.Processar;
begin
  if Validar then
    ShowMessage('Enviando e-mail...');
end;

function TProcessadorEmail.Validar: Boolean;
begin
  Result := True; // Validação específica
end;`

---

## Properties (Propriedades)

Properties fornecem uma interface elegante para acessar campos privados.

### Tipos de Properties

delphi

`type
  TProduto = class
  private
    FNome: string;
    FPreco: Currency;
    FDesconto: Double;
    function GetPrecoFinal: Currency;
    procedure SetDesconto(const Value: Double);
  public
    // Property de leitura e escrita
    property Nome: string read FNome write FNome;
    
    // Property com validação no set
    property Desconto: Double read FDesconto write SetDesconto;
    
    // Property calculada (somente leitura)
    property PrecoFinal: Currency read GetPrecoFinal;
    
    // Property com valor padrão
    property Preco: Currency read FPreco write FPreco default 0;
  end;

implementation

function TProduto.GetPrecoFinal: Currency;
begin
  Result := FPreco - (FPreco * FDesconto / 100);
end;

procedure TProduto.SetDesconto(const Value: Double);
begin
  if (Value >= 0) and (Value <= 100) then
    FDesconto := Value
  else
    raise Exception.Create('Desconto deve estar entre 0 e 100');
end;`

---

## Construtores e Destrutores

### Construtores

delphi

`type
  TCliente = class
  private
    FNome: string;
    FEmail: string;
    FCodigo: Integer;
  public
    constructor Create; overload;
    constructor Create(const ANome, AEmail: string); overload;
    constructor Create(ACodigo: Integer); overload;
    destructor Destroy; override;
  end;

implementation

constructor TCliente.Create;
begin
  inherited Create;
  // Inicialização padrão
end;

constructor TCliente.Create(const ANome, AEmail: string);
begin
  Create;  // Chama o construtor padrão
  FNome := ANome;
  FEmail := AEmail;
end;

constructor TCliente.Create(ACodigo: Integer);
begin
  Create;
  FCodigo := ACodigo;
  // Buscar dados do banco...
end;

destructor TCliente.Destroy;
begin
  // Liberar recursos
  inherited Destroy;
end;`

---

## Métodos de Classe (Class Methods)

Métodos que podem ser chamados sem instanciar a classe.

delphi

`type
  TCalculadora = class
  public
    class function Somar(A, B: Integer): Integer;
    class function Subtrair(A, B: Integer): Integer;
    class function Multiplicar(A, B: Integer): Integer;
  end;

implementation

class function TCalculadora.Somar(A, B: Integer): Integer;
begin
  Result := A + B;
end;

class function TCalculadora.Subtrair(A, B: Integer): Integer;
begin
  Result := A - B;
end;

class function TCalculadora.Multiplicar(A, B: Integer): Integer;
begin
  Result := A * B;
end;

// Uso:
var
  Resultado: Integer;
begin
  Resultado := TCalculadora.Somar(10, 20);  // Não precisa criar instância
end;`

### Singleton Pattern com Class Methods

delphi

`type
  TConfiguracoes = class
  private
    class var FInstancia: TConfiguracoes;
    constructor Create;
  public
    class function GetInstancia: TConfiguracoes;
    class destructor Destroy;
  end;

implementation

constructor TConfiguracoes.Create;
begin
  inherited Create;
end;

class function TConfiguracoes.GetInstancia: TConfiguracoes;
begin
  if FInstancia = nil then
    FInstancia := TConfiguracoes.Create;
  Result := FInstancia;
end;

class destructor TConfiguracoes.Destroy;
begin
  FInstancia.Free;
end;`

---

## Delegates e Events

### Declarando Events

delphi

`type
  TNotificacaoEvent = procedure(const Mensagem: string) of object;

  TProcesso = class
  private
    FOnInicio: TNotificacaoEvent;
    FOnFim: TNotificacaoEvent;
    FOnErro: TNotificacaoEvent;
  public
    procedure Executar;
    property OnInicio: TNotificacaoEvent read FOnInicio write FOnInicio;
    property OnFim: TNotificacaoEvent read FOnFim write FOnFim;
    property OnErro: TNotificacaoEvent read FOnErro write FOnErro;
  end;

implementation

procedure TProcesso.Executar;
begin
  if Assigned(FOnInicio) then
    FOnInicio('Processo iniciado');
    
  try
    // Processamento...
    Sleep(1000);
    
    if Assigned(FOnFim) then
      FOnFim('Processo concluído');
  except
    on E: Exception do
      if Assigned(FOnErro) then
        FOnErro('Erro: ' + E.Message);
  end;
end;

// Uso:
procedure TForm1.ProcessoIniciado(const Mensagem: string);
begin
  Memo1.Lines.Add(Mensagem);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Processo: TProcesso;
begin
  Processo := TProcesso.Create;
  try
    Processo.OnInicio := ProcessoIniciado;
    Processo.OnFim := ProcessoIniciado;
    Processo.Executar;
  finally
    Processo.Free;
  end;
end;`

---

## Generics

Generics permitem criar classes e métodos que trabalham com tipos não especificados.

### Lista Genérica

delphi

`type
  TLista<T> = class
  private
    FItens: array of T;
    FCount: Integer;
  public
    procedure Adicionar(const Item: T);
    function Obter(Index: Integer): T;
    property Count: Integer read FCount;
  end;

implementation

procedure TLista<T>.Adicionar(const Item: T);
begin
  SetLength(FItens, FCount + 1);
  FItens[FCount] := Item;
  Inc(FCount);
end;

function TLista<T>.Obter(Index: Integer): T;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FItens[Index]
  else
    raise Exception.Create('Índice fora do intervalo');
end;

// Uso:
var
  ListaStrings: TLista<string>;
  ListaInteiros: TLista<Integer>;
begin
  ListaStrings := TLista<string>.Create;
  try
    ListaStrings.Adicionar('Delphi');
    ListaStrings.Adicionar('POO');
  finally
    ListaStrings.Free;
  end;
end;`

### Constraints em Generics

delphi

`type
  TRepositorio<T: class, constructor> = class
  public
    function Criar: T;
    procedure Salvar(Entidade: T);
  end;

implementation

function TRepositorio<T>.Criar: T;
begin
  Result := T.Create;  // Funciona porque T tem constructor
end;

procedure TRepositorio<T>.Salvar(Entidade: T);
begin
  // Salvar no banco...
end;`

---

## RTTI (Run-Time Type Information)

RTTI permite inspecionar e manipular tipos em tempo de execução.

### Exemplo Básico

delphi

`uses
  System.RTTI;

type
  TPessoa = class
  private
    FNome: string;
    FIdade: Integer;
  published
    property Nome: string read FNome write FNome;
    property Idade: Integer read FIdade write FIdade;
  end;

procedure InspecionarObjeto(Obj: TObject);
var
  Contexto: TRttiContext;
  Tipo: TRttiType;
  Prop: TRttiProperty;
begin
  Contexto := TRttiContext.Create;
  try
    Tipo := Contexto.GetType(Obj.ClassType);
    
    for Prop in Tipo.GetProperties do
    begin
      ShowMessage(Format('Propriedade: %s = %s', 
        [Prop.Name, Prop.GetValue(Obj).ToString]));
    end;
  finally
    Contexto.Free;
  end;
end;`

---

## Boas Práticas

### 1. Nomenclatura

delphi

`// Classes começam com T
type
  TProduto = class
  
// Interfaces começam com I
  IProdutoRepository = interface
  
// Campos privados começam com F
private
  FNome: string;
  
// Parâmetros começam com A
procedure SetNome(const ANome: string);`

### 2. SOLID Principles

### Single Responsibility Principle

delphi

`// ❌ Ruim: Classe faz muitas coisas
type
  TCliente = class
    procedure Salvar;
    procedure EnviarEmail;
    procedure GerarRelatorio;
  end;

// ✅ Bom: Cada classe tem uma responsabilidade
type
  TCliente = class
    // Apenas dados e lógica de negócio
  end;
  
  TClienteRepository = class
    procedure Salvar(Cliente: TCliente);
  end;
  
  TEmailService = class
    procedure Enviar(Cliente: TCliente);
  end;`

### Dependency Inversion Principle

delphi

`// ✅ Dependa de abstrações, não de implementações
type
  ILogger = interface
    procedure Log(const Mensagem: string);
  end;
  
  TServico = class
  private
    FLogger: ILogger;
  public
    constructor Create(ALogger: ILogger);
    procedure Processar;
  end;

constructor TServico.Create(ALogger: ILogger);
begin
  FLogger := ALogger;
end;`

### 3. Gerenciamento de Memória

delphi

`// ✅ Sempre use try-finally
var
  Obj: TMinhaClasse;
begin
  Obj := TMinhaClasse.Create;
  try
    // Usar o objeto
  finally
    Obj.Free;
  end;
end;

// ✅ Ou use FreeAndNil
FreeAndNil(Obj);

// ✅ Para interfaces, não precisa liberar
var
  Intf: IMinhaInterface;
begin
  Intf := TMinhaClasse.Create;
  // Reference counting automático
end;`

### 4. Validação

delphi

`procedure TCliente.SetIdade(const Value: Integer);
begin
  if Value < 0 then
    raise EArgumentException.Create('Idade não pode ser negativa');
    
  if Value > 150 then
    raise EArgumentException.Create('Idade inválida');
    
  FIdade := Value;
end;`

### 5. Imutabilidade

delphi

`type
  TPonto = class
  private
    FX, FY: Double;
  public
    constructor Create(AX, AY: Double);
    property X: Double read FX;  // Somente leitura
    property Y: Double read FY;
    function Mover(DX, DY: Double): TPonto;  // Retorna novo objeto
  end;

function TPonto.Mover(DX, DY: Double): TPonto;
begin
  Result := TPonto.Create(FX + DX, FY + DY);
end;`

---

## Conclusão

Esta documentação cobre os principais conceitos de POO em Delphi. A prática constante e a leitura de código de qualidade são essenciais para dominar estes conceitos.

### Recursos Adicionais

- Documentação oficial Embarcadero
- Object Pascal Handbook (Marco Cantù)
- Delphi Programming (Ray Lischner)
- Clean Code e Design Patterns

### Próximos Passos

- Estudar Design Patterns (Factory, Builder, Observer, etc.)
- Praticar com projetos reais
- Explorar frameworks como Spring4D, DUnitX
- Aprender sobre arquiteturas (MVC, MVVM, Clean Architecture)
