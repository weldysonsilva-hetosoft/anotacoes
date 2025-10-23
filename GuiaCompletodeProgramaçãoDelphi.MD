# 📚 Guia Completo de Programação Delphi para o [Sol.NE](http://sol.net/)T

## 🎯 Sobre Este Guia

Este documento foi criado para desenvolvedores iniciantes e júniores que estão começando a trabalhar com o sistema [Sol.NET](http://sol.net/). Aqui você encontrará explicações detalhadas sobre os conceitos fundamentais de programação em Delphi, com exemplos práticos extraídos do próprio código do [Sol.NET](http://sol.net/).

---

## 📑 Índice

1. [Variáveis e Constantes](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
2. [Operadores](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
3. [Estruturas de Decisão](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
4. [Estruturas de Repetição (Loops)](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
5. [Funções e Procedimentos](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
6. [Trabalhando com Strings](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
7. [Arrays e Listas](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
8. [Classes e Objetos](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
9. [Construtores e Destrutores](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
10. [Encapsulamento](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
11. [Herança](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
12. [Polimorfismo](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
13. [Classes Abstratas e Interfaces](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)
14. [Boas Práticas no Sol.NET](https://www.notion.so/Guia-Completo-de-Programa-o-Delphi-para-o-Sol-NET-2956d2e107ee80019d14e0449edb771e?pvs=21)

---

## 1. Variáveis e Constantes

### 1.1 Definição de Escopo

O **escopo** determina onde uma variável pode ser acessada no código.

### Escopo Local

Variáveis declaradas dentro de um procedimento/função só existem dentro dele.

```
procedure ExemploEscopoLocal;
begin
  var MinhaVariavel: Integer := 10; // Existe apenas dentro deste procedimento
  ShowMessage(MinhaVariavel.ToString);
end; // MinhaVariavel deixa de existir aqui

```

### Escopo Global (Campos de Classe)

Variáveis declaradas na seção `private`, `protected` ou `public` de uma classe.

```
type
  TFrmConversao = class(TForm)
  private
    FNomeIntegracao: string; // Campo privado - acessível em toda a classe
    FDados: TDados;
  public
    property NomeIntegracao: string read FNomeIntegracao write FNomeIntegracao;
  end;

```

**Exemplo do [Sol.NET](http://sol.net/):**

```
// Em uFrmConversao.pas
type
  TFrmConversao = class(TForm)
  private
    FConexaoOrigem: TConexao;
    FConexaoDestino: TConexao;
    FControleUtilitarios: TControleUtilitarios;
  public
    procedure ConversaoPessoas(ParametrosConversao: TParametrosConversao);
  end;

```

### 1.2 Tipos Mais Comuns

| Tipo | Descrição | Exemplo |
| --- | --- | --- |
| `Integer` | Números inteiros (-2.147.483.648 a 2.147.483.647) | `var Idade: Integer := 25;` |
| `Double` | Números decimais | `var Preco: Double := 10.50;` |
| `String` | Texto | `var Nome: string := 'João';` |
| `Boolean` | Verdadeiro ou Falso | `var Ativo: Boolean := True;` |
| `TDateTime` | Data e hora | `var DataAtual: TDateTime := Now;` |
| `Currency` | Valores monetários | `var ValorTotal: Currency := 1500.75;` |

### 1.3 Declarando Variáveis (Padrão Inline)

No [Sol.NET](http://sol.net/), seguimos a convenção de **declaração inline** do Delphi 12.2:

```
// ✅ CORRETO - Padrão Sol.NET
procedure ProcessarPedido;
begin
  var IdPedido: Integer := 1001;
  var NomeCliente: string := 'Maria Silva';
  var ValorTotal: Double := 0.0;
  var DataPedido: TDateTime := Now;

  // Código de processamento...
end;

```

```
// ❌ EVITAR - Padrão antigo
procedure ProcessarPedido;
var
  IdPedido: Integer;
  NomeCliente: string;
  ValorTotal: Double;
begin
  IdPedido := 1001;
  NomeCliente := 'Maria Silva';
  // ...
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em ConversaoBuilder.pas
function TBaseBuilder<T>.AddCampo(
  const CampoDestino, CampoOrigem: string;
  TabelaBuscar: ITabelaConversao;
  const BuscarDinamico, Apelido: string;
  ExecutarFuncoes: TArray<TFuncoes>
): IParametroSQL<T>;
begin
  var Campo: TCamposConversao := TCamposConversao.Create;
  Campo.CampoDestino := CampoDestino;
  Campo.CampoOrigem := CampoOrigem;
  Campo.TabelaBuscar := TabelaBuscar;
  Campo.BuscarDinamico := BuscarDinamico;
  Campo.Apelido := Apelido;
  Campo.ExecutarFuncoes := ExecutarFuncoes;

  FParametroSQLAtual.ListaCampos.Add(Campo);
  Result := Self;
end;

```

### 1.4 Declarando Constantes

Constantes são valores que não podem ser alterados durante a execução.

```
const
  TAXA_PADRAO: Double = 0.05;
  NOME_SISTEMA: string = 'Sol.NET';
  VERSAO_MINIMA: Integer = 12;
  DIAS_SEMANA: array[0..6] of string = (
    'Domingo', 'Segunda', 'Terça', 'Quarta',
    'Quinta', 'Sexta', 'Sábado'
  );

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Constantes de tipos de integração fiscal
const
  TIPO_SCHMITT = 'Schmitt';
  TIPO_IMENDES = 'IMendes';
  TIPO_MIXFISCAL = 'MixFiscal';

```

### 1.5 Declarando Arrays

### Array Estático

```
var Notas: array[0..3] of Double := [7.5, 8.0, 9.2, 6.8];
var Nomes: array[0..2] of string := ['Ana', 'Bruno', 'Carlos'];

```

### Array Dinâmico

```
var ListaIds: TArray<Integer>;
begin
  SetLength(ListaIds, 5);
  ListaIds[0] := 100;
  ListaIds[1] := 200;
  // ...
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em Integracao.Tipos.pas
type
  TArray<TFuncoes> = array of TFuncoes; // Array dinâmico de funções

// Uso:
Campo.ExecutarFuncoes := [TFuncoes.SoNumeros, TFuncoes.Maiuscula];

```

---

## 2. Operadores

### 2.1 Operadores Lógicos

| Operador | Significado | Exemplo |
| --- | --- | --- |
| `=` | Igual | `if Idade = 18 then` |
| `<>` | Diferente | `if Status <> 'Ativo' then` |
| `>` | Maior | `if Valor > 100 then` |
| `>=` | Maior ou igual | `if Nota >= 7.0 then` |
| `<` | Menor | `if Estoque < 10 then` |
| `<=` | Menor ou igual | `if Desconto <= 0.5 then` |
| `and` | E lógico | `if (Idade >= 18) and (Ativo) then` |
| `or` | Ou lógico | `if (Tipo = 'A') or (Tipo = 'B') then` |
| `not` | Negação | `if not Processado then` |

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em uFrmConversao.pas
procedure TFrmConversao.ValidarParametros;
begin
  if (FConexaoOrigem = nil) or (FConexaoDestino = nil) then
    raise EExcecaoUsuario.Create('Conexões não foram configuradas!');

  if (txtEmpresaOrigem.Text = '') and (txtEmpresaDestino.Text = '') then
    raise EExcecaoUsuario.Create('Informe pelo menos uma empresa!');
end;

```

### 2.2 Operadores Matemáticos

| Operador | Operação | Exemplo |
| --- | --- | --- |
| `+` | Adição | `Total := Valor1 + Valor2;` |
| `-` | Subtração | `Desconto := Total - 50;` |
| `*` | Multiplicação | `Area := Base * Altura;` |
| `/` | Divisão (real) | `Media := Soma / 4;` |
| `div` | Divisão (inteira) | `Metade := 10 div 2; // 5` |
| `mod` | Resto da divisão | `Resto := 10 mod 3; // 1` |

### Precedência de Operadores

1. `not` (negação)
2. , `/`, `div`, `mod`, `and`
3. `+`, , `or`
4. `=`, `<>`, `<`, `>`, `<=`, `>=`

```
// Sem parênteses - pode gerar confusão
var Resultado := 10 + 5 * 2; // = 20 (multiplicação primeiro)

// ✅ Com parênteses - mais claro
var Resultado := (10 + 5) * 2; // = 30

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Cálculo de desconto
procedure CalcularDesconto;
begin
  var ValorOriginal: Double := 100.0;
  var PercentualDesconto: Double := 0.15; // 15%
  var ValorDesconto: Double := ValorOriginal * PercentualDesconto;
  var ValorFinal: Double := ValorOriginal - ValorDesconto;

  ShowMessage(Format('Valor com desconto: R$ %.2f', [ValorFinal]));
end;

```

---

## 3. Estruturas de Decisão

### 3.1 IF / ELSE

```
// IF simples
if Idade >= 18 then
  ShowMessage('Maior de idade');

// IF com ELSE
if Estoque > 0 then
  ShowMessage('Produto disponível')
else
  ShowMessage('Produto indisponível');

// IF aninhado
if Usuario.Ativo then
begin
  if Usuario.Tipo = 'Admin' then
    ShowMessage('Bem-vindo, Administrador!')
  else
    ShowMessage('Bem-vindo, Usuário!');
end
else
  ShowMessage('Usuário inativo');

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em ConversaoBuilder.pas
function TBaseBuilder<T>.AddPrimaryKey(const CampoPKOrigem: string): IParametroSQL<T>;
begin
  if not Assigned(FParametroSQLAtual.TabelaConversao) then
    raise EExcecaoDesenvolvedor.Create(
      'Necessário definir a tabela de conversão antes de adicionar chave primária!'
    );

  if CampoPKOrigem <> '' then
  begin
    FParametroSQLAtual.AdicionarCamposPk(
      FParametroSQLAtual.TabelaConversao.GetCampoPKDestino,
      CampoPKOrigem
    );
  end;

  Result := Self;
end;

```

### 3.2 CASE / OF (Switch)

```
case TipoUsuario of
  1: ShowMessage('Cliente');
  2: ShowMessage('Fornecedor');
  3: ShowMessage('Funcionário');
else
  ShowMessage('Tipo desconhecido');
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Processamento por tipo de integração fiscal
procedure ProcessarIntegracaoFiscal(Tipo: TTipoIntegracaoFiscal);
begin
  case Tipo of
    tifSchmitt: ProcessarSchmitt;
    tifIMendes: ProcessarIMendes;
    tifMixFiscal: ProcessarMixFiscal;
  else
    raise Exception.Create('Tipo de integração fiscal não suportado');
  end;
end;

```

---

## 4. Estruturas de Repetição (Loops)

### 4.1 FOR Loop

```
// FOR crescente
for var I: Integer := 0 to 10 do
  ShowMessage('Iteração: ' + I.ToString);

// FOR decrescente
for var I: Integer := 10 downto 0 do
  ShowMessage('Contagem regressiva: ' + I.ToString);

// FOR com array
var Nomes: TArray<string> := ['Ana', 'Bruno', 'Carlos'];
for var Nome in Nomes do
  ShowMessage('Nome: ' + Nome);

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em uConversao.TConversaoGenerica.pas
procedure ProcessarRegistros;
begin
  for var I: Integer := 0 to cdsOrigem.RecordCount - 1 do
  begin
    cdsOrigem.RecNo := I + 1;

    // Processar cada registro
    ProcessarRegistroAtual;

    // Atualizar progresso
    AtualizarProgresso(I + 1, cdsOrigem.RecordCount);
  end;
end;

```

### 4.2 WHILE Loop

```
var Contador: Integer := 0;
while Contador < 10 do
begin
  ShowMessage('Contador: ' + Contador.ToString);
  Inc(Contador); // Incrementa em 1
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Processamento de registros até encontrar condição
procedure ProcessarAteEncontrar;
begin
  cdsOrigem.First;
  while not cdsOrigem.Eof do
  begin
    if cdsOrigem.FieldByName('STATUS').AsString = 'PROCESSADO' then
      Break; // Sai do loop

    ProcessarRegistro;
    cdsOrigem.Next;
  end;
end;

```

### 4.3 REPEAT / UNTIL

```
var Tentativas: Integer := 0;
repeat
  Inc(Tentativas);
  TentarConexao;
until (Conectado) or (Tentativas >= 3);

```

### 4.4 BREAK e CONTINUE

```
// BREAK - Sai do loop
for var I: Integer := 0 to 100 do
begin
  if I = 50 then
    Break; // Para no 50
  ProcessarItem(I);
end;

// CONTINUE - Pula para próxima iteração
for var I: Integer := 0 to 100 do
begin
  if I mod 2 = 0 then
    Continue; // Pula números pares
  ProcessarImpar(I);
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Processar apenas registros válidos
procedure ProcessarRegistrosValidos;
begin
  for var I: Integer := 0 to ListaProdutos.Count - 1 do
  begin
    var Produto := ListaProdutos[I];

    // Pular produtos inativos
    if not Produto.Ativo then
      Continue;

    // Parar se encontrar erro crítico
    if Produto.PrecoInvalido then
      Break;

    ProcessarProduto(Produto);
  end;
end;

```

---

## 5. Funções e Procedimentos

### 5.1 Diferença entre Function e Procedure

**Procedure:** Não retorna valor

```
procedure ExibirMensagem(const Texto: string);
begin
  ShowMessage(Texto);
end;

```

**Function:** Retorna um valor

```
function Somar(A, B: Integer): Integer;
begin
  Result := A + B;
end;

```

### 5.2 Parâmetros

### Por Valor (Padrão)

```
procedure Incrementar(Valor: Integer);
begin
  Valor := Valor + 1; // Não afeta variável original
end;

var X: Integer := 10;
Incrementar(X);
// X ainda é 10

```

### Por Referência (var)

```
procedure Incrementar(var Valor: Integer);
begin
  Valor := Valor + 1; // Afeta variável original
end;

var X: Integer := 10;
Incrementar(X);
// X agora é 11

```

### Parâmetro Constante (const)

```
function CalcularDesconto(const ValorOriginal: Double;
  const Percentual: Double): Double;
begin
  // Não pode modificar ValorOriginal ou Percentual
  Result := ValorOriginal * (1 - Percentual);
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em ConversaoBuilder.pas
function TBaseBuilder<T>.AddCampo(
  const CampoDestino, CampoOrigem: string;  // const - otimização
  TabelaBuscar: ITabelaConversao;           // por valor (interface)
  const BuscarDinamico, Apelido: string;
  ExecutarFuncoes: TArray<TFuncoes>
): IParametroSQL<T>;
begin
  var Campo: TCamposConversao := TCamposConversao.Create;
  Campo.CampoDestino := CampoDestino;
  Campo.CampoOrigem := CampoOrigem;

  FParametroSQLAtual.ListaCampos.Add(Campo);
  Result := Self; // Retorna a própria instância (fluent interface)
end;

```

### 5.3 Sobrecarga (Overload)

Múltiplas versões da mesma função com parâmetros diferentes:

```
// Versão com 2 parâmetros
function Somar(A, B: Integer): Integer; overload;
begin
  Result := A + B;
end;

// Versão com 3 parâmetros
function Somar(A, B, C: Integer): Integer; overload;
begin
  Result := A + B + C;
end;

// Uso
var Resultado1 := Somar(5, 10);      // Usa primeira versão
var Resultado2 := Somar(5, 10, 15);  // Usa segunda versão

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Diferentes versões de conversão de pessoas
procedure ConversaoPessoas(ParametrosConversao: TParametrosConversao); overload;
procedure ConversaoPessoas(
  ParametrosConversao: TParametrosConversao;
  ParametrosEndereco: TParametrosSubConversao
); overload;
procedure ConversaoPessoas(
  ParametrosConversao: TParametrosConversao;
  ParametrosEndereco: TParametrosSubConversao;
  ParametrosContatos: TParametrosSubConversao
); overload;

```

---

## 6. Trabalhando com Strings

### 6.1 Funções Básicas de String

```
var Texto: string := 'Sol.NET ERP';

// Tamanho da string
var Tamanho: Integer := Length(Texto); // 11

// Maiúsculas e minúsculas
var Maiuscula: string := UpperCase(Texto);   // 'SOL.NET ERP'
var Minuscula: string := LowerCase(Texto);   // 'sol.net erp'

// Posição de substring
var Posicao: Integer := Pos('NET', Texto);   // 5

// Copiar parte da string
var Parte: string := Copy(Texto, 1, 7);      // 'Sol.NET'

// Remover espaços
var ComEspacos: string := '  Texto  ';
var SemEspacos: string := Trim(ComEspacos);  // 'Texto'

// Substituir texto
var NovoTexto: string := StringReplace(Texto, 'ERP', 'Sistema', []);

```

### 6.2 Concatenação

```
// Operador +
var Nome: string := 'João';
var Sobrenome: string := 'Silva';
var NomeCompleto: string := Nome + ' ' + Sobrenome;

// Format (mais profissional)
var Idade: Integer := 25;
var Mensagem: string := Format('%s tem %d anos', [Nome, Idade]);

// Concatenação múltipla
var SQL: string := 'SELECT * FROM produtos ' +
                   'WHERE ativo = 1 ' +
                   'AND estoque > 0';

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Montagem de SQL dinâmico
function MontarSQL: string;
begin
  var SQL: TStringBuilder := TStringBuilder.Create;
  try
    SQL.Append('SELECT ');
    SQL.Append('  p.ID_PRODUTO, ');
    SQL.Append('  p.DESCRICAO, ');
    SQL.Append('  p.PRECO ');
    SQL.Append('FROM PRODUTO p ');
    SQL.Append('WHERE p.ATIVO = 1 ');

    if FiltroCategoria <> '' then
      SQL.AppendFormat('  AND p.CATEGORIA = %s ', [QuotedStr(FiltroCategoria)]);

    Result := SQL.ToString;
  finally
    SQL.Free;
  end;
end;

```

### 6.3 QuotedStr - Muito Importante!

```
// ❌ ERRADO - Vulnerável a SQL Injection
var Nome: string := 'João';
var SQL: string := 'SELECT * FROM clientes WHERE nome = ' + Nome;
// Gera: SELECT * FROM clientes WHERE nome = João (ERRO!)

// ✅ CORRETO - Usa QuotedStr
var SQL: string := 'SELECT * FROM clientes WHERE nome = ' + QuotedStr(Nome);
// Gera: SELECT * FROM clientes WHERE nome = 'João'

```

---

## 7. Arrays e Listas

### 7.1 TArray<T> (Array Dinâmico Genérico)

```
// Declaração e inicialização
var Numeros: TArray<Integer> := [1, 2, 3, 4, 5];
var Nomes: TArray<string> := ['Ana', 'Bruno', 'Carlos'];

// Definir tamanho dinamicamente
var Lista: TArray<Integer>;
SetLength(Lista, 10);

// Acessar elementos
Lista[0] := 100;
var Valor := Lista[0];

// Iterar
for var Numero in Numeros do
  ShowMessage(Numero.ToString);

```

### 7.2 TList<T> (Lista Genérica)

```
uses System.Generics.Collections;

var ListaNomes: TList<string> := TList<string>.Create;
try
  // Adicionar
  ListaNomes.Add('Ana');
  ListaNomes.Add('Bruno');
  ListaNomes.Add('Carlos');

  // Acessar
  var Primeiro: string := ListaNomes[0];
  var Quantidade: Integer := ListaNomes.Count;

  // Remover
  ListaNomes.Delete(1);        // Remove 'Bruno'
  ListaNomes.Remove('Carlos'); // Remove por valor

  // Verificar existência
  if ListaNomes.Contains('Ana') then
    ShowMessage('Ana está na lista');

  // Limpar
  ListaNomes.Clear;
finally
  ListaNomes.Free;
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em Integracao.Tipos.pas
type
  TListaCallbacks = class(TObjectList<TCallbackConversao>)
  public
    procedure ExecutarCallbacks(Momento: TMomentoConversao;
      Conversao: TObject);
  end;

procedure TListaCallbacks.ExecutarCallbacks(
  Momento: TMomentoConversao;
  Conversao: TObject
);
begin
  for var Callback in Self do
  begin
    if Callback.Momento = Momento then
      Callback.Proc(Conversao);
  end;
end;

```

### 7.3 TClientDataSet (Dataset em Memória)

Muito usado no [Sol.NET](http://sol.net/) para manipulação de dados:

```
var cds: TClientDataSet := TClientDataSet.Create(nil);
try
  // Criar estrutura
  cds.FieldDefs.Add('ID', ftInteger);
  cds.FieldDefs.Add('NOME', ftString, 100);
  cds.FieldDefs.Add('PRECO', ftFloat);
  cds.CreateDataSet;

  // Inserir registro
  cds.Append;
  cds.FieldByName('ID').AsInteger := 1;
  cds.FieldByName('NOME').AsString := 'Produto A';
  cds.FieldByName('PRECO').AsFloat := 10.50;
  cds.Post;

  // Navegar
  cds.First;
  while not cds.Eof do
  begin
    var Nome := cds.FieldByName('NOME').AsString;
    ShowMessage(Nome);
    cds.Next;
  end;

  // Localizar (importante no Sol.NET)
  if cds.Locate('ID', 1, []) then
    ShowMessage('Encontrado!');
finally
  cds.Free;
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Função LocalizarBinário (melhor performance)
function LocalizarBinario(
  cds: TClientDataSet;
  const Campo: string;
  const Valor: Variant
): Boolean;
begin
  // Dataset DEVE estar ordenado pelo campo
  cds.IndexFieldNames := Campo;
  Result := cds.FindKey([Valor]);
end;

```

---

## 8. Classes e Objetos

### 8.1 Definição de Classe

```
type
  TProduto = class
  private
    FId: Integer;
    FDescricao: string;
    FPreco: Double;
  public
    constructor Create(Id: Integer; const Descricao: string; Preco: Double);
    destructor Destroy; override;

    property Id: Integer read FId write FId;
    property Descricao: string read FDescricao write FDescricao;
    property Preco: Double read FPreco write FPreco;

    function CalcularPrecoComDesconto(Percentual: Double): Double;
  end;

```

### 8.2 Implementação

```
constructor TProduto.Create(Id: Integer; const Descricao: string; Preco: Double);
begin
  inherited Create; // Chama construtor da classe pai (TObject)
  FId := Id;
  FDescricao := Descricao;
  FPreco := Preco;
end;

destructor TProduto.Destroy;
begin
  // Limpar recursos se necessário
  inherited; // Sempre chamar no final
end;

function TProduto.CalcularPrecoComDesconto(Percentual: Double): Double;
begin
  Result := FPreco * (1 - Percentual);
end;

```

### 8.3 Criação e Uso de Objetos

```
procedure TestarProduto;
begin
  var Produto: TProduto := TProduto.Create(1, 'Mouse USB', 25.90);
  try
    ShowMessage('Produto: ' + Produto.Descricao);
    ShowMessage('Preço: R$ ' + FormatFloat('0.00', Produto.Preco));

    var PrecoComDesconto := Produto.CalcularPrecoComDesconto(0.10);
    ShowMessage('Com 10% desconto: R$ ' + FormatFloat('0.00', PrecoComDesconto));
  finally
    Produto.Free; // SEMPRE liberar memória!
  end;
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em uConversao.TiposAuxiliares.pas
type
  TCamposConversao = class
  private
    FCampoDestino: string;
    FCampoOrigem: string;
    FTabelaBuscar: ITabelaConversao;
    FExecutarFuncoes: TArray<TFuncoes>;
  public
    property CampoDestino: string read FCampoDestino write FCampoDestino;
    property CampoOrigem: string read FCampoOrigem write FCampoOrigem;
    property TabelaBuscar: ITabelaConversao read FTabelaBuscar write FTabelaBuscar;
    property ExecutarFuncoes: TArray<TFuncoes> read FExecutarFuncoes write FExecutarFuncoes;
  end;

```

---

## 9. Construtores e Destrutores

### 9.1 Construtor Padrão

```
type
  TCliente = class
  private
    FNome: string;
    FAtivo: Boolean;
  public
    constructor Create; // Construtor sem parâmetros
  end;

constructor TCliente.Create;
begin
  inherited;
  FNome := 'Novo Cliente';
  FAtivo := True;
end;

```

### 9.2 Construtores Parametrizados

```
type
  TCliente = class
  public
    constructor Create; overload;
    constructor Create(const Nome: string); overload;
    constructor Create(const Nome: string; Ativo: Boolean); overload;
  end;

constructor TCliente.Create;
begin
  inherited;
  FNome := 'Novo Cliente';
  FAtivo := True;
end;

constructor TCliente.Create(const Nome: string);
begin
  Create; // Chama o construtor sem parâmetros
  FNome := Nome;
end;

constructor TCliente.Create(const Nome: string; Ativo: Boolean);
begin
  Create(Nome); // Chama o construtor com um parâmetro
  FAtivo := Ativo;
end;

```

### 9.3 Destrutor

```
type
  TGerenciador = class
  private
    FLista: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TGerenciador.Create;
begin
  inherited;
  FLista := TList<string>.Create;
end;

destructor TGerenciador.Destroy;
begin
  FLista.Free; // Liberar objetos criados
  inherited;   // SEMPRE chamar inherited no final
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em ConversaoBuilder.pas
type
  TBaseBuilder<T: TParametrosConversao> = class(TInterfacedObject,
    IConversaoBuilder<T>, IParametroSQL<T>)
  private
    FParametrosConversao: T;
    FParametroSQLAtual: TParametroSQL;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TBaseBuilder<T>.Create;
begin
  inherited;
  FParametrosConversao := T.Create(nil);
  FParametroSQLAtual := TParametroSQL.Create;
end;

destructor TBaseBuilder<T>.Destroy;
begin
  // FParametrosConversao será liberado por quem chamou Build
  // FParametroSQLAtual já foi adicionado aos parâmetros
  inherited;
end;

```

---

## 10. Encapsulamento

### 10.1 Modificadores de Acesso

```
type
  TPessoa = class
  strict private
    FSenha: string;  // Só acessível dentro desta classe
  private
    FIdade: Integer; // Acessível nesta unit
  protected
    FNome: string;   // Acessível em classes filhas
  public
    FEmail: string;  // Acessível em qualquer lugar (evitar!)
  end;

```

### 10.2 Properties (Getters e Setters)

```
type
  TContaBancaria = class
  private
    FSaldo: Double;
    function GetSaldo: Double;
    procedure SetSaldo(const Value: Double);
  public
    property Saldo: Double read GetSaldo write SetSaldo;
  end;

function TContaBancaria.GetSaldo: Double;
begin
  // Pode adicionar lógica antes de retornar
  LogAdd('Consultando saldo...');
  Result := FSaldo;
end;

procedure TContaBancaria.SetSaldo(const Value: Double);
begin
  // Validação antes de atribuir
  if Value < 0 then
    raise Exception.Create('Saldo não pode ser negativo!');
  FSaldo := Value;
end;

```

### 10.3 Property Somente Leitura

```
type
  TProduto = class
  private
    FId: Integer;
    FDescricao: string;
    FPreco: Double;
  public
    property Id: Integer read FId;  // Somente leitura
    property Descricao: string read FDescricao write FDescricao;
    property Preco: Double read FPreco write FPreco;
  end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em Integracao.Tipos.pas
type
  TResumoIntegracao = class
  private
    FRecebidos: Integer;
    FEnviados: Integer;
    FAtualizados: Integer;
    FExcluidos: Integer;
  public
    property Recebidos: Integer read FRecebidos;
    property Enviados: Integer read FEnviados;
    property Atualizados: Integer read FAtualizados;
    property Excluidos: Integer read FExcluidos;

    function IncRecebidos(Recebidos: Integer = 0): TResumoIntegracao;
    function ToString(ExibirZeros: Boolean = False): string; override;
  end;

```

---

## 11. Herança

### 11.1 Conceito Básico

```
// Classe base (pai)
type
  TAnimal = class
  protected
    FNome: string;
  public
    constructor Create(const Nome: string);
    procedure Comer; virtual;  // Pode ser sobrescrito
    property Nome: string read FNome write FNome;
  end;

// Classe derivada (filha)
type
  TCachorro = class(TAnimal)
  public
    procedure Latir;
    procedure Comer; override;  // Sobrescreve método da classe pai
  end;

constructor TAnimal.Create(const Nome: string);
begin
  inherited Create;
  FNome := Nome;
end;

procedure TAnimal.Comer;
begin
  ShowMessage(FNome + ' está comendo');
end;

procedure TCachorro.Latir;
begin
  ShowMessage(FNome + ' está latindo: Au au!');
end;

procedure TCachorro.Comer;
begin
  inherited; // Chama TAnimal.Comer primeiro
  ShowMessage('E abanando o rabo!');
end;

```

### 11.2 Virtual vs Override

- **virtual**: Marca método que PODE ser sobrescrito
- **override**: Sobrescreve método virtual da classe pai
- **reintroduce**: Cria nova implementação (não sobrescreve)

```
type
  TBase = class
    procedure Metodo1; virtual;
    procedure Metodo2;
  end;

  TDerivada = class(TBase)
    procedure Metodo1; override;     // Sobrescreve
    procedure Metodo2; reintroduce;  // Nova implementação
  end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Em uFrmConversao.pas (classe base)
type
  TFrmConversao = class(TForm)
  protected
    procedure ConversaoPessoas(ParametrosConversao: TParametrosConversao); virtual;
    procedure ConversaoProduto(ParametrosConversao: TParametrosConversao); virtual;
  end;

// Classes filhas específicas de clientes
type
  TFrmConversaoCliente = class(TFrmConversao)
  protected
    procedure ConversaoPessoas(ParametrosConversao: TParametrosConversao); override;
    // Adiciona lógica específica do cliente
  end;

```

### 11.3 Herança vs Composição

**Herança (É UM):**

```
type
  TCarro = class(TVeiculo)  // Carro É UM Veículo
  end;

```

**Composição (TEM UM):**

```
type
  TCarro = class
  private
    FMotor: TMotor;  // Carro TEM UM Motor
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TCarro.Create;
begin
  inherited;
  FMotor := TMotor.Create;
end;

destructor TCarro.Destroy;
begin
  FMotor.Free;
  inherited;
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Composição em TIntegracaoBase
type
  TIntegracaoBase = class(TInterfacedObject, IIntegracaoBase)
  protected
    FDados: TDados;              // TEM UM objeto de dados
    FDadosBlob: TDados;          // TEM UM objeto de dados blob
    FOwner: TForm;               // TEM UM formulário dono
  public
    constructor Create(Owner: TForm; Dados, DadosBlob: TDados);
  end;

```

---

## 12. Polimorfismo

### 12.1 Polimorfismo de Sobrescrita

```
type
  TForma = class
  public
    function CalcularArea: Double; virtual; abstract;
  end;

  TQuadrado = class(TForma)
  private
    FLado: Double;
  public
    constructor Create(Lado: Double);
    function CalcularArea: Double; override;
  end;

  TCirculo = class(TForma)
  private
    FRaio: Double;
  public
    constructor Create(Raio: Double);
    function CalcularArea: Double; override;
  end;

function TQuadrado.CalcularArea: Double;
begin
  Result := FLado * FLado;
end;

function TCirculo.CalcularArea: Double;
begin
  Result := Pi * FRaio * FRaio;
end;

// Uso polimórfico
procedure ProcessarFormas;
begin
  var Formas: TList<TForma> := TList<TForma>.Create;
  try
    Formas.Add(TQuadrado.Create(5));
    Formas.Add(TCirculo.Create(3));

    for var Forma in Formas do
    begin
      // Chama o método correto dependendo do tipo real
      var Area := Forma.CalcularArea;
      ShowMessage('Área: ' + FormatFloat('0.00', Area));
    end;
  finally
    for var Forma in Formas do
      Forma.Free;
    Formas.Free;
  end;
end;

```

### 12.2 Upcasting e Downcasting

```
// Upcasting (implícito - sempre seguro)
var Cachorro: TCachorro := TCachorro.Create('Rex');
var Animal: TAnimal := Cachorro; // OK - Cachorro é um Animal

// Downcasting (explícito - pode dar erro)
var Animal: TAnimal := TCachorro.Create('Rex');
var Cachorro: TCachorro := Animal as TCachorro; // Conversão forçada

// Verificação segura com 'is'
if Animal is TCachorro then
begin
  var Cachorro := TCachorro(Animal);
  Cachorro.Latir;
end;

```

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Polimorfismo com ITabelaConversao
type
  ITabelaConversao = interface
    function GetTabelaDestino: string;
    function GetTabelaAux: string;
    function GetCampoPKDestino: string;
  end;

  TTabelaPessoa = class(TInterfacedObject, ITabelaConversao)
  private
    FTipo: TTipoPessoa;
  public
    constructor Create(Tipo: TTipoPessoa);
    function GetTabelaDestino: string;
    function GetTabelaAux: string;
    function GetCampoPKDestino: string;
  end;

  TTabelaProduto = class(TInterfacedObject, ITabelaConversao)
  public
    function GetTabelaDestino: string;
    function GetTabelaAux: string;
    function GetCampoPKDestino: string;
  end;

// Uso polimórfico
procedure ProcessarTabela(Tabela: ITabelaConversao);
begin
  var SQL := Format('SELECT * FROM %s', [Tabela.GetTabelaDestino]);
  QryExecutar(SQL);
end;

```

---

## 13. Classes Abstratas e Interfaces

### 13.1 Classes Abstratas

```
type
  // Classe abstrata - não pode ser instanciada diretamente
  TProcessador = class abstract
  public
    procedure Inicializar; virtual; abstract;
    procedure Processar; virtual; abstract;
    procedure Finalizar; virtual; abstract;

    // Método concreto (implementado)
    procedure Executar;
  end;

  TProcessadorPedidos = class(TProcessador)
  public
    procedure Inicializar; override;
    procedure Processar; override;
    procedure Finalizar; override;
  end;

procedure TProcessador.Executar;
begin
  Inicializar;
  try
    Processar;
  finally
    Finalizar;
  end;
end;

```

### 13.2 Interfaces

```
type
  // Interface - contrato puro
  IConexao = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function Conectar: Boolean;
    procedure Desconectar;
    function Executar(const SQL: string): Boolean;
  end;

  // Implementação
  TConexaoFirebird = class(TInterfacedObject, IConexao)
  public
    function Conectar: Boolean;
    procedure Desconectar;
    function Executar(const SQL: string): Boolean;
  end;

```

### 13.3 Diferença: Classe Abstrata vs Interface

| Aspecto | Classe Abstrata | Interface |
| --- | --- | --- |
| Implementação | Pode ter métodos implementados | Só assinaturas |
| Herança Múltipla | Não | Sim |
| Campos | Pode ter | Não pode |
| Quando usar | Compartilhar código | Definir contratos |

**Exemplo Real do [Sol.NET](http://sol.net/):**

```
// Interface principal
type
  IIntegracaoBase = interface
    ['{1278E936-BCBB-43E9-9135-A613AED576C1}']
    function GetOwner: TForm;
    procedure IniciarTransacao(Principal: Boolean = True);
    procedure CommitTransacao(Principal: Boolean = True);
    procedure RollbackTransacao(Principal: Boolean = True);
    function BuscarConfigGerais(Config: string): string;
    procedure SalvarConfigGerais(Config, Valor: string);
  end;

// Interface estendida
  IIntegracaoPlataforma = interface(IIntegracaoBase)
    ['{71EF7637-9E9A-4A74-9922-A0A455178C30}']
    procedure SendMessage(mensagem: string);
    procedure Handshake(StatusIntegracao: TStatusIntegracao);
  end;

// Implementação base
  TIntegracaoBase = class(TInterfacedObject, IIntegracaoBase)
  protected
    FDados: TDados;
    FOwner: TForm;
  public
    constructor Create(Owner: TForm; Dados, DadosBlob: TDados);

    // Implementação dos métodos da interface
    function GetOwner: TForm;
    procedure IniciarTransacao(Principal: Boolean = True);
    // etc...
  end;

```

---

## 14. Boas Práticas no [Sol.NET](http://sol.net/)

### 14.1 Nomenclatura

```
// ✅ CORRETO
type
  TProduto = class            // Classes: TPascalCase
  private
    FDescricao: string;       // Campos privados: F + PascalCase
    FPreco: Double;
  public
    property Descricao: string read FDescricao write FDescricao;
    function CalcularTotal(Quantidade: Integer): Double;
  end;

  IRepositorio = interface    // Interfaces: I + PascalCase
    procedure Salvar;
  end;

procedure ProcessarPedido;    // Procedures/Functions: PascalCase
var Total: Double := 0.0;     // Variáveis locais: PascalCase

const
  TAXA_PADRAO = 0.05;         // Constantes: UPPER_CASE

```

### 14.2 Try/Finally (Gerenciamento de Memória)

```
// ✅ SEMPRE usar try/finally com objetos
procedure ProcessarDados;
begin
  var Lista: TList<string> := TList<string>.Create;
  try
    Lista.Add('Item 1');
    Lista.Add('Item 2');
    // Processar...
  finally
    Lista.Free;  // Garante liberação mesmo com exceção
  end;
end;

// ✅ Múltiplos objetos
procedure ProcessarMultiplos;
begin
  var Obj1: TObjecto1 := TObjecto1.Create;
  try
    var Obj2: TObjecto2 := TObjecto2.Create;
    try
      // Usar obj1 e obj2
    finally
      Obj2.Free;
    end;
  finally
    Obj1.Free;
  end;
end;

```

### 14.3 Tratamento de Exceções Específicas

```
// ✅ Usar exceções específicas do Sol.NET
uses HetoExceptions, HetoExceptions.Conversao;

procedure ValidarDados;
begin
  if Usuario = '' then
    raise EExcecaoUsuario.Create('Usuário não informado!');

  if not Assigned(FDados) then
    raise EExcecaoDesenvolvedor.Create('Objeto FDados não inicializado!');

  if not ValidarCPF(CPF) then
    raise EFalhaValidacao.Create('CPF inválido: ' + CPF);
end;

```

### 14.4 SQL Seguro

```
// ❌ ERRADO - SQL Injection
procedure BuscarClienteErrado(Nome: string);
begin
  var SQL := 'SELECT * FROM PESSOA WHERE NOME = ' + Nome;
  QryExecutar(SQL);
end;

// ✅ CORRETO - Com QuotedStr
procedure BuscarClienteCerto(Nome: string);
begin
  var SQL := 'SELECT * FROM PESSOA WHERE NOME = ' + QuotedStr(Nome);
  QryExecutar(SQL);
end;

// ✅ MELHOR - Com parâmetros (quando possível)
procedure BuscarClienteMelhor(Nome: string);
begin
  Query.SQL.Text := 'SELECT * FROM PESSOA WHERE NOME = :NOME';
  Query.ParamByName('NOME').AsString := Nome;
  Query.Open;
end;

```

### 14.5 Logging

```
// ✅ Sempre logar operações importantes
procedure ProcessarIntegracao;
begin
  LogAdd('Iniciando integração...');
  try
    // Processar
    LogAdd('Integração concluída com sucesso');
  except
    on E: Exception do
    begin
      LogAdd('ERRO: ' + E.Message);
      raise;
    end;
  end;
end;

```

### 14.6 Padrão Builder (Fluent Interface)

```
// ✅ Usar interface fluente para configuração
var Parametros := TConversaoBuilder.Create
  .SetTabelaConversao(TTabelaPessoa.Create(Pessoa), 'CLIENTES C')
  .AddPrimaryKey('C.CODIGO')
  .AddCampo('NOME', 'C.NOME_CLIENTE')
  .AddCampo('EMAIL', 'C.EMAIL')
  .AddWhere('C.ATIVO = ''S''')
  .Build;

```

### 14.7 Validação de Parâmetros

```
// ✅ Validar parâmetros no início dos métodos
function CalcularDesconto(ValorOriginal: Double; Percentual: Double): Double;
begin
  if ValorOriginal < 0 then
    raise EArgumentException.Create('Valor original não pode ser negativo');

  if (Percentual < 0) or (Percentual > 1) then
    raise EArgumentException.Create('Percentual deve estar entre 0 e 1');

  Result := ValorOriginal * (1 - Percentual);
end;

```

### 14.8 Comentários Úteis

```
// ✅ Comentar PORQUÊ, não O QUE
procedure ProcessarPedido;
begin
  // Necessário ordenar antes de localizar (LocalizarBinário exige)
  cdsProdutos.IndexFieldNames := 'ID_PRODUTO';

  if LocalizarBinario(cdsProdutos, 'ID_PRODUTO', IdProduto) then
  begin
    // Atualizar estoque aqui previne condição de corrida
    AtualizarEstoque;
  end;
end;

```

---

## 🎓 Exercícios Práticos

### Exercício 1: Variáveis e Operadores

```
// Crie um procedimento que:
// 1. Declare variáveis inline para: Produto, Quantidade, Preço
// 2. Calcule o total
// 3. Aplique 10% de desconto se total > 100
// 4. Exiba o resultado

procedure CalcularPedido;
begin
  // SEU CÓDIGO AQUI
end;

```

### Exercício 2: Loops

```
// Percorra um TClientDataSet e:
// 1. Some todos os valores da coluna PRECO
// 2. Conte quantos produtos têm preço > 50
// 3. Exiba o resultado

procedure AnalisarProdutos(cdsProdutos: TClientDataSet);
begin
  // SEU CÓDIGO AQUI
end;

```

### Exercício 3: Classe Simples

```
// Crie uma classe TCliente com:
// 1. Campos privados: Id, Nome, Email
// 2. Propriedades públicas
// 3. Método ValidarEmail que retorna Boolean
// 4. Constructor e Destructor

type
  TCliente = class
    // SEU CÓDIGO AQUI
  end;

```

### Exercício 4: Herança

```
// Crie hierarquia de classes:
// 1. TUsuario (base) com Nome, Login
// 2. TUsuarioAdmin (herda de TUsuario) com método CriarOutroUsuario
// 3. TUsuarioComum (herda de TUsuario) com método SolicitarPermissao

// SEU CÓDIGO AQUI

```

---

## 📖 Recursos Adicionais

### Documentação Oficial

- [Embarcadero DocWiki](https://docwiki.embarcadero.com/)
- [Delphi Basics](http://www.delphibasics.co.uk/)

### Ferramentas Úteis

- **IDE Delphi 12.2**: Ambiente de desenvolvimento oficial
- **Git**: Controle de versão
- **SQL Server Management Studio**: Gerenciamento de banco de dados

### Próximos Passos

1. Estudar o código existente em `Framework/`
2. Praticar com conversões simples
3. Implementar uma conversão completa supervisionada
4. Contribuir com melhorias no código

---

## ✅ Checklist de Conhecimento

Marque conforme for dominando cada tópico:

### Fundamentos

- [ ]  Declaração de variáveis inline
- [ ]  Uso de constantes
- [ ]  Operadores lógicos e matemáticos
- [ ]  IF/ELSE e CASE
- [ ]  FOR, WHILE, REPEAT loops
- [ ]  BREAK e CONTINUE

### Funções e Strings

- [ ]  Procedures vs Functions
- [ ]  Parâmetros por valor e referência
- [ ]  Sobrecarga de métodos
- [ ]  Manipulação de strings
- [ ]  QuotedStr para SQL

### Estruturas de Dados

- [ ]  TArray<T>
- [ ]  TList<T>
- [ ]  TClientDataSet
- [ ]  LocalizarBinário

### POO - Básico

- [ ]  Criação de classes
- [ ]  Construtores e destrutores
- [ ]  Properties (get/set)
- [ ]  Encapsulamento (private/public)
- [ ]  Try/Finally para memória

### POO - Avançado

- [ ]  Herança (extends)
- [ ]  Virtual/Override
- [ ]  Polimorfismo
- [ ]  Classes abstratas
- [ ]  Interfaces
- [ ]  Composição vs Herança

### [Sol.NET](http://sol.net/) Específico

- [ ]  Estrutura do projeto
- [ ]  Framework de Integrações
- [ ]  Framework de Conversão
- [ ]  Padrão Builder
- [ ]  Exceções customizadas
- [ ]  Logging e auditoria

---

## 🎯 Conclusão

Este guia cobre os fundamentos essenciais para trabalhar com Delphi no contexto do [Sol.NET](http://sol.net/). Lembre-se:

1. **Pratique constantemente** - A melhor forma de aprender é codificando
2. **Leia código existente** - O [Sol.NET](http://sol.net/) tem muitos exemplos bem implementados
3. **Faça perguntas** - Sempre que tiver dúvidas, consulte a equipe
4. **Siga os padrões** - Clean Code e convenções do projeto
5. **Teste seu código** - Sempre valide antes de commitar

**Boa sorte nos estudos! 🚀**

---

*Última atualização: 23/10/2025*

*Versão: 1.0*

*Responsável: Equipe de Desenvolvimento [Sol.NET](http://sol.net/)*
