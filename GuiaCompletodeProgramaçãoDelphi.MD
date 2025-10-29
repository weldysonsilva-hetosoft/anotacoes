# 📚 Guia Completo de Programação Delphi para o [Sol.NE](http://sol.net/)T

## 🎯 Sobre Este Guia

Este documento foi criado para desenvolvedores iniciantes e júniores que estão começando a trabalhar com o sistema [Sol.NET](http://sol.net/). Aqui você encontrará explicações detalhadas sobre os conceitos fundamentais de programação em Delphi, com exemplos práticos extraídos do próprio código do [Sol.NET](http://sol.net/).

---

## 📑 Índice

Claro! Aqui está o índice com todos os links atualizados para apontarem para o repositório do GitHub:

---

## 📑 Índice

1. [Variáveis e Constantes](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
2. [Operadores](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
3. [Estruturas de Decisão](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
4. [Estruturas de Repetição (Loops)](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
5. [Funções e Procedimentos](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
6. [Trabalhando com Strings](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
7. [Arrays , Listas e Matriz](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
8. [Classes e Objetos](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
9. [Construtores e Destrutores](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
10. [Encapsulamento](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
11. [Herança](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
12. [Polimorfismo](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
13. [Classes Abstratas e Interfaces](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
14.[Cliente Data Set](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)  
15. [Boas Práticas no Sol.NET](https://github.com/weldysonsilva-hetosoft/anotacoes/edit/main/GuiaCompletodePrograma%C3%A7%C3%A3oDelphi.MD)

---

Se quiser, posso também formatar esse índice em Markdown para facilitar a edição no GitHub. Deseja isso?


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

Definição
Um vetor (ou array unidimensional) é uma estrutura de dados que armazena uma coleção sequencial de elementos do mesmo tipo, acessíveis por um índice numérico.

Sintaxe Delphi
// Declaração estática
var NomesClientes: array[0..9] of string;

// Declaração dinâmica (tamanho variável)
var CodigosProdutos: TArray<Integer>;

// Declaração inline (Delphi 12.2)
var ListaPrecos: TArray<Double> := [10.50, 25.00, 35.75];
Exemplos Práticos do Sol.NET
1. Array de Funções de Transformação (Conversão)
// Arquivo: uConversao.TiposAuxiliares.pas
type
  TFuncoes = (SoNumeros, RemoverAcentos, Maiuscula, Minuscula, 
              DataParaString, StringParaData, ValorParaString);

// Uso no Builder de Conversão
.AddCampo('CPF', 'CPF_CLIENTE', nil, '', '', [TFuncoes.SoNumeros])
.AddCampo('NOME', 'NOME_CLIENTE', nil, '', '', [TFuncoes.RemoverAcentos, TFuncoes.Maiuscula])
Explicação: O array [TFuncoes.SoNumeros] armazena uma sequência de transformações que serão aplicadas ao campo durante a conversão.

2. Array de Campos para Concatenação SQL
// Uso no Framework de Conversão
SQLOrigem.CONCAT(['EMPRESA', 'CODIGO', 'FILIAL'], '#')

// Resultado gerado automaticamente:
// SQL Server: EMPRESA + '#' + CODIGO + '#' + FILIAL
// Firebird: EMPRESA || '#' || CODIGO || '#' || FILIAL
Explicação: O vetor ['EMPRESA', 'CODIGO', 'FILIAL'] armazena os nomes dos campos que serão concatenados.

3. Array de Configurações de Empresas
// Declaração
var EmpresasAtivas: TArray<Integer>;

// Preenchimento dinâmico
procedure CarregarEmpresasAtivas;
begin
  SetLength(EmpresasAtivas, 0); // Limpa o array
  
  var qry := TDados.Create.CriarQuery;
  try
    qry.SQL.Text := 'SELECT ID_EMPRESA FROM EMPRESAS WHERE INATIVO = 0';
    qry.Open;
    
    while not qry.Eof do
    begin
      SetLength(EmpresasAtivas, Length(EmpresasAtivas) + 1);
      EmpresasAtivas[High(EmpresasAtivas)] := qry.FieldByName('ID_EMPRESA').AsInteger;
      qry.Next;
    end;
  finally
    qry.Free;
  end;
end;
4. Array de Callbacks (Framework de Conversão)
// Arquivo: ConversaoBuilder.pas
type
  TArrayCallbacks = TArray<TCallbackConversao>;

// Uso
ParametrosConversao.ListaCallbacks.Add(ValidarCPF);
ParametrosConversao.ListaCallbacks.Add(ValidarEmail);
ParametrosConversao.ListaCallbacks.Add(NormalizarEndereco);

// Execução sequencial
for var Callback in ParametrosConversao.ListaCallbacks do
  Callback.Executar(Self);
# Vetores e Matrizes no Contexto do Sol.NET

## 📚 Visão Geral

No desenvolvimento Delphi do Sol.NET, vetores e matrizes são estruturas fundamentais para manipulação eficiente de dados. Este documento explica esses conceitos usando exemplos práticos do próprio sistema.

---

## 📊 Vetores (Arrays)

### Definição
Um **vetor** (ou array unidimensional) é uma estrutura de dados que armazena uma coleção sequencial de elementos do mesmo tipo, acessíveis por um índice numérico.

### Sintaxe Delphi
```delphi
// Declaração estática
var NomesClientes: array[0..9] of string;

// Declaração dinâmica (tamanho variável)
var CodigosProdutos: TArray<Integer>;

// Declaração inline (Delphi 12.2)
var ListaPrecos: TArray<Double> := [10.50, 25.00, 35.75];
```

### Exemplos Práticos do Sol.NET

#### 1. Array de Funções de Transformação (Conversão)
```delphi
// Arquivo: uConversao.TiposAuxiliares.pas
type
  TFuncoes = (SoNumeros, RemoverAcentos, Maiuscula, Minuscula, 
              DataParaString, StringParaData, ValorParaString);

// Uso no Builder de Conversão
.AddCampo('CPF', 'CPF_CLIENTE', nil, '', '', [TFuncoes.SoNumeros])
.AddCampo('NOME', 'NOME_CLIENTE', nil, '', '', [TFuncoes.RemoverAcentos, TFuncoes.Maiuscula])
```
**Explicação:** O array `[TFuncoes.SoNumeros]` armazena uma sequência de transformações que serão aplicadas ao campo durante a conversão.

#### 2. Array de Campos para Concatenação SQL
```delphi
// Uso no Framework de Conversão
SQLOrigem.CONCAT(['EMPRESA', 'CODIGO', 'FILIAL'], '#')

// Resultado gerado automaticamente:
// SQL Server: EMPRESA + '#' + CODIGO + '#' + FILIAL
// Firebird: EMPRESA || '#' || CODIGO || '#' || FILIAL
```
**Explicação:** O vetor `['EMPRESA', 'CODIGO', 'FILIAL']` armazena os nomes dos campos que serão concatenados.

#### 3. Array de Configurações de Empresas
```delphi
// Declaração
var EmpresasAtivas: TArray<Integer>;

// Preenchimento dinâmico
procedure CarregarEmpresasAtivas;
begin
  SetLength(EmpresasAtivas, 0); // Limpa o array
  
  var qry := TDados.Create.CriarQuery;
  try
    qry.SQL.Text := 'SELECT ID_EMPRESA FROM EMPRESAS WHERE INATIVO = 0';
    qry.Open;
    
    while not qry.Eof do
    begin
      SetLength(EmpresasAtivas, Length(EmpresasAtivas) + 1);
      EmpresasAtivas[High(EmpresasAtivas)] := qry.FieldByName('ID_EMPRESA').AsInteger;
      qry.Next;
    end;
  finally
    qry.Free;
  end;
end;
```

#### 4. Array de Callbacks (Framework de Conversão)
```delphi
// Arquivo: ConversaoBuilder.pas
type
  TArrayCallbacks = TArray<TCallbackConversao>;

// Uso
ParametrosConversao.ListaCallbacks.Add(ValidarCPF);
ParametrosConversao.ListaCallbacks.Add(ValidarEmail);
ParametrosConversao.ListaCallbacks.Add(NormalizarEndereco);

// Execução sequencial
for var Callback in ParametrosConversao.ListaCallbacks do
  Callback.Executar(Self);
```

---

## 🔲 Matrizes (Arrays Bidimensionais)

### Definição
Uma **matriz** é uma estrutura de dados bidimensional (linhas × colunas) que armazena elementos do mesmo tipo, acessíveis por dois índices.

### Sintaxe Delphi
```delphi
// Declaração estática
var TabelaPrecos: array[0..9, 0..4] of Double; // 10 linhas × 5 colunas

// Declaração dinâmica
var MatrizDados: TArray<TArray<Variant>>;

// Acesso
TabelaPrecos[2, 3] := 150.00; // Linha 2, Coluna 3
```

### Exemplos Práticos do Sol.NET

#### 1. Matriz de Mapeamento de Campos (Conversão)
```delphi
// Conceito: Matriz que mapeia campos origem → destino com transformações

type
  TCampoMapeado = record
    Origem: string;
    Destino: string;
    Funcoes: TArray<TFuncoes>;
  end;

var MapeamentoCampos: TArray<TCampoMapeado>;

// Configuração
SetLength(MapeamentoCampos, 3);
MapeamentoCampos[0] := TCampoMapeado.Create('CPF_CLIENTE', 'CPF', [TFuncoes.SoNumeros]);
MapeamentoCampos[1] := TCampoMapeado.Create('NOME_CLIENTE', 'NOME', [TFuncoes.Maiuscula]);
MapeamentoCampos[2] := TCampoMapeado.Create('EMAIL_CLIENTE', 'EMAIL', [TFuncoes.Minuscula]);

// Uso
for var i := 0 to High(MapeamentoCampos) do
  Builder.AddCampo(MapeamentoCampos[i].Destino, 
                   MapeamentoCampos[i].Origem, 
                   nil, '', '', 
                   MapeamentoCampos[i].Funcoes);
```

#### 2. Matriz de Preços por Tabela e Produto
```delpril
// Sistema de precificação com múltiplas tabelas
type
  TMatrizPrecos = array of array of Double; // [IdProduto, IdTabelaPreco]

var MatrizPrecos: TMatrizPrecos;

procedure CarregarMatrizPrecos(Produtos, Tabelas: TArray<Integer>);
begin
  SetLength(MatrizPrecos, Length(Produtos), Length(Tabelas));
  
  for var i := 0 to High(Produtos) do
    for var j := 0 to High(Tabelas) do
    begin
      var qry := TDados.Create.CriarQuery;
      try
        qry.SQL.Text := 'SELECT PRECO FROM PRODUTO_PRECO ' +
                        'WHERE ID_PRODUTO = :PROD AND ID_TABELA = :TAB';
        qry.ParamByName('PROD').AsInteger := Produtos[i];
        qry.ParamByName('TAB').AsInteger := Tabelas[j];
        qry.Open;
        
        MatrizPrecos[i, j] := qry.FieldByName('PRECO').AsFloat;
      finally
        qry.Free;
      end;
    end;
end;

// Consulta de preço
function ObterPreco(IdxProduto, IdxTabela: Integer): Double;
begin
  Result := MatrizPrecos[IdxProduto, IdxTabela];
end;
```

#### 3. Matriz de Tributação Estadual (Fiscal)
```delphi
// Framework de Integrações Fiscais
type
  TTributacaoEstado = record
    UF: string;
    ICMS: Double;
    MVA: Double;
    Reducao: Double;
  end;

  TMatrizTributacao = array of array of TTributacaoEstado; 
  // [IdProduto, IdEstado]

var TributacaoNacional: TMatrizTributacao;

procedure CarregarTributacao(NCM: string);
begin
  // Preenche matriz com alíquotas por produto e UF
  var Produtos := BuscarProdutosPorNCM(NCM);
  var Estados := ['AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 
                  'MA', 'MG', 'MS', 'MT', 'PA', 'PB', 'PE', 'PI', 'PR', 
                  'RJ', 'RN', 'RO', 'RR', 'RS', 'SC', 'SE', 'SP', 'TO'];
  
  SetLength(TributacaoNacional, Length(Produtos), Length(Estados));
  
  for var i := 0 to High(Produtos) do
    for var j := 0 to Low(Estados) to High(Estados) do
      TributacaoNacional[i, j] := ConsultarProvedor(Produtos[i], Estados[j]);
end;
```

#### 4. Matriz de Status de Sincronização (MonitorIntegração)
```delphi
// Controle de múltiplas integrações × empresas
type
  TStatusSync = (ssAguardando, ssProcessando, ssSucesso, ssErro);
  
  TMatrizIntegracao = array of array of TStatusSync; 
  // [IdEmpresa, TipoIntegracao]

var StatusIntegracoes: TMatrizIntegracao;

procedure InicializarMonitoramento;
begin
  var Empresas := [1, 2, 5, 7, 10];
  var Integracoes := [tiIFood, tiWBuy, tiScanntech, tiHetoBank];
  
  SetLength(StatusIntegracoes, Length(Empresas), Length(Integracoes));
  
  // Inicializa todos como aguardando
  for var i := 0 to High(Empresas) do
    for var j := 0 to High(Integracoes) do
      StatusIntegracoes[i, j] := ssAguardando;
end;

procedure AtualizarStatus(IdxEmpresa, IdxIntegracao: Integer; 
                          NovoStatus: TStatusSync);
begin
  StatusIntegracoes[IdxEmpresa, IdxIntegracao] := NovoStatus;
  LogAdd(Format('Empresa %d - Integração %d: %s', 
                [IdxEmpresa, IdxIntegracao, GetEnumName(TypeInfo(TStatusSync), Ord(NovoStatus))]));
end;
```

---

## 🔍 Comparação: Vetor vs Matriz no Sol.NET

| Aspecto | Vetor (Array 1D) | Matriz (Array 2D) |
|---------|------------------|-------------------|
| **Dimensões** | 1 (linear) | 2 (linhas × colunas) |
| **Índices** | `Array[i]` | `Matriz[i, j]` |
| **Uso Típico** | Listas simples, configurações | Tabelas de relacionamento, cálculos complexos |
| **Exemplo Sol.NET** | Lista de callbacks | Preços produto×tabela |
| **Performance** | Acesso O(1) | Acesso O(1) |
| **Memória** | N elementos | N × M elementos |

---

## 📋 Estruturas Especializadas no Sol.NET

### TArrayCamposConversao (Vetor Tipado)
```delphi
// Arquivo: uConversao.TiposAuxiliares.pas
type
  TArrayCamposConversao = class
  private
    FLista: TObjectList<TCamposConversao>;
  public
    function AddCampo(CampoDestino, CampoOrigem: string; 
                      TabelaBuscar: ITabelaConversao = nil;
                      BuscarDinamico: string = '';
                      Apelido: string = '';
                      ExecutarFuncoes: TArray<TFuncoes> = nil): TCamposConversao;
    
    property Lista: TObjectList<TCamposConversao> read FLista;
  end;
```
**Explicação:** Encapsula um vetor dinâmico de objetos `TCamposConversao` com métodos especializados para conversão de dados.

### TClientDataSet (Matriz em Memória)
```delphi
// Uso comum no Sol.NET para datasets em memória
var cdsClientes: TClientDataSet;

procedure CarregarClientes;
begin
  cdsClientes := TClientDataSet.Create(nil);
  cdsClientes.FieldDefs.Add('ID', ftInteger);
  cdsClientes.FieldDefs.Add('NOME', ftString, 100);
  cdsClientes.FieldDefs.Add('CPF', ftString, 14);
  cdsClientes.CreateDataSet;
  
  // Preencher dados (conceitualmente uma matriz)
  cdsClientes.Append;
  cdsClientes.FieldByName('ID').AsInteger := 1;
  cdsClientes.FieldByName('NOME').AsString := 'João Silva';
  cdsClientes.FieldByName('CPF').AsString := '12345678900';
  cdsClientes.Post;
end;

// Localização binária (otimização para vetores ordenados)
if LocalizarBinário(cdsClientes, 'ID', 1) then
  ShowMessage('Cliente encontrado!');
```

---

## ✅ Boas Práticas no Sol.NET

### 1. Sempre use SetLength antes de popular arrays dinâmicos
```delphi
// ✅ Correto
var Lista: TArray<string>;
SetLength(Lista, 10);
Lista[0] := 'Primeiro item';

// ❌ Errado - causará exceção
var Lista: TArray<string>;
Lista[0] := 'Primeiro item'; // Index out of bounds
```

### 2. Prefira LocalizarBinário para pesquisas em datasets ordenados
```delphi
// ✅ Otimizado O(log n)
cdsClientes.IndexFieldNames := 'ID';
if LocalizarBinário(cdsClientes, 'ID', 123) then
  ProcessarCliente;

// ❌ Menos eficiente O(n)
cdsClientes.First;
while not cdsClientes.Eof do
begin
  if cdsClientes.FieldByName('ID').AsInteger = 123 then
  begin
    ProcessarCliente;
    Break;
  end;
  cdsClientes.Next;
end;
```

### 3. Use declaração inline quando possível (Delphi 12.2)
```delphi
// ✅ Moderno e conciso
var StatusPermitidos: TArray<Integer> := [0, 1, 5, 10];

// ✅ Também válido, mas mais verboso
var StatusPermitidos: TArray<Integer>;
SetLength(StatusPermitidos, 4);
StatusPermitidos[0] := 0;
StatusPermitidos[1] := 1;
StatusPermitidos[2] := 5;
StatusPermitidos[3] := 10;
```

### 4. Libere memória de arrays de objetos
```delphi
// ✅ Correto - evita memory leak
var ListaObjetos: TObjectList<TCliente>;
ListaObjetos := TObjectList<TCliente>.Create(True); // OwnsObjects = True
try
  // Usar lista
finally
  ListaObjetos.Free; // Libera objetos automaticamente
end;

// ❌ Vazamento de memória
var ListaObjetos: TList<TCliente>;
ListaObjetos := TList<TCliente>.Create;
// Objetos não são liberados automaticamente
ListaObjetos.Free; // Vazamento!
```

---

## 🎯 Casos de Uso Avançados

### Grid Dinâmico de Produtos (Matriz Virtual)
```delphi
// Simula matriz usando TClientDataSet para interface de grades de produtos
procedure MontarGradePrecos;
var
  cdsProdutos: TClientDataSet;
  TabelasPreco: TArray<Integer>;
  Produtos: TArray<Integer>;
begin
  cdsProdutos := TClientDataSet.Create(nil);
  try
    // Define estrutura (1 campo fixo + N colunas dinâmicas)
    cdsProdutos.FieldDefs.Add('PRODUTO', ftString, 100);
    
    TabelasPreco := [1, 2, 3, 4, 5]; // IDs das tabelas
    
    for var IdTabela in TabelasPreco do
      cdsProdutos.FieldDefs.Add('PRECO_' + IdTabela.ToString, ftFloat);
    
    cdsProdutos.CreateDataSet;
    
    // Preenche dados (matriz conceitual)
    Produtos := [10, 20, 30];
    
    for var IdProduto in Produtos do
    begin
      cdsProdutos.Append;
      cdsProdutos.FieldByName('PRODUTO').AsString := 
        BuscarNomeProduto(IdProduto);
      
      for var IdTabela in TabelasPreco do
        cdsProdutos.FieldByName('PRECO_' + IdTabela.ToString).AsFloat :=
          ConsultarPreco(IdProduto, IdTabela);
      
      cdsProdutos.Post;
    end;
    
    // Vincular ao grid visual
    DataSource1.DataSet := cdsProdutos;
  finally
    // Não liberar aqui - DataSource controla lifecycle
  end;
end;
```

---

## 📖 Glossário de Termos

| Termo | Definição |
|-------|-----------|
| **Vetor (Array)** | Estrutura unidimensional indexada |
| **Matriz** | Estrutura bidimensional (linhas × colunas) |
| **Array Dinâmico** | Tamanho definido em tempo de execução |
| **Array Estático** | Tamanho fixo definido em compilação |
| **Índice** | Posição numérica de acesso ao elemento |
| **High()** | Função que retorna o maior índice válido |
| **Low()** | Função que retorna o menor índice válido |
| **SetLength()** | Redefine tamanho de array dinâmico |

---
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

## 15. cliente data set [Sol.NET](http://sol.net/)
# Documentação Técnica: TClientDataSet no Sol.NET

## 📋 Visão Geral

O **TClientDataSet** é um componente fundamental no Sol.NET para manipulação de dados em memória. Ele armazena o resultado de consultas ao banco de dados e fornece funcionalidades avançadas de navegação, filtragem, ordenação e localização de registros.

## 🎯 Tipos de ClientDataSet no Sol.NET

O sistema Sol.NET utiliza principalmente dois tipos de TClientDataSet:

### 1. **cdsBuscar**
- **Propósito:** Armazenar resultados de consultas gerais
- **Uso comum:** Listagens, relatórios, busca de dados
- **Escopo:** Variável local ou global dependendo do contexto

### 2. **cdsGeral** (Nomenclatura Variável)
- **Propósito:** Manipulação de entidades específicas do sistema
- **Exemplos:** `cdsProdutos`, `cdsPessoas`, `cdsMovimentos`
- **Uso comum:** CRUD de entidades, validações, processamentos

---

## 🔍 O que é um TClientDataSet

### Definição
É um objeto que armazena o resultado de uma consulta no banco de dados em memória, funcionando como um conjunto de dados desconectado.

### Características Principais

✅ **Armazena dados:** Mantém registros em memória após a consulta  
✅ **Metadados:** Preserva nome das colunas, tipos de dados, índices  
✅ **Navegação:** Permite percorrer registros com cursores  
✅ **Filtragem:** Suporta filtros locais sem nova consulta ao banco  
✅ **Ordenação:** Permite ordenar dados em memória  
✅ **Localização:** Busca binária e sequencial de registros  

---

## 📊 Estrutura e Metadados

### Informações Armazenadas

```delphi
// Quantidade de registros
var TotalRegistros: Integer := MeuCds.RecordCount;

// Nomes dos campos
var NomeCampo: string := MeuCds.Fields[0].FieldName;

// Tipo de dado de um campo
var TipoCampo: TFieldType := MeuCds.FieldByName('NOME').DataType;

// Verifica se há registros
var TemDados: Boolean := not MeuCds.IsEmpty;
```

---

## 🔧 Populando um TClientDataSet

### Método Padrão no Sol.NET

```delphi
var strList: TStringList := TStringList.Create;
try
  strList.Add('SELECT         ');
  strList.Add('    FIRST 5 *  ');
  strList.Add('FROM           ');
  strList.Add('    CARGOS     ');
  strList.Add('ORDER BY       ');
  strList.Add('    DESCRICAO  ');
  
  cdsBuscar.Data := Dados.QryOpenOle(strList.Text);
finally
  strList.Free;
end;
```

### Exemplo com Parâmetros

```delphi
var strSQL: TStringList := TStringList.Create;
try
  strSQL.Add('SELECT             ');
  strSQL.Add('    ID_PRODUTO,    ');
  strSQL.Add('    DESCRICAO,     ');
  strSQL.Add('    PRECO_VENDA    ');
  strSQL.Add('FROM               ');
  strSQL.Add('    PRODUTOS       ');
  strSQL.Add('WHERE              ');
  strSQL.Add('    INATIVO = 0    ');
  strSQL.Add('    AND PRECO_VENDA > ' + FloatToStr(PrecoMinimo));
  
  cdsProdutos.Data := Dados.QryOpenOle(strSQL.Text);
finally
  strSQL.Free;
end;
```

---

## 🎭 Acessando Valores dos Campos

### Método Básico

```delphi
// Retorna Variant (pode ser null)
var Valor: Variant := MeuCds.FieldByName('NOME_COLUNA').Value;
```

### Conversões Tipadas (Seguras)

```delphi
// String
var Nome: string := MeuCds.FieldByName('NOME').AsString;

// Inteiro
var Codigo: Integer := MeuCds.FieldByName('CODIGO').AsInteger;

// Double/Currency
var Preco: Double := MeuCds.FieldByName('PRECO').AsFloat;

// Data/Hora
var DataCadastro: TDateTime := MeuCds.FieldByName('DATA_CADASTRO').AsDateTime;

// Boolean (campos 0/1)
var Ativo: Boolean := MeuCds.FieldByName('INATIVO').AsInteger = 0;
```

### Tratamento de Valores Nulos

```delphi
// Verificar se campo é nulo
if MeuCds.FieldByName('EMAIL').IsNull then
  ShowMessage('Email não informado');

// Valor padrão se nulo
var Email: string := IfThen(
  MeuCds.FieldByName('EMAIL').IsNull, 
  'sem-email@exemplo.com', 
  MeuCds.FieldByName('EMAIL').AsString
);
```

---

## 🔄 Navegação entre Registros

### Métodos de Movimentação

```delphi
// Move para o primeiro registro
MeuCds.First;

// Move para o último registro
MeuCds.Last;

// Próximo registro
MeuCds.Next;

// Registro anterior
MeuCds.Prior;

// Move para um registro específico (baseado em 0)
MeuCds.RecNo := 5;
```

### Verificações de Posição

```delphi
// Está no primeiro registro?
var NoPrimeiro: Boolean := MeuCds.Bof;

// Está no último registro?
var NoUltimo: Boolean := MeuCds.Eof;

// Está vazio?
var Vazio: Boolean := MeuCds.IsEmpty;

// Posição atual do cursor (baseado em 1)
var PosicaoAtual: Integer := MeuCds.RecNo;
```

### Loop Completo nos Registros

```delphi
// Padrão Clean Code
MeuCds.First;
while not MeuCds.Eof do
begin
  // Processar registro atual
  var Descricao: string := MeuCds.FieldByName('DESCRICAO').AsString;
  LogAdd(Format('Processando: %s', [Descricao]));
  
  // Avança para o próximo
  MeuCds.Next;
end;
```

### Loop com Controle de Índice

```delphi
var I: Integer;
MeuCds.First;
for I := 0 to MeuCds.RecordCount - 1 do
begin
  // Processar registro
  ProcessarRegistro(MeuCds);
  
  // Não precisa chamar Next no último registro
  if I < MeuCds.RecordCount - 1 then
    MeuCds.Next;
end;
```

---

## 🔎 Localização de Registros

### Método Preferencial: Busca Binária

**⚠️ Importante:** Para usar busca binária, o ClientDataSet **DEVE** estar ordenado pelo campo de busca.

```delphi
// Ordenar antes de localizar
MeuCds.IndexFieldNames := 'CODIGO';

// Localizar usando busca binária
if LocalizarBinario(MeuCds, 'CODIGO', CodigoBuscado) then
  ShowMessage('Registro encontrado!')
else
  ShowMessage('Registro não encontrado.');
```

### Busca Sequencial (Locate)

```delphi
// Localizar por um campo
if MeuCds.Locate('EMAIL', 'usuario@exemplo.com', [loCaseInsensitive]) then
  ShowMessage('Email encontrado!');

// Localizar por múltiplos campos
if MeuCds.Locate('CODIGO;TIPO', VarArrayOf([123, 'A']), []) then
  ShowMessage('Registro encontrado!');
```

### Opções de Localização

```delphi
// Busca case insensitive
[loCaseInsensitive]

// Busca parcial (contém)
[loPartialKey]

// Combinação
[loCaseInsensitive, loPartialKey]
```

---

## 🎨 Filtragem de Dados

### Filtro Simples

```delphi
// Aplicar filtro
MeuCds.Filter := 'INATIVO = 0';
MeuCds.Filtered := True;

// Remover filtro
MeuCds.Filtered := False;
```

### Filtros Complexos

```delphi
// Com múltiplas condições
MeuCds.Filter := 'INATIVO = 0 AND PRECO_VENDA > 10.00';
MeuCds.Filtered := True;

// Com LIKE (SQL Server)
MeuCds.Filter := 'DESCRICAO LIKE ''%PRODUTO%''';
MeuCds.Filtered := True;

// Com IN
MeuCds.Filter := 'TIPO IN (''A'', ''B'', ''C'')';
MeuCds.Filtered := True;
```

### Filtro por Evento

```delphi
procedure TFrmMeuForm.ConfigurarFiltro;
begin
  MeuCds.OnFilterRecord := FiltrarPorPreco;
  MeuCds.Filtered := True;
end;

procedure TFrmMeuForm.FiltrarPorPreco(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := DataSet.FieldByName('PRECO_VENDA').AsFloat >= 100.00;
end;
```

---

## 📐 Ordenação de Dados

### Ordenação Simples

```delphi
// Por um campo
MeuCds.IndexFieldNames := 'DESCRICAO';

// Ordem decrescente (SQL Server)
MeuCds.IndexFieldNames := 'PRECO_VENDA DESC';
```

### Ordenação por Múltiplos Campos

```delphi
MeuCds.IndexFieldNames := 'CATEGORIA;DESCRICAO';
```

### Ordenação Customizada

```delphi
// Criar índice
MeuCds.AddIndex('IdxPrecoDesc', 'PRECO_VENDA', [ixDescending], '', '', 0);

// Usar índice
MeuCds.IndexName := 'IdxPrecoDesc';
```

---

## 🛠️ Manipulação de Dados (CRUD)

### Inserção

```delphi
MeuCds.Append;
try
  MeuCds.FieldByName('DESCRICAO').AsString := 'Novo Produto';
  MeuCds.FieldByName('PRECO_VENDA').AsFloat := 50.00;
  MeuCds.FieldByName('INATIVO').AsInteger := 0;
  MeuCds.Post;
except
  MeuCds.Cancel;
  raise;
end;
```

### Edição

```delphi
if MeuCds.Locate('ID_PRODUTO', 123, []) then
begin
  MeuCds.Edit;
  try
    MeuCds.FieldByName('PRECO_VENDA').AsFloat := 75.00;
    MeuCds.Post;
  except
    MeuCds.Cancel;
    raise;
  end;
end;
```

### Exclusão

```delphi
if MeuCds.Locate('ID_PRODUTO', 123, []) then
  MeuCds.Delete;
```

### Verificar Estado do Registro

```delphi
case MeuCds.State of
  dsInactive: // Inativo
  dsBrowse:   // Navegação normal
  dsEdit:     // Em edição
  dsInsert:   // Em inserção
end;
```

---

## 📊 Agregações e Cálculos

### Contadores e Totalizadores

```delphi
// Contar registros
var TotalProdutos: Integer := MeuCds.RecordCount;

// Somar valores
var TotalVendas: Double := 0;
MeuCds.First;
while not MeuCds.Eof do
begin
  TotalVendas := TotalVendas + MeuCds.FieldByName('VALOR_TOTAL').AsFloat;
  MeuCds.Next;
end;

// Calcular média
var MediaPrecos: Double := TotalVendas / MeuCds.RecordCount;
```

### Campos Calculados

```delphi
// Adicionar campo calculado
var CampoCalculado: TField := TFloatField.Create(MeuCds);
CampoCalculado.FieldName := 'TOTAL_COM_DESCONTO';
CampoCalculado.FieldKind := fkCalculated;
CampoCalculado.DataSet := MeuCds;

// Implementar cálculo
procedure TFrmMeuForm.MeuCdsCalcFields(DataSet: TDataSet);
begin
  var Subtotal: Double := DataSet.FieldByName('SUBTOTAL').AsFloat;
  var Desconto: Double := DataSet.FieldByName('DESCONTO').AsFloat;
  DataSet.FieldByName('TOTAL_COM_DESCONTO').AsFloat := Subtotal - Desconto;
end;
```

---

## 🔄 Sincronização com Banco de Dados

### Aplicar Alterações (ApplyUpdates)

```delphi
// Para DataSetProvider
if MeuCds.ChangeCount > 0 then
begin
  MeuCds.ApplyUpdates(0); // 0 = aplicar todas as alterações
end;
```

### Cancelar Alterações Pendentes

```delphi
MeuCds.CancelUpdates;
```

---

## 🎯 Boas Práticas no Sol.NET

### 1. Ordenação Antes de Localização

```delphi
// ✅ Correto
cdsProdutos.IndexFieldNames := 'ID_PRODUTO';
if LocalizarBinario(cdsProdutos, 'ID_PRODUTO', CodigoBuscado) then
  ProcessarProduto(cdsProdutos);

// ❌ Evitar (busca lenta)
if cdsProdutos.Locate('ID_PRODUTO', CodigoBuscado, []) then
  ProcessarProduto(cdsProdutos);
```

### 2. Liberação de Recursos

```delphi
var strSQL: TStringList := TStringList.Create;
try
  strSQL.Add('SELECT * FROM PRODUTOS');
  cdsBuscar.Data := Dados.QryOpenOle(strSQL.Text);
  
  // Processar dados
  
finally
  strSQL.Free;
end;
```

### 3. Validação de Dados

```delphi
// Verificar se há dados antes de processar
if cdsBuscar.IsEmpty then
begin
  ShowMessage('Nenhum registro encontrado.');
  Exit;
end;

// Validar campo não nulo
if cdsBuscar.FieldByName('EMAIL').IsNull then
  raise Exception.Create('Email é obrigatório.');
```

### 4. Performance em Loops

```delphi
// ✅ Desabilitar controles para melhor performance
cdsProdutos.DisableControls;
try
  cdsProdutos.First;
  while not cdsProdutos.Eof do
  begin
    ProcessarProduto(cdsProdutos);
    cdsProdutos.Next;
  end;
finally
  cdsProdutos.EnableControls;
end;
```

### 5. Nomenclatura Consistente

```delphi
// ✅ Padrões do Sol.NET
cdsBuscar       // Consultas gerais
cdsProdutos     // Entidade específica
cdsMovimentos   // Entidade específica

// ❌ Evitar
cds1
ClientDataSet1
DataSet
```

---

## 🚨 Erros Comuns e Soluções

### 1. "Dataset not in edit or insert mode"

```delphi
// ❌ Erro
MeuCds.FieldByName('NOME').AsString := 'Novo Nome';

// ✅ Solução
MeuCds.Edit;
MeuCds.FieldByName('NOME').AsString := 'Novo Nome';
MeuCds.Post;
```

### 2. "Field not found"

```delphi
// ❌ Erro (nome incorreto)
var Valor := MeuCds.FieldByName('NOME_PRODUTO').AsString;

// ✅ Solução (verificar nome exato)
var Valor := MeuCds.FieldByName('DESCRICAO').AsString;
```

### 3. Busca binária sem ordenação

```delphi
// ❌ Erro (resultado imprevisível)
LocalizarBinario(MeuCds, 'CODIGO', 123);

// ✅ Solução
MeuCds.IndexFieldNames := 'CODIGO';
LocalizarBinario(MeuCds, 'CODIGO', 123);
```

---

## 📚 Referência Rápida

### Propriedades Importantes

| Propriedade | Descrição |
|------------|-----------|
| `RecordCount` | Número total de registros |
| `RecNo` | Posição atual do cursor (1-based) |
| `Bof` | Está no início? |
| `Eof` | Está no fim? |
| `IsEmpty` | Está vazio? |
| `State` | Estado atual (dsEdit, dsInsert, etc) |
| `FieldCount` | Número de campos |
| `Filter` | Expressão de filtro |
| `Filtered` | Filtro ativo? |
| `IndexFieldNames` | Campos de ordenação |

### Métodos Principais

| Método | Descrição |
|--------|-----------|
| `First` | Vai para o primeiro registro |
| `Last` | Vai para o último registro |
| `Next` | Próximo registro |
| `Prior` | Registro anterior |
| `Locate` | Busca sequencial |
| `Append` | Adiciona novo registro |
| `Edit` | Entra em modo de edição |
| `Post` | Confirma alterações |
| `Cancel` | Cancela alterações |
| `Delete` | Exclui registro atual |
| `DisableControls` | Desabilita atualização visual |
| `EnableControls` | Habilita atualização visual |

---

## 🔗 Documentações Relacionadas

- [Framework de Integrações](./Framework%20Integracoes.md)
- [Sistema de Conversão](./Conversao/Documentacao%20Basica.md)
- Padrões de Desenvolvimento Clean Code

---

### O que é um TClientDataSet
 
`É o objeto que armazena o resultado de uma consulta no banco de dados.
Também armazena os metadados da consulta (Nome das colunas e seus tipos de dados, quantidade de registros carregados, etc)`
 
`Retorna o valor de uma coluna da linha selecionada através da chamada de cdsMeuCds.`FieldByName('NOME_DA_COLUNA'`).Value'. O Value pode ser convertido para um tipo específico para fins de segurança de valores null, ex: FieldByName('NOME_DA_COLUNA').AsInteger, AsString, AsDateTime, AsString, etc`.
 
### Populando um TClientDataSet
 ```pascal
var strList: TStringList := TStringList.Create;
strList.Add('SELECT         ');
strList.Add('    FIRST 5 *  ');
strList.Add('FROM           ');
strList.Add('    cargos     ');
strList.Add('ORDER BY       ');
strList.Add('    descricao  ');
 
 
MeuCds.Data := Dados.QryOpenOle(strList.Text);
 ````
### Fazendo operações com o TClientDataSet

 ```pascal
while not MeuCds.Eof do
begin
	// Executa o código necessário, baseado na linha atual que o TClientDataSet está apontando
	ShowMessage(MeuCds.FieldByName('ID_CARGO').AsString);
	// Move o cursor para a próxima posição (caso ela exista)
	MeuCds.Next;
end;
````
Copilot said: # 📚 Documentação: `cdsBuscar` e `cdsGeral` no
📚 Documentação: cdsBuscar e cdsGeral no Sol.NET
Vou explicar detalhadamente esses dois componentes fundamentais usados nos CRUDs do Sol.NET.

🔍 1. cdsBuscar - ClientDataSet de Busca
O que é?
O cdsBuscar é um TClientDataSet utilizado especificamente para operações de consulta e localização de registros no banco de dados. Ele é otimizado para:

✅ Buscas rápidas em telas de pesquisa
✅ Autocompletar em campos de lookup
✅ Listagens filtradas
✅ Localização de registros para relacionamentos
Características Principais
delphi
// Declaração típica
var cdsBuscar: TClientDataSet;

// Uso comum
cdsBuscar := TClientDataSet.Create(nil);
try
  cdsBuscar.Data := DadosPrincipal.QryOpenOle(
    'SELECT ID_PESSOA, NOME, CPF FROM PESSOAS WHERE ATIVO = 1'
  );
  
  // Localização binária (mais eficiente)
  if LocalizarBinário(cdsBuscar, 'ID_PESSOA', IdPessoa) then
    ShowMessage('Pessoa encontrada: ' + cdsBuscar.FieldByName('NOME').AsString);
finally
  cdsBuscar.Free;
end;
Quando Usar cdsBuscar?
Cenário	Exemplo
Busca em telas de pesquisa	SELECT * FROM PRODUTOS WHERE DESCRICAO LIKE '%'+Texto+'%'
Autocompletar de campos	Carregar lista de clientes ao digitar nome
Validação de existência	Verificar se CPF já está cadastrado
Montagem de combos/listas	Preencher dropdown de categorias
Lookup de relacionamentos	Buscar ID_PRODUTO a partir do código de barras
Boas Práticas com cdsBuscar
delphi
// ✅ CORRETO - Liberar memória
var cdsBuscar: TClientDataSet := TClientDataSet.Create(nil);
try
  cdsBuscar.Data := DadosPrincipal.QryOpenOle('SELECT ...');
  // Usar o dataset
finally
  cdsBuscar.Free; // Sempre liberar
end;

// ✅ CORRETO - Ordenar antes de LocalizarBinário
cdsBuscar.IndexFieldNames := 'CODIGO';
if LocalizarBinário(cdsBuscar, 'CODIGO', '12345') then
  ShowMessage('Produto encontrado!');

// ❌ EVITAR - Não liberar memória
cdsBuscar := TClientDataSet.Create(nil);
cdsBuscar.Data := DadosPrincipal.QryOpenOle('SELECT ...');
// Faltou o Free → Memory Leak!

// ❌ EVITAR - LocalizarBinário sem ordenação
if LocalizarBinário(cdsBuscar, 'NOME', 'João') then // Erro se não estiver ordenado!
🗂️ 2. cdsGeral - ClientDataSet Geral
O que é?
O cdsGeral é um TClientDataSet de uso genérico e temporário, utilizado para:

✅ Operações auxiliares e cálculos
✅ Armazenamento temporário de dados
✅ Manipulação de dados antes de gravar no banco
✅ Estruturas de dados complexas em memória
Características Principais
delphi
// Uso típico em conversões
var cdsGeral: TClientDataSet := TClientDataSet.Create(nil);
try
  cdsGeral.Data := DadosOrigem.QryOpenOle(
    'SELECT * FROM TABELA_ORIGEM WHERE ID > 1000'
  );
  
  // Processar dados
  cdsGeral.First;
  while not cdsGeral.Eof do
  begin
    // Aplicar transformações
    var Nome := cdsGeral.FieldByName('NOME').AsString;
    var CPF := SoNumeros(cdsGeral.FieldByName('CPF').AsString);
    
    // Gravar no destino
    DadosDestino.QryExecutar(
      Format('INSERT INTO PESSOAS (NOME, CPF) VALUES (%s, %s)',
        [QuotedStr(Nome), QuotedStr(CPF)])
    );
    
    cdsGeral.Next;
  end;
finally
  cdsGeral.Free;
end;
Quando Usar cdsGeral?
Cenário	Exemplo
Processamento em lote	Importar 1000 produtos de arquivo CSV
Cálculos complexos	Totalizar vendas antes de gravar
Dados temporários	Armazenar itens do carrinho antes de finalizar venda
Transformações de dados	Aplicar regras de negócio em memória
Preview antes de salvar	Mostrar dados para usuário confirmar
Exemplo Prático - Conversão com cdsGeral
delphi
procedure ConverterClientes;
var
  cdsGeral: TClientDataSet;
  cdsBuscar: TClientDataSet;
begin
  cdsGeral := TClientDataSet.Create(nil);
  cdsBuscar := TClientDataSet.Create(nil);
  try
    // 1. Carregar dados da origem
    cdsGeral.Data := DadosOrigem.QryOpenOle(
      'SELECT CODIGO, NOME, CPF, ID_CIDADE FROM CLIENTES_LEGADO'
    );
    
    // 2. Carregar lookup de cidades (cdsBuscar)
    cdsBuscar.Data := DadosDestino.QryOpenOle(
      'SELECT ID_CIDADE, ID_CIDADE_NOVA FROM AUX_CIDADES'
    );
    cdsBuscar.IndexFieldNames := 'ID_CIDADE';
    
    // 3. Processar cada registro do cdsGeral
    cdsGeral.First;
    while not cdsGeral.Eof do
    begin
      var CPF := SoNumeros(cdsGeral.FieldByName('CPF').AsString);
      
      // Buscar novo ID da cidade usando cdsBuscar
      var IdCidadeNova: Integer := 0;
      if LocalizarBinário(cdsBuscar, 'ID_CIDADE', 
         cdsGeral.FieldByName('ID_CIDADE').AsInteger) then
      begin
        IdCidadeNova := cdsBuscar.FieldByName('ID_CIDADE_NOVA').AsInteger;
      end;
      
      // Gravar no destino
      DadosDestino.QryExecutar(
        Format('INSERT INTO PESSOAS (NOME, CPF, ID_CIDADE) VALUES (%s, %s, %d)',
          [QuotedStr(cdsGeral.FieldByName('NOME').AsString),
           QuotedStr(CPF),
           IdCidadeNova])
      );
      
      cdsGeral.Next;
    end;
  finally
    cdsGeral.Free;
    cdsBuscar.Free;
  end;
end;
🆚 Diferenças Entre cdsBuscar e cdsGeral
Aspecto	cdsBuscar	cdsGeral
Propósito	Busca e localização	Processamento e manipulação
Dados	Geralmente menor volume	Pode conter grandes volumes
Tempo de vida	Curto (busca pontual)	Pode ser mais longo (loop)
Modificação	Raramente modificado	Frequentemente modificado
Ordenação	Quase sempre ordenado	Nem sempre precisa
Exemplo de uso	Localizar produto por código	Processar 1000 vendas
🔧 Padrões de Uso no Framework de Conversão
No TConversao (classe base)
delphi
type
  TConversao = class
  private
    FcdsOrigem: TClientDataSet;  // Equivalente ao cdsGeral (dados origem)
    FcdsDestino: TClientDataSet; // Dados do destino
  public
    procedure ExecutarConversao;
  end;

procedure TConversao.ExecutarConversao;
begin
  // FcdsOrigem atua como "cdsGeral"
  FcdsOrigem.Data := DadosOrigem.QryOpenOle(MontarSQLOrigem);
  
  FcdsOrigem.First;
  while not FcdsOrigem.Eof do
  begin
    // Processar registro
    ProcessarRegistro(FcdsOrigem);
    FcdsOrigem.Next;
  end;
end;
Exemplo com Ambos no Mesmo Método
delphi
procedure ConverterProdutosComCategorias;
var
  cdsGeral: TClientDataSet;   // Produtos origem
  cdsBuscar: TClientDataSet;  // Categorias de/para
begin
  cdsGeral := TClientDataSet.Create(nil);
  cdsBuscar := TClientDataSet.Create(nil);
  try
    // Carregar produtos
    cdsGeral.Data := DadosOrigem.QryOpenOle(
      'SELECT ID, DESCRICAO, ID_CATEGORIA FROM PRODUTOS_ORIGEM'
    );
    
    // Carregar lookup de categorias
    cdsBuscar.Data := DadosDestino.QryOpenOle(
      'SELECT ID_CATEGORIA_ORIGEM, ID_CATEGORIA_DESTINO FROM AUX_CATEGORIAS'
    );
    cdsBuscar.IndexFieldNames := 'ID_CATEGORIA_ORIGEM';
    
    // Processar
    cdsGeral.First;
    while not cdsGeral.Eof do
    begin
      var IdCategoriaNova := 0;
      
      // Buscar categoria convertida
      if LocalizarBinário(cdsBuscar, 'ID_CATEGORIA_ORIGEM',
         cdsGeral.FieldByName('ID_CATEGORIA').AsInteger) then
      begin
        IdCategoriaNova := cdsBuscar.FieldByName('ID_CATEGORIA_DESTINO').AsInteger;
      end;
      
      // Inserir produto
      DadosDestino.QryExecutar(
        Format('INSERT INTO PRODUTOS (DESCRICAO, ID_CATEGORIA) VALUES (%s, %d)',
          [QuotedStr(cdsGeral.FieldByName('DESCRICAO').AsString),
           IdCategoriaNova])
      );
      
      cdsGeral.Next;
    end;
  finally
    cdsGeral.Free;
    cdsBuscar.Free;
  end;
end;
✅ Checklist de Boas Práticas
Para cdsBuscar:
 Sempre ordenar antes de usar LocalizarBinário
 Liberar memória com Free no finally
 Usar para buscas pontuais e rápidas
 Manter dados mínimos necessários (campos essenciais)
Para cdsGeral:
 Criar dentro de try/finally
 Usar para processamento em lote
 Validar se está vazio antes de processar (IsEmpty)
 Liberar após processamento completo
Ambos:
 Nunca deixar datasets abertos desnecessariamente
 Usar variáveis locais quando possível
 Documentar o propósito do dataset no código
 Verificar campos existem antes de acessar
🎯 Resumo Executivo
Dataset	Analogia	Uso Principal
cdsBuscar	📖 Dicionário	"Procurar" informação específica
cdsGeral	📦 Caixa de trabalho	"Processar" múltiplos itens
Regra de Ouro:

Use cdsBuscar para ENCONTRAR dados.
Use cdsGeral para PROCESSAR dados.




---

## 15. Boas Práticas no [Sol.NET](http://sol.net/)

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
