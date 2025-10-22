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

## 📚 Referências

- **Documentação Sol.NET Conversão:** `Documentacao/Conversao/Documentacao Basica.md`
- **Framework de Integrações:** `Documentacao/Integracoes/Framework Integracoes.md`
- **Código-fonte:** `Framework/Integracoes/*`, `SolNET_Conversao/*`

---

**Última atualização:** 22 de outubro de 2025  
**Versão:** 1.0  
**Responsável:** Copilot Space - Sol.NET  
**Contexto:** Sistema ERP Sol.NET (Delphi 12.2)
