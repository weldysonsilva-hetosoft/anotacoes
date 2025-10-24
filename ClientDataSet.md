# Documentação Técnica: TClientDataSet no Sol.NET

## 📋 Visão Geral

O **TClientDataSet** é um componente fundamental no Sol.NET para manipulação de dados em memória. Ele armazena o resultado de consultas ao banco de dados e fornece funcionalidades avançadas de navegação, filtragem, ordenação e localização de registros.

## 🎯 Tipos de ClientDataSet no Sol.NET

O sistema Sol.NET utiliza principalmente dois tipos de TClientDataSet:

### 1. **cdsBuscar**
- **Propósito:** Armazenar resultados de consultas gerais
- **Uso comum:** Listagens, relatórios, busca de dados
- **Escopo:** Variável local ou global dependendo do contexto

### 2. **cdsEntidade** (Nomenclatura Variável)
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
 
É o objeto que armazena o resultado de uma consulta no banco de dados.
Também armazena os metadados da consulta (Nome das colunas e seus tipos de dados, quantidade de registros carregados, etc)
 
Retorna o valor de uma coluna da linha selecionada através da chamada de cdsMeuCds.`FieldByName('NOME_DA_COLUNA').Value'. O Value pode ser convertido para um tipo específico para fins de segurança de valores null, ex: FieldByName('NOME_DA_COLUNA').AsInteger, AsString, AsDateTime, AsString, etc.
 
### Populando um TClientDataSet
 
var strList: TStringList := TStringList.Create;
strList.Add('SELECT         ');
strList.Add('    FIRST 5 *  ');
strList.Add('FROM           ');
strList.Add('    cargos     ');
strList.Add('ORDER BY       ');
strList.Add('    descricao  ');
 
 
MeuCds.Data := Dados.QryOpenOle(strList.Text);
 
### Fazendo operações com o TClientDataSet
 
while not MeuCds.Eof do
begin
	// Executa o código necessário, baseado na linha atual que o TClientDataSet está apontando
	ShowMessage(MeuCds.FieldByName('ID_CARGO').AsString);
	// Move o cursor para a próxima posição (caso ela exista)
	MeuCds.Next;
end;


**Última atualização:** 24 de outubro de 2025  
**Versão:** 1.0  
**Responsável:** Equipe de Desenvolvimento Sol.NET
