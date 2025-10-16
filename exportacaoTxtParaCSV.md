# 📚 EXPLICAÇÃO COMPLETA PASSO A PASSO DO EXPORTADOR CSV

## 🎯 ÍNDICE
1. [Declaração do Método](#parte-1-declaração-do-método-genérico)
2. [Implementação Detalhada](#parte-2-implementação-do-método-genérico)
3. [Uso no BotaoCSV](#parte-3-uso-do-método-no-botaocsv)
4. [Fluxo Completo](#fluxo-completo-do-programa)
5. [Conceitos-Chave](#conceitos-chave-aprendidos)
6. [Resumo Final](#resumo-final)

---

## 🎯 PARTE 1: DECLARAÇÃO DO MÉTODO GENÉRICO

### No arquivo `uFrmLitePDV.pas` - Seção `private`:

```pascala
private
  { Private declarations }
  procedure BotaoFornecedor;
  procedure BotaoFabricante;
  procedure BotaoUnidade;
  procedure BotaoProdutos;
  procedure BotaoProdutoCodigos;
  procedure BotaoProdutoSituacaoEstoque;
  procedure BotaoCSV;
  procedure ExportarParaCSV(Query: TDataSet; FileName, SeparatorChar: String); // ← NOVO!
```

### 📖 Explicação:

**`procedure ExportarParaCSV(...)`** - Declaração do método genérico

**Parâmetros:**
- **`Query: TDataSet`** - Qualquer query/dataset com dados (TFDQuery, TClientDataSet, etc.)
- **`FileName: String`** - Caminho completo do arquivo a ser criado
- **`SeparatorChar: String`** - Caractere separador (`|`, `;`, `,`, TAB, etc.)

**Por que `TDataSet`?** 
- É a classe base de **todas** as fontes de dados no Delphi
- Aceita: TFDQuery, TClientDataSet, TQuery, TADOQuery, etc.
- **Máxima reutilização!**

---

## 🔧 PARTE 2: IMPLEMENTAÇÃO DO MÉTODO GENÉRICO

### Declaração de Variáveis

```pascal
procedure TFrmLitePDV.ExportarParaCSV(Query: TDataSet; FileName, SeparatorChar: String);
var
  CSVFile: TextFile;    // ← Variável para manipular arquivo de texto
  i: Integer;           // ← Contador para loops
  Line: string;         // ← Linha temporária para montar o texto
begin
```

---

### 🎯 PASSO 1: Deletar arquivo existente

```pascal
  // Deletar arquivo se já existir
  if FileExists(FileName) then
    DeleteFile(FileName);
```

**Por quê?**
- Evita erro de "arquivo já existe"
- Garante arquivo limpo/novo
- `FileExists()` verifica se arquivo existe
- `DeleteFile()` apaga o arquivo

---

### 🎯 PASSO 2: Abrir arquivo para escrita

```pascal
  AssignFile(CSVFile, FileName);
  Rewrite(CSVFile);
```

**Linha por linha:**

| Comando | O que faz |
|---------|-----------|
| `AssignFile(CSVFile, FileName)` | **Associa** a variável `CSVFile` ao nome do arquivo no disco |
| `Rewrite(CSVFile)` | **Cria/abre** o arquivo para escrita (modo WRITE) |

**💡 Analogia:**
```
AssignFile = "Pegar uma caneta e apontar para o caderno"
Rewrite    = "Abrir o caderno na primeira página limpa"
```

---

### 🎯 PASSO 3: Escrever CABEÇALHO (Nomes dos Campos)

```pascal
  try
    // Escrever cabeçalho automaticamente
    Line := '';  // ← Começa com linha vazia
    
    for i := 0 to Query.FieldCount - 1 do  // ← Loop por TODOS os campos
    begin
      Line := Line + Query.Fields[i].FieldName;  // ← Adiciona nome do campo
      
      if i < Query.FieldCount - 1 then  // ← Se NÃO for o último campo
        Line := Line + SeparatorChar;   // ← Adiciona separador
    end;
    
    Writeln(CSVFile, Line);  // ← Escreve a linha no arquivo
```

### 📖 Explicação DETALHADA do Loop:

Imagine uma query com 3 campos: `ID`, `NOME`, `PRECO`

| Iteração | `i` | Campo | `Line` antes | Ação | `Line` depois | Adiciona separador? |
|----------|-----|-------|--------------|------|---------------|---------------------|
| 1ª | 0 | ID | `""` | Adiciona "ID" | `"ID"` | ✅ Sim (não é o último) |
| 1ª | 0 | ID | `"ID"` | Adiciona `\|` | `"ID\|"` | - |
| 2ª | 1 | NOME | `"ID\|"` | Adiciona "NOME" | `"ID\|NOME"` | ✅ Sim (não é o último) |
| 2ª | 1 | NOME | `"ID\|NOME"` | Adiciona `\|` | `"ID\|NOME\|"` | - |
| 3ª | 2 | PRECO | `"ID\|NOME\|"` | Adiciona "PRECO" | `"ID\|NOME\|PRECO"` | ❌ Não (é o último!) |

**Resultado final:**
```
ID|NOME|PRECO
```

**Detalhes técnicos:**

```pascal
Query.FieldCount          // ← Total de campos (ex: 3)
Query.FieldCount - 1      // ← Último índice (ex: 2, pois índices são 0,1,2)
Query.Fields[i]           // ← Acessa o campo na posição i
Query.Fields[i].FieldName // ← Pega o NOME do campo (ex: "ID_PRODUTO")
```

---

### 🎯 PASSO 4: Escrever DADOS (Valores dos Campos)

```pascal
    // Escrever dados
    Query.First;  // ← Volta para o PRIMEIRO registro
    
    while not Query.Eof do  // ← Enquanto NÃO chegou no fim (End Of File)
    begin
      Line := '';  // ← Limpa a linha para novo registro
      
      for i := 0 to Query.FieldCount - 1 do  // ← Loop por todos os campos
      begin
        Line := Line + Query.Fields[i].AsString;  // ← Pega VALOR como string
        
        if i < Query.FieldCount - 1 then  // ← Se NÃO for o último campo
          Line := Line + SeparatorChar;   // ← Adiciona separador
      end;
      
      Writeln(CSVFile, Line);  // ← Escreve linha no arquivo
      Query.Next;              // ← Avança para PRÓXIMO registro
    end;
```

### 📖 Explicação DETALHADA com Exemplo:

Imagine que a query retornou 2 produtos:

| ID_PRODUTO | CODIGO | DESCRICAO | PRECO |
|------------|--------|-----------|-------|
| 1 | 001 | Produto A | 10.50 |
| 2 | 002 | Produto B | 25.00 |

**Processamento do 1º REGISTRO:**

| Iteração | `i` | Campo | Valor | `Line` antes | Ação | `Line` depois |
|----------|-----|-------|-------|--------------|------|---------------|
| 1ª | 0 | ID_PRODUTO | 1 | `""` | Adiciona "1" | `"1"` |
| 1ª | 0 | - | - | `"1"` | Adiciona `\|` | `"1\|"` |
| 2ª | 1 | CODIGO | 001 | `"1\|"` | Adiciona "001" | `"1\|001"` |
| 2ª | 1 | - | - | `"1\|001"` | Adiciona `\|` | `"1\|001\|"` |
| 3ª | 2 | DESCRICAO | Produto A | `"1\|001\|"` | Adiciona "Produto A" | `"1\|001\|Produto A"` |
| 3ª | 2 | - | - | `"1\|001\|Produto A"` | Adiciona `\|` | `"1\|001\|Produto A\|"` |
| 4ª | 3 | PRECO | 10.50 | `"1\|001\|Produto A\|"` | Adiciona "10.50" | `"1\|001\|Produto A\|10.50"` |

**Linha escrita no arquivo:**
```
1|001|Produto A|10.50
```

Depois:
- `Writeln(CSVFile, Line)` - Escreve no arquivo
- `Query.Next` - Avança para o próximo registro (produto 2)
- Loop repete até `Query.Eof` (End of File = fim dos dados)

**Detalhes técnicos:**

```pascal
Query.First              // ← Vai para o PRIMEIRO registro (posição 0)
Query.Eof                // ← True se chegou no FIM (End Of File)
not Query.Eof            // ← True enquanto TEM dados
Query.Fields[i].AsString // ← Converte valor para STRING (qualquer tipo)
Query.Next               // ← Avança para PRÓXIMO registro
```

---

### 🎯 PASSO 5: Fechar o arquivo

```pascal
  finally
    CloseFile(CSVFile);  // ← SEMPRE fecha o arquivo
  end;
end;
```

**Por que `finally`?**
- Garante que o arquivo será fechado **SEMPRE**
- Mesmo se der erro no meio
- Libera o arquivo para outros programas usarem

**Sem `CloseFile()`:**
- Arquivo fica "travado"
- Windows não consegue abrir
- Pode corromper dados

---

## 🚀 PARTE 3: USO DO MÉTODO NO BotaoCSV

Agora vou explicar como o método é **USADO**:

```pascal
procedure TFrmLitePDV.BotaoCSV;
var
  Qry: TFDQuery;           // ← Query para buscar dados
  Arquivo: string;         // ← Caminho do arquivo
  SaveDialog: TSaveDialog; // ← Diálogo "Salvar Como"
begin
```

### 🎯 PASSO 1: Criar objetos

```pascal
  SaveDialog := TSaveDialog.Create(nil);
  Qry := TFDQuery.Create(nil);
  try
```

**`SaveDialog`** - Janela do Windows "Salvar Como"  
**`Qry`** - Query para buscar produtos do banco

---

### 🎯 PASSO 2: Configurar diálogo "Salvar Como"

```pascal
    SaveDialog.Title := 'Salvar Exportação de Produtos';
    SaveDialog.Filter := 'Arquivos TXT (*.txt)|*.txt|Arquivos CSV (*.csv)|*.csv|Todos os arquivos (*.*)|*.*';
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.FileName := 'Produtos_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt';
    SaveDialog.InitialDir := ExtractFilePath(ParamStr(0));
```

**Linha por linha:**

| Propriedade | Valor | O que faz |
|-------------|-------|-----------|
| `Title` | "Salvar Exportação..." | Título da janela |
| `Filter` | "Arquivos TXT..." | Tipos de arquivo no dropdown |
| `DefaultExt` | "txt" | Extensão padrão se usuário não digitar |
| `FileName` | "Produtos_20251016..." | Nome sugerido do arquivo |
| `InitialDir` | Pasta do executável | Pasta inicial ao abrir |

**`FormatDateTime('yyyymmdd_hhnnss', Now)`** - Cria timestamp único:
```
Exemplo: Produtos_20251016_153045.txt
         └─────────┘ └──────┘ └────┘
            Data      Hora    Seg
```

---

### 🎯 PASSO 3: Mostrar diálogo e validar

```pascal
    if not SaveDialog.Execute then
    begin
      ShowMessage('Exportação cancelada pelo usuário.');
      Exit;
    end;

    Arquivo := SaveDialog.FileName;
```

**Fluxo:**

```
┌─────────────────────┐
│ SaveDialog.Execute  │
└──────────┬──────────┘
           │
      ┌────┴────┐
      │ Usuário │
      └────┬────┘
           │
    ┌──────┴──────┐
    │ Escolheu?   │
    └──────┬──────┘
           │
    ┌──────┴──────────┐
    │                 │
   Sim               Não
    │                 │
Arquivo =      Exit (sai)
SaveDialog.
FileName
```

---

### 🎯 PASSO 4: Executar consulta SQL

```pascal
    Qry.Connection := Self.DadosDestino.Conexao;
    Qry.SQL.Text :=
      'SELECT P.ID_PRODUTO, P.CODIGO, ' +
      '       COALESCE(P.CODIGO_BARRA, '''') AS CODIGO_BARRA, ' +
      '       P.DESCRICAO, ' +
      '       COALESCE(F.DESCRICAO, '''') AS DESCRICAO_FAMILIA, ' +
      '       COALESCE(P.PRECO_VENDA_1, 0) AS PRECO ' +
      'FROM PRODUTOS P ' +
      'LEFT JOIN FAMILIAS_PRODUTOS F ON F.ID_FAMILIA_PRODUTO = P.ID_FAMILIA_PRODUTO ' +
      'ORDER BY P.ID_PRODUTO';

    Qry.Open;
```

**Detalhando o SQL:**

```sql
SELECT 
  P.ID_PRODUTO,                                      -- Campo 1
  P.CODIGO,                                          -- Campo 2
  COALESCE(P.CODIGO_BARRA, '') AS CODIGO_BARRA,     -- Campo 3 (vazio se NULL)
  P.DESCRICAO,                                       -- Campo 4
  COALESCE(F.DESCRICAO, '') AS DESCRICAO_FAMILIA,   -- Campo 5 (vazio se NULL)
  COALESCE(P.PRECO_VENDA_1, 0) AS PRECO             -- Campo 6 (0 se NULL)
FROM PRODUTOS P
LEFT JOIN FAMILIAS_PRODUTOS F                       -- Busca família (pode não ter)
  ON F.ID_FAMILIA_PRODUTO = P.ID_FAMILIA_PRODUTO
ORDER BY P.ID_PRODUTO;                              -- Ordena por ID
```

**`COALESCE(campo, valor_padrao)`** - Se `campo` for NULL, usa `valor_padrao`

---

### 🎯 PASSO 5: Validar se tem dados

```pascal
    if Qry.IsEmpty then
    begin
      ShowMessage('Nenhum produto encontrado para exportar.');
      Exit;
    end;
```

**Fluxo:**
```
Query.Open
    ↓
┌─────────┐
│ IsEmpty?│
└────┬────┘
     │
  ┌──┴──┐
 Sim   Não
  │     │
Avisa Continua
e sai exportando
```

---

### 🎯 PASSO 6: CHAMAR O MÉTODO GENÉRICO!

```pascal
    // Exportar usando o método genérico (estilo TOTVS)
    ExportarParaCSV(Qry, Arquivo, '|');
```

**Aqui a mágica acontece!** ✨

Parâmetros enviados:
- **`Qry`** - Query com os produtos carregados
- **`Arquivo`** - Caminho completo (ex: "C:\Temp\Produtos_20251016_153045.txt")
- **`'|'`** - Separador pipe

**Internamente o método:**
1. Cria o arquivo
2. Escreve cabeçalho (nomes dos campos)
3. Loop pelos registros escrevendo dados
4. Fecha o arquivo

---

### 🎯 PASSO 7: Mostrar mensagem de sucesso

```pascal
    ShowMessage(Format('Exportação concluída com sucesso!' + sLineBreak +
                       'Total de produtos: %d' + sLineBreak +
                       'Arquivo: %s', [Qry.RecordCount, Arquivo]));
```

**`Format()`** - Substitui `%d` e `%s` pelos valores:
```
'Total de produtos: %d' + [Qry.RecordCount]
                     ↓
'Total de produtos: 150'

'Arquivo: %s' + [Arquivo]
           ↓
'Arquivo: C:\Temp\Produtos_20251016_153045.txt'
```

---

### 🎯 PASSO 8: Liberar memória

```pascal
  finally
    Qry.Free;
    SaveDialog.Free;
  end;
end;
```

**`finally`** - Sempre executa, mesmo com erro  
**`Free`** - Libera memória dos objetos

---

## 📊 FLUXO COMPLETO DO PROGRAMA

```
1. Usuário clica no botão "Desafio CSV"
         ↓
2. btnDesafioCSVClick é chamado
         ↓
3. ExecutarBotao(BotaoCSV, Sender)
         ↓
4. BotaoCSV é executado
         ↓
5. Cria SaveDialog e Qry
         ↓
6. Mostra janela "Salvar Como"
         ↓
7. Usuário escolhe local e nome
         ↓
8. Executa SQL buscando produtos
         ↓
9. Verifica se tem dados
         ↓
10. CHAMA ExportarParaCSV(Qry, Arquivo, '|')
         ↓
    ┌─────────────────────────────┐
    │ ExportarParaCSV             │
    ├─────────────────────────────┤
    │ 1. Deleta arquivo antigo    │
    │ 2. Cria arquivo novo        │
    │ 3. Escreve CABEÇALHO        │
    │ 4. Loop pelos REGISTROS:    │
    │    - Loop pelos CAMPOS      │
    │    - Adiciona valor + |     │
    │    - Writeln no arquivo     │
    │    - Next (próximo)         │
    │ 5. CloseFile                │
    └─────────────────────────────┘
         ↓
11. Mostra mensagem de sucesso
         ↓
12. Libera memória (Qry.Free)
         ↓
13. FIM
```

---

## 🎯 RESULTADO FINAL NO ARQUIVO

Exemplo do que é gerado:

```
ID_PRODUTO|CODIGO|CODIGO_BARRA|DESCRICAO|DESCRICAO_FAMILIA|PRECO
1|001|7891234567890|Produto Teste|Alimentos|10.50
2|002|7891234567891|Outro Produto|Bebidas|25.00
3|003|7891234567892|Mais Um|Limpeza|5.99
```

**Linha 1:** Cabeçalho (nomes dos campos)  
**Linhas 2+:** Dados (um produto por linha)  
**Separador:** `|` (pipe)

---

## 🔑 CONCEITOS-CHAVE APRENDIDOS

| Conceito | O que é | Para que serve |
|----------|---------|----------------|
| **TextFile** | Variável para arquivo texto | Ler/escrever arquivos linha por linha |
| **AssignFile** | Associa variável ao arquivo | Liga a variável ao caminho no disco |
| **Rewrite** | Cria/abre arquivo para escrita | Prepara arquivo para receber dados |
| **Writeln** | Escreve linha no arquivo | Adiciona texto + quebra de linha |
| **CloseFile** | Fecha o arquivo | Libera arquivo e garante gravação |
| **TDataSet** | Classe base de dados | Aceita qualquer query/dataset |
| **Query.Fields[i]** | Acessa campo por índice | Pega campo na posição i |
| **FieldName** | Nome do campo | Ex: "ID_PRODUTO" |
| **AsString** | Converte para string | Transforma qualquer tipo em texto |
| **FieldCount** | Total de campos | Quantos campos a query tem |
| **RecordCount** | Total de registros | Quantas linhas a query retornou |
| **Query.First** | Vai para primeiro registro | Posiciona no início |
| **Query.Next** | Próximo registro | Avança uma linha |
| **Query.Eof** | End of File | True se acabaram os dados |

---

## 💡 POR QUE ESSE MÉTODO É GENÉRICO?

```pascal
// ❌ Método ESPECÍFICO (só funciona para produtos)
procedure ExportarProdutos(Arquivo: string);
begin
  Writeln('ID_PRODUTO|CODIGO|DESCRICAO');  // ← Fixo
  Writeln(Qry.Fields[0] + '|' + Qry.Fields[1] + '|' + Qry.Fields[2]); // ← Fixo
end;

// ✅ Método GENÉRICO (funciona para QUALQUER query)
procedure ExportarParaCSV(Query: TDataSet; FileName, SeparatorChar: String);
begin
  for i := 0 to Query.FieldCount - 1 do     // ← Dinâmico!
    Writeln(Query.Fields[i].AsString);      // ← Funciona com qualquer campo!
end;
```

**Pode usar para:**
- Produtos ✅
- Clientes ✅
- Vendas ✅
- Fornecedores ✅
- **QUALQUER TABELA!** ✅

---

## 📚 EXEMPLOS DE USO

### Exemplo 1: Exportar Clientes

```pascal
procedure ExportarClientes;
var
  Qry: TFDQuery;
begin
  Qry := TFDQuery.Create(nil);
  try
    Qry.Connection := DadosDestino.Conexao;
    Qry.SQL.Text := 'SELECT * FROM PESSOAS WHERE TPCLIENTE = 1';
    Qry.Open;
    
    ExportarParaCSV(Qry, 'C:\Temp\Clientes.csv', ';');
    
    ShowMessage('Clientes exportados!');
  finally
    Qry.Free;
  end;
end;
```

### Exemplo 2: Exportar Vendas com TAB

```pascal
procedure ExportarVendas;
var
  Qry: TFDQuery;
begin
  Qry := TFDQuery.Create(nil);
  try
    Qry.Connection := DadosDestino.Conexao;
    Qry.SQL.Text := 'SELECT * FROM MOVIMENTOS WHERE TIPO = 0';
    Qry.Open;
    
    // Usa TAB como separador (#9)
    ExportarParaCSV(Qry, 'C:\Temp\Vendas.tsv', #9);
    
    ShowMessage('Vendas exportadas!');
  finally
    Qry.Free;
  end;
end;
```

### Exemplo 3: Exportar Fornecedores (CSV padrão)

```pascal
procedure ExportarFornecedores;
var
  Qry: TFDQuery;
begin
  Qry := TFDQuery.Create(nil);
  try
    Qry.Connection := DadosDestino.Conexao;
    Qry.SQL.Text := 'SELECT * FROM PESSOAS WHERE TPFORNECEDOR = 1';
    Qry.Open;
    
    // Usa vírgula (padrão CSV)
    ExportarParaCSV(Qry, 'C:\Temp\Fornecedores.csv', ',');
    
    ShowMessage('Fornecedores exportados!');
  finally
    Qry.Free;
  end;
end;
```

---

## 🎓 RESUMO FINAL

### O que foi feito:
1. ✅ Criado método **genérico** `ExportarParaCSV`
2. ✅ Método aceita **qualquer query**
3. ✅ Cabeçalho gerado **automaticamente**
4. ✅ Dados exportados em **loop automático**
5. ✅ Separador **parametrizável**
6. ✅ Código **reutilizável** para outras exportações
7. ✅ Inspirado no **padrão TOTVS profissional**

### Aprendizado:
- Trabalhar com `TextFile` (arquivos texto)
- Loops dinâmicos por campos (`FieldCount`)
- Acessar campos por índice (`Fields[i]`)
- Converter tipos automaticamente (`AsString`)
- Criar métodos genéricos reutilizáveis
- Padrão de código profissional

### Está pronto para:
- ✅ Usar em produção
- ✅ Adicionar novas exportações facilmente
- ✅ Manter e evoluir o código
- ✅ Impressionar seu supervisor! 🎉

---

## 🔗 COMPARAÇÃO: ANTES vs DEPOIS

### ANTES (Abordagem TStringList)

**Prós:**
- Simples de entender
- Funciona para casos específicos

**Contras:**
- Usa mais memória (carrega tudo)
- Código duplicado para cada exportação
- Cabeçalho manual (propenso a erros)
- Campos hardcoded
- Difícil de manter

### DEPOIS (Abordagem TOTVS)

**Prós:**
- Método genérico reutilizável
- Cabeçalho automático
- Campos dinâmicos
- Separador flexível
- Streaming (menos memória)
- Fácil manutenção
- Código profissional

**Contras:**
- Nenhum! 😎

---

## 📝 NOTAS IMPORTANTES

### Encoding
O método usa encoding padrão do sistema (ANSI). Se precisar UTF-8, pode:
1. Usar `TStringList` com `SaveToFile(Arquivo, TEncoding.UTF8)`
2. Ou adicionar BOM UTF-8 manualmente com `TFileStream`

### Performance
- Para arquivos pequenos (< 10.000 linhas): ambos métodos são rápidos
- Para arquivos grandes (> 100.000 linhas): TextFile é mais eficiente

### Separadores Comuns
- `|` - Pipe (usado neste projeto)
- `;` - Ponto e vírgula (padrão Brasil)
- `,` - Vírgula (padrão internacional)
- `#9` - TAB (para planilhas)
- `#32` - Espaço

---

**Arquivo criado em:** 16/10/2025  
**Versão:** 1.0  
**Projeto:** Sol.NET - Sistema de Conversão  
**Autor:** Equipe de Desenvolvimento

---

🎉 **FIM DA EXPLICAÇÃO** 🎉



```pascal
unit uFrmLitePDV;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uFrmConversao, Data.DB, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons, GeneralEdits, Vcl.Grids, Vcl.DBGrids,
  DBGridPlus, ComboBoxPlus, Vcl.ExtCtrls, RadioGroupPlus, Vcl.Mask,
  FireDAC.Comp.Client, FireDAC.Stan.Param;

type
  TFrmLitePDV = class(TFrmConversao)
    tabPessoas: TTabSheet;
    btnFornecedor: TBitBtn;
    btnFabricante: TBitBtn;
    tabProdutos: TTabSheet;
    btnProdutos: TBitBtn;
    btnUnidade: TBitBtn;
    btnProdSituacaoEstoque: TBitBtn;
    btnProdCodigos: TBitBtn;
    btnExportacaoCSV: TBitBtn;
    procedure btnProdutosClick(Sender: TObject);
    procedure btnUnidadeClick(Sender: TObject);
    procedure btnProdCodigosClick(Sender: TObject);
    procedure btnProdSituacaoEstoqueClick(Sender: TObject);
    procedure btnFornecedorClick(Sender: TObject);
    procedure btnFabricanteClick(Sender: TObject);
    procedure btnExportacaoCSVClick(Sender: TObject);
  private
    { Private declarations }
    procedure BotaoFornecedor;
    procedure BotaoFabricante;
    procedure BotaoUnidade;
    procedure BotaoProdutos;
    procedure BotaoProdutoCodigos;
    procedure BotaoProdutoSituacaoEstoque;
    procedure BotaoCSV;
    procedure ExportarParaCSV(Query: TDataSet; FileName, SeparatorChar: String);
  public
    { Public declarations }
  end;

var
    FrmLitePDV: TFrmLitePDV;

implementation

uses
  ConversaoBuilder, uConversao.TConversaoGenerica, uConversao.TiposAuxiliares, uCdsHelper, Datasnap.DBClient, System.Generics.Collections,
  uSolnetUtils;

{$R *.dfm}

{ TFrmlitePDV }

procedure TFrmLitePDV.BotaoUnidade;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaUnidade.Create, 'CADASTRODESCRICAO')
      .AddPrimaryKey('UN')
      .AddCampo('CODIGO', 'UN')
      .AddCampo('DESCRICAO', 'UN')
      .Build;
  ConversaoUnidade(ParametrosConversao);
end;

procedure TFrmLitePDV.BotaoProdutos;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaProduto.Create(Produto), 'CADASTRODESCRICAO')
      .AddPrimaryKey('CODIGO')
      .AddCampo('ID_UNIDADE', 'UN', TTabelaUnidade.Create)
      .AddCampo('ID_FABRICANTE', 'LINKFABRICANTE', TTabelaPessoa.Create(Fab))
      .AddCampo('ID_FORNECEDOR', 'FORNECEDOR', TTabelaPessoa.Create(Fornecedor))
      .AddCampo('PRECO_VENDA_1', 'PRECO')
      .AddCampo('DT_CADASTRO', 'DATA')
      .AddCampo('VL_PESO_BRUTO', 'PESOBRUTO')
      .AddCampo('VL_PESO_LIQUIDO', 'PESELIQUIDO')
      .AddCampo('COMPRIMENTO', 'COMPRIMENTO')
      .AddCampo('OBS_COMPLEMENTAR', 'CAMINHOFOTO')
      .AddCampo('ID_GRADE', 'IDGRADE')
      .AddCampo('PROMOCAO_1', 'PRECOPROMOCIONAL')
      .AddCampo('DESCRICAO_AUX2', 'DESCRICAOGRADE')
      .AddCampo('ID_GRADE', 'IDGRADE')
      .AddCampo('PROMOCAO_1', 'PRECOPROMOCIONAL')
      .AddCampo('DESCRICAO_AUX2', 'DESCRICAOGRADE')
      .AddCampo('DESCRICAO', 'DESCRICAO')
      .AddCampo('TP_HABILITAR_CUPOM_PROMO', 'IIF(ATIVARPROMOCAO = ''S'', 0, 1)')
      .AddCampo('DT_VERIFICACAO_DESC', 'VENCIMENTOPROMOCAO')
      .AddCampo('PROMOCAO_2', 'QUANTPROMOCAO')
      .AddCampo('TP_PROD_IMPORTADO', 'IDIMPORTACAOPROD')
      .AddCampo('ALTURA', 'ALTURA')
      .AddCampo('LARGURA', 'LARGURA')
      .AddCampo('LUCRO_LIQUIDO_1', 'LUCRO')
      .AddCampo('PC_MARGEM_LUCRO_1', 'MARGEMLUCRO')
      .AddCampo('ULTIMO_PRECO_COMPRA', 'PRECOCOMPRA')
      .AddCampo('DESCRICAO_AUX1', 'REFERENCIA')
      .AddCampo('INATIVO', 'IIF(PARTICIPACAO = ''DISPON�VEL'', 1, 0)')
      .Build;

  ConversaoProduto(ParametrosConversao);
end;

procedure TFrmLitePDV.BotaoProdutoCodigos;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaProdutoCodigos.Create, 'CADASTRODESCRICAO')
      .AddCampo('ID_PRODUTO', 'CODIGO', TTabelaProduto.Create(Produto))
      .AddCampo('TP_CODIGO', '1')
      .AddCampo('ID_PESSOA', '0', TTabelaPessoa.Create(Fab))
      .AddCampo('CODIGO', 'CODIGOBARRAS')
      .AddCampo('PADRAO', '1')
      .AddCampo('TP_BARRA', 'null')
      .AddCampo('TP_BALANCA', 'null')
      .AddWhere('CODIGOBARRAS IS NOT NULL AND CODIGOBARRAS <> ''''')
      .BuildAndCreateNewParametroSQL

      .SetTabelaConversao(TTabelaProdutoCodigos.Create, 'CADASTRODESCRICAO')
      .AddCampo('ID_PRODUTO', 'CODIGO', TTabelaProduto.Create(Produto))
      .AddCampo('TP_CODIGO', '1')
      .AddCampo('ID_PESSOA', '0')
      .AddCampo('CODIGO', 'CODBARRASALTERNATIVO')
      .AddCampo('PADRAO', '0')
      .AddCampo('TP_BARRA', '0')
      .AddCampo('TP_BALANCA', 'null')
      .AddWhere('CODBARRASALTERNATIVO IS NOT NULL AND CODBARRASALTERNATIVO <> ''''')
      .Build;

  ConversaoProdutoCodigos(ParametrosConversao);

end;

procedure TFrmLitePDV.BotaoProdutoSituacaoEstoque;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaProdutosSituacaoEstoque.Create, 'CADASTRODESCRICAO')
      .AddCampo('ID_EMPRESA', '0')
      .AddCampo('ID_LOCAL_ESTOQUE', '1')
      .AddCampo('ID_SITUACAO_ESTOQUE', '1')
      .AddCampo('ID_PRODUTO', 'CODIGO', TTabelaProduto.Create(Produto))
      .AddCampo('SALDO', 'ESTOQUE')
      .Build;

  ConversaoProdutoSituacaoEstoque(ParametrosConversao);

end;

procedure TFrmLitePDV.BotaoFornecedor;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaPessoa.Create(Fornecedor), 'CADASTRODESCRICAO')
      .AddPrimaryKey('CODIGO')
      .AddCampo('NOME', 'FORNECEDOR')
      .AddCampo('TPFORNECEDOR', 'LINKFORNECEDOR')
      .Build;

  ConversaoPessoas(ParametrosConversao);

end;

procedure TFrmLitePDV.BotaoFabricante;
begin
  var ParametrosConversao: TParametrosConversao := TConversaoBuilder.Create
      .SetTabelaConversao(TTabelaPessoa.Create(Fab), 'CADASTRODESCRICAO')
      .AddPrimaryKey('CODIGO')
      .AddCampo('TPFABRICANTE', 'LINKFABRICANTE')
      .Build;

  ConversaoPessoas(ParametrosConversao);
end;

procedure TFrmLitePDV.ExportarParaCSV(Query: TDataSet; FileName, SeparatorChar: String);
var
  CSVFile: TextFile;
  i: Integer;
  Line: string;
begin

  if FileExists(FileName) then
    DeleteFile(FileName);

  AssignFile(CSVFile, FileName);
  Rewrite(CSVFile);

  try

    Line := '';
    for i := 0 to Query.FieldCount - 1 do
    begin
      Line := Line + Query.Fields[i].FieldName;
      if i < Query.FieldCount - 1 then
        Line := Line + SeparatorChar;
    end;
    Writeln(CSVFile, Line);


    Query.First;
    while not Query.Eof do
    begin
      Line := '';
      for i := 0 to Query.FieldCount - 1 do
      begin
        Line := Line + Query.Fields[i].AsString;
        if i < Query.FieldCount - 1 then
          Line := Line + SeparatorChar;
      end;
      Writeln(CSVFile, Line);
      Query.Next;
    end;

  finally
    CloseFile(CSVFile);
  end;
end;

procedure TFrmLitePDV.BotaoCSV;
var
  Qry: TFDQuery;
  Arquivo: string;
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  Qry := TFDQuery.Create(nil);
  try

    SaveDialog.Title := 'Salvar Exportação de Produtos';
    SaveDialog.Filter := 'Arquivos TXT (*.txt)|*.txt|Arquivos CSV (*.csv)|*.csv|Todos os arquivos (*.*)|*.*';
    SaveDialog.DefaultExt := 'txt';
    SaveDialog.FileName := 'Produtos_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt';
    SaveDialog.InitialDir := ExtractFilePath(ParamStr(0));


    if not SaveDialog.Execute then
    begin
      ShowMessage('Exportação cancelada pelo usuário.');
      Exit;
    end;

    Arquivo := SaveDialog.FileName;


    Qry.Connection := Self.DadosDestino.Conexao;
    Qry.SQL.Text :=
      'SELECT P.ID_PRODUTO, P.CODIGO, ' +
      '       COALESCE(P.CODIGO_BARRA, '''') AS CODIGO_BARRA, ' +
      '       P.DESCRICAO, ' +
      '       COALESCE(F.DESCRICAO, '''') AS DESCRICAO_FAMILIA, ' +
      '       COALESCE(P.PRECO_VENDA_1, 0) AS PRECO ' +
      'FROM PRODUTOS P ' +
      'LEFT JOIN FAMILIAS_PRODUTOS F ON F.ID_FAMILIA_PRODUTO = P.ID_FAMILIA_PRODUTO ' +
      'ORDER BY P.ID_PRODUTO';

    Qry.Open;


    if Qry.IsEmpty then
    begin
      ShowMessage('Nenhum produto encontrado para exportar.');
      Exit;
    end;


    ExportarParaCSV(Qry, Arquivo, '|');


    ShowMessage(Format('Exportação concluída com sucesso!' + sLineBreak +
                       'Total de produtos: %d' + sLineBreak +
                       'Arquivo: %s', [Qry.RecordCount, Arquivo]));

  finally
    Qry.Free;
    SaveDialog.Free;
  end;
end;

procedure TFrmLitePDV.btnExportacaoCSVClick(Sender: TObject);
begin
  inherited;
  ExecutarBotao(BotaoCSV, Sender);

end;

procedure TFrmLitePDV.btnFabricanteClick(Sender: TObject);
begin
  inherited;
  ExecutarBotao(BotaoFabricante, Sender);
end;

procedure TFrmLitePDV.btnFornecedorClick(Sender: TObject);
begin
  inherited;
  ExecutarBotao(BotaoFornecedor, Sender);
end;

procedure TFrmLitePDV.btnProdCodigosClick(Sender: TObject);
begin
  inherited;
  ExecutarBotao(BotaoProdutoCodigos, Sender);
end;

procedure TFrmLitePDV.btnProdSituacaoEstoqueClick(Sender: TObject);
begin
  inherited;
  ExecutarBotao(BotaoProdutoSituacaoEstoque, Sender);

end;

procedure TFrmLitePDV.btnProdutosClick(Sender: TObject);
begin
  inherited;
  ExecutarBotao(BotaoProdutos, Sender);

end;

procedure TFrmLitePDV.btnUnidadeClick(Sender: TObject);
begin
  inherited;
  ExecutarBotao(BotaoUnidade, Sender);
end;

end.
```
