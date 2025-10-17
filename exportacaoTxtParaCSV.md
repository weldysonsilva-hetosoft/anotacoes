# 📚 GUIA COMPLETO: EXPORTAÇÃO CSV - DO BÁSICO AO AVANÇADO


## 1. VISÃO GERAL

### O que o sistema faz?

Exporta produtos do banco de dados para arquivo texto (CSV/TXT) com campos separados por `|`.

### Fluxo completo:

`Usuário clica → Escolhe local → Busca dados → Exporta → Confirma sucesso`

---

## 2. ESTRUTURA DO CÓDIGO

### Declarações no `private`:

```pascal

procedure BotaoCSV;
procedure ExportarParaCSV(Query: TDataSet; FileName, SeparatorChar: String);
```

**Por que private?**

- Só acessível dentro da classe `TFrmLitePDV`
- Organização e proteção do código

**Separação de responsabilidades:**

```
ProcedimentoFunçãoBotaoCSVCoordenador - gerencia processo (janela, SQL, validações)ExportarParaCSVExecutor - grava arquivo (genérico e reutilizável)
```

---

## 3. IMPLEMENTAÇÃO PASSO A PASSO

### 🔘 EVENTO DO BOTÃO

```pascal

procedure TFrmLitePDV.btnExportacaoCSVClick(Sender: TObject);
begin
  inherited;
  ExecutarBotao(BotaoCSV, Sender);
end;
```
**Explicação:**

- `inherited` → executa código da classe base primeiro
- `ExecutarBotao` → wrapper centralizado (validações, log, controle de acesso)
- `BotaoCSV` → procedimento com lógica real

---

### 📋 PROCEDIMENTO BOTAOCSV (COORDENADOR)

### **ETAPA 1: Preparação**

```pascal

procedure TFrmLitePDV.BotaoCSV;
var
  Qry: TFDQuery;
  Arquivo: string;
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  Qry := TFDQuery.Create(nil);
  try
```
**Variáveis:**

- `Qry` → consulta SQL
- `Arquivo` → caminho do arquivo
- `SaveDialog` → janela "Salvar como"

---

### **ETAPA 2: Configurar SaveDialog**

```pascal

SaveDialog.Title := 'Salvar Exportação de Produtos';
SaveDialog.Filter := 'Arquivos TXT (*.txt)|*.txt|Arquivos CSV (*.csv)|*.csv|Todos os arquivos (*.*)|*.*';
SaveDialog.DefaultExt := 'txt';
SaveDialog.FileName := 'Produtos_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt';
SaveDialog.InitialDir := ExtractFilePath(ParamStr(0));
```
**Detalhes:**

- **Filter**: 3 opções (TXT, CSV, Todos)
- **FileName**: `Produtos_20241017_143025.txt` (evita sobrescrever)
- **InitialDir**: pasta do executável

---

### **ETAPA 3: Validar Usuário**

```pascal

if not SaveDialog.Execute then
begin
  ShowMessage('Exportação cancelada pelo usuário.');
  Exit;
end;
```
`Arquivo := SaveDialog.FileName;`

**Fluxo:**

- `Execute` retorna `True` (salvou) ou `False` (cancelou)
- Se cancelar → sai imediatamente

---

### **ETAPA 4: Executar SQL**

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
**Detalhes SQL:**

- `COALESCE(campo, valor_padrão)` → se NULL, usa padrão
- `LEFT JOIN` → traz TODOS produtos (com ou sem família)
- `ORDER BY` → ordena por ID

---

### **ETAPA 5: Validar Dados**

```pascal

if Qry.IsEmpty then
begin
  ShowMessage('Nenhum produto encontrado para exportar.');
  Exit;
end;
```
---

### **ETAPA 6: Exportar**

```pascal

ExportarParaCSV(Qry, Arquivo, '|');
```
**Parâmetros:**

1. `Qry` → dados
2. `Arquivo` → destino
3. `'|'` → separador

---

### **ETAPA 7: Sucesso**

```pascal

ShowMessage(Format('Exportação concluída com sucesso!' + sLineBreak +
                   'Total de produtos: %d' + sLineBreak +
                   'Arquivo: %s', [Qry.RecordCount, Arquivo]));
```

**Resultado:**

Exportação concluída com sucesso!
Total de produtos: 237
Arquivo: C:\Export\Produtos_20241017_143025.txt

---

### **ETAPA 8: Limpar Memória**

```pascal

  finally
    Qry.Free;
    SaveDialog.Free;
  end;
end;
```
**Crítico:** `finally` sempre executa (evita memory leak)

---

### 💾 PROCEDIMENTO EXPORTARPARACSV (EXECUTOR)

```pascal

procedure TFrmLitePDV.ExportarParaCSV(Query: TDataSet; FileName, SeparatorChar: String);
var
  CSVFile: TextFile;
  i: Integer;
  Line: string;
begin
```
**Por que `TDataSet`?**

- Classe base de TODAS as fontes de dados
- Aceita: TFDQuery, TClientDataSet, TADOQuery, etc.
- **Máxima reutilização!**

---

### **PASSO 1: Preparar Arquivo**

```pascal

if FileExists(FileName) then
  DeleteFile(FileName);

AssignFile(CSVFile, FileName);
Rewrite(CSVFile);
```
**Comandos:**

```
ComandoFunçãoFileExistsVerifica se arquivo existeDeleteFileApaga arquivo antigoAssignFileAssocia variável ao arquivoRewriteCria/abre para escrita
```

---

### **PASSO 2: Escrever Cabeçalho**

```pascal

try
  Line := '';
  for i := 0 to Query.FieldCount - 1 do
  begin
    Line := Line + Query.Fields[i].FieldName;
    if i < Query.FieldCount - 1 then
      Line := Line + SeparatorChar;
  end;
  Writeln(CSVFile, Line);
````
**Exemplo de processamento:**

```
iCampoLineAdiciona separador?0ID_PRODUTO"ID_PRODUTO"✅ Sim1CODIGO"ID_PRODUTO|CODIGO"✅ Sim2DESCRICAO"ID_PRODUTO|CODIGO|DESCRICAO"❌ Não (último)
```

**Resultado:** `ID_PRODUTO|CODIGO|DESCRICAO`

---

### **PASSO 3: Escrever Dados**

```pascal

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
```
**Loop:**

- `Query.First` → vai para primeiro registro
- `while not Query.Eof` → enquanto não acabar
- `Fields[i].AsString` → converte VALOR para texto
- `Query.Next` → próximo registro

**Exemplo de linha:** `1|001|7891234567890|Arroz Integral 1kg|Alimentos|15.90`

---

### **PASSO 4: Fechar Arquivo**

```pascal

  finally
    CloseFile(CSVFile);
  end;
end;
```
**Crítico:** Sem `CloseFile`, arquivo fica travado e dados podem ser perdidos!

---

## 4. CONCEITOS FUNDAMENTAIS

### TRY...FINALLY

```pascal

try
  *// código que pode dar erro*
finally
  *// SEMPRE executado (limpeza)*
end;
```
**Uso:** Garantir liberação de recursos (memória, arquivos, conexões)

---

### CREATE e FREE

```pascal

Objeto := TClasse.Create(nil);
try
  *// usar objeto*
finally
  Objeto.Free;
end;
```
**Regra de ouro:** Tudo que você `.Create`, deve `.Free`

---

### TDataSet - Principais Membros

```
MembroDescriçãoFieldCountNúmero de colunasFields[i]Acessa campo pelo índiceFields[i].FieldNameNome do campoFields[i].AsStringValor como stringRecordCountNúmero de registrosFirstVai para primeiro registroNextPróximo registroEofTrue se acabaram os dadosIsEmptyTrue se não tem dados
```

---

### TextFile - Operações

```pascal

AssignFile(Arquivo, 'caminho.txt');  *// Associar*
Rewrite(Arquivo);                     *// Criar/abrir*
Writeln(Arquivo, 'texto');            *// Escrever linha*
CloseFile(Arquivo);                   *// Fechar (crítico!)*`
```
---

### Function vs Procedure

**PROCEDURE** - só executa:

```pascal

procedure MostrarMensagem(texto: string);
begin
  ShowMessage(texto);
end;
```
**FUNCTION** - executa E retorna:

```pascal

function Somar(a, b: Integer): Integer;
begin
  Result := a + b;
end;
```
**Quando usar function?**

- Quando **precisa do retorno** para decidir algo
- Exemplo: `if ExportarCSV(Query) then ShowMessage('OK');`

**Para este caso:** Procedure está bom (exception já trata erros)

---

## 5. EXEMPLOS DE USO

### Exportar Clientes

```pascal

Qry.SQL.Text := 'SELECT * FROM PESSOAS WHERE TPCLIENTE = 1';
Qry.Open;
ExportarParaCSV(Qry, 'C:\Temp\Clientes.csv', ';');
```
### Exportar Vendas (separador TAB)

```pascal

Qry.SQL.Text := 'SELECT * FROM MOVIMENTOS WHERE TIPO = 0';
Qry.Open;
ExportarParaCSV(Qry, 'C:\Temp\Vendas.tsv', #9);
```
### Separadores Comuns

```
SeparadorCódigoUsoPipe'|'Padrão do projetoPonto e vírgula';'CSV BrasilVírgula','CSV internacionalTAB#9Planilhas
```

---

## 6. MELHORIAS E BOAS PRÁTICAS

### ✅ Tratamento de Erros

```pascal

try
  ExportarParaCSV(Qry, Arquivo, '|');
except
  on E: Exception do
    ShowMessage('Erro ao exportar: ' + E.Message);
end;
```
### ✅ Barra de Progresso (muitos registros)

```pascal

ProgressBar.Max := Query.RecordCount;
while not Query.Eof do
begin
  *// exportar linha*
  ProgressBar.Position := ProgressBar.Position + 1;
  Query.Next;
end;
```
### ✅ Encoding UTF-8 (acentos)

```pascal

var
  Lista: TStringList;
begin
  Lista := TStringList.Create;
  try
    *// adicionar linhas*
    Lista.SaveToFile(Arquivo, TEncoding.UTF8);
  finally
    Lista.Free;
  end;
end;`
```
### ⚠️ Atenção: Caracteres Especiais

Se descrição contém `|`, vai "quebrar" colunas.

**Solução:** Usar aspas ou escapar:

```pascal

Line := Line + '"' + StringReplace(Valor, '"', '""', [rfReplaceAll]) + '"';
```

---

## 📊 ARQUIVO FINAL GERADO
```
ID_PRODUTO|CODIGO|CODIGO_BARRA|DESCRICAO|DESCRICAO_FAMILIA|PRECO
1|001|7891234567890|Arroz Integral 1kg|Alimentos|15.90
2|002|7891234567891|Feijão Preto 1kg|Alimentos|8.50
3|003||Detergente Líquido|Limpeza|3.20
4|004|7891234567892|Sabonete 90g|Higiene|2.50`
```
**Observe:** Linha 3 tem campo vazio (`||`) - produto sem código de barras

---

## 🎯 RESUMO FINAL

### Vantagens do método genérico:

✅ Reutilizável para qualquer tabela

✅ Cabeçalho automático

✅ Separador parametrizável

✅ Código profissional (padrão TOTVS)

✅ Fácil manutenção

### Checklist de conclusão:

- [x]  Leitura correta do banco
- [x]  Arquivo gerado conforme especificado
- [x]  Código organizado e limpo
- [x]  Validações implementadas
- [x]  Memória liberada corretamente

---

**Versão:** 2.0 Unificada

**Data:** 17/10/2024

**Projeto:** Sol.NET - Sistema de Conversão
