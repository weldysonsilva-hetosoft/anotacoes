# ANOTAÇÕES SOL.NET
## 1. Buscar SQL dentro do Delphi

```pascal
// Exemplo de como buscar e copiar SQL no Delphi
procedure TForm1.BuscarSQL;
var
  strSql: string;
begin
  strSql := 'SELECT * FROM tabela WHERE condicao = 1';
  
  // Copiar para área de transferência
  Geral.CopiarAreaTransferencia(strSql);
  
  // Alternativa usando Clipboard unit
  Clipboard.AsText := strSql;
end;
```

**Métodos para buscar SQL:**
- `Ctrl + F` - Buscar no código fonte
- `Ctrl + Shift + F` - Buscar em todos os arquivos do projeto
- `Find in Files` - Buscar em múltiplos arquivos

## 2. Inspecionar Variáveis (Ctrl + F7)

```pascal
// Exemplo prático de inspeção
procedure TForm1.ExemploInspecionar;
var
  i: Integer;
  strTexto: string;
  obj: TObject;
begin
  i := 10;
  strTexto := 'Texto de exemplo';
  obj := TObject.Create;
  
  try
    // Coloque o cursor sobre qualquer variável e pressione Ctrl + F7
    ShowMessage(IntToStr(i));
    ShowMessage(strTexto);
  finally
    obj.Free;
  end;
end;
```

**Funcionalidades do Ctrl + F7:**
- 🔍 **Inspecionar valor** de variáveis em tempo de desenvolvimento
- 📊 **Ver conteúdo** de objetos e arrays
- 🔬 **Analisar expressões** complexas
- 🎯 **Debug visual** durante a execução

**Dica:** Use `Evaluate/Modify` (Ctrl + F7) durante o debug para testar expressões e modificar valores de variáveis em tempo real.


**Classe é a definição de como vai ser e Objerto é a realização dessa definição. A partir do momento que cria algo seguindo a definiçãoestamos instanciando o objeto dessa classe. criando o objeto** 
