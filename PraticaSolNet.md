

---

# 🧠 Exercícios de Domínio - Fundamentos Delphi

# 📚 Lista de Exercícios Práticos - Delphi (Sol.NET)

> **Objetivo:** Praticar lógica de programação e fundamentos do Delphi sem tópicos avançados (sem classes, BD, arquivos).

---

## 🧮 **1. Estruturas e Lógica Básica**

### **Exercício 1.1 - Arrays**
**Enunciado:** Crie um array de 5 notas de alunos. Percorra o array, some todas as notas e calcule a média.

```pascal
procedure TForm1.Exercicio1_1;
const
  TOTAL_NOTAS = 5;
var
  Notas: array[0..4] of Double;
  I: Integer := 0;
  Soma: Double := 0.0;
  Media: Double := 0.0;
begin
  // Inicializar notas
  Notas[0] := 7.5;
  Notas[1] := 8.0;
  Notas[2] := 6.5;
  Notas[3] := 9.0;
  Notas[4] := 7.0;
  
  // Somar todas as notas
  for I := 0 to 4 do
    Soma := Soma + Notas[I];
  
  // Calcular média
  Media := Soma / TOTAL_NOTAS;
  
  ShowMessage('Soma das notas: ' + FormatFloat('0.00', Soma) + sLineBreak +
              'Média: ' + FormatFloat('0.00', Media));
end;
```

**Saída esperada:**
```
Soma das notas: 38.00
Média: 7.60
```

---

### **Exercício 1.2 - Constantes**
**Enunciado:** Calcule o salário anual de um funcionário usando constantes para o salário mensal e a taxa de desconto (INSS).

```pascal
procedure TForm1.Exercicio1_2;
const
  SALARIO_MENSAL = 3500.00;
  TAXA_INSS = 11.0; // Percentual
  MESES_ANO = 12;
var
  DescontoMensal: Double := 0.0;
  SalarioLiquidoMensal: Double := 0.0;
  SalarioAnual: Double := 0.0;
begin
  // Calcular desconto mensal
  DescontoMensal := SALARIO_MENSAL * (TAXA_INSS / 100);
  
  // Calcular salário líquido mensal
  SalarioLiquidoMensal := SALARIO_MENSAL - DescontoMensal;
  
  // Calcular salário anual
  SalarioAnual := SalarioLiquidoMensal * MESES_ANO;
  
  ShowMessage(
    'Salário Bruto Mensal: ' + FormatFloat('R$ #,##0.00', SALARIO_MENSAL) + sLineBreak +
    'Desconto INSS (' + FormatFloat('0.0', TAXA_INSS) + '%): ' + 
      FormatFloat('R$ #,##0.00', DescontoMensal) + sLineBreak +
    'Salário Líquido Mensal: ' + FormatFloat('R$ #,##0.00', SalarioLiquidoMensal) + sLineBreak +
    'Salário Anual Líquido: ' + FormatFloat('R$ #,##0.00', SalarioAnual)
  );
end;
```

**Saída esperada:**
```
Salário Bruto Mensal: R$ 3.500,00
Desconto INSS (11,0%): R$ 385,00
Salário Líquido Mensal: R$ 3.115,00
Salário Anual Líquido: R$ 37.380,00
```

---

### **Exercício 1.3 - IF/ELSE (Sistema de Classificação de Idade)**
**Enunciado:** Crie um sistema que classifique pessoas em categorias conforme a idade. Use 5 estruturas if/else.

```pascal
procedure TForm1.Exercicio1_3;
var
  Idade: Integer := 0;
  Categoria: string := '';
begin
  // Simulando entrada de idade
  Idade := 25;
  
  // Classificação com 5 estruturas if/else
  if Idade < 0 then
    Categoria := 'Idade inválida'
  else if Idade <= 12 then
    Categoria := 'Criança'
  else if Idade <= 17 then
    Categoria := 'Adolescente'
  else if Idade <= 59 then
    Categoria := 'Adulto'
  else if Idade >= 60 then
    Categoria := 'Idoso'
  else
    Categoria := 'Erro de processamento';
    
  ShowMessage('Idade: ' + IntToStr(Idade) + sLineBreak +
              'Categoria: ' + Categoria);
end;
```

**Saída esperada:**
```
Idade: 25
Categoria: Adulto
```

---

### **Exercício 1.4 - IF/ELSE (Calculadora de IMC com Classificação)**
**Enunciado:** Calcule o IMC (Índice de Massa Corporal) e classifique em 5 categorias usando if/else.

```pascal
procedure TForm1.Exercicio1_4;
var
  Peso: Double := 0.0;
  Altura: Double := 0.0;
  IMC: Double := 0.0;
  Classificacao: string := '';
begin
  // Dados de entrada
  Peso := 75.0; // kg
  Altura := 1.75; // metros
  
  // Calcular IMC
  IMC := Peso / (Altura * Altura);
  
  // Classificação com 5 estruturas if/else
  if IMC < 18.5 then
    Classificacao := 'Abaixo do peso'
  else if IMC < 25 then
    Classificacao := 'Peso normal'
  else if IMC < 30 then
    Classificacao := 'Sobrepeso'
  else if IMC < 35 then
    Classificacao := 'Obesidade Grau I'
  else if IMC >= 35 then
    Classificacao := 'Obesidade Grau II ou III'
  else
    Classificacao := 'Erro no cálculo';
    
  ShowMessage(
    'Peso: ' + FormatFloat('0.0', Peso) + ' kg' + sLineBreak +
    'Altura: ' + FormatFloat('0.00', Altura) + ' m' + sLineBreak +
    'IMC: ' + FormatFloat('0.00', IMC) + sLineBreak +
    'Classificação: ' + Classificacao
  );
end;
```

**Saída esperada:**
```
Peso: 75,0 kg
Altura: 1,75 m
IMC: 24,49
Classificação: Peso normal
```

---

### **Exercício 1.5 - IF/ELSE (Sistema de Notas com Conceitos)**
**Enunciado:** Dado um valor numérico de 0 a 100, atribua conceitos (A, B, C, D, F) usando 5 estruturas if/else.

```pascal
procedure TForm1.Exercicio1_5;
var
  Nota: Integer := 0;
  Conceito: string := '';
begin
  // Nota simulada
  Nota := 85;
  
  // Atribuição de conceito com 5 estruturas if/else
  if Nota < 0 then
    Conceito := 'Nota inválida (menor que zero)'
  else if Nota < 60 then
    Conceito := 'F - Reprovado'
  else if Nota < 70 then
    Conceito := 'D - Regular'
  else if Nota < 80 then
    Conceito := 'C - Bom'
  else if Nota < 90 then
    Conceito := 'B - Muito Bom'
  else if Nota <= 100 then
    Conceito := 'A - Excelente'
  else
    Conceito := 'Nota inválida (maior que 100)';
    
  ShowMessage('Nota: ' + IntToStr(Nota) + sLineBreak +
              'Conceito: ' + Conceito);
end;
```

**Saída esperada:**
```
Nota: 85
Conceito: B - Muito Bom
```

---

### **Exercício 1.6 - IF/ELSE (Cálculo de Desconto Progressivo)**
**Enunciado:** Calcule desconto progressivo conforme valor da compra. Use 5 estruturas if/else.

```pascal
procedure TForm1.Exercicio1_6;
var
  ValorCompra: Double := 0.0;
  PercentualDesconto: Double := 0.0;
  ValorDesconto: Double := 0.0;
  ValorFinal: Double := 0.0;
begin
  // Valor da compra
  ValorCompra := 350.00;
  
  // Cálculo de desconto progressivo com 5 estruturas if/else
  if ValorCompra < 100 then
    PercentualDesconto := 0.0
  else if ValorCompra < 200 then
    PercentualDesconto := 5.0
  else if ValorCompra < 300 then
    PercentualDesconto := 10.0
  else if ValorCompra < 500 then
    PercentualDesconto := 15.0
  else if ValorCompra >= 500 then
    PercentualDesconto := 20.0
  else
    PercentualDesconto := 0.0;
    
  // Calcular valores
  ValorDesconto := ValorCompra * (PercentualDesconto / 100);
  ValorFinal := ValorCompra - ValorDesconto;
  
  ShowMessage(
    'Valor da Compra: ' + FormatFloat('R$ #,##0.00', ValorCompra) + sLineBreak +
    'Desconto: ' + FormatFloat('0.0', PercentualDesconto) + '%' + sLineBreak +
    'Valor do Desconto: ' + FormatFloat('R$ #,##0.00', ValorDesconto) + sLineBreak +
    'Valor Final: ' + FormatFloat('R$ #,##0.00', ValorFinal)
  );
end;
```

**Saída esperada:**
```
Valor da Compra: R$ 350,00
Desconto: 15,0%
Valor do Desconto: R$ 52,50
Valor Final: R$ 297,50
```

---

### **Exercício 1.7 - IF/ELSE (Verificação de Triângulo)**
**Enunciado:** Dados 3 lados, verifique se formam um triângulo válido e classifique (Equilátero, Isósceles, Escaleno). Use 5 estruturas if/else.

```pascal
procedure TForm1.Exercicio1_7;
var
  Lado1, Lado2, Lado3: Double;
  Resultado: string := '';
begin
  // Dados dos lados
  Lado1 := 5.0;
  Lado2 := 5.0;
  Lado3 := 7.0;
  
  // Verificação e classificação com 5 estruturas if/else
  if (Lado1 <= 0) or (Lado2 <= 0) or (Lado3 <= 0) then
    Resultado := 'Valores inválidos (lados devem ser positivos)'
  else if (Lado1 + Lado2 <= Lado3) or (Lado1 + Lado3 <= Lado2) or (Lado2 + Lado3 <= Lado1) then
    Resultado := 'Não forma um triângulo válido'
  else if (Lado1 = Lado2) and (Lado2 = Lado3) then
    Resultado := 'Triângulo Equilátero (3 lados iguais)'
  else if (Lado1 = Lado2) or (Lado2 = Lado3) or (Lado1 = Lado3) then
    Resultado := 'Triângulo Isósceles (2 lados iguais)'
  else if (Lado1 <> Lado2) and (Lado2 <> Lado3) and (Lado1 <> Lado3) then
    Resultado := 'Triângulo Escaleno (todos os lados diferentes)'
  else
    Resultado := 'Erro na classificação';
    
  ShowMessage(
    'Lados: ' + FormatFloat('0.0', Lado1) + ', ' + 
                FormatFloat('0.0', Lado2) + ', ' + 
                FormatFloat('0.0', Lado3) + sLineBreak +
    'Resultado: ' + Resultado
  );
end;
```

**Saída esperada:**
```
Lados: 5,0, 5,0, 7,0
Resultado: Triângulo Isósceles (2 lados iguais)
```

---

### **Exercício 1.8 - Operadores de Comparação (=, <>, >, <, >=, <=)**
**Enunciado:** Compare dois números e mostre todos os resultados de comparação possíveis.

```pascal
procedure TForm1.Exercicio1_8;
var
  Numero1: Integer := 0;
  Numero2: Integer := 0;
  Resultado: string := '';
begin
  // Números para comparação
  Numero1 := 10;
  Numero2 := 20;
  
  // Testes de comparação
  Resultado := 'Comparando ' + IntToStr(Numero1) + ' e ' + IntToStr(Numero2) + ':' + sLineBreak + sLineBreak;
  
  // Igual (=)
  if Numero1 = Numero2 then
    Resultado := Resultado + '✅ ' + IntToStr(Numero1) + ' = ' + IntToStr(Numero2) + sLineBreak
  else
    Resultado := Resultado + '❌ ' + IntToStr(Numero1) + ' = ' + IntToStr(Numero2) + sLineBreak;
  
  // Diferente (<>)
  if Numero1 <> Numero2 then
    Resultado := Resultado + '✅ ' + IntToStr(Numero1) + ' <> ' + IntToStr(Numero2) + sLineBreak
  else
    Resultado := Resultado + '❌ ' + IntToStr(Numero1) + ' <> ' + IntToStr(Numero2) + sLineBreak;
  
  // Maior (>)
  if Numero1 > Numero2 then
    Resultado := Resultado + '✅ ' + IntToStr(Numero1) + ' > ' + IntToStr(Numero2) + sLineBreak
  else
    Resultado := Resultado + '❌ ' + IntToStr(Numero1) + ' > ' + IntToStr(Numero2) + sLineBreak;
  
  // Menor (<)
  if Numero1 < Numero2 then
    Resultado := Resultado + '✅ ' + IntToStr(Numero1) + ' < ' + IntToStr(Numero2) + sLineBreak
  else
    Resultado := Resultado + '❌ ' + IntToStr(Numero1) + ' < ' + IntToStr(Numero2) + sLineBreak;
  
  // Maior ou igual (>=)
  if Numero1 >= Numero2 then
    Resultado := Resultado + '✅ ' + IntToStr(Numero1) + ' >= ' + IntToStr(Numero2) + sLineBreak
  else
    Resultado := Resultado + '❌ ' + IntToStr(Numero1) + ' >= ' + IntToStr(Numero2) + sLineBreak;
  
  // Menor ou igual (<=)
  if Numero1 <= Numero2 then
    Resultado := Resultado + '✅ ' + IntToStr(Numero1) + ' <= ' + IntToStr(Numero2)
  else
    Resultado := Resultado + '❌ ' + IntToStr(Numero1) + ' <= ' + IntToStr(Numero2);
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
Comparando 10 e 20:

❌ 10 = 20
✅ 10 <> 20
❌ 10 > 20
✅ 10 < 20
❌ 10 >= 20
✅ 10 <= 20
```

---

### **Exercício 1.9 - Operadores Lógicos (OR, NOT)**
**Enunciado:** Verifique se um usuário pode acessar o sistema usando operadores lógicos `or` e `not`.

```pascal
procedure TForm1.Exercicio1_9;
var
  IsAdmin: Boolean := False;
  IsGerente: Boolean := True;
  IsBloqueado: Boolean := False;
  PodeAcessar: Boolean := False;
  Resultado: string := '';
begin
  // Lógica: Pode acessar se (Admin OU Gerente) E (NÃO Bloqueado)
  PodeAcessar := (IsAdmin or IsGerente) and (not IsBloqueado);
  
  Resultado := 'Status do Usuário:' + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Admin: ' + BoolToStr(IsAdmin, True) + sLineBreak;
  Resultado := Resultado + 'Gerente: ' + BoolToStr(IsGerente, True) + sLineBreak;
  Resultado := Resultado + 'Bloqueado: ' + BoolToStr(IsBloqueado, True) + sLineBreak + sLineBreak;
  
  if PodeAcessar then
    Resultado := Resultado + '✅ ACESSO PERMITIDO'
  else
    Resultado := Resultado + '❌ ACESSO NEGADO';
    
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
Status do Usuário:

Admin: False
Gerente: True
Bloqueado: False

✅ ACESSO PERMITIDO
```

---

### **Exercício 1.10 - Operadores Lógicos Complexos**
**Enunciado:** Valide se um produto pode ser vendido verificando múltiplas condições com operadores lógicos.

```pascal
procedure TForm1.Exercicio1_10;
var
  TemEstoque: Boolean := True;
  PrecoValido: Boolean := True;
  IsAtivo: Boolean := False;
  TemDesconto: Boolean := True;
  PodeVender: Boolean := False;
  Mensagem: string := '';
begin
  // Regra: Pode vender se (Tem Estoque E Preço Válido E Ativo) OU Tem Desconto Especial
  PodeVender := ((TemEstoque and PrecoValido and IsAtivo) or TemDesconto);
  
  Mensagem := 'Validação de Venda:' + sLineBreak + sLineBreak;
  Mensagem := Mensagem + 'Tem Estoque: ' + BoolToStr(TemEstoque, True) + sLineBreak;
  Mensagem := Mensagem + 'Preço Válido: ' + BoolToStr(PrecoValido, True) + sLineBreak;
  Mensagem := Mensagem + 'Produto Ativo: ' + BoolToStr(IsAtivo, True) + sLineBreak;
  Mensagem := Mensagem + 'Tem Desconto Especial: ' + BoolToStr(TemDesconto, True) + sLineBreak + sLineBreak;
  
  if PodeVender then
    Mensagem := Mensagem + '✅ PRODUTO DISPONÍVEL PARA VENDA'
  else
    Mensagem := Mensagem + '❌ PRODUTO INDISPONÍVEL';
    
  // Explicação lógica
  Mensagem := Mensagem + sLineBreak + sLineBreak + 'Lógica aplicada:' + sLineBreak;
  Mensagem := Mensagem + '(Estoque AND Preço AND Ativo) OR Desconto' + sLineBreak;
  Mensagem := Mensagem + '(' + BoolToStr(TemEstoque and PrecoValido and IsAtivo, True) + 
              ') OR ' + BoolToStr(TemDesconto, True) + ' = ' + BoolToStr(PodeVender, True);
  
  ShowMessage(Mensagem);
end;
```

**Saída esperada:**
```
Validação de Venda:

Tem Estoque: True
Preço Válido: True
Produto Ativo: False
Tem Desconto Especial: True

✅ PRODUTO DISPONÍVEL PARA VENDA

Lógica aplicada:
(Estoque AND Preço AND Ativo) OR Desconto
(False) OR True = True
```

---

### **Exercício 1.11 - Operadores Matemáticos (+, -, *, /)**
**Enunciado:** Crie uma calculadora básica que execute as 4 operações matemáticas fundamentais.

```pascal
procedure TForm1.Exercicio1_11;
var
  Numero1: Double := 0.0;
  Numero2: Double := 0.0;
  Soma, Subtracao, Multiplicacao, Divisao: Double;
  Resultado: string := '';
begin
  // Valores de entrada
  Numero1 := 20.0;
  Numero2 := 4.0;
  
  // Operações matemáticas
  Soma := Numero1 + Numero2;
  Subtracao := Numero1 - Numero2;
  Multiplicacao := Numero1 * Numero2;
  
  // Divisão com validação
  if Numero2 <> 0 then
    Divisao := Numero1 / Numero2
  else
    Divisao := 0.0;
  
  // Montar resultado
  Resultado := 'Calculadora - Operações Básicas' + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Número 1: ' + FormatFloat('0.0', Numero1) + sLineBreak;
  Resultado := Resultado + 'Número 2: ' + FormatFloat('0.0', Numero2) + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Soma (+): ' + FormatFloat('0.0', Soma) + sLineBreak;
  Resultado := Resultado + 'Subtração (-): ' + FormatFloat('0.0', Subtracao) + sLineBreak;
  Resultado := Resultado + 'Multiplicação (*): ' + FormatFloat('0.0', Multiplicacao) + sLineBreak;
  
  if Numero2 <> 0 then
    Resultado := Resultado + 'Divisão (/): ' + FormatFloat('0.00', Divisao)
  else
    Resultado := Resultado + 'Divisão (/): ERRO - Divisão por zero!';
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
Calculadora - Operações Básicas

Número 1: 20,0
Número 2: 4,0

Soma (+): 24,0
Subtração (-): 16,0
Multiplicação (*): 80,0
Divisão (/): 5,00
```

---

### **Exercício 1.12 - Operadores MOD e DIV**
**Enunciado:** Use os operadores `mod` (resto da divisão) e `div` (divisão inteira) para verificar se um número é par ou ímpar e decompor segundos em horas/minutos/segundos.

```pascal
procedure TForm1.Exercicio1_12;
var
  Numero: Integer := 0;
  TotalSegundos: Integer := 0;
  Horas, Minutos, Segundos: Integer;
  Resultado: string := '';
begin
  // Teste 1: Par ou Ímpar usando MOD
  Numero := 47;
  
  Resultado := '=== TESTE 1: PAR OU ÍMPAR ===' + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Número: ' + IntToStr(Numero) + sLineBreak;
  
  if Numero mod 2 = 0 then
    Resultado := Resultado + 'Resultado: PAR' + sLineBreak
  else
    Resultado := Resultado + 'Resultado: ÍMPAR' + sLineBreak;
    
  Resultado := Resultado + '(Resto da divisão por 2: ' + IntToStr(Numero mod 2) + ')' + sLineBreak + sLineBreak;
  
  // Teste 2: Converter segundos usando DIV e MOD
  TotalSegundos := 3725; // 1h 2min 5seg
  
  Horas := TotalSegundos div 3600;
  Minutos := (TotalSegundos mod 3600) div 60;
  Segundos := TotalSegundos mod 60;
  
  Resultado := Resultado + '=== TESTE 2: CONVERSÃO DE TEMPO ===' + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Total de segundos: ' + IntToStr(TotalSegundos) + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Convertido:' + sLineBreak;
  Resultado := Resultado + Format('%d horas, %d minutos, %d segundos', [Horas, Minutos, Segundos]);
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
=== TESTE 1: PAR OU ÍMPAR ===

Número: 47
Resultado: ÍMPAR
(Resto da divisão por 2: 1)

=== TESTE 2: CONVERSÃO DE TEMPO ===

Total de segundos: 3725

Convertido:
1 horas, 2 minutos, 5 segundos
```

---

### **Exercício 1.13 - Precedência de Operadores Matemáticos**
**Enunciado:** Demonstre a precedência de operadores matemáticos em diferentes expressões.

```pascal
procedure TForm1.Exercicio1_13;
var
  Resultado1, Resultado2, Resultado3, Resultado4: Double;
  Mensagem: string := '';
begin
  // Expressões com diferentes precedências
  Resultado1 := 10 + 5 * 2;           // Multiplicação primeiro: 10 + 10 = 20
  Resultado2 := (10 + 5) * 2;         // Parênteses primeiro: 15 * 2 = 30
  Resultado3 := 20 / 4 + 3 * 2;       // Divisão e multiplicação, depois soma: 5 + 6 = 11
  Resultado4 := 20 / (4 + 3) * 2;     // Parênteses, depois div/mult: 20 / 7 * 2 ≈ 5.71
  
  Mensagem := 'PRECEDÊNCIA DE OPERADORES MATEMÁTICOS' + sLineBreak + sLineBreak;
  
  Mensagem := Mensagem + 'Expressão 1: 10 + 5 * 2' + sLineBreak;
  Mensagem := Mensagem + 'Resultado: ' + FormatFloat('0.00', Resultado1) + sLineBreak;
  Mensagem := Mensagem + 'Explicação: Multiplicação primeiro (5*2=10), depois soma (10+10=20)' + sLineBreak + sLineBreak;
  
  Mensagem := Mensagem + 'Expressão 2: (10 + 5) * 2' + sLineBreak;
  Mensagem := Mensagem + 'Resultado: ' + FormatFloat('0.00', Resultado2) + sLineBreak;
  Mensagem := Mensagem + 'Explicação: Parênteses primeiro (10+5=15), depois multiplicação (15*2=30)' + sLineBreak + sLineBreak;
  
  Mensagem := Mensagem + 'Expressão 3: 20 / 4 + 3 * 2' + sLineBreak;
  Mensagem := Mensagem + 'Resultado: ' + FormatFloat('0.00', Resultado3) + sLineBreak;
  Mensagem := Mensagem + 'Explicação: Divisão e multiplicação (20/4=5 e 3*2=6), depois soma (5+6=11)' + sLineBreak + sLineBreak;
  
  Mensagem := Mensagem + 'Expressão 4: 20 / (4 + 3) * 2' + sLineBreak;
  Mensagem := Mensagem + 'Resultado: ' + FormatFloat('0.00', Resultado4) + sLineBreak;
  Mensagem := Mensagem + 'Explicação: Parênteses (4+3=7), depois divisão e multiplicação (20/7*2≈5.71)';
  
  ShowMessage(Mensagem);
end;
```

**Saída esperada:**
```
PRECEDÊNCIA DE OPERADORES MATEMÁTICOS

Expressão 1: 10 + 5 * 2
Resultado: 20,00
Explicação: Multiplicação primeiro (5*2=10), depois soma (10+10=20)

Expressão 2: (10 + 5) * 2
Resultado: 30,00
Explicação: Parênteses primeiro (10+5=15), depois multiplicação (15*2=30)

Expressão 3: 20 / 4 + 3 * 2
Resultado: 11,00
Explicação: Divisão e multiplicação (20/4=5 e 3*2=6), depois soma (5+6=11)

Expressão 4: 20 / (4 + 3) * 2
Resultado: 5,71
Explicação: Parênteses (4+3=7), depois divisão e multiplicação (20/7*2≈5.71)
```

---

## 🔁 **2. Laços de Repetição**

### **Exercício 2.1 - FOR (Tabuada)**
**Enunciado:** Gere a tabuada de um número usando laço `for`.

```pascal
procedure TForm1.Exercicio2_1;
var
  Numero: Integer := 0;
  I: Integer := 0;
  Resultado: string := '';
begin
  // Número para tabuada
  Numero := 7;
  
  Resultado := 'TABUADA DO ' + IntToStr(Numero) + sLineBreak + sLineBreak;
  
  // Gerar tabuada de 1 a 10
  for I := 1 to 10 do
  begin
    Resultado := Resultado + IntToStr(Numero) + ' x ' + IntToStr(I) + ' = ' + 
                 IntToStr(Numero * I) + sLineBreak;
  end;
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
TABUADA DO 7

7 x 1 = 7
7 x 2 = 14
7 x 3 = 21
7 x 4 = 28
7 x 5 = 35
7 x 6 = 42
7 x 7 = 49
7 x 8 = 56
7 x 9 = 63
7 x 10 = 70
```

---

### **Exercício 2.2 - FOR (Soma de Números Pares)**
**Enunciado:** Some todos os números pares de 1 a 100 usando laço `for`.

```pascal
procedure TForm1.Exercicio2_2;
var
  I: Integer := 0;
  Soma: Integer := 0;
  Contador: Integer := 0;
  Resultado: string := '';
begin
  // Percorrer de 1 a 100
  for I := 1 to 100 do
  begin
    // Verificar se é par
    if I mod 2 = 0 then
    begin
      Soma := Soma + I;
      Inc(Contador);
    end;
  end;
  
  Resultado := 'SOMA DE NÚMEROS PARES DE 1 A 100' + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Quantidade de números pares: ' + IntToStr(Contador) + sLineBreak;
  Resultado := Resultado + 'Soma total: ' + IntToStr(Soma);
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
SOMA DE NÚMEROS PARES DE 1 A 100

Quantidade de números pares: 50
Soma total: 2550
```

---

### **Exercício 2.3 - WHILE (Contagem Regressiva)**
**Enunciado:** Faça uma contagem regressiva de 10 até 0 usando `while`.

```pascal
procedure TForm1.Exercicio2_3;
var
  Contador: Integer := 10;
  Resultado: string := '';
begin
  Resultado := 'CONTAGEM REGRESSIVA' + sLineBreak + sLineBreak;
  
  // Contagem regressiva com while
  while Contador >= 0 do
  begin
    Resultado := Resultado + IntToStr(Contador);
    
    if Contador = 0 then
      Resultado := Resultado + ' 🚀 LANÇAMENTO!'
    else
      Resultado := Resultado + '...' + sLineBreak;
      
    Dec(Contador);
  end;
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
CONTAGEM REGRESSIVA

10...
9...
8...
7...
6...
5...
4...
3...
2...
1...
0 🚀 LANÇAMENTO!
```

---

### **Exercício 2.4 - WHILE (Validação de Senha)**
**Enunciado:** Simule um sistema de validação de senha que permite até 3 tentativas usando `while`.

```pascal
procedure TForm1.Exercicio2_4;
const
  SENHA_CORRETA = '1234';
  MAX_TENTATIVAS = 3;
var
  SenhaDigitada: string := '';
  Tentativas: Integer := 0;
  Resultado: string := '';
  Acesso: Boolean := False;
begin
  Resultado := 'SISTEMA DE VALIDAÇÃO DE SENHA' + sLineBreak + sLineBreak;
  
  // Simular 3 tentativas com senhas erradas, depois correta
  SenhaDigitada := '0000'; // 1ª tentativa (errada)
  
  while (Tentativas < MAX_TENTATIVAS) and (not Acesso) do
  begin
    Inc(Tentativas);
    
    Resultado := Resultado + 'Tentativa ' + IntToStr(Tentativas) + ': ';
    
    if SenhaDigitada = SENHA_CORRETA then
    begin
      Acesso := True;
      Resultado := Resultado + '✅ SENHA CORRETA!' + sLineBreak;
    end
    else
    begin
      Resultado := Resultado + '❌ Senha incorreta' + sLineBreak;
      
      // Simular próximas tentativas
      if Tentativas = 1 then
        SenhaDigitada := '5678' // 2ª tentativa (errada)
      else if Tentativas = 2 then
        SenhaDigitada := '1234'; // 3ª tentativa (correta)
    end;
  end;
  
  Resultado := Resultado + sLineBreak;
  
  if Acesso then
    Resultado := Resultado + '🔓 ACESSO LIBERADO'
  else
    Resultado := Resultado + '🔒 ACESSO BLOQUEADO - Número máximo de tentativas atingido';
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
SISTEMA DE VALIDAÇÃO DE SENHA

Tentativa 1: ❌ Senha incorreta
Tentativa 2: ❌ Senha incorreta
Tentativa 3: ✅ SENHA CORRETA!

🔓 ACESSO LIBERADO
```

---

### **Exercício 2.5 - REPEAT (Menu Interativo)**
**Enunciado:** Crie um menu que se repete até o usuário escolher a opção "Sair" usando `repeat`.

```pascal
procedure TForm1.Exercicio2_5;
var
  Opcao: Integer := 0;
  Contador: Integer := 0;
  Resultado: string := '';
  Continuar: Boolean := True;
begin
  Resultado := 'SIMULAÇÃO DE MENU INTERATIVO' + sLineBreak + sLineBreak;
  
  // Simular escolhas: 1, 2, 3, 0 (sair)
  repeat
    Inc(Contador);
    
    // Simular opções escolhidas
    case Contador of
      1: Opcao := 1;
      2: Opcao := 2;
      3: Opcao := 3;
      4: Opcao := 0;
    end;
    
    Resultado := Resultado + '--- Iteração ' + IntToStr(Contador) + ' ---' + sLineBreak;
    Resultado := Resultado + 'Opção escolhida: ' + IntToStr(Opcao) + sLineBreak;
    
    case Opcao of
      1: Resultado := Resultado + 'Ação: Cadastrar Cliente' + sLineBreak;
      2: Resultado := Resultado + 'Ação: Listar Clientes' + sLineBreak;
      3: Resultado := Resultado + 'Ação: Excluir Cliente' + sLineBreak;
      0: begin
           Resultado := Resultado + 'Ação: Sair do sistema' + sLineBreak;
           Continuar := False;
         end;
    end;
    
    Resultado := Resultado + sLineBreak;
    
  until not Continuar;
  
  Resultado := Resultado + '👋 Sistema encerrado!';
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
SIMULAÇÃO DE MENU INTERATIVO

--- Iteração 1 ---
Opção escolhida: 1
Ação: Cadastrar Cliente

--- Iteração 2 ---
Opção escolhida: 2
Ação: Listar Clientes

--- Iteração 3 ---
Opção escolhida: 3
Ação: Excluir Cliente

--- Iteração 4 ---
Opção escolhida: 0
Ação: Sair do sistema

👋 Sistema encerrado!
```

---

### **Exercício 2.6 - REPEAT (Soma até Atingir Meta)**
**Enunciado:** Some números consecutivos até atingir ou ultrapassar uma meta usando `repeat`.

```pascal
procedure TForm1.Exercicio2_6;
const
  META = 100;
var
  Numero: Integer := 1;
  Soma: Integer := 0;
  Resultado: string := '';
begin
  Resultado := 'SOMA ATÉ ATINGIR META DE ' + IntToStr(META) + sLineBreak + sLineBreak;
  
  repeat
    Soma := Soma + Numero;
    Resultado := Resultado + 'Número: ' + IntToStr(Numero) + ' | Soma acumulada: ' + IntToStr(Soma) + sLineBreak;
    Inc(Numero);
  until Soma >= META;
  
  Resultado := Resultado + sLineBreak + '✅ Meta atingida!' + sLineBreak;
  Resultado := Resultado + 'Total de números somados: ' + IntToStr(Numero - 1) + sLineBreak;
  Resultado := Resultado + 'Soma final: ' + IntToStr(Soma);
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
SOMA ATÉ ATINGIR META DE 100

Número: 1 | Soma acumulada: 1
Número: 2 | Soma acumulada: 3
Número: 3 | Soma acumulada: 6
Número: 4 | Soma acumulada: 10
Número: 5 | Soma acumulada: 15
Número: 6 | Soma acumulada: 21
Número: 7 | Soma acumulada: 28
Número: 8 | Soma acumulada: 36
Número: 9 | Soma acumulada: 45
Número: 10 | Soma acumulada: 55
Número: 11 | Soma acumulada: 66
Número: 12 | Soma acumulada: 78
Número: 13 | Soma acumulada: 91
Número: 14 | Soma acumulada: 105

✅ Meta atingida!
Total de números somados: 14
Soma final: 105
```

---

### **Exercício 2.7 - BREAK (Busca em Array)**
**Enunciado:** Busque um valor específico em um array e interrompa o laço ao encontrar usando `break`.

```pascal
procedure TForm1.Exercicio2_7;
var
  Numeros: array[0..9] of Integer;
  I: Integer := 0;
  Buscar: Integer := 0;
  Posicao: Integer := -1;
  Resultado: string := '';
begin
  // Inicializar array
  Numeros[0] := 10;
  Numeros[1] := 25;
  Numeros[2] := 33;
  Numeros[3] := 47;
  Numeros[4] := 52;
  Numeros[5] := 68;
  Numeros[6] := 71;
  Numeros[7] := 89;
  Numeros[8] := 94;
  Numeros[9] := 100;
  
  // Valor a buscar
  Buscar := 52;
  
  Resultado := 'BUSCA EM ARRAY COM BREAK' + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Array: [10, 25, 33, 47, 52, 68, 71, 89, 94, 100]' + sLineBreak;
  Resultado := Resultado + 'Buscando: ' + IntToStr(Buscar) + sLineBreak + sLineBreak;
  
  // Buscar com break
  for I := 0 to 9 do
  begin
    Resultado := Resultado + 'Verificando posição ' + IntToStr(I) + ': ' + IntToStr(Numeros[I]);
    
    if Numeros[I] = Buscar then
    begin
      Posicao := I;
      Resultado := Resultado + ' ✅ ENCONTRADO!' + sLineBreak;
      Break; // Interrompe o laço
    end
    else
      Resultado := Resultado + sLineBreak;
  end;
  
  Resultado := Resultado + sLineBreak;
  
  if Posicao >= 0 then
    Resultado := Resultado + Format('Valor %d encontrado na posição %d', [Buscar, Posicao])
  else
    Resultado := Resultado + 'Valor não encontrado';
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
BUSCA EM ARRAY COM BREAK

Array: [10, 25, 33, 47, 52, 68, 71, 89, 94, 100]
Buscando: 52

Verificando posição 0: 10
Verificando posição 1: 25
Verificando posição 2: 33
Verificando posição 3: 47
Verificando posição 4: 52 ✅ ENCONTRADO!

Valor 52 encontrado na posição 4
```

---

## 🧷 **3. Estruturas de Dados**

### **Exercício 3.1 - Matriz 3x3 (Preenchimento e Exibição)**
**Enunciado:** Crie uma matriz 3x3, preencha com valores e exiba em formato tabular.

```pascal
procedure TForm1.Exercicio3_1;
var
  Matriz: array[0..2, 0..2] of Integer;
  I, J: Integer;
  Valor: Integer := 1;
  Resultado: string := '';
begin
  // Preencher matriz
  for I := 0 to 2 do
  begin
    for J := 0 to 2 do
    begin
      Matriz[I, J] := Valor;
      Inc(Valor);
    end;
  end;
  
  // Exibir matriz
  Resultado := 'MATRIZ 3x3' + sLineBreak + sLineBreak;
  
  for I := 0 to 2 do
  begin
    for J := 0 to 2 do
    begin
      Resultado := Resultado + Format('%3d', [Matriz[I, J]]);
      if J < 2 then
        Resultado := Resultado + ' ';
    end;
    Resultado := Resultado + sLineBreak;
  end;
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
MATRIZ 3x3

  1   2   3
  4   5   6
  7   8   9
```

---

### **Exercício 3.2 - Matriz (Soma de Linhas e Colunas)**
**Enunciado:** Dada uma matriz 3x3, calcule a soma de cada linha e cada coluna.

```pascal
procedure TForm1.Exercicio3_2;
var
  Matriz: array[0..2, 0..2] of Integer;
  SomaLinhas: array[0..2] of Integer;
  SomaColunas: array[0..2] of Integer;
  I, J: Integer;
  Resultado: string := '';
begin
  // Inicializar matriz
  Matriz[0, 0] := 1;  Matriz[0, 1] := 2;  Matriz[0, 2] := 3;
  Matriz[1, 0] := 4;  Matriz[1, 1] := 5;  Matriz[1, 2] := 6;
  Matriz[2, 0] := 7;  Matriz[2, 1] := 8;  Matriz[2, 2] := 9;
  
  // Zerar somas
  for I := 0 to 2 do
  begin
    SomaLinhas[I] := 0;
    SomaColunas[I] := 0;
  end;
  
  // Calcular somas
  for I := 0 to 2 do
  begin
    for J := 0 to 2 do
    begin
      SomaLinhas[I] := SomaLinhas[I] + Matriz[I, J];
      SomaColunas[J] := SomaColunas[J] + Matriz[I, J];
    end;
  end;
  
  // Montar resultado
  Resultado := 'SOMA DE LINHAS E COLUNAS' + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Matriz original:' + sLineBreak;
  
  for I := 0 to 2 do
  begin
    for J := 0 to 2 do
      Resultado := Resultado + Format('%3d', [Matriz[I, J]]);
    Resultado := Resultado + sLineBreak;
  end;
  
  Resultado := Resultado + sLineBreak + 'Soma das linhas:' + sLineBreak;
  for I := 0 to 2 do
    Resultado := Resultado + Format('Linha %d: %d', [I, SomaLinhas[I]]) + sLineBreak;
  
  Resultado := Resultado + sLineBreak + 'Soma das colunas:' + sLineBreak;
  for J := 0 to 2 do
    Resultado := Resultado + Format('Coluna %d: %d', [J, SomaColunas[J]]) + sLineBreak;
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
SOMA DE LINHAS E COLUNAS

Matriz original:
  1  2  3
  4  5  6
  7  8  9

Soma das linhas:
Linha 0: 6
Linha 1: 15
Linha 2: 24

Soma das colunas:
Coluna 0: 12
Coluna 1: 15
Coluna 2: 18
```

---

### **Exercício 3.3 - Matriz (Diagonal Principal)**
**Enunciado:** Calcule a soma dos elementos da diagonal principal de uma matriz 4x4.

```pascal
procedure TForm1.Exercicio3_3;
var
  Matriz: array[0..3, 0..3] of Integer;
  I, J: Integer;
  SomaDiagonal: Integer := 0;
  Resultado: string := '';
begin
  // Preencher matriz com valores sequenciais
  for I := 0 to 3 do
    for J := 0 to 3 do
      Matriz[I, J] := (I * 4) + J + 1;
  
  // Exibir matriz
  Resultado := 'DIAGONAL PRINCIPAL - MATRIZ 4x4' + sLineBreak + sLineBreak;
  Resultado := Resultado + 'Matriz:' + sLineBreak;
  
  for I := 0 to 3 do
  begin
    for J := 0 to 3 do
    begin
      Resultado := Resultado + Format('%4d', [Matriz[I, J]]);
      if J < 3 then
        Resultado := Resultado + ' ';
    end;
    Resultado := Resultado + sLineBreak;
  end;
  
  // Calcular soma da diagonal principal (onde I = J)
  Resultado := Resultado + sLineBreak + 'Diagonal principal: ';
  for I := 0 to 3 do
  begin
    SomaDiagonal := SomaDiagonal + Matriz[I, I];
    Resultado := Resultado + IntToStr(Matriz[I, I]);
    if I < 3 then
      Resultado := Resultado + ' + ';
  end;
  
  Resultado := Resultado + sLineBreak + 'Soma: ' + IntToStr(SomaDiagonal);
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
DIAGONAL PRINCIPAL - MATRIZ 4x4

Matriz:
   1    2    3    4
   5    6    7    8
   9   10   11   12
  13   14   15   16

Diagonal principal: 1 + 6 + 11 + 16
Soma: 34
```

---

### **Exercício 3.4 - Matriz (Transposição)**
**Enunciado:** Crie uma matriz 3x2 e gere sua transposta (2x3).

```pascal
procedure TForm1.Exercicio3_4;
var
  MatrizOriginal: array[0..2, 0..1] of Integer;
  MatrizTransposta: array[0..1, 0..2] of Integer;
  I, J: Integer;
  Resultado: string := '';
begin
  // Preencher matriz original 3x2
  MatrizOriginal[0, 0] := 1;  MatrizOriginal[0, 1] := 2;
  MatrizOriginal[1, 0] := 3;  MatrizOriginal[1, 1] := 4;
  MatrizOriginal[2, 0] := 5;  MatrizOriginal[2, 1] := 6;
  
  // Realizar transposição (trocar linhas por colunas)
  for I := 0 to 2 do
    for J := 0 to 1 do
      MatrizTransposta[J, I] := MatrizOriginal[I, J];
  
  // Exibir resultados
  Resultado := 'TRANSPOSIÇÃO DE MATRIZ' + sLineBreak + sLineBreak;
  
  Resultado := Resultado + 'Matriz Original (3x2):' + sLineBreak;
  for I := 0 to 2 do
  begin
    for J := 0 to 1 do
      Resultado := Resultado + Format('%3d', [MatrizOriginal[I, J]]);
    Resultado := Resultado + sLineBreak;
  end;
  
  Resultado := Resultado + sLineBreak + 'Matriz Transposta (2x3):' + sLineBreak;
  for I := 0 to 1 do
  begin
    for J := 0 to 2 do
      Resultado := Resultado + Format('%3d', [MatrizTransposta[I, J]]);
    Resultado := Resultado + sLineBreak;
  end;
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
TRANSPOSIÇÃO DE MATRIZ

Matriz Original (3x2):
  1  2
  3  4
  5  6

Matriz Transposta (2x3):
  1  3  5
  2  4  6
```

---

### **Exercício 3.5 - Matriz (Multiplicação por Escalar)**
**Enunciado:** Multiplique todos os elementos de uma matriz 2x2 por um valor escalar.

```pascal
procedure TForm1.Exercicio3_5;
const
  ESCALAR = 3;
var
  Matriz: array[0..1, 0..1] of Integer;
  MatrizResultado: array[0..1, 0..1] of Integer;
  I, J: Integer;
  Resultado: string := '';
begin
  // Inicializar matriz
  Matriz[0, 0] := 2;  Matriz[0, 1] := 4;
  Matriz[1, 0] := 6;  Matriz[1, 1] := 8;
  
  // Multiplicar por escalar
  for I := 0 to 1 do
    for J := 0 to 1 do
      MatrizResultado[I, J] := Matriz[I, J] * ESCALAR;
  
  // Exibir resultados
  Resultado := 'MULTIPLICAÇÃO POR ESCALAR' + sLineBreak + sLineBreak;
  
  Resultado := Resultado + 'Matriz Original:' + sLineBreak;
  for I := 0 to 1 do
  begin
    for J := 0 to 1 do
      Resultado := Resultado + Format('%3d', [Matriz[I, J]]);
    Resultado := Resultado + sLineBreak;
  end;
  
  Resultado := Resultado + sLineBreak + Format('Escalar: %d', [ESCALAR]) + sLineBreak + sLineBreak;
  
  Resultado := Resultado + 'Matriz Resultado:' + sLineBreak;
  for I := 0 to 1 do
  begin
    for J := 0 to 1 do
      Resultado := Resultado + Format('%3d', [MatrizResultado[I, J]]);
    Resultado := Resultado + sLineBreak;
  end;
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
MULTIPLICAÇÃO POR ESCALAR

Matriz Original:
  2  4
  6  8

Escalar: 3

Matriz Resultado:
  6 12
 18 24
```

---

### **Exercício 3.6 - Array (Ordenação Simples - Bubble Sort)**
**Enunciado:** Implemente o algoritmo Bubble Sort para ordenar um array de 5 números.

```pascal
procedure TForm1.Exercicio3_6;
var
  Numeros: array[0..4] of Integer;
  I, J, Temp: Integer;
  Trocou: Boolean;
  Resultado: string := '';
begin
  // Inicializar array desordenado
  Numeros[0] := 64;
  Numeros[1] := 34;
  Numeros[2] := 25;
  Numeros[3] := 12;
  Numeros[4] := 22;
  
  Resultado := 'ORDENAÇÃO - BUBBLE SORT' + sLineBreak + sLineBreak;
  
  // Array original
  Resultado := Resultado + 'Array original: ';
  for I := 0 to 4 do
  begin
    Resultado := Resultado + IntToStr(Numeros[I]);
    if I < 4 then
      Resultado := Resultado + ', ';
  end;
  Resultado := Resultado + sLineBreak + sLineBreak;
  
  // Algoritmo Bubble Sort
  for I := 0 to 3 do
  begin
    Trocou := False;
    for J := 0 to 3 - I do
    begin
      if Numeros[J] > Numeros[J + 1] then
      begin
        // Trocar elementos
        Temp := Numeros[J];
        Numeros[J] := Numeros[J + 1];
        Numeros[J + 1] := Temp;
        Trocou := True;
      end;
    end;
    
    // Se não houve troca, já está ordenado
    if not Trocou then
      Break;
  end;
  
  // Array ordenado
  Resultado := Resultado + 'Array ordenado: ';
  for I := 0 to 4 do
  begin
    Resultado := Resultado + IntToStr(Numeros[I]);
    if I < 4 then
      Resultado := Resultado + ', ';
  end;
  
  ShowMessage(Resultado);
end;
```

**Saída esperada:**
```
ORDENAÇÃO - BUBBLE SORT

Array original: 64, 34, 25, 12, 22

Array ordenado: 12, 22, 25, 34, 64
```

---

### **Exercício 3.7 - Array (Busca de Maior e Menor)**
**Enunciado:** Encontre o maior e o menor valor em um array de 8 elementos.

```pascal
procedure TForm1.Exercicio3_7;
var
  Numeros: array[0..7] of Integer;
  I: Integer;
  Maior, Menor: Integer;
  PosicaoMaior, PosicaoMenor: Integer;
  Resultado: string := '';
begin
  // Inicializar array
  Numeros[0] := 45;
  Numeros[1] := 23;
  Numeros[2] := 89;
  Numeros[3] := 12;
  Numeros[4] := 67;
  Numeros[5] := 34;
  Numeros[6] := 91;
  Numeros[7] := 56;
  
  // Inicializar maior e menor com primeiro elemento
  Maior := Numeros[0];
  Menor := Numeros[0];
  PosicaoMaior := 0;
  PosicaoMenor := 0;
  
  // Buscar maior e menor
  for I := 1 to 7 do
  begin
    if Numeros[I] > Maior then
    begin
      Maior := Numeros[I];
      PosicaoMaior := I;
    end;
    
    if Numeros[I] < Menor then
    begin
      Menor := Numeros[I];
      PosicaoMenor := I;
    end;
  end;
  
  // Exibir resultado
  Resultado := 'BUSCA DE MAIOR E MENOR VALOR' + sLineBreak + sLineBreak;
  
  Resultado := Resultado + 'Array: ';
  for I := 0 to 7 do
  begin
    Resultado := Resultado + IntToStr(Numeros[I]);
    if I < 7 then
      Resultado := Resultado + ', ';
