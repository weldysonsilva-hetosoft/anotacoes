# 📘 Exercicios Delphi para pratica diarias sem precisar consultar ia;

🧩 **Exercício: Cadastro Simples de Produto**
Enunciado:
Crie um programa que peça ao usuário:
- Nome do produto (String)
- Preço unitário (Double)
- Quantidade (Integer)
Calcule o valor total da compra e exiba com ShowMessage.
Dicas:
- Use InputBox para cada entrada.
- Multiplique preço * quantidade para obter o total.
- Use FloatToStr para exibir o valor com casas decimais.
```pascal
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
nome : String;
Preco, total : Double;
Quantidade : Integer; 
begin
  Write('Digite o nome do produto: ');
  readln(nome);
  Write('Digite o preço do produto: ');
  readln(Preco);
  Write('Digite a quantidade de produto que você quer: ');
  readln(quantidade);
  
  total := preco * quantidade;

  Writeln('A quantidade total da sua compra é de: ', total:0:2);


  ReadLn;

end.
```

🧩 **Exercício : Verificador de Maioridade**
Enunciado:
Solicite o nome e a idade de uma pessoa.
Verifique se ela é maior de idade (18 anos ou mais) e exiba uma mensagem personalizada com o nome.
Dicas:
- Use String para o nome e Integer para a idade.
- Use if para verificar a condição.
- Exiba mensagens diferentes para maior e menor de idade.

```pascal
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
    nome: string;
  idade: Integer;

begin
  Write('Digite seu nome: ');
  Readln(nome);
  Write('Digite sua idade: ');
  Readln(idade);

  if idade >= 18 then
    Writeln('Você é maior de idade!')
  else
    Writeln('Voce é menor de idade!');

  Readln;

end.

```


🧩 **Exercício 6: Soma de Três Números**
Enunciado:
Peça ao usuário três números inteiros.
Calcule a soma e a média deles.
Mostre os resultados com ShowMessage.
Dicas:
- Use três variáveis Integer para os números.
- A média pode ser calculada como soma / 3.0 (use Double).
- Converta os valores para string antes de exibir.

```pascal
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
    num1, num2, num3: Integer;
  media: double;

begin
  Write('Escreva o primeiro: ');
  ReadLn(num1);
  Write('Escreva o segundo: ');
  ReadLn(num2);
  Write('Escreva o terceiro: ');
  ReadLn(num3);

  media := (num1 + num2 + num3) / 3;
  Write('A media da soma dos 3 numeros é: ', media:0:2);
  ReadLn;

end.
```
  
🧪 **Exercício 7: Conversor de Moeda**
Enunciado:
Crie um programa que peça ao usuário um valor em reais (R$) e o converta para dólares (US$), usando uma taxa de câmbio fixa.
Dicas:
- Use Double para valores monetários.
- Defina a taxa de câmbio como uma constante.
- Exiba o resultado com duas casas decimais.

```pascal

  program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
    TAXA_REAL_DOLAR = 5.20;

var
    reais, resultado: double;

begin
  Write('Digite o valor para conversão em R$: ');
  ReadLn(reais);

  resultado := reais / TAXA_REAL_DOLAR;
  Writeln('R$ ' + Format('%.2f', [reais]) + ' = US$ ' + Format('%.2f', [resultado]));

  ReadLn;

end.
```


  

🧪 **Exercício 8: Calculadora de Área de Retângulo**
Enunciado:
Solicite ao usuário a largura e a altura de um retângulo.
Calcule e exiba a área.
Dicas:
- Use Double para medidas.
- A fórmula é área = largura * altura.
- Mostre o resultado com Writeln.
  
```pascal
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
    area, altura, largura: double;

begin
  Write('Digite a altura do Retangulo: ');
  ReadLn(altura);
  Write('Digite a largura do Retangulo: ');
  ReadLn(largura);

  area := largura * altura;

  Writeln('A area do retangulo é: ', area:0:2);
  ReadLn;

end.
```

   

🧪 ** Exercício 9: Verificador de Nota**
Enunciado:
Peça ao usuário uma nota de 0 a 10.
Verifique se o aluno foi aprovado (nota ≥ 7), em recuperação (nota entre 5 e 6.9), ou reprovado (nota < 5).
Dicas:
- Use Double para a nota.
- Utilize if...else if...else para as três faixas.
- Exiba uma mensagem correspondente à situação.

  
```pascal
  program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
    nota: double;

begin
  Write('Digite a nota do Aluno: ');
  ReadLn(nota);

  if (nota >= 0) and (nota <= 10) then

  begin
    if (nota >= 7) then
      Writeln('Aprovado')
    else if (nota > 5) then
      Writeln('Recuperação')
    else
      Writeln('Reprovado');

    ReadLn;
  end;

end.
  ```

🧠 **Exercício 1: Classificador de Faixa Etária**
Enunciado:
Peça a idade do usuário e classifique em:
- Criança (0–12)
- Adolescente (13–17)
- Adulto (18–59)
- Idoso (60+)
Conceitos: if/else, operadores relacionais, validação básica.
Dica: Verifique se a idade é válida (≥ 0) antes de classificar.
```pascal
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  idade: Integer;

begin
  Write('Digite a sua idade: ');
  ReadLn(idade);

  if (idade >= 0) then
  begin
    if (idade > 60) then
      Writeln('Idoso')
    else if (idade > 18) then
      Writeln('Adulto')
    else if (idade > 12) then
      Writeln('Adolescente')
    else
      Writeln('Criança');
  end
  else
    Writeln('Idade inválida!');

  ReadLn;
end.

```

🧠 **Exercício 2: Calculadora de Operações**
Enunciado:
Solicite dois números e uma operação (+, -, *, /).
Use case of para realizar a operação e mostrar o resultado.
Conceitos: case of, operadores relacionais, validação de divisão por zero.
Dica: Use Char ou String para representar a operação.

```pascal
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  opcao: Integer;
  num1, num2, resultado: Double;

begin
  Writeln('=== CALCULADORA DE OPERAÇÕES ===');
  Writeln('1 - SOMA');
  Writeln('2 - SUBTRAÇÃO');
  Writeln('3 - DIVISÃO');
  Writeln('4 - MULTIPLICAÇÃO');
  Write('Escolha uma opção (1-4): ');
  ReadLn(opcao);
  Write('Digite o primeiro número: ');
  ReadLn(num1);
  Write('Digite o segundo número: ');
  ReadLn(num2);

  case opcao of
    1: resultado := num1 + num2;
    2: resultado := num1 - num2;
    3:
      begin
        if num2 = 0 then
          Writeln('Erro: divisão por zero!')
        else
          resultado := num1 / num2;
      end;
    4: resultado := num1 * num2;
  else
    Writeln('Opção inválida!');
  end;

  if (opcao in [1, 2, 4]) or ((opcao = 3) and (num2 <> 0)) then
    Writeln('Resultado: ', resultado:0:2);

  ReadLn;
end.
```

🧠 **Exercício 3: Verificador de Acesso**
Enunciado:
Peça ao usuário:
- Nome de usuário
- Senha
Verifique se ambos correspondem aos valores esperados (ex: "admin" e "1234").
Conceitos: if, operadores lógicos (AND), validação básica.
Dica: Use String e compare com =.

```pascal
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
  USUARIO_PADRAO = 'admin';
  SENHA_PADRAO = '1234';

var
  nome, senha: String;

begin
  Write('Digite o nome do usuário: ');
  ReadLn(nome);
  Write('Digite a senha do usuário: ');
  ReadLn(senha);

  if (nome = USUARIO_PADRAO) and (senha = SENHA_PADRAO) then
    Writeln('Senha correta! Usuário logado.')
  else
    Writeln('Senha ou usuário incorreto!');

  ReadLn;
end.
```

🧠 **Exercício 4: Avaliação de Desempenho**
Enunciado:
Peça três notas e calcule a média.
Use decisão aninhada para classificar:
- Excelente (≥ 9)
- Bom (≥ 7 e < 9)
- Regular (≥ 5 e < 7)
- Insuficiente (< 5)
Conceitos: if/else if, operadores relacionais, decisão aninhada.
Dica: Use Double para média e Format para exibir com 2 casas decimais.

```sql
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  nota1, nota2, nota3, media: Double;

begin
  Write('Digite a primeira nota: ');
  ReadLn(nota1);
  Write('Digite a segunda nota: ');
  ReadLn(nota2);
  Write('Digite a terceira nota: ');
  ReadLn(nota3);

  if (nota1 < 0) or (nota2 < 0) or (nota3 < 0) then
    Writeln('Notas inválidas!')
  else
  begin
    media := (nota1 + nota2 + nota3) / 3;

    if (media >= 9) then
      Writeln('Excelente')
    else if (media >= 7) then
      Writeln('Bom')
    else if (media >= 5) then
      Writeln('Regular')
    else
      Writeln('Insuficiente');

    Writeln('A média do aluno é: ', media:0:2, '!');
  end;

  ReadLn;
end.
```

🧠 **Exercício 5: Verificador de Número Válido**
Enunciado:
Peça um número inteiro.
Verifique se ele está entre 10 e 100 ou se é negativo.
Exiba mensagens diferentes para cada caso.
Conceitos: if, operadores lógicos (OR, NOT), validação.
Dica: Use if...else com OR e NOT para testar as condições.

```pascal
program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  numero: Integer;

begin
  Write('Digite um número: ');
  ReadLn(numero);

  if (numero < 0) or (numero > 100) then
    Writeln('Número fora do intervalo permitido.')
  else
    Writeln('Número dentro do intervalo permitido.');

  ReadLn;
end.
```

🧠  Verificador de Triângulo
Enunciado:
Peça ao usuário três valores inteiros que representam os lados de um triângulo.
Verifique se os lados formam um triângulo válido (a soma de dois lados deve ser maior que o terceiro).
Se for válido, classifique como:
- Equilátero (todos iguais)
- Isósceles (dois iguais)
- Escaleno (todos diferentes)
Dicas:
- Use if...else if...else com operadores relacionais.
- Primeiro verifique se é um triângulo válido antes de classificar.

```pascal
  program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  lado1, lado2, lado3: Integer;

begin
  Writeln('=== VERIFICADOR DE TRIÂNGULO ===');
  Write('Digite o primeiro lado: ');
  ReadLn(lado1);
  Write('Digite o segundo lado: ');
  ReadLn(lado2);
  Write('Digite o terceiro lado: ');
  ReadLn(lado3);

  if (lado1 + lado2 > lado3) and
     (lado1 + lado3 > lado2) and
     (lado2 + lado3 > lado1) then
  begin
    if (lado1 = lado2) and (lado2 = lado3) then
      Writeln('O triângulo é Equilátero!')
    else if ((lado1 = lado2) or (lado1 = lado3) or (lado2 = lado3)) then
      Writeln('O triângulo é Isósceles!')
    else
      Writeln('O triângulo é Escaleno!');
  end
  else
    Writeln('Os lados informados não formam um triângulo válido.');

  ReadLn;
end.
```
 



🧠 Classificador de Letra
Enunciado:
Peça ao usuário uma letra (char).
Verifique se é:
- Vogal
- Consoante
- Dígito numérico
- Outro símbolo
Dicas:
- Use case of ou if com operadores lógicos (OR) para comparar.
- Pode usar UpCase para facilitar a verificação de vogais.  
```pascal
    program Exercicio;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  caractere: Char;

begin
  Write('Digite um caractere: ');
  ReadLn(caractere);
  caractere := UpCase(caractere);

  if (caractere in ['A', 'E', 'I', 'O', 'U']) then
    Writeln('É uma vogal.')
  else if (caractere in ['B'..'Z']) then
    Writeln('É uma consoante.')
  else if (caractere in ['0'..'9']) then
    Writeln('É um dígito numérico.')
  else
    Writeln('É outro símbolo.');

  ReadLn;
end.
```

🔁 1. Contador de 1 a 10 com FOR
Enunciado:
Imprima os números de 1 a 10 usando um laço FOR.
```pascal
program ContadorFor;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
    i: Integer;

begin
  Writeln('=== CONTADOR DE 1 A 10 COM FOR ===');


  for i := 1 to 10 do
  begin
    Writeln('Número: ', i);
  end;

  Writeln;
  Writeln('Fim da contagem!');
  Readln;

end.
```

🔁 2. Soma de Números com WHILE
Enunciado:
Peça números ao usuário até que ele digite 0.
Some todos os valores digitados e exiba o total.
Conceito: WHILE, acumulador.
```pascal
program SomaNumerosWhile;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  entrada: string;
  numero, soma, quantidade: Integer;

begin
  soma := 0;
  quantidade := 0;

  Writeln('=== Soma de Números com WHILE ===');
  Write('Digite um número ou "0" para encerrar: ');
  ReadLn(entrada);

  while entrada <> '0' do
  begin
    if TryStrToInt(entrada, numero) then
    begin
      soma := soma + numero;
      quantidade := quantidade + 1;
    end
    else
      Writeln('Entrada inválida. Digite um número inteiro.');

    Write('Digite um número ou "0" para encerrar: ');
    ReadLn(entrada);
  end;

  Writeln('Soma dos números: ', soma);
  Writeln('Quantidade de números digitados: ', quantidade);
  ReadLn;
end.

```

🔁 3. Tabuada com REPEAT-UNTIL
Enunciado:
Peça um número e exiba sua tabuada de 1 a 10 usando REPEAT-UNTIL.

```pascal
program TabuadaRepeat;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  n, i: Integer;

begin
  Write('Digite um número para ver a tabuada: ');
  ReadLn(n);

  i := 1;
  repeat
    Writeln(n, ' x ', i, ' = ', n * i);
    i := i + 1;
  until i > 10;

  ReadLn;
end.
```

🔁 4. Verificador de Par ou Ímpar
Enunciado:
Peça 10 números e diga se cada um é par ou ímpar.
Conceito: FOR, operador mod.

```pascal
program ParOuImpar;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  i, numero: Integer;

begin
  for i := 1 to 10 do
  begin
    Write('Digite um número: ');
    ReadLn(numero);

    if numero mod 2 = 0 then
      Writeln('Par')
    else
      Writeln('Ímpar');
  end;

  ReadLn;
end.
```
🔁 5. Média de Alunos com WHILE
Enunciado:
Peça a quantidade de alunos.
Para cada aluno, peça a nota e calcule a média geral.
Conceito: WHILE, contador, acumulador.
```pascal
program MediaAlunosWhile;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  totalAlunos, contador: Integer;
  nota, soma, media: Double;

begin
  Write('Digite a quantidade de alunos: ');
  ReadLn(totalAlunos);

  soma := 0;
  contador := 0;

  while contador < totalAlunos do
  begin
    Write('Digite a nota do aluno ', contador + 1, ': ');
    ReadLn(nota);
    soma := soma + nota;
    contador := contador + 1;
  end;

  media := soma / totalAlunos;
  Writeln('Média da turma: ', media:0:2);

  ReadLn;
end.
```
🔁 6. Senha com REPEAT-UNTIL
Enunciado:
Peça uma senha até que o usuário digite a correta (1234).
Exiba mensagem de sucesso ao acertar.
Conceito: REPEAT-UNTIL, validação.
```pascal
program SenhaRepeat;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  senha: String;

begin
  repeat
    Write('Digite a senha: ');
    ReadLn(senha);
  until senha = '1234';

  Writeln('Acesso liberado!');
  ReadLn;
end.
``` 

🔁 7. Números entre 1 e 100 (com BREAK)
Enunciado:
Imprima os números de 1 a 100, mas pare se encontrar um múltiplo de 17.
Conceito: FOR, BREAK.
```pascal
program BreakComFor;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  i: Integer;

begin
  for i := 1 to 100 do
  begin
    if i mod 17 = 0 then
      Break;
    Writeln(i);
  end;

  ReadLn;
end.

``` 
🔁 8. Números entre 1 e 50 (com CONTINUE)
Enunciado:
Imprima os números de 1 a 50, pulando os múltiplos de 5.
Conceito: FOR, CONTINUE.
```pascal
program ContinueComFor;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  i: Integer;

begin
  for i := 1 to 50 do
  begin
    if i mod 5 = 0 then
      Continue;
    Writeln(i);
  end;

  ReadLn;
end.

```

🔁 9. Triângulo de Asteriscos
Enunciado:
Peça um número n e imprima um triângulo de asteriscos com n linhas.
Exemplo para n = 3:
*
**
***
```pascal
program TrianguloAsteriscos;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  i, j, n: Integer;

begin
  Write('Digite o número de linhas: ');
  ReadLn(n);

  for i := 1 to n do
  begin
    for j := 1 to i do
      Write('*');
    Writeln;
  end;

  ReadLn;
end.
```


Conceito: FOR aninhado.

🔁 10. Multiplicação de Matrizes (simples)
Enunciado:
Crie duas matrizes 2x2 com valores fixos.
Multiplique e exiba o resultado.
Conceito: FOR aninhado, lógica prática.

```pascal
program MultiplicacaoMatrizes;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  matrizA, matrizB, resultado: array[1..2, 1..2] of Integer;
  i, j: Integer;

begin
  
  matrizA[1][1] := 1; matrizA[1][2] := 2;
  matrizA[2][1] := 3; matrizA[2][2] := 4;

  matrizB[1][1] := 5; matrizB[1][2] := 6;
  matrizB[2][1] := 7; matrizB[2][2] := 8;

  for i := 1 to 2 do
    for j := 1 to 2 do
      resultado[i][j] := matrizA[i][1] * matrizB[1][j] + matrizA[i][2] * matrizB[2][j];

  Writeln('Resultado da multiplicação:');
  for i := 1 to 2 do
  begin
    for j := 1 to 2 do
      Write(resultado[i][j]:4);
    Writeln;
  end;

  ReadLn;
end.
```




