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
    idade: integer;

begin
  Write('Digite a sua idade: ');
  ReadLn(idade);

  if (idade >= 0) then
  begin
    if (idade > 60) then
      Writeln('Idoso')
    else if (idade > 18) then
      Writeln('Adulto')
    else if (idade > 13) then
      Writeln('Adolescente')
    else
      Writeln('Criaça');

    ReadLn;
  end;

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
    1:
      resultado := num1 + num2;
    2:
      resultado := num1 - num2;
    3:
      begin
        if num2 = 0 then
          Writeln('Erro: divisão por zero!')
        else
          resultado := num1 / num2;
      end;
    4:
      resultado := num1 * num2;
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

var
    nome, senha: string;

begin
  Write('Digite o nome do usuario: ');
  ReadLn(nome);
  Write('Digite a senha do usuario: ');
  ReadLn(senha);

  if (senha = '1234') and (nome = 'admin') then
    Writeln('Senha correta! Usuario logado.')
  else
    Writeln('Senha ou usuario incorreto!');

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

🧠 **Exercício 5: Verificador de Número Válido**
Enunciado:
Peça um número inteiro.
Verifique se ele está entre 10 e 100 ou se é negativo.
Exiba mensagens diferentes para cada caso.
Conceitos: if, operadores lógicos (OR, NOT), validação.
Dica: Use if...else com OR e NOT para testar as condições.
