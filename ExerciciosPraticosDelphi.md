###📘 Exercicios Delphi para pratica diarias sem precisar consultar ia; ###

🧩 Exercício: Cadastro Simples de Produto
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

🧩 Exercício : Verificador de Maioridade
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


🧩 Exercício 6: Soma de Três Números
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
  
🧪 Exercício 7: Conversor de Moeda
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


  

🧪 Exercício 8: Calculadora de Área de Retângulo
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

   

🧪 Exercício 9: Verificador de Nota
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

