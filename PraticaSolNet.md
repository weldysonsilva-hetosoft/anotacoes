# 📚 50 Exercícios Práticos de Delphi - Sol.NET

## 🎯 Nível Básico (Exercícios 1–15)

### 1. Crie um formulário com dois `TEdit` e um `TButton`. Ao clicar no botão, concatene os valores dos dois `TEdit` e exiba o resultado em um `ShowMessage`.
```pascal
unit uFrmPraticaSolnet;

interface

uses
  Windows, Messages, SysUtils, System.Math, Variants, Classes, Graphics, DateUtils,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, UnitExecutaBotao, uFuncoes;

type
  TForm1 = class(TForm)
    edt1: TEdit;
    edt2: TEdit;
    btn1: TButton;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    procedure Ex1ConcatenarTextos;

  public
    { Public declarations }
  end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Ex1ConcatenarTextos;
var
    TextoFinal: string;

begin
  TextoFinal := edt1.text + edt2.text;
  ShowMessage('Resultado: ' + TextoFinal);

end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex1ConcatenarTextos);
end;

end.
```
### 2. Desenvolva uma calculadora simples com quatro operações básicas (`+`, `-`, `*`, `/`) utilizando `TEdit` para entrada de valores e `TButton` para cada operação.
```pascal
procedure TForm1.Ex2CalculadoraBasica;
var
    num1, num2: Double;
  resultado: Double;
begin

  num1 := StrToFloatDef(edt1.text, 0);
  num2 := StrToFloatDef(edt2.text, 0);

  memo1.clear;

  resultado := num1 + num2;
  memo1.Lines.Add('Soma: ' + FloatToStr(resultado));

  resultado := num1 - num2;
  memo1.Lines.Add('Subtração: ' + FloatToStr(resultado));

  resultado := num1 * num2;
  memo1.Lines.Add('Multiplicação: ' + FloatToStr(resultado));

  if num2 <> 0 then
  begin
    resultado := num1 / num2;
    memo1.Lines.Add('Divisão: ' + FloatToStr(resultado));
  end
  else
    memo1.Lines.Add('Divisão: Erro - divisão por zero');
end;
```
  
### 3. Crie um conversor de temperatura que transforme Celsius em Fahrenheit e Kelvin, exibindo os três valores simultaneamente em `TLabel`.
  ```pascal
procedure TForm1.Ex3ConversorTemp;
const
    TEMPERATURA_INFORMADA = 25;
var
    celsius, fahrenheit, kelvin: Double;
begin
  celsius := TEMPERATURA_INFORMADA;
  fahrenheit := celsius * 9 / 5 + 32;
  kelvin := celsius + 273.15;

    lbl1.Caption := Format('Celsius: %.2f °C' + #13#10 + 'Fahrenheit: %.2f °F ' +#13#10+ 'Kelvin: %.2f K',
    [celsius, fahrenheit, kelvin]);

end;
```
   
## 4. Implemente um validador de CPF que receba o CPF em um `TEdit`, remova caracteres especiais usando `TFuncoes.SoNumeros` e valide o dígito verificador.
```pascal
procedure TForm1.Ex4ValidadorCpf;
var
    cpf: string;
  i, soma, resto, digito1, digito2: Integer;
  valido: Boolean;
begin

  cpf := TFuncoes.SoNumeros(edt1.text);

  if Length(cpf) <> 11 then
  begin
    ShowMessage('CPF inválido: deve conter 11 dígitos.');
    Exit;
  end;

  if cpf = StringOfChar(cpf[1], 11) then
  begin
    ShowMessage('CPF inválido: todos os dígitos iguais.');
    Exit;
  end;

  soma := 0;
  for i := 1 to 9 do
    soma := soma + StrToInt(cpf[i]) * (11 - i);

  resto := soma mod 11;
  if resto < 2 then
    digito1 := 0
  else
    digito1 := 11 - resto;

  soma := 0;
  for i := 1 to 10 do
    soma := soma + StrToInt(cpf[i]) * (12 - i);

  resto := soma mod 11;
  if resto < 2 then
    digito2 := 0
  else
    digito2 := 11 - resto;

  valido := (digito1 = StrToInt(cpf[10])) and (digito2 = StrToInt(cpf[11]));

  if valido then
    ShowMessage('CPF válido!')
  else
    ShowMessage('CPF inválido!');
end;
```
## 5. Desenvolva um formulário que receba uma data em `TEdit` e calcule quantos dias faltam para o próximo aniversário da pessoa.
```pascal
procedure TForm1.Ex5ProximoAniversario;
var
    DataNasc, Hoje, ProxAniv: TdateTime;
  IdadeAnos, DiasVida, DiasParaAniv, MesesParaAniv: Integer;
  DiaSemanaNasc, DiaSemanaAniv: string;
begin
  memo1.clear;

  if not TryStrToDate(edt1.text, DataNasc) then
  begin
    memo1.Lines.Add('Data de nascimento inválida!');
    Exit;
  end;

  Hoje := Date;

  IdadeAnos := YearsBetween(Hoje, DataNasc);

  ProxAniv := RecodeYear(DataNasc, YearOf(Hoje));
  if ProxAniv < Hoje then
    ProxAniv := RecodeYear(DataNasc, YearOf(Hoje) + 1);

  DiasParaAniv := DaysBetween(Hoje, ProxAniv);
  MesesParaAniv := MonthsBetween(ProxAniv, Hoje) * -1;

  DiaSemanaNasc := FormatDateTime('dddd', DataNasc);

  DiaSemanaAniv := FormatDateTime('dddd', ProxAniv);

  DiasVida := DaysBetween(Hoje, DataNasc);

  memo1.Lines.Add('Data de nascimento: ' + FormatDateTime('dd/mm/yyyy', DataNasc));
  memo1.Lines.Add('Idade: ' + IntToStr(IdadeAnos) + ' anos');
  memo1.Lines.Add('Próximo aniversário: ' + FormatDateTime('dd/mm/yyyy', ProxAniv) +
    ' (' + DiaSemanaAniv + ')');
  memo1.Lines.Add('Faltam ' + IntToStr(DiasParaAniv) + ' dias para seu aniversário');
  memo1.Lines.Add('Faltam ' + IntToStr(MesesParaAniv) + ' meses para seu aniversário');
  memo1.Lines.Add('Você nasceu em uma ' + DiaSemanaNasc);
  memo1.Lines.Add('Você já viveu ' + IntToStr(DiasVida) + ' dias!');
end;
```

6. Crie uma aplicação que converta um valor monetário digitado em extenso (ex: `1234.56 → "Um mil duzentos e trinta e quatro reais e cinquenta e seis centavos"`).
```pascal
function TForm1.NumeroParaExtenso(Numero: Integer): string;
const
  Unidades: array[0..9] of string = ('', 'um', 'dois', 'três', 'quatro', 
    'cinco', 'seis', 'sete', 'oito', 'nove');
  Dezenas: array[0..9] of string = ('', 'dez', 'vinte', 'trinta', 'quarenta',
    'cinquenta', 'sessenta', 'setenta', 'oitenta', 'noventa');
  Especiais: array[10..19] of string = ('dez', 'onze', 'doze', 'treze', 
    'quatorze', 'quinze', 'dezesseis', 'dezessete', 'dezoito', 'dezenove');
  Centenas: array[0..9] of string = ('', 'cento', 'duzentos', 'trezentos',
    'quatrocentos', 'quinhentos', 'seiscentos', 'setecentos', 'oitocentos', 'novecentos');
var
  milhar, centena, dezena, unidade: Integer;
  resultado: string;
begin
  if Numero = 0 then
  begin
    Result := 'zero';
    Exit;
  end;
  
  if Numero = 100 then
  begin
    Result := 'cem';
    Exit;
  end;
  
  resultado := '';
  
  milhar := Numero div 1000;
  centena := (Numero mod 1000) div 100;
  dezena := (Numero mod 100) div 10;
  unidade := Numero mod 10;
  
  if milhar > 0 then
  begin
    if milhar = 1 then
      resultado := 'mil'
    else
      resultado := NumeroParaExtenso(milhar) + ' mil';
  end;
  
  if centena > 0 then
  begin
    if resultado <> '' then
      resultado := resultado + ' ';
    resultado := resultado + Centenas[centena];
  end;
  
  if (dezena = 1) and (unidade > 0) then
  begin
    if resultado <> '' then
      resultado := resultado + ' e ';
    resultado := resultado + Especiais[10 + unidade];
  end
  else
  begin
    if dezena > 0 then
    begin
      if resultado <> '' then
        resultado := resultado + ' e ';
      resultado := resultado + Dezenas[dezena];
    end;
    
    if unidade > 0 then
    begin
      if resultado <> '' then
        resultado := resultado + ' e ';
      resultado := resultado + Unidades[unidade];
    end;
  end;
  
  Result := resultado;
end;

procedure TForm1.Ex6ConversorNumeroParaString;
var
  valor: Double;
  reais, centavos: Integer;
  extenso: string;
begin
  if not TryStrToFloat(edt1.Text, valor) then
  begin
    ShowMessage('Digite um valor válido!');
    Exit;
  end;
  
  if valor < 0 then
  begin
    ShowMessage('Valor não pode ser negativo!');
    Exit;
  end;
  
  reais := Trunc(valor);
  centavos := Round((valor - reais) * 100);
  
  extenso := '';
  
  if reais > 0 then
  begin
    extenso := NumeroParaExtenso(reais);
    if reais = 1 then
      extenso := extenso + ' real'
    else
      extenso := extenso + ' reais';
  end
  else
    extenso := 'zero reais';
  
  if centavos > 0 then
  begin
    extenso := extenso + ' e ' + NumeroParaExtenso(centavos);
    if centavos = 1 then
      extenso := extenso + ' centavo'
    else
      extenso := extenso + ' centavos';
  end;
  
  ShowMessage(extenso);
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex6ConversorNumeroParaString);
end;
```  
7. Implemente um gerador de senhas aleatórias com opções de tamanho (6–20 caracteres) e inclusão de números, letras maiúsculas, minúsculas e caracteres especiais.
```pascal
function TForm1.GeradorSenhaForte(Senha: string): Boolean;
var
    temNumero, temMaiuscula, temMinuscula: Boolean;
  i: Integer;
begin
  temNumero := False;
  temMaiuscula := False;
  temMinuscula := False;

  for i := 1 to Length(Senha) do
  begin
    if Senha[i] in ['0' .. '9'] then
      temNumero := True
    else if Senha[i] in ['A' .. 'Z'] then
      temMaiuscula := True
    else if Senha[i] in ['a' .. 'z'] then
      temMinuscula := True;
  end;

  Result := (Length(Senha) >= 6) and (Length(Senha) <= 20) and temNumero and temMaiuscula and temMinuscula;
end;

procedure TForm1.Ex7GeradorSenha;
var
    Senha: string;
begin
  memo1.clear;
  Senha := edt1.text;

  if GeradorSenhaForte(Senha) then
    memo1.Lines.Add('✅ Senha forte!')
  else
  begin
    memo1.Lines.Add('❌ Senha fraca. Faltam os seguintes critérios:');
    if Length(Senha) < 6 then
      memo1.Lines.Add(' - MINIMO DE 6 CARACTERES');
    if Length(Senha) > 20 then
      memo1.Lines.Add(' - MAXIMO DE 20 CARACTERES');

    if not(Senha.Contains('0') or Senha.Contains('1') or Senha.Contains('2') or
      Senha.Contains('3') or Senha.Contains('4') or Senha.Contains('5') or
      Senha.Contains('6') or Senha.Contains('7') or Senha.Contains('8') or
      Senha.Contains('9')) then
      memo1.Lines.Add('- Pelo menos 1 número');

    if Senha = LowerCase(Senha) then
      memo1.Lines.Add('- Pelo menos 1 letra maiúscula');

    if Senha = UpperCase(Senha) then
      memo1.Lines.Add('- Pelo menos 1 letra minúscula');

  end;

end;

```

8. Desenvolva um contador regressivo visual usando `TTimer` que inicie em um valor definido pelo usuário e exiba o tempo restante em um `TLabel`.
```pascal

// ESSE PRECISEI DE AJUDA DA IA PARA DESENVOLVER. 
procedure TForm1.Ex8ContadorRegressivo;
var
    minutos: Integer;
begin
  // Obter o valor do usuário (minutos)
  if not TryStrToInt(edt1.text, minutos) or (minutos <= 0) then
  begin
    ShowMessage('Digite um valor válido em minutos (maior que 0)');
    Exit;
  end;

  // Converter minutos para segundos
  tempoRestante := minutos * 60;

  // Configurar e iniciar o timer
  tmr1.Interval := 1000; // 1 segundo
  tmr1.Enabled := True;

  // Atualizar display inicial
  AtualizarDisplayTempo;

  // Feedback para o usuário
  memo1.clear;
  memo1.Lines.Add('Contador regressivo iniciado!');
  memo1.Lines.Add('Tempo total: ' + IntToStr(minutos) + ' minutos');
  memo1.Lines.Add('Pressione o botão novamente para reiniciar');
end;

procedure TForm1.AtualizarDisplayTempo;
var
    minutos, segundos: Integer;
  tempoFormatado: string;
begin
  minutos := tempoRestante div 60;
  segundos := tempoRestante mod 60;

  tempoFormatado := Format('%.2d:%.2d', [minutos, segundos]);

  lbl1.Caption := 'Tempo restante: ' + tempoFormatado;

  // Mudar cor conforme o tempo diminui
  if tempoRestante <= 30 then // últimos 30 segundos
    lbl1.Font.Color := clRed
  else if tempoRestante <= 60 then // último minuto
    lbl1.Font.Color := clMaroon
  else
    lbl1.Font.Color := clBlack;
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  // Decrementar o tempo restante
  Dec(tempoRestante);

  // Atualizar o display
  AtualizarDisplayTempo;

  // Verificar se o tempo acabou
  if tempoRestante <= 0 then
  begin
    tmr1.Enabled := False;
    lbl1.Caption := 'TEMPO ESGOTADO!';
    lbl1.Font.Color := clRed;
    ShowMessage('Tempo esgotado!');
    memo1.Lines.Add('--- CONTADOR FINALIZADO ---');
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex8ContadorRegressivo);
end;

end.


(*precisou declarar o  public
    { Public declarations }
  var
    tempoRestante: Integer; No public e mais uma procedure no private
procedure Ex8ContadorRegressivo;
    procedure AtualizarDisplayTempo;*)


```


9. Crie um formulário que receba um texto em `TMemo` e conte: total de caracteres, palavras, linhas e vogais.
```pascal
function TForm1.AnalisarTexto(texto: String; var palavras, vogais, consoantes: Integer): Integer;
var
    i: Integer;
  C: Char;
  emPalavra: Boolean;
begin
  palavras := 0;
  vogais := 0;
  consoantes := 0;
  emPalavra := False;

  for i := 1 to Length(texto) do
  begin
    C := texto[i];


    if C in ['A' .. 'Z', 'a' .. 'z', 'À' .. 'ü'] then
    begin
      if not emPalavra then
      begin
        Inc(palavras);
        emPalavra := True;
      end;
    end
    else
      emPalavra := False;


    if UpCase(C) in ['A', 'E', 'I', 'O', 'U', 'Á', 'É', 'Í', 'Ó', 'Ú'] then
      Inc(vogais)
    else if UpCase(C) in ['A' .. 'Z'] then
      Inc(consoantes);
  end;

  Result := Length(texto);
end;

procedure TForm1.Ex9ContadorCaracter;
var
  texto: String;
  total, palavras, vogais, consoantes, linhas: Integer;
begin

  tmr1.Enabled := False;


  texto := memo1.Text;
  linhas := memo1.Lines.Count;


  total := AnalisarTexto(texto, palavras, vogais, consoantes);


  memo1.Clear;
  memo1.Lines.Add('=== ANÁLISE DO TEXTO ===');
  memo1.Lines.Add('Total de caracteres: ' + IntToStr(total));
  memo1.Lines.Add('Total de palavras: ' + IntToStr(palavras));
  memo1.Lines.Add('Total de linhas: ' + IntToStr(linhas));
  memo1.Lines.Add('Total de vogais: ' + IntToStr(vogais));
  memo1.Lines.Add('Total de consoantes: ' + IntToStr(consoantes));
  memo1.Lines.Add('=======================');
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex9ContadorCaracter);
end;
```

10. Implemente uma calculadora de IMC (Índice de Massa Corporal) que classifique o resultado em: abaixo do peso, peso normal, sobrepeso ou obesidade.
```pascal
procedure TForm1.Ex10CalculadoraIMC;
var
    peso, altura, imc: Double;
  classificacao, corTexto: string;
begin

  if not TryStrToFloat(edt1.text, peso) or (peso <= 0) then
  begin
    ShowMessage('Digite um peso válido em kg (maior que 0)');
    Exit;
  end;

  if not TryStrToFloat(edt2.text, altura) or (altura <= 0) then
  begin
    ShowMessage('Digite uma altura válida em metros (maior que 0)');
    Exit;
  end;

  imc := peso / (altura * altura);

  if imc < 18.5 then
  begin
    classificacao := 'ABAIXO DO PESO';
    corTexto := 'Azul';
    lbl2.Font.Color := clBlue;
  end
  else if imc < 25 then
  begin
    classificacao := 'PESO NORMAL';
    corTexto := 'Verde';
    lbl2.Font.Color := clGreen;
  end
  else if imc < 30 then
  begin
    classificacao := 'SOBREPESO';
    corTexto := 'Laranja';
    lbl2.Font.Color := clWebOrange;
  end
  else if imc < 35 then
  begin
    classificacao := 'OBESIDADE GRAU I';
    corTexto := 'Vermelho';
    lbl2.Font.Color := clRed;
  end
  else if imc < 40 then
  begin
    classificacao := 'OBESIDADE GRAU II';
    corTexto := 'Vermelho Escuro';
    lbl2.Font.Color := clMaroon;
  end
  else
  begin
    classificacao := 'OBESIDADE GRAU III';
    corTexto := 'Vermelho Intenso';
    lbl2.Font.Color := clPurple;
  end;

  lbl1.Caption := 'IMC: ' + FormatFloat('0.00', imc);
  lbl2.Caption := classificacao;

  memo1.clear;
  memo1.Lines.Add('=== CALCULADORA IMC ===');
  memo1.Lines.Add('Peso: ' + FloatToStr(peso) + ' kg');
  memo1.Lines.Add('Altura: ' + FloatToStr(altura) + ' m');
  memo1.Lines.Add('IMC Calculado: ' + FormatFloat('0.00', imc));
  memo1.Lines.Add('Classificação: ' + classificacao);
  memo1.Lines.Add('Cor indicativa: ' + corTexto);
  memo1.Lines.Add('======================');

end;
```

11. Desenvolva um sistema que receba uma lista de 10 números em um `TMemo` (um por linha) e exiba o maior, menor e a média em `TLabel` separados.
```pascal
procedure TForm1.Ex11ListNumber;
var
    i, valor, maior, menor, soma: Integer;
  numeros: array [1 .. 10] of Integer;
begin

  if memo1.Lines.Count <> 10 then
  begin
    ShowMessage('Insira exatamente 10 números, um por linha.');
    Exit;
  end;
  soma := 0;
  maior := StrToIntDef(memo1.Lines[0], 0);
  menor := maior;

  for i := 0 to 9 do
  begin
    valor := StrToIntDef(memo1.Lines[i], 0);
    if valor > maior then
      maior := valor;
    if valor < menor then
      menor := valor;
    soma := soma + valor;
  end;
  lbl1.Caption := 'Maior ' + IntToStr(maior);
  lbl2.Caption := 'Menor ' + IntToStr(menor);
  lbl3.Caption := 'Média ' + FloatToStr(soma / 10);
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex11ListNumber);
end;

```
12. Crie um validador de email que verifique se o formato está correto (presença de `@`, domínio válido, etc.).
```pascal
procedure TForm1.Ex12ValidaEmail;
var
    email: string;
begin
  memo1.clear;
  email := edt1.text;

  if ValidadorEmail(email) then
    memo1.Lines.Add('Emailválido!')
  else
  begin
    memo1.Lines.Add('❌ Email inválido. Faltam os seguintes critérios:');

    if Pos('@', email) = 0 then
      memo1.Lines.Add('- Deve conter "@"');

    if Pos('.', Copy(email, Pos('@', email) + 1, Length(email))) = 0 then
      memo1.Lines.Add('- Domínio deve conter "." após o "@"');

    if Length(email) < 8 then
      memo1.Lines.Add('- Mínimo de 8 caracteres');

    if Length(email) > 25 then
      memo1.Lines.Add('- Máximo de 25 caracteres');

    if not(email.Contains('a') or email.Contains('b') or email.Contains('c') or
      email.Contains('d') or email.Contains('e') or email.Contains('f') or
      email.Contains('g') or email.Contains('h') or email.Contains('i') or
      email.Contains('j') or email.Contains('k') or email.Contains('l') or
      email.Contains('m') or email.Contains('n') or email.Contains('o') or
      email.Contains('p') or email.Contains('q') or email.Contains('r') or
      email.Contains('s') or email.Contains('t') or email.Contains('u') or
      email.Contains('v') or email.Contains('w') or email.Contains('x') or
      email.Contains('y') or email.Contains('z')) then
      memo1.Lines.Add('- Deve conter pelo menos uma letra');
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex12ValidaEmail);
end;

```

13. Implemente um conversor de bases numéricas (decimal, binário, octal, hexadecimal) com validação de entrada.
```pascal
function IntToBin(Value: Integer; Digits: Integer): string;
begin
  Result := '';
  while Value > 0 do
  begin
    Result := Chr(Ord('0') + (Value mod 2)) + Result;
    Value := Value div 2;
  end;
  while Length(Result) < Digits do
    Result := '0' + Result;
end;

function IntToOct(Value: Integer): string;
begin
  Result := '';
  while Value > 0 do
  begin
    Result := Chr(Ord('0') + (Value mod 8)) + Result;
    Value := Value div 8;
  end;
  if Result = '' then
    Result := '0';
end;

procedure TForm1.Ex13ConversorBaseNumericas;
var
    decimal: Integer;
  entrada: string;
begin
  entrada := edt1.text;

  if not TryStrToInt(entrada, decimal) then
  begin
    ShowMessage('Valor inválido para conversão. Digite um número inteiro.');
    Exit;
  end;

  memo1.clear;
  memo1.Lines.Add('Decimal: ' + IntToStr(decimal));
  memo1.Lines.Add('Binário: ' + IntToBin(decimal, 8));
  memo1.Lines.Add('Octal: ' + IntToOct(decimal));
  memo1.Lines.Add('Hexadecimal: ' + IntToHex(decimal, 8));
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex13ConversorBaseNumericas);
end;
```
14. Desenvolva um formulário que calcule o valor de parcelas de um financiamento com base no valor total, taxa de juros e número de parcelas.
```pascal
procedure TForm1.Ex14ParcelasFinanciamento;

var
    valorTotal, taxaJuros, parcelaMensal: Double;
  numParcelas: Integer;
  entradaValor, entradaJuros, entradaParcelas: string;
begin

  entradaValor := edt1.text;
  entradaJuros := edt2.text;
  entradaParcelas := edt3.text;

  if not TryStrToFloat(entradaValor, valorTotal) then
  begin
    ShowMessage('Valor total inválido.');
    Exit;
  end;

  if not TryStrToFloat(entradaJuros, taxaJuros) then
  begin
    ShowMessage('Taxa de juros inválida.');
    Exit;
  end;

  if not TryStrToInt(entradaParcelas, numParcelas) then
  begin
    ShowMessage('Número de parcelas inválido.');
    Exit;
  end;

  taxaJuros := taxaJuros / 100;

  if taxaJuros = 0 then
    parcelaMensal := valorTotal / numParcelas
  else
    parcelaMensal := (valorTotal * taxaJuros) / (1 - Power(1 + taxaJuros, -numParcelas));

  memo1.clear;
  memo1.Lines.Add('Valor total: R$ ' + FormatFloat('0.00', valorTotal));
  memo1.Lines.Add('Taxa de juros: ' + FormatFloat('0.00%', taxaJuros * 100));
  memo1.Lines.Add('Número de parcelas: ' + IntToStr(numParcelas));
  memo1.Lines.Add('Valor da parcela: R$ ' + FormatFloat('0.00', parcelaMensal));
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  ExecutarBotao(Ex14ParcelasFinanciamento);
end;

```

15. Crie um sistema que receba duas datas e calcule: diferença em dias, meses, anos e dia da semana de cada data.
```pascal
procedure TForm1.Ex15SistemasData;
var
    data1, data2: TdateTime;
  Dias, Semanas, Meses, Anos: Integer;
begin
  memo1.clear;

  if not TryStrToDate(edt1.text, data1) then
    ShowMessage('Data 1 inválida! Forneca uma data no formato DD/MM/AAAA');

  if not TryStrToDate(edt2.text, data2) then
    ShowMessage('Data 2 inválida! Forneca uma data no formato DD/MM/AAAA');

  Dias := DaysBetween(data1, data2);
  Semanas := Round(Dias / 7);
  Meses := MonthsBetween(data1, data2);
  Anos := YearsBetween(data1, data2);

  memo1.Lines.Add('Diferença em dias: ' + IntToStr(Dias));
  memo1.Lines.Add('Diferença em semanas (arredondado): ' + IntToStr(Semanas));
  memo1.Lines.Add('Diferença em meses (aproximado): ' + IntToStr(Meses));
  memo1.Lines.Add('Diferença em anos (aproximado): ' + IntToStr(Anos));

end;
``` 

---

## 🔧 Nível Intermediário (Exercícios 16–35)

16. Desenvolva um CRUD completo (Create, Read, Update, Delete) para uma tabela de Clientes usando `TFDQuery` e `TFDConnection` com Firebird.
  ```pascal
unit Crud;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Comp.Client,
  FireDAC.Stan.Param, Vcl.StdCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,
  FireDAC.DApt, System.StrUtils, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Phys.PGDef, FireDAC.Phys.PG, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL;

type
  TForm1 = class(TForm)

    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    DataSource1: TDataSource;

    DBGrid1: TDBGrid;

    edtNome: TEdit;
    edtCPF: TEdit;
    edtEmail: TEdit;
    edtTelefone: TEdit;
    chkAtivo: TCheckBox;

    btnNovo: TButton;
    btnSalvar: TButton;
    btnEditar: TButton;
    btnExcluir: TButton;
    btnCancelar: TButton;
    btnPesquisar: TButton;

    lblNome: TLabel;
    lblCPF: TLabel;
    lblEmail: TLabel;
    lblTelefone: TLabel;
    FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink;

    procedure FormCreate(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnPesquisarClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure fdphyspgdrvrlnkDriverCreated(Sender: TObject);

  private
    FModoEdicao: Boolean;
    FIdClienteAtual: Integer;

    procedure ConfigurarConexao;
    procedure CarregarClientes;
    procedure LimparCampos;
    procedure HabilitarCampos(Habilitar: Boolean);
    procedure ValidarCampos;

    procedure CriarCliente;
    procedure AtualizarCliente;
    procedure ExcluirCliente;
    procedure CarregarClienteParaEdicao(IdCliente: Integer);

  public
    { Public declarations }
  end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

uses UFuncoes;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDConnection1.Params.Clear;
  ConfigurarConexao;
  CarregarClientes;
  LimparCampos;
  HabilitarCampos(False);
  FModoEdicao := False;
  FIdClienteAtual := 0;
end;

procedure TForm1.ConfigurarConexao;
begin

  FDConnection1.Params.Clear;
  FDConnection1.DriverName := 'MSSQL';

  FDConnection1.Params.Values['Server'] := 'Localhost';
  FDConnection1.Params.Values['Database'] := 'WeldysonTeste';
  FDConnection1.Params.Values['User_Name'] := 'sa';
  FDConnection1.Params.Values['Password'] := 'Heto124353';
  FDConnection1.Params.Values['CharacterSet'] := 'UTF8';
  FDConnection1.Params.Values['LoginTimeout'] := '30';
  FDConnection1.Params.Values['MetaDefSchema'] := 'dbo';
  FDConnection1.Params.Values['DriverID'] := 'MSSQL';

  if FDConnection1.Connected then
    ShowMessage('Conexão estabelecida com sucesso!');
  Exit;
  if not FDConnection1.Connected then
    ShowMessage('Erro ao conectar ao banco: ');
end;

procedure TForm1.CarregarClientes;
begin
  FDQuery1.SQL.Clear;
  FDQuery1.SQL.Add('SELECT');
  FDQuery1.SQL.Add('  ID_PESSOA,');
  FDQuery1.SQL.Add('  NOME,');
  FDQuery1.SQL.Add('  CPF,');
  FDQuery1.SQL.Add('  EMAIL,');
  FDQuery1.SQL.Add('  TELEFONE,');
  FDQuery1.SQL.Add('  INATIVO,');
  FDQuery1.SQL.Add('  DT_CADASTRO');
  FDQuery1.SQL.Add('FROM PESSOAS');
  FDQuery1.SQL.Add('ORDER BY NOME');

  FDQuery1.Open;

  if FDQuery1.IsEmpty then
    ShowMessage('Não há Cliente a ser carregado!');

end;

procedure TForm1.btnPesquisarClick(Sender: TObject);
var
    Filtro: string;
begin
  Filtro := InputBox('Pesquisar Cliente', 'Digite o nome:', '');

  if Filtro.Trim.IsEmpty then
  begin
    CarregarClientes;
    Exit;
  end;

  FDQuery1.Close;
  FDQuery1.SQL.Clear;
  FDQuery1.SQL.Add('SELECT * FROM CLIENTES');
  FDQuery1.SQL.Add('WHERE UPPER(NOME) LIKE :NOME');
  FDQuery1.SQL.Add('ORDER BY NOME');
  FDQuery1.ParamByName('NOME').AsString := '%' + UpperCase(Filtro) + '%';

  try
    FDQuery1.Open;

    if FDQuery1.IsEmpty then
      ShowMessage('️ Nenhum cliente encontrado com esse nome.');
  except
    on E: Exception do
      ShowMessage(' Erro na pesquisa: ' + E.Message);
  end;
end;


// OPERAÇÃO CREATE - INSERIR CLIENTE

procedure TForm1.btnNovoClick(Sender: TObject);
begin
  LimparCampos;
  HabilitarCampos(True);
  FModoEdicao := False;
  FIdClienteAtual := 0;
  edtNome.SetFocus;
end;

procedure TForm1.CriarCliente;
var
    QueryInsert: TFDQuery;
begin
  ValidarCampos;

  QueryInsert := TFDQuery.Create(nil);
  try
    QueryInsert.Connection := FDConnection1;
    QueryInsert.SQL.Clear;
    QueryInsert.SQL.Add('INSERT INTO CLIENTES (');
    QueryInsert.SQL.Add('  NOME,');
    QueryInsert.SQL.Add('  CPF,');
    QueryInsert.SQL.Add('  EMAIL,');
    QueryInsert.SQL.Add('  TELEFONE,');
    QueryInsert.SQL.Add('  ATIVO');
    QueryInsert.SQL.Add(') VALUES (');
    QueryInsert.SQL.Add('  :NOME,');
    QueryInsert.SQL.Add('  :CPF,');
    QueryInsert.SQL.Add('  :EMAIL,');
    QueryInsert.SQL.Add('  :TELEFONE,');
    QueryInsert.SQL.Add('  :ATIVO');
    QueryInsert.SQL.Add(')');

    QueryInsert.ParamByName('NOME').AsString := edtNome.Text;
    QueryInsert.ParamByName('CPF').AsString := TFuncoes.SoNumeros(edtCPF.Text);
    QueryInsert.ParamByName('EMAIL').AsString := edtEmail.Text;
    QueryInsert.ParamByName('TELEFONE').AsString := TFuncoes.SoNumeros(edtTelefone.Text);
    QueryInsert.ParamByName('ATIVO').AsString := IfThen(chkAtivo.Checked, 'S', 'N');

    try
      FDConnection1.StartTransaction;
      QueryInsert.ExecSQL;
      FDConnection1.Commit;

      ShowMessage(' Cliente cadastrado com sucesso!');
      CarregarClientes;
      LimparCampos;
      HabilitarCampos(False);
    except
      on E: Exception do
      begin
        FDConnection1.Rollback;
        raise Exception.Create(' Erro ao cadastrar cliente: ' + E.Message);
      end;
    end;
  finally
    QueryInsert.Free;
  end;
end;


// OPERAÇÃO UPDATE - ATUALIZAR CLIENTE

procedure TForm1.btnEditarClick(Sender: TObject);
begin
  if FDQuery1.IsEmpty then
  begin
    ShowMessage('Selecione um cliente para editar.');
    Exit;
  end;

  FIdClienteAtual := FDQuery1.FieldByName('ID_CLIENTE').AsInteger;
  CarregarClienteParaEdicao(FIdClienteAtual);
  HabilitarCampos(True);
  FModoEdicao := True;
  edtNome.SetFocus;
end;

procedure TForm1.CarregarClienteParaEdicao(IdCliente: Integer);
var
    QueryLoad: TFDQuery;
begin
  QueryLoad := TFDQuery.Create(nil);
  try
    QueryLoad.Connection := FDConnection1;
    QueryLoad.SQL.Text := 'SELECT * FROM CLIENTES WHERE ID_CLIENTE = :ID';
    QueryLoad.ParamByName('ID').AsInteger := IdCliente;
    QueryLoad.Open;

    if not QueryLoad.IsEmpty then
    begin
      edtNome.Text := QueryLoad.FieldByName('NOME').AsString;
      edtCPF.Text := QueryLoad.FieldByName('CPF').AsString;
      edtEmail.Text := QueryLoad.FieldByName('EMAIL').AsString;
      edtTelefone.Text := QueryLoad.FieldByName('TELEFONE').AsString;
      chkAtivo.Checked := QueryLoad.FieldByName('ATIVO').AsString = 'S';
    end;
  finally
    QueryLoad.Free;
  end;
end;

procedure TForm1.AtualizarCliente;
var
    QueryUpdate: TFDQuery;
begin
  ValidarCampos;

  if FIdClienteAtual <= 0 then
    raise Exception.Create(' Nenhum cliente selecionado para atualização.');

  QueryUpdate := TFDQuery.Create(nil);
  try
    QueryUpdate.Connection := FDConnection1;
    QueryUpdate.SQL.Clear;
    QueryUpdate.SQL.Add('UPDATE CLIENTES SET');
    QueryUpdate.SQL.Add('  NOME = :NOME,');
    QueryUpdate.SQL.Add('  CPF = :CPF,');
    QueryUpdate.SQL.Add('  EMAIL = :EMAIL,');
    QueryUpdate.SQL.Add('  TELEFONE = :TELEFONE,');
    QueryUpdate.SQL.Add('  ATIVO = :ATIVO,');
    QueryUpdate.SQL.Add('  DATA_ALTERACAO = CURRENT_TIMESTAMP');
    QueryUpdate.SQL.Add('WHERE ID_CLIENTE = :ID');

    QueryUpdate.ParamByName('NOME').AsString := edtNome.Text;
    QueryUpdate.ParamByName('CPF').AsString := TFuncoes.SoNumeros(edtCPF.Text);
    QueryUpdate.ParamByName('EMAIL').AsString := edtEmail.Text;
    QueryUpdate.ParamByName('TELEFONE').AsString := TFuncoes.SoNumeros(edtTelefone.Text);
    QueryUpdate.ParamByName('ATIVO').AsString := IfThen(chkAtivo.Checked, 'S', 'N');
    QueryUpdate.ParamByName('ID').AsInteger := FIdClienteAtual;

    try
      FDConnection1.StartTransaction;
      QueryUpdate.ExecSQL;
      FDConnection1.Commit;

      ShowMessage(' Cliente atualizado com sucesso!');
      CarregarClientes;
      LimparCampos;
      HabilitarCampos(False);
      FModoEdicao := False;
    except
      on E: Exception do
      begin
        FDConnection1.Rollback;
        raise Exception.Create('Erro ao atualizar cliente: ' + E.Message);
      end;
    end;
  finally
    QueryUpdate.Free;
  end;
end;


// OPERAÇÃO DELETE - EXCLUIR CLIENTE

procedure TForm1.btnExcluirClick(Sender: TObject);
begin
  if FDQuery1.IsEmpty then
  begin
    ShowMessage(' Selecione um cliente para excluir.');
    Exit;
  end;

  if MessageDlg('Confirma a exclusão do cliente "' +
    FDQuery1.FieldByName('NOME').AsString + '"?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FIdClienteAtual := FDQuery1.FieldByName('ID_CLIENTE').AsInteger;
    ExcluirCliente;
  end;
end;

procedure TForm1.ExcluirCliente;
var
    QueryDelete: TFDQuery;
begin
  if FIdClienteAtual <= 0 then
    raise Exception.Create(' Nenhum cliente selecionado para exclusão.');

  QueryDelete := TFDQuery.Create(nil);
  try
    QueryDelete.Connection := FDConnection1;
    QueryDelete.SQL.Text := 'DELETE FROM CLIENTES WHERE ID_CLIENTE = :ID';
    QueryDelete.ParamByName('ID').AsInteger := FIdClienteAtual;

    try
      FDConnection1.StartTransaction;
      QueryDelete.ExecSQL;
      FDConnection1.Commit;

      ShowMessage(' Cliente excluído com sucesso!');
      CarregarClientes;
      LimparCampos;
      FIdClienteAtual := 0;
    except
      on E: Exception do
      begin
        FDConnection1.Rollback;
        raise Exception.Create(' Erro ao excluir cliente: ' + E.Message);
      end;
    end;
  finally
    QueryDelete.Free;
  end;
end;

procedure TForm1.fdphyspgdrvrlnkDriverCreated(Sender: TObject);
begin

end;

// MÉTODOS AUXILIARES

procedure TForm1.LimparCampos;
begin
  edtNome.Clear;
  edtCPF.Clear;
  edtEmail.Clear;
  edtTelefone.Clear;
  chkAtivo.Checked := True;
end;

procedure TForm1.HabilitarCampos(Habilitar: Boolean);
begin
  edtNome.Enabled := Habilitar;
  edtCPF.Enabled := Habilitar;
  edtEmail.Enabled := Habilitar;
  edtTelefone.Enabled := Habilitar;
  chkAtivo.Enabled := Habilitar;

  btnSalvar.Enabled := Habilitar;
  btnCancelar.Enabled := Habilitar;
  btnNovo.Enabled := not Habilitar;
  btnEditar.Enabled := not Habilitar;
  btnExcluir.Enabled := not Habilitar;
  btnPesquisar.Enabled := not Habilitar;
end;

procedure TForm1.ValidarCampos;
var
    CPF: string;
begin
  if Trim(edtNome.Text) = '' then
    raise Exception.Create('O campo NOME é obrigatório.');

  if Trim(edtCPF.Text) <> '' then
  begin
    CPF := TFuncoes.SoNumeros(edtCPF.Text);
    if Length(CPF) <> 11 then
      raise Exception.Create('CPF deve conter 11 dígitos.');
  end;

  if Trim(edtEmail.Text) <> '' then
  begin
    if Pos('@', edtEmail.Text) = 0 then
      raise Exception.Create('Email inválido.');
  end;
end;

procedure TForm1.btnSalvarClick(Sender: TObject);
begin
  try
    if FModoEdicao then
      AtualizarCliente
    else
      CriarCliente;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TForm1.btnCancelarClick(Sender: TObject);
begin
  LimparCampos;
  HabilitarCampos(False);
  FModoEdicao := False;
  FIdClienteAtual := 0;
end;

procedure TForm1.DBGrid1DblClick(Sender: TObject);
begin
  btnEditarClick(Sender);
end;

end.


```
  
    
18. 
19. Implemente um sistema de login que valide usuário e senha contra uma tabela no banco de dados, criptografando a senha com MD5.
20. Crie um formulário de cadastro de produtos com validação de código de barras (EAN-13) incluindo cálculo do dígito verificador.
21. Desenvolva um relatório de vendas usando `TFDQuery` que agrupe por período, categoria de produto e vendedor, exibindo totais em um `TDBGrid`.
22. Implemente um sistema de estoque que controle entradas e saídas de produtos, atualizando o saldo automaticamente e impedindo vendas com estoque zerado.
23. Crie um importador de arquivo CSV que leia dados de clientes e insira automaticamente em uma tabela usando transações.
24. Desenvolva um exportador de dados que gere arquivo XML com estrutura hierárquica a partir de uma consulta SQL (Pedidos → Itens).
25. Implemente um sistema de backup automático do banco de dados Firebird que execute em horários agendados usando `TTimer`.
26. Crie um formulário de pesquisa avançada de produtos com filtros dinâmicos (nome, categoria, faixa de preço) construindo SQL dinamicamente.
27. Desenvolva um sistema de auditoria que registre todas as alterações (insert, update, delete) em uma tabela de log com data, hora, usuário e valores anteriores/novos.
28. Implemente um gerador de código de barras que converta um código numérico em imagem Code128 ou EAN-13 e exiba em um `TImage`.
29. Crie um sistema de contas a pagar/receber com controle de vencimento, juros, multa e baixa de títulos.
30. Desenvolva um módulo de integração REST que consuma uma API externa (ex: ViaCEP) e preencha automaticamente campos de endereço ao digitar o CEP.
31. Implemente um sistema de impressão de etiquetas personalizadas com preview, usando componentes de relatório (ex: QuickReport ou FastReport).
32. Crie um dashboard com gráficos (`TChart`) exibindo: vendas por mês, produtos mais vendidos e ticket médio.
33. Desenvolva um sistema de permissões de usuário que controle acesso a formulários e funcionalidades específicas baseado em perfis (Admin, Vendedor, Caixa).
34. Implemente um sincronizador de dados que compare duas tabelas (local e servidor) e aplique apenas as diferenças (delta sync).
35. Crie um módulo de envio de emails automáticos usando Indy (`IdSMTP`) com anexos e templates HTML.
36. Desenvolva um sistema de controle de versão de banco de dados que execute scripts SQL sequenciais para atualização de estrutura.
37. Implemente um gerenciador de filas de processamento assíncrono que execute tarefas em background sem travar a interface.

---

## 🚀 Nível Avançado (Exercícios 36–50)

36. Desenvolva um framework de conversão de dados genérico similar ao `ConversaoBuilder` do Sol.NET, com suporte a mapeamento fluente de campos e callbacks.
37. Implemente um sistema de integração fiscal completo que envie NFC-e para SEFAZ, valide retorno e armazene XML assinado.
38. Crie um módulo de sincronização PDV-Servidor que implemente controle de conflitos, versionamento e retry automático com backoff exponencial.
39. Desenvolva um motor de regras de negócio configurável via banco de dados que execute validações dinâmicas sem recompilar a aplicação.
40. Implemente um sistema de multi-tenancy que isole dados por empresa usando schemas ou filtros automáticos em todas as queries.
41. Crie um framework de integrações seguindo o padrão Strategy do Framework de Integrações do Sol.NET, com suporte a múltiplos provedores.
42. Desenvolva um sistema de cache distribuído usando Redis ou Memcached para otimizar consultas frequentes.
43. Implemente um módulo de Business Intelligence que processe OLAP Cubes e permita análise dimensional de vendas.
44. Crie um sistema de monitoramento em tempo real que exiba status de integrações, filas de processamento e alertas críticos.
45. Desenvolva um gerador de relatórios dinâmicos que permita ao usuário criar consultas SQL via interface visual drag-and-drop.
46. Implemente um sistema de replicação de dados entre Firebird e SQL Server mantendo sincronização bidirecional.
47. Crie um módulo de machine learning básico que classifique produtos automaticamente com base em descrição usando TensorFlow ou similar.
48. Desenvolva um sistema de workflow configurável que permita criar fluxos de aprovação personalizados para diferentes processos.
49. Implemente um gateway de pagamentos que integre múltiplos adquirentes (Stone, Cielo, Rede) usando padrão Factory e Strategy.
50. Crie uma aplicação completa de PDV off-line que sincronize vendas, estoque e clientes com servidor central, implementando controle transacional robusto, gestão de exceções customizadas (`EExcecaoUsuario`, `EExcecaoDesenvolvedor`), logging detalhado e interface responsiva seguindo todos os padrões arquiteturais do Sol.NET.

---

## ✨ Observações

- **Exercícios 1–15**: Foco em lógica, componentes básicos e funções nativas  
- **Exercícios 16–35**: Banco de dados, integrações e funcionalidades intermediárias  
- **Exercícios 36–50**: Arquitetura avançada, frameworks e sistemas complexos  

## 🎓 Progressão sugerida

Complete os exercícios sequencialmente para construir conhecimento gradualmente, sempre aplicando os padrões de código do Sol.NET (Clean Code, variáveis inline, nomenclatura PascalCase).

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
