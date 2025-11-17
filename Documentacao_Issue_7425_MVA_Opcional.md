# Documentação - Issue #7425: MVA Opcional no Cadastro NCM

## Resumo da Implementação

Esta implementação adiciona a funcionalidade de tornar **opcional** a validação do campo MVA (Margem de Valor Agregado) no cadastro de NCM quando o CST (Código de Situação Tributária) for **060**.

### Comportamento Anterior
- Quando CST = 060, o sistema **obrigatoriamente** exigia preenchimento do campo MVA
- Não havia opção de configuração para desabilitar esta validação

### Comportamento Atual
- Nova configuração no menu **Configuração Geral → Aba Fiscal**
- Checkbox: **"Não utiliza Substituição Tributária(NCM)"**
- **Desmarcado (padrão)**: Mantém comportamento atual - exige MVA quando CST = 060
- **Marcado**: Permite salvar NCM com CST = 060 mesmo sem MVA preenchido

---

## Arquivos Modificados

### 1. `Framework/uVariaveisGlobais.pas`
**Linha 757**: Adicionada constante de configuração
```pascal
conConfig_NaoUtilizaSubstituicaoTributaria = 'NaoUtilizaSubstituicaoTributaria';
```

### 2. `Sol.NET/FormEspecias/uFrmConfiguracao.pas`
**Linha 702**: Declaração do componente checkbox
```pascal
cbxNaoUtilizaSubstituicaoTributaria: TCheckBoxPlus;
```

**Linha 4879**: Carregamento da configuração ao abrir tela
```pascal
cbxNaoUtilizaSubstituicaoTributaria.MostrarValor(
  Dal.BuscarConfiguracaoGeral(conConfig_NaoUtilizaSubstituicaoTributaria)
);
```

**Linha 6518**: Salvamento da configuração
```pascal
Dal.SalvarConfiguracaoGeral(
  conConfig_NaoUtilizaSubstituicaoTributaria, 
  cbxNaoUtilizaSubstituicaoTributaria.AsString, 
  False
);
```

### 3. `Sol.NET/FormEspecias/uFrmConfiguracao.dfm`
**Linha 14105**: Definição visual do componente na aba Fiscal
```pascal
object cbxNaoUtilizaSubstituicaoTributaria: TCheckBoxPlus
  Caption = 'Não utiliza Substituição Tributária(NCM)'
  TabOrder = 6
end
```

### 4. `Sol.NET/Form/uFrmCadastroNCM.pas`
**Linhas 228 e 329**: Lógica de validação condicional

**Linha 228**: Busca configuração
```pascal
var NaoUtilizaST := Dal.BuscarConfiguracaoGeral(
  conConfig_NaoUtilizaSubstituicaoTributaria
) = '1';
```

**Linha 329**: Validação condicional do MVA
```pascal
if (not NaoUtilizaST) and (txtCadICMSCST.AsString = '060') and (txtMVA2.AsFloat = 0) then
begin
  Geral.Men(txtCadICMSCST.EditLabel.Caption + ': 060' + BR + 
    'Obrigatório Ter ' + txtMVA2.EditLabel.Caption + '  ');
  Geral.SetFocus(txtMVA2);
  Exit;
end;
```

---

## Como Utilizar

### Para Ativar a Funcionalidade
1. Acesse: **Menu Principal → Configuração Geral**
2. Clique na aba **Fiscal**
3. Localize o checkbox **"Não utiliza Substituição Tributária(NCM)"**
4. **Marque** o checkbox
5. Clique em **Salvar**

### Resultado
- Ao cadastrar/editar um NCM com CST = 060, o sistema **não exigirá** mais o preenchimento do MVA
- A validação será ignorada e o registro poderá ser salvo normalmente

### Para Manter Comportamento Padrão
- Deixe o checkbox **desmarcado** (comportamento original do sistema)
- MVA continuará obrigatório quando CST = 060

---

## Armazenamento da Configuração

- **Tabela**: `CONFIGURACAO_GERAL`
- **Chave**: `NaoUtilizaSubstituicaoTributaria`
- **Valores possíveis**:
  - `'0'` ou vazio: Valida MVA (padrão)
  - `'1'`: Não valida MVA

---

## Compatibilidade e Retroatividade

✅ **Totalmente compatível com versões anteriores**
- Por padrão (checkbox desmarcado), o sistema mantém o comportamento original
- Clientes que não ativarem a configuração não terão nenhuma alteração no funcionamento
- Sem impacto em processos fiscais existentes

---

## Testes Sugeridos

### Cenário 1: Validação Ativa (Padrão)
1. Deixar checkbox desmarcado
2. Cadastrar NCM com CST = 060 e MVA vazio
3. **Resultado esperado**: Sistema exibe mensagem de erro

### Cenário 2: Validação Desativada
1. Marcar checkbox "Não utiliza Substituição Tributária(NCM)"
2. Salvar configuração
3. Cadastrar NCM com CST = 060 e MVA vazio
4. **Resultado esperado**: Sistema permite salvar sem erro

### Cenário 3: Outros CST
1. Cadastrar NCM com CST diferente de 060
2. **Resultado esperado**: Validação não é aplicada (comportamento normal)

---

## Informações Técnicas

- **Branch**: `7425-244626-solnet--verificar-a-situacao-do-mva-para-nao-ser-obrigatorio-ao-cliente-quando-utilizar-o-0`
- **Compilação**: Testada e aprovada
- **Padrão utilizado**: Segue mesmo padrão de outras configurações fiscais (cbxDIFAL, cbxPreecherPisCofinsNFe)
- **Componente**: TCheckBoxPlus com auditoria desativada

---

**Data**: 17/11/2025  
**Issue**: #7425  
**Tipo**: Feature - Configuração Opcional
