# FUNÇÕES QUE PODEM AJUDAR NA CONVERSÃO #


## SqlOrigem.Concat ##

  **Contexto:** 
No módulo SolNET_Conversao, ao configurar a conversão de dados usando o Builder fluente (ConversaoBuilder), é preciso garantir que comandos SQL 
sejam compatíveis com Firebird e SQL Server. Por isso, existe o helper SqlOrigem.Concat, que padroniza a forma de construir expressões de concatenação.


*ANTES*

 ```pascal
.AddCampo('Email', 'Email1 + '';'' + EmailNFE')
```
- Utiliza o operador + para concatenar campos SQL.
- Isso funciona apenas no SQL Server, pois no Firebird o operador de concatenação é ||.
- Problema: Usar 'Email1 + '';'' + EmailNFE' não é compatível com Firebird.

*DEPOIS*

```pascal
.AddCampo('Email', SqlOrigem.Concat(['Email1', 'EmailNFE'], ';'))
```

- Aqui usamos o helper **SqlOrigem.Concat**.
- Ele recebe um array de campos (`['Email1', 'EmailNFE']`) e o separador (`';'`).
- O método gera automaticamente o SQL correto para o banco de dados em uso:
  - SQL Server: `Email1 + ';' + EmailNFE`
  - Firebird: `Email1 || ';' || EmailNFE`
- Vantagem: O mesmo código funciona tanto em Firebird quanto em SQL Server, pois o helper adapta a sintaxe conforme o SGBD.



**Resumo Visual**

| SGBD | Sintaxe correta para concatenação | 
|----------|----------|
| SQL Server	  | campo1 + ';' + campo2| 
| Firebird  | 	`campo1  |

**Conclusão**
Sempre que precisar concatenar campos no SQL de conversão, use SqlOrigem.Concat, nunca operadores fixos (+ ou ||) diretamente no texto do campo.
Assim, seu código fica compatível, padronizado e mais seguro!

Se precisar de exemplos práticos, só pedir!
