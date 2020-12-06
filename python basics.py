############# Python Básico ################

###Operadores Básicos

# Soma e Subtração: + -
# divisão: /
# multiplicação: *
# Exponencial a: **
# Modulo: %

###Tipos de dados

# float: número com decimais
# int: Número inteiro
# str: Cadeias de texto
# bool: Booleano (V/F)

###Operadores básicos podem fucionar com str também

### Atribuição de variáveis "=" / No R é "==", "=" teste de boolean

### Converter as variáveis use o nome delas, ex: str("3.14")

### Listas podem ser compostas por qualquer objeto

#Subset de uma lista pode ser negativo, ou positivo. Por exemplo: x[-1] -> "d" / x[0] = "a". Toda lista começa por 0 

x = ["a","b","c","d"]
print(x[-1])
print(x[0])

# [start:end] sendo start inclusivo e end exclusivo

print(x[1,-1])

# Listas podem ser formadas por outras listas, 

z = [x,[1,2,3,4]]

# Posso substituir valores dentro da lista também

z[1][1] = 1 <- substituo o valor antigo da minha lista

# Posso adicionar mais elementos a lista também

z_expand = z + [True, True, False, True]

# Apagando elementos da lista

del(z_expand[2])

# Igualando uma varíavel a outra, elas criam uma relação interdepente, a melhor forma para igualar uma varíavel a outra sem afetar o valor das suas entrar é usar um dos seguintes comandos:

z2 = list(z)

#ou

z2 = z[:]

# Possível usar dois comandos numa mesma linhas, como demonstrado abaixo:

#comando1; comando2

#comando1
#comando2

# As duas formas funcionam no python

# funções: Códigos que geram uma função específica

g = [1,2,3,1,2,6,0,1.33]

max(g)
round (g[-1] , 1)
len(g)
int(g[-1]
#*help(): Descreve a função e seus atributos
sorted(g)

# métodos: funções que pertencem aos objetos

me = "mat"

x.index("x")
me.capitalize
me.replace("t","th")
x.append("e")
me.upper
me.index("T")
x.count
x.reverse

### pacotes

# pacotes são compostos por módulos que são scripts feitos por outros usuários

import pandas as pd

from numpy import array

# numpy pode fazer contas com arrays diferente do sistema sozinho

# numpy arrays só trabalho com um tipo

#.shape : da os números de linhas e colunas

#np_2d[0][2] = np_2d[0,2]

#np.mean(np_city[0,:])

#np.median

#np.corrcoef

#np.std

#np.sum

#np.sort

#np.random.normal(180,15,1000)

#corr = np.corrcoef(np_baseball[:,0],np_baseball[:,1])
