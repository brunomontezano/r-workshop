#### R como uma calculadora ####

# Nota: as linhas de código que iniciam com um `#` indicam um comentário.
# O R não interpreta essas linhas como código, logo, não são interpretadas
# pelo console como comandos que devem ser executados.

# Quanto dá 25 vezes 11?
25 * 11

# Qual o resultado de 111 + 222 + 333?
111 + 222 + 333

# Raíz quadrada de 81
sqrt(81)

# Solicitar ajuda sobre a função sqrt()
# Não recomendo inserir pedidos de ajuda ?nome_da_funçao nos scripts, e sim no console
# Nesse caso, foi inserido para fins didáticos
?sqrt

#### Objetos ####

# Criando um novo objeto
# Através do operador de atribuição (<-), atribuímos um valor ao nome de um objeto
novo_objeto <- 144

# Após a criação do objeto, observe o seu Ambiente (na direita superior) e veja
# que ele aparece lá

# Para mostrar um objeto criado, podemos simplesmente digitar o nome dele
novo_objeto

# Podemos utilizar o objeto criado de várias formas, por exemplo:

# Para somar 10 ao valor do objeto
novo_objeto + 10

# Para somar o objeto com ele mesmo
novo_objeto + novo_objeto

# Para solicitar a raíz quadrada do objeto
sqrt(novo_objeto)

# Para elevar o objeto ao quadrado
novo_objeto ^ 2

# Para pedir o módulo na divisão por 2 (resto da divisão)
novo_objeto %% 2

#### Vetores ####

# Uma estrutura muito usada no R são os vetores (séries de elementos)

# Para criar um novo vetor, usamos o mesmo operador de atribuição (<-)
# E também usamos a função c() para "combinar" ou "concatenar" elementos
novo_objeto <- c(4, 9, 16, 25, 36)

novo_objeto

# Calcular raíz quadrada para cada elemento do vetor
sqrt(novo_objeto) 

# Calcular a média do vetor
mean(novo_objeto)


# Somar todos os elementos do vetor
sum(novo_objeto)

#### Classes de vetores ####

# Vetor numérico
vetor_numerico <- 215
vetor_numerico

# Vetor de caractere
vetor_de_caractere <- c("Bruno", "Montezano")
vetor_de_caractere

# Vetor lógico
vetor_logico <- c(TRUE, FALSE, FALSE, TRUE)
vetor_logico

# Os vetores podem possuir apenas um elemento, como no `vetor_numerico` criado acima.

#### Indexando vetores por posição ####

# Criando um vetor de exemplo
vetor_exemplo <- c(2, 13, 22, 95)

# Dessa forma, extraímos apenas o primeiro e o terceiro elemento do vetor
vetor_exemplo[c(1, 3)]

# Criando vetor de animais
animais <- c("Gato", "Cachorro", "Vaca", "Zebra", "Gorila", "Avestruz", "Cabra")

# Extrair apenas do elemento 3 até o elemento 5
animais[3:5]

# Extrair apenas o "melhor amigo do homem"
animais[2]

#### Indexando vetores por expressões ####

# Mostrar/chamar o vetor de exemplo
vetor_exemplo

# Extrair apenas os elementos de valor maior que 15
vetor_exemplo[vetor_exemplo > 15]

# Extrair elementos de valor menor ou igual a 13
vetor_exemplo[vetor_exemplo <= 13]

# Extrair valores maiores que 5 E menores que 15
vetor_exemplo[vetor_exemplo > 5 & vetor_exemplo < 15]

# Extrair valores menores que 5 OU menores que 15
vetor_exemplo[vetor_exemplo > 5 | vetor_exemplo < 15]

# Elementos que são iguais a algum número de 1 até 15
vetor_exemplo[vetor_exemplo %in% c(1:15)]

# Elementos que NÃO são iguais a algum número de 1 até 15
vetor_exemplo[!vetor_exemplo %in% c(1:15)]

#### Data frames ####

# Criando um data frame da aula
df_aula <- data.frame(
  nome = c("Bruno", "Marcos", "Pedro", "José"),
  idade = c(18, 22, 25, 21),
  profissao = c("Psicólogo", "Professor", "Jogador de futebol", "Bombeiro"),
  tem_namorada = c("Sim", "Sim", "Não", "Sim"),
  gosta_de_r = c("Sim", "Não", "Não", "Não")
)

# Mostrando o data frame a partir de seu nome
df_aula

#### Subconjuntos de data frames por posição ####

# Criando subconjuntos de data.frames através do formato dados[linha, coluna]

# Extraindo da segunda até a quarta linha e a primeira e terceira coluna
# O operador dois pontos (:) cria um vetor de sequência do primeiro número até o segundo
# Nesse caso, 2:4 é igual a c(2, 3, 4)
df_aula[2:4, c(1, 3)]

# Extraindo a primeira e a quarta linha e todas as colunas da base de dados
# Quando deixamos em branco as posições das linhas ou colunas, retornamos
# todos os elementos daquela dimensão
df_aula[c(1, 4), ]

#### Subconjuntos de data frames por expressões ####

# Podemos também criar subconjuntos a partir de expressões lógicas

# Abaixo, extraímos apenas as observações (linhas) em que os sujeitos têm
# namorada
# Notem que a dimensão da coluna está vazia, logo, todas colunas serão retornadas
df_aula[df_aula$tem_namorada == "Sim", ]

# Extrair apenas observações que tenham idade maior que 21 e que não gostam de R
# Notem que a dimensão da coluna está vazia, logo, todas colunas serão retornadas
df_aula[df_aula$gosta_de_r == "Não" & df_aula$idade > 21, ]

#### Operador cifrão ($) ####

# Com o cifrão, podemos retornar uma determinada coluna da base de dados como um vetor

# No exemplo abaixo, retornamos a idade dos sujeitos como um vetor
df_aula$idade

# Então, podemos fazer operações com esse vetor...

# Solicitar a média da idade da coluna `idade` da base de dados `df_aula`
mean(df_aula$idade)

#### Pacotes ####

# Se você precisar instalar o pacote dados, remova o jogo da velha no início da linha abaixo
# install.packages("dados")

# Lembre-se que pacotes só precisam ser instalados uma única vez no seu computador

# Carregar o pacote dados
library(dados)

# Mostrar a base de dados `pixar_bilheteria`
pixar_bilheteria

