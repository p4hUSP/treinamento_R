library(tidyverse)
library(magrittr)

# fonte: https://www.kaggle.com/claudiodavi/superhero-set/version/1#heroes_information.csv
data <- read_csv("manipulacao_de_dados/heroes_information.csv")

glimpse(data)

# Vamos comecar arrumando o nome de algumas variaveis
## No R, é muito ruim trabalhar com variaveis como
## "Eye color", sobretudo, por conta do espaço

### Uma opcao, caso nao tenhamos muitas vavriaveis é mudar todas 
### com uma funcao chamada set_names do pacote magrittr
### porem eh necessario colocar todas as variaveis
data %>% 
  set_names(c("ID", "name", "gender", "eye_color", "race", "hair_color", "height", "publisher", "skin_color", "alignment", "weight"))

### outra forma eh utilizarmos uma funcao do dplyr
### chamada rename
data %>% 
  rename("eye_color" = `Eye color`,
         "hair_color" = `Hair color`,
         "skin_color" = `Skin color`)

### Podemos utilizar um pouco mais de programacao nisso
data <- data %>%  {
  nome_variaveis <- names(.) %>% # a funcao names obtem o nome das variaveis
    str_to_lower() %>%  # str_to_lower transforma tudo em minusculo
    str_replace_all(" ", "_") %>% # str_replace_all, neste caso, substitui todo espaco por underline
    str_replace("x1", "id") # str_replace substitui x1 por id
  set_names(., nome_variaveis) # atribuimos os novos nomes
}

head(data)

# Precisamos tambem alterar as categorias dos valores missings
## Dados em formato de texto (character) estao categorizados como "-"
## Enquanto que dados em formato númerico "-99"

### Para fazermos isso utilizamos mutate_if
### cujos argumentos sao:
### 1. Condicao (no nosso caso se for um caracter ou se for um numero)
### 2. Uma funcao que ira realizar as alterar no dado
#### Para este segundo caso, utilizamos o auxilio de uma funcao chamada funs
#### que permite colocarmos, de forma mais flexiva, a nossa funcao para alterar o dado

data <- data %>% 
  mutate_if(is.character, funs(ifelse(str_detect(., "-") == T & str_length(.) == 1, NA_character_, .))) # perceba que is.character eh uma funcao porem neste caso nao precisamos utilizar o ()

data <- data %>% 
  mutate_if(is.numeric, funs(ifelse(. == -99, NA_integer_, .)))

# Vamos criar variaveis, que tal o imc dos herois?
## Porem a altura esta em cm (precisamos dela em m)
## e o pesso em libras (precisamos dele em kg) - 1 libra = 0,453592 kg
data <- data %>% 
  mutate(weight_kg = weight * 0.453592,
         height_m = height * 0.01,
         imc = weight_kg/(height_m^2))

# Vamos aprender mais uma funcao, dessa vez para categorizar de acordo com varias condicoes
## a funcao se chama case_when e eh utilizada junto com o mutate
data <- data %>% 
  mutate(imc_categoria = case_when(imc < 17 ~ "Muito abaixo do peso",
                                   imc >= 17 & imc < 18.49 ~ "Abaixo do peso",
                                   imc >= 18.49 & imc < 24.99 ~ "Peso normal",
                                   imc >= 24.99 & imc < 29.99 ~ "Acima do peso",
                                   imc >= 29.99 & imc < 34.99 ~ "Obesidade I",
                                   imc >= 34.99 & imc < 39.99 ~ "Obesidade II",
                                   imc >= 39.99 ~ "Obesidade III",
                                   T ~ NA_character_)) #  Caso nenum dos casos seja preenchido atribuir como NA

# Se rodarmos o codigo abaixo podemos notar que existem herois com duas racas
# Uma forma de contar quais deles possuem mais de uma raca eh utilizando a funcao
# separate do tidyr

data %>% count(race) %>% View("contagem de racas")

data <- data %>% 
  separate(col = race, into = c("race_1", "race_2"), sep = "/", remove = FALSE, fill = "right")

# Agora podemos criar uma variavel dummy
## que informa que caso race_2 for um NA, o heroi tem apenas uma raca

data <- data %>% 
  mutate(duas_racas = case_when(is.na(race_2) == T ~ FALSE,
                                T ~ TRUE)) %>% select(-race_2)

# vamos fazer filtros diferetes

data %>% filter_if(is.numeric, all_vars(. >= 1))

data %>% filter_if(is.numeric, any_vars(. >= 100))
