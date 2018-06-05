# RColetum
[![Build Status](https://travis-ci.org/geo-sapiens/RColetum.svg)](https://travis-ci.org/geo-sapiens/RColetum)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RColetum)](https://cran.r-project.org/package=RColetum)

Um pacote R para obter dados do [Coletum](https://coletum.com).

## Pré-requisitos
Para poder usar este pacote, primeiro de tudo, você precisa acessar a sua conta 
Coletum [https://coletum.com] do seu navegador para gerar o seu token 
autenticado na página do Web service.

Se você não tiver uma conta, [INSCREVA-SE AGORA](https://coletum.com/pt_BR/register/).

## Como instalar o RColetum
Instale este pacote diretamente no R, usando o pacote 'devtools':

```{r}
install.packages("devtools")
devtools::install_github("geo-sapiens/RColetum")
```
## Como usar o RColetum
Nesta versão do pacote, existem três funções principais disponíveis,
que permitem obter seus principais dados do [Coletum](https://coletum.com):

### Obter meus formulários
* `GetForms` essa função obtém a lista de formulários da sua conta.

```{r}
meusFormularios <- GetForms("TOKEN_HERE")
```
### Obter a estrutura de um formulário
* `GetFormStructure` essa função obtém a estrutura de um formulário específico.
A estrutura contém as especificações de cada campo, como o nome, tipo,
hierarquia e outros.

```{r}
meusFormulariostructure <- GetFormStructure(token = "TOKEN_HERE", idForm = FORM_ID)
```
### Obter as respostas 
* `GetAnswers` esta função obtém as respostas de um formulário específico. 
A estrutura de dados retornada depende da estrutura do formulário. Quando o 
formulário não tem campo com cardinalidade maior que 1, a estrutura é um data
frame. Quando o formulário tem um ou mais campos com cardinalidade maior que um,
a estrutura é uma lista de data frames.

```{r}
myAnswers <- GetAnswers(token = "TOKEN_HERE", idForm = FORM_ID)
```

Se você deseja obter as respostas em um único quadro de dados com dados 
redundantes (causado por campos com cardinalidade maior que 1), você deve usar
parâmetro `singleDataFrame` como TRUE.

```{r}
myAnswers <- GetAnswers(token = "TOKEN_HERE", 
                        idForm = FORM_ID, 
                        singleDataFrame = TRUE)
```

## Examplo completo
```{r}
install.packages("devtools")
devtools::install_github("geo-sapiens/RColetum")

####@> Carregando o pacote RColetum
library(RColetum)

####@> Criando uma variável para armazenar meu token da API Coletum
####@> Esta variável será usada em todos os métodos abaixo
meuToken <- "cizio7xeohwgc8k4g4koo008kkoocwg"

####@> Obtendo meus formulários
meusFormularios <- GetForms(meuToken)

####@> Obtendo a estrutura do formulário usando o ID do formulário
starWarsEstruturaFormulario <- GetFormStructure(token = meuToken,
                                                idForm = 5713)

####@> Obtendo estrutura do formulário usando o nome do formulário
starWarsEstruturaFormulario <-
  GetFormStructure(token = meuToken,  
                   nameForm = "RColetum Test - Star Wars")

####@> Obtendo respostas para um formulário usando o ID do formulário
starWarsRespostas <- GetAnswers(token = meuToken, 
                                idForm = 5713)

####@> Obtendo respostas para um formulário usando o nome do formulário
####@> Neste caso, temos X + 1 dataframes, onde X é o número de campos N
####@> (campos com cardinalidade> 1)
starWarsRespostas <- GetAnswers(token = meuToken, 
                                nameForm = "RColetum Test - Star Wars")

####@> Obtendo respostas de um formulário obtendo resultado como um único 
####@> dataframe
####@> Neste caso, temos redundância para N campos
starWarsRespostasUnicoDataframe <- GetAnswers(token = meuToken, 
                                              idForm = 5713, 
                                              singleDataFrame = TRUE)


####@> VAMOS TER ALGUMA DIVERSÃO E MOSTRAR UM GRÁFICO COM IMC (ÍNDICE DE MASSA
####@> CORPORAL) DE CADA PERSONAGEM DE STAR WARS
library(ggplot2)

meuGrafico <- ggplot(data = starWarsRespostas[[1]], 
                     mapping = 
                       aes(x = name66298, 
                           y = (mass66300) / (height66299/100)^2 )) +
  geom_bar(stat = "identity", 
           fill = "black", 
           colour = "green",
           alpha = 0.8) +
  geom_label(mapping = aes(label = (mass66300) / (height66299/100)^2)) +
  labs(x = "Character", 
       y = "IMC") +
  theme_bw(base_size = 14)

meuGrafico

```

## Versionamento
Usamos [SemVer](http://semver.org/) para controle de versão. Para as versões 
disponíveis, veja as
[tags neste repositório](https://github.com/geo-sapiens/RColetum/tags).

-----
Observe que este projeto é lançado com um [Código de Conduta do 
Colaborador](CODE_OF_CONDUCT.md).
Ao participar deste projeto, você concorda em cumprir seus termos.
