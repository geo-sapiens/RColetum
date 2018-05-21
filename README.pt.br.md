# RColetum

Um pacote R para obter informações de [COLETUM](https://coletum.com).

## Pré-requisitos
Para poder usar este pacote, primeiro de tudo, você precisa acessar a sua conta
Coletum [https://coletum.com] do seu navegador para gerar o seu token 
autenticado na página Webservice.

Se você não tiver uma conta, [INSCREVA-SE AGORA](https://coletum.com/register/).

Para obter instruções de como gerar seu token, visite a
[Documentação da API](https://coletum.docs.apiary.io/).

## Como instalar o RColetum
Instale este pacote diretamente em R, usando o pacote 'devtools':

```{r}
install.packages ("devtools")
devtools :: install_github ("geo-sapiens / RColetum")
```
## Como usar o RColetum
Nesta primeira versão do pacote, estão disponíveis três funções principais.
São essas funções que lhe permitem obter seus principais dados do 
[COLETUM](https://coletum.com):

### Obter meus formulários
* `GetForms` permite obter as informações principais sobre cada formulário 
disponível na sua conta;
```{r}
myForms <- GetForms ("YOUR_TOKEN_HERE")
```

* `GetFormStructure` esta função obtém a estrutura de um formulário específico. 
Essa estrutura contém informações sobre os nomes, tipos,  hierarquia e outros 
de cada questão, isso pode ser usado para obter as respostas posteriormente;
```{r}
myFormStructure <- GetFormStructure ("YOUR_TOKEN_HERE", FORM_ID)
```

* `GetAnswers` retorna um ou mais data frames com todas as respostas de um
formulário específico.
```{r}
myAnswers <- GetAnswers ("YOUR_TOKEN_HERE", ,FORM_NAME)
```

## Versioning
Usamos [SemVer](http://semver.org/) para controle de versão. Para as versões 
disponíveis, veja as
[tags neste repositório](https://github.com/geo-sapiens/RColetum/tags).
