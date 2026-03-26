context("GetFormStructure")

test_that("error by wrong token", {
  expect_error(
    GetFormStructure("notexisttoken", 5705),
    "Error 401: O token informado não é válido para o acesso."
  )
  expect_error(
    GetFormStructure("", 5705),
    "Error 401: Unauthorized"
  )
})

test_that("error by wrong idForm or nameForm", {
  expect_error(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg", 5715),
    "Form not found."
  )

  expect_error(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg",
                     nameForm = "RColetum Test - NaN"),
    "Name not found."
  )

  expect_warning(
    expect_error(
      GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg",
                       5841,
                       "RColetum Test - NaN"),
      "Form not found."
    )
  )

  expect_warning(
    expect_error(
      GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg",
                       5715,
                       "RColetum Test - Iris"),
      "Form not found."
    )
  )

  expect_error(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg"),
    "IdForm or nameForm should be provided."
  )

})

test_that("Warming when is informed the idForm and nameForm", {
  expect_warning(
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg",
                     5705,
                     "RColetum Test - Iris"),
    "The idForm and nameForm are provided. Ignoring the nameForm."
  )
})

test_that("error by duplicated form name", {
  expect_error(
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               nameForm = "RColetum Test - Westeros")
  )
})


test_that("GetFormStructure in simple form", {
  # Create the data frame to compare (long command creation)
  ## Created using dput()
  myExpectedFormStructure <-
    structure(list(label = c("Specie", "Sepal", "Petal"), componentId = c("specie66137",
    "sepal66138", "petal66141"), type = c("selectfield", "group",
    "group"), helpBlock = c(NA, NA, NA), order = c("0", "1", "2"),
        options = list(c("setosa", "versicolor", "virginica"), NULL,
            NULL), visibility = structure(list(type = c("FIXED",
        "FIXED", "FIXED"), value = c(TRUE, TRUE, TRUE)), class = "data.frame", row.names = c(NA,
        3L)), minimum = structure(list(type = c("FIXED", "FIXED",
        "FIXED"), value = c(0L, 0L, 0L)), class = "data.frame", row.names = c(NA,
        3L)), maximum = c(1L, 1L, 1L), widget = c("select", NA, NA
        ), components = list(NULL, structure(list(label = c("Length",
        "Width"), componentId = c("length66139", "width66140"), type = c("floatfield",
        "floatfield"), helpBlock = c(NA, NA), order = c("0", "1"),
            options = c(NA, NA), visibility = structure(list(type = c("FIXED",
            "FIXED"), value = c(TRUE, TRUE)), class = "data.frame", row.names = 1:2),
            minimum = structure(list(type = c("FIXED", "FIXED"),
                value = c(0L, 0L)), class = "data.frame", row.names = 1:2),
            maximum = c(1L, 1L), widget = c(NA, NA), components = c(NA,
            NA)), class = "data.frame", row.names = 1:2), structure(list(
            label = c("Length", "Width"), componentId = c("length66142",
            "width66143"), type = c("floatfield", "floatfield"),
            helpBlock = c(NA, NA), order = c("0", "1"), options = c(NA,
            NA), visibility = structure(list(type = c("FIXED", "FIXED"
            ), value = c(TRUE, TRUE)), class = "data.frame", row.names = 1:2),
            minimum = structure(list(type = c("FIXED", "FIXED"),
                value = c(0L, 0L)), class = "data.frame", row.names = 1:2),
            maximum = c(1L, 1L), widget = c(NA, NA), components = c(NA,
            NA)), class = "data.frame", row.names = 1:2))), class = "data.frame", row.names = c(NA,
    3L))

  myFullFormStructure <-
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
  expect_equal(myFullFormStructure, myExpectedFormStructure)

  myFormStructureFiltered <-
    GetFormStructure(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                     idForm = 5705,
                     componentId = "width66140")
  myFormStructureFiltered2 <-
    dplyr::filter(myExpectedFormStructure$components[[2]],
                  componentId == "width66140")
  expect_equal(myFormStructureFiltered, myFormStructureFiltered2)

  })

test_that("GetFormStructure in complex and nested form", {
  # Create the data frame to compare (very long command creation)
  ## Created using dput()
  myExpectedComplexNestedFormStructure <-
    structure(list(label = c("Artist name", "Active", "Origin localition",
    "Members", "Music"), componentId = c("artistName66429", "active66430",
    "originLocalition66431", "members66433", "music66436"), type = c("textfield",
    "agreementfield", "group", "group", "group"), helpBlock = c(NA,
    NA, NA, NA, NA), order = c("0", "1", "2", "3", "4"), options = c(NA,
    NA, NA, NA, NA), visibility = structure(list(type = c("FIXED",
    "FIXED", "FIXED", "FIXED", "FIXED"), value = c(TRUE, TRUE, TRUE,
    TRUE, TRUE)), class = "data.frame", row.names = c(NA, 5L)), minimum = structure(list(
        type = c("FIXED", "FIXED", "FIXED", "FIXED", "FIXED"), value = c(0L,
        0L, 0L, 0L, 0L)), class = "data.frame", row.names = c(NA,
    5L)), maximum = c(1L, 1L, 1L, 1L, 1L), widget = c(NA, NA, NA,
    NA, NA), components = list(NULL, NULL, structure(list(label = "Country",
        componentId = "country66432", type = "selectfield", helpBlock = NA,
        order = "0", options = list(c("Afeganistão", "Ilhas Aland",
        "Albânia", "Argélia", "Samoa Americana", "Andorra", "Angola",
        "Anguilla", "Antártida", "Antígua e Barbuda", "Argentina",
        "Armênia", "Aruba", "Ilha de Ascensão", "Austrália", "Áustria",
        "Azerbaijão", "Bahamas", "Bahrein", "Bangladesh", "Barbados",
        "Bielorrússia", "Bélgica", "Belize", "Benin", "Bermudas",
        "Butão", "Bolívia", "Bósnia e Herzegovina", "Botsuana",
        "Brasil", "Território Britânico do Oceano Índico", "Ilhas Virgens Britânicas",
        "Brunei", "Bulgária", "Burquina Faso", "Burundi", "Camboja",
        "Camarões", "Canadá", "Ilhas Canárias", "Cabo Verde",
        "Países Baixos Caribenhos", "Ilhas Cayman", "República Centro-Africana",
        "Ceuta e Melilha", "Chade", "Chile", "China", "Ilha Christmas",
        "Ilhas Cocos (Keeling)", "Colômbia", "Comores", "Congo - Brazzaville",
        "Congo - Kinshasa", "Ilhas Cook", "Costa Rica", "Costa do Marfim",
        "Croácia", "Cuba", "Curaçao", "Chipre", "Tchéquia", "Dinamarca",
        "Diego Garcia", "Djibuti", "Dominica", "República Dominicana",
        "Equador", "Egito", "El Salvador", "Guiné Equatorial", "Eritreia",
        "Estônia", "Etiópia", "Ilhas Malvinas", "Ilhas Faroe",
        "Fiji", "Finlândia", "França", "Guiana Francesa", "Polinésia Francesa",
        "Territórios Franceses do Sul", "Gabão", "Gâmbia", "Geórgia",
        "Alemanha", "Gana", "Gibraltar", "Grécia", "Groenlândia",
        "Granada", "Guadalupe", "Guam", "Guatemala", "Guernsey",
        "Guiné", "Guiné-Bissau", "Guiana", "Haiti", "Honduras",
        "Hong Kong, RAE da China", "Hungria", "Islândia", "Índia",
        "Indonésia", "Irã", "Iraque", "Irlanda", "Ilha de Man",
        "Israel", "Itália", "Jamaica", "Japão", "Jersey", "Jordânia",
        "Cazaquistão", "Quênia", "Quiribati", "Kosovo", "Kuwait",
        "Quirguistão", "Laos", "Letônia", "Líbano", "Lesoto",
        "Libéria", "Líbia", "Liechtenstein", "Lituânia", "Luxemburgo",
        "Macau, RAE da China", "Macedônia", "Madagascar", "Malaui",
        "Malásia", "Maldivas", "Mali", "Malta", "Ilhas Marshall",
        "Martinica", "Mauritânia", "Maurício", "Mayotte", "México",
        "Micronésia", "Moldávia", "Mônaco", "Mongólia", "Montenegro",
        "Montserrat", "Marrocos", "Moçambique", "Mianmar (Birmânia)",
        "Namíbia", "Nauru", "Nepal", "Holanda", "Nova Caledônia",
        "Nova Zelândia", "Nicarágua", "Níger", "Nigéria", "Niue",
        "Ilha Norfolk", "Coreia do Norte", "Ilhas Marianas do Norte",
        "Noruega", "Omã", "Paquistão", "Palau", "Territórios palestinos",
        "Panamá", "Papua-Nova Guiné", "Paraguai", "Peru", "Filipinas",
        "Ilhas Pitcairn", "Polônia", "Portugal", "Porto Rico", "Catar",
        "Reunião", "Romênia", "Rússia", "Ruanda", "Samoa", "San Marino",
        "São Tomé e Príncipe", "Arábia Saudita", "Senegal", "Sérvia",
        "Seicheles", "Serra Leoa", "Singapura", "Sint Maarten", "Eslováquia",
        "Eslovênia", "Ilhas Salomão", "Somália", "África do Sul",
        "Ilhas Geórgia do Sul e Sandwich do Sul", "Coreia do Sul",
        "Sudão do Sul", "Espanha", "Sri Lanka", "São Bartolomeu",
        "Santa Helena", "São Cristóvão e Névis", "Santa Lúcia",
        "São Martinho", "São Pedro e Miquelão", "São Vicente e Granadinas",
        "Sudão", "Suriname", "Svalbard e Jan Mayen", "Suazilândia",
        "Suécia", "Suíça", "Síria", "Taiwan", "Tadjiquistão",
        "Tanzânia", "Tailândia", "Timor-Leste", "Togo", "Tokelau",
        "Tonga", "Trinidad e Tobago", "Tristão da Cunha", "Tunísia",
        "Turquia", "Turcomenistão", "Ilhas Turks e Caicos", "Tuvalu",
        "Ilhas Menores Distantes dos EUA", "Ilhas Virgens Americanas",
        "Uganda", "Ucrânia", "Emirados Árabes Unidos", "Reino Unido",
        "Estados Unidos", "Uruguai", "Uzbequistão", "Vanuatu", "Cidade do Vaticano",
        "Venezuela", "Vietnã", "Wallis e Futuna", "Saara Ocidental",
        "Iêmen", "Zâmbia", "Zimbábue")), visibility = structure(list(
            type = "FIXED", value = TRUE), class = "data.frame", row.names = 1L),
        minimum = structure(list(type = "FIXED", value = 1L), class = "data.frame", row.names = 1L),
        maximum = 1L, widget = "select", components = NA), class = "data.frame", row.names = 1L),
    structure(list(label = c("Active", "Past"), componentId = c("active66434",
    "past66435"), type = c("textfield", "textfield"), helpBlock = c(NA,
    NA), order = c("0", "1"), options = c(NA, NA), visibility = structure(list(
        type = c("FIXED", "FIXED"), value = c(TRUE, TRUE)), class = "data.frame", row.names = 1:2),
        minimum = structure(list(type = c("FIXED", "FIXED"),
            value = c(0L, 0L)), class = "data.frame", row.names = 1:2),
        maximum = c(NA, NA), widget = c(NA, NA), components = c(NA,
        NA)), class = "data.frame", row.names = 1:2), structure(list(
        label = "Album", componentId = "album66437", type = "group",
        helpBlock = NA, order = "0", options = NA, visibility = structure(list(
            type = "FIXED", value = TRUE), class = "data.frame", row.names = 1L),
        minimum = structure(list(type = "FIXED", value = 0L), class = "data.frame", row.names = 1L),
        maximum = NA, widget = NA, components = list(structure(list(
            label = c("Name", "Year", "Genres", "Members"), type = c("textfield",
            "integerfield", "relationalfield", "group"), componentId = c("name66438",
            "year66439", "genres66441", "members66443"), helpBlock = c(NA,
            NA, NA, NA), order = c("0", "1", "2", "3"), options = list(
                NULL, NULL, c("1.121091 - Arena rock", "1.121099 - Art rock",
                "1.121011 - Blues rock", "1.121014 - Boogie rock",
                "1.121015 - Country rock", "1.121114 - Experimental rock",
                "1.121090 - Glam metal", "1.121012 - Hard rock",
                "1.121017 - Heavy metal", "1.121092 - Pop rock",
                "1.121098 - Rock progressive", "1.121010 - Southern rock"
                ), NULL), visibility = structure(list(type = c("FIXED",
            "FIXED", "FIXED", "FIXED"), value = c(TRUE, TRUE,
            TRUE, TRUE)), class = "data.frame", row.names = c(NA,
            4L)), minimum = structure(list(type = c("FIXED",
            "FIXED", "FIXED", "FIXED"), value = c(1L, 1L, 1L,
            0L)), class = "data.frame", row.names = c(NA, 4L)),
            maximum = c(1L, 1L, NA, NA), widget = c(NA, NA, NA,
            NA), components = list(NULL, NULL, NULL, structure(list(
                label = c("Name", "Instruments"), componentId = c("name66444",
                "instruments66446"), type = c("textfield", "relationalfield"
                ), helpBlock = c(NA, NA), order = c("0", "1"),
                options = list(NULL, c("1.121121 - ARP String Ensemble V",
                "1.121022 - Backing vocals", "1.121025 - Bagpipes",
                "1.121023 - Bass guitar", "1.121020 - Drums",
                "1.121119 - EMS Synthi AKS", "1.121124 - EMS VCS 3",
                "1.121096 - Electric lead guitar", "1.121108 - Electric piano",
                "1.121094 - Finger cymbals", "1.121031 - Guitar",
                "1.121120 - Hammond C-3 organ", "1.121125 - Hohner Clavinet D6",
                "1.121095 - Horns", "1.121028 - Keyboards", "1.121118 - Lap steel guitar",
                "1.121019 - Lead guitar", "1.121040 - Lead vocals",
                "1.121122 - Minimoog", "1.121107 - Organ", "1.121027 - Percussion",
                "1.121026 - Piano", "1.121021 - Rhythm guitar",
                "1.121029 - Saxophone", "1.121041 - Slide guitar",
                "1.121123 - Steinway piano", "1.121034 - Synthesizers",
                "1.121105 - Synthi AKS", "1.121103 - Tape effects",
                "1.121106 - VCS 3", "1.121032 - Vocals", "1.121126 - Wurlitzer EP-200 electric piano"
                )), visibility = structure(list(type = c("FIXED",
                "FIXED"), value = c(TRUE, TRUE)), class = "data.frame", row.names = 1:2),
                minimum = structure(list(type = c("FIXED", "FIXED"
                ), value = c(1L, 1L)), class = "data.frame", row.names = 1:2),
                maximum = c(1L, NA), widget = c(NA, NA), components = c(NA,
                NA)), class = "data.frame", row.names = 1:2))), class = "data.frame", row.names = c(NA,
        4L)))), class = "data.frame", row.names = 1L))), class = "data.frame", row.names = c(NA,
    5L))

  myFullFormStructure <-
    GetFormStructure("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)
  expect_equal(myExpectedComplexNestedFormStructure, myFullFormStructure)

  myFilteredFormStructure <-
    GetFormStructure(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                     idForm = 5722,
                     componentId = "past66435")
  myFilteredFormStructure2 <-
    dplyr::filter(myExpectedComplexNestedFormStructure$components[[4]],
                  componentId == "past66435")
  expect_equal(myFilteredFormStructure, myFilteredFormStructure2)

})
