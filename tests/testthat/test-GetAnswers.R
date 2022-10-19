context("GetAnswers")

# Create the data frame to compare (long command creation)
## Created using dput()
myExpectedAnswersIrisForm <-
  structure(
    list(answer_id =
           c("1.121804", "1.121803", "1.121802",
             "1.120620", "1.120619", "1.120617"),
         specie66137 = c("virginica", "versicolor", "setosa", "virginica",
                         "versicolor", "setosa"),
         sepal66138.length66139 = c(5.8, 6.4, 4.9, 6.3, 7, 5.1),
         sepal66138.width66140 = c(2.7, 3.2, 3, 3.3, 3.2, 3.5),
         petal66141.length66142 = c(5.1, 4.5, 1.4, 6, 4.7, 1.4),
         petal66141.width66143 = c(1.9, 1.5, 0.2, 2.5, 1.4, 0.2),
         userName = c("André Smaniotto", "André Smaniotto",
                      "André Smaniotto", "André Smaniotto", "André Smaniotto",
                      "André Smaniotto"),
         userId = c(8403L, 8403L, 8403L, 8403L,
                    8403L, 8403L),
         source = c("web_private", "web_private", "web_private",
                    "web_private", "web_private", "web_private"),
         createdAt = c("2018-05-30T14:05:08+0000",
                       "2018-05-30T14:04:30+0000",
                       "2018-05-30T14:02:47+0000",
                       "2018-05-24T20:01:06+0000",
                       "2018-05-24T20:00:39+0000",
                       "2018-05-24T20:00:12+0000"),
         createdAtDevice = c("2018-05-30T14:05:08+0000",
                             "2018-05-30T14:04:30+0000",
                             "2018-05-30T14:02:47+0000",
                             "2018-05-24T20:01:06+0000",
                             "2018-05-24T20:00:39+0000",
                             "2018-05-24T20:00:12+0000"),
         createdAtCoordinates.latitude = c(NA,  NA, NA, NA, NA, NA),
         createdAtCoordinates.longitude = c(NA,  NA, NA, NA, NA, NA),
         updatedAt = c(NA, NA, NA, "2018-05-24T20:03:04+0000", NA, NA),
         updatedAtCoordinates.latitude = c(NA, NA, NA, NA, NA, NA),
         updatedAtCoordinates.longitude = c(NA, NA, NA, NA, NA, NA)),
    row.names = c(NA, 6L),
    class = "data.frame")

# Create the data frame to compare (very long command creation)
## Created using dput()
myExpectedAnswersStormFormMultDF <-
  structure(
    list(
      structure(
        list(
          answer_id = c("1.120992", "1.120990", "1.120989", "1.120988"),
          name66415 = c("Otto", "Bonnie", "Doris", "Amy"),
          userName = c("André Smaniotto", "André Smaniotto", "André Smaniotto",
                       "André Smaniotto"),
          userId = c(8403L, 8403L, 8403L, 8403L),
          source = c("web_private", "web_private", "web_private", "web_private"),
          createdAt = c("2018-05-28T12:25:27+0000", "2018-05-28T12:21:29+0000",
                        "2018-05-28T12:17:12+0000", "2018-05-28T12:16:13+0000"),
          createdAtDevice = c("2018-05-28T12:25:27+0000", "2018-05-28T12:21:29+0000",
                              "2018-05-28T12:17:12+0000", "2018-05-28T12:16:13+0000"),
          createdAtCoordinates.latitude = c(NA, NA, NA, NA),
          createdAtCoordinates.longitude = c(NA, NA, NA, NA),
          updatedAt = c(NA, NA, NA, NA),
          updatedAtCoordinates.latitude = c(NA, NA, NA, NA),
          updatedAtCoordinates.longitude = c(NA, NA, NA, NA)),
        row.names = c(NA, 4L),
        class = "data.frame"),
      list(
        infos66416 = structure(
          list(
            date66417 = c("2010-10-08", "2010-08-10", "2010-09-10", "2010-10-10",
                          "2010-07-22", "2010-07-23", "2010-07-23", "1975-08-29",
                          "1975-06-27", "1975-06-27", "1975-06-28", "1975-06-29"),
            hour66418 = c("06:00", "12:00", "06:00", "00:00", "06:00", "00:00",
                          "14:00", "12:00", "00:00", "06:00", "12:00", "12:00"),
            location66419.longitude = c(-66.09999, -64.8, -59.7, -50.9, -73.8,
                                        -75.9, -80.2, -48.9, -79, -79, -78, -73.8),
            location66419.latitude = c(24.1, 25.2, 28.5, 32.7, 21.5, 23.1, 25.4,
                                       34.9, 27.5, 28.5, 33.29999, 33.79999),
            status66420 = c("Tropical Storm", "Hurricane", "Hurricane",
                            "Tropical Storm", "Tropical Depression",
                            "Tropical Storm", "Tropical Storm", "Tropical Storm",
                            "Tropical Depression", "Tropical Depression",
                            "Tropical Depression", "Tropical Storm"),
            wind66421 = c(60L, 65L, 75L, 60L, 30L, 35L, 35L, 45L, 25L, 25L, 25L, 45L),
            pressure66422 = c(989L, 983L, 978L, 988L, 1009L, 1006L, 1007L, 1005L,
                              1013L, 1013L, 1011L, 1000L),
            ts_diameter66423 = c(241.6638, 264.6794, 299.2028, 299.2028, 0,
                                 46.0312, 92.0624, NA, NA, NA, NA, NA),
            hu_diameter66424 = c(0, 40.2773, 46.0312, 0, 0, 0, 0, NA, NA, NA,
                                 NA, NA),
            answer_id = c("1.120992", "1.120992", "1.120992", "1.120992", "1.120990",
                          "1.120990", "1.120990", "1.120989", "1.120988", "1.120988",
                          "1.120988", "1.120988"),
            infos66416_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                              "11", "12")),
          row.names = c(NA, -12L),
          class = "data.frame"))),
    names = c("", ""))

# Create the data frame to compare (long command creation)
## Created using dput()
myExpectedAnswersStormFormSingleDF <-
  structure(
    list(
      answer.answer_id = c("1.120992", "1.120992", "1.120992", "1.120992",
                           "1.120990", "1.120990", "1.120990", "1.120989",
                           "1.120988", "1.120988", "1.120988", "1.120988"),
      answer.name66415 = c("Otto", "Otto", "Otto", "Otto", "Bonnie", "Bonnie",
                           "Bonnie", "Doris", "Amy", "Amy", "Amy", "Amy"),
      answer.userName = c("André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto"),
      answer.userId = c(8403L, 8403L, 8403L, 8403L, 8403L, 8403L, 8403L, 8403L,
                        8403L, 8403L, 8403L, 8403L),
      answer.source = c("web_private", "web_private", "web_private", "web_private",
                        "web_private", "web_private", "web_private", "web_private",
                        "web_private", "web_private", "web_private", "web_private"),
      answer.createdAt = c("2018-05-28T12:25:27+0000", "2018-05-28T12:25:27+0000",
                           "2018-05-28T12:25:27+0000", "2018-05-28T12:25:27+0000",
                           "2018-05-28T12:21:29+0000", "2018-05-28T12:21:29+0000",
                           "2018-05-28T12:21:29+0000", "2018-05-28T12:17:12+0000",
                           "2018-05-28T12:16:13+0000", "2018-05-28T12:16:13+0000",
                           "2018-05-28T12:16:13+0000", "2018-05-28T12:16:13+0000"),
      answer.createdAtDevice = c("2018-05-28T12:25:27+0000",
                                 "2018-05-28T12:25:27+0000",
                                 "2018-05-28T12:25:27+0000",
                                 "2018-05-28T12:25:27+0000",
                                 "2018-05-28T12:21:29+0000",
                                 "2018-05-28T12:21:29+0000",
                                 "2018-05-28T12:21:29+0000",
                                 "2018-05-28T12:17:12+0000",
                                 "2018-05-28T12:16:13+0000",
                                 "2018-05-28T12:16:13+0000",
                                 "2018-05-28T12:16:13+0000",
                                 "2018-05-28T12:16:13+0000"),
      answer.createdAtCoordinates.latitude = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                               NA, NA, NA, NA),
      answer.createdAtCoordinates.longitude = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                                NA, NA, NA, NA),
      answer.updatedAt = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
      answer.updatedAtCoordinates.latitude = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                               NA, NA, NA, NA),
      answer.updatedAtCoordinates.longitude = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                                NA, NA, NA, NA),
      infos66416.date66417 = c("2010-10-08", "2010-08-10", "2010-09-10", "2010-10-10",
                               "2010-07-22", "2010-07-23", "2010-07-23", "1975-08-29",
                               "1975-06-27", "1975-06-27", "1975-06-28", "1975-06-29"),
      infos66416.hour66418 = c("06:00", "12:00", "06:00", "00:00", "06:00", "00:00",
                               "14:00", "12:00", "00:00", "06:00", "12:00", "12:00"),
      infos66416.location66419.longitude = c(-66.09999, -64.8, -59.7, -50.9,
                                             -73.8, -75.9, -80.2, -48.9, -79,
                                             -79, -78, -73.8),
      infos66416.location66419.latitude = c(24.1, 25.2, 28.5, 32.7, 21.5, 23.1,
                                            25.4, 34.9, 27.5, 28.5, 33.29999,
                                            33.79999),
      infos66416.status66420 = c("Tropical Storm", "Hurricane", "Hurricane",
                                 "Tropical Storm", "Tropical Depression",
                                 "Tropical Storm", "Tropical Storm",
                                 "Tropical Storm", "Tropical Depression",
                                 "Tropical Depression", "Tropical Depression",
                                 "Tropical Storm"),
      infos66416.wind66421 = c(60L, 65L, 75L, 60L, 30L, 35L, 35L, 45L, 25L, 25L,
                               25L, 45L),
      infos66416.pressure66422 = c(989L, 983L, 978L, 988L, 1009L, 1006L, 1007L,
                                   1005L, 1013L, 1013L, 1011L, 1000L),
      infos66416.ts_diameter66423 = c(241.6638, 264.6794, 299.2028, 299.2028, 0,
                                      46.0312, 92.0624, NA, NA, NA, NA, NA),
      infos66416.hu_diameter66424 = c(0, 40.2773, 46.0312, 0, 0, 0, 0, NA, NA,
                                      NA, NA, NA),
      infos66416.infos66416_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
                                   "10", "11", "12")),
    row.names = c(NA, -12L),
    class = "data.frame")

# Create the data frame to compare (long command creation)
## Created using dput()
myExpectedAnswersStarWarsFormMultDF <-
  structure(
    list(
      structure(
        list(
          answer_id = c("1.120986", "1.120985", "1.120984", "1.120978"),
          name66298 = c("R2-D2", "Yoda", "Anakin Skywalker", "Luke Skywalker"),
          height66299 = c(96L, 66L, 188L, 172L),
          mass66300 = c(32L, 17L, 84L, 77L),
          birthYear66380 = c(33, 896, 41.9, 19),
          gender66302 = c("none", "male", "male", "male"),
          specie66303.answerIdFromAnotherForm = c("1.120773", "1.120777",
                                                  "1.120771", "1.120771"),
          specie66303.answerFromAnotherForm = c("Droid", "Yoda's species", "Human",
                                                "Human"),
          userName = c("André Smaniotto", "André Smaniotto", "André Smaniotto",
                       "André Smaniotto"),
          userId = c(8403L, 8403L, 8403L, 8403L),
          source = c("web_private", "web_private", "web_private", "web_private"),
          createdAt = c("2018-05-28T11:44:48+0000", "2018-05-28T11:43:26+0000",
                        "2018-05-28T11:42:22+0000", "2018-05-28T11:27:40+0000"),
          createdAtDevice = c("2018-05-28T11:44:48+0000", "2018-05-28T11:43:26+0000",
                              "2018-05-28T11:42:22+0000", "2018-05-28T11:27:40+0000"),
          createdAtCoordinates.latitude = c(-26.9075, -26.9075, -26.9075, -26.9075),
          createdAtCoordinates.longitude = c(-48.65393, -48.65393, -48.65393, -48.65393),
          updatedAt = c(NA, NA, NA, "2018-05-28T11:32:09+0000"),
          updatedAtCoordinates.latitude = c(NA, NA, NA, -26.9075),
          updatedAtCoordinates.longitude = c(NA, NA, NA, -48.65393)),
        row.names = c(NA, 4L),
        class = "data.frame"),
      list(
        films66304 = structure(
          list(
            answerIdFromAnotherForm = c("1.120782", "1.120786", "1.120784",
                                        "1.120787", "1.120783", "1.120788",
                                        "1.120789", "1.120785", "1.120782",
                                        "1.120786", "1.120787", "1.120783",
                                        "1.120785", "1.120786", "1.120787",
                                        "1.120785", "1.120782", "1.120784",
                                        "1.120783", "1.120788", "1.120789"),
            answerFromAnotherForm = c("A New Hope", "Attack of the Clones",
                                      "Return of the Jedi", "Revenge of the Sith",
                                      "The Empire Strikes Back", "The Force Awakens",
                                      "The Last Jedi", "The Phantom Menace",
                                      "A New Hope", "Attack of the Clones",
                                      "Revenge of the Sith", "The Empire Strikes Back",
                                      "The Phantom Menace", "Attack of the Clones",
                                      "Revenge of the Sith", "The Phantom Menace",
                                      "A New Hope", "Return of the Jedi",
                                      "The Empire Strikes Back", "The Force Awakens",
                                      "The Last Jedi"),
            answer_id = c("1.120986", "1.120986", "1.120986", "1.120986", "1.120986",
                          "1.120986", "1.120986", "1.120986", "1.120985", "1.120985",
                          "1.120985", "1.120985", "1.120985", "1.120984", "1.120984",
                          "1.120984", "1.120978", "1.120978", "1.120978", "1.120978",
                          "1.120978"),
            films66304_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                              "11", "12", "13", "14", "15", "16", "17", "18", "19",
                              "20", "21")),
          row.names = c(NA, -21L),
          class = "data.frame"))),
    names = c("", ""))


# Create the data frame to compare (very long command creation)
## Created using dput()
myExpectedAnswersStarWarsFormSingleDF <-
  structure(
    list(
      answer.answer_id = c("1.120986", "1.120986", "1.120986", "1.120986",
                           "1.120986", "1.120986", "1.120986", "1.120986",
                           "1.120985", "1.120985", "1.120985", "1.120985",
                           "1.120985", "1.120984", "1.120984", "1.120984",
                           "1.120978", "1.120978", "1.120978", "1.120978",
                           "1.120978"),
      answer.name66298 = c("R2-D2", "R2-D2", "R2-D2", "R2-D2", "R2-D2", "R2-D2",
                           "R2-D2", "R2-D2", "Yoda", "Yoda", "Yoda", "Yoda",
                           "Yoda", "Anakin Skywalker", "Anakin Skywalker",
                           "Anakin Skywalker", "Luke Skywalker", "Luke Skywalker",
                           "Luke Skywalker", "Luke Skywalker", "Luke Skywalker"),
      answer.height66299 = c(96L, 96L, 96L, 96L, 96L, 96L, 96L, 96L, 66L, 66L,
                             66L, 66L, 66L, 188L, 188L, 188L, 172L, 172L, 172L,
                             172L, 172L),
      answer.mass66300 = c(32L, 32L, 32L, 32L, 32L, 32L, 32L, 32L, 17L, 17L, 17L,
                           17L, 17L, 84L, 84L, 84L, 77L, 77L, 77L, 77L, 77L),
      answer.birthYear66380 = c(33, 33, 33, 33, 33, 33, 33, 33, 896, 896, 896,
                                896, 896, 41.9, 41.9, 41.9, 19, 19, 19, 19, 19),
      answer.gender66302 = c("none", "none", "none", "none", "none", "none",
                             "none", "none", "male", "male", "male", "male",
                             "male", "male", "male", "male", "male", "male",
                             "male", "male", "male"),
      answer.specie66303.answerIdFromAnotherForm = c("1.120773", "1.120773",
                                                     "1.120773", "1.120773",
                                                     "1.120773", "1.120773",
                                                     "1.120773", "1.120773",
                                                     "1.120777", "1.120777",
                                                     "1.120777", "1.120777",
                                                     "1.120777", "1.120771",
                                                     "1.120771", "1.120771",
                                                     "1.120771", "1.120771",
                                                     "1.120771", "1.120771",
                                                     "1.120771"),
      answer.specie66303.answerFromAnotherForm = c("Droid", "Droid", "Droid",
                                                   "Droid", "Droid", "Droid",
                                                   "Droid", "Droid", "Yoda's species",
                                                   "Yoda's species", "Yoda's species",
                                                   "Yoda's species", "Yoda's species",
                                                   "Human", "Human", "Human", "Human",
                                                   "Human", "Human", "Human", "Human"),
      answer.userName = c("André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto",
                          "André Smaniotto", "André Smaniotto", "André Smaniotto"),
      answer.userId = c(8403L, 8403L, 8403L, 8403L, 8403L, 8403L, 8403L, 8403L,
                        8403L, 8403L, 8403L, 8403L, 8403L, 8403L, 8403L, 8403L,
                        8403L, 8403L, 8403L, 8403L, 8403L),
      answer.source = c("web_private", "web_private", "web_private", "web_private",
                        "web_private", "web_private", "web_private", "web_private",
                        "web_private", "web_private", "web_private", "web_private",
                        "web_private", "web_private", "web_private", "web_private",
                        "web_private", "web_private", "web_private", "web_private",
                        "web_private"),
      answer.createdAt = c("2018-05-28T11:44:48+0000", "2018-05-28T11:44:48+0000",
                           "2018-05-28T11:44:48+0000", "2018-05-28T11:44:48+0000",
                           "2018-05-28T11:44:48+0000", "2018-05-28T11:44:48+0000",
                           "2018-05-28T11:44:48+0000", "2018-05-28T11:44:48+0000",
                           "2018-05-28T11:43:26+0000", "2018-05-28T11:43:26+0000",
                           "2018-05-28T11:43:26+0000", "2018-05-28T11:43:26+0000",
                           "2018-05-28T11:43:26+0000", "2018-05-28T11:42:22+0000",
                           "2018-05-28T11:42:22+0000", "2018-05-28T11:42:22+0000",
                           "2018-05-28T11:27:40+0000", "2018-05-28T11:27:40+0000",
                           "2018-05-28T11:27:40+0000", "2018-05-28T11:27:40+0000",
                           "2018-05-28T11:27:40+0000"),
      answer.createdAtDevice = c("2018-05-28T11:44:48+0000", "2018-05-28T11:44:48+0000",
                                 "2018-05-28T11:44:48+0000", "2018-05-28T11:44:48+0000",
                                 "2018-05-28T11:44:48+0000", "2018-05-28T11:44:48+0000",
                                 "2018-05-28T11:44:48+0000", "2018-05-28T11:44:48+0000",
                                 "2018-05-28T11:43:26+0000", "2018-05-28T11:43:26+0000",
                                 "2018-05-28T11:43:26+0000", "2018-05-28T11:43:26+0000",
                                 "2018-05-28T11:43:26+0000", "2018-05-28T11:42:22+0000",
                                 "2018-05-28T11:42:22+0000", "2018-05-28T11:42:22+0000",
                                 "2018-05-28T11:27:40+0000", "2018-05-28T11:27:40+0000",
                                 "2018-05-28T11:27:40+0000", "2018-05-28T11:27:40+0000",
                                 "2018-05-28T11:27:40+0000"),
      answer.createdAtCoordinates.latitude = c(-26.9075, -26.9075, -26.9075,
                                               -26.9075, -26.9075, -26.9075,
                                               -26.9075, -26.9075, -26.9075,
                                               -26.9075, -26.9075, -26.9075,
                                               -26.9075, -26.9075, -26.9075,
                                               -26.9075, -26.9075, -26.9075,
                                               -26.9075, -26.9075, -26.9075),
      answer.createdAtCoordinates.longitude = c(-48.65393, -48.65393, -48.65393,
                                                -48.65393, -48.65393, -48.65393,
                                                -48.65393, -48.65393, -48.65393,
                                                -48.65393, -48.65393, -48.65393,
                                                -48.65393, -48.65393, -48.65393,
                                                -48.65393, -48.65393, -48.65393,
                                                -48.65393, -48.65393, -48.65393),
      answer.updatedAt = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                           NA, NA, NA, "2018-05-28T11:32:09+0000",
                           "2018-05-28T11:32:09+0000", "2018-05-28T11:32:09+0000",
                           "2018-05-28T11:32:09+0000", "2018-05-28T11:32:09+0000"),
      answer.updatedAtCoordinates.latitude = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                               NA, NA, NA, NA, NA, NA, NA, NA,
                                               -26.9075, -26.9075, -26.9075,
                                               -26.9075, -26.9075),
      answer.updatedAtCoordinates.longitude = c(NA, NA, NA, NA, NA, NA, NA, NA,
                                                NA, NA, NA, NA, NA, NA, NA, NA,
                                                -48.65393, -48.65393, -48.65393,
                                                -48.65393, -48.65393),
      films66304.answerIdFromAnotherForm = c("1.120782", "1.120786", "1.120784",
                                             "1.120787", "1.120783", "1.120788",
                                             "1.120789", "1.120785", "1.120782",
                                             "1.120786", "1.120787", "1.120783",
                                             "1.120785", "1.120786", "1.120787",
                                             "1.120785", "1.120782", "1.120784",
                                             "1.120783", "1.120788", "1.120789"),
      films66304.answerFromAnotherForm = c("A New Hope", "Attack of the Clones",
                                           "Return of the Jedi", "Revenge of the Sith",
                                           "The Empire Strikes Back", "The Force Awakens",
                                           "The Last Jedi", "The Phantom Menace",
                                           "A New Hope", "Attack of the Clones",
                                           "Revenge of the Sith", "The Empire Strikes Back",
                                           "The Phantom Menace", "Attack of the Clones",
                                           "Revenge of the Sith", "The Phantom Menace",
                                           "A New Hope", "Return of the Jedi",
                                           "The Empire Strikes Back", "The Force Awakens",
                                           "The Last Jedi"),
      films66304.films66304_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
                                   "10", "11", "12", "13", "14", "15", "16", "17",
                                   "18", "19", "20", "21")),
    row.names = c(NA, -21L),
    class = "data.frame")


# Create the data frame to compare (very long command creation)
## Created using dput()
myExpectedAnswersClassicRocksFormMultDF <-
  structure(
    list(
      structure(
        list(
          answer_id = c("1.121101", "1.121093", "1.121042"),
          artistName66429 = c("Pink Floyd", "Bon jovi", "Lynyrd Skynyrd"),
          active66430 = c(TRUE, TRUE, TRUE),
          originLocalition66431.country66432 = c("Reino Unido", "Estados Unidos",
                                                 "Estados Unidos"),
          userName = c("André Smaniotto", "André Smaniotto", "André Smaniotto"),
          userId = c(8403L, 8403L, 8403L), source = c("web_private", "web_private",
                                                      "web_private"),
          createdAt = c("2018-05-28T17:04:53+0000", "2018-05-28T16:49:14+0000",
                        "2018-05-28T14:49:03+0000"),
          createdAtDevice = c("2018-05-28T17:04:53+0000", "2018-05-28T16:49:14+0000",
                              "2018-05-28T14:49:03+0000"),
          createdAtCoordinates.latitude = c(NA, NA, NA),
          createdAtCoordinates.longitude = c(NA, NA, NA),
          updatedAt = c("2018-05-28T17:16:34+0000", "2018-05-28T16:50:54+0000",
                        "2018-05-28T16:40:26+0000"),
          updatedAtCoordinates.latitude = c(NA, NA, NA),
          updatedAtCoordinates.longitude = c(NA, NA, NA)),
        row.names = c(NA, 3L), class = "data.frame"),
      list(
        members66433.active66434 = structure(
          list(
            answer_id = c("1.121101", "1.121101", "1.121101", "1.121093",
                          "1.121093", "1.121093", "1.121093", "1.121093",
                          "1.121042", "1.121042", "1.121042", "1.121042",
                          "1.121042", "1.121042", "1.121042"),
            members66433.active66434 = c("Nick Mason", "Roger Waters",
                                         "David Gilmour", "Jon Bon Jovi",
                                         "David Bryan", "Tico Torres",
                                         "Hugh McDonald", "Phil X", "Gary Rossington",
                                         "Rickey Medlocke", "Johnny Van Zant",
                                         "Michael Cartellone", "Mark Matejka",
                                         "Peter Keys", "Keith Christopher"),
            members66433.active66434_id = c("1", "2", "3", "4", "5", "6", "7",
                                            "8", "9", "10", "11", "12", "13",
                                            "14", "15")),
          row.names = c(NA, -15L), class = "data.frame"),
        members66433.past66435 = structure(
          list(
            answer_id = c("1.121101", "1.121101", "1.121093", "1.121093", "1.121093",
                          "1.121042", "1.121042", "1.121042", "1.121042", "1.121042",
                          "1.121042", "1.121042", "1.121042", "1.121042", "1.121042",
                          "1.121042", "1.121042", "1.121042", "1.121042"),
            members66433.past66435 = c("Richard Wright", "Syd Barrett",
                                       "Richie Sambora", "Alec John Such",
                                       "Dave Sabo", "Ronnie Van Zant",
                                       "Allen Collins", "Bob Burns",
                                       "Larry Junstrom", "Greg T. Walker",
                                       "Leon Wilkeson", "Billy Powell", "Ed King",
                                       "Artimus Pyle", "Steve Gaines",
                                       "Hughie Thomasson", "Ean Evans",
                                       "Robert Kearns", "Johnny Colt"),
            members66433.past66435_id = c("1", "2", "3", "4", "5", "6", "7", "8",
                                          "9", "10", "11", "12", "13", "14", "15",
                                          "16", "17", "18", "19")),
          row.names = c(NA, -19L), class = "data.frame"),
        music66436.album66437 = structure(
          list(
            name66438 = c("The Dark Side of the Moon", "Wish You Were Here",
                          "Slippery When Wet", "(Pronounced 'Lĕh-'nérd 'Skin-'nérd)",
                          "Second Helping", "Nuthin' Fancy", "Gimme Back My Bullets",
                          "Street Survivors"),
            year66439 = c(1973L, 1975L, 1986L, 1973L, 1974L, 1975L, 1976L, 1977L),
            answer_id = c("1.121101", "1.121101", "1.121093", "1.121042",
                          "1.121042", "1.121042", "1.121042", "1.121042"),
            music66436.album66437_id = c("1", "2", "3", "4", "5", "6", "7", "8")),
          row.names = c(NA, -8L), class = "data.frame"),
        genres66441 = structure(
          list(
            answerIdFromAnotherForm = c("1.121099", "1.121098", "1.121099",
                                        "1.121098", "1.121114", "1.121090",
                                        "1.121012", "1.121011", "1.121012",
                                        "1.121010", "1.121011", "1.121014",
                                        "1.121015", "1.121012", "1.121010",
                                        "1.121011", "1.121014", "1.121012",
                                        "1.121010", "1.121010", "1.121014",
                                        "1.121010"),
            answerFromAnotherForm = c("Art rock", "Rock progressive", "Art rock",
                                      "Rock progressive", "Experimental rock",
                                      "Glam metal", "Hard rock", "Blues rock",
                                      "Hard rock", "Southern rock", "Blues rock",
                                      "Boggie rock", "Country rock", "Hard rock",
                                      "Southern rock", "Blues rock", "Boogie rock",
                                      "Hard rock", "Southern rock", "Southern rock",
                                      "Boogie rock", "Southern rock"),
            music66436.album66437_id = c("1", "1", "2", "2", "2", "3", "3", "4",
                                         "4", "4", "5", "5", "5", "5", "5", "6",
                                         "6", "6", "6", "7", "8", "8"),
            genres66441_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                               "11", "12", "13", "14", "15", "16", "17", "18",
                               "19", "20", "21", "22")),
          row.names = c(NA, -22L), class = "data.frame"),
        members66443 = structure(
          list(
            name66444 = c("Roger Waters", "David Gilmour", "Nick Mason",
                          "Richard Wright", "David Gilmour", "Roger Waters",
                          "Nick Mason", "Richard Wright", "Jon Bon Jovi",
                          "Richie Sambora", "Alec John Such", "Tico Torres",
                          "David Bryan", "Ronnie Van Zant", "Gary Rossington",
                          "Allen Collins", "Ed King", "Billy Powell", "Bob Burns",
                          "Ronnie Van Zant", "Gary Rossington", "Allen Collins",
                          "Ed King", "Billy Powell", "Leon Wilkeson", "Bob Burns",
                          "Ronnie Van Zant", "Allen Collins", "Ed King",
                          "Gary Rossington", "Billy Powell", "Leon Wilkeson",
                          "Artimus Pyle", "Ronnie Van Zant", "Allen Collins",
                          "Gary Rossington", "Billy Powell", "Leon Wilkeson",
                          "Artimus Pyle", "Ronnie Van Zant", "Steve Gaines",
                          "Allen Collins", "Gary Rossington", "Billy Powell",
                          "Leon Wilkeson", "Artimus Pyle"),
            music66436.album66437_id = c("1", "1", "1", "1", "2", "2", "2", "2",
                                         "3", "3", "3", "3", "3", "4", "4", "4",
                                         "4", "4", "4", "5", "5", "5", "5", "5",
                                         "5", "5", "6", "6", "6", "6", "6", "6",
                                         "6", "7", "7", "7", "7", "7", "7", "8",
                                         "8", "8", "8", "8", "8", "8"),
            members66443_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                "11", "12", "13", "14", "15", "16", "17", "18",
                                "19", "20", "21", "22", "23", "24", "25", "26",
                                "27", "28", "29", "30", "31", "32", "33", "34",
                                "35", "36", "37", "38", "39", "40", "41", "42",
                                "43", "44", "45", "46")),
          row.names = c(NA, -46L), class = "data.frame"),
        instruments66446 = structure(
          list(
            answerIdFromAnotherForm = c("1.121023", "1.121103", "1.121106",
                                        "1.121032", "1.121031", "1.121105",
                                        "1.121032", "1.121020", "1.121027",
                                        "1.121103", "1.121108", "1.121107",
                                        "1.121026", "1.121105", "1.121106",
                                        "1.121032", "1.121032", "1.121119",
                                        "1.121031", "1.121118", "1.121103",
                                        "1.121023", "1.121124", "1.121031",
                                        "1.121103", "1.121032", "1.121020",
                                        "1.121027", "1.121103", "1.121121",
                                        "1.121022", "1.121124", "1.121120",
                                        "1.121125", "1.121122", "1.121123",
                                        "1.121126", "1.121031", "1.121040",
                                        "1.121021", "1.121022", "1.121019",
                                        "1.121096", "1.121022", "1.121023",
                                        "1.121020", "1.121094", "1.121022",
                                        "1.121028", "1.121095", "1.121040",
                                        "1.121019", "1.121021", "1.121041",
                                        "1.121019", "1.121021", "1.121023",
                                        "1.121019", "1.121028", "1.121020",
                                        "1.121040", "1.121031", "1.121031",
                                        "1.121022", "1.121023", "1.121031",
                                        "1.121028", "1.121022", "1.121023",
                                        "1.121020", "1.121040", "1.121031",
                                        "1.121031", "1.121031", "1.121028",
                                        "1.121023", "1.121020", "1.121027",
                                        "1.121040", "1.121031", "1.121031",
                                        "1.121028", "1.121022", "1.121023",
                                        "1.121020", "1.121027", "1.121040",
                                        "1.121022", "1.121031", "1.121040",
                                        "1.121031", "1.121031", "1.121028",
                                        "1.121022", "1.121023", "1.121020"),
            answerFromAnotherForm = c("Bass guitar", "Tape effects", "VCS 3",
                                      "Vocals", "Guitar", "Synthi AKS", "Vocals",
                                      "Drums", "Percussion", "Tape effects",
                                      "Electric piano", "Organ", "Piano",
                                      "Synthi AKS", "VCS 3", "Vocals", "Vocals",
                                      "EMS Synthi AKS", "Guitar", "Lap steel guitar",
                                      "Tape effects", "Bass guitar", "EMS VCS 3",
                                      "Guitar", "Tape effects", "Vocals", "Drums",
                                      "Percussion", "Tape effects",
                                      "ARP String Ensemble V", "Backing vocals",
                                      "EMS VCS 3", "Hammond C-3 organ",
                                      "Hohner Clavinet D6", "Minimoog",
                                      "Steinway piano", "Wurlitzer EP-200 electric piano",
                                      "Guitar", "Lead vocals", "Rhythm guitar",
                                      "Backing vocals", "Lead guitar",
                                      "Electric lead guitar", "Backing vocals",
                                      "Bass guitar", "Drums", "Finger cymbals",
                                      "Backing vocals", "Keyboards", "Horns",
                                      "Lead vocals", "Lead guitar", "Rhythm guitar",
                                      "Slide guitar", "Lead guitar", "Rhythm guitar",
                                      "Bass guitar", "Lead guitar", "Keyboards",
                                      "Drums", "Lead vocals", "Guitars", "Guitars",
                                      "Backing vocals", "Bass guitar", "Guitars",
                                      "Keyboards", "Backing vocals", "Bass guitar",
                                      "Drums", "Lead vocals", "Guitar", "Guitar",
                                      "Guitar", "Keyboards", "Bass guitar", "Drums",
                                      "Percussion", "Lead vocals", "Guitar",
                                      "Guitar", "Keyboards", "Backing vocals",
                                      "Bass guitar", "Drums", "Percussion",
                                      "Lead vocals", "Backing vocals", "Guitar",
                                      "Lead vocals", "Guitar", "Guitar",
                                      "Keyboards", "Backing vocals", "Bass guitar",
                                      "Drums"),
            members66443_id = c("1", "1", "1", "1", "2", "2", "2", "3", "3", "3",
                                "4", "4", "4", "4", "4", "4", "5", "5", "5", "5",
                                "5", "6", "6", "6", "6", "6", "7", "7", "7", "8",
                                "8", "8", "8", "8", "8", "8", "8", "9", "9", "9",
                                "10", "10", "10", "11", "11", "12", "12", "13",
                                "13", "13", "14", "15", "15", "15", "16", "16",
                                "17", "17", "18", "19", "20", "21", "22", "23",
                                "23", "23", "24", "25", "25", "26", "27", "28",
                                "29", "30", "31", "32", "33", "33", "34", "35",
                                "36", "37", "38", "38", "39", "39", "40", "41",
                                "41", "41", "42", "43", "44", "45", "45", "46"),
            instruments66446_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
                                    "10", "11", "12", "13", "14", "15", "16",
                                    "17", "18", "19", "20", "21", "22", "23",
                                    "24", "25", "26", "27", "28", "29", "30",
                                    "31", "32", "33", "34", "35", "36", "37",
                                    "38", "39", "40", "41", "42", "43", "44",
                                    "45", "46", "47", "48", "49", "50", "51",
                                    "52", "53", "54", "55", "56", "57", "58",
                                    "59", "60", "61", "62", "63", "64", "65",
                                    "66", "67", "68", "69", "70", "71", "72",
                                    "73", "74", "75", "76", "77", "78", "79",
                                    "80", "81", "82", "83", "84", "85", "86",
                                    "87", "88", "89", "90", "91", "92", "93",
                                    "94", "95", "96")),
          row.names = c(NA, -96L), class = "data.frame"))),
    names = c("", ""))


##### Tests ===========
test_that("error by wrong token", {
  expect_error(
    GetAnswers("notexisttoken", 5705),
    "Error 401: O token informado não é válido para o acesso."
  )
  expect_error(
    GetAnswers("", 5705),
    "Error 401: Unauthorized"
  )
})

test_that("error by wrong idForm or nameForm", {
  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5715),
    "Form not found."
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
                     nameForm = "RColetum Test - NaN"),
    "Name not found."
  )

  expect_warning(
    expect_error(
      GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
                 5841,
                 "RColetum Test - NaN"),
      "Form not found."
    )
  )

  expect_warning(
    expect_error(
      GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
                 5715,
                 "RColetum Test - Iris"),
      "Form not found."
    )
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg"),
    "IdForm or nameForm should be provided."
  )

})

test_that("error by duplicated form name", {
  expect_error(
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                     nameForm = "RColetum Test - Westeros")
  )
})

test_that("errors in the bad use of filters parameters", {
  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               source = "web"),
    paste0("The option 'web' are not avaliable for the filter 'source'. ",
           "The avaliable options to this filter are: 'web_public' or",
           " 'web_private' or 'mobile'.")
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               source = "private"),
    paste0("The option 'private' are not avaliable for the filter 'source'. ",
           "The avaliable options to this filter are: 'web_public' or",
           " 'web_private' or 'mobile'.")
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               createdAfter = "1995-30-15")
    )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               createdBefore = "1995-30-15")
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               createdAfter = "15-30-15")
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               createdBefore = "15-30-15")
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               createdAfter = "2018-12-20 19:20:30+01:00")
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               createdBefore = "2018-12-20 19:20:30+01:00")
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               createdAfter = "2018-12-20T19:20:30")
  )

  expect_error(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
               5705,
               createdBefore = "2018-12-20T19:20:30")
  )


})

test_that("warming when is informed the idForm and nameForm", {
  expect_warning(
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg",
                     5705,
                     "RColetum Test - Iris"),
    "The idForm and nameForm are provided. Ignoring the nameForm."
  )
})

test_that("get answers in simple form", {
  myFullAnswersSimpleForm <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5705)
  expect_equal(myExpectedAnswersIrisForm, myFullAnswersSimpleForm)

  myFilteredAnswersSource <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               source = "web_private")
  myFilteredAnswersSource2 <-
    dplyr::filter(myExpectedAnswersIrisForm, source == "web_private")
  expect_equal(myFilteredAnswersSource, myFilteredAnswersSource2)

  myFilteredAnswersCreatedBefore <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdBefore = "2018-05-30")
  myFilteredAnswersCreatedBefore2 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdBefore = "2018-05-30T06:20:30+01:00")
  myFilteredAnswersCreatedBefore3 <-
    dplyr::filter(myExpectedAnswersIrisForm, createdAt < "2018-05-30")
  expect_equal(myFilteredAnswersCreatedBefore, myFilteredAnswersCreatedBefore3)
  expect_equal(myFilteredAnswersCreatedBefore2, myFilteredAnswersCreatedBefore3)

  myFilteredAnswersCreatedAfter <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdAfter = "2018-05-30")
  myFilteredAnswersCreatedAfter2 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdAfter = "2018-05-30T16:20:30+01:00")
  expect_equal(myFilteredAnswersCreatedAfter, NULL)
  expect_equal(myFilteredAnswersCreatedAfter2,NULL)

  myFilteredAnswersCreatedAfter3 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdAfter = "2018-05-30T12:20:30+01:00")
  myFilteredAnswersCreatedAfter4 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdAfter = "2018-05-30T12:20:30Z")
  myFilteredAnswersCreatedAfter5 <-
    dplyr::mutate(
      dplyr::filter(myExpectedAnswersIrisForm, createdAt > "2018-05-29"),
      updatedAt = as.logical(updatedAt))
  expect_equal(myFilteredAnswersCreatedAfter3, myFilteredAnswersCreatedAfter5)
  expect_equal(myFilteredAnswersCreatedAfter4, myFilteredAnswersCreatedAfter5)

  myFilteredAnswersCreatedDeviceBefore <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdDeviceBefore = "2018-05-30")
  myFilteredAnswersCreatedDeviceBefore2 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdDeviceBefore = "2018-05-30T06:20:30+01:00")
  myFilteredAnswersCreatedDeviceBefore3 <-
    dplyr::filter(myExpectedAnswersIrisForm, createdAtDevice < "2018-05-30")
  expect_equal(myFilteredAnswersCreatedDeviceBefore,
               myFilteredAnswersCreatedDeviceBefore3)
  expect_equal(myFilteredAnswersCreatedDeviceBefore2,
               myFilteredAnswersCreatedDeviceBefore3)

  myFilteredAnswersCreatedDeviceAfter <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdDeviceAfter = "2018-05-30")
  myFilteredAnswersCreatedDeviceAfter2 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdDeviceAfter = "2018-05-30T16:20:30+01:00")
  expect_equal(myFilteredAnswersCreatedDeviceAfter, NULL)
  expect_equal(myFilteredAnswersCreatedDeviceAfter2,NULL)

  myFilteredAnswersCreatedDeviceAfter3 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdDeviceAfter = "2018-05-30T12:20:30+01:00")
  myFilteredAnswersCreatedDeviceAfter4 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               createdDeviceAfter = "2018-05-30T12:20:30Z")
  myFilteredAnswersCreatedDeviceAfter5 <-
    dplyr::mutate(
      dplyr::filter(myExpectedAnswersIrisForm, createdAtDevice > "2018-05-29"),
      updatedAt = as.logical(updatedAt))
  expect_equal(myFilteredAnswersCreatedDeviceAfter3,
               myFilteredAnswersCreatedDeviceAfter5)
  expect_equal(myFilteredAnswersCreatedDeviceAfter4,
               myFilteredAnswersCreatedDeviceAfter5)

  myFilteredAnswersUpdatedBefore <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               updatedBefore = "2018-05-30")
  myFilteredAnswersUpdatedBefore2 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               updatedBefore = "2018-05-30T06:20:30+01:00")
  myFilteredAnswersUpdatedBefore3 <-
    dplyr::filter(myExpectedAnswersIrisForm, updatedAt < "2018-05-30")
  expect_equal(myFilteredAnswersUpdatedBefore, myFilteredAnswersUpdatedBefore3)
  expect_equal(myFilteredAnswersUpdatedBefore2, myFilteredAnswersUpdatedBefore3)

  myFilteredAnswersUpdatedAfter <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               updatedAfter = "2018-05-30")
  myFilteredAnswersUpdatedAfter2 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               updatedAfter = "2018-05-30T16:20:30+01:00")
  expect_equal(myFilteredAnswersUpdatedAfter, NULL)
  expect_equal(myFilteredAnswersUpdatedAfter2,NULL)

  myFilteredAnswersUpdatedAfter3 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               updatedAfter = "2018-05-20T12:20:30+01:00")
  myFilteredAnswersUpdatedAfter4 <-
    GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
               idForm = 5705,
               updatedAfter = "2018-05-20T12:20:30Z")
  myFilteredAnswersUpdatedAfter5 <-
    dplyr::mutate(
      dplyr::filter(myExpectedAnswersIrisForm, updatedAt > "2018-05-20"))
  expect_equal(myFilteredAnswersUpdatedAfter3, myFilteredAnswersUpdatedAfter5)
  expect_equal(myFilteredAnswersUpdatedAfter4, myFilteredAnswersUpdatedAfter5)
})

test_that("get answers in more complex forms", {
  myNQuestionsAnswers <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5719)
  expect_equal(myNQuestionsAnswers,myExpectedAnswersStormFormMultDF)

  myRelationalAnswers <- GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5713)
  expect_equal(myRelationalAnswers,myExpectedAnswersStarWarsFormMultDF)

  myComplexGroupRelationalNQuestionsAnswers <-
    GetAnswers("cizio7xeohwgc8k4g4koo008kkoocwg", 5722)
  expect_equal(myComplexGroupRelationalNQuestionsAnswers,
               myExpectedAnswersClassicRocksFormMultDF)
})

test_that("get answers in single data frame", {
  myNQuestionsAnswers <- GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                                    idForm = 5719,
                                    singleDataFrame = TRUE)
  expect_equal(myNQuestionsAnswers,myExpectedAnswersStormFormSingleDF)

  myRelationalAnswers <- GetAnswers(token = "cizio7xeohwgc8k4g4koo008kkoocwg",
                                    idForm = 5713,
                                    singleDataFrame = TRUE)
  expect_equal(myRelationalAnswers,myExpectedAnswersStarWarsFormSingleDF)

})
