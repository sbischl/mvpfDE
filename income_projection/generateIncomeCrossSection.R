# This file calculates the average income for each age:

# We have average incomes for each age and degree from an "IAB Kurzbericht", http://doku.iab.de/kurzber/2014/kb0114.pdf
age_income_degree <-  read.csv("./income_projection/age_income_degree.csv")
# This does not include the average income however. -> But the average income can be calculated as a weighted average
# because the IAB published a frequency table of the education variable. (See below and source.txt in the "income_projection" folder)

age_income_degree[age_income_degree == 0] <- NA

# School education and vocational training (bild)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#|                                                                           Year
#School education and vocational training |         1975          1985          1992          1998          1999          2000          2001          2003          2005          2007          2010
#-----------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------
#1  Secondary / intermediate school leavi |      126,597       114,142       115,049        88,199       106,100       107,825       105,503        94,538        88,169        84,818        77,685
#2  Secondary / intermediate school leavi |      210,922       235,298       359,667       321,380       344,182       347,444       344,619       326,089       316,780       320,287       312,315
#3  Upper secondary school leaving certif |        1,872         3,964         6,062         7,610        10,348        11,313        11,592        11,622        11,878        12,920        14,043
#4  Upper secondary school leaving certif |        3,445         5,608        14,596        18,208        20,138        21,339        22,440        23,182        24,306        26,316        28,573
#5  Completion of a university of applied |        5,891         8,501        17,478        17,245        17,702        18,107        18,442        18,544        18,930        20,243        21,861
#6  College / university degree           |        6,245        10,653        23,999        26,974        28,987        30,046        30,965        31,533        32,568        34,640        38,272
#21  No vocational training               |            /           112         1,569        36,384        35,339        34,367        33,770        35,666        56,666        19,245         4,914
#22  In-company vocational training/appre |            /            72         1,463        52,744        51,669        48,445        48,827        55,447        67,133        58,100        56,663
#23  External (on-school) vocational trai |                                        /           572           786         1,034         1,357         3,044         5,400           981            71
#24  Specialised vocational school (full  |                          /            68         1,720         1,642         1,448         1,427         1,703         2,445         3,016         3,222
#25  Technical school                     |                          /            77         4,507         4,309         3,901         3,646         3,431         3,850         2,200         2,028
#26  University of applied sciences       |                          /            23         1,475         1,493         1,355         1,384         2,326         2,829         3,731         3,234
#27  University                           |                          /           104         4,108         3,949         3,693         3,469         4,571         5,390         2,985         4,213
#.n  n/a                                  |          362        27,655        65,693        75,006        71,297        66,616        68,282        82,406       120,894       130,673       122,585
#.z  no entry                             |       34,058        22,711        42,166        50,292       104,430       112,193       117,050       124,181       150,890       224,562       245,769
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# From the table (2010):
education_levels_frequency <- list(no_vocational_educ <- 77685,
                         vocational_educ <-  312315,
                         abitur <- 14043 + 28573,
                         applied_sciences_degree <- 21861,
                         university_degree <- 38272)

observations <- sum(unlist(education_levels))

education_levels_shares <- unlist(education_levels_frequency) / observations

age_income_cross_section <- data.frame(age = age_income_degree$age,
                                       income = apply(age_income_degree[,2:ncol(age_income_degree)], 1, function(x) {
                                         # Devide by the sum of shares that are not NA.
                                         sum(x * education_levels_shares, na.rm =  TRUE) / sum(education_levels_shares * x / x, na.rm = TRUE)
                                       }))

write.csv(x = age_income_cross_section, file = "./income_projection/age_income_cross_section.csv", row.names =  FALSE)

