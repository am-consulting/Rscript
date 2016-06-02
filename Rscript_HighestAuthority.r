# License:GPL(version 2 or later)
highestAuthority <- function() {
  options("getSymbols.warning4.0" = FALSE)
  indextitle <<-
    c(
      "Prime Minister of Japan",
      "Governor of Bank of Japan",
      "President of the United States",
      "Chair of the Federal Reserve"
    )
  titleAbb <<-
    tolower(abbreviate(indextitle, minlength = 4, use.classes = T))
  highestAuthority <<- list()
  highestAuthority[[1]] <<- c(
    #Reference https://en.wikipedia.org/wiki/List_of_Prime_Ministers_of_Japan
    "1885-12-22",    "Hirobumi Ito(1)",
    "1888-4-30" ,    "Kiyotaka Kuroda",
    "1889-10-25",    "Sanetomi Sanjo",
    "1889-12-24",    "Aritomo Yamagata(1)",
    "1891-5-6"  ,    "Masayoshi Matsukata(1)",
    "1892-8-8"  ,    "Hirobumi Ito(2)",
    "1896-9-18" ,    "Masayoshi Matsukata(2)",
    "1898-1-12" ,    "Hirobumi Ito(3)",
    "1898-6-30" ,    "Shigenobu Okuma(1)",
    "1898-11-8" ,    "Aritomo Yamagata(2)",
    "1900-10-19",    "Hirobumi Ito(4)",
    "1901-6-2"  ,    "Taro Katsura(1)",
    "1906-1-7"  ,    "Kinmochi Saionji(1)",
    "1908-7-14" ,    "Taro Katsura(2)",
    "1911-8-30" ,    "Kinmochi Saionji(2)",
    "1912-12-21",    "Taro Katsura(3)",
    "1913-2-20" ,    "Gonnohyoe Yamamoto(1)",
    "1914-4-16" ,    "Shigenobu Okuma(2)",
    "1916-10-9" ,    "Masatake Terauchi",
    "1918-9-29" ,    "Takashi Hara",
    "1921-11-13",    "Korekiyo Takahashi",
    "1922-6-12" ,    "Tomosaburo Kato",
    "1923-9-2"  ,    "Gonnohyoe Yamamoto(2)",
    "1924-1-7"  ,    "Keigo Kiyoura",
    "1924-6-11" ,    "Takaaki Kato",
    "1926-1-30" ,    "Reijiro Wakatsuki(1)",
    "1927-4-20" ,    "Giichi Tanaka",
    "1929-7-2"  ,    "Osachi Hamaguchi",
    "1931-4-14" ,    "Reijiro Wakatsuki(2)",
    "1931-12-13",    "Tsuyoshi Inukai",
    "1932-5-26" ,    "Makoto Saito",
    "1934-7-8"  ,    "Keisuke Okada",
    "1936-3-9"  ,    "Koki Hirota",
    "1937-2-2"  ,    "Senjuro Hayashi",
    "1937-6-4"  ,    "Fumimaro Konoe(1)",
    "1939-1-5"  ,    "Kiichiro Hiranuma",
    "1939-8-30" ,    "Nobuyuki Abe",
    "1940-1-16" ,    "Mitsumasa Yonai",
    "1940-7-22" ,    "Fumimaro Konoe(2-3)",
    "1941-10-18",    "Hideki Tojo",
    "1944-7-22" ,    "Kuniaki Koiso",
    "1945-4-7"  ,    "Kantaro Suzuki",
    "1945-8-17" ,    "Prince Naruhiko Higashikuni",
    "1945-10-9" ,    "Kijuro Shidehara",
    "1946-5-22" ,    "Shigeru Yoshida(1)",
    "1947-5-24" ,    "Tetsu Katayama",
    "1948-3-10" ,    "Hitoshi Ashida",
    "1948-10-15",    "Shigeru Yoshida(2-5)",
    "1954-12-10",    "Ichiro Hatoyama(1-3)",
    "1956-12-23",    "Tanzan Ishibashi",
    "1957-2-25" ,    "Nobusuke Kishi(1-2)",
    "1960-7-19" ,    "Hayato Ikeda(1-3)",
    "1964-11-9" ,    "Eisaku Sato(1-3)",
    "1972-7-7"  ,    "Kakuei Tanaka(1-2)",
    "1974-12-9" ,    "Takeo Mike",
    "1976-12-24",    "Takeo Fukuda",
    "1978-12-7" ,    "Masayoshi Ohira(1-2)",
    "1980-7-17" ,    "Zenko Suzuki",
    "1982-11-27",    "Yasuhiro Nakasone(1-3)",
    "1987-11-6" ,    "Noboru Takeshita",
    "1989-6-3"  ,    "Sosuke Uno",
    "1989-8-10" ,    "Toshiki Kaifu(1-2)",
    "1991-11-5" ,    "Kiichi Miyazawa",
    "1993-8-9"  ,    "Morihiro Hosokawa",
    "1994-4-28" ,    "Tsutomu Hata",
    "1994-6-30" ,    "Tomiichi Murayama",
    "1996-1-11" ,    "Ryutaro Hashimoto(1-2)",
    "1998-7-30" ,    "Keizo Obuchi",
    "2000-4-5"  ,    "Yoshiro Mori(1-2)",
    "2001-4-26" ,    "Junichiro Koizumi(1-3)",
    "2006-9-26" ,    "Shinzo Abe(1)",
    "2007-9-26" ,    "Yasuo Fukuda",
    "2008-9-24" ,    "Taro Aso",
    "2009-9-16" ,    "Yukio Hatoyama",
    "2010-6-8"  ,    "Naoto Kan",
    "2011-9-2"  ,    "Yoshihiko Noda",
    "2012-12-26",    "Shinzo Abe(2,3)"
  )
  highestAuthority[[2]] <<- c(
    #Reference https://en.wikipedia.org/wiki/Bank_of_Japan
    "1882-10-6" ,    "Shigetoshi Yoshihara",
    "1888-2-21" ,    "Tetsunosuke Tomita",
    "1889-9-3"  ,    "Koichiro Kawada",
    "1896-11-11",    "Yanosuke Iwasaki",
    "1898-10-20",    "Tatsuo Yamamoto",
    "1903-10-20",    "Shigeyoshi Matsuo",
    "1911-6-1"  ,    "Korekiyo Takahashi",
    "1913-2-28" ,    "Yataro Mishima",
    "1919-3-13" ,    "Junnosuke Inoue(1)",
    "1923-9-5"  ,    "Otohiko Ichiki",
    "1927-5-10" ,    "Junnosuke Inoue(2)",
    "1928-6-12" ,    "Hisaakira Hijikata",
    "1935-6-4"  ,    "Eigo Fukai",
    "1937-2-9"  ,    "Shigeaki Ikeda",
    "1937-7-27" ,    "Toyotaro Yuki",
    "1944-3-18" ,    "Keizo Shibusawa",
    "1945-10-9" ,    "Eikichi Araki(1)",
    "1946-6-1"  ,    "Hisato Ichimada",
    "1954-12-11",    "Eikichi Araki(2)",
    "1956-11-30",    "Masamichi Yamagiwa",
    "1964-12-17",    "Makoto Usami",
    "1969-12-17",    "Tadashi Sasaki",
    "1974-12-17",    "Teiichiro Morinaga",
    "1979-12-17",    "Haruo Maekawa",
    "1984-12-17",    "Satoshi Sumita",
    "1989-12-17",    "Yasushi Mieno",
    "1994-12-17",    "Yasuo Matsushita",
    "1998-3-20" ,    "Masaru Hayami",
    "2003-3-20" ,    "Toshihiko Fukui",
    "2008-4-9"  ,    "Masaaki Shirakawa",
    "2013-3-20" ,    "Haruhiko Kuroda"
  )
  
  highestAuthority[[3]] <<- c(
    #Reference https://en.wikipedia.org/wiki/List_of_Presidents_of_the_United_States
    "1933-3-4"  ,    "Franklin Delano Roosevelt",
    "1945-4-12" ,    "Harry S. Truman",
    "1953-1-20" ,    "Dwight David Eisenhower",
    "1961-1-20" ,    "John Fitzgerald Kennedy",
    "1963-11-22",    "Lyndon Baines Johnson",
    "1969-1-20" ,    "Richard Milhouse Nixon",
    "1974-8-9"  ,    "Gerald Rudolph Ford Jr.",
    "1977-1-20" ,    "James Earl Carter",
    "1981-1-20" ,    "Ronald Wilson Reagan",
    "1989-1-20" ,    "George H.W Bush",
    "1993-1-20" ,    "Bill Clinton",
    "2001-1-20" ,    "George W. Bush",
    "2009-1-20" ,    "Barack Obama"
  )
  highestAuthority[[4]] <<- c(
    #Reference https://en.wikipedia.org/wiki/Chair_of_the_Federal_Reserve
    "1934-11-15",    "Marriner Eccles",
    "1948-4-15" ,    "Thomas B. McCabe",
    "1951-4-2"  ,    "William M. Martin",
    "1970-2-1"  ,    "Arthur F.Burns",
    "1978-3-8"  ,    "G.William Miller",
    "1987-8-11" ,    "Alan Greenspan",
    "2006-2-1"  ,    "Ben S.Bernanke",
    "2014-2-1"  ,    "Janet Yellen"
  )
  borderDate <- as.Date(highestAuthority[[1]][1])
  for (jjj in 1:length(indextitle)) {
    tmp01 <- highestAuthority[[jjj]]
    inaugural.Date <- as.Date(tmp01[seq(1, length(tmp01), by = 2)])
    resignation.Date <- c((inaugural.Date - 1)[-1], Sys.Date())
    Name <- tmp01[seq(2, length(tmp01), by = 2)]
    Name[length(Name)] <-
      paste(Name[length(Name)], "-incumbency", sep = "")
    tenure.Days <- resignation.Date - inaugural.Date
    tmp02 <-
      data.frame(inaugural.Date,
                 resignation.Date,
                 Name,
                 tenure.Days,
                 check.names = F)
    colnames(tmp02)[2] <-
      paste(colnames(tmp02)[2], "(or incumbency)", sep = "")
    colnames(tmp02)[3] <- indextitle[jjj]
    tmp02[, 3] <-
      ordered(tmp02[, 3], levels = tmp02[, 3][1:nrow(tmp02)])
    assign(titleAbb[jjj], tmp02, env = .GlobalEnv)
    borderDate <<- max(inaugural.Date[1], borderDate)
  }
}