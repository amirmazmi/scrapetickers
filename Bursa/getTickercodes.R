# ---------------------------------------------------------
#
# Script to automate finding tickers from MalaysiaStock.biz
#
# https://www.malaysiastock.biz/Listed-Companies.aspx?
# type=A&value=T
#
#
# ---------------------------------------------------------
# ---------------------------------------------------------
library(pacman)
p_load(httr)
p_load(dplyr)
p_load(xml2)
# p_load(dplyr)
# ---------------------------------------------------------
# define functions
# ---------------------------------------------------------

querypage <- LETTERS[ sample( 1:length(LETTERS),1)]

base_url <- "https://www.malaysiastock.biz/"
path <- "Listed-Companies.aspx"
query <- list(
              type = "A",
              value = querypage  )
page <- modify_url(base_url, path=path, query=query)

htmlCode <- GET(page)
content <- content(htmlCode, "text")


# ---------------------------------------------------------
# XML code
# ---------------------------------------------------------
parsed <- htmlParse(content, asText = T)
# rootNode <- xmlRoot(parsed)
# xmlValue(rootNode[[2]][[4]][[18]])

gaga <- xpathApply(parsed, "//table[@class='marketWatch']")[[1]]
# wawa <-xpathApply(gaga, "//tr")

# -----------------------
# headers
headnode <- xmlClone(gaga[[1]])
headers <- unlist(xpathApply( headnode, "//th",
                               xmlValue))
ddf <- setNames( data.frame(matrix( ncol=length(headers), nrow=0)), headers)


# -----------------------
# get values
lulu <- xmlClone(gaga[[2]])
part2 <- unlist(xpathApply(lulu, "//td", xmlValue))[3:8]
part1 <- unlist(xpathApply(lulu, "//h3", xmlValue))


# ---------------------------------------------------------
# xml2 code
# ---------------------------------------------------------
rootNode <- read_html(content)

nodes <- xml_find_all(rootNode,
                         "//table[@class='marketWatch']")[[1]]
datanodes <- xml_children(nodes)
header <- xml_children(datanodes[[1]]) %>%
            xml_contents() %>%
            as_list() %>%
            unlist()
coln <- c(header[1],"code",header[2:8])

#create dataframe
dfdata <- data.frame( matrix( ncol=length(coln), nrow=0)) %>%
          setNames( coln)

# run this in a loop based on length of  datanodes
# for(k in seq(2, length(datanodes)){}
newnode <- xml_new_root(datanodes[[2]])

# shariah compliance
shariah <- xml_find_all(newnode, "//img[@src]") %>%
              xml_attr("src") %>%
              strsplit("/") %>%
              unlist %>%
              grep(".png", ., value=T) %>%
              strsplit(".png") %>%
              unlist

part1 <- newnode %>%
          xml_find_all("//h3") %>%
          xml_text() %>%
          unlist()
# company name
part1[2]

# ticker code
patt <- "(?<=\\().*(?=\\))" #"\\(.*?\\)"
m <- regexec(patt, part1[1], perl=T)
tickcode <- regmatches( part1[1], m)[[1]]

# Sector, Market Cap, Last Price, PE, DY, ROE
part2 <- newnode %>%
          xml_find_all("//td") %>%
          xml_text()
part2[3:8]

rowdf <- c(part1[2], tickcode, shariah, part2[3:8])
dfdata[1,] <- rowdf




# ---------------------------------------------------------
getTicker <- function(name){
  # set up urlquery
  spl <- trimws(name[1], "right")
  spl1 <- sub("BERHAD | HOLDINGS", "", as.character(spl))
  con <- modify_url("https://www.google.com/finance", query=
                      list( q=spl1))
  con1 <- paste0(con,"+klse", collapse="")

  # get html
  htmlCode <- GET(con1)
  if(htmlCode$status_code == 200){
    content2 <- content( htmlCode, as="text")

    # parse to get ticker
    parsedhtml <- htmlParse(content2, asText=T)
    rootNode <- xmlRoot(parsedhtml)
    charstring <- xmlValue(rootNode[[1]][[2]])
    charlist <- unlist(strsplit(charstring, split=" "))

    ticker <- grep("KLSE", charlist, value=T)
    # if unable to find or any issues
    if(length(ticker)==0){
      ticker <- "skipped"
    }
  } else {
    ticker <- "skipped"
  }

  # some visual
  print( paste(spl,":",ticker, collapse="") )
  return(c(spl, ticker))
}