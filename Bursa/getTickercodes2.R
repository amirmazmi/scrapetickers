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

# ---------------------------------------------------------
dirwork <- "/home/speedy/Desktop/intrno"
setwd(dirwork)

pathout <- file.path(dirwork, "data" )


# ---------------------------------------------------------
# define functions
# ---------------------------------------------------------

# get the html of a page
getHTML <- function(page){
    base_url <- "https://www.malaysiastock.biz/"
    path <- "Listed-Companies.aspx"
    query <- list(
                  type = "A",
                  value = page  )
    page <- modify_url(base_url, path=path, query=query)
    cat("\nGeting the following url:\n\t", page)
    htmlCode <- GET(page)
    content <- content(htmlCode, "text")

    return(content)
}

# parse the content into xml nodes
parseHTML <- function( HTMLcontent){
  rootNode <- read_html(HTMLcontent)
  nodes <- xml_find_all(rootNode,
                        "//table[@class='marketWatch']")[[1]]
  datanodes <- xml_children(nodes)
  return(datanodes)
}

# extract into dataframe
makeDF <- function( datanodes, dfdata){
    # run this in a loop based on length of  datanodes
    for(k in seq(2, length(datanodes))){
        newnode <- xml_new_root(datanodes[[k]])

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
        # print(part1[k])

        # ticker code
        patt <- "(?<=\\().*(?=\\))" #"\\(.*?\\)"
        m <- regexec(patt, part1[1], perl=T)
        tickcode <- regmatches( part1[1], m)[[1]]

        # Sector, Market Cap, Last Price, PE, DY, ROE
        part2 <- newnode %>%
                  xml_find_all("//td") %>%
                  xml_text()
        # print(part2[3:8])

        rowdf <- c(part1[2], tickcode, shariah, part2[3:8])
        dfdata[k-1,] <- rowdf
    }
    return(dfdata)
}



# ---------------------------------------------------------
# define vector of pages
lspage <- sample(c(LETTERS, "0"))

cat("\n\n\t ##### Starting stock sraper #####\n\n")
for(k in seq(2)){
# for(k in seq_along(lspage)){
    currpage <- lspage[k]
    cat( paste0("\n\n--------- Iteration: [", currpage, "]"))
    datanodes <- getHTML(currpage) %>%
    parseHTML()

    header<- xml_children(datanodes[[1]]) %>%
      xml_contents() %>%
      as_list() %>%
      unlist()
    coln <- c(header[1],"code",header[2:8])

    #create dataframe
    dfdata <- data.frame( matrix( ncol=length(coln), nrow=0)) %>%
      setNames( coln)
    cat("\nEmpty dataframe created with the following headers:\n\t",
        names(dfdata))
    dfout <- datanodes %>% makeDF( dfdata)
    cat( paste0("\nData for page \"", currpage,
                "\" has following dimensions: ",
                dim(dfout)[[1]], "x", dim(dfout)[[2]]
                ))

    # write out to csv
    fname <- paste0( "stockdata_", currpage, ".csv")
    cat("\nWriting file out to CSV: ", fname)
    write.csv(dfout, file=file.path(pathout, fname), row.names=F)

    # rate limit to prevent being blocked
    sleeptime <- sample( seq( 7, 12, 0.1), 1)
    cat("\n\t ##### Sleeping for ", sleeptime, " seconds #####")
    Sys.sleep(sleeptime)
}







