# --------------------------------------------------------- 
# 
# Script to automate finding tickers from google finance
# 
# 
# ---------------------------------------------------------
# NOTES
# ---------------------------------------------------------
# first understand components of URLquery
#   using parse_url
# 
#   parse_url("https://www.google.com/finance?q=A%20%26%20M%20REALTY%20+klse")
#  
# then can construct using modify_url() 
#
# ---------------------------------------------------------



library(pacman)
p_load(dplyr)
p_load(httr)
p_load(XML)

# ---------------------------------------------------------
# define functions
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


get_ticker_loop <- function(df){
    # create ticker column
    df[,"ticker"] <- "empty"
    # loop and get ticker
    for(i in seq_along(df[,1]) ){
        df[i,c("companyname","ticker")] <- tryCatch({
            getTicker(df[i,])
        }, warning=function(war){
            print(paste("Warning:",war))
        },  error=function(err){
            print(paste("Error:",err))
        },
        finally=function(){print("Done")
        })
        
        if(i==10){
            Sys.sleep( sample(3:10, 1))
            
        }
    }
    return(df)
}



# ---------------------------------------------------------
# get list of companies from BURSA
#   table is dynamic - can't scrape html, copy and paste
# ---------------------------------------------------------


# urlklse <- "http://www.bursamalaysia.com/market/listed-companies/list-of-companies/main-market"
# htmlbursa <- GET(urlklse)
# if(htmlbursa$status_code == 200){
#     nodebursa <- xmlRoot( htmlParse(content(htmlbursa, "text"), asText=T ))
#     which(xpathSApply(nodebursa, "//div", xmlGetAttr, "id", "NA") != "NA")
#     dfklse <- xmlToList(nodebursa, addAttributes=F, simplify=T)
# }
# 
# close(htmlbursa)




# ---------------------------------------------------------
# get tickers from GOOGLE
# ---------------------------------------------------------

setwd("path/to/folder")

fpath <- getwd()
fname <- grep("klse main",list.files(),value=T)
locfile <- file.path(fpath,fname)

outfile <- "ticker_names.csv"
outpath <- file.path(fpath, outfile)

downloadDate <- gsub(":",".",date())
# "Wed May 03 14.41.13 2017"

comp_names <- read.csv(locfile, stringsAsFactors=F, 
                        header=F, col.names="companyname")



# get the tickers from google
comp_ticker <- comp_names %>% get_ticker_loop()

# look at the ones skipped
df[which(comp_ticker[,2]=="skipped"),]


# write out
write.csv(comp_ticker, file=outpath, 
          quote=F, row.names=F)







########################################################
########################################################
########################################################

# ---------------------------------------------------------
# scratchpad - just to fix whitespace
# ---------------------------------------------------------
# 
# fixfile <- grep("ticker_names.csv", list.files(), value=T)
# fixloc <- file.path(getwd(),fixfile)
# 
# fix_names <- read.csv(fixloc, stringsAsFactors=F)
# 
# 
# fix_names[,1] <- unlist(lapply(fix_names[,1], trimws, "right"))
# 
# write.csv(fix_names, file="ticker_names.csv", 
#           quote=F, row.names=F)

fixfile <- grep("ticker_names.csv", list.files(), value=T)
fixloc <- file.path(getwd(),fixfile)

fix_names <- read.csv(fixloc, stringsAsFactors=F)


length(which(fix_names[,2] == "skipped"))
# 17  19  29  30  33  34  35  37  56  59  69  70
#  74  86  92  93  98 105 119 120 122 136 138 140
# 142 153 163 172 175 180 184 200 201 204 207 209
# 214 222 223 225 228 230 235 237 245 271 273 285
# 288 292 295 296 298 321 326 327 346 351 353 360
# 364 374 378 382 385 392 393 397 406 411 426 431
# 438 445 452 464 469 471 474 475 478 502 504 510
# 511 512 513 525 528 536 540 542 562 570 584 585
# 589 596 615 616 619 620 628 630 633 634 637 657
# 661 672 676 678 680 681 698 701 704 709 714 729
# 753 773 777 780 787 788 798


df1 <- df %>% get_ticker_loop()

length(df1[which(df1[,2] != "skipped"),])



spl <- df[125,1]

spl1 <- sub("BERHAD | HOLDINGS", "", as.character(spl))
con <- modify_url("https://www.google.com/finance", query= 
                      list( q=spl1))
con1 <- paste0(con,"+klse", collapse="")
print(con1)

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






# ---------------------------------------------------------
# manual replacement
# ---------------------------------------------------------

# 
# df <- fix_names[which(fix_names[,2] == "skipped"),]
# dim(df)
# 
# print( df[1:7])
# 
# df[3,] <- c(df[3,1],"KLSE:TASEK")
# df[4,] <- c(df[4,1],"KLSE:TEKALA")
# df[5,] <- c(df[5,1],"KLSE:TEXCHEM")
# df[6,] <- c(df[6,1],"KLSE:TOMYPA")
# df[7,] <- c(df[7,1],"KLSE:UNISEM")
# print( df[1:7,])
# 
# 
# 
# 
# # assign back to main and write
# fix_names[which(fix_names[,2] == "skipped"),] <- df
# write.csv(fix_names, file="ticker_names2.csv", 
#                     quote=F, row.names=F)
# 
# 



# ---------------------------------------------------------
# Pre-lim test

# name <- as.character(comp_names[5,1])
# 
# spl <- unlist(strsplit(name, split=" ", fixed=T))
# spl1 <- sub("BERHAD", "", name)
# con <- modify_url("https://www.google.com/finance", query= 
#                       list( q=spl1))
# con1 <- paste0(con,"+klse", collapse="")
# 
# content2 <- content( GET(con1), as="text")
# 
# 
# parsedhtml <- htmlParse(content2, asText=T)
# rootNode <- xmlRoot(parsedhtml)
# charstring <- xmlValue(rootNode[[1]][[2]])
# 
# # xpathSApply(parsedhtml,"//title", xmlValue)
# charlist <- unlist(strsplit(charstring, split=" "))
# print(charlist)
# 
# 
# grep("KLSE", charlist, value=T)
# 



