## SSRN_stats <- function(id, write.files = FALSE,
##                        prefix = "SSRN",
##                        timestamp = TRUE) {
##     u <- url(paste("http://papers.ssrn.com/sol3/cf_dev/AbsByAuth.",
##                    "cfm?per_id=", id, sep = ""))
##     txt <- readLines(u, warn = FALSE, encoding = "UTF-8")
##     close(u)
##     txt_ <- paste(txt, collapse = "\n")
##     il <- grep(paste("<a class=\"textlink\" ",
##                      "href=\"http://ssrn\\.com/abstract=[0-9]+\" ",
##                      "target=\"_blank\">.*</a>", sep = ""), txt)
##     nid <- gsub(paste("<a class=\"textlink\" ",
##                       "href=\"http://ssrn\\.com/abstract=(\\d+)\" ",
##                       "target=\"_blank\">(.*)</a>", sep = ""),
##                 "\\1 <%split%> \\2", txt[il])
##     nid <- strsplit(nid, " <%split%> ")
##     pids <- sapply(nid, `[[`, 1L)
##     pnames <- sapply(nid, `[[`, 2L)
##     il <- grep("<td align=\"center\" class=\"partofVersion\">", txt)
##     dl <- suppressWarnings(gsub("([0-9,]*)<br/>", "\\1",
##                                            txt[il+1L]))
##     dl <- as.numeric(gsub(",", "", dl, fixed = TRUE))
##     dl[is.na(dl)] <- 0
##     res <- data.frame(Id = pids, Title = pnames,
##                       Downloads = dl, stringsAsFactors = FALSE)
##     ## rank <- gsub(".*SSRN Author Rank: +([0-9,]+) +<a.*", "\\1",
##     ##              txt[grep("SSRN Author Rank", txt_)])

##     tmp <- gsub(".*SSRN Author Rank:.*?([0-9,]+) by Downloads.*?([0-9,]+) by Citations.*",
##                 "\\1---\\2", txt_)
##     tmp <- strsplit(tmp, "---", fixed = TRUE)[[1L]]
##     rank <- as.numeric(gsub(",", "", tmp, fixed = TRUE))
    
    
##     if (write.files && NROW(res))
##         for (i in seq_len(NROW(res)))
##             cat(as.character(Sys.time()), ";", dl[i], ";\n",
##                 file = paste(prefix, pids[i], ".csv", sep = ""),
##                 append = TRUE, sep = "")

##     list(author.rank.downloads = rank[[1L]],
##          author.rank.citations = rank[[2L]],
##          total.downloads = sum(dl),
##          downloads = res)
## }

## id <- 372409
## id <- 895233
SSRN_stats <- function(id, write.file = FALSE,
                       append.file = TRUE,
                       file.prefix = "SSRN",
                       file.timestamp = TRUE) {

    u <- url(paste0("https://papers.ssrn.com/sol3/",
                    "cf_dev/AbsByAuth.cfm?per_id=", id))
    txt <- readLines(u, warn = FALSE, encoding = "UTF-8")
    close(u)
    
    txt_ <- paste(txt, collapse = "\n")
    pos <- gregexpr("<a class=\"textlink\" href=\"https://ssrn.com/abstract=",
                    txt_)
    pos <- c(pos[[1]], nchar(txt_))
    ## ans <- character(length(pos) - 1)

    papers <- data.frame(Id = numeric(length(pos) - 1),
                         Title = character(length(pos) - 1),
                         Downloads = character(length(pos) - 1),
                         stringsAsFactors = FALSE)
    for (i in seq_len(length(pos) - 1)) {
        txt0 <- substr(txt_, pos[i], pos[i+1])
        
        ## three backreferences: id, title, downloads
        i_id <- as.numeric(gsub(".*https://ssrn.com/abstract=([0-9]+).*", "\\1", txt0))
        i_title <- gsub(paste0("<a class=\"textlink\" href=\"https://ssrn.com/abstract=",
                               "[0-9][0-9]*\" target=\"_blank\">([^<]+).*"), "\\1", txt0)
        i_dl <- gsub(".*partofVersion\">\n?([,0-9]+).*", "\\1", txt0)

        papers[["Id"]][i] <- i_id
        papers[["Title"]][i] <- i_title
        papers[["Downloads"]][i] <- i_dl
        ## ans[i] <- gsub(paste0("<a class=\"textlink\" href=\"https://ssrn.com/abstract=",
        ##                       "([0-9][0-9]*)\" target=\"_blank\">",
        ##                       "([^<]+)</a>\n?<span class=\"nobr\">.*",
        ##                       "<td align=\"center\" class=\"partofVersion\">\n?([0-9,]*?)<.*"),
        ##                "\\1---\\2---\\3", txt0)

        ## i_dl <- gsub(".*partofVersion\">\n([0-9]+).*", "\\1", txt0)

        ##     paste0("<a class=\"textlink\" href=\"https://ssrn.com/abstract=",
        ##             "[0-9][0-9]*\" target=\"_blank\">",
        ##             "[^<]+</a>\n?<span class=\"nobr\">.*",
        ##             "<td align=\"center\" class=\"partofVersion\">\n?([0-9,]*?)<.*"),
        ##                "\\1---\\2---\\3", txt0)
        
        ## gsub(paste0("<a class=\"textlink\" href=\"https://ssrn.com/abstract=",
        ##             "([0-9][0-9]*)\" target=\"_blank\">",
        ##             "([^<]+).*"),
        ##      "\\1---\\2---\\3", txt0)
        
    }

    papers[["Downloads"]] <- as.numeric(gsub(",", "", papers[["Downloads"]], fixed = TRUE))
    
    ## ans0 <- strsplit(ans, "---", fixed = TRUE)
    
    ## pid <- as.numeric(unlist(lapply(ans0, `[[`, 1)))
    ## nd <- as.numeric(gsub(",", "", unlist(lapply(ans0, `[[`, 3)),
    ##                       fixed = TRUE))
    ## title <- unlist(lapply(ans0, `[[`, 2))

    ## papers <- data.frame(Id = pid,
    ##                      Title = title,
    ##                      Downloads = nd,
    ##                      stringsAsFactors = FALSE)

    ## overall rank; two backreferences: rank by downloads, by citations
    tmp <- gsub(paste0(".*SSRN Author Rank:.*?([0-9,]+) ",
                       "by Downloads.*?([0-9,]+) by Citations.*"),
                "\\1---\\2", txt_)
    tmp <- strsplit(tmp, "---", fixed = TRUE)[[1L]]
    rank <- as.numeric(gsub(",", "", tmp, fixed = TRUE))
        
    if (write.file && NROW(papers)) {
        for (i in seq_len(NROW(papers)))
            cat(as.character(Sys.time()), ";", papers[["Downloads"]][[i]], ";\n",
                file = paste(prefix, papers[["Id"]][[i]], ".csv", sep = ""),
                append = TRUE, sep = "")
    }
    list(author.rank.downloads = rank[[1L]],
         author.rank.citations = rank[[2L]],
         total.downloads = sum(dl),
         downloads = papers)
    
}
## SSRN_stats()
