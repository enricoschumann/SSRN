SSRN_stats <- function(id, write.files = FALSE,
                       prefix = "SSRN",
                       timestamp = TRUE) {
    u <- url(paste("http://papers.ssrn.com/sol3/cf_dev/AbsByAuth.",
                   "cfm?per_id=", id, sep = ""))
    txt <- readLines(u, warn = FALSE, encoding = "UTF-8")
    close(u)
    txt_ <- paste(txt, collapse = "\n")
    il <- grep(paste("<a class=\"textlink\" ",
                     "href=\"http://ssrn\\.com/abstract=[0-9]+\" ",
                     "target=\"_blank\">.*</a>", sep = ""), txt)
    nid <- gsub(paste("<a class=\"textlink\" ",
                      "href=\"http://ssrn\\.com/abstract=(\\d+)\" ",
                      "target=\"_blank\">(.*)</a>", sep = ""),
                "\\1 <%split%> \\2", txt[il])
    nid <- strsplit(nid, " <%split%> ")
    pids <- sapply(nid, `[[`, 1L)
    pnames <- sapply(nid, `[[`, 2L)
    il <- grep("<td align=\"center\" class=\"partofVersion\">", txt)
    dl <- suppressWarnings(gsub("([0-9,]*)<br/>", "\\1",
                                           txt[il+1L]))
    dl <- as.numeric(gsub(",", "", dl, fixed = TRUE))
    dl[is.na(dl)] <- 0
    res <- data.frame(Id = pids, Title = pnames,
                      Downloads = dl, stringsAsFactors = FALSE)
    ## rank <- gsub(".*SSRN Author Rank: +([0-9,]+) +<a.*", "\\1",
    ##              txt[grep("SSRN Author Rank", txt_)])

    tmp <- gsub(".*SSRN Author Rank:.*?([0-9,]+) by Downloads.*?([0-9,]+) by Citations.*",
                "\\1---\\2", txt_)
    tmp <- strsplit(tmp, "---", fixed = TRUE)[[1L]]
    rank <- as.numeric(gsub(",", "", tmp, fixed = TRUE))
    
    
    if (write.files && NROW(res))
        for (i in seq_len(NROW(res)))
            cat(as.character(Sys.time()), ";", dl[i], ";\n",
                file = paste(prefix, pids[i], ".csv", sep = ""),
                append = TRUE, sep = "")

    list(author.rank.downloads = rank[[1L]],
         author.rank.citations = rank[[2L]],
         total.downloads = sum(dl),
         downloads = res)
}
