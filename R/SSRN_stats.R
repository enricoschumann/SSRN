SSRN_stats <- function(id, write.file = FALSE,
                       append.file = TRUE,
                       file.prefix = "SSRN",
                       file.timestamp = TRUE) {

    u <- curl(paste0("https://papers.ssrn.com/sol3/",
                     "cf_dev/AbsByAuth.cfm?per_id=", id))
    txt <- readLines(u, warn = FALSE, encoding = "UTF-8")
    close(u)
    txt <- iconv(txt, from = "UTF-8", to = "UTF-8")

    txt_ <- paste(txt, collapse = "")
    pos <- gregexpr("optClickTitle", txt_)
    pos <- c(pos[[1]], nchar(txt_))

    papers <- data.frame(Id = numeric(length(pos) - 1),
                         Title = character(length(pos) - 1),
                         Downloads = character(length(pos) - 1),
                         stringsAsFactors = FALSE)
    for (i in seq_len(length(pos) - 1)) {
        p1 <- substr(txt_, pos[i], pos[i+1])
        i_title <- sub(".*?<span>([^<]+)<.*", "\\1", p1)
        i_id <- sub(".*?abstract_id=([0-9]+).*", "\\1", p1)
        i_dl <- sub(".*?Downloads[^0-9]+?([0-9,]+)</span.*", "\\1", p1)

        papers[["Id"]][i] <- i_id
        papers[["Title"]][i] <- i_title
        papers[["Downloads"]][i] <- i_dl

    }

    papers[["Downloads"]] <- as.numeric(gsub(",", "", papers[["Downloads"]], fixed = TRUE))


    ## overall rank; two backreferences: rank by downloads, by citations
    tmp <- gsub(".*<p>SSRN RANKINGS</p>.*?<h4>Top(.*?)</h4>.*<p>SSRN RANKINGS</p>.*?<h4>Top(.*?)</h4>.*",
         "\\1---\\2", txt_)

    tmp <- strsplit(tmp, "---", fixed = TRUE)[[1L]]
    rank <- as.numeric(gsub(",", "", tmp, fixed = TRUE))

    if (write.file && NROW(papers)) {
        for (i in seq_len(NROW(papers)))
            cat(as.character(Sys.time()), ";", papers[["Downloads"]][[i]], ";\n",
                file = paste(file.prefix, papers[["Id"]][[i]], ".csv", sep = ""),
                append = TRUE, sep = "")
    }
    list(author.rank.downloads = rank[[1L]],
         author.rank.citations = rank[[2L]],
         total.downloads = sum(papers[["Downloads"]]),
         downloads = papers)

}
