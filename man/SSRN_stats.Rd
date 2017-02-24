\name{SSRN_stats}
\alias{SSRN_stats}
\alias{SSRN-package}
\alias{SSRN}
\title{
  Download Download Statistics from SSRN
}
\description{
  Download download statistics from SSRN
}
\usage{
SSRN_stats(id, write.file = FALSE, append.file = TRUE,
           file.prefix = "SSRN", file.timestamp = TRUE)
}
\arguments{
  \item{id}{
    integer: the author's Id
  }
  \item{write.file}{
    logical
  }
  \item{append.file}{
    integer: the author's Id
  }
  \item{file.prefix}{
    character    
  }
  \item{file.timestamp}{
    logical
  }  
}
\details{

  The function reads the whole author page and then looks up the papers
  via regular expressions.
  
}
\value{
  A list
  \item{author.rank.downloads}{}
  \item{author.rank.citations}{}
  \item{total.downloads}{i}
  \item{downloads}{ a data.frame}
}
\author{
  Enrico Schumann
}
\examples{
\dontrun{
## the package author's SSRN page
SSRN_stats(895233)
}
}
