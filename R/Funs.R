#' Hello World
#'
#' `hello` says _"Hello"_ in the user specified language. The user is asked to give her/his name so that the hello
#' message gets personalized.
#'
#' @param
#' who a `character` vector of length 1 that specifies the name of the person to whom the message is addressed.
#' @param
#' lang a `character` vector of length 1 that specifies the preferred language. Default to "EN" for English.
#' Other possible values include "FR" for French, "IT" for Italian, "ES" for Spanish, or "DE" for German.
#' Case is ignored.
#' @param
#' LangData an optional data.frame with two columns each of mode `character`. The first column gives the
#' language codes and the second column gives the corresponding "hello" word. Default to
#' `language`.
#'
#' see `?language`
#'
#' @return a `character` vector with a personalized _"hello"_ message.
#'
#' @export
#'
#' @examples
#' hello("James")
#' hello("Amelia", "Es")
#'
#' @importFrom stringr str_c

hello <- function(who, lang = "EN",LangData = language ) {

  llang <- tolower(lang)

  code <- LangData[,1]
  hello <- subset(LangData, code == llang)[[2]]

  if (!is.character(who)) str_c("Error in hello(",who,"): Please enter a valid name; see ?hello")
  else {
    ifelse(
    length(hello) == 1,
    str_c(hello,", ",who,"!",sep = ""),
    str_c("Sorry, ", who, ", ", "your language ", "('", llang, "') ", "is not available!", sep = "")
  ) |> cat()
  }
}
