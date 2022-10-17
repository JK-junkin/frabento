
# devtools::install(here::here())
devtools::load_all(here::here(), export_all = FALSE)
devtools::document(here::here())
ls("package:frabento")

mixFonts(paste0(0:1, "歳"))

mixFonts("\nシラスを除く\nカタクチイワシ0歳魚")

needs(tidyverse, fontregisterer, ggtext)

devtools::check()

stringi::stri_escape_unicode("シラス込み\n0歳魚")
# [1] "\\u30b7\\u30e9\\u30b9\\u8fbc\\u307f\\n0\\u6b73\\u9b5a"
stringi::stri_unescape_unicode("シラス込み\n0歳魚")
# [1] "シラス込み\n0歳魚"
