pizzRtime <- function(color = "reset", cat = T){
  color_codes <- c(
    black      = "\033[30m",
    red        = "\033[31m",
    green      = "\033[32m",
    yellow     = "\033[33m",
    blue       = "\033[34m",
    magenta    = "\033[35m",
    cyan       = "\033[36m",
    white      = "\033[37m",
    reset      = "\033[39m"
  )

  if (!color %in% names(color_codes)) {
    stop(sprintf("Ungültige Farbe: '%s'. Verfügbare Farben sind: %s",
                 color,
                 paste(names(color_codes), collapse = ", ")))
  }

  art <- paste0('           _           ____  __  _                 __\n',
  ('    ____  (_)_______  / __ \\/ /_(_)___ ___  ___   / /\n'),
  ('   / __ \\/ /_  /_  / / /_/ / __/ / __ `__ \\/ _ \\ / /\n'),
  ('  / /_/ / / / /_/ /_/ _, _/ / / / / / / / /  __//_/\n'),
  (' / .___/_/ /___/___/ / |_|\\__/_/_/ /_/ /_/\\___/(_)\n'),
  ('/_/\n'))


  str_art <- sprintf("%s%s%s", color_codes[color], art, color_codes["reset"])
  if (cat) cat(str_art) else str_art
}
