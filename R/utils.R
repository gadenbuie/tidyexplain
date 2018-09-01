`%||%` <- function(x, y) if (is.null(x)) y else x

choose_text_color <- function(x, black = "#000000", white = "#FFFFFF") {
  # x = color_hex
  color_rgb <- col2rgb(x)
  # modified from https://stackoverflow.com/a/3943023/2022615
  # following W3 guidelines: https://www.w3.org/TR/WCAG20/#relativeluminancedef
  color_rgb <- color_rgb / 255
  color_rgb[color_rgb <= 0.03928] <- color_rgb[color_rgb <= 0.03928]/12.92
  color_rgb[color_rgb  > 0.03928] <- ((color_rgb[color_rgb > 0.03928] + 0.055)/1.055)^2.4
  lum <- t(color_rgb) %*% c(0.2126, 0.7152, 0.0722)
  lum <- lum[,1]
  # threshold is supposed to be 0.179 but 1/3 seems to work better for our plots
  ifelse(lum > 1/3, black, white)
}

get_input_text <- function(x) {
  if (!rlang::is_quosure(x)) x <- rlang::enquo(x)
  rlang::quo_name(x)
}

make_named_data <- function(x, y, data_names = c("x", "y")) {
  ll <- rlang::eval_tidy(rlang::quo(list(!!x, !!y)))
  names(ll) <- data_names
  ll
}
