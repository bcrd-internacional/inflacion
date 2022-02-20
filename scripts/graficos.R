
# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(dygraphs)
library(plotly)
library(ggthemes)
library(highcharter)

# Objetos utilitarios -----------------------------------------------------
source(here::here("scripts/functions.R"), encoding = "utf-8")

# Importando la data ------------------------------------------------------
ipc <- get_ipc_data()
monetary_stats <- get_monetary_stats()

# Adecuando la data -------------------------------------------------------
ipc <- ipc %>%
  mutate(ipc_2y = (ipc - lag(ipc, 24)) / lag(ipc, 24) * 100) %>% 
  pivot_longer(
    cols = contains("ipc"), names_to = "variable_key", values_to = "indicador") %>%
  mutate(
    variable = recode(variable_key,
                      "ipc" = "Índice",
                      "ipc_vm" = "Inflación mensual",
                      "ipc_vi" = "Inflación interanual",
                      "ipc_p12" = "Inflación promedio 12 meses",
                      "ipc_2y" = "Inflación a dos años",
                      "ipc_vd" = "Inflación con diciembre")
  )


  

# Gráficos ----------------------------------------------------------------

# Inflación mensaul
ipc %>%
  filter(
    variable_key == "ipc_vm",
    fecha > "2012-06-01"
  ) %>%
  ggplot(aes(x = fecha, y = indicador)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(breaks = scales::pretty_breaks(5)) +
  labs(x = NULL, y = 'Porcentaje')

# Inflación a 12 y 24 meses
(plot_inflacion <- ipc %>%
  filter(
    variable_key %in% c("ipc_vi", "ipc_2y"),
    fecha > "2012-06-01"
  ) %>%
  mutate(label = glue::glue("{date_label(fecha)}: {round(indicador, 2)}")) %>% 
  ggplot(aes(x = fecha, y = indicador, color = variable, text = label,
             group = variable)) +
  geom_line() +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(
    get_tailwind_colors('blue', c(700)),
    get_tailwind_colors('red', c(700))
    )
  )
)


ggplotly(plot_inflacion, tooltip = "text") %>%
  layout(legend = list(orientation = "h", x = 0.2)) %>% 
  plotly::config(displayModeBar = F, responsive = TRUE)

# Agregados monetarios

paleta <- c(
  get_tailwind_colors('sky', c(300)),
  get_tailwind_colors('sky', c(500)),
  get_tailwind_colors('blue', c(600))
)

monetary_stats %>%
  filter(nivel == 1, categoria == 'Agregados monetarios', date > '2011-12-01') %>%
  mutate(short_names = fct_reorder(factor(short_names), -values)) %>% 
  ggplot(aes(x = date, y = values)) +
  geom_area(aes(fill = short_names), alpha = .9, position = 'identity') +
  geom_line(aes(group = short_names), size = 1.5, color = 'white') + 
  geom_line(aes(color = short_names), size = 1, show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = 'Millones de RD$', x = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = paleta) +
  scale_color_manual(values = paleta)

monetary_stats %>%
  filter(nivel == 1, categoria == 'Agregados monetarios', date > '2011-12-01') %>%
  mutate(
    short_names = fct_reorder(factor(short_names), -values),
    values = round(values, 2)
    ) %>%
  hchart('line', hcaes(x = date, y = values, group = short_names))


