setwd("C:/Users/burgo/OneDrive/Documentos/Projetos de Análise de Dados/Projeto 1/Data")

library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyverse)

df <- read_csv("DataBase - 202402-divvy-tripdata.csv")
df <- na.omit(df)

# Calcular a retabilidade média de cada dia da semana
df$week_day <- factor(df$week_day, levels = 1:7, labels = c("Domingo", "Segunda-feira", "Terça-feira", "Quarta-feira", "Quinta-feira", "Sexta-feira", "Sábado"))

df$tour_duration <- as.numeric(hms(df$tour_duration)) / 60

summary_df <- df %>%
  group_by(week_day) %>%
  summarise(mean_rentabilidade = mean(tour_duration))

summary_df <- summary_df %>%
  mutate(percentage = mean_rentabilidade / sum(mean_rentabilidade) * 100)

ggplot(summary_df, aes(x = week_day, y = mean_rentabilidade, fill = week_day)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Rentabilidade Média por Dia da Semana",
       fill = "Dia da semana",
       y = "Rentabilidade Média (minutos)",
       x = "Dia da Semana")

# Alternativamente, criar o gráfico de pizza com porcentagens
ggplot(summary_df, aes(x = "", y = mean_rentabilidade, fill = week_day)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Rentabilidade Média por Dia da Semana",
       fill = "Dia da semana",
       y = "Rentabilidade Média (minutos)") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3)

# Rotas mais usadas
route_counts <- df %>%
  count(start_station_name, end_station_name, sort = TRUE)

top_routes <- route_counts %>%
  head(10)

ggplot(top_routes, aes(x = reorder(paste(start_station_name, "-", end_station_name), n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Rotas Mais Usadas",
       x = "Rota",
       y = "Número de Viagens")

# Trajetos mais percorridos
path_counts <- df %>%
  mutate(path = paste(pmin(start_station_id, end_station_id), "-", pmax(start_station_id, end_station_id))) %>%
  count(path, sort = TRUE)

path_routes <- path_counts %>%
  head(10)

ggplot(path_routes, aes(x = reorder(path, n), y = n)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  coord_flip() +
  labs(title = "Top 10 Rotas Mais Usadas (Ida e Volta)",
       x = "Rota",
       y = "Número de Viagens")

# Horário de pico
df$start_hour <- hour(hms(format(ymd_hms(df$started_at), "%H:%M:%S")))
df$end_hour <- hour(hms(format(ymd_hms(df$ended_at), "%H:%M:%S")))

start_hour_counts <- df %>%
  count(start_hour) %>%
  arrange(desc(n))

end_hour_counts <- df %>%
  count(end_hour) %>%
  arrange(desc(n))

top_start_hours <- start_hour_counts$`start_hour`[1:3]
top_end_hours <- end_hour_counts$`end_hour`[1:3]

peak_hours_df <- df %>%
  filter(start_hour %in% top_start_hours | end_hour %in% top_end_hours)

ggplot(peak_hours_df, aes(x = factor(start_hour))) +
  geom_bar(fill = "orange") +
  labs(title = "Número de Viagens nos Horários de Pico",
       x = "Horário de início da viagem",
       y = "Número de Viagens")


# Outras observações
mean_duration <- mean(df$tour_duration)

median_duration <- median(df$tour_duration)

max_duration <- max(df$tour_duration)

min_duration <- min(df$tour_duration)

print(paste("Mean tour duration:", mean_duration))
print(paste("Median tour duration:", median_duration))
print(paste("Maximum tour duration:", max_duration))
print(paste("Minimum tour duration:", min_duration))

# Estatísticas por tipo de membro
duration_by_member_type <- df %>%
  group_by(member_casual) %>%
  summarise(mean_duration = mean(tour_duration),
            median_duration = median(tour_duration),
            max_duration = max(tour_duration),
            min_duration = min(tour_duration))

print("Mean tour duration by member type:")
print(duration_by_member_type)

# Média de duração por tipo de membro e dia da semana
mean_duration_by_member_and_day <- df %>%
  group_by(member_casual, week_day) %>%
  summarise(mean_duration = mean(tour_duration))

print("Mean tour duration by member type and day of week:")
print(mean_duration_by_member_and_day)

# Gráfico de tipo clientes por dia da semana onde o primeiro dia é domingo
df %>%
  group_by(member_casual, week_day) %>%
  summarise(number_of_rides = n(), average_duration = mean(tour_duration)) %>%
  ggplot(aes(x = week_day, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Tipo de clientela por dia da semana",
       x = "Dia",
       y = "Alugueis",
       fill = "Tipo de cliente")

# Gráfico de tipo clientes por dia da semana onde o primeiro dia é domingo
ggplot(mean_duration_by_member_and_day, aes(x = week_day, y = mean_duration, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Média de Duração de Viagem por Tipo de Cliente e Dia da Semana",
       x = "Dia da Semana",
       y = "Média de Duração de Viagem (minutos)") +
  theme_minimal()

#Tempo Médio de Viagem por Tipo de Bicicleta
df$started_at <- ymd_hms(df$started_at)
df$ended_at <- ymd_hms(df$ended_at)

df$tour_duration <- as.numeric(difftime(df$ended_at, df$started_at, units = "mins"))

df <- df %>% filter(tour_duration > 0)

average_duration <- df %>%
  group_by(rideable_type) %>%
  summarise(mean_duration = mean(tour_duration, na.rm = TRUE))

print(average_duration)

ggplot(average_duration, aes(x = rideable_type, y = mean_duration, fill = rideable_type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Tempo Médio de Viagem por Tipo de Bicicleta",
       x = "Tipo de Bicicleta",
       y = "Tempo Médio de Viagem (minutos)") +
  theme(legend.position = "none")

# Comparação de duração de viagem entre membros e usuários casuais
duration_comparison <- df %>%
  group_by(member_casual) %>%
  summarise(mean_duration = mean(tour_duration, na.rm = TRUE),
            median_duration = median(tour_duration, na.rm = TRUE),
            max_duration = max(tour_duration),
            min_duration = min(tour_duration))

total_duration <- sum(duration_comparison$mean_duration)

ggplot(duration_comparison, aes(x = "", y = mean_duration, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Comparação de Duração de Viagem\nMembros vs Usuários Casuais",
       fill = "Tipo de Cliente",
       y = "Tempo Médio de Viagem") +
  geom_text(aes(label = paste0(round(mean_duration / total_duration * 100), "%")), position = position_stack(vjust = 0.5)) +
  theme(legend.position = "bottom")


# Definir dias da semana versus fim de semana
df <- df %>%
  mutate(day_type = ifelse(weekdays(started_at) %in% c("sábado", "domingo"), "Fim de Semana", "Dia de Semana"))

usage_comparison <- df %>%
  group_by(day_type) %>%
  summarise(number_of_rides = n())

ggplot(usage_comparison, aes(x = day_type, y = number_of_rides, fill = day_type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Diferença de Uso por Dia da Semana e Fim de Semana",
       x = "Tipo de Dia",
       y = "Número de Viagens") +
  theme(legend.position = "none")

df <- df %>%
  mutate(distance = distVincentyEllipsoid(cbind(start_lng, start_lat), cbind(end_lng, end_lat)))

df <- df %>%
mutate(tempo_gasto = as.numeric(difftime(ended_at, started_at)),
       tempo_gasto_segundos = tempo_gasto * 60) %>%
  group_by(member_casual) %>%
  summarise(tempo_medio_segundos = mean(tempo_gasto_segundos))

ggplot(df, aes(x = member_casual, y = tempo_medio_segundos)) +
  geom_bar(stat = "identity") +
  labs(title = "Tempo médio gasto por tipo de membro",
       x = "Tipo de membro",
       y = "Tempo médio (segundos)")

#member vs casual diferenças
df <- df %>%
  mutate(started_at = ymd_hms(started_at),
         ended_at = ymd_hms(ended_at))

df <- df %>%
  mutate(start_hour = hour(started_at))

hourly_counts <- df %>%
  group_by(start_hour, member_casual) %>%
  summarise(trip_count = n()) %>%
  ungroup()

ggplot(hourly_counts, aes(x = start_hour, y = trip_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Horários de Pico por Tipo de Usuário",
       x = "Hora do Dia",
       y = "Número de Viagens") +
  theme_minimal()

#Tipo de bicicletas por clientes
df_filtered <- df %>%
  filter(rideable_type %in% c("classic_bike", "electric_bike"))

summary_df <- df_filtered %>%
  group_by(member_casual, rideable_type) %>%
  summarise(total_trips = n()) %>%
  mutate(percent_trips = total_trips / sum(total_trips) * 100)

ggplot(summary_df %>% filter(member_casual == "member"), aes(x = rideable_type, y = percent_trips, fill = rideable_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentagem de Viagens por Tipo de Bicicleta (Membros)",
       x = "Tipo de Bicicleta",
       y = "Porcentagem de Viagens (%)") +
  theme_minimal()

ggplot(summary_df %>% filter(member_casual == "casual"), aes(x = rideable_type, y = percent_trips, fill = rideable_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentagem de Viagens por Tipo de Bicicleta (Usuários Casuais)",
       x = "Tipo de Bicicleta",
       y = "Porcentagem de Viagens (%)") +
  theme_minimal()

#Tipo de Bicicleta por estação
df_bikes <- df %>%
  filter(rideable_type %in% c("electric_bike", "classic_bike"))

station_counts <- df_bikes %>%
  count(start_station_name, rideable_type) %>%
  pivot_wider(names_from = rideable_type, values_from = n, values_fill = list(n = 0)) %>%
  mutate(total = electric_bike + classic_bike,
         percentage_electric = ifelse(total == 0, 0, electric_bike / total * 100)) %>%
  arrange(desc(percentage_electric))

top_station <- station_counts %>%
  slice_max(percentage_electric, n = 1)

top_station_name <- top_station$start_station_name

top_station_data <- df_bikes %>%
  filter(start_station_name == top_station_name) %>%
  count(rideable_type) %>%
  mutate(percentage = round(n / sum(n) * 100))

ggplot(top_station_data, aes(x = "", y = percentage, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = paste("Percentual de Bicicletas Alugadas na Estação", top_station_name),
       fill = "Tipo de Bicicleta",
       x = NULL,
       y = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5),
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("classic_bike" = "steelblue", "electric_bike" = "orange"))

f_bikes <- df %>%
  filter(rideable_type %in% c("electric_bike", "classic_bike"))

station_counts <- df_bikes %>%
  count(start_station_name, rideable_type) %>%
  pivot_wider(names_from = rideable_type, values_from = n, values_fill = list(n = 0)) %>%
  mutate(total = electric_bike + classic_bike,
         percentage_electric = ifelse(total == 0, 0, electric_bike / total * 100)) %>%
  filter(percentage_electric < 99.5) %>% 
  arrange(desc(percentage_electric))

top_station <- station_counts %>%
  slice_max(percentage_electric)

top_station_name <- top_station$start_station_name

top_station_data <- df_bikes %>%
  filter(start_station_name == top_station_name) %>%
  count(rideable_type) %>%
  mutate(percentage = round(n / sum(n) * 100))

ggplot(top_station_data, aes(x = "", y = percentage, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = paste("Percentual de Bicicletas Alugadas na Estação", top_station_name),
       fill = "Tipo de Bicicleta",
       x = NULL,
       y = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5),
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c("classic_bike" = "steelblue", "electric_bike" = "orange"))

