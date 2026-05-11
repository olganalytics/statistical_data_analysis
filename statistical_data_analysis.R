# Łazik Curiosity mierzy poziom promieniowania kosmicznego (w mikrosiwertach na dobę, μSv/day). 
# Poniżej znajduje się 35 odczytów z ostatniego miesiąca misji.

# Dane (promieniowanie w uSv/day):
promieniowanie <- c(
  210, 225, 230, 215, 220, 240, 205, 235, 218, 222,
  245, 212, 228, 233, 219, 238, 208, 224, 231, 226,
  250, 215, 210, 234, 221, 242, 217, 229, 223, 236,
  248, 211, 227, 232, 220)

# 1. Przeprowadzenie dwówch wybranych testów, aby sprawdzić normalność danych.

# Hipotezy:
# H0: Dane pochodzą z rozkładu normalnego
# H1: Dane nie pochodzą z rozkładu normalnego
# Poziom istotności: alfa = 0.05

# Test Shapiro-Wilka
shapiro_wynik <- shapiro.test(promieniowanie)
shapiro_wynik

# p-value > 0.05, więc brak podstaw do odrzucenia H0
# Wniosek: dane są zgodne z rozkładem normalnym.

# Test Lillieforsa
library(nortest)
lillie_wynik <- lillie.test(promieniowanie)
lillie_wynik

# p-value > 0.05, więc brak podstaw do odrzucenia H0
# Wniosek: oba testy potwierdzają normalność danych.


# 2. Zbudowanie 90% przedziału ufności dla średniego poziomu promieniowania.
# Wybieram metodę t-studenta

alpha <- 0.10
n <- length(promieniowanie)
srednia <- mean(promieniowanie)
s <- sd(promieniowanie)

t_alpha <- qt(1 - alpha/2, df = n - 1)
dolna <- srednia - t_alpha * s / sqrt(n)
gorna <- srednia + t_alpha * s / sqrt(n)

cat("90% przedział ufności dla średniej: [", round(dolna, 2), ",", round(gorna, 2), "]\n")

# 3. Zbudowanie 90% przedział ufności dla wariancji i odchylenia standardowego poziomu promieniowania.

df <- n - 1
s2 <- var(promieniowanie)
chi2_upper <- qchisq(1 - alpha/2, df)
chi2_lower <- qchisq(alpha/2, df)
dolna_var <- (df * s2) / chi2_upper
gorna_var <- (df * s2) / chi2_lower

cat("90% przedział ufności dla wariancji: [", round(dolna_var, 2), ",", round(gorna_var, 2), "]\n")

dolna_sd <- sqrt(dolna_var)
gorna_sd <- sqrt(gorna_var)

cat("90% przedział ufności dla odchylenia standardowego: [", round(dolna_sd, 2), ",", round(gorna_sd, 2), "]\n")

# 4. Napisz wnioski statystyczne. 

# Na podstawie testu Shapiro-Wilka oraz Lillieforsa nie ma podstaw do odrzucenia
# hipotezy o normalności danych (p-value > 0.05 w obu testach), co oznacza że dane
# można uznać za pochodzące z rozkładu normalnego. Średni poziom promieniowania
# wynosi ok. 225 uSv/day, a wyznaczony 90% przedział ufności dla średniej jest
# stosunkowo wąski przy n = 35 obserwacjach.
# Przedział ufności dla odchylenia standardowego również nie jest szeroki,
# co potwierdza że zmienność pomiarów jest umiarkowana i dane są dość jednorodne.

# 5. Narysowanie histogram z naniesioną linią średniej oraz przedziałem ufności.

library(ggplot2)

df_prom <- data.frame(x = promieniowanie)

ggplot(df_prom, aes(x = x)) +
  geom_histogram(aes(y = ..density..),
                 bins = 10,
                 fill = "lightblue", color = "white", alpha = 0.8) +
  stat_function(fun = dnorm,
                args = list(mean = srednia, sd = s),
                color = "black", linewidth = 1, linetype = "solid") +
  geom_vline(xintercept = srednia,
             color = "pink", linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = c(dolna, gorna),
             color = "lightgreen", linewidth = 1, linetype = "dashed") +
  annotate("text", x = srednia + 0.3, y = 0.065,
           label = paste0("Średnia = ", round(srednia, 1)),
           color = "pink", hjust = 0, size = 3.5) +
  annotate("text", x = dolna - 0.3, y = 0.055,
           label = paste0("L = ", round(dolna, 1)),
           color = "lightgreen", hjust = 1, size = 3.2) +
  annotate("text", x = gorna + 0.3, y = 0.055,
           label = paste0("U = ", round(gorna, 1)),
           color = "lightgreen", hjust = 0, size = 3.2) +
  labs(
    title = "Histogram poziomu promieniowania kosmicznego",
    subtitle = "Różowa linia – średnia | Zielone linie przerywane – 90% Pu dla średniej",
    x = "Promieniowanie [uSv/dzień]",
    y = "Gęstość"
  ) +
  theme_minimal(base_size = 12)

# Interpretacja wykrsu:
# Dane są rozmieszczone symetrycznie wokół średniej ~225 uSv/dzień.
# Nałożona krzywa rozkładu normalnego dobrze dopasowuje się do histogramu,
# co potwierdza to co nam wyszło w wynikach testów normalności.
# 90% przedział ufności jest stosunkowo wąski, co świadczy o dużej precyzji
# estymacji przy n = 35 obserwacjach.Brak wyraźnych wartości odstających.


# Astrofizycy odkryli nową grupę planet pozasłonecznych. Podejrzewają, że ich średnia masa jest większa niż masa Ziemi (1.0 Me). 
# Zmierzono masę 12 takich obiektów (w jednostkach masy Ziemi).

# Dane (masa w jednostkach masy Ziemi [Me]):
masa_planet <- c(1.12, 1.05, 1.21, 0.98, 1.15, 1.08, 1.30, 1.02, 0.97, 1.11, 1.25, 0.99)

# 1. Sformułowanie hipotezy zerowej (H0) oraz alternatywnej (H1).

# H0: srednia masa planet wynosi 1.0 (rowna masie Ziemi)
# H1: srednia masa planet jest wieksza niz 1.0 (wieksza niz masa Ziemi)
# Test prawostronny, poziom istotnosci alpha = 0.05

# 2. Zbadanie prawdziwości hipotezy przy użyciu wbudowanej funkcji t.test() z 
# odpowiednim parametrem 'alternative'.

alpha <- 0.05
masa_H0 <- 1.0

wynik_ttest <- t.test(masa_planet, mu = masa_H0, alternative = "greater", conf.level = 1 - alpha)
wynik_ttest

# p-value < 0.05 -> odrzucamy H0
# Wniosek: srednia masa planet jest istotnie wieksza niz 1.0


# 3. Napisanie funkccji t_test_manual, która obliczy:
#    - średnią z próby, odchylenie standardowe,
#    - statystykę testową t,
#    - zbiór krytyczny dla testu (zależny od wariantu hipotezy alternatywnej: dwustronny, prawo lub lewostronny),
#  W wyniku funkcja zwraca rezultat testu.

t_test_manual <- function(x, mu0, alpha = 0.05){
  
  n <- length(x)
  sr <- mean(x)
  s <- sd(x)
  
  stat <- sqrt(n) * (sr - mu0) / s
  
  t_kryt <- qt(1 - alpha, df = n - 1)
  
  decyzja <- ifelse(stat > t_kryt,
                    "Odrzucamy H0",
                    "Brak podstaw do odrzucenia H0")
  
  return(list(
    n = n,
    srednia = sr,
    odchylenie = s,
    statystyka_t = stat,
    t_kryt = t_kryt,
    decyzja = decyzja
  ))
}


wynik_manual <- t_test_manual(masa_planet, 1.0)

wynik_manual

# Wynik jest zgodny z testem z podpunktu 2.

#4. Przeprowadzenie wizualizacji rozkładu t-Studenta z zaznaczonym obszarem krytycznym 
#    oraz wartością statystyki t.

n2 <- length(masa_planet)

T <- wynik_manual$statystyka_t
t_kryt <- wynik_manual$t_kryt

stopnie_sw <- n2 - 1

x_vals <- seq(-5, 5, length.out = 1000)
df_curve <- data.frame(x = x_vals, y = dt(x_vals, df = stopnie_sw))

ggplot(df_curve, aes(x = x, y = y)) +
  
  geom_line(color = "steelblue", linewidth = 1.2) +
  
  geom_area(
    data = subset(df_curve, x >= t_kryt),
    aes(x = x, y = y),
    fill = "lightgreen",
    alpha = 0.4
  ) +
  
  geom_vline(xintercept = T,
             color = "orange",
             linetype = "dashed",
             linewidth = 1) +
  
  geom_vline(xintercept = t_kryt,
             color = "lightgreen",
             linetype = "dashed",
             linewidth = 1) +
  
  annotate("text",
           x = T + 0.15,
           y = 0.35,
           label = paste0("T = ", round(T, 3)),
           color = "orange",
           hjust = 0) +
  
  annotate("text",
           x = t_kryt + 0.15,
           y = 0.25,
           label = paste0("t_kryt = ", round(t_kryt, 3)),
           color = "lightgreen",
           hjust = 0) +
  
  labs(title = "Rozkład t-Studenta z obszarem krytycznym",
       x = "Wartośc statystyki t",
       y = "Gestość") +
  
  theme_minimal()

# Wnioski
# Mieliśmy małą próbę, dlatego zastosowaliśmy test t-Studenta.
# Statystyka testowa T przekroczyla wartosc krytyczna t_kryt,
# co oznacza ze wynik testu wpada w obszar krytyczny
# i odrzucamy H0 na korzyść H1.
# Średnia masa planet jest istotnie większa od masy Ziemi.




