# Analiza statystyczna danych w R

Projekt wykonany w języku R obejmujący analizę statystyczną danych, testowanie hipotez oraz wizualizację wyników.

Celem projektu było praktyczne zastosowanie metod statystyki matematycznej i wnioskowania statystycznego na przykładowych zbiorach danych związanych z astrofizyką.

## Zakres projektu

### 1. Analiza poziomu promieniowania kosmicznego

Dla danych dotyczących pomiarów promieniowania:

- przeprowadzono testy normalności:
  - test Shapiro–Wilka,
  - test Lillieforsa,
- wyznaczono:
  - 90% przedział ufności dla średniej,
  - przedział ufności dla wariancji,
  - przedział ufności dla odchylenia standardowego,
- wykonano wizualizację danych:
  - histogram,
  - dopasowanie rozkładu normalnego,
  - zaznaczenie średniej i przedziału ufności.

### 2. Weryfikacja hipotezy statystycznej dla mas egzoplanet

Dla danych dotyczących mas nowo odkrytych planet:

- sformułowano hipotezy statystyczne,
- przeprowadzono jednostronny test t-Studenta,
- zaimplementowano własną funkcję `t_test_manual()` obliczającą:
  - średnią z próby,
  - odchylenie standardowe,
  - statystykę testową,
  - wartość krytyczną,
  - decyzję testową,
- zwizualizowano rozkład t-Studenta wraz z obszarem krytycznym.

## Technologie

- **R**
- **ggplot2**
- **nortest**

## Czego nauczyłam się podczas projektu

- praktycznego wykorzystania testów statystycznych,
- budowy przedziałów ufności,
- implementacji testów statystycznych od podstaw,
- interpretacji wyników statystycznych,
- wizualizacji danych w R.

## Uruchomienie

1. Pobierz repozytorium:
```bash
git clone [link]
```

2. Otwórz plik `statistical_data_analysis.R` w RStudio.

3. Zainstaluj wymagane biblioteki:

```r
install.packages("ggplot2")
install.packages("nortest")
```

4. Uruchom skrypt.
