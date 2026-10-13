# ============================================================
# Математический и финансовый помощник
# shinydashboard · нейтрально-деловой стиль
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(moments)

# ---------- UI ----------
ui <- dashboardPage(
  skin = "blue",   # нейтральная синяя тема
  
  # ==================== ШАПКА ====================
  dashboardHeader(
    title = tags$span(
      tags$b("МатФин"),
      tags$span(class = "header-sub", " · помощник")
    ),
    titleWidth = 260
  ),
  
  # ==================== БОКОВОЕ МЕНЮ ====================
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Калькулятор",        tabName = "calc",  icon = icon("calculator")),
      menuItem("Математические задачи", tabName = "math", icon = icon("square-root-variable")),
      menuItem("Статистика",         tabName = "stat",  icon = icon("chart-bar")),
      menuItem("Корреляция",         tabName = "cor",   icon = icon("project-diagram")),
      menuItem("Финансы",            tabName = "fin",   icon = icon("coins"),
               menuSubItem("Сложный процент",     tabName = "ci"),
               menuSubItem("Аннуитетный платёж",  tabName = "an"))
    ),
    tags$div(class = "sidebar-note",
             "© 2025 · Shiny · R")
  ),
  
  # ==================== ТЕЛО ====================
  dashboardBody(
    # Подключаем кастомный CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      
      # ========== 1. КАЛЬКУЛЯТОР ==========
      tabItem(tabName = "calc",
        h2("Калькулятор"),
        p(class = "page-desc",
          "Введите математическое выражение, используя функции R: sqrt, log, sin, cos, exp, ^ и др."),
        fluidRow(
          box(
            title = "Ввод", status = "primary", solidHeader = TRUE,
            width = 6,
            textInput("expr", "Выражение:", value = "2 + 2 * 5"),
            helpText("Примеры: sqrt(16), log(100), sin(pi/2), 5^3"),
            actionButton("calc_btn", "Вычислить",
                         icon = icon("play"), class = "btn-primary")
          ),
          box(
            title = "Результат", status = "primary", solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("calc_out")
          )
        )
      ),
      
      # ========== 2. МАТЕМАТИЧЕСКИЕ ЗАДАЧИ ==========
      tabItem(tabName = "math",
        h2("Математические задачи"),
        p(class = "page-desc",
          "Выберите задачу, задайте параметры и нажмите «Решить»."),
        fluidRow(
          box(
            title = "Параметры", status = "primary", solidHeader = TRUE,
            width = 5,
            selectInput("math_task", "Задача:",
                        choices = c("Квадратное уравнение ax²+bx+c=0",
                                    "Площадь и периметр круга",
                                    "Площадь треугольника (3 стороны)",
                                    "Факториал",
                                    "Числа Фибоначчи",
                                    "НОД и НОК")),
            conditionalPanel(
              condition = "input.math_task == 'Квадратное уравнение ax²+bx+c=0'",
              numericInput("qa", "a:", 1),
              numericInput("qb", "b:", -3),
              numericInput("qc", "c:", 2)
            ),
            conditionalPanel(
              condition = "input.math_task == 'Площадь и периметр круга'",
              numericInput("radius", "Радиус:", 5, min = 0)
            ),
            conditionalPanel(
              condition = "input.math_task == 'Площадь треугольника (3 стороны)'",
              numericInput("ta", "Сторона a:", 3, min = 0),
              numericInput("tb", "Сторона b:", 4, min = 0),
              numericInput("tc", "Сторона c:", 5, min = 0)
            ),
            conditionalPanel(
              condition = "input.math_task == 'Факториал'",
              numericInput("fact_n", "n:", 5, min = 0, step = 1)
            ),
            conditionalPanel(
              condition = "input.math_task == 'Числа Фибоначчи'",
              numericInput("fib_n", "Сколько чисел:", 10, min = 1, step = 1)
            ),
            conditionalPanel(
              condition = "input.math_task == 'НОД и НОК'",
              numericInput("gcd_a", "a:", 24, min = 1, step = 1),
              numericInput("gcd_b", "b:", 36, min = 1, step = 1)
            ),
            actionButton("math_btn", "Решить",
                         icon = icon("check"), class = "btn-primary")
          ),
          box(
            title = "Решение", status = "primary", solidHeader = TRUE,
            width = 7,
            verbatimTextOutput("math_out")
          )
        )
      ),
      
      # ========== 3. СТАТИСТИКА ==========
      tabItem(tabName = "stat",
        h2("Описательная статистика"),
        p(class = "page-desc",
          "Загрузите CSV с одним числовым столбцом или введите данные вручную."),
        fluidRow(
          box(
            title = "Данные", status = "primary", solidHeader = TRUE,
            width = 4,
            fileInput("stat_file", "CSV (1 числовой столбец)",
                      accept = ".csv"),
            tags$hr(),
            tags$b("Или вручную:"),
            textAreaInput("stat_data", "Числа через запятую:",
                          value = "12, 15, 18, 20, 22, 25, 28, 30, 32, 35, 40, 45",
                          rows = 4),
            numericInput("bins", "Интервалов гистограммы:", 8, min = 2),
            actionButton("stat_btn", "Анализировать",
                         icon = icon("chart-line"), class = "btn-primary")
          ),
          column(width = 8,
            fluidRow(
              valueBoxOutput("vbox_mean",   width = 3),
              valueBoxOutput("vbox_median", width = 3),
              valueBoxOutput("vbox_sd",     width = 3),
              valueBoxOutput("vbox_n",      width = 3)
            ),
            box(
              title = "Сводка", status = "primary", solidHeader = TRUE,
              width = 12,
              verbatimTextOutput("stat_summary")
            )
          )
        ),
        fluidRow(
          box(
            title = "Гистограмма распределения",
            status = "primary", solidHeader = TRUE, width = 7,
            plotOutput("hist_plot", height = "380px")
          ),
          box(
            title = "Ящик с усами (boxplot)",
            status = "primary", solidHeader = TRUE, width = 5,
            plotOutput("box_plot", height = "380px")
          )
        )
      ),
      
      # ========== 4. КОРРЕЛЯЦИЯ ==========
      tabItem(tabName = "cor",
        h2("Корреляционный анализ"),
        p(class = "page-desc",
          "Нужно не менее двух числовых столбцов. Формат CSV или «x,y» построчно."),
        fluidRow(
          box(
            title = "Данные", status = "primary", solidHeader = TRUE,
            width = 4,
            fileInput("cor_file", "CSV (2+ числовых столбца)",
                      accept = ".csv"),
            tags$hr(),
            tags$b("Или вручную:"),
            textAreaInput("cor_data", "x,y — каждая строка:",
                          value = "1,2\n2,4\n3,5\n4,4\n5,7\n6,8\n7,9\n8,11\n9,10\n10,13",
                          rows = 8),
            actionButton("cor_btn", "Анализировать",
                         icon = icon("link"), class = "btn-primary")
          ),
          box(
            title = "Сводка", status = "primary", solidHeader = TRUE,
            width = 8,
            verbatimTextOutput("cor_summary")
          )
        ),
        fluidRow(
          box(
            title = "Диаграмма рассеяния",
            status = "primary", solidHeader = TRUE, width = 7,
            plotOutput("cor_plot", height = "420px")
          ),
          box(
            title = "Матрица корреляций",
            status = "primary", solidHeader = TRUE, width = 5,
            DTOutput("cor_matrix")
          )
        )
      ),
      
      # ========== 5. СЛОЖНЫЙ ПРОЦЕНТ ==========
      tabItem(tabName = "ci",
        h2("Сложный процент"),
        p(class = "page-desc",
          "Формула: P·(1 + r/m)^(m·t)"),
        fluidRow(
          box(
            title = "Параметры", status = "primary", solidHeader = TRUE,
            width = 4,
            numericInput("ci_p", "Начальная сумма (P):", 100000, min = 0),
            numericInput("ci_r", "Годовая ставка (%):", 10, min = 0, step = 0.1),
            numericInput("ci_n", "Срок (лет):", 5, min = 1, step = 1),
            selectInput("ci_m", "Начислений в год:",
                        choices = c("Ежегодно" = 1,
                                    "Раз в полгода" = 2,
                                    "Ежеквартально" = 4,
                                    "Ежемесячно" = 12,
                                    "Ежедневно" = 365)),
            actionButton("ci_btn", "Рассчитать",
                         icon = icon("calculator"), class = "btn-primary")
          ),
          column(width = 8,
            fluidRow(
              valueBoxOutput("vbox_final",  width = 4),
              valueBoxOutput("vbox_profit", width = 4),
              valueBoxOutput("vbox_yield",  width = 4)
            ),
            box(
              title = "Результат", status = "primary", solidHeader = TRUE,
              width = 12,
              verbatimTextOutput("ci_text")
            )
          )
        ),
        fluidRow(
          box(
            title = "Рост капитала по годам",
            status = "primary", solidHeader = TRUE, width = 7,
            plotOutput("ci_plot", height = "380px")
          ),
          box(
            title = "Таблица по годам",
            status = "primary", solidHeader = TRUE, width = 5,
            DTOutput("ci_table")
          )
        )
      ),
      
      # ========== 6. АННУИТЕТ ==========
      tabItem(tabName = "an",
        h2("Аннуитетный платёж по кредиту"),
        p(class = "page-desc",
          "Формула: S·i·(1+i)^n / ((1+i)^n − 1), где i = r/12, n = годы·12"),
        fluidRow(
          box(
            title = "Параметры", status = "primary", solidHeader = TRUE,
            width = 4,
            numericInput("an_sum", "Сумма кредита:", 1000000, min = 0),
            numericInput("an_rate", "Годовая ставка (%):", 12, min = 0, step = 0.1),
            numericInput("an_years", "Срок (лет):", 5, min = 1, step = 1),
            actionButton("an_btn", "Рассчитать",
                         icon = icon("calculator"), class = "btn-primary")
          ),
          column(width = 8,
            fluidRow(
              valueBoxOutput("vbox_pay",     width = 4),
              valueBoxOutput("vbox_total",   width = 4),
              valueBoxOutput("vbox_overpay", width = 4)
            ),
            box(
              title = "Результат", status = "primary", solidHeader = TRUE,
              width = 12,
              verbatimTextOutput("an_text")
            )
          )
        ),
        fluidRow(
          box(
            title = "График погашения кредита",
            status = "primary", solidHeader = TRUE, width = 7,
            plotOutput("an_plot", height = "380px")
          ),
          box(
            title = "Остаток долга по месяцам",
            status = "primary", solidHeader = TRUE, width = 5,
            DTOutput("an_table")
          )
        )
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  # ========== 1. КАЛЬКУЛЯТОР ==========
  calc_result <- eventReactive(input$calc_btn, {
    tryCatch({
      res <- eval(parse(text = input$expr))
      paste0("Результат: ", res)
    }, error = function(e) paste0("Ошибка: ", e$message))
  })
  output$calc_out <- renderPrint({ cat(calc_result()) })
  
  # ========== 2. МАТЕМАТИЧЕСКИЕ ЗАДАЧИ ==========
  math_result <- eventReactive(input$math_btn, {
    task <- input$math_task
    
    if (task == "Квадратное уравнение ax²+bx+c=0") {
      a <- input$qa; b <- input$qb; c <- input$qc
      if (a == 0) return("a не может быть 0 (это не квадратное уравнение)")
      D <- b^2 - 4*a*c
      if (D > 0) {
        x1 <- (-b + sqrt(D)) / (2*a)
        x2 <- (-b - sqrt(D)) / (2*a)
        paste0("Дискриминант D = ", D,
               "\nДва корня:\n  x1 = ", round(x1, 4),
               "\n  x2 = ", round(x2, 4))
      } else if (D == 0) {
        x <- -b / (2*a)
        paste0("Дискриминант D = 0\nОдин корень: x = ", round(x, 4))
      } else {
        paste0("Дискриминант D = ", D,
               "\nДействительных корней нет.")
      }
      
    } else if (task == "Площадь и периметр круга") {
      r <- input$radius
      paste0("Площадь S = πr² = ", round(pi * r^2, 4),
             "\nДлина окружности P = 2πr = ", round(2 * pi * r, 4))
      
    } else if (task == "Площадь треугольника (3 стороны)") {
      a <- input$ta; b <- input$tb; c <- input$tc
      if (a + b <= c || a + c <= b || b + c <= a)
        return("Такого треугольника не существует.")
      p <- (a + b + c) / 2
      S <- sqrt(p * (p - a) * (p - b) * (p - c))
      paste0("Полупериметр p = ", p,
             "\nПлощадь (формула Герона) S = ", round(S, 4))
      
    } else if (task == "Факториал") {
      n <- as.integer(input$fact_n)
      if (n < 0) return("n должно быть ≥ 0")
      paste0(n, "! = ", format(factorial(n), scientific = FALSE))
      
    } else if (task == "Числа Фибоначчи") {
      n <- as.integer(input$fib_n)
      fib <- numeric(n)
      if (n >= 1) fib[1] <- 0
      if (n >= 2) fib[2] <- 1
      if (n > 2) for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
      paste0("Первые ", n, " чисел Фибоначчи:\n",
             paste(fib, collapse = ", "))
      
    } else if (task == "НОД и НОК") {
      a <- as.integer(input$gcd_a); b <- as.integer(input$gcd_b)
      gcd_ab <- function(x, y) { while (y != 0) { t <- y; y <- x %% y; x <- t }; x }
      g <- gcd_ab(a, b); l <- a * b / g
      paste0("НОД(", a, ", ", b, ") = ", g,
             "\nНОК(", a, ", ", b, ") = ", l)
    }
  })
  output$math_out <- renderPrint({ cat(math_result()) })
  
  # ========== 3. СТАТИСТИКА ==========
  stat_data <- eventReactive(input$stat_btn, {
    if (!is.null(input$stat_file)) {
      df <- read.csv(input$stat_file$datapath, header = TRUE)
      num_col <- which(sapply(df, is.numeric))[1]
      if (is.na(num_col)) return(NULL)
      x <- df[[num_col]]
    } else {
      txt <- gsub(";", ",", input$stat_data)
      x <- as.numeric(trimws(unlist(strsplit(txt, ","))))
    }
    x <- x[!is.na(x)]
    if (length(x) < 2) return(NULL)
    x
  })
  
  output$vbox_n      <- renderValueBox({
    x <- stat_data()
    valueBox(if (is.null(x)) "—" else length(x), "Наблюдений",
             icon = icon("list"), color = "blue")
  })
  output$vbox_mean   <- renderValueBox({
    x <- stat_data()
    valueBox(if (is.null(x)) "—" else round(mean(x), 2), "Среднее",
             icon = icon("chart-line"), color = "light-blue")
  })
  output$vbox_median <- renderValueBox({
    x <- stat_data()
    valueBox(if (is.null(x)) "—" else round(median(x), 2), "Медиана",
             icon = icon("balance-scale"), color = "aqua")
  })
  output$vbox_sd     <- renderValueBox({
    x <- stat_data()
    valueBox(if (is.null(x)) "—" else round(sd(x), 2), "Ст. отклонение",
             icon = icon("arrows-alt-v"), color = "teal")
  })
  
  output$stat_summary <- renderPrint({
    x <- stat_data()
    if (is.null(x)) { cat("Введите корректные числовые данные (минимум 2 значения)."); return() }
    cat("Количество наблюдений:", length(x), "\n")
    cat("Среднее:      ", round(mean(x), 4), "\n")
    cat("Медиана:      ", round(median(x), 4), "\n")
    cat("Ст. откл.:    ", round(sd(x), 4), "\n")
    cat("Дисперсия:    ", round(var(x), 4), "\n")
    cat("Минимум:      ", min(x), "\n")
    cat("Максимум:     ", max(x), "\n")
    cat("Квартили:     ", paste(round(quantile(x), 3), collapse = ", "), "\n")
    cat("Асимметрия:   ", round(skewness(x), 4), "\n")
    cat("Эксцесс:      ", round(kurtosis(x), 4), "\n")
  })
  
  output$hist_plot <- renderPlot({
    x <- stat_data(); req(x)
    ggplot(data.frame(x = x), aes(x = x)) +
      geom_histogram(bins = input$bins, fill = "#3b6ea5",
                     color = "white", alpha = 0.9) +
      geom_vline(aes(xintercept = mean(x)), color = "#c0392b",
                 linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = median(x)), color = "#27ae60",
                 linetype = "dotted", size = 1) +
      labs(title = "Гистограмма",
           subtitle = "Красная — среднее, зелёная — медиана",
           x = "Значение", y = "Частота") +
      theme_minimal(base_size = 13)
  })
  
  output$box_plot <- renderPlot({
    x <- stat_data(); req(x)
    ggplot(data.frame(x = x), aes(y = x, x = "")) +
      geom_boxplot(fill = "#9bb3cc", alpha = 0.85) +
      labs(title = "Boxplot", x = "", y = "Значение") +
      theme_minimal(base_size = 13)
  })
  
  # ========== 4. КОРРЕЛЯЦИЯ ==========
  cor_data <- eventReactive(input$cor_btn, {
    if (!is.null(input$cor_file)) {
      df <- read.csv(input$cor_file$datapath)
    } else {
      lines <- strsplit(input$cor_data, "\n")[[1]]
      lines <- lines[lines != ""]
      parsed <- lapply(lines, function(l) {
        l <- gsub(";", ",", l)
        as.numeric(trimws(unlist(strsplit(l, ","))))
      })
      df <- as.data.frame(do.call(rbind, parsed))
      colnames(df) <- paste0("V", seq_len(ncol(df)))
    }
    df <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(df) < 2) return(NULL)
    df
  })
  
  output$cor_summary <- renderPrint({
    df <- cor_data()
    if (is.null(df)) { cat("Нужны минимум 2 числовых столбца."); return() }
    cat("Столбцов:", ncol(df), " | Наблюдений:", nrow(df), "\n\n")
    print(round(cor(df, use = "complete.obs"), 4))
    cat("\nМатрица p-value:\n")
    p_mat <- matrix(NA, ncol(df), ncol(df))
    for (i in 1:ncol(df)) for (j in 1:ncol(df)) {
      if (i != j) p_mat[i,j] <- cor.test(df[[i]], df[[j]])$p.value
    }
    dimnames(p_mat) <- list(colnames(df), colnames(df))
    print(round(p_mat, 4))
  })
  
  output$cor_plot <- renderPlot({
    df <- cor_data(); req(df, ncol(df) >= 2)
    ggplot(df, aes(x = .data[[names(df)[1]]], y = .data[[names(df)[2]]])) +
      geom_point(size = 3, color = "#3b6ea5", alpha = 0.85) +
      geom_smooth(method = "lm", se = TRUE, color = "#c0392b") +
      labs(title = paste0(names(df)[2], " в зависимости от ", names(df)[1]),
           subtitle = paste0("r = ", round(cor(df[[1]], df[[2]]), 4)),
           x = names(df)[1], y = names(df)[2]) +
      theme_minimal(base_size = 13)
  })
  
  output$cor_matrix <- renderDT({
    df <- cor_data(); req(df)
    datatable(round(cor(df, use = "complete.obs"), 4),
              options = list(pageLength = 10, dom = "t"),
              caption = "Матрица корреляций")
  })
  
  # ========== 5. СЛОЖНЫЙ ПРОЦЕНТ ==========
  ci_result <- eventReactive(input$ci_btn, {
    P <- input$ci_p
    r <- input$ci_r / 100
    n <- input$ci_n
    m <- as.numeric(input$ci_m)
    t_seq <- 0:n
    amounts <- P * (1 + r/m)^(m * t_seq)
    df <- data.frame(Год = t_seq, Сумма = round(amounts, 2))
    list(df = df, P = P, final = tail(amounts, 1), profit = tail(amounts, 1) - P)
  })
  
  output$vbox_final <- renderValueBox({
    r <- ci_result()
    valueBox(
      if (is.null(r)) "—" else format(round(r$final, 0), big.mark = " "),
      "Итоговая сумма, руб.",
      icon = icon("wallet"), color = "blue")
  })
  output$vbox_profit <- renderValueBox({
    r <- ci_result()
    valueBox(
      if (is.null(r)) "—" else format(round(r$profit, 0), big.mark = " "),
      "Прибыль, руб.",
      icon = icon("arrow-up"), color = "green")
  })
  output$vbox_yield <- renderValueBox({
    r <- ci_result()
    valueBox(
      if (is.null(r)) "—" else paste0(round(r$profit / r$P * 100, 2), " %"),
      "Доходность",
      icon = icon("percent"), color = "teal")
  })
  
  output$ci_text <- renderPrint({
    r <- ci_result(); req(r)
    cat("Начальная сумма:  ", format(r$P, big.mark = " "), "руб.\n")
    cat("Итоговая сумма:   ", format(round(r$final, 2), big.mark = " "), "руб.\n")
    cat("Прибыль:          ", format(round(r$profit, 2), big.mark = " "), "руб.\n")
    cat("Доходность:       ", round(r$profit / r$P * 100, 2), "%\n")
  })
  
  output$ci_plot <- renderPlot({
    r <- ci_result(); req(r)
    ggplot(r$df, aes(x = Год, y = Сумма)) +
      geom_line(color = "#3b6ea5", size = 1.2) +
      geom_point(color = "#3b6ea5", size = 3) +
      geom_text(aes(label = format(round(Сумма, 0), big.mark = " ")),
                vjust = -0.9, size = 3.3, color = "#2c3e50") +
      labs(title = "Рост капитала по сложному проценту",
           x = "Год", y = "Сумма (руб.)") +
      theme_minimal(base_size = 13)
  })
  
  output$ci_table <- renderDT({
    r <- ci_result(); req(r)
    datatable(r$df, options = list(pageLength = 8),
              caption = "Динамика по годам")
  })
  
  # ========== 6. АННУИТЕТ ==========
  an_result <- eventReactive(input$an_btn, {
    S <- input$an_sum
    r_year <- input$an_rate / 100
    years <- input$an_years
    n <- years * 12
    i <- r_year / 12
    payment <- if (i == 0) S / n else S * i * (1 + i)^n / ((1 + i)^n - 1)
    total <- payment * n
    overpay <- total - S
    balance <- numeric(n + 1); balance[1] <- S
    for (k in 1:n) {
      interest <- balance[k] * i
      balance[k + 1] <- balance[k] - (payment - interest)
    }
    df <- data.frame(Месяц = 0:n, Остаток = round(balance, 2))
    list(payment = payment, total = total, overpay = overpay, n = n, df = df)
  })
  
  output$vbox_pay <- renderValueBox({
    r <- an_result()
    valueBox(
      if (is.null(r)) "—" else format(round(r$payment, 0), big.mark = " "),
      "Ежемесячный платёж, руб.",
      icon = icon("credit-card"), color = "blue")
  })
  output$vbox_total <- renderValueBox({
    r <- an_result()
    valueBox(
      if (is.null(r)) "—" else format(round(r$total, 0), big.mark = " "),
      "Всего выплат, руб.",
      icon = icon("money-bill-wave"), color = "light-blue")
  })
  output$vbox_overpay <- renderValueBox({
    r <- an_result()
    valueBox(
      if (is.null(r)) "—" else format(round(r$overpay, 0), big.mark = " "),
      "Переплата, руб.",
      icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$an_text <- renderPrint({
    r <- an_result(); req(r)
    cat("Сумма кредита:      ", format(r$payment * r$n - r$overpay, big.mark = " "), "руб.\n")
    cat("Срок:               ", r$n, "месяцев\n")
    cat("Ежемесячный платёж: ", format(round(r$payment, 2), big.mark = " "), "руб.\n")
    cat("Всего выплат:       ", format(round(r$total, 2), big.mark = " "), "руб.\n")
    cat("Переплата:          ", format(round(r$overpay, 2), big.mark = " "), "руб.\n")
    cat("Переплата (%):      ", round(r$overpay / (r$total - r$overpay) * 100, 2), "%\n")
  })
  
  output$an_plot <- renderPlot({
    r <- an_result(); req(r)
    ggplot(r$df, aes(x = Месяц, y = Остаток)) +
      geom_area(fill = "#9bb3cc", alpha = 0.6) +
      geom_line(color = "#3b6ea5", size = 1) +
      labs(title = "График погашения кредита",
           x = "Месяц", y = "Остаток долга (руб.)") +
      theme_minimal(base_size = 13)
  })
  
  output$an_table <- renderDT({
    r <- an_result(); req(r)
    datatable(head(r$df, 25), options = list(pageLength = 8),
              caption = "Остаток долга по месяцам (первые 25)")
  })
}

shinyApp(ui, server)
