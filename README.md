# onemorelight
This is a suite of R functions (soon to be converted into a fully-fledged package) that visualise the number of occurrences of a certain event over a chronological or numerical scale.

This was inspired by the New York Times' "Wall of Grief" graphic, constructed as the United States approached **500,000** deaths from COVID-19. See below:

https://www.nytimes.com/2021/02/21/insider/covid-500k-front-page.html

This randomised density scatterplot, as I came to know it, is most effective when indicating the changing incidence of events that impact individuals. At a large scale, such as when used by the NYT, the density of points along the y-axis reflects the severity of a phenomenon at a population level. When used at a smaller scale, on the other hand, this format can bring more emotional weight to each individual impacted by said events.

For example, here is a graphic showing the number of COVID-19 deaths per day in India from February to May this year. I created this originally with Flourish but found the loading time excruciating (nothing against the software - it's not built to handle humongous datasets the way statistical software might be).

![14861626451159_ pic_hd](https://user-images.githubusercontent.com/69900601/125975853-7ac2ccf7-2608-4d90-9bde-7dd845d18c18.jpg)

Without further ado, here are the functions, which you can copy and paste into your own consoles if you're curious to use them before I release a package.

### Function 1a: onemorelight (for use with chronological data + y-axis labels)
Arguments:
<br> **dataframe**, a data.frame object with one column of dates (in any format) and one column of values (of class numeric).
<br> **datetime**, a string that describes the format of the dates in **dataframe**. Visit https://www.stat.berkeley.edu/~s133/dates.html and scroll down to the POSIX date/time classes section for more information (screenshot below taken from website):
<img width="612" alt="Screenshot 2021-07-16 at 17 11 46" src="https://user-images.githubusercontent.com/69900601/125977420-be5a1d0b-200e-436f-8500-7e0b27ce9a7d.png">
<br> **title**, a string that designates your desired plot title.

``` {r}
onemorelight <- function(dataframe, datetime, title){
  library(ggplot2)
    df <- dataframe
    df[, 1] <- as.POSIXct(strptime(dataframe[, 1], datetime))
    first_df <- data.frame(c(1:sum(df[, 2])), c(1:sum(df[, 2])), c(1:sum(df[, 2])))
    names(first_df) <- c("date", "X", "Y")
    duptimes <- df[, 2]
    index <- rep(1:nrow(df), duptimes)
    incomp_df <- df[index, ]
    new_df <- incomp_df
    new_df[, 2] <- runif(nrow(incomp_df), min = 0, max = 10000)
    new_df[, 3] <- runif(nrow(incomp_df), min = 0, max = 100)
    plot <- ggplot(new_df, aes(x = new_df[, 2], y = new_df[, 3])) + 
      geom_point(shape = 21, size = 0.02, color = "#edd290", fill = "white", stroke = 0.15) +
      facet_grid(new_df[, 1] ~.) +
      labs(title = title) +
      theme_void() +
      theme(plot.title = element_text(face = "bold", color = "white", size = 5, margin = margin(0, 0, 5, 0), hjust = 0.055),
            strip.text = element_text(size = 2.5, color = "light grey", face = "bold", margin = margin(2, 0, 3, 0)),
            plot.margin = margin(20, 20, 20, 20, "pt"),
            plot.background = element_rect(fill = "#002241", color = NA),
            panel.spacing = unit(0.008, "lines"))
    return(plot)
}
```

### Function 1b: onemorelight_bl (for use with chronological data + no y-axis labels)
Arguments:
<br> **dataframe**, a data.frame object with one column of dates (in any format) and one column of values (of class numeric).
<br> **datetime**, a string that describes the format of the dates in **dataframe**. 
<br> **title**, a string that designates your desired plot title.

``` {r}
onemorelight_bl <- function(dataframe, datetime, title){
  library(ggplot2)
  df <- dataframe
  df[, 1] <- as.POSIXct(strptime(dataframe[, 1], datetime))
  first_df <- data.frame(c(1:sum(df[, 2])), c(1:sum(df[, 2])), c(1:sum(df[, 2])))
  names(first_df) <- c("date", "X", "Y")
  duptimes <- df[, 2]
  index <- rep(1:nrow(df), duptimes)
  incomp_df <- df[index, ]
  new_df <- incomp_df
  new_df[, 2] <- runif(nrow(incomp_df), min = 0, max = 10000)
  new_df[, 3] <- runif(nrow(incomp_df), min = 0, max = 100)
  plot <- ggplot(new_df, aes(x = new_df[, 2], y = new_df[, 3])) + 
    geom_point(shape = 21, size = 0.02, color = "#edd290", fill = "white", stroke = 0.15) +
    facet_grid(new_df[, 1] ~.) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", color = "white", size = 5, margin = margin(0, 0, 5, 0), hjust = 0.055),
          strip.text = element_blank(),
          plot.margin = margin(20, 20, 20, 20, "pt"),
          plot.background = element_rect(fill = "#002241", color = NA),
          panel.spacing = unit(0, "lines"))
  return(plot)
}
```

### Function 2a: density_scatter (for use with numerical, non-chronological data + y-axis labels)
Arguments:
<br> **dataframe**, a data.frame object with one column of numbers (of class character or numeric) and one column of values (of class numeric).
<br> **title**, a string that designates your desired plot title.

``` {r}
density_scatter <- function(dataframe, title){
  library(ggplot2)
  df <- dataframe
  first_df <- data.frame(c(1:sum(df[, 2])), c(1:sum(df[, 2])), c(1:sum(df[, 2])))
  names(first_df) <- c("date", "X", "Y")
  duptimes <- df[, 2]
  index <- rep(1:nrow(df), duptimes)
  incomp_df <- df[index, ]
  new_df <- incomp_df
  new_df[, 2] <- runif(nrow(incomp_df), min = 0, max = 10000)
  new_df[, 3] <- runif(nrow(incomp_df), min = 0, max = 100)
  plot <- ggplot(new_df, aes(x = new_df[, 2], y = new_df[, 3])) + 
    geom_point(shape = 21, size = 0.02, color = "#edd290", fill = "white", stroke = 0.15) +
    facet_grid(new_df[, 1] ~.) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", color = "white", size = 5, margin = margin(0, 0, 5, 0), hjust = 0.055),
          strip.text = element_text(size = 2.5, color = "light grey", face = "bold", margin = margin(2, 0, 3, 0)),
          plot.margin = margin(20, 20, 20, 20, "pt"),
          plot.background = element_rect(fill = "#002241", color = NA),
          panel.spacing = unit(0.008, "lines"))
  return(plot)
}
```

### Function 2b: density_scatter_bl (for use with numerical, non-chronological data + no y-axis labels)
Arguments:
<br> **dataframe**, a data.frame object with one column of numbers (of class character or numeric) and one column of values (of class numeric).
<br> **title**, a string that designates your desired plot title.

``` {r}
density_scatter_bl <- function(dataframe, title){
  library(ggplot2)
  df <- dataframe
  first_df <- data.frame(c(1:sum(df[, 2])), c(1:sum(df[, 2])), c(1:sum(df[, 2])))
  names(first_df) <- c("date", "X", "Y")
  duptimes <- df[, 2]
  index <- rep(1:nrow(df), duptimes)
  incomp_df <- df[index, ]
  new_df <- incomp_df
  new_df[, 2] <- runif(nrow(incomp_df), min = 0, max = 10000)
  new_df[, 3] <- runif(nrow(incomp_df), min = 0, max = 100)
  plot <- ggplot(new_df, aes(x = new_df[, 2], y = new_df[, 3])) + 
    geom_point(shape = 21, size = 0.02, color = "#edd290", fill = "white", stroke = 0.15) +
    facet_grid(new_df[, 1] ~.) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", color = "white", size = 5, margin = margin(0, 0, 5, 0), hjust = 0.055),
          strip.text = element_blank(),
          plot.margin = margin(20, 20, 20, 20, "pt"),
          plot.background = element_rect(fill = "#002241", color = NA),
          panel.spacing = unit(0.008, "lines"))
  return(plot)
}
```

This README will be updated with more functions in due course, and I will try to upload an actual package soon. None of this would be possible without the ggplot2 library, which I have fallen in love with, so my thanks to Hadley Wickham for all of his hard work. :)
