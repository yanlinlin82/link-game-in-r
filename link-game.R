library(tidyverse)
library(ggmap)
library(grid)
library(imager)

sequencers <- c("1-miseq", "2-nextseq", "3-hiseqx", "4-novaseq", "5-nanopore", "6-roche-454", "7-ion-proton", "8-bgi-seq")
img_list <- lapply(sequencers, function(name) {
  file <- paste0("img/", name, ".png")
  if (file.exists(file)) {
    img <- load.image(file)
  } else {
    url <- paste0("https://github.com/yanlinlin82/link-game-in-r/raw/master/", file)
    img <- load.image(url)
  }
  rasterGrob(img, interpolate = TRUE)
})

init <- function(n, w, h = w) {
  stopifnot((w * h) %% n == 0)
  stopifnot(((w * h) / n) %% 2 == 0)
  tibble(x = rep(1:w, times = h),
         y = rep(1:h, each = w),
         m = sample(rep(1:n, each = (w * h) / n)) %>% factor(1:n))
}

draw <- function(a, cur, n, p = NULL) {
  cols <- rainbow(n)
  g <- a %>%
    ggplot() +
    geom_tile(aes(x, y, fill = m), color = "#333333", size = .5, alpha = .6) +
    geom_text(aes(x, y, label = m)) +
    guides(fill = FALSE) +
    scale_x_continuous(limits = c(0, max(a$x) + 1)) +
    scale_y_continuous(limits = c(0, max(a$y) + 1)) +
    scale_fill_manual(values = cols[sort(unique(a$m))]) +
    theme_void()
  for (i in 1:nrow(a)) {
    if (!is.na(a$m[[i]])) {
      g <- g + annotation_custom(img_list[[as.integer(a$m[[i]])]],
                                 xmin = a$x[[i]] - .4, xmax = a$x[[i]] + .4,
                                 ymin = a$y[[i]] - .4, ymax = a$y[[i]] + .4)
    }
  }
  if (nrow(cur) > 0) {
    g <- g + geom_rect(data = cur,
                       aes(xmin = x - .5, xmax = x + .5,
                           ymin = y - .5, ymax = y + .5),
                       color = "red", fill = NA, size = 3)
  }
  if (!is.null(p)) {
    g <- g + geom_segment(aes(x = xstart, y = ystart, xend = xend, yend = yend),
                          data = p,
                          size = 2, color = "red")
  }
  g %>% print
}

get_pos <- function(a, cur) {
  pos <- gglocator() %>%
    mutate(x = round(x) %>% pmax(1) %>% pmin(max(a$x)),
           y = round(y) %>% pmax(1) %>% pmin(max(a$y)))
  if (!is.na(a$m[a$x == pos$x & a$y == pos$y])) {
    if ((nrow(cur) == 0) ||
        (a$m[a$x == cur$x[[1]] & a$y == cur$y[[1]]] == a$m[a$x == pos$x & a$y == pos$y])) {
      cur <- rbind(cur, pos)
    } else {
      cur <- cur[-(1:nrow(cur)),]
    }
  }
  return(cur)
}

win <- function() {
  g <- ggplot() +
    geom_text(data = tibble(x = 0, y = 0, text = "You Win!"),
              aes(x = x, y = y, label = text), size = 30, color = "blue") +
    theme_void()
  g %>% print
}

check_path <- function(a, cur, b) {
  b %>%
    distinct(x, y) %>%
    left_join(cur %>% mutate(selected = TRUE), by = c("x", "y")) %>%
    filter(is.na(selected)) %>%
    inner_join(a, by = c("x", "y")) %>%
    with(sum(!is.na(m)) == 0)
}

find_path <- function(a, cur) {
  stopifnot(nrow(cur) == 2)
  if (cur$y[[1]] != cur$y[[2]]) {
    for (x in c(cur$x[[1]]:cur$x[[2]],
                (min(cur$x) - 1):0,
                (max(cur$x) + 1):(max(a$x) + 1))) {
      b <- tibble(x = 0, y = 0)[-1,]
      s <- tibble(xstart = 0, xend = 0, ystart = 0, yend = 0)[-1,]
      if (cur$x[[1]] != x) {
        b <- rbind(b, tibble(x = cur$x[[1]]:x, y = cur$y[[1]]))
        s <- rbind(s, tibble(xstart = cur$x[[1]], xend = x,
                             ystart = cur$y[[1]], yend = cur$y[[1]]))
      }
      b <- rbind(b, tibble(x = x, y = cur$y[[1]]:cur$y[[2]]))
      s <- rbind(s, tibble(xstart = x, xend = x,
                           ystart = cur$y[[1]], yend = cur$y[[2]]))
      if (cur$x[[2]] != x) {
        b <- rbind(b, tibble(x = x:cur$x[[2]], y = cur$y[[2]]))
        s <- rbind(s, tibble(xstart = x, xend = cur$x[[2]],
                             ystart = cur$y[[2]], yend = cur$y[[2]]))
      }
      if (check_path(a, cur, b)) {
        return(s)
      }
    }
  }
  if (cur$x[[1]] != cur$x[[2]]) {
    for (y in c(cur$y[[1]]:cur$y[[2]],
                (min(cur$y) - 1):0,
                (max(cur$y) + 1):(max(a$y) + 1))) {
      b <- tibble(x = 0, y = 0)[-1,]
      s <- tibble(xstart = 0, xend = 0, ystart = 0, yend = 0)[-1,]
      if (cur$y[[1]] != y) {
        b <- rbind(b, tibble(x = cur$x[[1]], y = cur$y[[1]]:y))
        s <- rbind(s, tibble(xstart = cur$x[[1]], xend = cur$x[[1]],
                             ystart = cur$y[[1]], yend = y))
      }
      b <- rbind(b, tibble(x = cur$x[[1]]:cur$x[[2]], y = y))
      s <- rbind(s, tibble(xstart = cur$x[[1]], xend = cur$x[[2]],
                           ystart = y, yend = y))
      if (cur$y[[2]] != y) {
        b <- rbind(b, tibble(x = cur$x[[2]], y = y:cur$y[[2]]))
        s <- rbind(s, tibble(xstart = cur$x[[2]], xend = cur$x[[2]],
                             ystart = y, yend = cur$y[[2]]))
      }
      if (check_path(a, cur, b)) {
        return(s)
      }
    }
  }
  return(NULL)
}

play <- function(n, w, h = w) {
  a <- init(n, w, h)
  cur <- tibble(x = 0, y = 0)[-1,]

  while (sum(!is.na(a$m)) > 0) {
    draw(a, cur, n)
    cur <- get_pos(a, cur)
    if (nrow(cur) > 1) {
      if (nrow(distinct(cur)) == 1) {
        cur <- cur[-(1:nrow(cur)),]
      } else {
        p <- find_path(a, cur)
        if (!is.null(p)) {
          draw(a, cur, n, p)
          Sys.sleep(.5)
          for (i in 1:nrow(cur)) {
            a$m[a$x == cur$x[[i]] & a$y == cur$y[[i]]] <- NA
          }
        }
        cur <- cur[-(1:nrow(cur)),]
      }
    }
  }
  win()
}

cat("try: play(4, 4)\n")
