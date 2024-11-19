Tidy Tuesday!
================
Kauanoe Greene
2024-11-19

# Libraries

``` r
# install.packages("ISOcodes")
# install.packages("ggiraph")
# install.packages("webshot")
# install.packages("webshot2")
library(here)
library(ggiraph)
library(ISOcodes)
library(tidyverse)
library(janitor) # clean data  
```

# Data Upload

``` r
# read in dataset 1 from github
countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/countries.csv')
glimpse(countries) # check it out  
```

    ## Rows: 249
    ## Columns: 6
    ## $ alpha_2       <chr> "AW", "AF", "AO", "AI", "AX", "AL", "AD", "AE", "AR", "A…
    ## $ alpha_3       <chr> "ABW", "AFG", "AGO", "AIA", "ALA", "ALB", "AND", "ARE", …
    ## $ numeric       <dbl> 533, 4, 24, 660, 248, 8, 20, 784, 32, 51, 16, 10, 260, 2…
    ## $ name          <chr> "Aruba", "Afghanistan", "Angola", "Anguilla", "Åland Isl…
    ## $ official_name <chr> NA, "Islamic Republic of Afghanistan", "Republic of Ango…
    ## $ common_name   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

``` r
# read in dataset 2 from github  
country_subdivisions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/country_subdivisions.csv')
glimpse(country_subdivisions) # check it out  
```

    ## Rows: 5,046
    ## Columns: 5
    ## $ code    <chr> "AD-02", "AD-03", "AD-04", "AD-05", "AD-06", "AD-07", "AD-08",…
    ## $ name    <chr> "Canillo", "Encamp", "La Massana", "Ordino", "Sant Julià de Lò…
    ## $ type    <chr> "Parish", "Parish", "Parish", "Parish", "Parish", "Parish", "P…
    ## $ parent  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ alpha_2 <chr> "AD", "AD", "AD", "AD", "AD", "AD", "AD", "AE", "AE", "AE", "A…

``` r
# read in dataset 3 from github  
former_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/former_countries.csv')
glimpse(former_countries) # check it out  
```

    ## Rows: 31
    ## Columns: 6
    ## $ alpha_4        <chr> "AIDJ", "ANHH", "BQAQ", "BUMM", "BYAA", "CSHH", "CSXX",…
    ## $ alpha_3        <chr> "AFI", "ANT", "ATB", "BUR", "BYS", "CSK", "SCG", "CTE",…
    ## $ numeric        <dbl> 262, 530, NA, 104, 112, 200, 891, 128, 278, 204, NA, 24…
    ## $ name           <chr> "French Afars and Issas", "Netherlands Antilles", "Brit…
    ## $ date_withdrawn <chr> "1977", "2010-12-15", "1979", "1989-12-05", "1992-06-15…
    ## $ comment        <chr> NA, "had numeric code 532 until Aruba split away in 198…

# Data clean

``` r
# clean dataset 1 code from github to get us started  

clean_countries <- 
  ISOcodes::ISO_3166_1 |> 
  tibble::as_tibble() |> 
  dplyr::mutate(Numeric = as.integer(Numeric)) |> 
  janitor::clean_names()
glimpse(clean_countries)
```

    ## Rows: 249
    ## Columns: 6
    ## $ alpha_2       <chr> "AW", "AF", "AO", "AI", "AX", "AL", "AD", "AE", "AR", "A…
    ## $ alpha_3       <chr> "ABW", "AFG", "AGO", "AIA", "ALA", "ALB", "AND", "ARE", …
    ## $ numeric       <int> 533, 4, 24, 660, 248, 8, 20, 784, 32, 51, 16, 10, 260, 2…
    ## $ name          <chr> "Aruba", "Afghanistan", "Angola", "Anguilla", "Åland Isl…
    ## $ official_name <chr> NA, "Islamic Republic of Afghanistan", "Republic of Ango…
    ## $ common_name   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

``` r
# clean dataset 2 code from github to get us started  

clean_country_subdivisions <- 
  ISOcodes::ISO_3166_2 |> 
  tibble::as_tibble() |> 
  janitor::clean_names() |> 
  dplyr::mutate(
    alpha_2 = stringr::str_extract(code, "^[^-]+(?=-)")
  )
glimpse(clean_country_subdivisions)
```

    ## Rows: 5,046
    ## Columns: 5
    ## $ code    <chr> "AD-02", "AD-03", "AD-04", "AD-05", "AD-06", "AD-07", "AD-08",…
    ## $ name    <chr> "Canillo", "Encamp", "La Massana", "Ordino", "Sant Julià de Lò…
    ## $ type    <chr> "Parish", "Parish", "Parish", "Parish", "Parish", "Parish", "P…
    ## $ parent  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ alpha_2 <chr> "AD", "AD", "AD", "AD", "AD", "AD", "AD", "AE", "AE", "AE", "A…

``` r
# clean dataset 3 code from github to get us started  

clean_former_countries <-
  ISOcodes::ISO_3166_3 |> 
  tibble::as_tibble() |> 
  janitor::clean_names() %>% 
  filter(date_withdrawn <= "1979") # filter by year
glimpse(clean_former_countries)
```

    ## Rows: 7
    ## Columns: 6
    ## $ alpha_4        <chr> "AIDJ", "BQAQ", "DYBJ", "FQHH", "GEHH", "SKIN", "VDVN"
    ## $ alpha_3        <chr> "AFI", "ATB", "DHY", "ATF", "GEL", "SKM", "VDR"
    ## $ numeric        <chr> "262", NA, "204", NA, "296", NA, NA
    ## $ name           <chr> "French Afars and Issas", "British Antarctic Territory"…
    ## $ date_withdrawn <chr> "1977", "1979", "1977", "1979", "1979", "1975", "1977"
    ## $ comment        <chr> NA, NA, NA, "now split between AQ and TF", "now split i…

# Plot

``` r
plot <- 
  ggplot(
  data = clean_former_countries,
  mapping = 
    aes(
    x = alpha_3, 
    y = date_withdrawn,
    # here we add interactive aesthetics
    tooltip = name, data_id = name
  )
) +
  geom_point_interactive(
    size = 3, hover_nearest = TRUE
  ) + 
  labs(title = "Countries withdrawn in the 1970's", 
       x = "Former Countries", 
       y = "Year Withdrawn")

tidyplot3 <- girafe(ggobj = plot)
htmltools::save_html(tidyplot3, "../Output/tidyplot3.html")

tidyplot3
```

<div class="girafe html-widget html-fill-item" id="htmlwidget-eb15ed371b2ea8b0b9d2" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-eb15ed371b2ea8b0b9d2">{"x":{"html":"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='ggiraph-svg' role='graphics-document' id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8' viewBox='0 0 504 360'>\n <defs id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_defs'>\n  <clipPath id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_c1'>\n   <rect x='0' y='0' width='504' height='360'/>\n  <\/clipPath>\n  <clipPath id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_c2'>\n   <rect x='42.93' y='23.18' width='455.59' height='305.33'/>\n  <\/clipPath>\n <\/defs>\n <g id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_rootg' class='ggiraph-svg-rootg'>\n  <g clip-path='url(#svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_c1)'>\n   <rect x='0' y='0' width='504' height='360' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='0.75' stroke-linejoin='round' stroke-linecap='round' class='ggiraph-svg-bg'/>\n   <rect x='0' y='0' width='504' height='360' fill='#FFFFFF' fill-opacity='1' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='round'/>\n  <\/g>\n  <g clip-path='url(#svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_c2)'>\n   <rect x='42.93' y='23.18' width='455.59' height='305.33' fill='#EBEBEB' fill-opacity='1' stroke='none'/>\n   <polyline points='42.93,271.26 498.52,271.26' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='42.93,175.84 498.52,175.84' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='42.93,80.43 498.52,80.43' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='80.90,328.50 80.90,23.18' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='144.17,328.50 144.17,23.18' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='207.45,328.50 207.45,23.18' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='270.73,328.50 270.73,23.18' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='334.00,328.50 334.00,23.18' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='397.28,328.50 397.28,23.18' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='460.55,328.50 460.55,23.18' fill='none' stroke='#FFFFFF' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <circle id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_e1' cx='80.9' cy='175.84' r='2.67pt' fill='#000000' fill-opacity='1' stroke='#000000' stroke-opacity='1' stroke-width='0.71' stroke-linejoin='round' stroke-linecap='round' title='French Afars and Issas' data-id='French Afars and Issas' nearest='true'/>\n   <circle id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_e2' cx='144.17' cy='80.43' r='2.67pt' fill='#000000' fill-opacity='1' stroke='#000000' stroke-opacity='1' stroke-width='0.71' stroke-linejoin='round' stroke-linecap='round' title='British Antarctic Territory' data-id='British Antarctic Territory' nearest='true'/>\n   <circle id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_e3' cx='270.73' cy='175.84' r='2.67pt' fill='#000000' fill-opacity='1' stroke='#000000' stroke-opacity='1' stroke-width='0.71' stroke-linejoin='round' stroke-linecap='round' title='Dahomey' data-id='Dahomey' nearest='true'/>\n   <circle id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_e4' cx='207.45' cy='80.43' r='2.67pt' fill='#000000' fill-opacity='1' stroke='#000000' stroke-opacity='1' stroke-width='0.71' stroke-linejoin='round' stroke-linecap='round' title='French Southern and Antarctic Territories' data-id='French Southern and Antarctic Territories' nearest='true'/>\n   <circle id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_e5' cx='334' cy='80.43' r='2.67pt' fill='#000000' fill-opacity='1' stroke='#000000' stroke-opacity='1' stroke-width='0.71' stroke-linejoin='round' stroke-linecap='round' title='Gilbert and Ellice Islands' data-id='Gilbert and Ellice Islands' nearest='true'/>\n   <circle id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_e6' cx='397.28' cy='271.26' r='2.67pt' fill='#000000' fill-opacity='1' stroke='#000000' stroke-opacity='1' stroke-width='0.71' stroke-linejoin='round' stroke-linecap='round' title='Sikkim' data-id='Sikkim' nearest='true'/>\n   <circle id='svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_e7' cx='460.55' cy='175.84' r='2.67pt' fill='#000000' fill-opacity='1' stroke='#000000' stroke-opacity='1' stroke-width='0.71' stroke-linejoin='round' stroke-linecap='round' title='Viet-Nam, Democratic Republic of' data-id='Viet-Nam, Democratic Republic of' nearest='true'/>\n  <\/g>\n  <g clip-path='url(#svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8_c1)'>\n   <text x='18.41' y='274.41' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>1975<\/text>\n   <text x='18.41' y='178.99' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>1977<\/text>\n   <text x='18.41' y='83.58' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>1979<\/text>\n   <polyline points='40.19,271.26 42.93,271.26' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='40.19,175.84 42.93,175.84' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='40.19,80.43 42.93,80.43' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='80.90,331.24 80.90,328.50' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='144.17,331.24 144.17,328.50' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='207.45,331.24 207.45,328.50' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='270.73,331.24 270.73,328.50' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='334.00,331.24 334.00,328.50' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='397.28,331.24 397.28,328.50' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <polyline points='460.55,331.24 460.55,328.50' fill='none' stroke='#333333' stroke-opacity='1' stroke-width='1.07' stroke-linejoin='round' stroke-linecap='butt'/>\n   <text x='74.05' y='339.74' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>AFI<\/text>\n   <text x='135.95' y='339.74' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>ATB<\/text>\n   <text x='199.47' y='339.74' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>ATF<\/text>\n   <text x='261.44' y='339.74' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>DHY<\/text>\n   <text x='325.2' y='339.74' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>GEL<\/text>\n   <text x='387.75' y='339.74' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>SKM<\/text>\n   <text x='451.27' y='339.74' font-size='6.6pt' font-family='Arial' fill='#4D4D4D' fill-opacity='1'>VDR<\/text>\n   <text x='227.92' y='352.2' font-size='8.25pt' font-family='Arial'>Former Countries<\/text>\n   <text transform='translate(13.36,214.47) rotate(-90.00)' font-size='8.25pt' font-family='Arial'>Year Withdrawn<\/text>\n   <text x='42.93' y='14.93' font-size='9.9pt' font-family='Arial'>Countries withdrawn in the 1970's<\/text>\n  <\/g>\n <\/g>\n<\/svg>","js":null,"uid":"svg_d2a3157b_08bd_474d_ad8b_1225a1ccc6a8","ratio":1.4,"settings":{"tooltip":{"css":".tooltip_SVGID_ { padding:5px;background:black;color:white;border-radius:2px;text-align:left; ; position:absolute;pointer-events:none;z-index:999;}","placement":"doc","opacity":0.9,"offx":10,"offy":10,"use_cursor_pos":true,"use_fill":false,"use_stroke":false,"delay_over":200,"delay_out":500},"hover":{"css":".hover_data_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_data_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_data_SVGID_ { fill:orange;stroke:black; }\nline.hover_data_SVGID_, polyline.hover_data_SVGID_ { fill:none;stroke:orange; }\nrect.hover_data_SVGID_, polygon.hover_data_SVGID_, path.hover_data_SVGID_ { fill:orange;stroke:none; }\nimage.hover_data_SVGID_ { stroke:orange; }","reactive":true,"nearest_distance":null},"hover_inv":{"css":""},"hover_key":{"css":".hover_key_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_key_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_key_SVGID_ { fill:orange;stroke:black; }\nline.hover_key_SVGID_, polyline.hover_key_SVGID_ { fill:none;stroke:orange; }\nrect.hover_key_SVGID_, polygon.hover_key_SVGID_, path.hover_key_SVGID_ { fill:orange;stroke:none; }\nimage.hover_key_SVGID_ { stroke:orange; }","reactive":true},"hover_theme":{"css":".hover_theme_SVGID_ { fill:orange;stroke:black;cursor:pointer; }\ntext.hover_theme_SVGID_ { stroke:none;fill:orange; }\ncircle.hover_theme_SVGID_ { fill:orange;stroke:black; }\nline.hover_theme_SVGID_, polyline.hover_theme_SVGID_ { fill:none;stroke:orange; }\nrect.hover_theme_SVGID_, polygon.hover_theme_SVGID_, path.hover_theme_SVGID_ { fill:orange;stroke:none; }\nimage.hover_theme_SVGID_ { stroke:orange; }","reactive":true},"select":{"css":".select_data_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_data_SVGID_ { stroke:none;fill:red; }\ncircle.select_data_SVGID_ { fill:red;stroke:black; }\nline.select_data_SVGID_, polyline.select_data_SVGID_ { fill:none;stroke:red; }\nrect.select_data_SVGID_, polygon.select_data_SVGID_, path.select_data_SVGID_ { fill:red;stroke:none; }\nimage.select_data_SVGID_ { stroke:red; }","type":"multiple","only_shiny":true,"selected":[]},"select_inv":{"css":""},"select_key":{"css":".select_key_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_key_SVGID_ { stroke:none;fill:red; }\ncircle.select_key_SVGID_ { fill:red;stroke:black; }\nline.select_key_SVGID_, polyline.select_key_SVGID_ { fill:none;stroke:red; }\nrect.select_key_SVGID_, polygon.select_key_SVGID_, path.select_key_SVGID_ { fill:red;stroke:none; }\nimage.select_key_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"select_theme":{"css":".select_theme_SVGID_ { fill:red;stroke:black;cursor:pointer; }\ntext.select_theme_SVGID_ { stroke:none;fill:red; }\ncircle.select_theme_SVGID_ { fill:red;stroke:black; }\nline.select_theme_SVGID_, polyline.select_theme_SVGID_ { fill:none;stroke:red; }\nrect.select_theme_SVGID_, polygon.select_theme_SVGID_, path.select_theme_SVGID_ { fill:red;stroke:none; }\nimage.select_theme_SVGID_ { stroke:red; }","type":"single","only_shiny":true,"selected":[]},"zoom":{"min":1,"max":1,"duration":300},"toolbar":{"position":"topright","pngname":"diagram","tooltips":null,"fixed":false,"hidden":[],"delay_over":200,"delay_out":500},"sizing":{"rescale":true,"width":1}}},"evals":[],"jsHooks":[]}</script>

``` r
# save plot to my output folder
ggsave(here("Tidy_Tuesday", "Output", "tidyplot3.png")) 

# save csv file to data folder
write.csv(clean_former_countries, here("Tidy_Tuesday", "Data", "tidydata3.csv"))
```
