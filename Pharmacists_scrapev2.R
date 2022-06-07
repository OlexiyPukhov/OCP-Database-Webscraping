# Install and load packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
               skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
               rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes, plyr,
               webshot)

# What link to scrape from, initializing the scrapper ----
link = "https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003D&form=0x800000000000002C&command=0x80000000000007C5&donotreload=ShowSRL1"

rs_driver_object = rsDriver(browser = c("chrome"),
                            chromever = "101.0.4951.41",
                            verbose = FALSE,
                            port = free_port(),
                            extraCapabilities = list("chromeOptions" = list(args = list('--headless'))))

remDr = rs_driver_object$client
remDr$setTimeout("page load", 40000)
remDr$setTimeout("implicit", 40000)
remDr$setTimeout("script", 40000)

remDr$navigate(link)
Sys.sleep(3)
pages = remDr$findElement(using = "css selector", value = ".items")$getElementText() %>% unlist()
num = pages %>% str_locate("of") %>% as_tibble() %>% select(end) %>% as.integer()
len = pages %>% str_length()
num = str_sub(pages, num+2, len-6) %>% str_remove(",") %>% as.numeric()
pages = num / 10
pages = round_any(pages, 1, f = ceiling)
results_on_last_page = num - (pages-1)*10
pharm_data = tibble()

if (file.exists("data.rds") == TRUE) {
    pharm_data = readRDS("data.rds") 
    second_pass = FALSE
    page = round(pharm_data %>% nrow() + 5, -1)/10
    #page = if (page != 1) page - 1 else page
    remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003D&form=0x800000000000002C&command=0x80000000000007C5&donotreload=ShowSRL{page}"))
    result_num = nrow(pharm_data) - page*10 + 10
    first_run = FALSE
    
} else {
    
    pharm_data = tibble()
    second_pass = TRUE
    first_run = TRUE
    page = 1
}

scrap_pages <- function(start_result_num = 1, end_result_num = 10) {
    for (i in start_result_num:end_result_num) {
        Sys.sleep(4)
        
        result = remDr$findElement(using = "css selector", value = str_glue(".f15:nth-child({i}) a"))
        result$clickElement()
        
        Sys.sleep(3.2)
        
        # General Information Tab
        gen <- remDr$findElement(using = "css selector", value = "#General .verticalDiv")
        gen$clickElement()
        gen_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
        
        # Pharmacists Registration Tab
        pharmacists_reg <- remDr$findElement(using = "css selector", value = "#RegistrationHistory .verticalDiv")
        pharmacists_reg$clickElement()
        reg_history_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
        
        #Academic History
        aca_his <- remDr$findElement(using = "css selector", value = "#Academic .verticalDiv")
        aca_his$clickElement()
        aca_his_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
        
        #Concerns
        concerns <- remDr$findElement(using = "css selector", value = "#Concerns .verticalDiv")
        concerns$clickElement()
        concerns_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
        
        for (j in 1:5){
            remDr$goBack()
        }
        gen_txt <<- gen_txt %>% unlist()
        reg_history_txt <<- reg_history_txt %>% unlist()
        aca_his_txt <<- concerns_txt %>% unlist()
        concerns_txt <<- concerns_txt %>% unlist()
        pharm_data <<- rbind(pharm_data, tibble(gen_txt, reg_history_txt, aca_his_txt, concerns_txt))
        print(str_glue("Completed {nrow(pharm_data)} times."))
    }
}

Sys.sleep(3)

# Scrapping Script
for (q in page:pages) {
    remDr$refresh()
    Sys.sleep(3)
    
    if (q == 1 | second_pass == FALSE) {
    } else if (q %in% seq(11, 11000,10)) {
        
        result = remDr$findElement(using = "css selector", value = ".next a")
        result$clickElement()      
    } else {
        result = remDr$findElement(using = "css selector", value = ".active+ li a")
        result$clickElement()
        
    }
    
    Sys.sleep(1)
    
    # This can look confusing but it's easier than it seems.
    # There are two cases: getting the info at the end, and getting the info
    # from the rest of the pages.
    if (q == pages) {
        
        # IF AT END CASE ---
        scrap_pages(end_result_num = results_on_last_page)
        
    } else {
        # FOR THE REST OF THE DATABASE CASE ----
        
        if (second_pass == TRUE) {
            scrap_pages() 
        }
        
        else {
            if (result_num != 10) scrap_pages(start_result_num = result_num + 1) else TRUE
            second_pass = TRUE
            
        }
        
        
    }
}

pharm_data = pharm_data %>%
    mutate(gen_txt = str_replace_all(gen_txt, "\\n", "|")) %>% 
    mutate(reg_history_txt = str_replace_all(reg_history_txt, "\\n", "|")) %>% 
    mutate(aca_his_txt = str_replace_all(aca_his_txt, "\\n", "|")) %>% 
    mutate(concerns_txt = str_replace_all(concerns_txt, "\\n", "|"))

write_rds(pharm_data, "data.rds")
write_csv(pharm_data, "data_pharmacists.csv")

remDr$close()
rm(rs_driver_object)
rm(remDr)

print("Completed!")