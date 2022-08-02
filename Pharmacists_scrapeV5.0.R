# Install and load packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
               skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
               rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes, plyr,
               webshot)

# What link to scrape from, initializing the scrapper ----
link = "https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003D&form=0x800000000000002C&command=0x80000000000007C5&donotreload=ShowSRL1"

rs_driver_object = rsDriver(browser = c("chrome"),
                            chromever = "103.0.5060.53",
                            verbose = FALSE,
                            port = free_port(),
                            extraCapabilities = list("chromeOptions" = list(args = list('--headless'))))

remDr = rs_driver_object$client
remDr$setTimeout("page load", 100000)
remDr$setTimeout("implicit", 100000)
remDr$setTimeout("script", 100000)

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

scrap_pages <- function(start_result_num = 1, end_result_num = 10) {
    for (i in start_result_num:end_result_num) {
        Sys.sleep(1)
        
        result = remDr$findElement(using = "css selector", value = str_glue(".f15:nth-child({i}) a"))
        result$clickElement()
        
        Sys.sleep(0.8)
        # General Information Tab
        gen <- remDr$findElement(using = "css selector", value = "#General .verticalDiv")
        gen$clickElement()
        gen_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
        
        pharmacist_url <- remDr$getCurrentUrl() %>% unlist()
        pharmacy_locations_primary_keys_url <- ""
        no_worksites <-  remDr$findElement(using = "css selector", value = "#PracticeLocationContent")$getElementText() %>% unlist()
        
        if (no_worksites == "This pharmacy professional has not reported a workplace.") {
            pharmacy_locations_primary_keys_url <- ",This pharmacy professional has not reported a workplace."
        } else if (no_worksites == "This person is not entitled to practice and does not have a workplace.") {
            pharmacy_locations_primary_keys_url <- ",This person is not entitled to practice and does not have a workplace."
        } else {
            alllinks <- remDr$findElements(using = "css selector", value = "#PracticeLocationContent a")
            Sys.sleep(0.25)
            if ((length(alllinks)) != 0) {
                
                for (z in  1:length(alllinks)) {
                    if (z == 1) {
                        workplace <- remDr$findElement(using = "css selector", value = str_glue("#PracticeLocationContent a"))
                        workplace$clickElement()
                        wait_for_page <- remDr$findElement(using = "css selector", value = "#FindPlacelink a")$getElementText() %>% unlist()
                    } else if (nrow(pharm_data) == 4960) {
                        
                    } else if (z == 2) {
                        workplace <- remDr$findElement(using = "css selector", value = str_glue(".ResultDiv+ .ResultDiv a"))
                        workplace$clickElement()
                        wait_for_page <- remDr$findElement(using = "css selector", value = "#FindPlacelink a")$getElementText() %>% unlist()
                    } else {
                        workplace <- remDr$findElement(using = "css selector", value = str_glue(".ResultDiv:nth-child({z}) a"))
                        workplace$clickElement()
                        wait_for_page <- remDr$findElement(using = "css selector", value = "#FindPlacelink a")$getElementText() %>% unlist()
                    }
                    workplace_url <- remDr$getCurrentUrl() %>% unlist()
                    
                    remDr$navigate(pharmacist_url)
                    wait_for_page <- remDr$findElement(using = "css selector", value = "#FindAPeronLink a")$getElementText() %>% unlist()
                    Sys.sleep(0.5)
                    
                    pharmacy_locations_primary_keys_url <- str_c(pharmacy_locations_primary_keys_url, ",", workplace_url)
                    
                    print(str_glue("Completed {z} / {length(alllinks)} worksites."))
                    
                }
            }
        }
        
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
        
        #for (j in 1:5){
        #    remDr$goBack()
        #}
        
        remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003D&form=0x800000000000002C&command=0x80000000000007C5&donotreload=ShowSRL{g_page}"))
        
        gen_txt <<- gen_txt %>% unlist()
        reg_history_txt <<- reg_history_txt %>% unlist()
        aca_his_txt <<- concerns_txt %>% unlist()
        concerns_txt <<- concerns_txt %>% unlist()
        pharmacist_url <- pharmacist_url %>% unlist()
        pharm_data <<- rbind(pharm_data, tibble(gen_txt, reg_history_txt, aca_his_txt, concerns_txt, pharmacist_url, pharmacy_locations_primary_keys_url))
        print(str_glue("Completed {nrow(pharm_data)} times."))
    }
}





Sys.sleep(3)

# Scrapping Script ----
while (nrow(pharm_data) < num) {
    tryCatch({ 
        
        if (file.exists("data11.rds") == TRUE) {
            pharm_data = readRDS("data11.rds") 
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
        
        for (q in page:pages) {
            remDr$refresh()
            Sys.sleep(2)
            g_page <<- q
            
            if (q == 1 | second_pass == FALSE) {
            } else if (q %in% seq(11, 11000,10)) {
                
                #result = remDr$findElement(using = "css selector", value = ".next a")
                #result$clickElement()
                
                remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003D&form=0x800000000000002C&command=0x80000000000007C5&donotreload=ShowSRL{g_page}"))
            } else {
                #result = remDr$findElement(using = "css selector", value = ".active+ li a")
                #result$clickElement()
                
                remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003D&form=0x800000000000002C&command=0x80000000000007C5&donotreload=ShowSRL{g_page}"))
                
                
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
        
        
    }, error = function(e) {
        
        pharm_data = pharm_data %>%
            mutate(gen_txt = str_replace_all(gen_txt, "\\n", "|")) %>% 
            mutate(reg_history_txt = str_replace_all(reg_history_txt, "\\n", "|")) %>% 
            mutate(aca_his_txt = str_replace_all(aca_his_txt, "\\n", "|")) %>% 
            mutate(concerns_txt = str_replace_all(concerns_txt, "\\n", "|"))
        
        write_rds(pharm_data, "data11.rds")
        Sys.sleep(1)#10
    })
}

pharm_data = pharm_data %>%
    mutate(gen_txt = str_replace_all(gen_txt, "\\n", "|")) %>% 
    mutate(reg_history_txt = str_replace_all(reg_history_txt, "\\n", "|")) %>% 
    mutate(aca_his_txt = str_replace_all(aca_his_txt, "\\n", "|")) %>% 
    mutate(concerns_txt = str_replace_all(concerns_txt, "\\n", "|"))

write_rds(pharm_data, "data11.rds")

remDr$close()
rm(rs_driver_object)
rm(remDr)

print("Completed!")