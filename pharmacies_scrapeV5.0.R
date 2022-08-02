# Install and load packages ----
    if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
               skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
               rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes,
               webshot, plyr, rvest, strex)


link = "https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003C&form=0x800000000000002B&command=0x80000000000007C4&donotreload=ShowSRL1"

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
Sys.sleep(9)
pages = remDr$findElement(using = "css selector", value = ".items")$getElementText() %>% unlist()
num = pages %>% str_locate("of") %>% as_tibble() %>% select(end) %>% as.integer()
len = pages %>% str_length()
num = str_sub(pages, num+2, len-6) %>% str_remove(",") %>% as.numeric()
pages = num / 10
pages = round_any(pages, 1, f = ceiling)
results_on_last_page = num - (pages-1)*10
pharm_data = tibble()

if (file.exists("dataPharmacy3.0.rds") == TRUE) {
    pharm_data = readRDS("dataPharmacy3.0.rds") 
} else {
    pharm_data = tibble()
}


while (nrow(pharm_data) < num) {
    tryCatch({ 
        
        # Main Script ----
        if (file.exists("dataPharmacy3.0.rds") == TRUE) {
            pharm_data = readRDS("dataPharmacy3.0.rds") 
            second_pass = FALSE
            page = round(pharm_data %>% nrow() + 5, -1)/10
            #page = if (page != 1) page - 1 else page
            remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003C&form=0x800000000000002B&command=0x80000000000007C4&donotreload=ShowSRL{page}"))
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
                Sys.sleep(4)#7
                
                result = remDr$findElement(using = "css selector", value = str_glue(".f15:nth-child({i}) a"))
                result$clickElement()
                
                
                Sys.sleep(4)#4.2
                
                # General Information Tab
                gen <- remDr$findElement(using = "css selector", value = "#General .verticalDiv")
                gen$clickElement()
                gen_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
                
                Sys.sleep(1)
                if (str_detect(gen_txt, "Remote dispensing location") == FALSE) {
                    # Pharmacists Registration Tab
                    pharm_staff <- remDr$findElement(using = "css selector", value = "#PharmacyStaff .verticalDiv")
                    pharm_staff$clickElement()
                    pharm_staff_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
                    
                    
                    staff_primary_keys <- tibble()
                    pharmacy_url <- remDr$getCurrentUrl() %>% unlist()
                    pharmacy_staffs_primary_keys_url <- ""
                    current_status = remDr$findElement(using = "css selector", value = "#FriendlyStatus")$getElementText() %>% unlist()
                    
                    if (current_status == "Entitled to operate") {
                        
                        alllinks <- remDr$findElements(using = "css selector", value = "#PharmacyStaffSRLContent a")
                        
                        if ((length(alllinks)) != 0) {
                            
                            for (z in  1:length(alllinks)) {
                                
                                staff <- remDr$findElement(using = "css selector", value = str_glue(".mb-10:nth-child({z}) .mb-10 a"))
                                staff$clickElement()
                                Sys.sleep(1)
                                primary_key_staff_url <- remDr$getCurrentUrl() %>% unlist()
                                staff_primary_keys <- rbind(staff_primary_keys, tibble(primary_key_staff_url))
                                remDr$navigate(pharmacy_url)
                                wait_for_page <- remDr$findElement(using = "css selector", value = "#FindPlacelink a")$getElementText() %>% unlist()
                                Sys.sleep(1.5)
                                pharm_staff <- remDr$findElement(using = "css selector", value = "#PharmacyStaff .verticalDiv")
                                pharm_staff$clickElement()
                                Sys.sleep(1)
                                
                                pharmacy_staffs_primary_keys_url <- str_c(pharmacy_staffs_primary_keys_url, ",", primary_key_staff_url)
                                
                                print(str_glue("Completed {z} / {length(alllinks)} employees."))
                                
                            }
                        }
                    }
                    
                    #Academic History
                    assess_his <- remDr$findElement(using = "css selector", value = "#Assessment .verticalDiv")
                    assess_his$clickElement()
                    assess_his_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
                    
                    #Concerns
                    concerns <- remDr$findElement(using = "css selector", value = "#Concerns .verticalDiv")
                    concerns$clickElement()
                    concerns_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
                    
                    
                    #for (j in 1:5){
                    #    remDr$goBack()
                    #    Sys.sleep(0.1)
                    #}
                    
                    remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003C&form=0x800000000000002B&command=0x80000000000007C4&donotreload=ShowSRL{g_page}"))
                    Sys.sleep(1)#3
                } else {
                    # Pharmacists Registration Tab
                    # pharm_staff_txt <- "No Data"
                    # 
                    # #Academic History
                    # assess_his <- remDr$findElement(using = "css selector", value = "#Assessment .verticalDiv")
                    # assess_his$clickElement()
                    # assess_his_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
                    # 
                    # #Concerns
                    # concerns_txt <- "No Data"
                    # 
                    # remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003C&form=0x800000000000002B&command=0x80000000000007C4&donotreload=ShowSRL{g_page}"))
                    # Sys.sleep(1)#3
                    gen_txt <- NA
                    pharm_staff_txt <- NA
                    assess_his_txt <- NA
                    concerns_txt <- NA
                    pharmacy_staffs_primary_keys_url <- NA
                    remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003C&form=0x800000000000002B&command=0x80000000000007C4&donotreload=ShowSRL{g_page}"))
                }
                gen_txt <- gen_txt %>% unlist()
                pharm_staff_txt <- pharm_staff_txt %>% unlist()
                assess_his_txt <- assess_his_txt %>% unlist()
                concerns_txt <- concerns_txt %>% unlist()
                
                pharm_data <<- rbind(pharm_data, tibble(gen_txt, pharm_staff_txt, assess_his_txt, concerns_txt, pharmacy_staffs_primary_keys_url, pharmacy_url))
                
                print(str_glue("Completed {nrow(pharm_data)} times."))
            }
            
        }
        
        Sys.sleep(9)#9
        
        # Scrapping Script
        for (q in page:pages) {
            remDr$refresh()
            Sys.sleep(5)#5
            g_page <<- q
            if (q == 1 | second_pass == FALSE) {
            } else if (q %in% seq(11, 11000,10)) {
                
                #result = remDr$findElement(using = "css selector", value = ".next a")
                #result$clickElement()
                remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003C&form=0x800000000000002B&command=0x80000000000007C4&donotreload=ShowSRL{g_page}"))
            } else {
                #result = remDr$findElement(using = "css selector", value = ".active+ li a")
                #result$clickElement()
                remDr$navigate(str_glue("https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003C&form=0x800000000000002B&command=0x80000000000007C4&donotreload=ShowSRL{g_page}"))
                
            }
            
            Sys.sleep(9)#9,12
            
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
        
    }, error = function(e) {
        
        pharm_data = pharm_data %>%
            mutate(gen_txt = str_replace_all(gen_txt, "\\n", "|")) %>% 
            mutate(pharm_staff_txt = str_replace_all(pharm_staff_txt, "\\n", "|")) %>% 
            mutate(assess_his_txt = str_replace_all(assess_his_txt, "\\n", "|")) %>% 
            mutate(concerns_txt = str_replace_all(concerns_txt, "\\n", "|"))
        
        write_rds(pharm_data, "dataPharmacy3.0.rds")
        Sys.sleep(1)#10
    })
}

pharm_data = pharm_data %>%
    mutate(gen_txt = str_replace_all(gen_txt, "\\n", "|")) %>% 
    mutate(pharm_staff_txt = str_replace_all(pharm_staff_txt, "\\n", "|")) %>% 
    mutate(assess_his_txt = str_replace_all(assess_his_txt, "\\n", "|")) %>% 
    mutate(concerns_txt = str_replace_all(concerns_txt, "\\n", "|"))

write_rds(pharm_data, "dataPharmacy3.0.rds")

remDr$close()
rm(rs_driver_object)
rm(remDr)

print("Done!")