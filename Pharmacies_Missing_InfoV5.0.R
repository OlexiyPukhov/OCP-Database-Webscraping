# Install and load packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
               skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
               rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes,
               webshot, strex)

web_scrape = import("dataPharmacy3.0.rds")
pharm_data = import("dataPharmacy3.0.rds")
ocp_pharmacies = import("OCPSearch6k_pharmacies.csv", fill = TRUE)

missing_data_tbl = tibble()

web_scrape = web_scrape %>% distinct()
pharm_data = pharm_data %>% distinct()


web_scrape = web_scrape %>% 
    filter(!gen_txt %in% c(pharm_staff_txt, assess_his_txt, concerns_txt)) %>% 
    filter(!pharm_staff_txt %in% c(gen_txt, assess_his_txt, concerns_txt)) %>% 
    filter(!assess_his_txt %in% c(gen_txt, pharm_staff_txt, concerns_txt)) %>% 
    filter(!concerns_txt %in% c(gen_txt, pharm_staff_txt, assess_his_txt))

web_scrape = pharm_data

web_scrape = web_scrape %>% 
    mutate(V1 = str_after_nth(gen_txt, "Accreditation number:", n = 1)) %>% 
    mutate(V2 = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
    mutate(V1 = str_before_nth(V1, "\\|", n = 1)) %>% 
    select(V1, V2, everything()) %>% 
    mutate(V1 = V1 %>% as.numeric())

ocp_pharmacies = ocp_pharmacies %>% filter(!row_number() == 1) %>% 
    mutate(V1 = V1 %>% as.numeric())

ocp_pharmacies = ocp_pharmacies %>% 
    select(V1,V2,V7)

missing_data = ocp_pharmacies %>% 
    anti_join(web_scrape) %>% 
    mutate(V1 = V1 %>% as.character())

missing_data = missing_data %>% na.omit()

link = "https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003C&form=0x800000000000002B&command=0x80000000000007C4"

rs_driver_object = rsDriver(browser = c("chrome"),
                            chromever = "103.0.5060.24",
                            verbose = FALSE,
                            port = free_port(),
                            extraCapabilities = list("chromeOptions" = list(args = list('--headless'))))

remDr = rs_driver_object$client
remDr$setTimeout("page load", 90000)
remDr$setTimeout("implicit", 90000)
remDr$setTimeout("script", 90000)


#main script ----
first_run = TRUE


while (nrow(missing_data_tbl) + 1 < nrow(missing_data)) {
    tryCatch({ 
        
        for (i in (nrow(missing_data_tbl) + 1):nrow(missing_data)) { #main----
            remDr$navigate(link)
            remDr$refresh()
            # if (first_run == TRUE) {
            #     remDr$navigate(link)
            #     Sys.sleep(1)
            #     more_button <- remDr$findElement(using = "css selector", value = "#AdvancedSearch span")
            #     more_button$clickElement()
            #     first_run = FALSE
            # } else {
            #     remDr$navigate(link)}
            
            more_button <- remDr$findElement(using = "css selector", value = "#AdvancedSearch span")
            more_button$clickElement()

            Sys.sleep(1)
            number <- remDr$findElement(using = "css selector", value = "#regNum .no-sidepadding")
            number$sendKeysToElement(list(missing_data$V1[i]))
            search <- remDr$findElement(using = "css selector", value = "#LaunchSearch")
            search$clickElement()
            Sys.sleep(1)
            result = remDr$findElement(using = "css selector", value = str_glue(".f15:nth-child(1) a"))
            result$clickElement()
            
            gen <- remDr$findElement(using = "css selector", value = "#General .verticalDiv")
            gen$clickElement()
            gen_txt <- remDr$findElement(using = "css selector", value = "#TabContent")$getElementText()
            Sys.sleep(0.1)
            
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
                            Sys.sleep(1)
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
                
                Sys.sleep(1)#3
                
            } else {
                gen_txt <- NA
                pharm_staff_txt <- NA
                assess_his_txt <- NA
                concerns_txt <- NA
            }
            gen_txt <<- gen_txt %>% unlist()
            pharm_staff_txt <<- pharm_staff_txt %>% unlist()
            assess_his_txt <<- assess_his_txt %>% unlist()
            concerns_txt <<- concerns_txt %>% unlist()
            
            missing_data_tbl <<- rbind(missing_data_tbl, tibble(gen_txt, pharm_staff_txt, assess_his_txt, concerns_txt, pharmacy_staffs_primary_keys_url, pharmacy_url))
            print(str_glue("Completed {i} / {nrow(missing_data)} times."))
            
            missing_data_tbl = missing_data_tbl %>%
                mutate(gen_txt = str_replace_all(gen_txt, "\\n", "|")) %>% 
                mutate(pharm_staff_txt = str_replace_all(pharm_staff_txt, "\\n", "|")) %>% 
                mutate(assess_his_txt = str_replace_all(assess_his_txt, "\\n", "|")) %>% 
                mutate(concerns_txt = str_replace_all(concerns_txt, "\\n", "|"))
            
        }
    }, error = function(e) {
        first_run = TRUE
    })
}

    pharm_data = pharm_data %>%
        rbind(missing_data_tbl)

    pharm_data = pharm_data %>% distinct()

write_rds(pharm_data, "Pharmacies_2022_V5.0_Verified.rds")


remDr$close()
rm(rs_driver_object)
rm(remDr)

print("Done!")
