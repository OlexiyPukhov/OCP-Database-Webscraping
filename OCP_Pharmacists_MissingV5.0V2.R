# Install and load packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
               skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
               rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes,
               webshot, strex)

web_scrape = import("data11 - pharmacists v5.0.rds")
 pharm_data = import("data11 - pharmacists v5.0.rds")
ocp_pharmacists = import("OCPSearch27KV5.0_Pharmacists.csv")

missing_data_tbl = tibble()

web_scrape = web_scrape %>% distinct()

# Sometimes the scraper glitches out and gen_txt is the same as reg_txt, this should not be the case
# so remove the entries and rescrape
web_scrape = web_scrape %>% 
    filter(!gen_txt %in% c(reg_history_txt, aca_his_txt, concerns_txt)) %>% 
    filter(!reg_history_txt %in% c(gen_txt, aca_his_txt, concerns_txt)) %>% 
    filter(!aca_his_txt %in% c(gen_txt, reg_history_txt, concerns_txt)) %>% 
    filter(!concerns_txt %in% c(gen_txt, reg_history_txt, aca_his_txt))

pharm_data = web_scrape

web_scrape = web_scrape %>% 
    mutate(Member_Number = str_after_nth(gen_txt, "Registration number:", n = 1)) %>% 
    mutate(Name = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
    mutate(Member_Number = str_before_nth(Member_Number, "\\|", n = 1)) %>% 
    select(Member_Number, Name, everything()) %>% 
    mutate(Member_Number = Member_Number %>% as.numeric())

web_scrape = web_scrape %>% distinct(Member_Number, .keep_all = TRUE)

ocp_pharmacists = ocp_pharmacists %>% 
    select(Member_Number,First_Name,Last_Name, Member_Status)

missing_data = ocp_pharmacists %>% 
    anti_join(web_scrape) %>% 
    mutate(Member_Number = Member_Number %>% as.character())


missing_data = missing_data %>% distinct(Member_Number, .keep_all = TRUE)
missing_data = missing_data %>% na.omit()

link = "https://members.ocpinfo.com/tcpr/public/pr/en/#/forms/new/?table=0x800000000000003D&form=0x800000000000002C&command=0x80000000000007C5"

rs_driver_object = rsDriver(browser = c("chrome"),
                            chromever = "103.0.5060.53",
                            verbose = FALSE,
                            port = free_port())
                            #extraCapabilities = list("chromeOptions" = list(args = list('--headless'))))

remDr = rs_driver_object$client
remDr$setTimeout("page load", 400000)
remDr$setTimeout("implicit", 400000)
remDr$setTimeout("script", 400000)

#main script ----
first_run = TRUE 
while (nrow(missing_data_tbl) < nrow(missing_data)) {
    tryCatch({
        for (i in (nrow(missing_data_tbl) + 1):nrow(missing_data)) { #main----
            Sys.sleep(0.5)
            remDr$navigate(link)
            remDr$refresh()
            gen_txt <- ""
            aca_his_txt <- ""
            reg_history_txt <- ""
            concerns_txt <- ""
            #if (first_run == TRUE) {
            #    remDr$navigate(link)
            #    Sys.sleep(3)
            #    more_button <- remDr$findElement(using = "css selector", value = "#AdvancedSearch span")
            #    more_button$clickElement()
            #   first_run = FALSE
            #} else {
            #    remDr$navigate(link)}
            Sys.sleep(3)
            more_button <- remDr$findElement(using = "css selector", value = "#AdvancedSearch span")
            more_button$clickElement()
            Sys.sleep(2)
            number <- remDr$findElement(using = "css selector", value = "#regNum .delInput")
            number$sendKeysToElement(list(missing_data$Member_Number[i]))
            search <- remDr$findElement(using = "css selector", value = "#LaunchSearch")
            search$clickElement()
            Sys.sleep(2)
            result = remDr$findElement(using = "css selector", value = str_glue(".f15:nth-child(1) a"))
            result$clickElement()
            Sys.sleep(0.5)
            
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
            
            gen_txt <- gen_txt %>% unlist()
            reg_history_txt <- reg_history_txt %>% unlist()
            aca_his_txt <- aca_his_txt %>% unlist()
            concerns_txt <- concerns_txt %>% unlist()
            pharmacist_url <- pharmacist_url %>% unlist()
            missing_data_tbl <<- rbind(missing_data_tbl, tibble(gen_txt, reg_history_txt, aca_his_txt, concerns_txt, pharmacist_url, pharmacy_locations_primary_keys_url))
            
            print(str_glue("Completed {i} / {nrow(missing_data)} times."))
            
            missing_data_tbl = missing_data_tbl %>%
                mutate(gen_txt = str_replace_all(gen_txt, "\\n", "|")) %>% 
                mutate(reg_history_txt = str_replace_all(reg_history_txt, "\\n", "|")) %>% 
                mutate(aca_his_txt = str_replace_all(aca_his_txt, "\\n", "|")) %>% 
                mutate(concerns_txt = str_replace_all(concerns_txt, "\\n", "|"))
            
            
            
            
        }
    }, error = function(e){
        
    } )
}



pharm_data = pharm_data %>% distinct()

pharm_data = pharm_data %>% 
    rbind(missing_data_tbl)

write_rds(pharm_data, "Pharmacists_2022_V5.0.rds")


remDr$close()
rm(rs_driver_object)
rm(remDr)

print("Done!")
