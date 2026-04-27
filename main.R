# โหลด Libraries ที่จำเป็นทั้งหมด
library(httr2)
library(jsonlite)
library(tidyverse)
library(readr)
library(bigrquery) # 🌟 เปลี่ยนมาใช้ Library สำหรับ BigQuery

# 1. ตั้งค่าและดึงข้อมูลจาก API
api_url <- "https://opendata.moph.go.th/api/report_data"

# แนะนำให้เทสด้วย 2567 หรือ 2568 ก่อน
target_year <- "2569" 

# รายชื่อรหัสจังหวัด 11-96 (ครอบคลุมทั้งประเทศ)
province_list <- sprintf("%02d", c(11:19, 20:27, 30:39, 40:49, 50:58, 60:67, 70:77, 80:86, 90:96))

# ฟังก์ชันดึงข้อมูล API พร้อมเพิ่มการนับคอลัมน์และแถว
get_herbal_data <- function(prov_code) {
  message(paste("กำลังดึงข้อมูลจังหวัด:", prov_code))
  req_body <- list(tableName = "s_ttm4", year = target_year, province = prov_code, type = "json")
  
  tryCatch({
    resp <- request(api_url) %>% 
      req_body_json(req_body) %>% req_perform()
    data <- resp %>% 
      resp_body_json(simplifyVector = TRUE)
    
    df <- as_tibble(data)
    
    # 🌟 เพิ่มส่วนแสดงผลเมื่อดึงข้อมูลสำเร็จ
    message(sprintf("  ✅ สำเร็จ! ได้ข้อมูล %d แถว และ %d คอลัมน์", nrow(df), ncol(df)))
    
    return(df)
  }, error = function(e) {
    message("  ❌ ไม่สามารถดึงข้อมูลได้หรือเกิดข้อผิดพลาด")
    return(NULL)
  })
}

# เริ่มดึงข้อมูลทุกจังหวัดมารวมกัน
all_provinces_data <- province_list %>% map_df(~get_herbal_data(.x))

# เช็กว่ามีข้อมูลหรือไม่
if(nrow(all_provinces_data) == 0) {
  stop("ไม่มีข้อมูลจาก API ในปีที่เลือก โปรแกรมจึงหยุดทำงาน")
}

# 🌟 เพิ่มสรุปข้อมูลที่ดึงมารวมกันทั้งหมด
message(sprintf("📊 สรุปข้อมูลดิบที่ดึงมาทั้งหมด: %d แถว %d คอลัมน์", nrow(all_provinces_data), ncol(all_provinces_data)))

# 2. โหลดตาราง Master จากไฟล์ CSV
master_drug <- read_csv("code_drug1_158.csv", 
                        col_types = cols(
                          code24 = col_character(), 
                          code11 = col_character()
                        ))

master_unit <- read_csv("2.unit_service.csv",
                        col_types = cols(
                          code_hospital = col_character()
                        ))

# เตรียมตารางยาให้สะอาด
master_drug_unique <- master_drug %>% 
  mutate(code24_fixed = str_trim(as.character(code24))) %>%
  distinct(code24_fixed, .keep_all = TRUE) 

# 3. เริ่มกระบวนการ Clean, Join และ คำนวณ
final_data <- all_provinces_data %>%
  select(-any_of(c("id", "date_com", "areacode"))) %>%
  mutate(
    didstd_clean = str_trim(as.character(didstd)),
    code11_api = substr(didstd_clean, 1, 11),
    hospcode_clean = as.numeric(hospcode) %>% as.integer() %>% as.character() %>% 
      str_pad(width = 5, side = "left", pad = "0")
  ) %>%
  left_join(master_drug_unique, by = c("didstd_clean" = "code24_fixed")) %>%
  
  # กรองเฉพาะรายการที่ codedrug เป็น -1 หรือ 1-
  filter(codedrug %in% c("-1", "1-")) %>%
  
  left_join(
    master_unit %>% 
      mutate(code_hospital_fixed = as.numeric(code_hospital) %>% as.integer() %>% as.character() %>% 
               str_pad(width = 5, side = "left", pad = "0")),
    by = c("hospcode_clean" = "code_hospital_fixed")
  ) %>%
  mutate(
    vs_uc = as.numeric(vs_uc),
    price = as.numeric(price),
    total_value = vs_uc * price
  ) %>%
  
  # 4. จัดรูปแบบผลลัพธ์
  select(
    hospital,            
    S_Care,              
    too,                 
    Affiliation,         
    health_region,       
    province,            
    district,            
    district5,           
    code24 = didstd_clean, 
    Formula,             
    codedrug,            
    code11 = code11_api,   
    cleandrug_name,      
    vs_uc,               
    price,               
    total_value    # 🌟 แนะนำให้ใช้ชื่อภาษาอังกฤษแทน "มูลค่า" เพื่อป้องกันปัญหาใน BigQuery
  )

# 🌟 เพิ่มสรุปมูลค่ารวม (Total Value) และโครงสร้างข้อมูลสุดท้ายก่อนเข้า BigQuery
sum_total_value <- sum(final_data$total_value, na.rm = TRUE)

message("📈 สรุปผลลัพธ์ข้อมูลที่ผ่านกระบวนการ Clean & Join แล้ว")
message(sprintf("📌 จำนวนข้อมูลที่เตรียมอัปโหลด: %d แถว %d คอลัมน์", nrow(final_data), ncol(final_data)))
message(sprintf("💰 มูลค่ารวมทั้งหมด (Total Value): %s บาท", format(sum_total_value, big.mark = ",", nsmall = 2, scientific = FALSE)))

# 5. ส่งข้อมูลเข้า Google BigQuery
bq_auth(email = "your-email@gmail.com") 

project_id <- "api-opendata"
dataset_id <- "moph_data"
table_name <- "opendata2569"

bq_table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_name)

# --- ส่วนที่แก้ไข: กำหนด Schema เพื่อป้องกัน Scientific Notation ---
# เราจะสร้าง schema แบบเจาะจงเฉพาะคอลัมน์ที่มีปัญหา
# คอลัมน์อื่นๆ bigrquery จะจัดการให้ตามความเหมาะสมของ data type ใน R

message("กำลังอัปโหลดข้อมูลไปยัง BigQuery พร้อมกำหนด Schema...")

bq_table_upload(
  x = bq_table_ref, 
  values = final_data, 
  create_disposition = "CREATE_IF_NEEDED", 
  write_disposition = "WRITE_TRUNCATE",
  # บังคับให้คอลัมน์รหัสเป็น STRING
  fields = as_bq_fields(final_data) 
)

message("✅ ทำงานเสร็จสมบูรณ์!")