# ใช้ R version ที่เสถียร
FROM rocker/r-ver:4.3.0

# ติดตั้ง System Libraries สำหรับ httr2 และ bigrquery
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# ติดตั้ง R Packages ที่คุณใช้ใน Code
RUN R -e "install.packages(c('httr2', 'jsonlite', 'tidyverse', 'bigrquery', 'gargle'))"

# ก๊อปปี้ไฟล์ทุกอย่าง (R Script และไฟล์ CSV ทั้งหมด) เข้าไปในกล่อง
COPY main.R /main.R
COPY code_drug1_158.csv /code_drug1_158.csv
COPY 2.unit_service.csv /2.unit_service.csv

# สั่งให้เริ่มรัน Script
CMD ["Rscript", "/main.R"]