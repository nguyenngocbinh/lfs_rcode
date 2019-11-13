# Author: Nguyễn Ngọc Bình
# First: 19/10/2019
# --------------------------
# session info
# R 3.6.1
# rio_0.5.16
# dplyr_0.8.3
# tidyr_1.0.0
# magrittr_1.5

library(rio)
library(dplyr)
library(tidyr)
library(magrittr)
# Import data
lfs_2018 <- import('LFS_2018.dta')

lfs_2018 %<>% rename_all(tolower)

# Support functions
eval(parse("R/labels.R", encoding = "UTF-8"))


ldvl_2018 <- lfs_2018 %>%
  mutate(
    in_vietnam = if_else(c6 == 1, 1, 0),
    gender = case_when(c3 == 1 ~ 'Nam', c3 == 2 ~ 'Nữ', TRUE ~ 'Missing'),
    age = c5,
    agegroup5 = f_age_grp5(age),
    marital_status = case_when(
      c9 == 1 ~ 'Chưa có vợ/chồng',
      c9 == 2 ~ 'Đã có vợ/chồng',
      c9 == 3 ~ 'Góa',
      c9 == 4 |
        c9 == 5 ~ 'Ly hôn/Ly thân',
      TRUE ~ 'Missing'
    ),
    tdvh = case_when(
      c17 >= 5 & c17 <= 9 ~ 'THPT',
      c17 == 4 ~ 'THCS',
      c17 == 3 ~ 'Tiểu học',
      c17 == 2 ~ 'Chưa TN tiểu học',
      c17 == 1 ~ 'Không biết đọc/viết',
      TRUE ~ 'Missing'
    ),
    tdvh0 = case_when(
      c17 == 9 ~ 'Trên ĐH',
      c17 == 8 ~ 'ĐH',
      c17 == 7 ~ 'CĐCN',
      c17 == 6 ~ 'THCN',
      c17 == 5 ~ 'THPT',
      c17 == 4 ~ 'THCS',
      c17 == 3 ~ 'Tiểu học',
      c17 == 2 ~ 'Chưa TN tiểu học',
      c17 == 1 ~ 'Không biết đọc/viết',
      TRUE ~ 'Missing'
    ),
    train0 = case_when(
      tdvh0 == 'Trên ĐH' ~ 'Trên ĐH',
      tdvh0 == 'ĐH' ~ 'ĐH',
      tdvh0 == 'CĐCN' ~ 'CĐCN',
      c19 == 7 ~ 'CĐ nghề',
      tdvh0 == 'THCN' ~ 'THCN',
      # THCN thấp hơn CĐ nghề
      c19 == 6 ~ 'TC nghề',
      c19 == 5 ~ 'Sơ cấp nghề',
      c19 == 4 ~ 'Chứng chỉ nghề dưới 3 tháng',
      c19 == 3 ~ 'Kỹ năng nghề dưới 3 tháng',
      c19 == 2 ~ 'CNKT không bằng',
      c19 == 1 ~ 'Không có trình độ, kỹ năng nghề',
      TRUE ~ 'Missing'
    ),
    train = case_when(
      train0 %in% c('ĐH', 'Trên ĐH') ~ 'ĐH trở lên',
      train0 %in% c(
        'Chứng chỉ nghề dưới 3 tháng',
        'Kỹ năng nghề dưới 3 tháng',
        'CNKT không bằng',
        'Không có trình độ, kỹ năng nghề'
      ) ~ 'Không có trình độ, kỹ năng nghề',
      TRUE ~ train0
    ),
    train2 = case_when(
      train0 %in% c('CĐCN', 'ĐH', 'Trên ĐH') ~ 'CĐ, ĐH trở lên',
      train0 %in% c(
        'Chứng chỉ nghề dưới 3 tháng',
        'Kỹ năng nghề dưới 3 tháng',
        'CNKT không bằng',
        'Không có trình độ, kỹ năng nghề'
      ) ~ 'Không có trình độ CMKT',
      train0 %in% c('TC nghề', 'Sơ cấp nghề', 'CÐ nghề') ~ 'LĐ qua đào tạo nghề',
      TRUE ~ train0
    ),
    ttnt = case_when(ttnt == 1 ~ 'Thành thị', ttnt == 2 ~ 'Nông thôn', TRUE ~ 'Missing'),
    employment = case_when(
      hdkt == 1 &
        in_vietnam == 1 &
        age >= 15 ~
        'Có việc làm',
      in_vietnam == 1 &
        age >= 15 ~ 'Không',
      TRUE ~ 'Missing'
    ),
    unemployment = case_when(
      hdkt == 2 &
        in_vietnam == 1 &
        age >= 15 ~
        'Thất nghiệp',
      in_vietnam == 1 &
        age >= 15 ~ 'Không',
      TRUE ~ 'Missing'
    ),
    # Tổng số giờ lv trong tuần
    weekhour = c45,
    underemployment = case_when(
      thieuvieclamthongthuong1 == 1 &
        employment == 'Có việc làm' ~ 'Thiếu việc làm',
      employment == 'Có việc làm' ~ "Không",
      TRUE ~ 'Missing'
    ) ,
    labour_force = case_when(
      hdkt %in% c(1, 2) &
        in_vietnam == 1 &
        age >= 15 ~ 'Lực lượng lao động',
      in_vietnam == 1 &
        age >= 15 ~ 'Không',
      TRUE ~ 'Missing'
    ),
    lfp_rate = case_when(
      labour_force == 'Lực lượng lao động' ~ 100,
      labour_force == 'Không' ~ 0,
      TRUE ~ NA_real_
    ),
    unemployment_rate = case_when(
      unemployment == 'Thất nghiệp' ~ 100,
      unemployment == 'Không' ~ 0,
      TRUE ~ NA_real_
    ),
    underemployment_rate = case_when(
      underemployment == 'Thiếu việc làm' ~ 100,
      underemployment == 'Không' ~ 0,
      TRUE ~ NA_real_
    ),
    economic_sector = case_when(
      c31  %in% c(1, 2) ~ 'Hộ NLT/ Cá nhân',
      c31 == 3 ~ 'Hộ SXKD cá thể',
      c31 == 4 ~ 'Tập thể',
      c31 %in% c(5, 6) ~ 'Tư nhân',
      c31 %in% c(7, 8, 9, 10) ~ 'Nhà nước',
      c31 == 11 ~ 'Vốn đầu tư nước ngoài',
      c31 == 12 ~ 'Tổ chức, đoàn thể khác',
      TRUE ~ 'Missing'
    ),
    economic_sector0 = case_when(
      c31 == 1 ~ "Hộ NLTS",
      c31 == 2 ~ "Cá nhân làm tự do",
      c31 == 3 ~ "Cơ sở SXKD cá thể",
      c31 == 4 ~ "Tập thể",
      c31 == 5 ~ "DN ngoài NN",
      c31 == 6 ~ "ĐV sự nghiệp ngoài NN",
      c31 == 7 ~ "CQ Lập pháp, Hành pháp, Tư pháp",
      c31 == 8 ~ "Tổ chức NN",
      c31 == 9 ~ "ĐV sự nghiệp NN",
      c31 == 10 ~ "Doanh nghiệp NN",
      c31 == 11 ~ "Vốn đầu tư nước ngoài",
      c31 == 12 ~ "Tổ chức, đoàn thể khác",
      TRUE ~  "Missing"
    ),
    occup2 = c29c %/% 100,
    occup1 = f_occup1(occup2),
    indus2 = c30c %/% 100,
    indus1 = f_indus1(indus2),
    employment_status = case_when(
      c35 == 1 ~ 'Chủ cơ sở',
      c35 == 2 ~ 'Tự làm',
      c35 == 3 ~ 'Lao động GĐ',
      c35 == 4 ~ 'Xã viên HTX',
      c35 == 5 ~ 'Làm công hưởng lương',
      TRUE ~ 'Missing'
    ),
    income_all_job = case_when(c44 > 0 ~ c44, TRUE ~ 0),
    income_main_job = case_when(c44a > 0 ~ c44a, TRUE ~ 0),
    # Thời gian thất nghiệp
    time_tn = case_when(
      c64 == 1 ~ 'Dưới 1 tháng',
      c64 == 2 ~ 'Từ 1 - 3 tháng',
      c64 == 3 ~ 'Từ 3 - 12 tháng',
      c64 == 4 ~ 'Từ 1 - 5 năm',
      c64 == 5 ~ 'Trên 5 năm',
      TRUE ~ 'Missing'
    ),
    hinhthuctimviec = case_when(
      c58 == 1 ~ "Nộp đơn xin việc",
      c58 == 2 ~ "Liên hệ/ tư vấn cơ sở dịch vụ việc làm",
      c58 == 3 ~ "Qua bạn bè, người thân",
      c58 == 4 ~ "Đặt quảng cáo tìm việc",
      c58 == 5 ~ "Qua thông báo tuyển dụng",
      c58 == 6 ~ "Đang tham gia phỏng vấn",
      c58 == 7 ~ "Tìm kiếm việc tự do",
      c58 == 8 ~ "Chuẩn bị để bắt đầu hoạt động SX-KD",
      TRUE ~ "Missing"
    ),
    prev_occup2 = c66c %/% 100,
    prev_occup1 = f_occup1(prev_occup2),
    prev_indus2 = c67c %/% 100,
    prev_indus1 = f_indus1(prev_indus2),
    prev_nganh_n_c_d = f_nganh_n_c_d(prev_indus2),
    prev_employment_status = case_when(
      c68 == 1 ~ 'Chủ cơ sở',
      c68 == 2 ~ 'Tự làm',
      c68 == 3 ~ 'Lao động GĐ',
      c68 == 4 ~ 'Xã viên HTX',
      c68 == 5 ~ 'Làm công hưởng lương',
      TRUE ~ 'Missing'
    ),
    prev_economic_sector = case_when(
      c69 %in% c(1, 2) ~ 'Hộ NLT/ Cá nhân',
      c69 == 3 ~ 'Hộ SXKD cá thể',
      c69 == 4 ~ 'Tập thể',
      c69 %in% c(5, 6) ~ 'Tư nhân',
      c69 %in% c(7, 8, 9, 10) ~ 'Nhà nước',
      c69 == 11 ~ 'Vốn đầu tư nước ngoài',
      c69 == 12 ~ 'Tổ chức, đoàn thể khác',
      TRUE ~ 'Missing'
    ),
    hoatdong = case_when(
      labour_force == 'Lực lượng lao động' ~ 'Tham gia LLLĐ',
      c24 == 2 ~ 'Sinh viên/ học sinh',
      !is.na(c24) ~ 'Khác',
      TRUE ~ 'Missing'
    ),
    lydokolv = case_when(
      labour_force == 'Lực lượng lao động' ~ 'Missing',
      c24 == 2 ~ 'Sinh viên/ học sinh',
      c24 == 3 ~ 'Mất khả năng lao động',
      c24 == 5 ~ 'Nội trợ',
      !is.na(c24) ~ 'Khác',
      TRUE ~ 'Missing'
    ),
    extra_hours = case_when(c45b > 0 ~ c45, TRUE ~ NA_real_),
    migration = case_when(
      c10 %in% 1:4 & age >= 5 ~ 'Di cư',
      c10 == 5 & age >= 5 ~ 'Không di cư',
      TRUE ~ 'Khác'
    ),
    migration_reasons = case_when(
      c13 == 1 ~ 'Tìm việc',
      c13 == 2 ~ 'Bắt đầu công việc mới',
      c13 == 3 ~ 'Mất việc/ không tìm được việc',
      c13 == 4 ~ 'Theo gia đình/ nghỉ hưu',
      c13 == 5 ~ 'Kết hôn',
      c13 == 6 ~ 'Chuyển nhà',
      c13 == 7 ~ 'Cải thiện điều kiện sống',
      c13 == 8 ~ 'Đi học',
      c13 == 9 ~ 'Ảnh hưởng môi trường',
      c13 == 10 ~ 'Khác',
      migration == 'Di cư' ~ 'Khác',
      TRUE ~ 'Missing'
    ),
    provinces_migration = c12a,
    reg_migration = f_reg6(provinces_migration),
    weight = cal_weigh
  )
