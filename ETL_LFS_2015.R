# Author: Nguyễn Ngọc Bình
# First: 16/11/2019
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
lfs_2015 <- import('LFS_2015.dta') %>% rename_all(tolower)

# Support functions
eval(parse("R/labels.R", encoding = "UTF-8"))


ldvl_2015 <- lfs_2015 %>%
  mutate(
    in_vietnam = 1,
    gender = case_when(c3 == 1 ~ 'Nam', c3 == 2 ~ 'Nữ', TRUE ~ 'Missing'),
    age = c5,
    agegroup5 = f_age_grp5(age),
    marital_status = case_when(
      c7 == 1 ~ 'Chưa có vợ/chồng',
      c7 == 2 ~ 'Đã có vợ/chồng',
      c7 == 3 ~ 'Góa',
      c7 == 4 |
        c7 == 5 ~ 'Ly hôn/Ly thân',
      TRUE ~ 'Missing'
    ),
    tdvh = case_when(
      c12 >= 5 & c12 <= 9 ~ 'THPT',
      c12 == 4 ~ 'THCS',
      c12 == 3 ~ 'Tiểu học',
      c12 == 2 ~ 'Chưa TN tiểu học',
      c12 == 1 ~ 'Không biết đọc/viết',
      TRUE ~ 'Missing'
    ),
    tdvh0 = case_when(
      c12 == 9 ~ 'Trên ĐH',
      c12 == 8 ~ 'ĐH',
      c12 == 7 ~ 'CĐCN',
      c12 == 6 ~ 'THCN',
      c12 == 5 ~ 'THPT',
      c12 == 4 ~ 'THCS',
      c12 == 3 ~ 'Tiểu học',
      c12 == 2 ~ 'Chưa TN tiểu học',
      c12 == 1 ~ 'Không biết đọc/viết',
      TRUE ~ 'Missing'
    ),
    train0 = case_when(
      tdvh0 == 'Trên ĐH' ~ 'Trên ĐH',
      tdvh0 == 'ĐH' ~ 'ĐH',
      tdvh0 == 'CĐCN' ~ 'CĐCN',
      c13 == 7 ~ 'CĐ nghề',
      tdvh0 == 'THCN' ~ 'THCN',
      # THCN thấp hơn CĐ nghề
      c13 == 6 ~ 'TC nghề',
      c13 == 5 ~ 'Sơ cấp nghề',
      c13 == 4 ~ 'Chứng chỉ nghề dưới 3 tháng',
      c13 == 3 ~ 'Kỹ năng nghề dưới 3 tháng',
      c13 == 2 ~ 'CNKT không bằng',
      c13 == 1 ~ 'Không có trình độ, kỹ năng nghề',
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
    underemployment = NA,
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
      c24  %in% c(1, 2) ~ 'Hộ NLT/ Cá nhân',
      c24 == 3 ~ 'Hộ SXKD cá thể',
      c24 == 4 ~ 'Tập thể',
      c24 %in% c(5, 6) ~ 'Tư nhân',
      c24 %in% c(7, 8, 9, 10) ~ 'Nhà nước',
      c24 == 11 ~ 'Vốn đầu tư nước ngoài',
      c24 == 12 ~ 'Tổ chức, đoàn thể khác',
      TRUE ~ 'Missing'
    ),
    economic_sector0 = case_when(
      c24 == 1 ~ "Hộ NLTS",
      c24 == 2 ~ "Cá nhân làm tự do",
      c24 == 3 ~ "Cơ sở SXKD cá thể",
      c24 == 4 ~ "Tập thể",
      c24 == 5 ~ "DN ngoài NN",
      c24 == 6 ~ "ĐV sự nghiệp ngoài NN",
      c24 == 7 ~ "CQ Lập pháp, Hành pháp, Tư pháp",
      c24 == 8 ~ "Tổ chức NN",
      c24 == 9 ~ "ĐV sự nghiệp NN",
      c24 == 10 ~ "Doanh nghiệp NN",
      c24 == 11 ~ "Vốn đầu tư nước ngoài",
      c24 == 12 ~ "Tổ chức, đoàn thể khác",
      TRUE ~  "Missing"
    ),
    occup2 = c22 %/% 100,
    occup1 = f_occup1(occup2),
    indus2 = c23 %/% 100,
    indus1 = f_indus1(indus2),
    employment_status = case_when(
      c28 == 1 ~ 'Chủ cơ sở',
      c28 == 2 ~ 'Tự làm',
      c28 == 3 ~ 'Lao động GĐ',
      c28 == 4 ~ 'Xã viên HTX',
      c28 == 5 ~ 'Làm công hưởng lương',
      TRUE ~ 'Missing'
    ),
    # income_all_job = case_when(c39 > 0 ~ c39, TRUE ~ 0),
    income_main_job = case_when(c40a > 0 ~ c40a, TRUE ~ 0),
    # Thời gian thất nghiệp
    time_tn = case_when(
      c59 == 1 ~ 'Dưới 1 tháng',
      c59 == 2 ~ 'Từ 1 - 3 tháng',
      c59 == 3 ~ 'Từ 3 - 12 tháng',
      c59 == 4 ~ 'Từ 1 - 5 năm',
      c59 == 5 ~ 'Trên 5 năm',
      TRUE ~ 'Missing'
    ),
    hinhthuctimviec = case_when(
      c53 == 1 ~ "Nộp đơn xin việc",
      c53 == 2 ~ "Liên hệ/ tư vấn cơ sở dịch vụ việc làm",
      c53 == 3 ~ "Qua bạn bè, người thân",
      c53 == 4 ~ "Đặt quảng cáo tìm việc",
      c53 == 5 ~ "Qua thông báo tuyển dụng",
      c53 == 6 ~ "Đang tham gia phỏng vấn",
      c53 == 7 ~ "Tìm kiếm việc tự do",
      c53 == 8 ~ "Chuẩn bị để bắt đầu hoạt động SX-KD",
      TRUE ~ "Missing"
    ),
    prev_occup2 = c61 %/% 100,
    prev_occup1 = f_occup1(prev_occup2),
    prev_indus2 = c62 %/% 100,
    prev_indus1 = f_indus1(prev_indus2),
    prev_nganh_n_c_d = f_nganh_n_c_d(prev_indus2),
    prev_employment_status = case_when(
      c63 == 1 ~ 'Chủ cơ sở',
      c63 == 2 ~ 'Tự làm',
      c63 == 3 ~ 'Lao động GĐ',
      c63 == 4 ~ 'Xã viên HTX',
      c63 == 5 ~ 'Làm công hưởng lương',
      TRUE ~ 'Missing'
    ),
    prev_economic_sector = case_when(
      c64 %in% c(1, 2) ~ 'Hộ NLT/ Cá nhân',
      c64 == 3 ~ 'Hộ SXKD cá thể',
      c64 == 4 ~ 'Tập thể',
      c64 %in% c(5, 6) ~ 'Tư nhân',
      c64 %in% c(7, 8, 9, 10) ~ 'Nhà nước',
      c64 == 11 ~ 'Vốn đầu tư nước ngoài',
      c64 == 12 ~ 'Tổ chức, đoàn thể khác',
      TRUE ~ 'Missing'
    ),
    hoatdong = case_when(
      labour_force == 'Lực lượng lao động' ~ 'Tham gia LLLĐ',
      c17 == 2 ~ 'Sinh viên/ học sinh',
      !is.na(c17) ~ 'Khác',
      TRUE ~ 'Missing'
    ),
    lydokolv = case_when(
      labour_force == 'Lực lượng lao động' ~ 'Missing',
      c17 == 2 ~ 'Sinh viên/ học sinh',
      c17 == 3 ~ 'Mất khả năng lao động',
      c17 == 5 ~ 'Nội trợ',
      !is.na(c17) ~ 'Khác',
      TRUE ~ 'Missing'
    ),
    extra_hours = case_when(c43 ==1 ~ c45-c41, TRUE ~ NA_real_),
    migration = case_when(
      c8 %in% 1:4 & age >= 5 ~ 'Di cư',
      c8 == 5 & age >= 5 ~ 'Không di cư',
      TRUE ~ 'Khác'
    ),
    migration_reasons = case_when(
      c11 == 1 ~ 'Tìm việc',
      c11 == 2 ~ 'Bắt đầu công việc mới',
      c11 == 3 ~ 'Mất việc/ không tìm được việc',
      c11 == 4 ~ 'Theo gia đình/ nghỉ hưu',
      c11 == 5 ~ 'Kết hôn',
      c11 == 6 ~ 'Chuyển nhà',
      c11 == 7 ~ 'Cải thiện điều kiện sống',
      c11 == 8 ~ 'Đi học',
      c11 == 9 ~ 'Khác',
      migration == 'Di cư' ~ 'Khác',
      TRUE ~ 'Missing'
    ),
    provinces_migration = c10,
    reg_migration = f_reg6(provinces_migration),
    weight = weigh
  )
