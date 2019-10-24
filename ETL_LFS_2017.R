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
lfs_2017 <- import('LFS_2017.dta')

lfs_2017 %<>% rename_all(tolower)

# Support functions
eval(parse("R/labels.R", encoding = "UTF-8"))


ldvl_2017 <- lfs_2017 %>%
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
      c14 >= 5 & c14 <= 9 ~ 'THPT',
      c14 == 4 ~ 'THCS',
      c14 == 3 ~ 'Tiểu học',
      c14 == 2 ~ 'Chưa TN tiểu học',
      c14 == 1 ~ 'Không biết đọc/viết',
      TRUE ~ 'Missing'
    ),
    tdvh0 = case_when(
      c14 == 9 ~ 'Trên ĐH',
      c14 == 8 ~ 'ĐH',
      c14 == 7 ~ 'CĐCN',
      c14 == 6 ~ 'THCN',
      c14 == 5 ~ 'THPT',
      c14 == 4 ~ 'THCS',
      c14 == 3 ~ 'Tiểu học',
      c14 == 2 ~ 'Chưa TN tiểu học',
      c14 == 1 ~ 'Không biết đọc/viết',
      TRUE ~ 'Missing'
    ),
    train0 = case_when(
      tdvh0 == 'Trên ĐH' ~ 'Trên ĐH',
      tdvh0 == 'ĐH' ~ 'ĐH',
      tdvh0 == 'CĐCN' ~ 'CĐCN',
      c15 == 7 ~ 'CĐ nghề',
      tdvh0 == 'THCN' ~ 'THCN',
      # THCN thấp hơn CĐ nghề
      c15 == 6 ~ 'TC nghề',
      c15 == 5 ~ 'Sơ cấp nghề',
      c15 == 4 ~ 'Chứng chỉ nghề dưới 3 tháng',
      c15 == 3 ~ 'Kỹ năng nghề dưới 3 tháng',
      c15 == 2 ~ 'CNKT không bằng',
      c15 == 1 ~ 'Không có trình độ, kỹ năng nghề',
      TRUE ~ 'Missing'
    ),
    train = case_when(
      train0 %in% c('ĐH', 'Trên ĐH') ~ 'ĐH trở lên',
      train0 %in% c(
        'Chứng chỉ nghề dưới 3 tháng',
        'Kỹ năng nghề dưới 3 tháng',
        'CNKT không bằng'
      ) ~ 'CNKT không bằng, CC, kỹ năng nghề dưới 3 tháng',
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
    employment = if_else(hdkt == 1 &
                           in_vietnam == 1 &
                           age >= 15, 'Có việc làm', 'Không'),
    unemployment = if_else(hdkt == 2 &
                             in_vietnam == 1 &
                             age >= 15, 'Thất nghiệp', 'Không'),
    # Tổng số giờ lv trong tuần
    weekhour = c40,
    underemployment = case_when(
      thieuvieclamthongthuong1 == 1 &
        employment == 'Có việc làm' ~ 'Thiếu việc làm',
      employment == 'Có việc làm' ~ "Không",
      TRUE ~ 'Missing'
    ) ,
    labour_force = case_when(
      hdkt %in% c(1, 2) &
        in_vietnam == 1 &
        age >= 15 ~ 'Lực lượng lao đông',
      TRUE ~ 'Không'
    ),
    lfp_rate = if_else(labour_force == 1 , 100, 0),
    unemployment_rate = if_else(unemployment == 1, 100, 0),
    underemployment_rate = if_else(underemployment == 1, 100, 0),
    economic_sector = case_when(
      c26 %in% c(1, 2) ~ 'Hộ NLT/ Cá nhân',
      c26 == 3 ~ 'Hộ SXKD cá thể',
      c26 == 4 ~ 'Tập thể',
      c26 %in% c(5, 6) ~ 'Tư nhân',
      c26 %in% c(7, 8, 9, 10) ~ 'Nhà nước',
      c26 == 11 ~ 'Vốn đầu tư nước ngoài',
      c26 == 12 ~ 'Tổ chức, đoàn thể khác',
      TRUE ~ 'Missing'
    ),
    economic_sector0 = case_when(
      c26 == 1 ~ "Hộ NLTS",
      c26 == 2 ~ "Cá nhân làm tự do",
      c26 == 3 ~ "Cơ sở SXKD cá thể",
      c26 == 4 ~ "Tập thể",
      c26 == 5 ~ "DN ngoài NN",
      c26 == 6 ~ "ĐV sự nghiệp ngoài NN",
      c26 == 7 ~ "CQ Lập pháp, Hành pháp, Tư pháp",
      c26 == 8 ~ "Tổ chức NN",
      c26 == 9 ~ "ĐV sự nghiệp NN",
      c26 == 10 ~ "Doanh nghiệp NN",
      c26 == 11 ~ "Vốn đầu tư nước ngoài",
      c26 == 12 ~ "Tổ chức, đoàn thể khác",
      TRUE ~  "Missing"
    ),
    occup2 = round(c24 / 100, 0),
    occup1 = f_occup1(occup2),
    indus2 = round(c25 / 100, 0),
    indus1 = f_indus1(indus2),
    employment_status = case_when(c30 == 1 ~ 'Chủ cơ sở',
                                  c30 == 2 ~ 'Tự làm',
                                  c30 == 3 ~ 'Lao động GĐ',
                                  c30 == 4 ~ 'Xã viên HTX',
                                  c30 == 5 ~ 'Làm công hưởng lương',
                                  TRUE ~ 'Missing'
    ),
    income_all_job = case_when(c39 > 0 ~ c39, TRUE ~ 0),
    income_main_job = case_when(c39a > 0 ~ c39a, TRUE ~ 0),
    # Thời gian thất nghiệp
    time_tn = case_when(c59 == 1 ~ 'Dưới 1 tháng', 
                        c59 == 2 ~ 'Từ 1 - 3 tháng',
                        c59 == 3 ~ 'Từ 3 - 12 tháng',
                        c59 == 4 ~ 'Từ 1 - 5 năm',
                        c59 == 5 ~ 'Trên 5 năm',
                        TRUE ~ 'Missing'),
    hinhthuctimviec = case_when(c53 == 1 ~ "Nộp đơn xin việc",
                                c53 == 2 ~ "Liên hệ/ tư vấn cơ sở dịch vụ việc làm",
                                c53 == 3 ~ "Qua bạn bè, người thân",
                                c53 == 4 ~ "Đặt quảng cáo tìm việc",
                                c53 == 5 ~ "Qua thông báo tuyển dụng",
                                c53 == 6 ~ "Đang tham gia phỏng vấn",
                                c53 == 7 ~ "Tìm kiếm việc tự do",
                                c53 == 8 ~ "Chuẩn bị để bắt đầu hoạt động SX-KD",
                                TRUE ~ "Missing"),
    prev_occup2 = round(c61/100, 0),
    prev_occup1 = f_occup1(prev_occup2),
    prev_indus2 = round(c62/100, 0),
    prev_indus1 = f_indus1(prev_indus2),
    prev_nganh_n_c_d = f_nganh_n_c_d(prev_indus2),
    employment_status = case_when(c63 == 1 ~ 'Chủ cơ sở',
                                  c63 == 2 ~ 'Tự làm',
                                  c63 == 3 ~ 'Lao động GĐ',
                                  c63 == 4 ~ 'Xã viên HTX',
                                  c63 == 5 ~ 'Làm công hưởng lương',
                                  TRUE ~ 'Missing'
    ),
    economic_sector = case_when(
      c64 %in% c(1, 2) ~ 'Hộ NLT/ Cá nhân',
      c64 == 3 ~ 'Hộ SXKD cá thể',
      c64 == 4 ~ 'Tập thể',
      c64 %in% c(5, 6) ~ 'Tư nhân',
      c64 %in% c(7, 8, 9, 10) ~ 'Nhà nước',
      c64 == 11 ~ 'Vốn đầu tư nước ngoài',
      c64 == 12 ~ 'Tổ chức, đoàn thể khác',
      TRUE ~ 'Missing'
    ),
    hoatdong = case_when(labour_force == 'Lực lượng lao động' ~ 'Tham gia LLLĐ',
                         c19 == 2 ~ 'Sinh viên/ học sinh',
                         !is.na(c19) ~ 'Khác',
                         TRUE ~ 'Missing'),
    lydokolv = case_when(c19 == 2 ~ 'Sinh viên/ học sinh',
                         c19 == 3 ~ 'Mất khả năng lao động',
                         c19 == 5 ~ 'Nội trợ',
                         !is.na(c19) ~ 'Khác',
                         TRUE ~ 'Missing'),
    extra_hours = case_when(c40b > 0 ~ c40, TRUE ~ NA_real_ ),
    migration = case_when(c10 %in% 1:4 & age >= 5 ~ 'Di cư',
                          c10 == 5 & age >=5 ~ 'Không di cư',
                          TRUE ~ 'Khác'),
    migration_reasons = case_when(c13 == 1 ~ 'Tìm việc',
                                  c13 == 2 ~ 'Bắt đầu công việc mới',
                                  c13 == 3 ~ 'Mất việc/ không tìm được việc',
                                  c13 == 4 ~ 'Theo gia đình/ nghỉ hưu',
                                  c13 == 5 ~ 'Kết hôn',
                                  c13 == 6 ~ 'Chuyển nhà',
                                  c13 == 7 ~ 'Cải thiện điều kiện sống',
                                  c13 == 8 ~ 'Đi học',
                                  c13 == 9 ~ 'Khác',
                                  migration == 'Di cư' ~ 'Khác',
                                  TRUE ~ 'Missing'),
    provinces_migration = c12a,
    reg_migration = f_reg6(provinces_migration),
    weight = weigh_final
  )
