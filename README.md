README **đã chỉnh đúng theo package** (anh chỉ việc paste vào: `D:/GitHub/p_ackage/rtool/README.md`)

------

```
# 📦 rtool — Bộ lệnh tiện dụng cho mọi project R

**Author:** Đỗ Thanh Liêm  
**Mục tiêu:** Làm việc nhanh – gọn – thân thiện Windows & macOS – dùng tốt khi sync GitHub đa nền tảng.

---

## 🚀 Cài đặt

### Cách 1: cài từ GitHub (khuyến nghị)

```r
install.packages("remotes")
remotes::install_github("henrydoth/rtool")
```

### Cách 2: dev mode trong project (dành cho develop)

```
devtools::load_all("D:/GitHub/p_ackage/rtool")
```

------

## ✅ Cách dùng

```
library(rtool)
```

Gợi ý: xem nhanh help dạng HTML:

```
h_elp()
```

------

# 🔰 Nguyên lý cực cơ bản (phải hiểu)

## 1️⃣ Project Root

- Nếu có `{here}` → dùng `here::here()` làm root.
- Nếu không → fallback `getwd()`.

```
g_w()
```

------

## 2️⃣ Cách hiểu đường dẫn

| Kiểu viết         | Hiểu theo        |
| ----------------- | ---------------- |
| `"R"`             | project root     |
| `"m_p4"`          | project root     |
| `".."`            | thư mục hiện tại |
| `"./"`            | thư mục hiện tại |
| `"D:/GitHub/..."` | absolute Windows |
| `"/Users/..."`    | absolute macOS   |

------

## 3️⃣ Glob (* và ?)

Trong R **phải đặt trong dấu nháy**

✅ Đúng:

```
c_d("*yuan*")
d_ir("R/*.R")
g_ind("*bai_bao*")
```

❌ Sai:

```
c_d(*yuan*)
```

------

# 📂 Các lệnh chính

## 📁 1) Thư mục làm việc

```
g_w()
```

Hiển thị:

- Working directory
- Project root

------

## 📂 2) Chuyển thư mục – `c_d()`

```
c_d()                  # về root
c_d("m_p4")            # vào folder m_p4
c_d("m_p4", ls=TRUE)   # vào + list luôn
c_d("..")              # lên 1 cấp
c_d("*hop*")           # match glob (chọn match đầu tiên)
c_d("D:/GitHub/x")     # absolute Windows
c_d("/Users/mac/x")    # absolute macOS
```

------

## 📄 3) List nhanh – `l_s()` (alias của `d_ir()`)

```
l_s()
l_s("R")
l_s("*.qmd")
l_s(type="dir")
l_s(type="file")
```

------

## 📄 4) List nâng cao – `d_ir()`

```
d_ir()
d_ir("R")
d_ir("R/*.R")
d_ir("*.docx")
```

------

## 🔍 5) Tìm file

### Regex search (pattern)

```
f_ind("template")
```

### Glob recursive

```
g_ind("*.docx")
g_ind("*bai_bao*", path="R")
g_find("*.docx")   # alias
```

------

## ✏️ 6) Mở file

```
e_dit("R/temp_backup.R")   # mở trong RStudio
o_pen("file.docx")         # mở bằng app mặc định
```

------

## 🕒 7) File mới nhất

```
l_ast()
l_ast(5)
l_ast("R", n=15)
```

------

## 📁 8) Tạo folder / file

```
c_dir("r_md")
c_dir("r_md", "a.txt")
dir_("r_md", "a.txt")   # alias (đã đổi tên an toàn cho package)
mkdir("m_d")
```

------

## 🔁 9) Đổi tên file

```
r_name("r_md", "old.txt", "new.txt")
```

------

# 🧠 Workflow đề xuất

```
library(rtool)

g_w()
c_d("R")
l_s()
g_ind("*bai_bao*")
e_dit("08_script.R")
```

------

# 🎯 Triết lý thiết kế

- ✅ Hoạt động giống terminal
- ✅ Hỗ trợ glob
- ✅ Thân thiện Windows + macOS
- ✅ Không phụ thuộc IDE (có RStudio thì mở Viewer/editor đẹp hơn)
- ✅ An toàn khi dùng GitHub sync đa máy