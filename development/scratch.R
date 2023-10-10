
t <- c("aa", "bb", "cc", "ab", "at")

# drop items from that contain an "a"
print(t[!grepl("a", t)])
