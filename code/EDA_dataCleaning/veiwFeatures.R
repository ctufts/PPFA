[1] "n_0078" "n_0108" "c_0391" "c_0572" "c_0591" "c_0835" "c_0944" "c_1029" "c_1033"
[10] "c_1095" "c_1100" "c_1172" "c_1190" "c_1225" "c_1231" "c_1247" "c_1259"


ggplot(train.ds, aes(x = c_1259, fill = factor(service_b))) + 
  geom_histogram(position = "dodge")
