# ==========================================
# Numerical V-mask example for teaching
# ==========================================

rm(list=ls())

setwd("E:/統計品質管制/Week12")

library(ggplot2)

dir.create("figs",showWarnings=FALSE)

#-------------------------
# fixed example
#-------------------------
n <- 16:20

C <- c(1.4,1.8,2.1,2.5,2.8)

Cn <- 2.8
h <- 1
k <- 0.1

L <- Cn-h-k*(20-n)

dat <- data.frame(
  n=n,
  C=C,
  L=L
)

#=========================
# step 1
#=========================
p1 <- ggplot(dat,aes(n,C))+
  
  geom_line(linewidth=.8)+
  geom_point(size=3)+
  
  geom_line(aes(y=L),
            linetype=2,
            linewidth=.8)+
  
  theme_bw(base_size=15)+
  labs(
    title="Step 1: Draw the lower arm",
    x="Time",
    y="C_n"
  )

ggsave(
  "figs/vmask_numeric_step1.png",
  p1,width=6,height=4
)

#=========================
# step 2
#=========================
p2 <- p1+
  labs(
    title="Step 2: All points stay above"
  )

ggsave(
  "figs/vmask_numeric_step2.png",
  p2,width=6,height=4
)

#=========================
# step 3
#=========================
dat2 <- dat
dat2$C[1] <- 1.0

p3 <- ggplot(dat2,aes(n,C))+
  
  geom_line(linewidth=.8)+
  geom_point(size=3)+
  
  geom_line(
    aes(y=L),
    linetype=2,
    linewidth=.8
  )+
  
  geom_point(
    data=dat2[1,],
    size=5
  )+
  
  annotate(
    "text",
    x=16.4,
    y=0.8,
    label="outside"
  )+
  
  theme_bw(base_size=15)+
  labs(
    title="Step 3: Signal",
    x="Time",
    y="C_n"
  )

ggsave(
  "figs/vmask_numeric_step3.png",
  p3,width=6,height=4
)