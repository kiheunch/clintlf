round2 <- function(x, digits) {
  correction <- sqrt(.Machine$double.eps)
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + correction
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

round2_scaled <- function(x, digits) {
  correction <- .Machine$double.eps * abs(x) * 10^digits
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + correction
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

round2(       2436.845, 2)
round2_scaled(2436.845, 2)

round2(4.94999999       ,  1)
round2(4.949999999      ,  1)     # 5.0 threshold
round2(4.949999999999   ,  1)
round2(4.94999999999999 ,  1)
round2(4.949999999999999,  1)

round2_scaled(4.94999999       ,  1)
round2_scaled(4.949999999      ,  1) # round2 threshold    
round2_scaled(4.949999999999   ,  1) # SAS round threshold
round2_scaled(4.94999999999999 ,  1)
round2_scaled(4.949999999999999,  1) # 5.0 threshold

data test_round;
  a = 4.94999999999;
  b = 4.949999999999;
  c = 4.94999999999999;
  d = 4.949999999999999;
  

  a_round = round(a, 0.1);
  b_round = round(b, 0.1); /*rounds 5.0 at b*/
  c_round = round(c, 0.1);
  d_round = round(d, 0.1);

  put "Variable  Stored Value          Rounded";
  put "a      " a 20.15 a_round 8.1;
  put "b      " b 20.15 b_round 8.1;
  put "c      " c 20.15 c_round 8.1;
  put "d      " d 20.15 d_round 8.1;
run;