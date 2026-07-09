test_that("fmt_round: basic rounding to 2 decimal places", {
  expect_equal(fmt_round(3.14159, 2), "3.14")
  expect_equal(fmt_round(2.71828, 2), "2.72")
})

test_that("fmt_round: half away from zero at input of zero", {
  expect_equal(fmt_round(0,  0), "0") 
  expect_equal(fmt_round(0,  1), "0.0") 
  expect_equal(fmt_round(0,  2), "0.00") 
})

test_that("fmt_round: half away from zero at positive input ending in 5", {
  expect_equal(fmt_round(0.5,  0), "1")
  expect_equal(fmt_round(1.5,  0), "2")
  expect_equal(fmt_round(2.5,  0), "3")
  expect_equal(fmt_round(0.05, 1), "0.1") 
  expect_equal(fmt_round(0.15, 1), "0.2") 
  expect_equal(fmt_round(0.25, 1), "0.3")  
})

test_that("fmt_round: half away from zero at negative input ending in 5", {
  expect_equal(fmt_round(-0.5,  0), "-1")
  expect_equal(fmt_round(-1.5,  0), "-2")
  expect_equal(fmt_round(-2.5,  0), "-3")
  expect_equal(fmt_round(-0.05, 1), "-0.1") 
  expect_equal(fmt_round(-0.15, 1), "-0.2") 
  expect_equal(fmt_round(-0.25, 1), "-0.3")  
})

test_that("fmt_round: leading and trailing zero behavior", {
  expect_equal(fmt_round(1.1,   2), "1.10")
  expect_equal(fmt_round(02.0,  2), "2.00")
  expect_equal(fmt_round(000.1, 3), "0.100")
  expect_equal(fmt_round(10.0,  1), "10.0")
  expect_equal(fmt_round(10.00000000,  0), "10")
})

test_that("fmt_round: special input", {
  expect_equal(fmt_round(Inf     , 1), "Inf")
  expect_equal(fmt_round(-Inf    , 2), "-Inf")
  expect_equal(fmt_round(NA_real_, 3), "NA")
  expect_equal(fmt_round(NaN     , 4), "NaN")

  expect_equal(fmt_round(c(Inf, -Inf, NA_real_, NaN),
                            c(1, 2, 3, 4)),
               c("Inf", "-Inf", "NA", "NaN"))
})

test_that("fmt_round: vectorized input", {
  expect_equal(
    fmt_round(c(0.3, 0.035, 0.007, 0.355, 0.365, 0.0101, 0.0099), 2),
    c("0.30", "0.04", "0.01", "0.36", "0.37", "0.01", "0.01")
  )
  expect_equal(
    fmt_round(c(-0.5, -2.5, -1.1), c(0, 1, 2)),
    c("-1", "-2.5", "-1.10")
  )
})

test_that("fmt_round: floating-point representation edge cases", {
  # Simple decimals non-exact in binary
  expect_false(0.1 + 0.2 == 0.3)                   # 0.1 + 0.2 == 0.3 shows FALSE
  expect_equal(fmt_round(0.1 + 0.2, 1), "0.3")
  expect_equal(fmt_round(0.1 + 0.2, 2), "0.30")

  # Half-way values
  expect_equal(fmt_round(0.05,  1), "0.1")
  expect_equal(fmt_round(0.15,  1), "0.2")
  expect_equal(fmt_round(0.25,  1), "0.3")
  expect_equal(fmt_round(0.35,  1), "0.4")
  expect_equal(fmt_round(0.45,  1), "0.5")

  # Accumulated sum
  expect_equal(fmt_round(sum(rep(0.1, 10)), 0), "1")
  expect_equal(fmt_round(sum(rep(0.1, 10)), 1), "1.0")

  # Large integers with decimals
  expect_equal(fmt_round(99999999999.45  , 1), "99999999999.5")
  expect_equal(fmt_round(99999999999.450 , 0), "99999999999")
  expect_equal(fmt_round(99999999999.4999, 0), "99999999999")
  expect_equal(fmt_round(99999999999.5   , 0), "100000000000")

  # Trailing 9s - stored value may cross rounding boundary
  expect_equal(fmt_round(4.94999999999,  1), "4.9")
  expect_equal(fmt_round(4.949999999999, 1), "4.9")
})

test_that("fmt_round: SAS benchmark", {
# data test_round;
#   a = 4.94999999999;
#   b = 4.949999999999;
#   c = 4.94999999999999;
#   d = 4.949999999999999;

#   a_round = round(a, 0.1);
#   b_round = round(b, 0.1); /*rounds 5.0 at b*/
#   c_round = round(c, 0.1);
#   d_round = round(d, 0.1);

#   put "Variable  Stored Value          Rounded";
#   put "a      " a 20.15 a_round 8.1;
#   put "b      " b 20.15 b_round 8.1;
#   put "c      " c 20.15 c_round 8.1;
#   put "d      " d 20.15 d_round 8.1;
# run;
  expect_equal(fmt_round(4.94999999999    ,  1), "4.9")
  expect_equal(fmt_round(4.949999999999   ,  1), "4.9")
  expect_equal(fmt_round(4.94999999999999 ,  1), "4.9")
  expect_equal(fmt_round(4.949999999999999,  1), "5.0")
})


test_that("fmt_round: stackover flow discussion - scaled epsilon edge case c(2436.845, 4.94999999999)", {
  # 2436.845 should round to 2436.85, not 2436.84
  expect_equal(fmt_round(2436.845, 2, eps = TRUE) , "2436.85")
  expect_equal(fmt_round(2436.845, 2, eps = FALSE), "2436.84")  # not correcting for eps would bring it down to 2436.84
  # 4.94999999999999 should round to 4.9, not 5.0
  expect_equal(fmt_round(4.94999999999, 1, eps = TRUE),  "4.9") # non-scaled eps would jump this up to 5.0
  expect_equal(fmt_round(4.94999999999, 1, eps = FALSE), "4.9")
})

test_that("fmt_round: rejects non-numeric x", {
  expect_error(fmt_round("a", 2), "x must be numeric")
})

test_that("fmt_round: rejects non-integer n", {
  expect_error(fmt_round(1.5, 1.5), "n must be a non-negative integer")
})

test_that("fmt_round: rejects negative n", {
  expect_error(fmt_round(1.5, -1), "n must be a non-negative integer")
})

test_that("fmt_round: rejects NA n", {
  expect_error(fmt_round(1.5, NA), "n must be a non-negative integer")
})
test_that("fmt_round: negative values rounding to zero display without sign", {
  expect_equal(fmt_round(-0.001, 2), "0.00")
  expect_equal(fmt_round(-0.4, 0), "0")
  expect_equal(fmt_round(c(-0.001, -0.049), 1), c("0.0", "0.0"))
})

test_that("fmt_round: zero-length input returns character(0)", {
  expect_identical(fmt_round(numeric(0)), character(0))
})
