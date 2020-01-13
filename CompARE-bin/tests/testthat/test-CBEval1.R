context("CBEval1")

test_that("CBEval1 works", {

  #Variable generator

  #Probability of the union
  prob_ce_1 <- prob_ce(0.54,0.4,0.2)
  prob_ce_2 <- prob_ce(0.4,0.54,0.2)
  prob_ce_3 <- prob_ce(0.4,0.54,0.2)
  prob_ce_4 <- prob_ce(0.6,0.3,0.5)
  #Lower correlation
  Lower_corr_1 <- lower_corr(0.2,0.3)
  Lower_corr_2 <- lower_corr(0.5,0.5)
  #Upper correlation
  Upper_corr_1 <- upper_corr(0.6,0.4)
  Upper_corr_2 <- upper_corr(0.2,0.2)
  Upper_corr_3 <- upper_corr(0.2,0.3)
  #Effects CBE
  eff_a = effect_ce(0.3,0.4,0.2,0.5,0.32, type = "diff")
  eff_b = effect_ce(0.3,0.4,0.2,0.5,0.32)
  eff_c = effect_ce(0.3,0.4,0.2,0.5,0.32, type = "or")
  eff_d = effect_ce(0.65,0.23,0.42,0.15,0.32, type = "or")
  eff_e = effect_ce(0.3,0.4,0.2,0.5,0.32, type = "rr")
  eff_f = effect_ce(0.43,0.87,0.32,0.25,0.23, type = "rr")
  #Sample size CBE
  ss_a = sample_size_ce(0.2,0.3,0.1,0.2,"diff",0.4, 0.05,0.2,"unpooled Variance")
  ss_b = sample_size_ce(0.2,0.3,0.1,0.2,rho = 0.4)
  ss_c = sample_size_ce(0.2,0.3,0.1,0.2,rho = 0.4, unpooled = "unpooled Variance")
  ss_d = sample_size_ce(0.1,0.2,0.2,0.3,"rr",0.4, 0.05,0.2,"unpooled Variance")
  ss_e = sample_size_ce(0.2,0.3,0.1,0.2,"rr",0.4, 0.05,0.2,"Variance")
  ss_f = sample_size_ce(0.1,0.2,0.2,0.3,"or",0.4, 0.05,0.2,"unpooled Variance")
  ss_g = sample_size_ce(0.2,0.3,0.1,0.2,"or",0.4, 0.05,0.2,"unpooled Variance")
  ss_h = sample_size_ce(0.2,0.3,0.1,0.2,"or",0.4, 0.05,0.2,"Variance")

  #Validation A

  #Probability of the union
  expect_error(prob_ce())
  expect_error(prob_ce(p_E1=0.3, p_E2=NULL, rho=NULL))
  expect_error(prob_ce(p_E1=NULL, p_E2=0.3, rho=NULL))
  expect_error(prob_ce(p_E1=NULL, p_E2=NULL, rho=0.3))
  #Lower correlation
  expect_error(Lower_corr())
  expect_error(Lower_corr(p_E1=0.3, p_E2=NULL))
  expect_error(Lower_corr(p_E1=NULL, p_E2=0.3))
  #Upper correlation
  expect_error(Upper_corr())
  expect_error(Upper_corr(p_E1=0.3, p_E2=NULL))
  expect_error(Upper_corr(p_E1=NULL, p_E2=0.3))
  #Effects CBE
  expect_error(effect_ce())
  expect_error(effect_ce(p0_E1 = 0.3, p0_E2 = 0.4,p1_E1 = 0.2, p1_E2 = 0.5))
  expect_error(effect_ce(p0_E1 = 0.3, p0_E2 = 0.4,p1_E1 = 0.2, rho = 0.5))
  expect_error(effect_ce(p0_E1 = 0.3, p0_E2 = 0.4,p1_E2 = 0.2, rho = 0.5))
  expect_error(effect_ce(p0_E1 = 0.3, p1_E1 = 0.4,p1_E2 = 0.2, rho = 0.5))
  expect_error(effect_ce(p0_E2 = 0.3, p1_E1 = 0.4,p1_E2 = 0.2, rho = 0.5))
  #Sample size CBE
  expect_error(sample_size_ce())
  expect_error(sample_size_ce(p0_E1 = 0.3, p0_E2 = 0.4,p1_E1 = 0.2, p1_E2 = 0.5))
  expect_error(sample_size_ce(p0_E1 = 0.3, p0_E2 = 0.4,p1_E1 = 0.2, rho = 0.5))
  expect_error(sample_size_ce(p0_E1 = 0.3, p0_E2 = 0.4,p1_E2 = 0.2, rho = 0.5))
  expect_error(sample_size_ce(p0_E1 = 0.3, p1_E1 = 0.4,p1_E2 = 0.2, rho = 0.5))
  expect_error(sample_size_ce(p0_E2 = 0.3, p1_E1 = 0.4,p1_E2 = 0.2, rho = 0.5))


  #Validation B

  #Probability of the union
  expect_that(prob_ce(3,0.4,0.5), throws_error("p_e1 numeric value are not allowed"))
  expect_that(prob_ce(0.3,4,0.5), throws_error("p_e2 numeric value are not allowed"))
  expect_that(prob_ce(0.2,0.4,5), throws_error("rho numeric value are not allowed"))
  #Lower correlation
  expect_that(Lower_corr(4,0.3), throws_error("p.e1 numeric value are not allowed"))
  expect_that(Lower_corr(0.4,3), throws_error("p.e2 numeric value are not allowed"))
  #Upper correlation
  expect_that(Upper_corr(0.4,3), throws_error("p.e2 numeric value are not allowed"))
  expect_that(Upper_corr(4,0.3), throws_error("p.e1 numeric value are not allowed"))
  #Effects CBE
  expect_that(effect_ce(3,0.4,0.2,0.5,0.32), throws_error("p0_e1 numeric value are not allowed"))
  expect_that(effect_ce(0.3,4,0.2,0.5,0.32), throws_error("p0_e2 numeric value are not allowed"))
  expect_that(effect_ce(0.3,0.4,2,0.5,0.32), throws_error("p1_e1 numeric value are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,5,0.32), throws_error("p1_e2 numeric value are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,5,32), throws_error("p1_e2 numeric value are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,0.5,32), throws_error("rho numeric value are not allowed"))
  #For characters
  expect_that(effect_ce(0.3,0.4,0.2,0.5,0.32, type = "Diff"), throws_error("This effect type are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,0.5,0.32, type = "dIff"), throws_error("This effect type are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,0.5,0.32, type = "diFf"), throws_error("This effect type are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,0.5,0.32, type = "difF"), throws_error("This effect type are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,0.5,0.32, type = "Or"), throws_error("This effect type are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,0.5,0.32, type = "oR"), throws_error("This effect type are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,0.5,0.32, type = "Rr"), throws_error("This effect type are not allowed"))
  expect_that(effect_ce(0.3,0.4,0.2,0.5,0.32, type = "rR"), throws_error("This effect type are not allowed"))
  #Sample size CBE
  expect_that(sample_size_ce(0.2,0.3,0.1,0.2,"diff",0.4, 0.05,2,"unpooled Variance"), throws_error("beta numeric value are not allowed"))
  expect_that(sample_size_ce(0.2,0.3,0.1,0.2,"diff",0.4, 5,0.2,"unpooled Variance"), throws_error("alpha numeric value are not allowed"))
  expect_that(sample_size_ce(0.2,0.3,0.1,0.2,"diff",4, 0.05,0.2,"unpooled Variance"), throws_error("rho numeric value are not allowed"))
  expect_that(sample_size_ce(0.2,0.3,0.1,2,"diff",0.4, 0.05,0.2,"unpooled Variance"), throws_error("p1.e2 numeric value are not allowed"))
  expect_that(sample_size_ce(0.2,0.3,1,0.2,"diff",0.4, 0.05,0.2,"unpooled Variance"), throws_error("p1.e1 numeric value are not allowed"))
  expect_that(sample_size_ce(0.2,3,0.1,0.2,"diff",0.4, 0.05,0.2,"unpooled Variance"), throws_error("p0.e1 numeric value are not allowed"))
  expect_that(sample_size_ce(2,0.3,0.1,0.2,"diff",0.4, 0.05,0.2,"unpooled Variance"), throws_error("p0.e2 numeric value are not allowed"))

  #Validation C

  condition <- function(a,b,c){
    i <- 3
    if(c == "T"){
      expect_true(a[i,] == b[i,])
    }else if(c == "F"){
      expect_false(a[i,] == b[i,])
    }
  }

  #Probability of the union
  condition(prob_ce_1, prob_ce_2,"T")
  condition(prob_ce_3, prob_ce_2,"T")
  condition(prob_ce_1, prob_ce_3,"T")
  condition(prob_ce_1, prob_ce_4,"F")
  #Lower correlation
  condition(Lower_corr_2,1,"T")
  #Upper correlation
  condition(Upper_corr_2,1,"T")
  #Correlations together
  condition(Lower_corr_1,Upper_corr_3,"F")
  #Effects CBE
  condition(eff_a,eff_b,"T")
  condition(eff_a,eff_c,"F")
  condition(eff_a,eff_e,"F")
  condition(eff_e,eff_c,"F")
  condition(eff_c,eff_d,"F")
  condition(eff_e,eff_f,"F")
  #Sample size CBE
  validation(ss_a,s_b,"T")
  validation(ss_a,s_c,"T")
  validation(ss_c,s_b,"T")
  validation(ss_d,ss_e,"F")
  validation(ss_f,ss_g,"T")
  validation(ss_f,ss_h, "F")

  #Validation D

  #Probability of the union
  expect_that(prob_ce_1, is_a("numeric"))
  expect_that(prob_ce_2, is_a("numeric"))
  expect_that(prob_ce_3, is_a("numeric"))
  expect_that(prob_ce_4, is_a("numeric"))
  #Lower correlation
  expect_that(Lower_corr_1, is_a("numeric"))
  expect_that(Lower_corr_2, is_a("numeric"))
  #Upper correlation
  expect_that(Upper_corr_1, is_a("numeric"))
  expect_that(Upper_corr_2, is_a("numeric"))
  expect_that(Upper_corr_3, is_a("numeric"))
  #Effects CBE
  expect_that(eff_a, is_a("numeric"))
  expect_that(eff_b, is_a("numeric"))
  expect_that(eff_c, is_a("numeric"))
  expect_that(eff_d, is_a("numeric"))
  expect_that(eff_e, is_a("numeric"))
  expect_that(eff_f, is_a("numeric"))
  #Sample size CBE
  expect_that(ss_a, is_a("numeric"))
  expect_that(ss_b, is_a("numeric"))
  expect_that(ss_c, is_a("numeric"))
  expect_that(ss_d, is_a("numeric"))
  expect_that(ss_e, is_a("numeric"))
  expect_that(ss_f, is_a("numeric"))
  expect_that(ss_g, is_a("numeric"))
  expect_that(ss_h, is_a("numeric"))


}
)
