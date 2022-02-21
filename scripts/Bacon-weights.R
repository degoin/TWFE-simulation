#https://andrewcbaker.netlify.app/2019/09/25/difference-in-differences-methodology/
#trying to calculate the weights from this blog post using the formulas in there
#plus those shown in the Goodman Bacon paper on pages 7-8
#currently unsure how to calculate Vhat_D which is used in the denominator for
#sku on page 8 so this is where I got stuck
  
T <- 100

nk <- nl <- nU <- T
nkU <- nk/(nk + nU)
nlU <- nl/(nl + nU)

nkl <- nk/(nk + nl)
nlk <- nl/(nl + nk)

tk <- 34
tl <- 85

Dk <- (T-tk)/T
Dl <- (T-tl)/T

VkU <- nkU * (1 - nkU) * Dk * (1 - Dk)
VlU <- nlU * (1 - nlU) * Dl * (1 - Dl)
Vkl_Dk <- nkl * (1 - nkl) * ((Dk - Dl)/(1 - Dl)) * ((1 - Dk)/(1 - Dl))
Vkl_Dl <- nkl * (1 - nkl) * (Dl/Dk) * ((Dk - Dl)/Dk)

skU_numerator = ((nk + nU)^2 * VkU)
