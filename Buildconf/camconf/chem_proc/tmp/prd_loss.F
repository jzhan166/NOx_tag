      module mo_prod_loss

      use shr_kind_mod, only : r8 => shr_kind_r8

      use chem_mods, only : veclen
      private
      public :: exp_prod_loss
      public :: imp_prod_loss

      contains

      subroutine exp_prod_loss( ofl, ofu, prod, loss, y, &
                                rxt, het_rates, chnkpnts )

      use chem_mods,    only : gas_pcnst,rxntot,clscnt1

      implicit none

!--------------------------------------------------------------------
!     ... dummy args                                                                      
!--------------------------------------------------------------------
      integer, intent(in) :: ofl, ofu, chnkpnts
      real(r8), dimension(chnkpnts,max(1,clscnt1)), intent(out) :: &
            prod, &
            loss
      real(r8), intent(in)    ::  y(chnkpnts,gas_pcnst)
      real(r8), intent(in)    ::  rxt(chnkpnts,rxntot)
      real(r8), intent(in)    ::  het_rates(chnkpnts,gas_pcnst)


!--------------------------------------------------------------------
!     ... local variables                                                                 
!--------------------------------------------------------------------
      integer :: k

!--------------------------------------------------------------------
!       ... loss and production for Explicit method
!--------------------------------------------------------------------

      do k = ofl,ofu
         loss(k,1) = ( + het_rates(k,232))* y(k,232)
         prod(k,1) = 0._r8
         loss(k,2) = ( + het_rates(k,258))* y(k,258)
         prod(k,2) = 0._r8
         loss(k,3) = ( + het_rates(k,259))* y(k,259)
         prod(k,3) = 0._r8
      end do

      end subroutine exp_prod_loss

      subroutine imp_prod_loss( avec_len, prod, loss, y, &
                                rxt, het_rates )

      use chem_mods,    only : gas_pcnst,rxntot,clscnt4

      implicit none

!--------------------------------------------------------------------
!     ... dummy args                                                                      
!--------------------------------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), dimension(veclen,clscnt4), intent(out) :: &
            prod, &
            loss
      real(r8), intent(in)       ::  y(veclen,gas_pcnst)
      real(r8), intent(in)       ::  rxt(veclen,rxntot)
      real(r8), intent(in)       ::  het_rates(veclen,gas_pcnst)


!--------------------------------------------------------------------
!     ... local variables                                                                 
!--------------------------------------------------------------------
      integer :: k

!--------------------------------------------------------------------
!       ... loss and production for Implicit method
!--------------------------------------------------------------------

      do k = 1,avec_len
         loss(k,192) = (rxt(k,608)* y(k,105) +rxt(k,609)* y(k,151) +rxt(k,650) &
                 * y(k,152) + het_rates(k,1))* y(k,1)
         prod(k,192) =rxt(k,616)*y(k,167)*y(k,27) +rxt(k,657)*y(k,170)
         loss(k,190) = (rxt(k,545)* y(k,167) + rxt(k,34) + het_rates(k,2))* y(k,2)
         prod(k,190) =rxt(k,548)*y(k,151)*y(k,3)
         loss(k,222) = (rxt(k,546)* y(k,105) + (rxt(k,547) +rxt(k,548))* y(k,151) &
                  + het_rates(k,3))* y(k,3)
         prod(k,222) = (rxt(k,551)*y(k,4) +rxt(k,552)*y(k,20))*y(k,167)
         loss(k,189) = (rxt(k,551)* y(k,167) + rxt(k,35) + het_rates(k,4))* y(k,4)
         prod(k,189) =rxt(k,546)*y(k,105)*y(k,3)
         loss(k,1) = ( + het_rates(k,5))* y(k,5)
         prod(k,1) = 0._r8
         loss(k,2) = ( + het_rates(k,6))* y(k,6)
         prod(k,2) = 0._r8
         loss(k,3) = ( + het_rates(k,7))* y(k,7)
         prod(k,3) = 0._r8
         loss(k,225) = (rxt(k,669)* y(k,153) +rxt(k,670)* y(k,163) +rxt(k,671) &
                 * y(k,167) + het_rates(k,8))* y(k,8)
         prod(k,225) = 0._r8
         loss(k,55) = (rxt(k,757)* y(k,105) +rxt(k,758)* y(k,151) + het_rates(k,9)) &
                 * y(k,9)
         prod(k,55) =rxt(k,760)*y(k,167)*y(k,8)
         loss(k,88) = ((rxt(k,611) +rxt(k,761))* y(k,167) + het_rates(k,10))* y(k,10)
         prod(k,88) = 0._r8
         loss(k,179) = (rxt(k,612)* y(k,105) +rxt(k,613)* y(k,151) + het_rates(k,11)) &
                 * y(k,11)
         prod(k,179) = (.350_r8*rxt(k,611)*y(k,10) +rxt(k,615)*y(k,13))*y(k,167)
         loss(k,56) = (rxt(k,762)* y(k,105) +rxt(k,763)* y(k,151) + het_rates(k,12)) &
                 * y(k,12)
         prod(k,56) =rxt(k,761)*y(k,167)*y(k,10)
         loss(k,153) = (rxt(k,615)* y(k,167) + rxt(k,36) + het_rates(k,13))* y(k,13)
         prod(k,153) =rxt(k,612)*y(k,105)*y(k,11)
         loss(k,89) = ( + rxt(k,37) + het_rates(k,14))* y(k,14)
         prod(k,89) =.120_r8*rxt(k,611)*y(k,167)*y(k,10)
         loss(k,152) = ( + rxt(k,38) + het_rates(k,15))* y(k,15)
         prod(k,152) = (.100_r8*rxt(k,670)*y(k,8) +.100_r8*rxt(k,675)*y(k,136)) &
                 *y(k,163)
         loss(k,157) = ( + rxt(k,39) + het_rates(k,16))* y(k,16)
         prod(k,157) = (.500_r8*rxt(k,613)*y(k,11) +.200_r8*rxt(k,653)*y(k,214) + &
                 .060_r8*rxt(k,661)*y(k,252))*y(k,151) +.500_r8*rxt(k,36)*y(k,13) &
                  +rxt(k,37)*y(k,14) +.200_r8*rxt(k,85)*y(k,215) +.060_r8*rxt(k,97) &
                 *y(k,253)
         loss(k,125) = ( + rxt(k,40) + het_rates(k,17))* y(k,17)
         prod(k,125) = (.200_r8*rxt(k,653)*y(k,214) +.200_r8*rxt(k,661)*y(k,252)) &
                 *y(k,151) +.200_r8*rxt(k,85)*y(k,215) +.200_r8*rxt(k,97)*y(k,253)
         loss(k,180) = ( + rxt(k,41) + het_rates(k,18))* y(k,18)
         prod(k,180) = (.200_r8*rxt(k,653)*y(k,214) +.150_r8*rxt(k,661)*y(k,252)) &
                 *y(k,151) +rxt(k,61)*y(k,111) +rxt(k,71)*y(k,143) +.200_r8*rxt(k,85) &
                 *y(k,215) +.150_r8*rxt(k,97)*y(k,253)
         loss(k,137) = ( + rxt(k,42) + het_rates(k,19))* y(k,19)
         prod(k,137) =.210_r8*rxt(k,661)*y(k,252)*y(k,151) +.210_r8*rxt(k,97)*y(k,253)
         loss(k,105) = (rxt(k,552)* y(k,167) + het_rates(k,20))* y(k,20)
         prod(k,105) = (.050_r8*rxt(k,670)*y(k,8) +.050_r8*rxt(k,675)*y(k,136)) &
                 *y(k,163)
         loss(k,161) = (rxt(k,501)* y(k,153) +rxt(k,502)* y(k,167) + het_rates(k,21)) &
                 * y(k,21)
         prod(k,161) = 0._r8
         loss(k,252) = (rxt(k,315)* y(k,51) +rxt(k,316)* y(k,105) +rxt(k,317) &
                 * y(k,163) + het_rates(k,22))* y(k,22)
         prod(k,252) = (rxt(k,100) +2.000_r8*rxt(k,318)*y(k,24) +rxt(k,319)*y(k,70) + &
                 rxt(k,320)*y(k,70) +rxt(k,323)*y(k,151) +rxt(k,327)*y(k,161) + &
                 rxt(k,328)*y(k,167) +rxt(k,714)*y(k,183))*y(k,24) &
                  + (rxt(k,293)*y(k,43) +rxt(k,337)*y(k,44) + &
                 3.000_r8*rxt(k,338)*y(k,66) +2.000_r8*rxt(k,339)*y(k,93) + &
                 rxt(k,340)*y(k,96) +2.000_r8*rxt(k,369)*y(k,50) +rxt(k,370)*y(k,52)) &
                 *y(k,162) + (rxt(k,333)*y(k,96) +2.000_r8*rxt(k,358)*y(k,50) + &
                 rxt(k,360)*y(k,52) +3.000_r8*rxt(k,365)*y(k,66))*y(k,167) &
                  + (2.000_r8*rxt(k,357)*y(k,50) +rxt(k,359)*y(k,52) + &
                 3.000_r8*rxt(k,364)*y(k,66))*y(k,67) + (rxt(k,124) + &
                 rxt(k,332)*y(k,161))*y(k,96) +rxt(k,99)*y(k,23) +rxt(k,102)*y(k,25) &
                  +rxt(k,104)*y(k,43) +rxt(k,105)*y(k,44) +2.000_r8*rxt(k,111)*y(k,50) &
                  +rxt(k,112)*y(k,52) +3.000_r8*rxt(k,115)*y(k,66) &
                  +2.000_r8*rxt(k,123)*y(k,93) +rxt(k,130)*y(k,107)
         loss(k,102) = ( + rxt(k,99) + het_rates(k,23))* y(k,23)
         prod(k,102) = (rxt(k,792)*y(k,107) +rxt(k,797)*y(k,107))*y(k,100) &
                  +rxt(k,321)*y(k,70)*y(k,24)
         loss(k,264) = (2._r8*rxt(k,318)* y(k,24) + (rxt(k,319) +rxt(k,320) + &
                 rxt(k,321))* y(k,70) +rxt(k,322)* y(k,105) +rxt(k,323)* y(k,151) &
                  +rxt(k,324)* y(k,152) +rxt(k,327)* y(k,161) +rxt(k,328)* y(k,167) &
                  +rxt(k,714)* y(k,183) + rxt(k,100) + het_rates(k,24))* y(k,24)
         prod(k,264) = (rxt(k,101) +rxt(k,325)*y(k,161))*y(k,25) +rxt(k,317)*y(k,163) &
                 *y(k,22) +rxt(k,341)*y(k,162)*y(k,96) +rxt(k,335)*y(k,161)*y(k,107)
         loss(k,174) = (rxt(k,325)* y(k,161) + rxt(k,101) + rxt(k,102) + rxt(k,786) &
                  + rxt(k,789) + rxt(k,794) + het_rates(k,25))* y(k,25)
         prod(k,174) =rxt(k,324)*y(k,152)*y(k,24)
         loss(k,4) = ( + het_rates(k,26))* y(k,26)
         prod(k,4) = 0._r8
         loss(k,103) = (rxt(k,616)* y(k,167) + het_rates(k,27))* y(k,27)
         prod(k,103) =rxt(k,619)*y(k,151)*y(k,28) +rxt(k,43)*y(k,29)
         loss(k,175) = (rxt(k,617)* y(k,105) +rxt(k,619)* y(k,151) + het_rates(k,28)) &
                 * y(k,28)
         prod(k,175) = (rxt(k,618)*y(k,29) +.070_r8*rxt(k,656)*y(k,216) + &
                 .060_r8*rxt(k,659)*y(k,251))*y(k,167)
         loss(k,132) = (rxt(k,618)* y(k,167) + rxt(k,43) + het_rates(k,29))* y(k,29)
         prod(k,132) =rxt(k,617)*y(k,105)*y(k,28)
         loss(k,123) = (rxt(k,435)* y(k,67) +rxt(k,436)* y(k,167) + het_rates(k,30)) &
                 * y(k,30)
         prod(k,123) = 0._r8
         loss(k,169) = (rxt(k,437)* y(k,67) +rxt(k,438)* y(k,163) +rxt(k,468) &
                 * y(k,167) + het_rates(k,31))* y(k,31)
         prod(k,169) = 0._r8
         loss(k,213) = (2._r8*rxt(k,440)* y(k,32) +rxt(k,441)* y(k,62) +rxt(k,442) &
                 * y(k,105) +rxt(k,443)* y(k,151) + het_rates(k,32))* y(k,32)
         prod(k,213) = (rxt(k,447)*y(k,67) +rxt(k,448)*y(k,167))*y(k,35) &
                  +.500_r8*rxt(k,446)*y(k,167)*y(k,34) +rxt(k,67)*y(k,132)
         loss(k,112) = (rxt(k,445)* y(k,167) + het_rates(k,33))* y(k,33)
         prod(k,112) = (.400_r8*rxt(k,440)*y(k,32) +.200_r8*rxt(k,441)*y(k,62)) &
                 *y(k,32)
         loss(k,136) = (rxt(k,446)* y(k,167) + rxt(k,44) + het_rates(k,34))* y(k,34)
         prod(k,136) =rxt(k,442)*y(k,105)*y(k,32)
         loss(k,124) = (rxt(k,447)* y(k,67) +rxt(k,448)* y(k,167) + het_rates(k,35)) &
                 * y(k,35)
         prod(k,124) = 0._r8
         loss(k,229) = (rxt(k,474)* y(k,153) +rxt(k,475)* y(k,163) +rxt(k,498) &
                 * y(k,167) + het_rates(k,36))* y(k,36)
         prod(k,229) =.130_r8*rxt(k,582)*y(k,163)*y(k,115) +.700_r8*rxt(k,70)*y(k,138)
         loss(k,209) = (rxt(k,478)* y(k,62) +rxt(k,479)* y(k,105) +rxt(k,480) &
                 * y(k,151) + het_rates(k,37))* y(k,37)
         prod(k,209) = (rxt(k,482)*y(k,38) +rxt(k,483)*y(k,39))*y(k,167)
         loss(k,151) = (rxt(k,482)* y(k,167) + rxt(k,45) + het_rates(k,38))* y(k,38)
         prod(k,151) =rxt(k,479)*y(k,105)*y(k,37)
         loss(k,69) = (rxt(k,483)* y(k,167) + het_rates(k,39))* y(k,39)
         prod(k,69) = 0._r8
         loss(k,197) = (rxt(k,621)* y(k,105) +rxt(k,622)* y(k,151) + het_rates(k,40)) &
                 * y(k,40)
         prod(k,197) = (.400_r8*rxt(k,608)*y(k,105) +rxt(k,609)*y(k,151))*y(k,1) &
                  +rxt(k,624)*y(k,167)*y(k,41) +rxt(k,646)*y(k,171)*y(k,163)
         loss(k,114) = (rxt(k,624)* y(k,167) + rxt(k,46) + het_rates(k,41))* y(k,41)
         prod(k,114) =rxt(k,621)*y(k,105)*y(k,40)
         loss(k,70) = (rxt(k,292)* y(k,162) + rxt(k,103) + het_rates(k,42))* y(k,42)
         prod(k,70) = 0._r8
         loss(k,83) = (rxt(k,293)* y(k,162) + rxt(k,104) + het_rates(k,43))* y(k,43)
         prod(k,83) = 0._r8
         loss(k,84) = (rxt(k,337)* y(k,162) + rxt(k,105) + het_rates(k,44))* y(k,44)
         prod(k,84) = 0._r8
         loss(k,78) = (rxt(k,294)* y(k,162) + rxt(k,106) + het_rates(k,45))* y(k,45)
         prod(k,78) = 0._r8
         loss(k,85) = (rxt(k,295)* y(k,162) + rxt(k,107) + het_rates(k,46))* y(k,46)
         prod(k,85) = 0._r8
         loss(k,79) = (rxt(k,296)* y(k,162) + rxt(k,108) + het_rates(k,47))* y(k,47)
         prod(k,79) = 0._r8
         loss(k,86) = (rxt(k,297)* y(k,162) + rxt(k,109) + het_rates(k,48))* y(k,48)
         prod(k,86) = 0._r8
         loss(k,80) = (rxt(k,298)* y(k,162) + rxt(k,110) + het_rates(k,49))* y(k,49)
         prod(k,80) = 0._r8
         loss(k,166) = (rxt(k,357)* y(k,67) +rxt(k,369)* y(k,162) +rxt(k,358) &
                 * y(k,167) + rxt(k,111) + het_rates(k,50))* y(k,50)
         prod(k,166) = 0._r8
         loss(k,261) = (rxt(k,315)* y(k,22) +rxt(k,260)* y(k,67) +rxt(k,379)* y(k,105) &
                  +rxt(k,380)* y(k,153) +rxt(k,381)* y(k,161) +rxt(k,382)* y(k,167) &
                  + rxt(k,47) + rxt(k,48) + het_rates(k,51))* y(k,51)
         prod(k,261) = (rxt(k,267)*y(k,70) +2.000_r8*rxt(k,385)*y(k,62) + &
                 rxt(k,386)*y(k,62) +rxt(k,388)*y(k,151) +.700_r8*rxt(k,441)*y(k,32) + &
                 rxt(k,454)*y(k,57) +rxt(k,478)*y(k,37) +.800_r8*rxt(k,493)*y(k,179) + &
                 .880_r8*rxt(k,510)*y(k,127) +2.000_r8*rxt(k,523)*y(k,130) + &
                 1.500_r8*rxt(k,557)*y(k,116) +.750_r8*rxt(k,564)*y(k,117) + &
                 .800_r8*rxt(k,575)*y(k,120) +.800_r8*rxt(k,601)*y(k,244) + &
                 .750_r8*rxt(k,679)*y(k,155) +.930_r8*rxt(k,686)*y(k,207) + &
                 .950_r8*rxt(k,692)*y(k,210))*y(k,62) + (.500_r8*rxt(k,461)*y(k,86) + &
                 rxt(k,490)*y(k,175) +rxt(k,495)*y(k,179) + &
                 .500_r8*rxt(k,504)*y(k,84) +.250_r8*rxt(k,513)*y(k,127) + &
                 rxt(k,526)*y(k,130) +.100_r8*rxt(k,547)*y(k,3) + &
                 .920_r8*rxt(k,559)*y(k,116) +.250_r8*rxt(k,603)*y(k,244) + &
                 .340_r8*rxt(k,688)*y(k,207) +.320_r8*rxt(k,694)*y(k,210))*y(k,151) &
                  + (rxt(k,390)*y(k,63) +.300_r8*rxt(k,391)*y(k,64) + &
                 .500_r8*rxt(k,459)*y(k,61) +.800_r8*rxt(k,465)*y(k,89) + &
                 rxt(k,467)*y(k,169) +.500_r8*rxt(k,535)*y(k,135) + &
                 .400_r8*rxt(k,545)*y(k,2) +.300_r8*rxt(k,571)*y(k,118) + &
                 .680_r8*rxt(k,700)*y(k,213))*y(k,167) + (rxt(k,438)*y(k,31) + &
                 .500_r8*rxt(k,475)*y(k,36) +.120_r8*rxt(k,518)*y(k,126) + &
                 .600_r8*rxt(k,536)*y(k,138) +.910_r8*rxt(k,582)*y(k,115) + &
                 .340_r8*rxt(k,670)*y(k,8) +.340_r8*rxt(k,675)*y(k,136))*y(k,163) &
                  + (.250_r8*rxt(k,509)*y(k,127) +rxt(k,522)*y(k,130) + &
                 rxt(k,556)*y(k,116) +.250_r8*rxt(k,600)*y(k,244))*y(k,57) &
                  + (.500_r8*rxt(k,501)*y(k,21) +.250_r8*rxt(k,512)*y(k,127) + &
                 rxt(k,527)*y(k,130) +rxt(k,560)*y(k,116))*y(k,153) &
                  + (.180_r8*rxt(k,55) +rxt(k,401)*y(k,162) +rxt(k,402)*y(k,162)) &
                 *y(k,65) + (.150_r8*rxt(k,494)*y(k,179) +.450_r8*rxt(k,524)*y(k,130)) &
                 *y(k,105) +.100_r8*rxt(k,34)*y(k,2) +.100_r8*rxt(k,35)*y(k,4) &
                  +rxt(k,53)*y(k,64) +2.000_r8*rxt(k,463)*y(k,85) +rxt(k,58)*y(k,89) &
                  +rxt(k,397)*y(k,108) +.330_r8*rxt(k,60)*y(k,110) +rxt(k,62)*y(k,112) &
                  +.690_r8*rxt(k,64)*y(k,123) +1.340_r8*rxt(k,65)*y(k,126) &
                  +2.000_r8*rxt(k,525)*y(k,130)*y(k,130) +rxt(k,72)*y(k,154) &
                  +rxt(k,77)*y(k,178) +rxt(k,78)*y(k,180) +.375_r8*rxt(k,80)*y(k,208) &
                  +.400_r8*rxt(k,82)*y(k,211) +.680_r8*rxt(k,84)*y(k,213)
         loss(k,181) = (rxt(k,359)* y(k,67) +rxt(k,370)* y(k,162) +rxt(k,360) &
                 * y(k,167) + rxt(k,112) + het_rates(k,52))* y(k,52)
         prod(k,181) = 0._r8
         loss(k,71) = (rxt(k,361)* y(k,167) + rxt(k,113) + het_rates(k,53))* y(k,53)
         prod(k,71) = 0._r8
         loss(k,233) = (rxt(k,449)* y(k,153) +rxt(k,450)* y(k,167) + rxt(k,49) &
                  + het_rates(k,54))* y(k,54)
         prod(k,233) = (rxt(k,443)*y(k,32) +.270_r8*rxt(k,480)*y(k,37) + &
                 rxt(k,490)*y(k,175) +rxt(k,504)*y(k,84) +rxt(k,531)*y(k,133) + &
                 .400_r8*rxt(k,547)*y(k,3))*y(k,151) + (rxt(k,445)*y(k,33) + &
                 .500_r8*rxt(k,446)*y(k,34) +.800_r8*rxt(k,545)*y(k,2))*y(k,167) &
                  + (1.600_r8*rxt(k,440)*y(k,32) +.800_r8*rxt(k,441)*y(k,62))*y(k,32) &
                  + (.500_r8*rxt(k,475)*y(k,36) +.100_r8*rxt(k,536)*y(k,138))*y(k,163) &
                  +.400_r8*rxt(k,34)*y(k,2) +.400_r8*rxt(k,35)*y(k,4) &
                  +rxt(k,501)*y(k,153)*y(k,21) +rxt(k,44)*y(k,34) &
                  +.200_r8*rxt(k,530)*y(k,133)*y(k,105) +.330_r8*rxt(k,60)*y(k,110) &
                  +rxt(k,68)*y(k,134) +rxt(k,77)*y(k,178)
         loss(k,143) = (rxt(k,362)* y(k,67) +rxt(k,363)* y(k,167) + rxt(k,114) &
                  + het_rates(k,55))* y(k,55)
         prod(k,143) = 0._r8
         loss(k,66) = (rxt(k,452)* y(k,167) + het_rates(k,56))* y(k,56)
         prod(k,66) = 0._r8
         loss(k,251) = (2._r8*rxt(k,453)* y(k,57) +rxt(k,454)* y(k,62) +rxt(k,455) &
                 * y(k,105) +rxt(k,556)* y(k,116) +rxt(k,563)* y(k,117) +rxt(k,574) &
                 * y(k,120) +rxt(k,509)* y(k,127) +rxt(k,456)* y(k,151) +rxt(k,469) &
                 * y(k,152) +rxt(k,600)* y(k,244) + het_rates(k,57))* y(k,57)
         prod(k,251) = (rxt(k,523)*y(k,62) +.450_r8*rxt(k,524)*y(k,105) + &
                 2.000_r8*rxt(k,525)*y(k,130) +rxt(k,526)*y(k,151) + &
                 rxt(k,527)*y(k,153))*y(k,130) + (.100_r8*rxt(k,518)*y(k,126) + &
                 .280_r8*rxt(k,536)*y(k,138) +.080_r8*rxt(k,582)*y(k,115) + &
                 .060_r8*rxt(k,670)*y(k,8) +.060_r8*rxt(k,675)*y(k,136))*y(k,163) &
                  + (.530_r8*rxt(k,509)*y(k,57) +.260_r8*rxt(k,510)*y(k,62) + &
                 .530_r8*rxt(k,512)*y(k,153) +.530_r8*rxt(k,513)*y(k,151))*y(k,127) &
                  + (rxt(k,450)*y(k,54) +.500_r8*rxt(k,459)*y(k,61) + &
                 rxt(k,485)*y(k,59) +.650_r8*rxt(k,700)*y(k,213))*y(k,167) &
                  + (.300_r8*rxt(k,493)*y(k,62) +.150_r8*rxt(k,494)*y(k,105) + &
                 rxt(k,495)*y(k,151))*y(k,179) + (rxt(k,51) +rxt(k,484)*y(k,153)) &
                 *y(k,59) + (.200_r8*rxt(k,530)*y(k,105) +rxt(k,531)*y(k,151)) &
                 *y(k,133) + (.600_r8*rxt(k,75) +rxt(k,471))*y(k,169) &
                  +.130_r8*rxt(k,38)*y(k,15) +rxt(k,42)*y(k,19) +rxt(k,449)*y(k,153) &
                 *y(k,54) +rxt(k,50)*y(k,58) +.330_r8*rxt(k,60)*y(k,110) +rxt(k,62) &
                 *y(k,112) +1.340_r8*rxt(k,65)*y(k,126) +rxt(k,67)*y(k,132) +rxt(k,68) &
                 *y(k,134) +.300_r8*rxt(k,70)*y(k,138) +rxt(k,72)*y(k,154) +rxt(k,78) &
                 *y(k,180) +.500_r8*rxt(k,79)*y(k,206) +.650_r8*rxt(k,84)*y(k,213)
         loss(k,223) = (rxt(k,499)* y(k,167) + rxt(k,50) + het_rates(k,58))* y(k,58)
         prod(k,223) = (.820_r8*rxt(k,480)*y(k,37) +.500_r8*rxt(k,504)*y(k,84) + &
                 .250_r8*rxt(k,547)*y(k,3) +.270_r8*rxt(k,688)*y(k,207) + &
                 .040_r8*rxt(k,694)*y(k,210))*y(k,151) + (.820_r8*rxt(k,478)*y(k,37) + &
                 .150_r8*rxt(k,686)*y(k,207) +.025_r8*rxt(k,692)*y(k,210))*y(k,62) &
                  + (.250_r8*rxt(k,34) +.800_r8*rxt(k,545)*y(k,167))*y(k,2) &
                  + (.520_r8*rxt(k,670)*y(k,8) +.520_r8*rxt(k,675)*y(k,136))*y(k,163) &
                  + (.500_r8*rxt(k,84) +.500_r8*rxt(k,700)*y(k,167))*y(k,213) &
                  +.250_r8*rxt(k,35)*y(k,4) +.500_r8*rxt(k,501)*y(k,153)*y(k,21) &
                  +.820_r8*rxt(k,45)*y(k,38) +.170_r8*rxt(k,60)*y(k,110) &
                  +.300_r8*rxt(k,80)*y(k,208) +.050_r8*rxt(k,82)*y(k,211)
         loss(k,244) = (rxt(k,484)* y(k,153) +rxt(k,485)* y(k,167) + rxt(k,51) &
                  + het_rates(k,59))* y(k,59)
         prod(k,244) = (.250_r8*rxt(k,513)*y(k,127) +.050_r8*rxt(k,567)*y(k,117) + &
                 .250_r8*rxt(k,603)*y(k,244) +.170_r8*rxt(k,627)*y(k,78) + &
                 .170_r8*rxt(k,637)*y(k,131) +.400_r8*rxt(k,653)*y(k,214) + &
                 .540_r8*rxt(k,661)*y(k,252) +.510_r8*rxt(k,665)*y(k,256))*y(k,151) &
                  + (.500_r8*rxt(k,493)*y(k,179) +.240_r8*rxt(k,510)*y(k,127) + &
                 .100_r8*rxt(k,601)*y(k,244))*y(k,62) + (.250_r8*rxt(k,512)*y(k,127) + &
                 .050_r8*rxt(k,568)*y(k,117) +.250_r8*rxt(k,604)*y(k,244))*y(k,153) &
                  + (.250_r8*rxt(k,509)*y(k,127) +.250_r8*rxt(k,600)*y(k,244))*y(k,57) &
                  + (.070_r8*rxt(k,626)*y(k,78) +.070_r8*rxt(k,636)*y(k,131))*y(k,105) &
                  + (.880_r8*rxt(k,518)*y(k,126) +.500_r8*rxt(k,536)*y(k,138)) &
                 *y(k,163) + (rxt(k,487)*y(k,112) +rxt(k,488)*y(k,154))*y(k,167) &
                  +.180_r8*rxt(k,38)*y(k,15) +rxt(k,42)*y(k,19) +.400_r8*rxt(k,85) &
                 *y(k,215) +.540_r8*rxt(k,97)*y(k,253) +.510_r8*rxt(k,98)*y(k,257)
         loss(k,191) = (rxt(k,458)* y(k,167) + het_rates(k,60))* y(k,60)
         prod(k,191) = (.100_r8*rxt(k,454)*y(k,62) +.150_r8*rxt(k,455)*y(k,105)) &
                 *y(k,57) +.120_r8*rxt(k,475)*y(k,163)*y(k,36) &
                  +.150_r8*rxt(k,524)*y(k,130)*y(k,105)
         loss(k,182) = (rxt(k,459)* y(k,167) + rxt(k,52) + het_rates(k,61))* y(k,61)
         prod(k,182) = (.400_r8*rxt(k,455)*y(k,57) +.400_r8*rxt(k,524)*y(k,130)) &
                 *y(k,105)
         loss(k,256) = (rxt(k,441)* y(k,32) +rxt(k,478)* y(k,37) +rxt(k,454)* y(k,57) &
                  + 2._r8*(rxt(k,385) +rxt(k,386))* y(k,62) +rxt(k,267)* y(k,70) &
                  +rxt(k,387)* y(k,105) +rxt(k,557)* y(k,116) +rxt(k,564)* y(k,117) &
                  +rxt(k,575)* y(k,120) +rxt(k,510)* y(k,127) +rxt(k,523)* y(k,130) &
                  +rxt(k,388)* y(k,151) +rxt(k,679)* y(k,155) +rxt(k,493)* y(k,179) &
                  +rxt(k,686)* y(k,207) +rxt(k,692)* y(k,210) +rxt(k,601)* y(k,244) &
                  + het_rates(k,62))* y(k,62)
         prod(k,256) = (2.000_r8*rxt(k,453)*y(k,57) +.900_r8*rxt(k,454)*y(k,62) + &
                 .450_r8*rxt(k,455)*y(k,105) +rxt(k,456)*y(k,151) + &
                 rxt(k,509)*y(k,127) +rxt(k,522)*y(k,130) +rxt(k,556)*y(k,116) + &
                 rxt(k,563)*y(k,117) +rxt(k,574)*y(k,120) +rxt(k,600)*y(k,244)) &
                 *y(k,57) + (rxt(k,54) +rxt(k,261)*y(k,67) +rxt(k,348)*y(k,88) + &
                 rxt(k,392)*y(k,167) +rxt(k,400)*y(k,162))*y(k,65) &
                  + (.330_r8*rxt(k,626)*y(k,105) +.830_r8*rxt(k,627)*y(k,151))*y(k,78) &
                  + (.070_r8*rxt(k,636)*y(k,105) +.170_r8*rxt(k,637)*y(k,151)) &
                 *y(k,131) + (.280_r8*rxt(k,475)*y(k,36) +.050_r8*rxt(k,582)*y(k,115)) &
                 *y(k,163) + (.700_r8*rxt(k,391)*y(k,64) +rxt(k,458)*y(k,60))*y(k,167) &
                  +rxt(k,112)*y(k,52) +rxt(k,49)*y(k,54) +rxt(k,114)*y(k,55) &
                  +rxt(k,50)*y(k,58) +rxt(k,52)*y(k,61) +.300_r8*rxt(k,70)*y(k,138) &
                  +.400_r8*rxt(k,75)*y(k,169)
         loss(k,201) = (rxt(k,390)* y(k,167) + het_rates(k,63))* y(k,63)
         prod(k,201) = (rxt(k,386)*y(k,62) +.300_r8*rxt(k,441)*y(k,32) + &
                 .500_r8*rxt(k,493)*y(k,179) +.250_r8*rxt(k,510)*y(k,127) + &
                 .250_r8*rxt(k,557)*y(k,116) +.250_r8*rxt(k,564)*y(k,117) + &
                 .200_r8*rxt(k,575)*y(k,120) +.300_r8*rxt(k,601)*y(k,244) + &
                 .250_r8*rxt(k,679)*y(k,155) +.250_r8*rxt(k,686)*y(k,207) + &
                 .250_r8*rxt(k,692)*y(k,210))*y(k,62)
         loss(k,155) = (rxt(k,391)* y(k,167) + rxt(k,53) + het_rates(k,64))* y(k,64)
         prod(k,155) =rxt(k,387)*y(k,105)*y(k,62)
         loss(k,253) = (rxt(k,261)* y(k,67) +rxt(k,348)* y(k,88) + (rxt(k,400) + &
                 rxt(k,401) +rxt(k,402))* y(k,162) +rxt(k,392)* y(k,167) + rxt(k,54) &
                  + rxt(k,55) + het_rates(k,65))* y(k,65)
         prod(k,253) =.100_r8*rxt(k,475)*y(k,163)*y(k,36)
         loss(k,158) = (rxt(k,364)* y(k,67) +rxt(k,338)* y(k,162) +rxt(k,365) &
                 * y(k,167) + rxt(k,115) + het_rates(k,66))* y(k,66)
         prod(k,158) = 0._r8
         loss(k,262) = (rxt(k,447)* y(k,35) +rxt(k,357)* y(k,50) +rxt(k,260)* y(k,51) &
                  +rxt(k,359)* y(k,52) +rxt(k,362)* y(k,55) +rxt(k,261)* y(k,65) &
                  +rxt(k,364)* y(k,66) +rxt(k,273)* y(k,71) +rxt(k,262)* y(k,92) &
                  +rxt(k,263)* y(k,94) + (rxt(k,264) +rxt(k,265))* y(k,105) &
                  +rxt(k,288)* y(k,109) +rxt(k,266)* y(k,163) + het_rates(k,67)) &
                 * y(k,67)
         prod(k,262) = (4.000_r8*rxt(k,292)*y(k,42) +rxt(k,293)*y(k,43) + &
                 2.000_r8*rxt(k,294)*y(k,45) +2.000_r8*rxt(k,295)*y(k,46) + &
                 2.000_r8*rxt(k,296)*y(k,47) +rxt(k,297)*y(k,48) + &
                 2.000_r8*rxt(k,298)*y(k,49) +rxt(k,299)*y(k,100) + &
                 rxt(k,354)*y(k,76) +rxt(k,371)*y(k,97) +rxt(k,372)*y(k,98) + &
                 rxt(k,373)*y(k,99))*y(k,162) + (rxt(k,118) +rxt(k,267)*y(k,62) + &
                 2.000_r8*rxt(k,268)*y(k,70) +rxt(k,270)*y(k,70) + &
                 rxt(k,272)*y(k,151) +rxt(k,278)*y(k,161) +rxt(k,279)*y(k,167) + &
                 rxt(k,320)*y(k,24) +rxt(k,715)*y(k,183))*y(k,70) &
                  + (rxt(k,286)*y(k,100) +3.000_r8*rxt(k,361)*y(k,53) + &
                 rxt(k,363)*y(k,55) +rxt(k,366)*y(k,97) +rxt(k,367)*y(k,98) + &
                 rxt(k,368)*y(k,99))*y(k,167) + (rxt(k,128) +rxt(k,285)*y(k,161)) &
                 *y(k,100) +rxt(k,99)*y(k,23) +4.000_r8*rxt(k,103)*y(k,42) +rxt(k,104) &
                 *y(k,43) +2.000_r8*rxt(k,106)*y(k,45) +2.000_r8*rxt(k,107)*y(k,46) &
                  +2.000_r8*rxt(k,108)*y(k,47) +rxt(k,109)*y(k,48) &
                  +2.000_r8*rxt(k,110)*y(k,49) +3.000_r8*rxt(k,113)*y(k,53) &
                  +rxt(k,114)*y(k,55) +2.000_r8*rxt(k,116)*y(k,68) &
                  +2.000_r8*rxt(k,117)*y(k,69) +rxt(k,119)*y(k,71) +rxt(k,122)*y(k,76) &
                  +rxt(k,125)*y(k,97) +rxt(k,126)*y(k,98) +rxt(k,127)*y(k,99) &
                  +rxt(k,131)*y(k,109)
         loss(k,87) = ( + rxt(k,116) + het_rates(k,68))* y(k,68)
         prod(k,87) = (rxt(k,785)*y(k,109) +rxt(k,790)*y(k,71) +rxt(k,791)*y(k,109) + &
                 rxt(k,795)*y(k,71) +rxt(k,796)*y(k,109) +rxt(k,800)*y(k,71))*y(k,100) &
                  +rxt(k,273)*y(k,71)*y(k,67) +rxt(k,269)*y(k,70)*y(k,70)
         loss(k,65) = ( + rxt(k,117) + rxt(k,302) + het_rates(k,69))* y(k,69)
         prod(k,65) =rxt(k,301)*y(k,70)*y(k,70)
         loss(k,260) = ((rxt(k,319) +rxt(k,320) +rxt(k,321))* y(k,24) +rxt(k,267) &
                 * y(k,62) + 2._r8*(rxt(k,268) +rxt(k,269) +rxt(k,270) +rxt(k,301)) &
                 * y(k,70) +rxt(k,271)* y(k,105) +rxt(k,272)* y(k,151) +rxt(k,274) &
                 * y(k,152) +rxt(k,278)* y(k,161) + (rxt(k,279) +rxt(k,280))* y(k,167) &
                  +rxt(k,715)* y(k,183) + rxt(k,118) + het_rates(k,70))* y(k,70)
         prod(k,260) = (rxt(k,265)*y(k,105) +rxt(k,266)*y(k,163) +rxt(k,288)*y(k,109)) &
                 *y(k,67) + (rxt(k,120) +rxt(k,275)*y(k,161))*y(k,71) &
                  + (rxt(k,289)*y(k,161) +rxt(k,290)*y(k,167))*y(k,109) &
                  + (rxt(k,132) +rxt(k,720)*y(k,183))*y(k,165) +2.000_r8*rxt(k,302) &
                 *y(k,69) +rxt(k,300)*y(k,162)*y(k,100)
         loss(k,212) = (rxt(k,273)* y(k,67) + (rxt(k,790) +rxt(k,795) +rxt(k,800)) &
                 * y(k,100) +rxt(k,275)* y(k,161) +rxt(k,276)* y(k,167) + rxt(k,119) &
                  + rxt(k,120) + rxt(k,788) + rxt(k,793) + rxt(k,799) &
                  + het_rates(k,71))* y(k,71)
         prod(k,212) =rxt(k,274)*y(k,152)*y(k,70)
         loss(k,5) = ( + het_rates(k,72))* y(k,72)
         prod(k,5) = 0._r8
         loss(k,239) = ((rxt(k,393) +rxt(k,404))* y(k,167) + het_rates(k,73))* y(k,73)
         prod(k,239) = (rxt(k,47) +rxt(k,48) +rxt(k,260)*y(k,67) +rxt(k,315)*y(k,22) + &
                 rxt(k,380)*y(k,153) +rxt(k,381)*y(k,161) +rxt(k,382)*y(k,167)) &
                 *y(k,51) + (.630_r8*rxt(k,438)*y(k,31) +.560_r8*rxt(k,475)*y(k,36) + &
                 .650_r8*rxt(k,518)*y(k,126) +.560_r8*rxt(k,536)*y(k,138) + &
                 .620_r8*rxt(k,582)*y(k,115) +.230_r8*rxt(k,670)*y(k,8) + &
                 .230_r8*rxt(k,675)*y(k,136))*y(k,163) &
                  + (.220_r8*rxt(k,513)*y(k,127) +.250_r8*rxt(k,603)*y(k,244) + &
                 .170_r8*rxt(k,627)*y(k,78) +.400_r8*rxt(k,632)*y(k,129) + &
                 .350_r8*rxt(k,637)*y(k,131) +.225_r8*rxt(k,688)*y(k,207))*y(k,151) &
                  + (.350_r8*rxt(k,436)*y(k,30) +rxt(k,466)*y(k,90) + &
                 rxt(k,485)*y(k,59) +.700_r8*rxt(k,700)*y(k,213) +rxt(k,711)*y(k,166)) &
                 *y(k,167) + (.110_r8*rxt(k,510)*y(k,127) + &
                 .200_r8*rxt(k,601)*y(k,244) +.125_r8*rxt(k,686)*y(k,207))*y(k,62) &
                  + (.070_r8*rxt(k,626)*y(k,78) +.160_r8*rxt(k,631)*y(k,129) + &
                 .140_r8*rxt(k,636)*y(k,131))*y(k,105) + (rxt(k,484)*y(k,59) + &
                 .220_r8*rxt(k,512)*y(k,127) +.500_r8*rxt(k,604)*y(k,244))*y(k,153) &
                  + (.220_r8*rxt(k,509)*y(k,127) +.250_r8*rxt(k,600)*y(k,244))*y(k,57) &
                  + (rxt(k,139) +rxt(k,710)*y(k,161))*y(k,166) +1.500_r8*rxt(k,37) &
                 *y(k,14) +.450_r8*rxt(k,38)*y(k,15) +.600_r8*rxt(k,41)*y(k,18) &
                  +rxt(k,42)*y(k,19) +rxt(k,49)*y(k,54) +rxt(k,362)*y(k,67)*y(k,55) &
                  +rxt(k,51)*y(k,59) +.380_r8*rxt(k,55)*y(k,65) +rxt(k,56)*y(k,74) &
                  +rxt(k,58)*y(k,89) +2.000_r8*rxt(k,59)*y(k,90) +.330_r8*rxt(k,60) &
                 *y(k,110) +1.340_r8*rxt(k,66)*y(k,126) +.700_r8*rxt(k,70)*y(k,138) &
                  +1.500_r8*rxt(k,79)*y(k,206) +.250_r8*rxt(k,80)*y(k,208) +rxt(k,83) &
                 *y(k,212) +1.700_r8*rxt(k,84)*y(k,213)
         loss(k,6) = ((rxt(k,405) +rxt(k,406))* y(k,167) + het_rates(k,261))* y(k,261)
         prod(k,6) = 0._r8
         loss(k,7) = ((rxt(k,407) +rxt(k,408))* y(k,167) + het_rates(k,262))* y(k,262)
         prod(k,7) = 0._r8
         loss(k,8) = ((rxt(k,409) +rxt(k,410))* y(k,167) + het_rates(k,263))* y(k,263)
         prod(k,8) = 0._r8
         loss(k,9) = ((rxt(k,411) +rxt(k,412))* y(k,167) + het_rates(k,264))* y(k,264)
         prod(k,9) = 0._r8
         loss(k,10) = ((rxt(k,413) +rxt(k,414))* y(k,167) + het_rates(k,265)) &
                 * y(k,265)
         prod(k,10) = 0._r8
         loss(k,11) = ((rxt(k,415) +rxt(k,416))* y(k,167) + het_rates(k,266)) &
                 * y(k,266)
         prod(k,11) = 0._r8
         loss(k,12) = ((rxt(k,417) +rxt(k,418))* y(k,167) + het_rates(k,267)) &
                 * y(k,267)
         prod(k,12) = 0._r8
         loss(k,13) = ((rxt(k,419) +rxt(k,420))* y(k,167) + het_rates(k,268)) &
                 * y(k,268)
         prod(k,13) = 0._r8
         loss(k,14) = ((rxt(k,421) +rxt(k,422))* y(k,167) + het_rates(k,269)) &
                 * y(k,269)
         prod(k,14) = 0._r8
         loss(k,15) = ((rxt(k,423) +rxt(k,424))* y(k,167) + het_rates(k,270)) &
                 * y(k,270)
         prod(k,15) = 0._r8
         loss(k,16) = ((rxt(k,425) +rxt(k,426))* y(k,167) + het_rates(k,271)) &
                 * y(k,271)
         prod(k,16) = 0._r8
         loss(k,17) = ((rxt(k,427) +rxt(k,428))* y(k,167) + het_rates(k,272)) &
                 * y(k,272)
         prod(k,17) = 0._r8
         loss(k,18) = ((rxt(k,429) +rxt(k,430))* y(k,167) + het_rates(k,273)) &
                 * y(k,273)
         prod(k,18) = 0._r8
         loss(k,207) = ( + rxt(k,56) + het_rates(k,74))* y(k,74)
         prod(k,207) = (rxt(k,393)*y(k,73) +rxt(k,395)*y(k,102) +rxt(k,404)*y(k,73) + &
                 rxt(k,458)*y(k,60) +.500_r8*rxt(k,459)*y(k,61) + &
                 .800_r8*rxt(k,465)*y(k,89) +rxt(k,466)*y(k,90) + &
                 .500_r8*rxt(k,535)*y(k,135) +1.800_r8*rxt(k,700)*y(k,213))*y(k,167) &
                  + (2.000_r8*rxt(k,453)*y(k,57) +.900_r8*rxt(k,454)*y(k,62) + &
                 rxt(k,456)*y(k,151) +2.000_r8*rxt(k,522)*y(k,130) + &
                 rxt(k,556)*y(k,116) +rxt(k,600)*y(k,244))*y(k,57) &
                  + (.200_r8*rxt(k,475)*y(k,36) +.100_r8*rxt(k,536)*y(k,138) + &
                 .270_r8*rxt(k,670)*y(k,8) +.270_r8*rxt(k,675)*y(k,136))*y(k,163) &
                  + (rxt(k,523)*y(k,62) +.450_r8*rxt(k,524)*y(k,105) + &
                 2.000_r8*rxt(k,525)*y(k,130))*y(k,130) &
                  + (.500_r8*rxt(k,686)*y(k,62) +.900_r8*rxt(k,688)*y(k,151))*y(k,207) &
                  +rxt(k,52)*y(k,61) +.440_r8*rxt(k,55)*y(k,65) +.400_r8*rxt(k,75) &
                 *y(k,169) +rxt(k,80)*y(k,208) +.800_r8*rxt(k,84)*y(k,213)
         loss(k,121) = (rxt(k,353)* y(k,162) + rxt(k,121) + het_rates(k,75))* y(k,75)
         prod(k,121) = (rxt(k,293)*y(k,43) +rxt(k,295)*y(k,46) + &
                 2.000_r8*rxt(k,296)*y(k,47) +2.000_r8*rxt(k,297)*y(k,48) + &
                 rxt(k,298)*y(k,49) +rxt(k,337)*y(k,44) +2.000_r8*rxt(k,339)*y(k,93) + &
                 rxt(k,372)*y(k,98) +rxt(k,373)*y(k,99))*y(k,162) + (rxt(k,126) + &
                 rxt(k,367)*y(k,167))*y(k,98) + (rxt(k,127) +rxt(k,368)*y(k,167)) &
                 *y(k,99) +rxt(k,104)*y(k,43) +rxt(k,105)*y(k,44) +rxt(k,107)*y(k,46) &
                  +2.000_r8*rxt(k,108)*y(k,47) +2.000_r8*rxt(k,109)*y(k,48) &
                  +rxt(k,110)*y(k,49) +2.000_r8*rxt(k,123)*y(k,93)
         loss(k,129) = (rxt(k,354)* y(k,162) + rxt(k,122) + het_rates(k,76))* y(k,76)
         prod(k,129) = (rxt(k,125) +rxt(k,366)*y(k,167) +rxt(k,371)*y(k,162))*y(k,97) &
                  + (rxt(k,106) +rxt(k,294)*y(k,162))*y(k,45) + (rxt(k,107) + &
                 rxt(k,295)*y(k,162))*y(k,46)
         loss(k,92) = (rxt(k,625)* y(k,167) + het_rates(k,77))* y(k,77)
         prod(k,92) =.180_r8*rxt(k,656)*y(k,216)*y(k,167)
         loss(k,203) = (rxt(k,626)* y(k,105) +rxt(k,627)* y(k,151) +rxt(k,628) &
                 * y(k,152) + het_rates(k,78))* y(k,78)
         prod(k,203) =.600_r8*rxt(k,40)*y(k,17)
         loss(k,144) = (rxt(k,707)* y(k,153) + (rxt(k,708) +rxt(k,725))* y(k,167) &
                  + het_rates(k,79))* y(k,79)
         prod(k,144) = 0._r8
         loss(k,19) = ( + het_rates(k,80))* y(k,80)
         prod(k,19) = 0._r8
         loss(k,20) = ( + het_rates(k,81))* y(k,81)
         prod(k,20) = 0._r8
         loss(k,21) = ( + het_rates(k,82))* y(k,82)
         prod(k,21) = 0._r8
         loss(k,22) = ( + rxt(k,819) + het_rates(k,83))* y(k,83)
         prod(k,22) = 0._r8
         loss(k,194) = ((rxt(k,504) +rxt(k,505))* y(k,151) + het_rates(k,84))* y(k,84)
         prod(k,194) =rxt(k,502)*y(k,167)*y(k,21)
         loss(k,119) = ( + rxt(k,463) + rxt(k,464) + het_rates(k,85))* y(k,85)
         prod(k,119) =.750_r8*rxt(k,461)*y(k,151)*y(k,86) +rxt(k,57)*y(k,87)
         loss(k,205) = (rxt(k,460)* y(k,105) +rxt(k,461)* y(k,151) + het_rates(k,86)) &
                 * y(k,86)
         prod(k,205) =rxt(k,468)*y(k,167)*y(k,31)
         loss(k,73) = ( + rxt(k,57) + het_rates(k,87))* y(k,87)
         prod(k,73) =rxt(k,460)*y(k,105)*y(k,86)
         loss(k,220) = (rxt(k,348)* y(k,65) +rxt(k,349)* y(k,92) +rxt(k,351)* y(k,104) &
                  +rxt(k,350)* y(k,260) + het_rates(k,88))* y(k,88)
         prod(k,220) = (rxt(k,297)*y(k,48) +rxt(k,337)*y(k,44) + &
                 2.000_r8*rxt(k,353)*y(k,75) +rxt(k,354)*y(k,76))*y(k,162) +rxt(k,105) &
                 *y(k,44) +rxt(k,109)*y(k,48) +2.000_r8*rxt(k,121)*y(k,75) +rxt(k,122) &
                 *y(k,76) +rxt(k,129)*y(k,103)
         loss(k,234) = (rxt(k,465)* y(k,167) + rxt(k,58) + het_rates(k,89))* y(k,89)
         prod(k,234) = (.530_r8*rxt(k,509)*y(k,57) +.260_r8*rxt(k,510)*y(k,62) + &
                 .530_r8*rxt(k,512)*y(k,153) +.530_r8*rxt(k,513)*y(k,151))*y(k,127) &
                  + (.250_r8*rxt(k,600)*y(k,57) +.100_r8*rxt(k,601)*y(k,62) + &
                 .250_r8*rxt(k,603)*y(k,151) +.250_r8*rxt(k,604)*y(k,153))*y(k,244) &
                  + (.700_r8*rxt(k,571)*y(k,118) +.500_r8*rxt(k,572)*y(k,119) + &
                 rxt(k,587)*y(k,142))*y(k,167) + (.050_r8*rxt(k,567)*y(k,151) + &
                 .050_r8*rxt(k,568)*y(k,153))*y(k,117) + (.125_r8*rxt(k,686)*y(k,62) + &
                 .225_r8*rxt(k,688)*y(k,151))*y(k,207) +rxt(k,464)*y(k,85) &
                  +.330_r8*rxt(k,60)*y(k,110) +.250_r8*rxt(k,80)*y(k,208)
         loss(k,227) = (rxt(k,466)* y(k,167) + rxt(k,59) + rxt(k,729) &
                  + het_rates(k,90))* y(k,90)
         prod(k,227) = (.050_r8*rxt(k,567)*y(k,117) +.250_r8*rxt(k,603)*y(k,244) + &
                 rxt(k,613)*y(k,11) +.400_r8*rxt(k,632)*y(k,129) + &
                 .170_r8*rxt(k,637)*y(k,131) +.700_r8*rxt(k,642)*y(k,172) + &
                 .600_r8*rxt(k,653)*y(k,214) +.340_r8*rxt(k,661)*y(k,252) + &
                 .170_r8*rxt(k,665)*y(k,256))*y(k,151) + (.650_r8*rxt(k,436)*y(k,30) + &
                 .200_r8*rxt(k,465)*y(k,89) +rxt(k,588)*y(k,143))*y(k,167) &
                  + (.250_r8*rxt(k,600)*y(k,57) +.100_r8*rxt(k,601)*y(k,62) + &
                 .250_r8*rxt(k,604)*y(k,153))*y(k,244) &
                  + (.160_r8*rxt(k,631)*y(k,129) +.070_r8*rxt(k,636)*y(k,131)) &
                 *y(k,105) +rxt(k,36)*y(k,13) +.130_r8*rxt(k,38)*y(k,15) &
                  +.050_r8*rxt(k,568)*y(k,153)*y(k,117) +.700_r8*rxt(k,76)*y(k,174) &
                  +.600_r8*rxt(k,85)*y(k,215) +.340_r8*rxt(k,97)*y(k,253) &
                  +.170_r8*rxt(k,98)*y(k,257)
         loss(k,254) = ((rxt(k,175) +rxt(k,176) +rxt(k,177))* y(k,105) +rxt(k,183) &
                 * y(k,163) + rxt(k,178) + het_rates(k,91))* y(k,91)
         prod(k,254) = (rxt(k,185)*y(k,92) +rxt(k,188)*y(k,161) +rxt(k,213)*y(k,139) + &
                 rxt(k,382)*y(k,51) +rxt(k,404)*y(k,73) +rxt(k,711)*y(k,166) + &
                 rxt(k,716)*y(k,181) +rxt(k,721)*y(k,183))*y(k,167) &
                  + (rxt(k,153)*y(k,162) +rxt(k,171)*y(k,161) +rxt(k,262)*y(k,67) + &
                 rxt(k,349)*y(k,88))*y(k,92) + (rxt(k,54) +.330_r8*rxt(k,55) + &
                 rxt(k,401)*y(k,162))*y(k,65) + (rxt(k,124) +rxt(k,341)*y(k,162)) &
                 *y(k,96) + (rxt(k,128) +rxt(k,300)*y(k,162))*y(k,100) &
                  + (2.000_r8*rxt(k,2) +rxt(k,3))*y(k,260) +2.000_r8*rxt(k,48)*y(k,51) &
                  +rxt(k,53)*y(k,64) +rxt(k,129)*y(k,103)
         loss(k,245) = (rxt(k,262)* y(k,67) +rxt(k,349)* y(k,88) +rxt(k,171)* y(k,161) &
                  +rxt(k,153)* y(k,162) +rxt(k,185)* y(k,167) + het_rates(k,92)) &
                 * y(k,92)
         prod(k,245) = (1.440_r8*rxt(k,55) +rxt(k,402)*y(k,162))*y(k,65) +rxt(k,47) &
                 *y(k,51) +rxt(k,175)*y(k,105)*y(k,91) +rxt(k,1)*y(k,260)
         loss(k,74) = (rxt(k,339)* y(k,162) + rxt(k,123) + het_rates(k,93))* y(k,93)
         prod(k,74) = 0._r8
         loss(k,183) = (rxt(k,263)* y(k,67) +rxt(k,172)* y(k,161) +rxt(k,186) &
                 * y(k,167) + rxt(k,4) + het_rates(k,94))* y(k,94)
         prod(k,183) = (.500_r8*rxt(k,730) +rxt(k,194)*y(k,105))*y(k,105) &
                  +rxt(k,191)*y(k,167)*y(k,167)
         loss(k,75) = ( + rxt(k,138) + het_rates(k,95))* y(k,95)
         prod(k,75) =rxt(k,727)*y(k,260)*y(k,185)
         loss(k,208) = (rxt(k,332)* y(k,161) + (rxt(k,340) +rxt(k,341))* y(k,162) &
                  +rxt(k,333)* y(k,167) + rxt(k,124) + het_rates(k,96))* y(k,96)
         prod(k,208) = (rxt(k,315)*y(k,51) +rxt(k,316)*y(k,105))*y(k,22)
         loss(k,128) = (rxt(k,371)* y(k,162) +rxt(k,366)* y(k,167) + rxt(k,125) &
                  + het_rates(k,97))* y(k,97)
         prod(k,128) = 0._r8
         loss(k,120) = (rxt(k,372)* y(k,162) +rxt(k,367)* y(k,167) + rxt(k,126) &
                  + het_rates(k,98))* y(k,98)
         prod(k,120) = 0._r8
         loss(k,133) = (rxt(k,373)* y(k,162) +rxt(k,368)* y(k,167) + rxt(k,127) &
                  + het_rates(k,99))* y(k,99)
         prod(k,133) = 0._r8
         loss(k,268) = ((rxt(k,790) +rxt(k,795) +rxt(k,800))* y(k,71) + (rxt(k,792) + &
                 rxt(k,797))* y(k,107) + (rxt(k,785) +rxt(k,791) +rxt(k,796)) &
                 * y(k,109) +rxt(k,285)* y(k,161) + (rxt(k,299) +rxt(k,300))* y(k,162) &
                  +rxt(k,286)* y(k,167) + rxt(k,128) + het_rates(k,100))* y(k,100)
         prod(k,268) = (rxt(k,260)*y(k,51) +rxt(k,261)*y(k,65) +rxt(k,262)*y(k,92) + &
                 rxt(k,263)*y(k,94) +rxt(k,264)*y(k,105) +rxt(k,288)*y(k,109) + &
                 rxt(k,357)*y(k,50) +rxt(k,359)*y(k,52) +2.000_r8*rxt(k,362)*y(k,55) + &
                 rxt(k,364)*y(k,66) +rxt(k,447)*y(k,35))*y(k,67) +rxt(k,280)*y(k,167) &
                 *y(k,70)
         loss(k,100) = (rxt(k,403)* y(k,162) +rxt(k,394)* y(k,167) + het_rates(k,101)) &
                 * y(k,101)
         prod(k,100) = 0._r8
         loss(k,224) = (rxt(k,395)* y(k,167) + het_rates(k,102))* y(k,102)
         prod(k,224) = (.370_r8*rxt(k,438)*y(k,31) +.120_r8*rxt(k,475)*y(k,36) + &
                 .330_r8*rxt(k,518)*y(k,126) +.120_r8*rxt(k,536)*y(k,138) + &
                 .110_r8*rxt(k,582)*y(k,115) +.050_r8*rxt(k,670)*y(k,8) + &
                 .050_r8*rxt(k,675)*y(k,136))*y(k,163) + (rxt(k,396)*y(k,105) + &
                 rxt(k,398)*y(k,151))*y(k,108) +.350_r8*rxt(k,436)*y(k,167)*y(k,30)
         loss(k,126) = ( + rxt(k,129) + het_rates(k,103))* y(k,103)
         prod(k,126) = (rxt(k,348)*y(k,65) +rxt(k,349)*y(k,92) +rxt(k,350)*y(k,260) + &
                 rxt(k,351)*y(k,104))*y(k,88)
         loss(k,255) = (rxt(k,351)* y(k,88) +rxt(k,233)* y(k,167) + rxt(k,11) &
                  + het_rates(k,104))* y(k,104)
         prod(k,255) = (rxt(k,788) +rxt(k,793) +rxt(k,799) +rxt(k,790)*y(k,100) + &
                 rxt(k,795)*y(k,100) +rxt(k,800)*y(k,100))*y(k,71) + (rxt(k,739) + &
                 rxt(k,380)*y(k,51) +rxt(k,449)*y(k,54) +rxt(k,484)*y(k,59) + &
                 rxt(k,707)*y(k,79))*y(k,153) + (2.000_r8*rxt(k,734) + &
                 2.000_r8*rxt(k,784) +2.000_r8*rxt(k,787) +2.000_r8*rxt(k,798)) &
                 *y(k,141) + (rxt(k,786) +rxt(k,789) +rxt(k,794))*y(k,25) &
                  + (.500_r8*rxt(k,738) +rxt(k,227)*y(k,167))*y(k,152) +rxt(k,731) &
                 *y(k,110) +rxt(k,732)*y(k,118) +rxt(k,733)*y(k,119) +rxt(k,735) &
                 *y(k,142) +rxt(k,736)*y(k,143) +rxt(k,740)*y(k,156) +rxt(k,741) &
                 *y(k,168) +rxt(k,742)*y(k,209)
         loss(k,258) = (rxt(k,608)* y(k,1) +rxt(k,546)* y(k,3) +rxt(k,612)* y(k,11) &
                  +rxt(k,316)* y(k,22) +rxt(k,322)* y(k,24) +rxt(k,617)* y(k,28) &
                  +rxt(k,442)* y(k,32) +rxt(k,479)* y(k,37) +rxt(k,621)* y(k,40) &
                  +rxt(k,379)* y(k,51) +rxt(k,455)* y(k,57) +rxt(k,387)* y(k,62) &
                  + (rxt(k,264) +rxt(k,265))* y(k,67) +rxt(k,271)* y(k,70) +rxt(k,626) &
                 * y(k,78) +rxt(k,460)* y(k,86) + (rxt(k,175) +rxt(k,176) +rxt(k,177)) &
                 * y(k,91) + 2._r8*rxt(k,194)* y(k,105) +rxt(k,396)* y(k,108) &
                  +rxt(k,558)* y(k,116) +rxt(k,565)* y(k,117) +rxt(k,576)* y(k,120) &
                  +rxt(k,511)* y(k,127) +rxt(k,631)* y(k,129) +rxt(k,524)* y(k,130) &
                  +rxt(k,636)* y(k,131) +rxt(k,530)* y(k,133) +rxt(k,214)* y(k,151) &
                  +rxt(k,225)* y(k,152) +rxt(k,207)* y(k,153) +rxt(k,680)* y(k,155) &
                  +rxt(k,179)* y(k,161) +rxt(k,180)* y(k,163) +rxt(k,187)* y(k,167) &
                  +rxt(k,641)* y(k,172) +rxt(k,489)* y(k,175) +rxt(k,494)* y(k,179) &
                  +rxt(k,687)* y(k,207) +rxt(k,693)* y(k,210) +rxt(k,652)* y(k,214) &
                  +rxt(k,602)* y(k,244) +rxt(k,660)* y(k,252) +rxt(k,664)* y(k,256) &
                  + rxt(k,730) + het_rates(k,105))* y(k,105)
         prod(k,258) = (rxt(k,186)*y(k,94) +rxt(k,189)*y(k,163) +rxt(k,210)*y(k,153) + &
                 rxt(k,279)*y(k,70) +rxt(k,328)*y(k,24) +rxt(k,360)*y(k,52) + &
                 rxt(k,363)*y(k,55) +rxt(k,390)*y(k,63) +rxt(k,393)*y(k,73) + &
                 rxt(k,394)*y(k,101) +rxt(k,395)*y(k,102) + &
                 .350_r8*rxt(k,436)*y(k,30) +rxt(k,445)*y(k,33) +rxt(k,452)*y(k,56) + &
                 rxt(k,465)*y(k,89) +rxt(k,466)*y(k,90) +rxt(k,487)*y(k,112) + &
                 rxt(k,508)*y(k,110) +.200_r8*rxt(k,520)*y(k,128) + &
                 .500_r8*rxt(k,535)*y(k,135) +.300_r8*rxt(k,571)*y(k,118) + &
                 rxt(k,572)*y(k,119) +rxt(k,581)*y(k,121) +rxt(k,587)*y(k,142) + &
                 rxt(k,588)*y(k,143) +.650_r8*rxt(k,611)*y(k,10) + &
                 .730_r8*rxt(k,625)*y(k,77) +.800_r8*rxt(k,644)*y(k,173) + &
                 .280_r8*rxt(k,656)*y(k,216) +.380_r8*rxt(k,659)*y(k,251) + &
                 .630_r8*rxt(k,667)*y(k,255) +.200_r8*rxt(k,700)*y(k,213) + &
                 .500_r8*rxt(k,725)*y(k,79) +rxt(k,726)*y(k,184))*y(k,167) &
                  + (rxt(k,388)*y(k,62) +rxt(k,398)*y(k,108) +rxt(k,443)*y(k,32) + &
                 .250_r8*rxt(k,461)*y(k,86) +rxt(k,480)*y(k,37) +rxt(k,490)*y(k,175) + &
                 rxt(k,504)*y(k,84) +.470_r8*rxt(k,513)*y(k,127) +rxt(k,547)*y(k,3) + &
                 .920_r8*rxt(k,559)*y(k,116) +.920_r8*rxt(k,567)*y(k,117) + &
                 rxt(k,577)*y(k,120) +rxt(k,603)*y(k,244) +rxt(k,613)*y(k,11) + &
                 rxt(k,619)*y(k,28) +.170_r8*rxt(k,627)*y(k,78) + &
                 .400_r8*rxt(k,632)*y(k,129) +.830_r8*rxt(k,637)*y(k,131) + &
                 rxt(k,642)*y(k,172) +rxt(k,653)*y(k,214) +rxt(k,661)*y(k,252) + &
                 rxt(k,665)*y(k,256) +.900_r8*rxt(k,688)*y(k,207) + &
                 .800_r8*rxt(k,694)*y(k,210))*y(k,151) + (rxt(k,267)*y(k,70) + &
                 2.000_r8*rxt(k,385)*y(k,62) +rxt(k,441)*y(k,32) + &
                 .900_r8*rxt(k,454)*y(k,57) +rxt(k,478)*y(k,37) + &
                 .300_r8*rxt(k,493)*y(k,179) +.730_r8*rxt(k,510)*y(k,127) + &
                 rxt(k,523)*y(k,130) +rxt(k,557)*y(k,116) +rxt(k,564)*y(k,117) + &
                 1.200_r8*rxt(k,575)*y(k,120) +.800_r8*rxt(k,601)*y(k,244) + &
                 .500_r8*rxt(k,679)*y(k,155) +rxt(k,686)*y(k,207) + &
                 rxt(k,692)*y(k,210))*y(k,62) + (.130_r8*rxt(k,438)*y(k,31) + &
                 .280_r8*rxt(k,475)*y(k,36) +.140_r8*rxt(k,518)*y(k,126) + &
                 .280_r8*rxt(k,536)*y(k,138) +.370_r8*rxt(k,582)*y(k,115) + &
                 .570_r8*rxt(k,670)*y(k,8) +.570_r8*rxt(k,675)*y(k,136))*y(k,163) &
                  + (rxt(k,380)*y(k,51) +.470_r8*rxt(k,512)*y(k,127) + &
                 rxt(k,560)*y(k,116) +rxt(k,568)*y(k,117) +rxt(k,578)*y(k,120) + &
                 rxt(k,604)*y(k,244))*y(k,153) + (.470_r8*rxt(k,509)*y(k,127) + &
                 rxt(k,556)*y(k,116) +rxt(k,563)*y(k,117) +rxt(k,574)*y(k,120) + &
                 rxt(k,600)*y(k,244))*y(k,57) + (rxt(k,260)*y(k,51) + &
                 rxt(k,263)*y(k,94) +rxt(k,359)*y(k,52) +rxt(k,362)*y(k,55))*y(k,67) &
                  + (.070_r8*rxt(k,626)*y(k,78) +.160_r8*rxt(k,631)*y(k,129) + &
                 .330_r8*rxt(k,636)*y(k,131))*y(k,105) + (rxt(k,315)*y(k,22) + &
                 rxt(k,381)*y(k,161))*y(k,51) + (rxt(k,463) +rxt(k,464))*y(k,85) &
                  + (rxt(k,12) +rxt(k,234))*y(k,106) + (1.340_r8*rxt(k,65) + &
                 .660_r8*rxt(k,66))*y(k,126) +rxt(k,34)*y(k,2) +.900_r8*rxt(k,35) &
                 *y(k,4) +rxt(k,36)*y(k,13) +1.500_r8*rxt(k,37)*y(k,14) &
                  +.560_r8*rxt(k,38)*y(k,15) +rxt(k,39)*y(k,16) +.600_r8*rxt(k,40) &
                 *y(k,17) +.600_r8*rxt(k,41)*y(k,18) +rxt(k,42)*y(k,19) +rxt(k,43) &
                 *y(k,29) +1.200_r8*rxt(k,440)*y(k,32)*y(k,32) +rxt(k,44)*y(k,34) &
                  +rxt(k,45)*y(k,38) +rxt(k,49)*y(k,54) +rxt(k,51)*y(k,59) &
                  +rxt(k,401)*y(k,162)*y(k,65) +2.000_r8*rxt(k,58)*y(k,89) &
                  +2.000_r8*rxt(k,59)*y(k,90) +rxt(k,178)*y(k,91) +rxt(k,172)*y(k,161) &
                 *y(k,94) +rxt(k,397)*y(k,108) +.670_r8*rxt(k,60)*y(k,110) +rxt(k,61) &
                 *y(k,111) +rxt(k,62)*y(k,112) +rxt(k,566)*y(k,117) +rxt(k,63) &
                 *y(k,121) +rxt(k,64)*y(k,123) +rxt(k,71)*y(k,143) +rxt(k,76)*y(k,174) &
                  +rxt(k,77)*y(k,178) +rxt(k,79)*y(k,206) +rxt(k,80)*y(k,208) &
                  +rxt(k,81)*y(k,209) +rxt(k,82)*y(k,211) +rxt(k,83)*y(k,212) &
                  +1.200_r8*rxt(k,84)*y(k,213) +rxt(k,85)*y(k,215) +rxt(k,97)*y(k,253) &
                  +rxt(k,98)*y(k,257)
         loss(k,154) = (rxt(k,195)* y(k,167) + rxt(k,12) + rxt(k,13) + rxt(k,234) &
                  + het_rates(k,106))* y(k,106)
         prod(k,154) =rxt(k,225)*y(k,152)*y(k,105)
         loss(k,198) = ((rxt(k,792) +rxt(k,797))* y(k,100) +rxt(k,335)* y(k,161) &
                  + rxt(k,130) + het_rates(k,107))* y(k,107)
         prod(k,198) = (rxt(k,786) +rxt(k,789) +rxt(k,794))*y(k,25) &
                  +rxt(k,322)*y(k,105)*y(k,24)
         loss(k,173) = (rxt(k,396)* y(k,105) +rxt(k,398)* y(k,151) + rxt(k,397) &
                  + het_rates(k,108))* y(k,108)
         prod(k,173) =rxt(k,379)*y(k,105)*y(k,51)
         loss(k,206) = (rxt(k,288)* y(k,67) + (rxt(k,785) +rxt(k,791) +rxt(k,796)) &
                 * y(k,100) +rxt(k,289)* y(k,161) +rxt(k,290)* y(k,167) + rxt(k,131) &
                  + het_rates(k,109))* y(k,109)
         prod(k,206) = (rxt(k,788) +rxt(k,793) +rxt(k,799) +rxt(k,276)*y(k,167)) &
                 *y(k,71) +rxt(k,271)*y(k,105)*y(k,70)
         loss(k,238) = (rxt(k,508)* y(k,167) + rxt(k,60) + rxt(k,731) &
                  + het_rates(k,110))* y(k,110)
         prod(k,238) = (rxt(k,505)*y(k,84) +rxt(k,514)*y(k,127))*y(k,151) &
                  + (.300_r8*rxt(k,571)*y(k,118) +.500_r8*rxt(k,572)*y(k,119)) &
                 *y(k,167)
         loss(k,101) = (rxt(k,553)* y(k,167) + rxt(k,61) + het_rates(k,111))* y(k,111)
         prod(k,101) =rxt(k,566)*y(k,117)
         loss(k,237) = (rxt(k,487)* y(k,167) + rxt(k,62) + het_rates(k,112))* y(k,112)
         prod(k,237) = (.220_r8*rxt(k,509)*y(k,57) +.230_r8*rxt(k,510)*y(k,62) + &
                 .220_r8*rxt(k,512)*y(k,153) +.220_r8*rxt(k,513)*y(k,151))*y(k,127) &
                  + (.500_r8*rxt(k,492)*y(k,178) +.500_r8*rxt(k,535)*y(k,135) + &
                 .700_r8*rxt(k,571)*y(k,118) +.500_r8*rxt(k,572)*y(k,119))*y(k,167) &
                  + (.250_r8*rxt(k,600)*y(k,57) +.100_r8*rxt(k,601)*y(k,62) + &
                 .250_r8*rxt(k,603)*y(k,151) +.250_r8*rxt(k,604)*y(k,153))*y(k,244) &
                  + (.050_r8*rxt(k,567)*y(k,151) +.050_r8*rxt(k,568)*y(k,153)) &
                 *y(k,117) +.200_r8*rxt(k,493)*y(k,179)*y(k,62) +.170_r8*rxt(k,60) &
                 *y(k,110)
         loss(k,139) = (rxt(k,554)* y(k,167) + het_rates(k,113))* y(k,113)
         prod(k,139) = (rxt(k,563)*y(k,57) +.750_r8*rxt(k,564)*y(k,62) + &
                 .870_r8*rxt(k,567)*y(k,151) +.950_r8*rxt(k,568)*y(k,153))*y(k,117)
         loss(k,76) = (rxt(k,555)* y(k,167) + het_rates(k,114))* y(k,114)
         prod(k,76) =.600_r8*rxt(k,584)*y(k,167)*y(k,123)
         loss(k,214) = (rxt(k,573)* y(k,153) +rxt(k,582)* y(k,163) +rxt(k,583) &
                 * y(k,167) + het_rates(k,115))* y(k,115)
         prod(k,214) = 0._r8
         loss(k,246) = (rxt(k,556)* y(k,57) +rxt(k,557)* y(k,62) +rxt(k,558)* y(k,105) &
                  +rxt(k,559)* y(k,151) +rxt(k,560)* y(k,153) + het_rates(k,116)) &
                 * y(k,116)
         prod(k,246) =.600_r8*rxt(k,583)*y(k,167)*y(k,115)
         loss(k,247) = (rxt(k,563)* y(k,57) +rxt(k,564)* y(k,62) +rxt(k,565)* y(k,105) &
                  +rxt(k,567)* y(k,151) +rxt(k,568)* y(k,153) + rxt(k,566) &
                  + het_rates(k,117))* y(k,117)
         prod(k,247) =.400_r8*rxt(k,583)*y(k,167)*y(k,115)
         loss(k,184) = (rxt(k,571)* y(k,167) + rxt(k,732) + het_rates(k,118)) &
                 * y(k,118)
         prod(k,184) =.080_r8*rxt(k,559)*y(k,151)*y(k,116)
         loss(k,168) = (rxt(k,572)* y(k,167) + rxt(k,733) + het_rates(k,119)) &
                 * y(k,119)
         prod(k,168) =.080_r8*rxt(k,567)*y(k,151)*y(k,117)
         loss(k,242) = (rxt(k,574)* y(k,57) +rxt(k,575)* y(k,62) +rxt(k,576)* y(k,105) &
                  +rxt(k,577)* y(k,151) +rxt(k,578)* y(k,153) + het_rates(k,120)) &
                 * y(k,120)
         prod(k,242) =rxt(k,573)*y(k,153)*y(k,115)
         loss(k,149) = (rxt(k,581)* y(k,167) + rxt(k,63) + het_rates(k,121))* y(k,121)
         prod(k,149) =rxt(k,576)*y(k,120)*y(k,105)
         loss(k,57) = (rxt(k,765)* y(k,105) +rxt(k,766)* y(k,151) + het_rates(k,122)) &
                 * y(k,122)
         prod(k,57) =rxt(k,768)*y(k,167)*y(k,115)
         loss(k,193) = (rxt(k,584)* y(k,167) + rxt(k,64) + het_rates(k,123))* y(k,123)
         prod(k,193) = (rxt(k,558)*y(k,116) +rxt(k,565)*y(k,117))*y(k,105) +rxt(k,63) &
                 *y(k,121)
         loss(k,59) = (rxt(k,771)* y(k,167) + het_rates(k,124))* y(k,124)
         prod(k,59) = 0._r8
         loss(k,58) = (rxt(k,769)* y(k,105) +rxt(k,770)* y(k,151) + het_rates(k,125)) &
                 * y(k,125)
         prod(k,58) =rxt(k,771)*y(k,167)*y(k,124)
         loss(k,243) = (rxt(k,518)* y(k,163) +rxt(k,519)* y(k,167) + rxt(k,65) &
                  + rxt(k,66) + het_rates(k,126))* y(k,126)
         prod(k,243) = (.390_r8*rxt(k,556)*y(k,57) +.310_r8*rxt(k,557)*y(k,62) + &
                 .360_r8*rxt(k,559)*y(k,151) +.400_r8*rxt(k,560)*y(k,153))*y(k,116) &
                  +.300_r8*rxt(k,582)*y(k,163)*y(k,115) +.288_r8*rxt(k,64)*y(k,123)
         loss(k,248) = (rxt(k,509)* y(k,57) +rxt(k,510)* y(k,62) +rxt(k,511)* y(k,105) &
                  + (rxt(k,513) +rxt(k,514))* y(k,151) +rxt(k,512)* y(k,153) &
                  + het_rates(k,127))* y(k,127)
         prod(k,248) = (.500_r8*rxt(k,519)*y(k,126) +.200_r8*rxt(k,520)*y(k,128) + &
                 rxt(k,537)*y(k,138))*y(k,167)
         loss(k,130) = (rxt(k,520)* y(k,167) + het_rates(k,128))* y(k,128)
         prod(k,130) =rxt(k,511)*y(k,127)*y(k,105)
         loss(k,200) = (rxt(k,631)* y(k,105) +rxt(k,632)* y(k,151) +rxt(k,633) &
                 * y(k,152) + het_rates(k,129))* y(k,129)
         prod(k,200) =.600_r8*rxt(k,39)*y(k,16)
         loss(k,250) = (rxt(k,522)* y(k,57) +rxt(k,523)* y(k,62) +rxt(k,524)* y(k,105) &
                  + 2._r8*rxt(k,525)* y(k,130) +rxt(k,526)* y(k,151) +rxt(k,539) &
                 * y(k,152) +rxt(k,527)* y(k,153) + het_rates(k,130))* y(k,130)
         prod(k,250) = (.660_r8*rxt(k,65) +.500_r8*rxt(k,519)*y(k,167))*y(k,126) &
                  + (rxt(k,69) +rxt(k,541))*y(k,135) +.500_r8*rxt(k,520)*y(k,167) &
                 *y(k,128)
         loss(k,219) = (rxt(k,636)* y(k,105) +rxt(k,637)* y(k,151) +rxt(k,638) &
                 * y(k,152) + het_rates(k,131))* y(k,131)
         prod(k,219) =.600_r8*rxt(k,41)*y(k,18)
         loss(k,163) = (rxt(k,533)* y(k,167) + rxt(k,67) + het_rates(k,132))* y(k,132)
         prod(k,163) =.800_r8*rxt(k,34)*y(k,2) +.800_r8*rxt(k,547)*y(k,151)*y(k,3) &
                  +.800_r8*rxt(k,35)*y(k,4)
         loss(k,196) = (rxt(k,530)* y(k,105) +rxt(k,531)* y(k,151) + het_rates(k,133)) &
                 * y(k,133)
         prod(k,196) = (rxt(k,533)*y(k,132) +rxt(k,534)*y(k,134))*y(k,167)
         loss(k,131) = (rxt(k,534)* y(k,167) + rxt(k,68) + het_rates(k,134))* y(k,134)
         prod(k,131) =.800_r8*rxt(k,530)*y(k,133)*y(k,105)
         loss(k,177) = (rxt(k,535)* y(k,167) + rxt(k,69) + rxt(k,541) &
                  + het_rates(k,135))* y(k,135)
         prod(k,177) =rxt(k,539)*y(k,152)*y(k,130)
         loss(k,228) = (rxt(k,674)* y(k,153) +rxt(k,675)* y(k,163) +rxt(k,676) &
                 * y(k,167) + het_rates(k,136))* y(k,136)
         prod(k,228) = 0._r8
         loss(k,60) = (rxt(k,773)* y(k,105) +rxt(k,774)* y(k,151) + het_rates(k,137)) &
                 * y(k,137)
         prod(k,60) =rxt(k,776)*y(k,167)*y(k,136)
         loss(k,249) = (rxt(k,536)* y(k,163) +rxt(k,537)* y(k,167) + rxt(k,70) &
                  + het_rates(k,138))* y(k,138)
         prod(k,249) = (.610_r8*rxt(k,556)*y(k,57) +.440_r8*rxt(k,557)*y(k,62) + &
                 .560_r8*rxt(k,559)*y(k,151) +.600_r8*rxt(k,560)*y(k,153))*y(k,116) &
                  +.200_r8*rxt(k,582)*y(k,163)*y(k,115) +.402_r8*rxt(k,64)*y(k,123)
         loss(k,178) = (rxt(k,196)* y(k,151) + (rxt(k,197) +rxt(k,198) +rxt(k,199)) &
                 * y(k,152) +rxt(k,213)* y(k,167) + rxt(k,200) + het_rates(k,139)) &
                 * y(k,139)
         prod(k,178) =rxt(k,17)*y(k,151)
         loss(k,111) = ((rxt(k,223) +rxt(k,224))* y(k,162) + rxt(k,14) &
                  + het_rates(k,140))* y(k,140)
         prod(k,111) =rxt(k,197)*y(k,152)*y(k,139)
         loss(k,122) = ( + rxt(k,15) + rxt(k,16) + rxt(k,235) + rxt(k,734) &
                  + rxt(k,784) + rxt(k,787) + rxt(k,798) + het_rates(k,141))* y(k,141)
         prod(k,122) =rxt(k,226)*y(k,153)*y(k,152)
         loss(k,147) = (rxt(k,587)* y(k,167) + rxt(k,735) + het_rates(k,142)) &
                 * y(k,142)
         prod(k,147) =.200_r8*rxt(k,575)*y(k,120)*y(k,62)
         loss(k,232) = (rxt(k,588)* y(k,167) + rxt(k,71) + rxt(k,736) &
                  + het_rates(k,143))* y(k,143)
         prod(k,232) = (rxt(k,574)*y(k,57) +.800_r8*rxt(k,575)*y(k,62) + &
                 rxt(k,577)*y(k,151) +rxt(k,578)*y(k,153))*y(k,120)
         loss(k,23) = ( + het_rates(k,144))* y(k,144)
         prod(k,23) = 0._r8
         loss(k,24) = ( + het_rates(k,145))* y(k,145)
         prod(k,24) = 0._r8
         loss(k,25) = ( + het_rates(k,146))* y(k,146)
         prod(k,25) = 0._r8
         loss(k,67) = (rxt(k,728)* y(k,167) + het_rates(k,147))* y(k,147)
         prod(k,67) = 0._r8
         loss(k,26) = ( + rxt(k,737) + het_rates(k,148))* y(k,148)
         prod(k,26) = 0._r8
         loss(k,27) = ( + rxt(k,821) + het_rates(k,149))* y(k,149)
         prod(k,27) = 0._r8
         loss(k,28) = ( + rxt(k,820) + het_rates(k,150))* y(k,150)
         prod(k,28) = 0._r8
         loss(k,257) = (rxt(k,609)* y(k,1) + (rxt(k,547) +rxt(k,548))* y(k,3) &
                  +rxt(k,613)* y(k,11) +rxt(k,323)* y(k,24) +rxt(k,619)* y(k,28) &
                  +rxt(k,443)* y(k,32) +rxt(k,480)* y(k,37) +rxt(k,622)* y(k,40) &
                  +rxt(k,456)* y(k,57) +rxt(k,388)* y(k,62) +rxt(k,272)* y(k,70) &
                  +rxt(k,627)* y(k,78) + (rxt(k,504) +rxt(k,505))* y(k,84) +rxt(k,461) &
                 * y(k,86) +rxt(k,214)* y(k,105) +rxt(k,398)* y(k,108) +rxt(k,559) &
                 * y(k,116) +rxt(k,567)* y(k,117) +rxt(k,577)* y(k,120) &
                  + (rxt(k,513) +rxt(k,514))* y(k,127) +rxt(k,632)* y(k,129) &
                  +rxt(k,526)* y(k,130) +rxt(k,637)* y(k,131) +rxt(k,531)* y(k,133) &
                  +rxt(k,196)* y(k,139) +rxt(k,208)* y(k,153) +rxt(k,681)* y(k,155) &
                  +rxt(k,216)* y(k,161) +rxt(k,215)* y(k,163) +rxt(k,642)* y(k,172) &
                  +rxt(k,490)* y(k,175) +rxt(k,495)* y(k,179) +rxt(k,688)* y(k,207) &
                  +rxt(k,694)* y(k,210) +rxt(k,653)* y(k,214) +rxt(k,603)* y(k,244) &
                  +rxt(k,661)* y(k,252) +rxt(k,665)* y(k,256) + rxt(k,17) &
                  + het_rates(k,151))* y(k,151)
         prod(k,257) = (rxt(k,18) +.500_r8*rxt(k,738) +2.000_r8*rxt(k,198)*y(k,139) + &
                 rxt(k,201)*y(k,161) +rxt(k,717)*y(k,183))*y(k,152) + (rxt(k,200) + &
                 rxt(k,213)*y(k,167))*y(k,139) +2.000_r8*rxt(k,223)*y(k,162)*y(k,140) &
                  +rxt(k,15)*y(k,141) +rxt(k,19)*y(k,153)
         loss(k,267) = (rxt(k,650)* y(k,1) +rxt(k,324)* y(k,24) +rxt(k,469)* y(k,57) &
                  +rxt(k,274)* y(k,70) +rxt(k,628)* y(k,78) +rxt(k,225)* y(k,105) &
                  +rxt(k,633)* y(k,129) +rxt(k,539)* y(k,130) +rxt(k,638)* y(k,131) &
                  + (rxt(k,197) +rxt(k,198) +rxt(k,199))* y(k,139) +rxt(k,226) &
                 * y(k,153) + (rxt(k,201) +rxt(k,203))* y(k,161) +rxt(k,202)* y(k,163) &
                  +rxt(k,227)* y(k,167) +rxt(k,645)* y(k,171) +rxt(k,717)* y(k,183) &
                  + rxt(k,18) + rxt(k,738) + het_rates(k,152))* y(k,152)
         prod(k,267) = (2.000_r8*rxt(k,208)*y(k,153) +rxt(k,214)*y(k,105) + &
                 rxt(k,215)*y(k,163) +rxt(k,216)*y(k,161) +rxt(k,272)*y(k,70) + &
                 rxt(k,323)*y(k,24) +rxt(k,388)*y(k,62) +rxt(k,398)*y(k,108) + &
                 rxt(k,443)*y(k,32) +rxt(k,456)*y(k,57) +rxt(k,461)*y(k,86) + &
                 rxt(k,480)*y(k,37) +rxt(k,490)*y(k,175) +rxt(k,495)*y(k,179) + &
                 rxt(k,504)*y(k,84) +rxt(k,513)*y(k,127) +rxt(k,526)*y(k,130) + &
                 rxt(k,531)*y(k,133) +rxt(k,547)*y(k,3) +.920_r8*rxt(k,559)*y(k,116) + &
                 .920_r8*rxt(k,567)*y(k,117) +rxt(k,577)*y(k,120) + &
                 rxt(k,603)*y(k,244) +rxt(k,609)*y(k,1) +rxt(k,613)*y(k,11) + &
                 rxt(k,619)*y(k,28) +rxt(k,622)*y(k,40) +rxt(k,627)*y(k,78) + &
                 rxt(k,632)*y(k,129) +rxt(k,637)*y(k,131) +rxt(k,642)*y(k,172) + &
                 rxt(k,653)*y(k,214) +rxt(k,661)*y(k,252) +rxt(k,665)*y(k,256) + &
                 1.600_r8*rxt(k,681)*y(k,155) +.900_r8*rxt(k,688)*y(k,207) + &
                 .800_r8*rxt(k,694)*y(k,210))*y(k,151) + (rxt(k,20) + &
                 rxt(k,207)*y(k,105) +rxt(k,209)*y(k,161) +rxt(k,210)*y(k,167) + &
                 rxt(k,501)*y(k,21) +rxt(k,512)*y(k,127) +rxt(k,527)*y(k,130) + &
                 rxt(k,560)*y(k,116) +rxt(k,568)*y(k,117) +rxt(k,578)*y(k,120) + &
                 rxt(k,604)*y(k,244) +2.000_r8*rxt(k,682)*y(k,155))*y(k,153) &
                  + (rxt(k,195)*y(k,106) +rxt(k,488)*y(k,154) +rxt(k,545)*y(k,2) + &
                 .700_r8*rxt(k,571)*y(k,118) +rxt(k,691)*y(k,209))*y(k,167) &
                  + (rxt(k,12) +rxt(k,234))*y(k,106) + (rxt(k,69) +rxt(k,541)) &
                 *y(k,135) + (rxt(k,16) +rxt(k,235))*y(k,141) + (.600_r8*rxt(k,75) + &
                 rxt(k,471))*y(k,169) +rxt(k,34)*y(k,2) +rxt(k,101)*y(k,25) &
                  +.500_r8*rxt(k,679)*y(k,155)*y(k,62) +rxt(k,120)*y(k,71) +rxt(k,11) &
                 *y(k,104) +rxt(k,60)*y(k,110) +rxt(k,63)*y(k,121) +rxt(k,71)*y(k,143) &
                  +rxt(k,72)*y(k,154) +rxt(k,73)*y(k,156) +rxt(k,74)*y(k,168) &
                  +rxt(k,657)*y(k,170) +rxt(k,81)*y(k,209)
         loss(k,259) = (rxt(k,669)* y(k,8) +rxt(k,501)* y(k,21) +rxt(k,474)* y(k,36) &
                  +rxt(k,380)* y(k,51) +rxt(k,449)* y(k,54) +rxt(k,484)* y(k,59) &
                  +rxt(k,707)* y(k,79) +rxt(k,207)* y(k,105) +rxt(k,573)* y(k,115) &
                  +rxt(k,560)* y(k,116) +rxt(k,568)* y(k,117) +rxt(k,578)* y(k,120) &
                  +rxt(k,512)* y(k,127) +rxt(k,527)* y(k,130) +rxt(k,674)* y(k,136) &
                  +rxt(k,208)* y(k,151) +rxt(k,226)* y(k,152) +rxt(k,682)* y(k,155) &
                  +rxt(k,209)* y(k,161) +rxt(k,210)* y(k,167) +rxt(k,697)* y(k,212) &
                  +rxt(k,604)* y(k,244) + rxt(k,19) + rxt(k,20) + rxt(k,739) &
                  + het_rates(k,153))* y(k,153)
         prod(k,259) = (rxt(k,119) +rxt(k,273)*y(k,67) +rxt(k,275)*y(k,161) + &
                 rxt(k,276)*y(k,167))*y(k,71) + (rxt(k,15) +rxt(k,16) +rxt(k,235)) &
                 *y(k,141) + (rxt(k,233)*y(k,104) +rxt(k,467)*y(k,169) + &
                 .500_r8*rxt(k,535)*y(k,135))*y(k,167) + (rxt(k,102) + &
                 rxt(k,325)*y(k,161))*y(k,25) + (rxt(k,202)*y(k,163) + &
                 rxt(k,203)*y(k,161))*y(k,152) +rxt(k,351)*y(k,104)*y(k,88) +rxt(k,13) &
                 *y(k,106) +.400_r8*rxt(k,75)*y(k,169)
         loss(k,210) = (rxt(k,488)* y(k,167) + rxt(k,72) + het_rates(k,154))* y(k,154)
         prod(k,210) = (.500_r8*rxt(k,572)*y(k,119) +rxt(k,581)*y(k,121) + &
                 rxt(k,587)*y(k,142) +rxt(k,588)*y(k,143))*y(k,167) &
                  +rxt(k,474)*y(k,153)*y(k,36)
         loss(k,230) = (rxt(k,679)* y(k,62) +rxt(k,680)* y(k,105) +rxt(k,681) &
                 * y(k,151) +rxt(k,682)* y(k,153) + het_rates(k,155))* y(k,155)
         prod(k,230) = (rxt(k,669)*y(k,8) +rxt(k,674)*y(k,136) + &
                 .500_r8*rxt(k,697)*y(k,212))*y(k,153) +rxt(k,685)*y(k,167)*y(k,156)
         loss(k,148) = (rxt(k,685)* y(k,167) + rxt(k,73) + rxt(k,740) &
                  + het_rates(k,156))* y(k,156)
         prod(k,148) =rxt(k,680)*y(k,155)*y(k,105)
         loss(k,29) = ( + het_rates(k,157))* y(k,157)
         prod(k,29) = 0._r8
         loss(k,30) = ( + het_rates(k,158))* y(k,158)
         prod(k,30) = 0._r8
         loss(k,31) = ( + het_rates(k,159))* y(k,159)
         prod(k,31) = 0._r8
         loss(k,32) = ( + het_rates(k,160))* y(k,160)
         prod(k,32) = 0._r8
         loss(k,269) = (rxt(k,327)* y(k,24) +rxt(k,325)* y(k,25) +rxt(k,381)* y(k,51) &
                  +rxt(k,278)* y(k,70) +rxt(k,275)* y(k,71) +rxt(k,171)* y(k,92) &
                  +rxt(k,172)* y(k,94) +rxt(k,332)* y(k,96) +rxt(k,285)* y(k,100) &
                  +rxt(k,179)* y(k,105) +rxt(k,335)* y(k,107) +rxt(k,289)* y(k,109) &
                  +rxt(k,216)* y(k,151) + (rxt(k,201) +rxt(k,203))* y(k,152) &
                  +rxt(k,209)* y(k,153) + 2._r8*rxt(k,161)* y(k,161) +rxt(k,159) &
                 * y(k,163) +rxt(k,710)* y(k,166) +rxt(k,188)* y(k,167) + rxt(k,162) &
                  + het_rates(k,161))* y(k,161)
         prod(k,269) = (rxt(k,200) +rxt(k,196)*y(k,151) +rxt(k,197)*y(k,152))*y(k,139) &
                  + (rxt(k,155) +rxt(k,156))*y(k,162) + (rxt(k,140) +rxt(k,718)) &
                 *y(k,183) +rxt(k,100)*y(k,24) +.180_r8*rxt(k,55)*y(k,65) +rxt(k,118) &
                 *y(k,70) +rxt(k,56)*y(k,74) +rxt(k,177)*y(k,105)*y(k,91) +rxt(k,15) &
                 *y(k,141) +rxt(k,17)*y(k,151) +rxt(k,18)*y(k,152) +rxt(k,20)*y(k,153) &
                  +rxt(k,7)*y(k,163) +rxt(k,132)*y(k,165) +rxt(k,190)*y(k,167) &
                 *y(k,167) +rxt(k,712)*y(k,181) +rxt(k,141)*y(k,184) +rxt(k,142) &
                 *y(k,185) +rxt(k,2)*y(k,260)
         loss(k,265) = (rxt(k,292)* y(k,42) +rxt(k,293)* y(k,43) +rxt(k,337)* y(k,44) &
                  +rxt(k,294)* y(k,45) +rxt(k,295)* y(k,46) +rxt(k,296)* y(k,47) &
                  +rxt(k,297)* y(k,48) +rxt(k,298)* y(k,49) +rxt(k,369)* y(k,50) &
                  +rxt(k,370)* y(k,52) + (rxt(k,400) +rxt(k,401) +rxt(k,402))* y(k,65) &
                  +rxt(k,338)* y(k,66) +rxt(k,353)* y(k,75) +rxt(k,354)* y(k,76) &
                  +rxt(k,153)* y(k,92) +rxt(k,339)* y(k,93) + (rxt(k,340) +rxt(k,341)) &
                 * y(k,96) +rxt(k,371)* y(k,97) +rxt(k,372)* y(k,98) +rxt(k,373) &
                 * y(k,99) + (rxt(k,299) +rxt(k,300))* y(k,100) +rxt(k,403)* y(k,101) &
                  + (rxt(k,223) +rxt(k,224))* y(k,140) +rxt(k,157)* y(k,163) &
                  +rxt(k,154)* y(k,260) + rxt(k,155) + rxt(k,156) + het_rates(k,162)) &
                 * y(k,162)
         prod(k,265) =rxt(k,14)*y(k,140) +rxt(k,8)*y(k,163) +rxt(k,1)*y(k,260)
         loss(k,266) = (rxt(k,670)* y(k,8) +rxt(k,317)* y(k,22) +rxt(k,438)* y(k,31) &
                  +rxt(k,475)* y(k,36) +rxt(k,266)* y(k,67) +rxt(k,183)* y(k,91) &
                  +rxt(k,180)* y(k,105) +rxt(k,582)* y(k,115) +rxt(k,518)* y(k,126) &
                  +rxt(k,675)* y(k,136) +rxt(k,536)* y(k,138) +rxt(k,215)* y(k,151) &
                  +rxt(k,202)* y(k,152) +rxt(k,159)* y(k,161) +rxt(k,157)* y(k,162) &
                  +rxt(k,189)* y(k,167) +rxt(k,646)* y(k,171) +rxt(k,713)* y(k,181) &
                  +rxt(k,719)* y(k,183) + rxt(k,7) + rxt(k,8) + het_rates(k,163)) &
                 * y(k,163)
         prod(k,266) = (.150_r8*rxt(k,455)*y(k,57) +.150_r8*rxt(k,524)*y(k,130)) &
                 *y(k,105) +rxt(k,162)*y(k,161)
         loss(k,33) = ( + het_rates(k,164))* y(k,164)
         prod(k,33) = 0._r8
         loss(k,134) = (rxt(k,720)* y(k,183) + rxt(k,132) + het_rates(k,165)) &
                 * y(k,165)
         prod(k,134) = (rxt(k,270)*y(k,70) +rxt(k,319)*y(k,24))*y(k,70)
         loss(k,142) = (rxt(k,710)* y(k,161) +rxt(k,711)* y(k,167) + rxt(k,139) &
                  + het_rates(k,166))* y(k,166)
         prod(k,142) = 0._r8
         loss(k,263) = (rxt(k,545)* y(k,2) +rxt(k,551)* y(k,4) +rxt(k,671)* y(k,8) &
                  + (rxt(k,611) +rxt(k,761))* y(k,10) +rxt(k,615)* y(k,13) +rxt(k,552) &
                 * y(k,20) +rxt(k,502)* y(k,21) +rxt(k,328)* y(k,24) +rxt(k,616) &
                 * y(k,27) +rxt(k,618)* y(k,29) +rxt(k,436)* y(k,30) +rxt(k,468) &
                 * y(k,31) +rxt(k,445)* y(k,33) +rxt(k,446)* y(k,34) +rxt(k,448) &
                 * y(k,35) +rxt(k,498)* y(k,36) +rxt(k,482)* y(k,38) +rxt(k,483) &
                 * y(k,39) +rxt(k,624)* y(k,41) +rxt(k,358)* y(k,50) +rxt(k,382) &
                 * y(k,51) +rxt(k,360)* y(k,52) +rxt(k,361)* y(k,53) +rxt(k,450) &
                 * y(k,54) +rxt(k,363)* y(k,55) +rxt(k,452)* y(k,56) +rxt(k,499) &
                 * y(k,58) +rxt(k,485)* y(k,59) +rxt(k,458)* y(k,60) +rxt(k,459) &
                 * y(k,61) +rxt(k,390)* y(k,63) +rxt(k,391)* y(k,64) +rxt(k,392) &
                 * y(k,65) +rxt(k,365)* y(k,66) + (rxt(k,279) +rxt(k,280))* y(k,70) &
                  +rxt(k,276)* y(k,71) + (rxt(k,393) +rxt(k,404))* y(k,73) +rxt(k,625) &
                 * y(k,77) + (rxt(k,708) +rxt(k,725))* y(k,79) +rxt(k,465)* y(k,89) &
                  +rxt(k,466)* y(k,90) +rxt(k,185)* y(k,92) +rxt(k,186)* y(k,94) &
                  +rxt(k,333)* y(k,96) +rxt(k,366)* y(k,97) +rxt(k,367)* y(k,98) &
                  +rxt(k,368)* y(k,99) +rxt(k,286)* y(k,100) +rxt(k,394)* y(k,101) &
                  +rxt(k,395)* y(k,102) +rxt(k,233)* y(k,104) +rxt(k,187)* y(k,105) &
                  +rxt(k,195)* y(k,106) +rxt(k,290)* y(k,109) +rxt(k,508)* y(k,110) &
                  +rxt(k,553)* y(k,111) +rxt(k,487)* y(k,112) +rxt(k,554)* y(k,113) &
                  +rxt(k,555)* y(k,114) +rxt(k,583)* y(k,115) +rxt(k,571)* y(k,118) &
                  +rxt(k,572)* y(k,119) +rxt(k,581)* y(k,121) +rxt(k,584)* y(k,123) &
                  +rxt(k,519)* y(k,126) +rxt(k,520)* y(k,128) +rxt(k,533)* y(k,132) &
                  +rxt(k,534)* y(k,134) +rxt(k,535)* y(k,135) +rxt(k,676)* y(k,136) &
                  +rxt(k,537)* y(k,138) +rxt(k,213)* y(k,139) +rxt(k,587)* y(k,142) &
                  +rxt(k,588)* y(k,143) +rxt(k,728)* y(k,147) +rxt(k,227)* y(k,152) &
                  +rxt(k,210)* y(k,153) +rxt(k,488)* y(k,154) +rxt(k,685)* y(k,156) &
                  +rxt(k,188)* y(k,161) +rxt(k,189)* y(k,163) +rxt(k,711)* y(k,166) &
                  + 2._r8*(rxt(k,190) +rxt(k,191))* y(k,167) +rxt(k,467)* y(k,169) &
                  +rxt(k,644)* y(k,173) +rxt(k,647)* y(k,174) +rxt(k,492)* y(k,178) &
                  +rxt(k,497)* y(k,180) +rxt(k,716)* y(k,181) +rxt(k,721)* y(k,183) &
                  +rxt(k,726)* y(k,184) +rxt(k,690)* y(k,208) +rxt(k,691)* y(k,209) &
                  +rxt(k,696)* y(k,211) +rxt(k,698)* y(k,212) +rxt(k,700)* y(k,213) &
                  +rxt(k,655)* y(k,215) + (rxt(k,656) +rxt(k,778))* y(k,216) &
                  +rxt(k,607)* y(k,247) + (rxt(k,659) +rxt(k,781))* y(k,251) &
                  +rxt(k,663)* y(k,253) +rxt(k,667)* y(k,255) +rxt(k,668)* y(k,257) &
                  + het_rates(k,167))* y(k,167)
         prod(k,263) = (2.000_r8*rxt(k,176)*y(k,91) +rxt(k,179)*y(k,161) + &
                 rxt(k,180)*y(k,163) +rxt(k,207)*y(k,153) +rxt(k,214)*y(k,151) + &
                 rxt(k,265)*y(k,67) +.450_r8*rxt(k,455)*y(k,57) + &
                 .150_r8*rxt(k,494)*y(k,179) +.450_r8*rxt(k,524)*y(k,130) + &
                 .200_r8*rxt(k,530)*y(k,133) +.400_r8*rxt(k,608)*y(k,1) + &
                 .400_r8*rxt(k,626)*y(k,78) +.400_r8*rxt(k,636)*y(k,131))*y(k,105) &
                  + (rxt(k,183)*y(k,91) +.130_r8*rxt(k,438)*y(k,31) + &
                 .360_r8*rxt(k,475)*y(k,36) +.240_r8*rxt(k,518)*y(k,126) + &
                 .360_r8*rxt(k,536)*y(k,138) +.320_r8*rxt(k,582)*y(k,115) + &
                 .630_r8*rxt(k,670)*y(k,8) +.630_r8*rxt(k,675)*y(k,136))*y(k,163) &
                  + (rxt(k,171)*y(k,92) +rxt(k,172)*y(k,94) +rxt(k,285)*y(k,100) + &
                 rxt(k,289)*y(k,109) +rxt(k,332)*y(k,96) +rxt(k,335)*y(k,107) + &
                 rxt(k,381)*y(k,51))*y(k,161) + (.300_r8*rxt(k,391)*y(k,64) + &
                 .650_r8*rxt(k,436)*y(k,30) +.500_r8*rxt(k,446)*y(k,34) + &
                 .500_r8*rxt(k,492)*y(k,178) +.100_r8*rxt(k,520)*y(k,128) + &
                 .600_r8*rxt(k,584)*y(k,123) +.500_r8*rxt(k,607)*y(k,247))*y(k,167) &
                  + (rxt(k,153)*y(k,92) +2.000_r8*rxt(k,154)*y(k,260) + &
                 rxt(k,299)*y(k,100) +rxt(k,340)*y(k,96) +rxt(k,400)*y(k,65) + &
                 rxt(k,403)*y(k,101))*y(k,162) + (rxt(k,3) +rxt(k,350)*y(k,88)) &
                 *y(k,260) +rxt(k,35)*y(k,4) +rxt(k,36)*y(k,13) +rxt(k,43)*y(k,29) &
                  +rxt(k,44)*y(k,34) +rxt(k,45)*y(k,38) +rxt(k,46)*y(k,41) +rxt(k,52) &
                 *y(k,61) +rxt(k,53)*y(k,64) +.330_r8*rxt(k,55)*y(k,65) +rxt(k,57) &
                 *y(k,87) +2.000_r8*rxt(k,4)*y(k,94) +rxt(k,11)*y(k,104) +rxt(k,13) &
                 *y(k,106) +rxt(k,130)*y(k,107) +rxt(k,131)*y(k,109) +rxt(k,61) &
                 *y(k,111) +rxt(k,68)*y(k,134) +.500_r8*rxt(k,738)*y(k,152) +rxt(k,73) &
                 *y(k,156) +rxt(k,76)*y(k,174) +rxt(k,77)*y(k,178) +rxt(k,78)*y(k,180) &
                  +rxt(k,80)*y(k,208) +rxt(k,82)*y(k,211) +rxt(k,85)*y(k,215) &
                  +rxt(k,94)*y(k,247) +rxt(k,97)*y(k,253) +rxt(k,98)*y(k,257)
         loss(k,104) = ( + rxt(k,74) + rxt(k,741) + het_rates(k,168))* y(k,168)
         prod(k,104) =rxt(k,508)*y(k,167)*y(k,110) +.100_r8*rxt(k,688)*y(k,207) &
                 *y(k,151)
         loss(k,170) = (rxt(k,467)* y(k,167) + rxt(k,75) + rxt(k,471) &
                  + het_rates(k,169))* y(k,169)
         prod(k,170) =rxt(k,469)*y(k,152)*y(k,57)
         loss(k,77) = ( + rxt(k,657) + het_rates(k,170))* y(k,170)
         prod(k,77) =rxt(k,650)*y(k,152)*y(k,1)
         loss(k,176) = (rxt(k,645)* y(k,152) +rxt(k,646)* y(k,163) + het_rates(k,171)) &
                 * y(k,171)
         prod(k,176) = (.070_r8*rxt(k,625)*y(k,77) +.060_r8*rxt(k,644)*y(k,173) + &
                 .070_r8*rxt(k,667)*y(k,255))*y(k,167) +rxt(k,622)*y(k,151)*y(k,40) &
                  +rxt(k,46)*y(k,41)
         loss(k,171) = (rxt(k,641)* y(k,105) +rxt(k,642)* y(k,151) + het_rates(k,172)) &
                 * y(k,172)
         prod(k,171) = (.200_r8*rxt(k,625)*y(k,77) +.140_r8*rxt(k,644)*y(k,173) + &
                 rxt(k,647)*y(k,174))*y(k,167)
         loss(k,90) = (rxt(k,644)* y(k,167) + het_rates(k,173))* y(k,173)
         prod(k,90) =.530_r8*rxt(k,611)*y(k,167)*y(k,10)
         loss(k,135) = (rxt(k,647)* y(k,167) + rxt(k,76) + het_rates(k,174))* y(k,174)
         prod(k,135) =rxt(k,641)*y(k,172)*y(k,105)
         loss(k,204) = (rxt(k,489)* y(k,105) +rxt(k,490)* y(k,151) + het_rates(k,175)) &
                 * y(k,175)
         prod(k,204) = (.500_r8*rxt(k,492)*y(k,178) +rxt(k,498)*y(k,36))*y(k,167)
         loss(k,34) = ( + het_rates(k,176))* y(k,176)
         prod(k,34) = 0._r8
         loss(k,35) = ( + het_rates(k,177))* y(k,177)
         prod(k,35) = 0._r8
         loss(k,172) = (rxt(k,492)* y(k,167) + rxt(k,77) + het_rates(k,178))* y(k,178)
         prod(k,172) =rxt(k,489)*y(k,175)*y(k,105)
         loss(k,236) = (rxt(k,493)* y(k,62) +rxt(k,494)* y(k,105) +rxt(k,495) &
                 * y(k,151) + het_rates(k,179))* y(k,179)
         prod(k,236) = (rxt(k,497)*y(k,180) +rxt(k,499)*y(k,58) + &
                 .150_r8*rxt(k,700)*y(k,213))*y(k,167) + (.060_r8*rxt(k,670)*y(k,8) + &
                 .060_r8*rxt(k,675)*y(k,136))*y(k,163) +.150_r8*rxt(k,84)*y(k,213)
         loss(k,150) = (rxt(k,497)* y(k,167) + rxt(k,78) + het_rates(k,180))* y(k,180)
         prod(k,150) =.850_r8*rxt(k,494)*y(k,179)*y(k,105)
         loss(k,167) = (rxt(k,713)* y(k,163) +rxt(k,716)* y(k,167) + rxt(k,712) &
                  + het_rates(k,181))* y(k,181)
         prod(k,167) =rxt(k,139)*y(k,166) +rxt(k,140)*y(k,183)
         loss(k,36) = ( + rxt(k,133) + het_rates(k,182))* y(k,182)
         prod(k,36) = 0._r8
         loss(k,240) = (rxt(k,714)* y(k,24) +rxt(k,715)* y(k,70) +rxt(k,717)* y(k,152) &
                  +rxt(k,719)* y(k,163) +rxt(k,720)* y(k,165) +rxt(k,721)* y(k,167) &
                  + rxt(k,140) + rxt(k,718) + het_rates(k,183))* y(k,183)
         prod(k,240) = (rxt(k,712) +rxt(k,713)*y(k,163) +rxt(k,716)*y(k,167))*y(k,181) &
                  +rxt(k,710)*y(k,166)*y(k,161) +rxt(k,141)*y(k,184)
         loss(k,211) = (rxt(k,726)* y(k,167) + rxt(k,141) + het_rates(k,184)) &
                 * y(k,184)
         prod(k,211) = (rxt(k,718) +rxt(k,714)*y(k,24) +rxt(k,715)*y(k,70) + &
                 rxt(k,717)*y(k,152) +rxt(k,719)*y(k,163) +rxt(k,720)*y(k,165) + &
                 rxt(k,721)*y(k,167))*y(k,183) + (rxt(k,707)*y(k,153) + &
                 rxt(k,708)*y(k,167) +.500_r8*rxt(k,725)*y(k,167))*y(k,79) &
                  +rxt(k,711)*y(k,167)*y(k,166) +rxt(k,142)*y(k,185)
         loss(k,113) = (rxt(k,727)* y(k,260) + rxt(k,142) + het_rates(k,185)) &
                 * y(k,185)
         prod(k,113) =rxt(k,138)*y(k,95) +rxt(k,726)*y(k,184)*y(k,167)
         loss(k,37) = ( + het_rates(k,186))* y(k,186)
         prod(k,37) = 0._r8
         loss(k,38) = ( + het_rates(k,187))* y(k,187)
         prod(k,38) = 0._r8
         loss(k,39) = ( + het_rates(k,188))* y(k,188)
         prod(k,39) = 0._r8
         loss(k,40) = ( + rxt(k,143) + het_rates(k,189))* y(k,189)
         prod(k,40) = 0._r8
         loss(k,41) = ( + rxt(k,144) + het_rates(k,190))* y(k,190)
         prod(k,41) = 0._r8
         loss(k,42) = ( + rxt(k,145) + het_rates(k,191))* y(k,191)
         prod(k,42) = 0._r8
         loss(k,43) = ( + rxt(k,146) + het_rates(k,192))* y(k,192)
         prod(k,43) = 0._r8
         loss(k,44) = ( + rxt(k,147) + het_rates(k,193))* y(k,193)
         prod(k,44) = 0._r8
         loss(k,45) = ( + rxt(k,148) + het_rates(k,194))* y(k,194)
         prod(k,45) = 0._r8
         loss(k,46) = ( + rxt(k,149) + het_rates(k,195))* y(k,195)
         prod(k,46) = 0._r8
         loss(k,47) = ( + rxt(k,150) + het_rates(k,196))* y(k,196)
         prod(k,47) = 0._r8
         loss(k,48) = ( + rxt(k,151) + het_rates(k,197))* y(k,197)
         prod(k,48) = 0._r8
         loss(k,49) = ( + rxt(k,152) + het_rates(k,198))* y(k,198)
         prod(k,49) = 0._r8
         loss(k,50) = ( + het_rates(k,199))* y(k,199)
         prod(k,50) = (.2202005_r8*rxt(k,757)*y(k,9) +.0023005_r8*rxt(k,762)*y(k,12) + &
                 .0031005_r8*rxt(k,765)*y(k,122) +.2381005_r8*rxt(k,769)*y(k,125) + &
                 .0508005_r8*rxt(k,773)*y(k,137) +.1364005_r8*rxt(k,779)*y(k,217) + &
                 .1677005_r8*rxt(k,782)*y(k,254))*y(k,105) &
                  + (.1279005_r8*rxt(k,758)*y(k,9) +.0097005_r8*rxt(k,763)*y(k,12) + &
                 .0003005_r8*rxt(k,766)*y(k,122) +.1056005_r8*rxt(k,770)*y(k,125) + &
                 .0245005_r8*rxt(k,774)*y(k,137) +.0154005_r8*rxt(k,780)*y(k,217) + &
                 .0063005_r8*rxt(k,783)*y(k,254))*y(k,151) &
                  + (.2202005_r8*rxt(k,759)*y(k,8) +.0508005_r8*rxt(k,775)*y(k,136)) &
                 *y(k,163) +rxt(k,729)*y(k,90) +.5931005_r8*rxt(k,777)*y(k,205) &
                 *y(k,167)
         loss(k,51) = ( + het_rates(k,200))* y(k,200)
         prod(k,51) = (.2067005_r8*rxt(k,757)*y(k,9) +.0008005_r8*rxt(k,762)*y(k,12) + &
                 .0035005_r8*rxt(k,765)*y(k,122) +.1308005_r8*rxt(k,769)*y(k,125) + &
                 .1149005_r8*rxt(k,773)*y(k,137) +.0101005_r8*rxt(k,779)*y(k,217) + &
                 .0174005_r8*rxt(k,782)*y(k,254))*y(k,105) &
                  + (.1792005_r8*rxt(k,758)*y(k,9) +.0034005_r8*rxt(k,763)*y(k,12) + &
                 .0003005_r8*rxt(k,766)*y(k,122) +.1026005_r8*rxt(k,770)*y(k,125) + &
                 .0082005_r8*rxt(k,774)*y(k,137) +.0452005_r8*rxt(k,780)*y(k,217) + &
                 .0237005_r8*rxt(k,783)*y(k,254))*y(k,151) &
                  + (.2067005_r8*rxt(k,759)*y(k,8) +.1149005_r8*rxt(k,775)*y(k,136)) &
                 *y(k,163) +.1534005_r8*rxt(k,777)*y(k,205)*y(k,167)
         loss(k,52) = ( + het_rates(k,201))* y(k,201)
         prod(k,52) = (.0653005_r8*rxt(k,757)*y(k,9) +.0843005_r8*rxt(k,762)*y(k,12) + &
                 .0003005_r8*rxt(k,765)*y(k,122) +.0348005_r8*rxt(k,769)*y(k,125) + &
                 .0348005_r8*rxt(k,773)*y(k,137) +.0763005_r8*rxt(k,779)*y(k,217) + &
                 .086_r8*rxt(k,782)*y(k,254))*y(k,105) &
                  + (.0676005_r8*rxt(k,758)*y(k,9) +.1579005_r8*rxt(k,763)*y(k,12) + &
                 .0073005_r8*rxt(k,766)*y(k,122) +.0521005_r8*rxt(k,770)*y(k,125) + &
                 .0772005_r8*rxt(k,774)*y(k,137) +.0966005_r8*rxt(k,780)*y(k,217) + &
                 .0025005_r8*rxt(k,783)*y(k,254))*y(k,151) &
                  + (.0653005_r8*rxt(k,759)*y(k,8) +.0348005_r8*rxt(k,775)*y(k,136)) &
                 *y(k,163) +.0459005_r8*rxt(k,777)*y(k,205)*y(k,167)
         loss(k,53) = ( + het_rates(k,202))* y(k,202)
         prod(k,53) = (.1284005_r8*rxt(k,757)*y(k,9) +.0443005_r8*rxt(k,762)*y(k,12) + &
                 .0271005_r8*rxt(k,765)*y(k,122) +.0076005_r8*rxt(k,769)*y(k,125) + &
                 .0554005_r8*rxt(k,773)*y(k,137) +.2157005_r8*rxt(k,779)*y(k,217) + &
                 .0512005_r8*rxt(k,782)*y(k,254))*y(k,105) &
                  + (.079_r8*rxt(k,758)*y(k,9) +.0059005_r8*rxt(k,763)*y(k,12) + &
                 .0057005_r8*rxt(k,766)*y(k,122) +.0143005_r8*rxt(k,770)*y(k,125) + &
                 .0332005_r8*rxt(k,774)*y(k,137) +.0073005_r8*rxt(k,780)*y(k,217) + &
                 .011_r8*rxt(k,783)*y(k,254))*y(k,151) &
                  + (.1749305_r8*rxt(k,756)*y(k,8) +.0590245_r8*rxt(k,764)*y(k,115) + &
                 .1749305_r8*rxt(k,772)*y(k,136))*y(k,153) &
                  + (.1284005_r8*rxt(k,759)*y(k,8) +.0033005_r8*rxt(k,767)*y(k,115) + &
                 .0554005_r8*rxt(k,775)*y(k,136))*y(k,163) &
                  +.0085005_r8*rxt(k,777)*y(k,205)*y(k,167)
         loss(k,54) = ( + het_rates(k,203))* y(k,203)
         prod(k,54) = (.114_r8*rxt(k,757)*y(k,9) +.1621005_r8*rxt(k,762)*y(k,12) + &
                 .0474005_r8*rxt(k,765)*y(k,122) +.0113005_r8*rxt(k,769)*y(k,125) + &
                 .1278005_r8*rxt(k,773)*y(k,137) +.0738005_r8*rxt(k,779)*y(k,217) + &
                 .1598005_r8*rxt(k,782)*y(k,254))*y(k,105) &
                  + (.1254005_r8*rxt(k,758)*y(k,9) +.0536005_r8*rxt(k,763)*y(k,12) + &
                 .0623005_r8*rxt(k,766)*y(k,122) +.0166005_r8*rxt(k,770)*y(k,125) + &
                 .130_r8*rxt(k,774)*y(k,137) +.238_r8*rxt(k,780)*y(k,217) + &
                 .1185005_r8*rxt(k,783)*y(k,254))*y(k,151) &
                  + (.5901905_r8*rxt(k,756)*y(k,8) +.0250245_r8*rxt(k,764)*y(k,115) + &
                 .5901905_r8*rxt(k,772)*y(k,136))*y(k,153) &
                  + (.114_r8*rxt(k,759)*y(k,8) +.1278005_r8*rxt(k,775)*y(k,136)) &
                 *y(k,163) +.0128005_r8*rxt(k,777)*y(k,205)*y(k,167)
         loss(k,61) = ( + rxt(k,822) + het_rates(k,204))* y(k,204)
         prod(k,61) = 0._r8
         loss(k,62) = (rxt(k,777)* y(k,167) + het_rates(k,205))* y(k,205)
         prod(k,62) = 0._r8
         loss(k,93) = ( + rxt(k,79) + het_rates(k,206))* y(k,206)
         prod(k,93) = (.100_r8*rxt(k,656)*y(k,216) +.230_r8*rxt(k,659)*y(k,251)) &
                 *y(k,167)
         loss(k,235) = (rxt(k,686)* y(k,62) +rxt(k,687)* y(k,105) +rxt(k,688) &
                 * y(k,151) + het_rates(k,207))* y(k,207)
         prod(k,235) = (rxt(k,690)*y(k,208) +rxt(k,698)*y(k,212))*y(k,167) &
                  +.500_r8*rxt(k,697)*y(k,212)*y(k,153)
         loss(k,187) = (rxt(k,690)* y(k,167) + rxt(k,80) + het_rates(k,208))* y(k,208)
         prod(k,187) =rxt(k,687)*y(k,207)*y(k,105)
         loss(k,186) = (rxt(k,691)* y(k,167) + rxt(k,81) + rxt(k,742) &
                  + het_rates(k,209))* y(k,209)
         prod(k,186) = (.200_r8*rxt(k,681)*y(k,155) +.200_r8*rxt(k,694)*y(k,210)) &
                 *y(k,151) +.500_r8*rxt(k,679)*y(k,155)*y(k,62)
         loss(k,221) = (rxt(k,692)* y(k,62) +rxt(k,693)* y(k,105) +rxt(k,694) &
                 * y(k,151) + het_rates(k,210))* y(k,210)
         prod(k,221) = (rxt(k,671)*y(k,8) +rxt(k,676)*y(k,136) +rxt(k,696)*y(k,211)) &
                 *y(k,167)
         loss(k,159) = (rxt(k,696)* y(k,167) + rxt(k,82) + het_rates(k,211))* y(k,211)
         prod(k,159) =rxt(k,693)*y(k,210)*y(k,105)
         loss(k,226) = (rxt(k,697)* y(k,153) +rxt(k,698)* y(k,167) + rxt(k,83) &
                  + het_rates(k,212))* y(k,212)
         prod(k,226) = (.500_r8*rxt(k,679)*y(k,62) +.800_r8*rxt(k,681)*y(k,151) + &
                 rxt(k,682)*y(k,153))*y(k,155) + (.330_r8*rxt(k,670)*y(k,8) + &
                 .330_r8*rxt(k,675)*y(k,136))*y(k,163) + (rxt(k,81) + &
                 rxt(k,691)*y(k,167))*y(k,209) + (rxt(k,692)*y(k,62) + &
                 .800_r8*rxt(k,694)*y(k,151))*y(k,210) +rxt(k,73)*y(k,156) +rxt(k,82) &
                 *y(k,211)
         loss(k,231) = (rxt(k,700)* y(k,167) + rxt(k,84) + het_rates(k,213))* y(k,213)
         prod(k,231) = (.300_r8*rxt(k,670)*y(k,8) +.300_r8*rxt(k,675)*y(k,136)) &
                 *y(k,163) + (rxt(k,686)*y(k,62) +.900_r8*rxt(k,688)*y(k,151)) &
                 *y(k,207) +rxt(k,80)*y(k,208) +rxt(k,83)*y(k,212)
         loss(k,199) = (rxt(k,652)* y(k,105) +rxt(k,653)* y(k,151) + het_rates(k,214)) &
                 * y(k,214)
         prod(k,199) = (rxt(k,655)*y(k,215) +.650_r8*rxt(k,656)*y(k,216) + &
                 rxt(k,761)*y(k,10) +rxt(k,778)*y(k,216) +rxt(k,781)*y(k,251)) &
                 *y(k,167)
         loss(k,188) = (rxt(k,655)* y(k,167) + rxt(k,85) + het_rates(k,215))* y(k,215)
         prod(k,188) =rxt(k,652)*y(k,214)*y(k,105)
         loss(k,91) = ((rxt(k,656) +rxt(k,778))* y(k,167) + het_rates(k,216)) &
                 * y(k,216)
         prod(k,91) = 0._r8
         loss(k,63) = (rxt(k,779)* y(k,105) +rxt(k,780)* y(k,151) + het_rates(k,217)) &
                 * y(k,217)
         prod(k,63) =rxt(k,778)*y(k,216)*y(k,167)
         loss(k,72) = (rxt(k,589)* y(k,167) + rxt(k,86) + het_rates(k,218))* y(k,218)
         prod(k,72) =rxt(k,550)*y(k,233)*y(k,3)
         loss(k,115) = (rxt(k,342)* y(k,161) + rxt(k,134) + rxt(k,135) + rxt(k,801) &
                  + rxt(k,806) + rxt(k,812) + het_rates(k,219))* y(k,219)
         prod(k,115) =rxt(k,329)*y(k,234)*y(k,24)
         loss(k,160) = (rxt(k,303)* y(k,67) + (rxt(k,807) +rxt(k,813) +rxt(k,818)) &
                 * y(k,100) +rxt(k,304)* y(k,161) +rxt(k,305)* y(k,167) + rxt(k,136) &
                  + rxt(k,137) + rxt(k,805) + rxt(k,811) + rxt(k,817) &
                  + het_rates(k,220))* y(k,220)
         prod(k,160) =rxt(k,282)*y(k,234)*y(k,70)
         loss(k,215) = (rxt(k,352)* y(k,88) +rxt(k,236)* y(k,167) + rxt(k,21) &
                  + het_rates(k,221))* y(k,221)
         prod(k,215) = (rxt(k,805) +rxt(k,811) +rxt(k,817) +rxt(k,807)*y(k,100) + &
                 rxt(k,813)*y(k,100) +rxt(k,818)*y(k,100))*y(k,220) + (rxt(k,751) + &
                 rxt(k,383)*y(k,51) +rxt(k,451)*y(k,54) +rxt(k,486)*y(k,59) + &
                 rxt(k,709)*y(k,79))*y(k,237) + (rxt(k,749) +rxt(k,802) +rxt(k,808) + &
                 rxt(k,814))*y(k,235) + (2.000_r8*rxt(k,750) +2.000_r8*rxt(k,804) + &
                 2.000_r8*rxt(k,810) +2.000_r8*rxt(k,816))*y(k,236) + (rxt(k,752) + &
                 rxt(k,803) +rxt(k,809) +rxt(k,815))*y(k,238) + (rxt(k,801) + &
                 rxt(k,806) +rxt(k,812))*y(k,219) + (.500_r8*rxt(k,748) + &
                 rxt(k,231)*y(k,167))*y(k,234) +rxt(k,743)*y(k,223) +rxt(k,744) &
                 *y(k,224) +rxt(k,745)*y(k,225) +rxt(k,746)*y(k,230) +rxt(k,747) &
                 *y(k,231) +rxt(k,753)*y(k,241) +rxt(k,754)*y(k,246) +rxt(k,755) &
                 *y(k,250)
         loss(k,94) = (rxt(k,241)* y(k,167) + rxt(k,22) + rxt(k,23) + rxt(k,237) &
                  + het_rates(k,222))* y(k,222)
         prod(k,94) =rxt(k,229)*y(k,234)*y(k,105)
         loss(k,138) = (rxt(k,543)* y(k,167) + rxt(k,87) + rxt(k,743) &
                  + het_rates(k,223))* y(k,223)
         prod(k,138) = (.300_r8*rxt(k,590)*y(k,224) +.500_r8*rxt(k,591)*y(k,225)) &
                 *y(k,167) + (rxt(k,507)*y(k,84) +rxt(k,517)*y(k,127))*y(k,233)
         loss(k,116) = (rxt(k,590)* y(k,167) + rxt(k,744) + het_rates(k,224)) &
                 * y(k,224)
         prod(k,116) =.080_r8*rxt(k,561)*y(k,233)*y(k,116)
         loss(k,117) = (rxt(k,591)* y(k,167) + rxt(k,745) + het_rates(k,225)) &
                 * y(k,225)
         prod(k,117) =.080_r8*rxt(k,569)*y(k,233)*y(k,117)
         loss(k,145) = (rxt(k,592)* y(k,57) +rxt(k,593)* y(k,62) +rxt(k,594)* y(k,105) &
                  +rxt(k,595)* y(k,151) +rxt(k,596)* y(k,153) + het_rates(k,226)) &
                 * y(k,226)
         prod(k,145) =rxt(k,585)*y(k,237)*y(k,115)
         loss(k,95) = (rxt(k,597)* y(k,167) + rxt(k,88) + het_rates(k,227))* y(k,227)
         prod(k,95) =rxt(k,594)*y(k,226)*y(k,105)
         loss(k,96) = (rxt(k,544)* y(k,167) + rxt(k,89) + rxt(k,542) &
                  + het_rates(k,228))* y(k,228)
         prod(k,96) =rxt(k,540)*y(k,234)*y(k,130)
         loss(k,81) = (rxt(k,242)* y(k,151) + (rxt(k,243) +rxt(k,244) +rxt(k,245)) &
                 * y(k,152) +rxt(k,254)* y(k,167) + rxt(k,246) + het_rates(k,229)) &
                 * y(k,229)
         prod(k,81) =rxt(k,24)*y(k,233)
         loss(k,97) = (rxt(k,598)* y(k,167) + rxt(k,746) + het_rates(k,230))* y(k,230)
         prod(k,97) =.200_r8*rxt(k,593)*y(k,226)*y(k,62)
         loss(k,140) = (rxt(k,599)* y(k,167) + rxt(k,90) + rxt(k,747) &
                  + het_rates(k,231))* y(k,231)
         prod(k,140) = (rxt(k,592)*y(k,57) +.800_r8*rxt(k,593)*y(k,62) + &
                 rxt(k,595)*y(k,151) +rxt(k,596)*y(k,153))*y(k,226)
         loss(k,216) = (rxt(k,610)* y(k,1) + (rxt(k,549) +rxt(k,550))* y(k,3) &
                  +rxt(k,614)* y(k,11) +rxt(k,620)* y(k,28) +rxt(k,444)* y(k,32) &
                  +rxt(k,481)* y(k,37) +rxt(k,623)* y(k,40) +rxt(k,457)* y(k,57) &
                  +rxt(k,389)* y(k,62) +rxt(k,281)* y(k,70) +rxt(k,629)* y(k,78) &
                  + (rxt(k,506) +rxt(k,507))* y(k,84) +rxt(k,462)* y(k,86) +rxt(k,255) &
                 * y(k,105) +rxt(k,399)* y(k,108) +rxt(k,561)* y(k,116) +rxt(k,569) &
                 * y(k,117) +rxt(k,579)* y(k,120) + (rxt(k,516) +rxt(k,517))* y(k,127) &
                  +rxt(k,634)* y(k,129) +rxt(k,528)* y(k,130) +rxt(k,639)* y(k,131) &
                  +rxt(k,532)* y(k,133) +rxt(k,219)* y(k,139) +rxt(k,211)* y(k,153) &
                  +rxt(k,683)* y(k,155) +rxt(k,257)* y(k,161) +rxt(k,256)* y(k,163) &
                  +rxt(k,643)* y(k,172) +rxt(k,491)* y(k,175) +rxt(k,496)* y(k,179) &
                  +rxt(k,689)* y(k,207) +rxt(k,695)* y(k,210) +rxt(k,654)* y(k,214) &
                  +rxt(k,605)* y(k,244) +rxt(k,662)* y(k,252) +rxt(k,666)* y(k,256) &
                  + rxt(k,24) + het_rates(k,233))* y(k,233)
         prod(k,216) = (rxt(k,25) +.500_r8*rxt(k,748) +rxt(k,221)*y(k,139) + &
                 rxt(k,247)*y(k,161) +rxt(k,722)*y(k,183))*y(k,234) + (rxt(k,246) + &
                 rxt(k,244)*y(k,152) +rxt(k,254)*y(k,167))*y(k,229) +rxt(k,26) &
                 *y(k,235) +rxt(k,28)*y(k,236) +rxt(k,31)*y(k,237)
         loss(k,217) = (rxt(k,651)* y(k,1) +rxt(k,329)* y(k,24) +rxt(k,470)* y(k,57) &
                  +rxt(k,282)* y(k,70) +rxt(k,630)* y(k,78) +rxt(k,229)* y(k,105) &
                  +rxt(k,635)* y(k,129) +rxt(k,540)* y(k,130) +rxt(k,640)* y(k,131) &
                  + (rxt(k,220) +rxt(k,221) +rxt(k,222))* y(k,139) +rxt(k,230) &
                 * y(k,153) + (rxt(k,247) +rxt(k,249))* y(k,161) +rxt(k,248)* y(k,163) &
                  +rxt(k,231)* y(k,167) +rxt(k,648)* y(k,171) +rxt(k,722)* y(k,183) &
                  +rxt(k,232)* y(k,237) + rxt(k,25) + rxt(k,748) + het_rates(k,234)) &
                 * y(k,234)
         prod(k,217) = (rxt(k,211)*y(k,153) +rxt(k,255)*y(k,105) + &
                 rxt(k,256)*y(k,163) +rxt(k,257)*y(k,161) +rxt(k,281)*y(k,70) + &
                 rxt(k,389)*y(k,62) +rxt(k,399)*y(k,108) +rxt(k,444)*y(k,32) + &
                 rxt(k,457)*y(k,57) +rxt(k,462)*y(k,86) +rxt(k,481)*y(k,37) + &
                 rxt(k,491)*y(k,175) +rxt(k,496)*y(k,179) +rxt(k,506)*y(k,84) + &
                 rxt(k,516)*y(k,127) +rxt(k,528)*y(k,130) +rxt(k,532)*y(k,133) + &
                 rxt(k,549)*y(k,3) +.920_r8*rxt(k,561)*y(k,116) + &
                 .920_r8*rxt(k,569)*y(k,117) +rxt(k,579)*y(k,120) + &
                 rxt(k,605)*y(k,244) +rxt(k,610)*y(k,1) +rxt(k,614)*y(k,11) + &
                 rxt(k,620)*y(k,28) +rxt(k,623)*y(k,40) +rxt(k,629)*y(k,78) + &
                 rxt(k,634)*y(k,129) +rxt(k,639)*y(k,131) +rxt(k,643)*y(k,172) + &
                 rxt(k,654)*y(k,214) +rxt(k,662)*y(k,252) +rxt(k,666)*y(k,256) + &
                 rxt(k,683)*y(k,155) +.900_r8*rxt(k,689)*y(k,207) + &
                 .800_r8*rxt(k,695)*y(k,210))*y(k,233) + (rxt(k,30) + &
                 rxt(k,250)*y(k,105) +rxt(k,251)*y(k,151) +rxt(k,252)*y(k,161) + &
                 rxt(k,253)*y(k,167) +rxt(k,503)*y(k,21) +rxt(k,515)*y(k,127) + &
                 rxt(k,529)*y(k,130) +rxt(k,562)*y(k,116) +rxt(k,570)*y(k,117) + &
                 rxt(k,580)*y(k,120) +rxt(k,606)*y(k,244) +rxt(k,684)*y(k,155)) &
                 *y(k,237) + (rxt(k,241)*y(k,222) +rxt(k,500)*y(k,239) + &
                 rxt(k,589)*y(k,218) +.700_r8*rxt(k,590)*y(k,224) + &
                 rxt(k,706)*y(k,250))*y(k,167) + (.500_r8*rxt(k,701)*y(k,62) + &
                 .600_r8*rxt(k,703)*y(k,151) +rxt(k,704)*y(k,153))*y(k,240) &
                  + (rxt(k,22) +rxt(k,237))*y(k,222) + (rxt(k,89) +rxt(k,542)) &
                 *y(k,228) + (rxt(k,27) +rxt(k,238))*y(k,235) + (rxt(k,29) + &
                 rxt(k,239))*y(k,236) + (.600_r8*rxt(k,95) +rxt(k,472))*y(k,248) &
                  +rxt(k,86)*y(k,218) +rxt(k,135)*y(k,219) +rxt(k,137)*y(k,220) &
                  +rxt(k,21)*y(k,221) +rxt(k,87)*y(k,223) +rxt(k,88)*y(k,227) &
                  +rxt(k,90)*y(k,231) +rxt(k,91)*y(k,239) +rxt(k,92)*y(k,241) &
                  +rxt(k,93)*y(k,246) +rxt(k,658)*y(k,249) +rxt(k,96)*y(k,250)
         loss(k,106) = ( + rxt(k,26) + rxt(k,27) + rxt(k,238) + rxt(k,749) &
                  + rxt(k,802) + rxt(k,808) + rxt(k,814) + het_rates(k,235))* y(k,235)
         prod(k,106) =rxt(k,230)*y(k,234)*y(k,153)
         loss(k,127) = ( + rxt(k,28) + rxt(k,29) + rxt(k,239) + rxt(k,750) &
                  + rxt(k,804) + rxt(k,810) + rxt(k,816) + het_rates(k,236))* y(k,236)
         prod(k,127) =rxt(k,232)*y(k,237)*y(k,234)
         loss(k,218) = (rxt(k,672)* y(k,8) +rxt(k,503)* y(k,21) +rxt(k,476)* y(k,36) &
                  +rxt(k,383)* y(k,51) +rxt(k,451)* y(k,54) +rxt(k,486)* y(k,59) &
                  +rxt(k,709)* y(k,79) +rxt(k,250)* y(k,105) +rxt(k,585)* y(k,115) &
                  +rxt(k,562)* y(k,116) +rxt(k,570)* y(k,117) +rxt(k,580)* y(k,120) &
                  +rxt(k,515)* y(k,127) +rxt(k,529)* y(k,130) +rxt(k,677)* y(k,136) &
                  +rxt(k,251)* y(k,151) +rxt(k,228)* y(k,152) +rxt(k,684)* y(k,155) &
                  +rxt(k,252)* y(k,161) +rxt(k,253)* y(k,167) +rxt(k,699)* y(k,212) &
                  +rxt(k,232)* y(k,234) +rxt(k,606)* y(k,244) + rxt(k,30) + rxt(k,31) &
                  + rxt(k,751) + het_rates(k,237))* y(k,237)
         prod(k,218) = (rxt(k,236)*y(k,221) +rxt(k,305)*y(k,220) + &
                 rxt(k,473)*y(k,248) +.500_r8*rxt(k,544)*y(k,228))*y(k,167) &
                  + (rxt(k,249)*y(k,234) +rxt(k,304)*y(k,220) +rxt(k,342)*y(k,219)) &
                 *y(k,161) + (rxt(k,28) +rxt(k,29) +rxt(k,239))*y(k,236) &
                  + (rxt(k,32) +rxt(k,33) +rxt(k,240))*y(k,238) + (rxt(k,136) + &
                 rxt(k,303)*y(k,67))*y(k,220) +rxt(k,352)*y(k,221)*y(k,88) &
                  +rxt(k,248)*y(k,234)*y(k,163) +rxt(k,134)*y(k,219) +rxt(k,23) &
                 *y(k,222) +.400_r8*rxt(k,95)*y(k,248)
         loss(k,82) = ( + rxt(k,32) + rxt(k,33) + rxt(k,240) + rxt(k,752) + rxt(k,803) &
                  + rxt(k,809) + rxt(k,815) + het_rates(k,238))* y(k,238)
         prod(k,82) =rxt(k,228)*y(k,237)*y(k,152)
         loss(k,107) = (rxt(k,500)* y(k,167) + rxt(k,91) + het_rates(k,239))* y(k,239)
         prod(k,107) = (.500_r8*rxt(k,591)*y(k,225) +rxt(k,597)*y(k,227) + &
                 rxt(k,598)*y(k,230) +rxt(k,599)*y(k,231))*y(k,167) &
                  +rxt(k,476)*y(k,237)*y(k,36)
         loss(k,162) = (rxt(k,701)* y(k,62) +rxt(k,702)* y(k,105) +rxt(k,703) &
                 * y(k,151) +rxt(k,704)* y(k,153) + het_rates(k,240))* y(k,240)
         prod(k,162) = (rxt(k,672)*y(k,8) +rxt(k,677)*y(k,136) + &
                 .500_r8*rxt(k,699)*y(k,212))*y(k,237) +rxt(k,705)*y(k,241)*y(k,167)
         loss(k,118) = (rxt(k,705)* y(k,167) + rxt(k,92) + rxt(k,753) &
                  + het_rates(k,241))* y(k,241)
         prod(k,118) =rxt(k,702)*y(k,240)*y(k,105)
         loss(k,165) = (rxt(k,330)* y(k,24) +rxt(k,326)* y(k,25) +rxt(k,384)* y(k,51) &
                  +rxt(k,283)* y(k,70) +rxt(k,277)* y(k,71) +rxt(k,174)* y(k,92) &
                  +rxt(k,173)* y(k,94) +rxt(k,334)* y(k,96) +rxt(k,287)* y(k,100) &
                  +rxt(k,181)* y(k,105) +rxt(k,336)* y(k,107) +rxt(k,291)* y(k,109) &
                  +rxt(k,218)* y(k,151) + (rxt(k,204) +rxt(k,206))* y(k,152) &
                  +rxt(k,212)* y(k,153) +rxt(k,163)* y(k,161) +rxt(k,170)* y(k,163) &
                  +rxt(k,192)* y(k,167) + rxt(k,164) + het_rates(k,242))* y(k,242)
         prod(k,165) = (rxt(k,25) +rxt(k,220)*y(k,139))*y(k,234) + (rxt(k,167) + &
                 rxt(k,168))*y(k,243) +rxt(k,24)*y(k,233) +rxt(k,26)*y(k,235) &
                  +rxt(k,28)*y(k,236) +rxt(k,30)*y(k,237) +rxt(k,10)*y(k,245)
         loss(k,146) = (rxt(k,306)* y(k,42) +rxt(k,307)* y(k,43) +rxt(k,343)* y(k,44) &
                  +rxt(k,308)* y(k,45) +rxt(k,309)* y(k,46) +rxt(k,310)* y(k,47) &
                  +rxt(k,311)* y(k,48) +rxt(k,312)* y(k,49) +rxt(k,374)* y(k,50) &
                  +rxt(k,375)* y(k,52) + (rxt(k,431) +rxt(k,432) +rxt(k,433))* y(k,65) &
                  +rxt(k,344)* y(k,66) +rxt(k,355)* y(k,75) +rxt(k,356)* y(k,76) &
                  +rxt(k,165)* y(k,92) +rxt(k,345)* y(k,93) + (rxt(k,346) +rxt(k,347)) &
                 * y(k,96) +rxt(k,376)* y(k,97) +rxt(k,377)* y(k,98) +rxt(k,378) &
                 * y(k,99) + (rxt(k,313) +rxt(k,314))* y(k,100) +rxt(k,434)* y(k,101) &
                  + (rxt(k,258) +rxt(k,259))* y(k,140) +rxt(k,169)* y(k,163) &
                  +rxt(k,166)* y(k,260) + rxt(k,167) + rxt(k,168) + het_rates(k,243)) &
                 * y(k,243)
         prod(k,146) =rxt(k,9)*y(k,245)
         loss(k,241) = (rxt(k,600)* y(k,57) +rxt(k,601)* y(k,62) +rxt(k,602)* y(k,105) &
                  +rxt(k,603)* y(k,151) +rxt(k,604)* y(k,153) + het_rates(k,244)) &
                 * y(k,244)
         prod(k,241) = (rxt(k,553)*y(k,111) +rxt(k,554)*y(k,113) + &
                 rxt(k,555)*y(k,114) +.400_r8*rxt(k,584)*y(k,123) + &
                 .500_r8*rxt(k,607)*y(k,247))*y(k,167)
         loss(k,164) = (rxt(k,673)* y(k,8) +rxt(k,331)* y(k,22) +rxt(k,439)* y(k,31) &
                  +rxt(k,477)* y(k,36) +rxt(k,284)* y(k,67) +rxt(k,184)* y(k,91) &
                  +rxt(k,182)* y(k,105) +rxt(k,586)* y(k,115) +rxt(k,521)* y(k,126) &
                  +rxt(k,678)* y(k,136) +rxt(k,538)* y(k,138) +rxt(k,217)* y(k,151) &
                  +rxt(k,205)* y(k,152) +rxt(k,160)* y(k,161) +rxt(k,158)* y(k,162) &
                  +rxt(k,193)* y(k,167) +rxt(k,649)* y(k,171) +rxt(k,724)* y(k,181) &
                  +rxt(k,723)* y(k,183) + rxt(k,9) + rxt(k,10) + het_rates(k,245)) &
                 * y(k,245)
         prod(k,164) =rxt(k,164)*y(k,242)
         loss(k,108) = ( + rxt(k,93) + rxt(k,754) + het_rates(k,246))* y(k,246)
         prod(k,108) =rxt(k,543)*y(k,223)*y(k,167) +.100_r8*rxt(k,689)*y(k,233) &
                 *y(k,207)
         loss(k,98) = (rxt(k,607)* y(k,167) + rxt(k,94) + het_rates(k,247))* y(k,247)
         prod(k,98) =rxt(k,602)*y(k,244)*y(k,105)
         loss(k,99) = (rxt(k,473)* y(k,167) + rxt(k,95) + rxt(k,472) &
                  + het_rates(k,248))* y(k,248)
         prod(k,99) =rxt(k,470)*y(k,234)*y(k,57)
         loss(k,68) = ( + rxt(k,658) + het_rates(k,249))* y(k,249)
         prod(k,68) =rxt(k,651)*y(k,234)*y(k,1)
         loss(k,141) = (rxt(k,706)* y(k,167) + rxt(k,96) + rxt(k,755) &
                  + het_rates(k,250))* y(k,250)
         prod(k,141) = (.500_r8*rxt(k,701)*y(k,62) +.200_r8*rxt(k,703)*y(k,151)) &
                 *y(k,240) +.200_r8*rxt(k,695)*y(k,233)*y(k,210)
         loss(k,109) = ((rxt(k,659) +rxt(k,781))* y(k,167) + het_rates(k,251)) &
                 * y(k,251)
         prod(k,109) = 0._r8
         loss(k,202) = (rxt(k,660)* y(k,105) +rxt(k,661)* y(k,151) + het_rates(k,252)) &
                 * y(k,252)
         prod(k,202) = (.560_r8*rxt(k,659)*y(k,251) +rxt(k,663)*y(k,253))*y(k,167)
         loss(k,195) = (rxt(k,663)* y(k,167) + rxt(k,97) + het_rates(k,253))* y(k,253)
         prod(k,195) =rxt(k,660)*y(k,252)*y(k,105)
         loss(k,64) = (rxt(k,782)* y(k,105) +rxt(k,783)* y(k,151) + het_rates(k,254)) &
                 * y(k,254)
         prod(k,64) =rxt(k,781)*y(k,251)*y(k,167)
         loss(k,110) = (rxt(k,667)* y(k,167) + het_rates(k,255))* y(k,255)
         prod(k,110) =.150_r8*rxt(k,659)*y(k,251)*y(k,167)
         loss(k,185) = (rxt(k,664)* y(k,105) +rxt(k,665)* y(k,151) + het_rates(k,256)) &
                 * y(k,256)
         prod(k,185) = (.300_r8*rxt(k,667)*y(k,255) +rxt(k,668)*y(k,257))*y(k,167)
         loss(k,156) = (rxt(k,668)* y(k,167) + rxt(k,98) + het_rates(k,257))* y(k,257)
         prod(k,156) =rxt(k,664)*y(k,256)*y(k,105)
         loss(k,270) = (rxt(k,350)* y(k,88) +rxt(k,154)* y(k,162) +rxt(k,727) &
                 * y(k,185) + rxt(k,1) + rxt(k,2) + rxt(k,3) + het_rates(k,260)) &
                 * y(k,260)
         prod(k,270) = (rxt(k,185)*y(k,92) +rxt(k,186)*y(k,94) +rxt(k,187)*y(k,105) + &
                 rxt(k,190)*y(k,167) +rxt(k,195)*y(k,106) +rxt(k,233)*y(k,104) + &
                 rxt(k,286)*y(k,100) +rxt(k,290)*y(k,109) +rxt(k,333)*y(k,96) + &
                 rxt(k,358)*y(k,50) +rxt(k,360)*y(k,52) +rxt(k,361)*y(k,53) + &
                 rxt(k,363)*y(k,55) +rxt(k,368)*y(k,99) +rxt(k,382)*y(k,51) + &
                 rxt(k,391)*y(k,64) +rxt(k,392)*y(k,65) +rxt(k,395)*y(k,102) + &
                 rxt(k,448)*y(k,35) +rxt(k,450)*y(k,54) +rxt(k,458)*y(k,60) + &
                 rxt(k,459)*y(k,61) +rxt(k,482)*y(k,38) +rxt(k,483)*y(k,39) + &
                 rxt(k,485)*y(k,59) +rxt(k,492)*y(k,178) +rxt(k,497)*y(k,180) + &
                 rxt(k,499)*y(k,58) +.500_r8*rxt(k,519)*y(k,126) +rxt(k,728)*y(k,147)) &
                 *y(k,167) + (rxt(k,785)*y(k,109) +rxt(k,791)*y(k,109) + &
                 rxt(k,792)*y(k,107) +rxt(k,796)*y(k,109) +rxt(k,797)*y(k,107)) &
                 *y(k,100) +.050_r8*rxt(k,55)*y(k,65) +rxt(k,177)*y(k,105)*y(k,91) &
                  +rxt(k,138)*y(k,95)
      end do

      end subroutine imp_prod_loss

      end module mo_prod_loss
