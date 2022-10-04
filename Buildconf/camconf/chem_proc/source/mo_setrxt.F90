
      module mo_setrxt

      use shr_kind_mod, only : r8 => shr_kind_r8

      private
      public :: setrxt
      public :: setrxt_hrates

      contains

      subroutine setrxt( rate, temp, m, ncol )
 
      use ppgrid, only : pcols, pver


      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol*pver)
      real(r8), intent(inout) :: rate(ncol*pver,max(1,rxntot))

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer   ::  n
      integer   ::  offset
      real(r8)  :: itemp(ncol*pver)
      real(r8)  :: exp_fac(ncol*pver)
      real(r8)  :: ko(ncol*pver)
      real(r8)  :: kinf(ncol*pver)

      rate(:,153) = 1.2e-10_r8
      rate(:,157) = 1.2e-10_r8
      rate(:,158) = 1.2e-10_r8
      rate(:,165) = 1.2e-10_r8
      rate(:,169) = 1.2e-10_r8
      rate(:,175) = 6.9e-12_r8
      rate(:,176) = 7.2e-11_r8
      rate(:,177) = 1.6e-12_r8
      rate(:,186) = 1.8e-12_r8
      rate(:,190) = 1.8e-12_r8
      rate(:,207) = 3.5e-12_r8
      rate(:,209) = 1e-11_r8
      rate(:,210) = 2.2e-11_r8
      rate(:,212) = 1e-11_r8
      rate(:,213) = 5e-11_r8
      rate(:,250) = 3.5e-12_r8
      rate(:,252) = 1e-11_r8
      rate(:,253) = 2.2e-11_r8
      rate(:,254) = 5e-11_r8
      rate(:,289) = 1.7e-13_r8
      rate(:,291) = 1.7e-13_r8
      rate(:,292) = 2.607e-10_r8
      rate(:,293) = 9.75e-11_r8
      rate(:,294) = 2.07e-10_r8
      rate(:,295) = 2.088e-10_r8
      rate(:,296) = 1.17e-10_r8
      rate(:,297) = 4.644e-11_r8
      rate(:,298) = 1.204e-10_r8
      rate(:,299) = 9.9e-11_r8
      rate(:,300) = 3.3e-12_r8
      rate(:,306) = 2.607e-10_r8
      rate(:,307) = 9.75e-11_r8
      rate(:,308) = 2.07e-10_r8
      rate(:,309) = 2.088e-10_r8
      rate(:,310) = 1.17e-10_r8
      rate(:,311) = 4.644e-11_r8
      rate(:,312) = 1.204e-10_r8
      rate(:,313) = 9.9e-11_r8
      rate(:,314) = 3.3e-12_r8
      rate(:,337) = 4.5e-11_r8
      rate(:,338) = 4.62e-10_r8
      rate(:,339) = 1.2e-10_r8
      rate(:,340) = 9e-11_r8
      rate(:,341) = 3e-11_r8
      rate(:,343) = 4.5e-11_r8
      rate(:,344) = 4.62e-10_r8
      rate(:,345) = 1.2e-10_r8
      rate(:,346) = 9e-11_r8
      rate(:,347) = 3e-11_r8
      rate(:,353) = 2.14e-11_r8
      rate(:,354) = 1.9e-10_r8
      rate(:,355) = 2.14e-11_r8
      rate(:,356) = 1.9e-10_r8
      rate(:,369) = 2.57e-10_r8
      rate(:,370) = 1.8e-10_r8
      rate(:,371) = 1.794e-10_r8
      rate(:,372) = 1.3e-10_r8
      rate(:,373) = 7.65e-11_r8
      rate(:,374) = 2.57e-10_r8
      rate(:,375) = 1.8e-10_r8
      rate(:,376) = 1.794e-10_r8
      rate(:,377) = 1.3e-10_r8
      rate(:,378) = 7.65e-11_r8
      rate(:,395) = 4e-13_r8
      rate(:,400) = 1.31e-10_r8
      rate(:,401) = 3.5e-11_r8
      rate(:,402) = 9e-12_r8
      rate(:,431) = 1.31e-10_r8
      rate(:,432) = 3.5e-11_r8
      rate(:,433) = 9e-12_r8
      rate(:,440) = 6.8e-14_r8
      rate(:,441) = 2e-13_r8
      rate(:,458) = 7e-13_r8
      rate(:,459) = 1e-12_r8
      rate(:,464) = 1e-14_r8
      rate(:,465) = 1e-11_r8
      rate(:,466) = 1.15e-11_r8
      rate(:,467) = 4e-14_r8
      rate(:,473) = 4e-14_r8
      rate(:,487) = 3e-12_r8
      rate(:,488) = 6.7e-13_r8
      rate(:,500) = 6.7e-13_r8
      rate(:,501) = 3.5e-13_r8
      rate(:,502) = 5.4e-11_r8
      rate(:,503) = 3.5e-13_r8
      rate(:,508) = 2e-12_r8
      rate(:,509) = 1.4e-11_r8
      rate(:,512) = 2.4e-12_r8
      rate(:,515) = 2.4e-12_r8
      rate(:,527) = 5e-12_r8
      rate(:,529) = 5e-12_r8
      rate(:,543) = 2e-12_r8
      rate(:,545) = 1.6e-12_r8
      rate(:,547) = 6.7e-12_r8
      rate(:,549) = 6.7e-12_r8
      rate(:,552) = 3.5e-12_r8
      rate(:,555) = 1.3e-11_r8
      rate(:,556) = 1.4e-11_r8
      rate(:,560) = 2.4e-12_r8
      rate(:,562) = 2.4e-12_r8
      rate(:,563) = 1.4e-11_r8
      rate(:,568) = 2.4e-12_r8
      rate(:,570) = 2.4e-12_r8
      rate(:,571) = 4e-11_r8
      rate(:,572) = 4e-11_r8
      rate(:,574) = 1.4e-11_r8
      rate(:,578) = 2.4e-12_r8
      rate(:,580) = 2.4e-12_r8
      rate(:,581) = 4e-11_r8
      rate(:,587) = 7e-11_r8
      rate(:,588) = 1e-10_r8
      rate(:,589) = 1.6e-12_r8
      rate(:,590) = 4e-11_r8
      rate(:,591) = 4e-11_r8
      rate(:,592) = 1.4e-11_r8
      rate(:,596) = 2.4e-12_r8
      rate(:,597) = 4e-11_r8
      rate(:,598) = 7e-11_r8
      rate(:,599) = 1e-10_r8
      rate(:,604) = 2.4e-12_r8
      rate(:,606) = 2.4e-12_r8
      rate(:,625) = 4.7e-11_r8
      rate(:,645) = 2.1e-12_r8
      rate(:,646) = 2.8e-13_r8
      rate(:,648) = 2.1e-12_r8
      rate(:,649) = 2.8e-13_r8
      rate(:,659) = 1.7e-11_r8
      rate(:,667) = 8.4e-11_r8
      rate(:,669) = 1.9e-11_r8
      rate(:,670) = 1.2e-14_r8
      rate(:,671) = 2e-10_r8
      rate(:,672) = 1.9e-11_r8
      rate(:,673) = 1.2e-14_r8
      rate(:,682) = 2.4e-12_r8
      rate(:,684) = 2.4e-12_r8
      rate(:,685) = 2e-11_r8
      rate(:,690) = 2.3e-11_r8
      rate(:,691) = 2e-11_r8
      rate(:,696) = 3.3e-11_r8
      rate(:,697) = 1e-12_r8
      rate(:,698) = 5.7e-11_r8
      rate(:,699) = 1e-12_r8
      rate(:,700) = 3.4e-11_r8
      rate(:,704) = 2.4e-12_r8
      rate(:,705) = 2e-11_r8
      rate(:,706) = 2e-11_r8
      rate(:,712) = 2.3e-12_r8
      rate(:,713) = 1.2e-11_r8
      rate(:,714) = 5.7e-11_r8
      rate(:,715) = 2.8e-11_r8
      rate(:,716) = 6.6e-11_r8
      rate(:,717) = 1.4e-11_r8
      rate(:,720) = 1.9e-12_r8
      rate(:,722) = 1.4e-11_r8
      rate(:,724) = 1.2e-11_r8
      rate(:,737) = 6.34e-08_r8
      rate(:,756) = 1.9e-11_r8
      rate(:,759) = 1.2e-14_r8
      rate(:,760) = 2e-10_r8
      rate(:,771) = 1.34e-11_r8
      rate(:,777) = 1.34e-11_r8
      rate(:,781) = 1.7e-11_r8
      rate(:,801) = 1._r8
      rate(:,802) = 1._r8
      rate(:,803) = 1._r8
      rate(:,804) = 1._r8
      rate(:,805) = 1._r8
      rate(:,806) = 1._r8
      rate(:,807) = 1._r8
      rate(:,808) = 1._r8
      rate(:,809) = 1._r8
      rate(:,810) = 1._r8
      rate(:,811) = 1._r8
      rate(:,812) = 1._r8
      rate(:,813) = 1._r8
      rate(:,814) = 1._r8
      rate(:,815) = 1._r8
      rate(:,816) = 1._r8
      rate(:,817) = 1._r8
      rate(:,818) = 1._r8
      rate(:,819) = 1.29e-07_r8
      rate(:,820) = 2.31e-07_r8
      rate(:,821) = 2.31e-06_r8
      rate(:,822) = 4.63e-07_r8
 
      do n = 1,pver
        offset = (n-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,n)
      end do

      exp_fac(:) = exp( 60._r8 * itemp(:) )
      rate(:,154) = 1.63e-10_r8 * exp_fac(:)
      rate(:,166) = 1.63e-10_r8 * exp_fac(:)
      exp_fac(:) = exp( 110._r8 * itemp(:) )
      rate(:,155) = 2.15e-11_r8 * exp_fac(:)
      rate(:,167) = 2.15e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 55._r8 * itemp(:) )
      rate(:,156) = 3.3e-11_r8 * exp_fac(:)
      rate(:,168) = 3.3e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( -2060._r8 * itemp(:) )
      rate(:,159) = 8e-12_r8 * exp_fac(:)
      rate(:,160) = 8e-12_r8 * exp_fac(:)
      rate(:,170) = 8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -4570._r8 * itemp(:) )
      rate(:,171) = 1.6e-11_r8 * exp_fac(:)
      rate(:,174) = 1.6e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( -2000._r8 * itemp(:) )
      rate(:,172) = 1.4e-12_r8 * exp_fac(:)
      rate(:,173) = 1.4e-12_r8 * exp_fac(:)
      rate(:,582) = 1.05e-14_r8 * exp_fac(:)
      rate(:,586) = 1.05e-14_r8 * exp_fac(:)
      rate(:,767) = 1.05e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 200._r8 * itemp(:) )
      rate(:,179) = 3e-11_r8 * exp_fac(:)
      rate(:,181) = 3e-11_r8 * exp_fac(:)
      rate(:,333) = 5.5e-12_r8 * exp_fac(:)
      rate(:,391) = 3.8e-12_r8 * exp_fac(:)
      rate(:,446) = 3.8e-12_r8 * exp_fac(:)
      rate(:,482) = 3.8e-12_r8 * exp_fac(:)
      rate(:,492) = 3.8e-12_r8 * exp_fac(:)
      rate(:,497) = 3.8e-12_r8 * exp_fac(:)
      rate(:,520) = 2.3e-11_r8 * exp_fac(:)
      rate(:,534) = 3.8e-12_r8 * exp_fac(:)
      rate(:,551) = 3.8e-12_r8 * exp_fac(:)
      rate(:,584) = 1.52e-11_r8 * exp_fac(:)
      rate(:,607) = 1.52e-12_r8 * exp_fac(:)
      rate(:,615) = 3.8e-12_r8 * exp_fac(:)
      rate(:,618) = 3.8e-12_r8 * exp_fac(:)
      rate(:,624) = 3.8e-12_r8 * exp_fac(:)
      rate(:,647) = 3.8e-12_r8 * exp_fac(:)
      rate(:,655) = 3.8e-12_r8 * exp_fac(:)
      rate(:,663) = 3.8e-12_r8 * exp_fac(:)
      rate(:,668) = 3.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -490._r8 * itemp(:) )
      rate(:,180) = 1e-14_r8 * exp_fac(:)
      rate(:,182) = 1e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( -470._r8 * itemp(:) )
      rate(:,183) = 1.4e-10_r8 * exp_fac(:)
      rate(:,184) = 1.4e-10_r8 * exp_fac(:)
      rate(:,185) = 2.8e-12_r8 * exp( -1800._r8 * itemp(:) )
      exp_fac(:) = exp( 250._r8 * itemp(:) )
      rate(:,187) = 4.8e-11_r8 * exp_fac(:)
      rate(:,328) = 1.7e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 180._r8 * itemp(:) )
      rate(:,188) = 1.8e-11_r8 * exp_fac(:)
      rate(:,192) = 1.8e-11_r8 * exp_fac(:)
      rate(:,461) = 4.2e-12_r8 * exp_fac(:)
      rate(:,462) = 4.2e-12_r8 * exp_fac(:)
      rate(:,480) = 4.2e-12_r8 * exp_fac(:)
      rate(:,481) = 4.2e-12_r8 * exp_fac(:)
      rate(:,490) = 4.2e-12_r8 * exp_fac(:)
      rate(:,491) = 4.2e-12_r8 * exp_fac(:)
      rate(:,531) = 4.2e-12_r8 * exp_fac(:)
      rate(:,532) = 4.2e-12_r8 * exp_fac(:)
      rate(:,559) = 4.4e-12_r8 * exp_fac(:)
      rate(:,561) = 4.4e-12_r8 * exp_fac(:)
      rate(:,567) = 4.4e-12_r8 * exp_fac(:)
      rate(:,569) = 4.4e-12_r8 * exp_fac(:)
      rate(:,681) = 4.2e-12_r8 * exp_fac(:)
      rate(:,683) = 4.2e-12_r8 * exp_fac(:)
      rate(:,688) = 4.2e-12_r8 * exp_fac(:)
      rate(:,689) = 4.2e-12_r8 * exp_fac(:)
      rate(:,694) = 4.2e-12_r8 * exp_fac(:)
      rate(:,695) = 4.2e-12_r8 * exp_fac(:)
      rate(:,703) = 4.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -940._r8 * itemp(:) )
      rate(:,189) = 1.7e-12_r8 * exp_fac(:)
      rate(:,193) = 1.7e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 380._r8 * itemp(:) )
      rate(:,195) = 1.3e-12_r8 * exp_fac(:)
      rate(:,241) = 1.3e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 100._r8 * itemp(:) )
      rate(:,196) = 2.1e-11_r8 * exp_fac(:)
      rate(:,219) = 2.1e-11_r8 * exp_fac(:)
      rate(:,242) = 2.1e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 220._r8 * itemp(:) )
      rate(:,197) = 2.9e-12_r8 * exp_fac(:)
      rate(:,198) = 1.45e-12_r8 * exp_fac(:)
      rate(:,199) = 1.45e-12_r8 * exp_fac(:)
      rate(:,220) = 2.9e-12_r8 * exp_fac(:)
      rate(:,221) = 1.45e-12_r8 * exp_fac(:)
      rate(:,222) = 1.45e-12_r8 * exp_fac(:)
      rate(:,243) = 2.9e-12_r8 * exp_fac(:)
      rate(:,244) = 1.45e-12_r8 * exp_fac(:)
      rate(:,245) = 1.45e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -3600._r8 * itemp(:) )
      rate(:,200) = 1.5e-11_r8 * exp_fac(:)
      rate(:,246) = 1.5e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 210._r8 * itemp(:) )
      rate(:,201) = 5.1e-12_r8 * exp_fac(:)
      rate(:,204) = 5.1e-12_r8 * exp_fac(:)
      rate(:,247) = 5.1e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -2450._r8 * itemp(:) )
      rate(:,202) = 1.2e-13_r8 * exp_fac(:)
      rate(:,205) = 1.2e-13_r8 * exp_fac(:)
      rate(:,248) = 1.2e-13_r8 * exp_fac(:)
      rate(:,268) = 3e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 170._r8 * itemp(:) )
      rate(:,208) = 1.5e-11_r8 * exp_fac(:)
      rate(:,211) = 1.5e-11_r8 * exp_fac(:)
      rate(:,251) = 1.5e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 270._r8 * itemp(:) )
      rate(:,214) = 3.3e-12_r8 * exp_fac(:)
      rate(:,255) = 3.3e-12_r8 * exp_fac(:)
      rate(:,264) = 1.4e-11_r8 * exp_fac(:)
      rate(:,279) = 7.4e-12_r8 * exp_fac(:)
      rate(:,456) = 8.1e-12_r8 * exp_fac(:)
      rate(:,457) = 8.1e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -1500._r8 * itemp(:) )
      rate(:,215) = 3e-12_r8 * exp_fac(:)
      rate(:,217) = 3e-12_r8 * exp_fac(:)
      rate(:,256) = 3e-12_r8 * exp_fac(:)
      rate(:,332) = 5.8e-12_r8 * exp_fac(:)
      rate(:,334) = 5.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 20._r8 * itemp(:) )
      rate(:,223) = 7.26e-11_r8 * exp_fac(:)
      rate(:,224) = 4.64e-11_r8 * exp_fac(:)
      rate(:,258) = 7.26e-11_r8 * exp_fac(:)
      rate(:,259) = 4.64e-11_r8 * exp_fac(:)
      rate(:,260) = 8.1e-11_r8 * exp( -30._r8 * itemp(:) )
      rate(:,261) = 7.1e-12_r8 * exp( -1270._r8 * itemp(:) )
      rate(:,262) = 3.05e-11_r8 * exp( -2270._r8 * itemp(:) )
      rate(:,263) = 1.1e-11_r8 * exp( -980._r8 * itemp(:) )
      rate(:,265) = 3.6e-11_r8 * exp( -375._r8 * itemp(:) )
      exp_fac(:) = exp( -200._r8 * itemp(:) )
      rate(:,266) = 2.3e-11_r8 * exp_fac(:)
      rate(:,284) = 2.3e-11_r8 * exp_fac(:)
      rate(:,267) = 3.3e-12_r8 * exp( -115._r8 * itemp(:) )
      rate(:,269) = 1e-12_r8 * exp( -1590._r8 * itemp(:) )
      rate(:,270) = 3.5e-13_r8 * exp( -1370._r8 * itemp(:) )
      exp_fac(:) = exp( 290._r8 * itemp(:) )
      rate(:,271) = 2.6e-12_r8 * exp_fac(:)
      rate(:,272) = 6.4e-12_r8 * exp_fac(:)
      rate(:,281) = 6.4e-12_r8 * exp_fac(:)
      rate(:,321) = 4.1e-13_r8 * exp_fac(:)
      rate(:,609) = 7.5e-12_r8 * exp_fac(:)
      rate(:,610) = 7.5e-12_r8 * exp_fac(:)
      rate(:,627) = 7.5e-12_r8 * exp_fac(:)
      rate(:,629) = 7.5e-12_r8 * exp_fac(:)
      rate(:,632) = 7.5e-12_r8 * exp_fac(:)
      rate(:,634) = 7.5e-12_r8 * exp_fac(:)
      rate(:,637) = 7.5e-12_r8 * exp_fac(:)
      rate(:,639) = 7.5e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 135._r8 * itemp(:) )
      rate(:,273) = 6.5e-12_r8 * exp_fac(:)
      rate(:,303) = 6.5e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -840._r8 * itemp(:) )
      rate(:,275) = 3.6e-12_r8 * exp_fac(:)
      rate(:,277) = 3.6e-12_r8 * exp_fac(:)
      rate(:,304) = 3.6e-12_r8 * exp_fac(:)
      rate(:,358) = 2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -330._r8 * itemp(:) )
      rate(:,276) = 1.2e-12_r8 * exp_fac(:)
      rate(:,305) = 1.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 85._r8 * itemp(:) )
      rate(:,278) = 2.8e-11_r8 * exp_fac(:)
      rate(:,283) = 2.8e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 230._r8 * itemp(:) )
      rate(:,280) = 6e-13_r8 * exp_fac(:)
      rate(:,318) = 1.5e-12_r8 * exp_fac(:)
      rate(:,327) = 1.9e-11_r8 * exp_fac(:)
      rate(:,330) = 1.9e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( -3300._r8 * itemp(:) )
      rate(:,285) = 1e-11_r8 * exp_fac(:)
      rate(:,287) = 1e-11_r8 * exp_fac(:)
      rate(:,286) = 1.8e-12_r8 * exp( -250._r8 * itemp(:) )
      rate(:,288) = 3.4e-12_r8 * exp( -130._r8 * itemp(:) )
      exp_fac(:) = exp( -500._r8 * itemp(:) )
      rate(:,290) = 3e-12_r8 * exp_fac(:)
      rate(:,349) = 1.4e-10_r8 * exp_fac(:)
      exp_fac(:) = exp( -800._r8 * itemp(:) )
      rate(:,315) = 1.7e-11_r8 * exp_fac(:)
      rate(:,357) = 6.3e-12_r8 * exp_fac(:)
      rate(:,316) = 4.8e-12_r8 * exp( -310._r8 * itemp(:) )
      exp_fac(:) = exp( -780._r8 * itemp(:) )
      rate(:,317) = 1.6e-11_r8 * exp_fac(:)
      rate(:,331) = 1.6e-11_r8 * exp_fac(:)
      rate(:,319) = 9.5e-13_r8 * exp( 550._r8 * itemp(:) )
      exp_fac(:) = exp( 260._r8 * itemp(:) )
      rate(:,320) = 2.3e-12_r8 * exp_fac(:)
      rate(:,323) = 8.8e-12_r8 * exp_fac(:)
      rate(:,322) = 4.5e-12_r8 * exp( 460._r8 * itemp(:) )
      exp_fac(:) = exp( 215._r8 * itemp(:) )
      rate(:,325) = 1.9e-11_r8 * exp_fac(:)
      rate(:,326) = 1.9e-11_r8 * exp_fac(:)
      rate(:,342) = 1.9e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( -430._r8 * itemp(:) )
      rate(:,335) = 1.2e-10_r8 * exp_fac(:)
      rate(:,336) = 1.2e-10_r8 * exp_fac(:)
      rate(:,348) = 1.6e-10_r8 * exp( -260._r8 * itemp(:) )
      exp_fac(:) = exp( 0._r8 * itemp(:) )
      rate(:,350) = 1.4e-11_r8 * exp_fac(:)
      rate(:,353) = 2.14e-11_r8 * exp_fac(:)
      rate(:,354) = 1.9e-10_r8 * exp_fac(:)
      rate(:,355) = 2.14e-11_r8 * exp_fac(:)
      rate(:,356) = 1.9e-10_r8 * exp_fac(:)
      rate(:,369) = 2.57e-10_r8 * exp_fac(:)
      rate(:,370) = 1.8e-10_r8 * exp_fac(:)
      rate(:,371) = 1.794e-10_r8 * exp_fac(:)
      rate(:,372) = 1.3e-10_r8 * exp_fac(:)
      rate(:,373) = 7.65e-11_r8 * exp_fac(:)
      rate(:,374) = 2.57e-10_r8 * exp_fac(:)
      rate(:,375) = 1.8e-10_r8 * exp_fac(:)
      rate(:,376) = 1.794e-10_r8 * exp_fac(:)
      rate(:,377) = 1.3e-10_r8 * exp_fac(:)
      rate(:,378) = 7.65e-11_r8 * exp_fac(:)
      rate(:,395) = 4e-13_r8 * exp_fac(:)
      rate(:,400) = 1.31e-10_r8 * exp_fac(:)
      rate(:,401) = 3.5e-11_r8 * exp_fac(:)
      rate(:,402) = 9e-12_r8 * exp_fac(:)
      rate(:,431) = 1.31e-10_r8 * exp_fac(:)
      rate(:,432) = 3.5e-11_r8 * exp_fac(:)
      rate(:,433) = 9e-12_r8 * exp_fac(:)
      rate(:,440) = 6.8e-14_r8 * exp_fac(:)
      rate(:,441) = 2e-13_r8 * exp_fac(:)
      rate(:,458) = 7e-13_r8 * exp_fac(:)
      rate(:,459) = 1e-12_r8 * exp_fac(:)
      rate(:,464) = 1e-14_r8 * exp_fac(:)
      rate(:,465) = 1e-11_r8 * exp_fac(:)
      rate(:,466) = 1.15e-11_r8 * exp_fac(:)
      rate(:,467) = 4e-14_r8 * exp_fac(:)
      rate(:,473) = 4e-14_r8 * exp_fac(:)
      rate(:,487) = 3e-12_r8 * exp_fac(:)
      rate(:,488) = 6.7e-13_r8 * exp_fac(:)
      rate(:,500) = 6.7e-13_r8 * exp_fac(:)
      rate(:,501) = 3.5e-13_r8 * exp_fac(:)
      rate(:,502) = 5.4e-11_r8 * exp_fac(:)
      rate(:,503) = 3.5e-13_r8 * exp_fac(:)
      rate(:,508) = 2e-12_r8 * exp_fac(:)
      rate(:,509) = 1.4e-11_r8 * exp_fac(:)
      rate(:,512) = 2.4e-12_r8 * exp_fac(:)
      rate(:,515) = 2.4e-12_r8 * exp_fac(:)
      rate(:,527) = 5e-12_r8 * exp_fac(:)
      rate(:,529) = 5e-12_r8 * exp_fac(:)
      rate(:,543) = 2e-12_r8 * exp_fac(:)
      rate(:,545) = 1.6e-12_r8 * exp_fac(:)
      rate(:,547) = 6.7e-12_r8 * exp_fac(:)
      rate(:,549) = 6.7e-12_r8 * exp_fac(:)
      rate(:,552) = 3.5e-12_r8 * exp_fac(:)
      rate(:,555) = 1.3e-11_r8 * exp_fac(:)
      rate(:,556) = 1.4e-11_r8 * exp_fac(:)
      rate(:,560) = 2.4e-12_r8 * exp_fac(:)
      rate(:,562) = 2.4e-12_r8 * exp_fac(:)
      rate(:,563) = 1.4e-11_r8 * exp_fac(:)
      rate(:,568) = 2.4e-12_r8 * exp_fac(:)
      rate(:,570) = 2.4e-12_r8 * exp_fac(:)
      rate(:,571) = 4e-11_r8 * exp_fac(:)
      rate(:,572) = 4e-11_r8 * exp_fac(:)
      rate(:,574) = 1.4e-11_r8 * exp_fac(:)
      rate(:,578) = 2.4e-12_r8 * exp_fac(:)
      rate(:,580) = 2.4e-12_r8 * exp_fac(:)
      rate(:,581) = 4e-11_r8 * exp_fac(:)
      rate(:,587) = 7e-11_r8 * exp_fac(:)
      rate(:,588) = 1e-10_r8 * exp_fac(:)
      rate(:,589) = 1.6e-12_r8 * exp_fac(:)
      rate(:,590) = 4e-11_r8 * exp_fac(:)
      rate(:,591) = 4e-11_r8 * exp_fac(:)
      rate(:,592) = 1.4e-11_r8 * exp_fac(:)
      rate(:,596) = 2.4e-12_r8 * exp_fac(:)
      rate(:,597) = 4e-11_r8 * exp_fac(:)
      rate(:,598) = 7e-11_r8 * exp_fac(:)
      rate(:,599) = 1e-10_r8 * exp_fac(:)
      rate(:,604) = 2.4e-12_r8 * exp_fac(:)
      rate(:,606) = 2.4e-12_r8 * exp_fac(:)
      rate(:,625) = 4.7e-11_r8 * exp_fac(:)
      rate(:,645) = 2.1e-12_r8 * exp_fac(:)
      rate(:,646) = 2.8e-13_r8 * exp_fac(:)
      rate(:,648) = 2.1e-12_r8 * exp_fac(:)
      rate(:,649) = 2.8e-13_r8 * exp_fac(:)
      rate(:,659) = 1.7e-11_r8 * exp_fac(:)
      rate(:,667) = 8.4e-11_r8 * exp_fac(:)
      rate(:,669) = 1.9e-11_r8 * exp_fac(:)
      rate(:,670) = 1.2e-14_r8 * exp_fac(:)
      rate(:,671) = 2e-10_r8 * exp_fac(:)
      rate(:,672) = 1.9e-11_r8 * exp_fac(:)
      rate(:,673) = 1.2e-14_r8 * exp_fac(:)
      rate(:,682) = 2.4e-12_r8 * exp_fac(:)
      rate(:,684) = 2.4e-12_r8 * exp_fac(:)
      rate(:,685) = 2e-11_r8 * exp_fac(:)
      rate(:,690) = 2.3e-11_r8 * exp_fac(:)
      rate(:,691) = 2e-11_r8 * exp_fac(:)
      rate(:,696) = 3.3e-11_r8 * exp_fac(:)
      rate(:,697) = 1e-12_r8 * exp_fac(:)
      rate(:,698) = 5.7e-11_r8 * exp_fac(:)
      rate(:,699) = 1e-12_r8 * exp_fac(:)
      rate(:,700) = 3.4e-11_r8 * exp_fac(:)
      rate(:,704) = 2.4e-12_r8 * exp_fac(:)
      rate(:,705) = 2e-11_r8 * exp_fac(:)
      rate(:,706) = 2e-11_r8 * exp_fac(:)
      rate(:,712) = 2.3e-12_r8 * exp_fac(:)
      rate(:,713) = 1.2e-11_r8 * exp_fac(:)
      rate(:,714) = 5.7e-11_r8 * exp_fac(:)
      rate(:,715) = 2.8e-11_r8 * exp_fac(:)
      rate(:,716) = 6.6e-11_r8 * exp_fac(:)
      rate(:,717) = 1.4e-11_r8 * exp_fac(:)
      rate(:,720) = 1.9e-12_r8 * exp_fac(:)
      rate(:,722) = 1.4e-11_r8 * exp_fac(:)
      rate(:,724) = 1.2e-11_r8 * exp_fac(:)
      rate(:,737) = 6.34e-08_r8 * exp_fac(:)
      rate(:,756) = 1.9e-11_r8 * exp_fac(:)
      rate(:,759) = 1.2e-14_r8 * exp_fac(:)
      rate(:,760) = 2e-10_r8 * exp_fac(:)
      rate(:,771) = 1.34e-11_r8 * exp_fac(:)
      rate(:,777) = 1.34e-11_r8 * exp_fac(:)
      rate(:,781) = 1.7e-11_r8 * exp_fac(:)
      rate(:,801) = 1._r8 * exp_fac(:)
      rate(:,802) = 1._r8 * exp_fac(:)
      rate(:,803) = 1._r8 * exp_fac(:)
      rate(:,804) = 1._r8 * exp_fac(:)
      rate(:,805) = 1._r8 * exp_fac(:)
      rate(:,806) = 1._r8 * exp_fac(:)
      rate(:,807) = 1._r8 * exp_fac(:)
      rate(:,808) = 1._r8 * exp_fac(:)
      rate(:,809) = 1._r8 * exp_fac(:)
      rate(:,810) = 1._r8 * exp_fac(:)
      rate(:,811) = 1._r8 * exp_fac(:)
      rate(:,812) = 1._r8 * exp_fac(:)
      rate(:,813) = 1._r8 * exp_fac(:)
      rate(:,814) = 1._r8 * exp_fac(:)
      rate(:,815) = 1._r8 * exp_fac(:)
      rate(:,816) = 1._r8 * exp_fac(:)
      rate(:,817) = 1._r8 * exp_fac(:)
      rate(:,818) = 1._r8 * exp_fac(:)
      rate(:,819) = 1.29e-07_r8 * exp_fac(:)
      rate(:,820) = 2.31e-07_r8 * exp_fac(:)
      rate(:,821) = 2.31e-06_r8 * exp_fac(:)
      rate(:,822) = 4.63e-07_r8 * exp_fac(:)
      exp_fac(:) = exp( 400._r8 * itemp(:) )
      rate(:,351) = 6e-12_r8 * exp_fac(:)
      rate(:,352) = 6e-12_r8 * exp_fac(:)
      rate(:,510) = 5e-13_r8 * exp_fac(:)
      rate(:,557) = 5e-13_r8 * exp_fac(:)
      rate(:,564) = 5e-13_r8 * exp_fac(:)
      rate(:,575) = 5e-13_r8 * exp_fac(:)
      rate(:,593) = 5e-13_r8 * exp_fac(:)
      rate(:,601) = 5e-13_r8 * exp_fac(:)
      rate(:,359) = 1.46e-11_r8 * exp( -1040._r8 * itemp(:) )
      rate(:,360) = 1.42e-12_r8 * exp( -1150._r8 * itemp(:) )
      exp_fac(:) = exp( -1520._r8 * itemp(:) )
      rate(:,361) = 1.64e-12_r8 * exp_fac(:)
      rate(:,536) = 8.5e-16_r8 * exp_fac(:)
      rate(:,538) = 8.5e-16_r8 * exp_fac(:)
      exp_fac(:) = exp( -1100._r8 * itemp(:) )
      rate(:,362) = 2.03e-11_r8 * exp_fac(:)
      rate(:,719) = 3.4e-12_r8 * exp_fac(:)
      rate(:,723) = 3.4e-12_r8 * exp_fac(:)
      rate(:,363) = 1.96e-12_r8 * exp( -1200._r8 * itemp(:) )
      rate(:,364) = 4.85e-12_r8 * exp( -850._r8 * itemp(:) )
      rate(:,365) = 9e-13_r8 * exp( -360._r8 * itemp(:) )
      exp_fac(:) = exp( -1600._r8 * itemp(:) )
      rate(:,366) = 1.25e-12_r8 * exp_fac(:)
      rate(:,381) = 3.4e-11_r8 * exp_fac(:)
      rate(:,384) = 3.4e-11_r8 * exp_fac(:)
      rate(:,367) = 1.3e-12_r8 * exp( -1770._r8 * itemp(:) )
      rate(:,368) = 9.2e-13_r8 * exp( -1560._r8 * itemp(:) )
      rate(:,379) = 9.7e-15_r8 * exp( 625._r8 * itemp(:) )
      exp_fac(:) = exp( -2058._r8 * itemp(:) )
      rate(:,380) = 6e-13_r8 * exp_fac(:)
      rate(:,383) = 6e-13_r8 * exp_fac(:)
      rate(:,382) = 5.5e-12_r8 * exp( 125._r8 * itemp(:) )
      rate(:,385) = 5e-13_r8 * exp( -424._r8 * itemp(:) )
      rate(:,386) = 1.9e-14_r8 * exp( 706._r8 * itemp(:) )
      rate(:,387) = 4.1e-13_r8 * exp( 750._r8 * itemp(:) )
      exp_fac(:) = exp( 300._r8 * itemp(:) )
      rate(:,388) = 2.8e-12_r8 * exp_fac(:)
      rate(:,389) = 2.8e-12_r8 * exp_fac(:)
      rate(:,495) = 2.9e-12_r8 * exp_fac(:)
      rate(:,496) = 2.9e-12_r8 * exp_fac(:)
      rate(:,390) = 2.9e-12_r8 * exp( -345._r8 * itemp(:) )
      rate(:,392) = 2.45e-12_r8 * exp( -1775._r8 * itemp(:) )
      exp_fac(:) = exp( 700._r8 * itemp(:) )
      rate(:,396) = 7.5e-13_r8 * exp_fac(:)
      rate(:,442) = 7.5e-13_r8 * exp_fac(:)
      rate(:,460) = 7.5e-13_r8 * exp_fac(:)
      rate(:,479) = 7.5e-13_r8 * exp_fac(:)
      rate(:,489) = 7.5e-13_r8 * exp_fac(:)
      rate(:,494) = 8.6e-13_r8 * exp_fac(:)
      rate(:,511) = 8e-13_r8 * exp_fac(:)
      rate(:,530) = 7.5e-13_r8 * exp_fac(:)
      rate(:,546) = 7.5e-13_r8 * exp_fac(:)
      rate(:,558) = 8e-13_r8 * exp_fac(:)
      rate(:,565) = 8e-13_r8 * exp_fac(:)
      rate(:,576) = 8e-13_r8 * exp_fac(:)
      rate(:,594) = 8e-13_r8 * exp_fac(:)
      rate(:,602) = 8e-13_r8 * exp_fac(:)
      rate(:,612) = 7.5e-13_r8 * exp_fac(:)
      rate(:,617) = 7.5e-13_r8 * exp_fac(:)
      rate(:,621) = 7.5e-13_r8 * exp_fac(:)
      rate(:,641) = 7.5e-13_r8 * exp_fac(:)
      rate(:,652) = 7.5e-13_r8 * exp_fac(:)
      rate(:,660) = 7.5e-13_r8 * exp_fac(:)
      rate(:,664) = 7.5e-13_r8 * exp_fac(:)
      rate(:,680) = 7.5e-13_r8 * exp_fac(:)
      rate(:,687) = 7.5e-13_r8 * exp_fac(:)
      rate(:,693) = 7.5e-13_r8 * exp_fac(:)
      rate(:,702) = 7.5e-13_r8 * exp_fac(:)
      rate(:,762) = 7.5e-13_r8 * exp_fac(:)
      rate(:,769) = 7.5e-13_r8 * exp_fac(:)
      rate(:,779) = 7.5e-13_r8 * exp_fac(:)
      rate(:,782) = 7.5e-13_r8 * exp_fac(:)
      rate(:,397) = 2.4e+12_r8 * exp( -7000._r8 * itemp(:) )
      exp_fac(:) = exp( 265._r8 * itemp(:) )
      rate(:,398) = 2.6e-12_r8 * exp_fac(:)
      rate(:,399) = 2.6e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 105._r8 * itemp(:) )
      rate(:,403) = 1.08e-10_r8 * exp_fac(:)
      rate(:,434) = 1.08e-10_r8 * exp_fac(:)
      exp_fac(:) = exp( -2630._r8 * itemp(:) )
      rate(:,438) = 1.2e-14_r8 * exp_fac(:)
      rate(:,439) = 1.2e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 365._r8 * itemp(:) )
      rate(:,443) = 2.6e-12_r8 * exp_fac(:)
      rate(:,444) = 2.6e-12_r8 * exp_fac(:)
      rate(:,613) = 2.6e-12_r8 * exp_fac(:)
      rate(:,614) = 2.6e-12_r8 * exp_fac(:)
      rate(:,619) = 2.6e-12_r8 * exp_fac(:)
      rate(:,620) = 2.6e-12_r8 * exp_fac(:)
      rate(:,622) = 2.6e-12_r8 * exp_fac(:)
      rate(:,623) = 2.6e-12_r8 * exp_fac(:)
      rate(:,642) = 2.6e-12_r8 * exp_fac(:)
      rate(:,643) = 2.6e-12_r8 * exp_fac(:)
      rate(:,653) = 2.6e-12_r8 * exp_fac(:)
      rate(:,654) = 2.6e-12_r8 * exp_fac(:)
      rate(:,661) = 2.6e-12_r8 * exp_fac(:)
      rate(:,662) = 2.6e-12_r8 * exp_fac(:)
      rate(:,665) = 2.6e-12_r8 * exp_fac(:)
      rate(:,666) = 2.6e-12_r8 * exp_fac(:)
      rate(:,763) = 2.6e-12_r8 * exp_fac(:)
      rate(:,770) = 2.6e-12_r8 * exp_fac(:)
      rate(:,780) = 2.6e-12_r8 * exp_fac(:)
      rate(:,783) = 2.6e-12_r8 * exp_fac(:)
      rate(:,445) = 6.9e-12_r8 * exp( -230._r8 * itemp(:) )
      rate(:,447) = 7.2e-11_r8 * exp( -70._r8 * itemp(:) )
      rate(:,448) = 7.66e-12_r8 * exp( -1020._r8 * itemp(:) )
      exp_fac(:) = exp( -1900._r8 * itemp(:) )
      rate(:,449) = 1.4e-12_r8 * exp_fac(:)
      rate(:,451) = 1.4e-12_r8 * exp_fac(:)
      rate(:,475) = 6.5e-15_r8 * exp_fac(:)
      rate(:,477) = 6.5e-15_r8 * exp_fac(:)
      exp_fac(:) = exp( 350._r8 * itemp(:) )
      rate(:,450) = 4.63e-12_r8 * exp_fac(:)
      rate(:,766) = 2.7e-12_r8 * exp_fac(:)
      rate(:,452) = 7.8e-13_r8 * exp( -1050._r8 * itemp(:) )
      exp_fac(:) = exp( 500._r8 * itemp(:) )
      rate(:,453) = 2.9e-12_r8 * exp_fac(:)
      rate(:,454) = 2e-12_r8 * exp_fac(:)
      rate(:,493) = 7.1e-13_r8 * exp_fac(:)
      rate(:,523) = 2e-12_r8 * exp_fac(:)
      rate(:,679) = 2e-12_r8 * exp_fac(:)
      rate(:,686) = 2e-12_r8 * exp_fac(:)
      rate(:,692) = 2e-12_r8 * exp_fac(:)
      rate(:,701) = 2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 1040._r8 * itemp(:) )
      rate(:,455) = 4.3e-13_r8 * exp_fac(:)
      rate(:,524) = 4.3e-13_r8 * exp_fac(:)
      rate(:,608) = 4.3e-13_r8 * exp_fac(:)
      rate(:,626) = 4.3e-13_r8 * exp_fac(:)
      rate(:,631) = 4.3e-13_r8 * exp_fac(:)
      rate(:,636) = 4.3e-13_r8 * exp_fac(:)
      rate(:,463) = 1.6e+11_r8 * exp( -4150._r8 * itemp(:) )
      exp_fac(:) = exp( -1156._r8 * itemp(:) )
      rate(:,474) = 4.6e-13_r8 * exp_fac(:)
      rate(:,476) = 4.6e-13_r8 * exp_fac(:)
      rate(:,478) = 3.75e-13_r8 * exp( -40._r8 * itemp(:) )
      rate(:,483) = 8.7e-12_r8 * exp( -615._r8 * itemp(:) )
      exp_fac(:) = exp( -1860._r8 * itemp(:) )
      rate(:,484) = 1.4e-12_r8 * exp_fac(:)
      rate(:,486) = 1.4e-12_r8 * exp_fac(:)
      rate(:,485) = 8.4e-13_r8 * exp( 830._r8 * itemp(:) )
      exp_fac(:) = exp( 120._r8 * itemp(:) )
      rate(:,504) = 4.8e-12_r8 * exp_fac(:)
      rate(:,506) = 4.8e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 693._r8 * itemp(:) )
      rate(:,505) = 5.1e-14_r8 * exp_fac(:)
      rate(:,507) = 5.1e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 360._r8 * itemp(:) )
      rate(:,513) = 2.7e-12_r8 * exp_fac(:)
      rate(:,514) = 1.3e-13_r8 * exp_fac(:)
      rate(:,516) = 2.7e-12_r8 * exp_fac(:)
      rate(:,517) = 1.3e-13_r8 * exp_fac(:)
      rate(:,519) = 9.6e-12_r8 * exp_fac(:)
      rate(:,526) = 5.3e-12_r8 * exp_fac(:)
      rate(:,528) = 5.3e-12_r8 * exp_fac(:)
      rate(:,577) = 2.7e-12_r8 * exp_fac(:)
      rate(:,579) = 2.7e-12_r8 * exp_fac(:)
      rate(:,595) = 2.7e-12_r8 * exp_fac(:)
      rate(:,603) = 2.7e-12_r8 * exp_fac(:)
      rate(:,605) = 2.7e-12_r8 * exp_fac(:)
      rate(:,758) = 2.7e-12_r8 * exp_fac(:)
      rate(:,774) = 2.7e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -2100._r8 * itemp(:) )
      rate(:,518) = 1.5e-15_r8 * exp_fac(:)
      rate(:,521) = 1.5e-15_r8 * exp_fac(:)
      exp_fac(:) = exp( 530._r8 * itemp(:) )
      rate(:,522) = 4.6e-12_r8 * exp_fac(:)
      rate(:,525) = 2.3e-12_r8 * exp_fac(:)
      rate(:,533) = 2.3e-12_r8 * exp( -170._r8 * itemp(:) )
      rate(:,537) = 4.13e-12_r8 * exp( 452._r8 * itemp(:) )
      exp_fac(:) = exp( 870._r8 * itemp(:) )
      rate(:,548) = 5.4e-14_r8 * exp_fac(:)
      rate(:,550) = 5.4e-14_r8 * exp_fac(:)
      exp_fac(:) = exp( 175._r8 * itemp(:) )
      rate(:,553) = 1.86e-11_r8 * exp_fac(:)
      rate(:,554) = 1.86e-11_r8 * exp_fac(:)
      rate(:,566) = 1.6e+09_r8 * exp( -8300._r8 * itemp(:) )
      exp_fac(:) = exp( -446._r8 * itemp(:) )
      rate(:,573) = 3.03e-12_r8 * exp_fac(:)
      rate(:,585) = 3.03e-12_r8 * exp_fac(:)
      rate(:,764) = 3.03e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 410._r8 * itemp(:) )
      rate(:,583) = 2.54e-11_r8 * exp_fac(:)
      rate(:,768) = 2.54e-11_r8 * exp_fac(:)
      rate(:,600) = 1.3e-12_r8 * exp( 640._r8 * itemp(:) )
      exp_fac(:) = exp( -193._r8 * itemp(:) )
      rate(:,611) = 2.3e-12_r8 * exp_fac(:)
      rate(:,761) = 2.3e-12_r8 * exp_fac(:)
      rate(:,616) = 5.9e-12_r8 * exp( 225._r8 * itemp(:) )
      rate(:,644) = 4.7e-13_r8 * exp( 1220._r8 * itemp(:) )
      exp_fac(:) = exp( 352._r8 * itemp(:) )
      rate(:,656) = 1.7e-12_r8 * exp_fac(:)
      rate(:,778) = 1.7e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( 490._r8 * itemp(:) )
      rate(:,674) = 1.2e-12_r8 * exp_fac(:)
      rate(:,677) = 1.2e-12_r8 * exp_fac(:)
      rate(:,772) = 1.2e-12_r8 * exp_fac(:)
      exp_fac(:) = exp( -580._r8 * itemp(:) )
      rate(:,675) = 6.3e-16_r8 * exp_fac(:)
      rate(:,678) = 6.3e-16_r8 * exp_fac(:)
      rate(:,775) = 6.3e-16_r8 * exp_fac(:)
      exp_fac(:) = exp( 440._r8 * itemp(:) )
      rate(:,676) = 1.2e-11_r8 * exp_fac(:)
      rate(:,776) = 1.2e-11_r8 * exp_fac(:)
      exp_fac(:) = exp( 520._r8 * itemp(:) )
      rate(:,707) = 1.9e-13_r8 * exp_fac(:)
      rate(:,709) = 1.9e-13_r8 * exp_fac(:)
      rate(:,708) = 9.6e-12_r8 * exp( -234._r8 * itemp(:) )
      rate(:,710) = 2.1e-11_r8 * exp( -2200._r8 * itemp(:) )
      rate(:,711) = 7.2e-14_r8 * exp( -1070._r8 * itemp(:) )
      rate(:,718) = 1.6e-13_r8 * exp( -2280._r8 * itemp(:) )
      rate(:,721) = 2.7e-11_r8 * exp( 335._r8 * itemp(:) )
      rate(:,728) = 1.7e-12_r8 * exp( -710._r8 * itemp(:) )
      exp_fac(:) = exp( 1300._r8 * itemp(:) )
      rate(:,757) = 2.75e-13_r8 * exp_fac(:)
      rate(:,765) = 2.12e-13_r8 * exp_fac(:)
      rate(:,773) = 2.6e-13_r8 * exp_fac(:)

      itemp(:) = 300._r8 * itemp(:)
 
      n = ncol*pver

      ko(:) = 4.4e-32_r8 * itemp(:)**1.3_r8
      kinf(:) = 7.5e-11_r8 * itemp(:)**(-0.2_r8)
      call jpl( rate(:,178), m, 0.6_r8, ko, kinf, n )

      ko(:) = 6.9e-31_r8 * itemp(:)**1._r8
      kinf(:) = 2.6e-11_r8
      call jpl( rate(:,191), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.5e-31_r8 * itemp(:)**1.8_r8
      kinf(:) = 2.2e-11_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,203), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.5e-31_r8 * itemp(:)**1.8_r8
      kinf(:) = 2.2e-11_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,206), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9e-32_r8 * itemp(:)**1.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,216), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9e-32_r8 * itemp(:)**1.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,218), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 4e-12_r8 * itemp(:)**0.3_r8
      call jpl( rate(:,225), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.4e-30_r8 * itemp(:)**3._r8
      kinf(:) = 1.6e-12_r8 * itemp(:)**(-0.1_r8)
      call jpl( rate(:,226), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-30_r8 * itemp(:)**3._r8
      kinf(:) = 2.8e-11_r8
      call jpl( rate(:,227), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.4e-30_r8 * itemp(:)**3._r8
      kinf(:) = 1.6e-12_r8 * itemp(:)**(-0.1_r8)
      call jpl( rate(:,228), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 4e-12_r8 * itemp(:)**0.3_r8
      call jpl( rate(:,229), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.4e-30_r8 * itemp(:)**3._r8
      kinf(:) = 1.6e-12_r8 * itemp(:)**(-0.1_r8)
      call jpl( rate(:,230), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-30_r8 * itemp(:)**3._r8
      kinf(:) = 2.8e-11_r8
      call jpl( rate(:,231), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.4e-30_r8 * itemp(:)**3._r8
      kinf(:) = 1.6e-12_r8 * itemp(:)**(-0.1_r8)
      call jpl( rate(:,232), m, 0.6_r8, ko, kinf, n )

      ko(:) = 2.5e-31_r8 * itemp(:)**1.8_r8
      kinf(:) = 2.2e-11_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,249), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9e-32_r8 * itemp(:)**1.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,257), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 1.5e-11_r8 * itemp(:)**1.9_r8
      call jpl( rate(:,274), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.8e-31_r8 * itemp(:)**3.4_r8
      kinf(:) = 1.5e-11_r8 * itemp(:)**1.9_r8
      call jpl( rate(:,282), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.9e-32_r8 * itemp(:)**3.6_r8
      kinf(:) = 3.7e-12_r8 * itemp(:)**1.6_r8
      call jpl( rate(:,301), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.2e-31_r8 * itemp(:)**3.2_r8
      kinf(:) = 6.9e-12_r8 * itemp(:)**2.9_r8
      call jpl( rate(:,324), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.2e-31_r8 * itemp(:)**3.2_r8
      kinf(:) = 6.9e-12_r8 * itemp(:)**2.9_r8
      call jpl( rate(:,329), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,393), m, 0.6_r8, ko, kinf, n )

      ko(:) = 4.28e-33_r8
      kinf(:) = 9.3e-15_r8 * itemp(:)**(-4.42_r8)
      call jpl( rate(:,394), m, 0.8_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,406), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,408), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,410), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,412), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,414), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,416), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,418), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,420), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,422), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,424), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,426), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,428), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.9e-33_r8 * itemp(:)**1._r8
      kinf(:) = 1.1e-12_r8 * itemp(:)**(-1.3_r8)
      call jpl( rate(:,430), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.2e-30_r8 * itemp(:)**2.4_r8
      kinf(:) = 2.2e-10_r8 * itemp(:)**0.7_r8
      call jpl( rate(:,435), m, 0.6_r8, ko, kinf, n )

      ko(:) = 5.5e-30_r8
      kinf(:) = 8.3e-13_r8 * itemp(:)**(-2._r8)
      call jpl( rate(:,436), m, 0.6_r8, ko, kinf, n )

      ko(:) = 1.6e-29_r8 * itemp(:)**3.3_r8
      kinf(:) = 3.1e-10_r8 * itemp(:)
      call jpl( rate(:,437), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8.6e-29_r8 * itemp(:)**3.1_r8
      kinf(:) = 9e-12_r8 * itemp(:)**0.85_r8
      call jpl( rate(:,468), m, 0.48_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,469), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,470), m, 0.6_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,498), m, 0.5_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,535), m, 0.5_r8, ko, kinf, n )

      ko(:) = 8e-27_r8 * itemp(:)**3.5_r8
      kinf(:) = 3e-11_r8
      call jpl( rate(:,544), m, 0.5_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,628), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,630), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,633), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,635), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,638), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,640), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,650), m, 0.6_r8, ko, kinf, n )

      ko(:) = 9.7e-29_r8 * itemp(:)**5.6_r8
      kinf(:) = 9.3e-12_r8 * itemp(:)**1.5_r8
      call jpl( rate(:,651), m, 0.6_r8, ko, kinf, n )

      end subroutine setrxt


      subroutine setrxt_hrates( rate, temp, m, ncol, kbot )
 
      use ppgrid, only : pcols, pver


      use chem_mods, only : rxntot
      use mo_jpl,    only : jpl

      implicit none

!-------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      integer, intent(in) :: kbot
      real(r8), intent(in)    :: temp(pcols,pver)
      real(r8), intent(in)    :: m(ncol*pver)
      real(r8), intent(inout) :: rate(ncol*pver,max(1,rxntot))

!-------------------------------------------------------
!       ... local variables
!-------------------------------------------------------
      integer   ::  n
      integer   ::  offset
      integer   ::  k
      real(r8)  :: itemp(ncol*kbot)
      real(r8)  :: exp_fac(ncol*kbot)
      real(r8)  :: ko(ncol*kbot)
      real(r8)  :: kinf(ncol*kbot)
      real(r8)  :: wrk(ncol*kbot)
 
      n = ncol*kbot

      rate(:n,175) = 6.9e-12_r8
 
      do k = 1,kbot
        offset = (k-1)*ncol
        itemp(offset+1:offset+ncol) = 1._r8 / temp(:ncol,k)
      end do

      rate(:n,155) = 2.15e-11_r8 * exp( 110._r8 * itemp(:) )
      rate(:n,159) = 8e-12_r8 * exp( -2060._r8 * itemp(:) )
      rate(:n,179) = 3e-11_r8 * exp( 200._r8 * itemp(:) )
      rate(:n,180) = 1e-14_r8 * exp( -490._r8 * itemp(:) )
      rate(:n,183) = 1.4e-10_r8 * exp( -470._r8 * itemp(:) )
      rate(:n,187) = 4.8e-11_r8 * exp( 250._r8 * itemp(:) )
      rate(:n,188) = 1.8e-11_r8 * exp( 180._r8 * itemp(:) )
      rate(:n,189) = 1.7e-12_r8 * exp( -940._r8 * itemp(:) )
      rate(:n,196) = 2.1e-11_r8 * exp( 100._r8 * itemp(:) )
      rate(:n,200) = 1.5e-11_r8 * exp( -3600._r8 * itemp(:) )
      rate(:n,201) = 5.1e-12_r8 * exp( 210._r8 * itemp(:) )
      rate(:n,214) = 3.3e-12_r8 * exp( 270._r8 * itemp(:) )
      rate(:n,215) = 3e-12_r8 * exp( -1500._r8 * itemp(:) )

      itemp(:) = 300._r8 * itemp(:)

      ko(:) = 4.4e-32_r8 * itemp(:)**1.3_r8
      kinf(:) = 7.5e-11_r8 * itemp(:)**(-0.2_r8)
      call jpl( wrk, m, 0.6_r8, ko, kinf, n )
      rate(:n,178) = wrk(:)





















































      end subroutine setrxt_hrates

      end module mo_setrxt
