      module mo_setrxt
      use shr_kind_mod, only : r8 => shr_kind_r8
      private
      public :: setrxt
      public :: setrxt_hrates
      contains
      subroutine setrxt( rate, temp, m, ncol )
      use ppgrid, only : pcols, pver
      use chem_mods, only : rxntot
      use mo_jpl, only : jpl
      implicit none
!-------------------------------------------------------
! ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      real(r8), intent(in) :: temp(pcols,pver)
      real(r8), intent(in) :: m(ncol*pver)
      real(r8), intent(inout) :: rate(ncol*pver,max(1,rxntot))
!-------------------------------------------------------
! ... local variables
!-------------------------------------------------------
      integer :: n
      integer :: offset
      real(r8) :: itemp(ncol*pver)
      real(r8) :: exp_fac(ncol*pver)
      real(r8) :: ko(ncol*pver)
      real(r8) :: kinf(ncol*pver)
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
      use mo_jpl, only : jpl
      implicit none
!-------------------------------------------------------
! ... dummy arguments
!-------------------------------------------------------
      integer, intent(in) :: ncol
      integer, intent(in) :: kbot
      real(r8), intent(in) :: temp(pcols,pver)
      real(r8), intent(in) :: m(ncol*pver)
      real(r8), intent(inout) :: rate(ncol*pver,max(1,rxntot))
!-------------------------------------------------------
! ... local variables
!-------------------------------------------------------
      integer :: n
      integer :: offset
      integer :: k
      real(r8) :: itemp(ncol*kbot)
      real(r8) :: exp_fac(ncol*kbot)
      real(r8) :: ko(ncol*kbot)
      real(r8) :: kinf(ncol*kbot)
      real(r8) :: wrk(ncol*kbot)
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
      module mo_adjrxt
      private
      public :: adjrxt
      contains
      subroutine adjrxt( rate, inv, m, ncol, nlev )
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only : nfs, rxntot
      implicit none
!--------------------------------------------------------------------
! ... dummy arguments
!--------------------------------------------------------------------
      integer, intent(in) :: ncol, nlev
      real(r8), intent(in) :: inv(ncol,nlev,nfs)
      real(r8), intent(in) :: m(ncol,nlev)
      real(r8), intent(inout) :: rate(ncol,nlev,rxntot)
      rate(:,:, 155) = rate(:,:, 155) * inv(:,:, 3)
      rate(:,:, 156) = rate(:,:, 156) * inv(:,:, 2)
      rate(:,:, 161) = rate(:,:, 161) * inv(:,:, 1)
      rate(:,:, 163) = rate(:,:, 163) * inv(:,:, 1)
      rate(:,:, 167) = rate(:,:, 167) * inv(:,:, 3)
      rate(:,:, 168) = rate(:,:, 168) * inv(:,:, 2)
      rate(:,:, 191) = rate(:,:, 191) * inv(:,:, 1)
      rate(:,:, 200) = rate(:,:, 200) * inv(:,:, 2)
      rate(:,:, 203) = rate(:,:, 203) * inv(:,:, 1)
      rate(:,:, 206) = rate(:,:, 206) * inv(:,:, 1)
      rate(:,:, 216) = rate(:,:, 216) * inv(:,:, 1)
      rate(:,:, 218) = rate(:,:, 218) * inv(:,:, 1)
      rate(:,:, 225) = rate(:,:, 225) * inv(:,:, 1)
      rate(:,:, 226) = rate(:,:, 226) * inv(:,:, 1)
      rate(:,:, 227) = rate(:,:, 227) * inv(:,:, 1)
      rate(:,:, 228) = rate(:,:, 228) * inv(:,:, 1)
      rate(:,:, 229) = rate(:,:, 229) * inv(:,:, 1)
      rate(:,:, 230) = rate(:,:, 230) * inv(:,:, 1)
      rate(:,:, 231) = rate(:,:, 231) * inv(:,:, 1)
      rate(:,:, 232) = rate(:,:, 232) * inv(:,:, 1)
      rate(:,:, 234) = rate(:,:, 234) * inv(:,:, 1)
      rate(:,:, 235) = rate(:,:, 235) * inv(:,:, 1)
      rate(:,:, 237) = rate(:,:, 237) * inv(:,:, 1)
      rate(:,:, 238) = rate(:,:, 238) * inv(:,:, 1)
      rate(:,:, 239) = rate(:,:, 239) * inv(:,:, 1)
      rate(:,:, 240) = rate(:,:, 240) * inv(:,:, 1)
      rate(:,:, 246) = rate(:,:, 246) * inv(:,:, 2)
      rate(:,:, 249) = rate(:,:, 249) * inv(:,:, 1)
      rate(:,:, 257) = rate(:,:, 257) * inv(:,:, 1)
      rate(:,:, 274) = rate(:,:, 274) * inv(:,:, 1)
      rate(:,:, 282) = rate(:,:, 282) * inv(:,:, 1)
      rate(:,:, 301) = rate(:,:, 301) * inv(:,:, 1)
      rate(:,:, 302) = rate(:,:, 302) * inv(:,:, 1)
      rate(:,:, 324) = rate(:,:, 324) * inv(:,:, 1)
      rate(:,:, 329) = rate(:,:, 329) * inv(:,:, 1)
      rate(:,:, 393) = rate(:,:, 393) * inv(:,:, 1)
      rate(:,:, 394) = rate(:,:, 394) * inv(:,:, 1)
      rate(:,:, 406) = rate(:,:, 406) * inv(:,:, 1)
      rate(:,:, 408) = rate(:,:, 408) * inv(:,:, 1)
      rate(:,:, 410) = rate(:,:, 410) * inv(:,:, 1)
      rate(:,:, 412) = rate(:,:, 412) * inv(:,:, 1)
      rate(:,:, 414) = rate(:,:, 414) * inv(:,:, 1)
      rate(:,:, 416) = rate(:,:, 416) * inv(:,:, 1)
      rate(:,:, 418) = rate(:,:, 418) * inv(:,:, 1)
      rate(:,:, 420) = rate(:,:, 420) * inv(:,:, 1)
      rate(:,:, 422) = rate(:,:, 422) * inv(:,:, 1)
      rate(:,:, 424) = rate(:,:, 424) * inv(:,:, 1)
      rate(:,:, 426) = rate(:,:, 426) * inv(:,:, 1)
      rate(:,:, 428) = rate(:,:, 428) * inv(:,:, 1)
      rate(:,:, 430) = rate(:,:, 430) * inv(:,:, 1)
      rate(:,:, 435) = rate(:,:, 435) * inv(:,:, 1)
      rate(:,:, 436) = rate(:,:, 436) * inv(:,:, 1)
      rate(:,:, 437) = rate(:,:, 437) * inv(:,:, 1)
      rate(:,:, 464) = rate(:,:, 464) * inv(:,:, 2)
      rate(:,:, 468) = rate(:,:, 468) * inv(:,:, 1)
      rate(:,:, 469) = rate(:,:, 469) * inv(:,:, 1)
      rate(:,:, 470) = rate(:,:, 470) * inv(:,:, 1)
      rate(:,:, 471) = rate(:,:, 471) * inv(:,:, 1)
      rate(:,:, 472) = rate(:,:, 472) * inv(:,:, 1)
      rate(:,:, 498) = rate(:,:, 498) * inv(:,:, 1)
      rate(:,:, 535) = rate(:,:, 535) * inv(:,:, 1)
      rate(:,:, 539) = rate(:,:, 539) * inv(:,:, 1)
      rate(:,:, 540) = rate(:,:, 540) * inv(:,:, 1)
      rate(:,:, 541) = rate(:,:, 541) * inv(:,:, 1)
      rate(:,:, 542) = rate(:,:, 542) * inv(:,:, 1)
      rate(:,:, 544) = rate(:,:, 544) * inv(:,:, 1)
      rate(:,:, 628) = rate(:,:, 628) * inv(:,:, 1)
      rate(:,:, 630) = rate(:,:, 630) * inv(:,:, 1)
      rate(:,:, 633) = rate(:,:, 633) * inv(:,:, 1)
      rate(:,:, 635) = rate(:,:, 635) * inv(:,:, 1)
      rate(:,:, 638) = rate(:,:, 638) * inv(:,:, 1)
      rate(:,:, 640) = rate(:,:, 640) * inv(:,:, 1)
      rate(:,:, 650) = rate(:,:, 650) * inv(:,:, 1)
      rate(:,:, 651) = rate(:,:, 651) * inv(:,:, 1)
      rate(:,:, 657) = rate(:,:, 657) * inv(:,:, 1)
      rate(:,:, 658) = rate(:,:, 658) * inv(:,:, 1)
      rate(:,:, 712) = rate(:,:, 712) * inv(:,:, 2)
      rate(:,:, 718) = rate(:,:, 718) * inv(:,:, 2)
      rate(:,:, 162) = rate(:,:, 162) * inv(:,:, 2) * inv(:,:, 1)
      rate(:,:, 164) = rate(:,:, 164) * inv(:,:, 2) * inv(:,:, 1)
      rate(:,:, 178) = rate(:,:, 178) * inv(:,:, 2) * inv(:,:, 1)
      rate(:,:, 153) = rate(:,:, 153) * m(:,:)
      rate(:,:, 154) = rate(:,:, 154) * m(:,:)
      rate(:,:, 157) = rate(:,:, 157) * m(:,:)
      rate(:,:, 158) = rate(:,:, 158) * m(:,:)
      rate(:,:, 159) = rate(:,:, 159) * m(:,:)
      rate(:,:, 160) = rate(:,:, 160) * m(:,:)
      rate(:,:, 161) = rate(:,:, 161) * m(:,:)
      rate(:,:, 163) = rate(:,:, 163) * m(:,:)
      rate(:,:, 165) = rate(:,:, 165) * m(:,:)
      rate(:,:, 166) = rate(:,:, 166) * m(:,:)
      rate(:,:, 169) = rate(:,:, 169) * m(:,:)
      rate(:,:, 170) = rate(:,:, 170) * m(:,:)
      rate(:,:, 171) = rate(:,:, 171) * m(:,:)
      rate(:,:, 172) = rate(:,:, 172) * m(:,:)
      rate(:,:, 173) = rate(:,:, 173) * m(:,:)
      rate(:,:, 174) = rate(:,:, 174) * m(:,:)
      rate(:,:, 175) = rate(:,:, 175) * m(:,:)
      rate(:,:, 176) = rate(:,:, 176) * m(:,:)
      rate(:,:, 177) = rate(:,:, 177) * m(:,:)
      rate(:,:, 179) = rate(:,:, 179) * m(:,:)
      rate(:,:, 180) = rate(:,:, 180) * m(:,:)
      rate(:,:, 181) = rate(:,:, 181) * m(:,:)
      rate(:,:, 182) = rate(:,:, 182) * m(:,:)
      rate(:,:, 183) = rate(:,:, 183) * m(:,:)
      rate(:,:, 184) = rate(:,:, 184) * m(:,:)
      rate(:,:, 185) = rate(:,:, 185) * m(:,:)
      rate(:,:, 186) = rate(:,:, 186) * m(:,:)
      rate(:,:, 187) = rate(:,:, 187) * m(:,:)
      rate(:,:, 188) = rate(:,:, 188) * m(:,:)
      rate(:,:, 189) = rate(:,:, 189) * m(:,:)
      rate(:,:, 190) = rate(:,:, 190) * m(:,:)
      rate(:,:, 191) = rate(:,:, 191) * m(:,:)
      rate(:,:, 192) = rate(:,:, 192) * m(:,:)
      rate(:,:, 193) = rate(:,:, 193) * m(:,:)
      rate(:,:, 194) = rate(:,:, 194) * m(:,:)
      rate(:,:, 195) = rate(:,:, 195) * m(:,:)
      rate(:,:, 196) = rate(:,:, 196) * m(:,:)
      rate(:,:, 197) = rate(:,:, 197) * m(:,:)
      rate(:,:, 198) = rate(:,:, 198) * m(:,:)
      rate(:,:, 199) = rate(:,:, 199) * m(:,:)
      rate(:,:, 201) = rate(:,:, 201) * m(:,:)
      rate(:,:, 202) = rate(:,:, 202) * m(:,:)
      rate(:,:, 203) = rate(:,:, 203) * m(:,:)
      rate(:,:, 204) = rate(:,:, 204) * m(:,:)
      rate(:,:, 205) = rate(:,:, 205) * m(:,:)
      rate(:,:, 206) = rate(:,:, 206) * m(:,:)
      rate(:,:, 207) = rate(:,:, 207) * m(:,:)
      rate(:,:, 208) = rate(:,:, 208) * m(:,:)
      rate(:,:, 209) = rate(:,:, 209) * m(:,:)
      rate(:,:, 210) = rate(:,:, 210) * m(:,:)
      rate(:,:, 211) = rate(:,:, 211) * m(:,:)
      rate(:,:, 212) = rate(:,:, 212) * m(:,:)
      rate(:,:, 213) = rate(:,:, 213) * m(:,:)
      rate(:,:, 214) = rate(:,:, 214) * m(:,:)
      rate(:,:, 215) = rate(:,:, 215) * m(:,:)
      rate(:,:, 216) = rate(:,:, 216) * m(:,:)
      rate(:,:, 217) = rate(:,:, 217) * m(:,:)
      rate(:,:, 218) = rate(:,:, 218) * m(:,:)
      rate(:,:, 219) = rate(:,:, 219) * m(:,:)
      rate(:,:, 220) = rate(:,:, 220) * m(:,:)
      rate(:,:, 221) = rate(:,:, 221) * m(:,:)
      rate(:,:, 222) = rate(:,:, 222) * m(:,:)
      rate(:,:, 223) = rate(:,:, 223) * m(:,:)
      rate(:,:, 224) = rate(:,:, 224) * m(:,:)
      rate(:,:, 225) = rate(:,:, 225) * m(:,:)
      rate(:,:, 226) = rate(:,:, 226) * m(:,:)
      rate(:,:, 227) = rate(:,:, 227) * m(:,:)
      rate(:,:, 228) = rate(:,:, 228) * m(:,:)
      rate(:,:, 229) = rate(:,:, 229) * m(:,:)
      rate(:,:, 230) = rate(:,:, 230) * m(:,:)
      rate(:,:, 231) = rate(:,:, 231) * m(:,:)
      rate(:,:, 232) = rate(:,:, 232) * m(:,:)
      rate(:,:, 233) = rate(:,:, 233) * m(:,:)
      rate(:,:, 236) = rate(:,:, 236) * m(:,:)
      rate(:,:, 241) = rate(:,:, 241) * m(:,:)
      rate(:,:, 242) = rate(:,:, 242) * m(:,:)
      rate(:,:, 243) = rate(:,:, 243) * m(:,:)
      rate(:,:, 244) = rate(:,:, 244) * m(:,:)
      rate(:,:, 245) = rate(:,:, 245) * m(:,:)
      rate(:,:, 247) = rate(:,:, 247) * m(:,:)
      rate(:,:, 248) = rate(:,:, 248) * m(:,:)
      rate(:,:, 249) = rate(:,:, 249) * m(:,:)
      rate(:,:, 250) = rate(:,:, 250) * m(:,:)
      rate(:,:, 251) = rate(:,:, 251) * m(:,:)
      rate(:,:, 252) = rate(:,:, 252) * m(:,:)
      rate(:,:, 253) = rate(:,:, 253) * m(:,:)
      rate(:,:, 254) = rate(:,:, 254) * m(:,:)
      rate(:,:, 255) = rate(:,:, 255) * m(:,:)
      rate(:,:, 256) = rate(:,:, 256) * m(:,:)
      rate(:,:, 257) = rate(:,:, 257) * m(:,:)
      rate(:,:, 258) = rate(:,:, 258) * m(:,:)
      rate(:,:, 259) = rate(:,:, 259) * m(:,:)
      rate(:,:, 260) = rate(:,:, 260) * m(:,:)
      rate(:,:, 261) = rate(:,:, 261) * m(:,:)
      rate(:,:, 262) = rate(:,:, 262) * m(:,:)
      rate(:,:, 263) = rate(:,:, 263) * m(:,:)
      rate(:,:, 264) = rate(:,:, 264) * m(:,:)
      rate(:,:, 265) = rate(:,:, 265) * m(:,:)
      rate(:,:, 266) = rate(:,:, 266) * m(:,:)
      rate(:,:, 267) = rate(:,:, 267) * m(:,:)
      rate(:,:, 268) = rate(:,:, 268) * m(:,:)
      rate(:,:, 269) = rate(:,:, 269) * m(:,:)
      rate(:,:, 270) = rate(:,:, 270) * m(:,:)
      rate(:,:, 271) = rate(:,:, 271) * m(:,:)
      rate(:,:, 272) = rate(:,:, 272) * m(:,:)
      rate(:,:, 273) = rate(:,:, 273) * m(:,:)
      rate(:,:, 274) = rate(:,:, 274) * m(:,:)
      rate(:,:, 275) = rate(:,:, 275) * m(:,:)
      rate(:,:, 276) = rate(:,:, 276) * m(:,:)
      rate(:,:, 277) = rate(:,:, 277) * m(:,:)
      rate(:,:, 278) = rate(:,:, 278) * m(:,:)
      rate(:,:, 279) = rate(:,:, 279) * m(:,:)
      rate(:,:, 280) = rate(:,:, 280) * m(:,:)
      rate(:,:, 281) = rate(:,:, 281) * m(:,:)
      rate(:,:, 282) = rate(:,:, 282) * m(:,:)
      rate(:,:, 283) = rate(:,:, 283) * m(:,:)
      rate(:,:, 284) = rate(:,:, 284) * m(:,:)
      rate(:,:, 285) = rate(:,:, 285) * m(:,:)
      rate(:,:, 286) = rate(:,:, 286) * m(:,:)
      rate(:,:, 287) = rate(:,:, 287) * m(:,:)
      rate(:,:, 288) = rate(:,:, 288) * m(:,:)
      rate(:,:, 289) = rate(:,:, 289) * m(:,:)
      rate(:,:, 290) = rate(:,:, 290) * m(:,:)
      rate(:,:, 291) = rate(:,:, 291) * m(:,:)
      rate(:,:, 292) = rate(:,:, 292) * m(:,:)
      rate(:,:, 293) = rate(:,:, 293) * m(:,:)
      rate(:,:, 294) = rate(:,:, 294) * m(:,:)
      rate(:,:, 295) = rate(:,:, 295) * m(:,:)
      rate(:,:, 296) = rate(:,:, 296) * m(:,:)
      rate(:,:, 297) = rate(:,:, 297) * m(:,:)
      rate(:,:, 298) = rate(:,:, 298) * m(:,:)
      rate(:,:, 299) = rate(:,:, 299) * m(:,:)
      rate(:,:, 300) = rate(:,:, 300) * m(:,:)
      rate(:,:, 301) = rate(:,:, 301) * m(:,:)
      rate(:,:, 303) = rate(:,:, 303) * m(:,:)
      rate(:,:, 304) = rate(:,:, 304) * m(:,:)
      rate(:,:, 305) = rate(:,:, 305) * m(:,:)
      rate(:,:, 306) = rate(:,:, 306) * m(:,:)
      rate(:,:, 307) = rate(:,:, 307) * m(:,:)
      rate(:,:, 308) = rate(:,:, 308) * m(:,:)
      rate(:,:, 309) = rate(:,:, 309) * m(:,:)
      rate(:,:, 310) = rate(:,:, 310) * m(:,:)
      rate(:,:, 311) = rate(:,:, 311) * m(:,:)
      rate(:,:, 312) = rate(:,:, 312) * m(:,:)
      rate(:,:, 313) = rate(:,:, 313) * m(:,:)
      rate(:,:, 314) = rate(:,:, 314) * m(:,:)
      rate(:,:, 315) = rate(:,:, 315) * m(:,:)
      rate(:,:, 316) = rate(:,:, 316) * m(:,:)
      rate(:,:, 317) = rate(:,:, 317) * m(:,:)
      rate(:,:, 318) = rate(:,:, 318) * m(:,:)
      rate(:,:, 319) = rate(:,:, 319) * m(:,:)
      rate(:,:, 320) = rate(:,:, 320) * m(:,:)
      rate(:,:, 321) = rate(:,:, 321) * m(:,:)
      rate(:,:, 322) = rate(:,:, 322) * m(:,:)
      rate(:,:, 323) = rate(:,:, 323) * m(:,:)
      rate(:,:, 324) = rate(:,:, 324) * m(:,:)
      rate(:,:, 325) = rate(:,:, 325) * m(:,:)
      rate(:,:, 326) = rate(:,:, 326) * m(:,:)
      rate(:,:, 327) = rate(:,:, 327) * m(:,:)
      rate(:,:, 328) = rate(:,:, 328) * m(:,:)
      rate(:,:, 329) = rate(:,:, 329) * m(:,:)
      rate(:,:, 330) = rate(:,:, 330) * m(:,:)
      rate(:,:, 331) = rate(:,:, 331) * m(:,:)
      rate(:,:, 332) = rate(:,:, 332) * m(:,:)
      rate(:,:, 333) = rate(:,:, 333) * m(:,:)
      rate(:,:, 334) = rate(:,:, 334) * m(:,:)
      rate(:,:, 335) = rate(:,:, 335) * m(:,:)
      rate(:,:, 336) = rate(:,:, 336) * m(:,:)
      rate(:,:, 337) = rate(:,:, 337) * m(:,:)
      rate(:,:, 338) = rate(:,:, 338) * m(:,:)
      rate(:,:, 339) = rate(:,:, 339) * m(:,:)
      rate(:,:, 340) = rate(:,:, 340) * m(:,:)
      rate(:,:, 341) = rate(:,:, 341) * m(:,:)
      rate(:,:, 342) = rate(:,:, 342) * m(:,:)
      rate(:,:, 343) = rate(:,:, 343) * m(:,:)
      rate(:,:, 344) = rate(:,:, 344) * m(:,:)
      rate(:,:, 345) = rate(:,:, 345) * m(:,:)
      rate(:,:, 346) = rate(:,:, 346) * m(:,:)
      rate(:,:, 347) = rate(:,:, 347) * m(:,:)
      rate(:,:, 348) = rate(:,:, 348) * m(:,:)
      rate(:,:, 349) = rate(:,:, 349) * m(:,:)
      rate(:,:, 350) = rate(:,:, 350) * m(:,:)
      rate(:,:, 351) = rate(:,:, 351) * m(:,:)
      rate(:,:, 352) = rate(:,:, 352) * m(:,:)
      rate(:,:, 353) = rate(:,:, 353) * m(:,:)
      rate(:,:, 354) = rate(:,:, 354) * m(:,:)
      rate(:,:, 355) = rate(:,:, 355) * m(:,:)
      rate(:,:, 356) = rate(:,:, 356) * m(:,:)
      rate(:,:, 357) = rate(:,:, 357) * m(:,:)
      rate(:,:, 358) = rate(:,:, 358) * m(:,:)
      rate(:,:, 359) = rate(:,:, 359) * m(:,:)
      rate(:,:, 360) = rate(:,:, 360) * m(:,:)
      rate(:,:, 361) = rate(:,:, 361) * m(:,:)
      rate(:,:, 362) = rate(:,:, 362) * m(:,:)
      rate(:,:, 363) = rate(:,:, 363) * m(:,:)
      rate(:,:, 364) = rate(:,:, 364) * m(:,:)
      rate(:,:, 365) = rate(:,:, 365) * m(:,:)
      rate(:,:, 366) = rate(:,:, 366) * m(:,:)
      rate(:,:, 367) = rate(:,:, 367) * m(:,:)
      rate(:,:, 368) = rate(:,:, 368) * m(:,:)
      rate(:,:, 369) = rate(:,:, 369) * m(:,:)
      rate(:,:, 370) = rate(:,:, 370) * m(:,:)
      rate(:,:, 371) = rate(:,:, 371) * m(:,:)
      rate(:,:, 372) = rate(:,:, 372) * m(:,:)
      rate(:,:, 373) = rate(:,:, 373) * m(:,:)
      rate(:,:, 374) = rate(:,:, 374) * m(:,:)
      rate(:,:, 375) = rate(:,:, 375) * m(:,:)
      rate(:,:, 376) = rate(:,:, 376) * m(:,:)
      rate(:,:, 377) = rate(:,:, 377) * m(:,:)
      rate(:,:, 378) = rate(:,:, 378) * m(:,:)
      rate(:,:, 379) = rate(:,:, 379) * m(:,:)
      rate(:,:, 380) = rate(:,:, 380) * m(:,:)
      rate(:,:, 381) = rate(:,:, 381) * m(:,:)
      rate(:,:, 382) = rate(:,:, 382) * m(:,:)
      rate(:,:, 383) = rate(:,:, 383) * m(:,:)
      rate(:,:, 384) = rate(:,:, 384) * m(:,:)
      rate(:,:, 385) = rate(:,:, 385) * m(:,:)
      rate(:,:, 386) = rate(:,:, 386) * m(:,:)
      rate(:,:, 387) = rate(:,:, 387) * m(:,:)
      rate(:,:, 388) = rate(:,:, 388) * m(:,:)
      rate(:,:, 389) = rate(:,:, 389) * m(:,:)
      rate(:,:, 390) = rate(:,:, 390) * m(:,:)
      rate(:,:, 391) = rate(:,:, 391) * m(:,:)
      rate(:,:, 392) = rate(:,:, 392) * m(:,:)
      rate(:,:, 393) = rate(:,:, 393) * m(:,:)
      rate(:,:, 394) = rate(:,:, 394) * m(:,:)
      rate(:,:, 395) = rate(:,:, 395) * m(:,:)
      rate(:,:, 396) = rate(:,:, 396) * m(:,:)
      rate(:,:, 398) = rate(:,:, 398) * m(:,:)
      rate(:,:, 399) = rate(:,:, 399) * m(:,:)
      rate(:,:, 400) = rate(:,:, 400) * m(:,:)
      rate(:,:, 401) = rate(:,:, 401) * m(:,:)
      rate(:,:, 402) = rate(:,:, 402) * m(:,:)
      rate(:,:, 403) = rate(:,:, 403) * m(:,:)
      rate(:,:, 404) = rate(:,:, 404) * m(:,:)
      rate(:,:, 405) = rate(:,:, 405) * m(:,:)
      rate(:,:, 406) = rate(:,:, 406) * m(:,:)
      rate(:,:, 407) = rate(:,:, 407) * m(:,:)
      rate(:,:, 408) = rate(:,:, 408) * m(:,:)
      rate(:,:, 409) = rate(:,:, 409) * m(:,:)
      rate(:,:, 410) = rate(:,:, 410) * m(:,:)
      rate(:,:, 411) = rate(:,:, 411) * m(:,:)
      rate(:,:, 412) = rate(:,:, 412) * m(:,:)
      rate(:,:, 413) = rate(:,:, 413) * m(:,:)
      rate(:,:, 414) = rate(:,:, 414) * m(:,:)
      rate(:,:, 415) = rate(:,:, 415) * m(:,:)
      rate(:,:, 416) = rate(:,:, 416) * m(:,:)
      rate(:,:, 417) = rate(:,:, 417) * m(:,:)
      rate(:,:, 418) = rate(:,:, 418) * m(:,:)
      rate(:,:, 419) = rate(:,:, 419) * m(:,:)
      rate(:,:, 420) = rate(:,:, 420) * m(:,:)
      rate(:,:, 421) = rate(:,:, 421) * m(:,:)
      rate(:,:, 422) = rate(:,:, 422) * m(:,:)
      rate(:,:, 423) = rate(:,:, 423) * m(:,:)
      rate(:,:, 424) = rate(:,:, 424) * m(:,:)
      rate(:,:, 425) = rate(:,:, 425) * m(:,:)
      rate(:,:, 426) = rate(:,:, 426) * m(:,:)
      rate(:,:, 427) = rate(:,:, 427) * m(:,:)
      rate(:,:, 428) = rate(:,:, 428) * m(:,:)
      rate(:,:, 429) = rate(:,:, 429) * m(:,:)
      rate(:,:, 430) = rate(:,:, 430) * m(:,:)
      rate(:,:, 431) = rate(:,:, 431) * m(:,:)
      rate(:,:, 432) = rate(:,:, 432) * m(:,:)
      rate(:,:, 433) = rate(:,:, 433) * m(:,:)
      rate(:,:, 434) = rate(:,:, 434) * m(:,:)
      rate(:,:, 435) = rate(:,:, 435) * m(:,:)
      rate(:,:, 436) = rate(:,:, 436) * m(:,:)
      rate(:,:, 437) = rate(:,:, 437) * m(:,:)
      rate(:,:, 438) = rate(:,:, 438) * m(:,:)
      rate(:,:, 439) = rate(:,:, 439) * m(:,:)
      rate(:,:, 440) = rate(:,:, 440) * m(:,:)
      rate(:,:, 441) = rate(:,:, 441) * m(:,:)
      rate(:,:, 442) = rate(:,:, 442) * m(:,:)
      rate(:,:, 443) = rate(:,:, 443) * m(:,:)
      rate(:,:, 444) = rate(:,:, 444) * m(:,:)
      rate(:,:, 445) = rate(:,:, 445) * m(:,:)
      rate(:,:, 446) = rate(:,:, 446) * m(:,:)
      rate(:,:, 447) = rate(:,:, 447) * m(:,:)
      rate(:,:, 448) = rate(:,:, 448) * m(:,:)
      rate(:,:, 449) = rate(:,:, 449) * m(:,:)
      rate(:,:, 450) = rate(:,:, 450) * m(:,:)
      rate(:,:, 451) = rate(:,:, 451) * m(:,:)
      rate(:,:, 452) = rate(:,:, 452) * m(:,:)
      rate(:,:, 453) = rate(:,:, 453) * m(:,:)
      rate(:,:, 454) = rate(:,:, 454) * m(:,:)
      rate(:,:, 455) = rate(:,:, 455) * m(:,:)
      rate(:,:, 456) = rate(:,:, 456) * m(:,:)
      rate(:,:, 457) = rate(:,:, 457) * m(:,:)
      rate(:,:, 458) = rate(:,:, 458) * m(:,:)
      rate(:,:, 459) = rate(:,:, 459) * m(:,:)
      rate(:,:, 460) = rate(:,:, 460) * m(:,:)
      rate(:,:, 461) = rate(:,:, 461) * m(:,:)
      rate(:,:, 462) = rate(:,:, 462) * m(:,:)
      rate(:,:, 465) = rate(:,:, 465) * m(:,:)
      rate(:,:, 466) = rate(:,:, 466) * m(:,:)
      rate(:,:, 467) = rate(:,:, 467) * m(:,:)
      rate(:,:, 468) = rate(:,:, 468) * m(:,:)
      rate(:,:, 469) = rate(:,:, 469) * m(:,:)
      rate(:,:, 470) = rate(:,:, 470) * m(:,:)
      rate(:,:, 473) = rate(:,:, 473) * m(:,:)
      rate(:,:, 474) = rate(:,:, 474) * m(:,:)
      rate(:,:, 475) = rate(:,:, 475) * m(:,:)
      rate(:,:, 476) = rate(:,:, 476) * m(:,:)
      rate(:,:, 477) = rate(:,:, 477) * m(:,:)
      rate(:,:, 478) = rate(:,:, 478) * m(:,:)
      rate(:,:, 479) = rate(:,:, 479) * m(:,:)
      rate(:,:, 480) = rate(:,:, 480) * m(:,:)
      rate(:,:, 481) = rate(:,:, 481) * m(:,:)
      rate(:,:, 482) = rate(:,:, 482) * m(:,:)
      rate(:,:, 483) = rate(:,:, 483) * m(:,:)
      rate(:,:, 484) = rate(:,:, 484) * m(:,:)
      rate(:,:, 485) = rate(:,:, 485) * m(:,:)
      rate(:,:, 486) = rate(:,:, 486) * m(:,:)
      rate(:,:, 487) = rate(:,:, 487) * m(:,:)
      rate(:,:, 488) = rate(:,:, 488) * m(:,:)
      rate(:,:, 489) = rate(:,:, 489) * m(:,:)
      rate(:,:, 490) = rate(:,:, 490) * m(:,:)
      rate(:,:, 491) = rate(:,:, 491) * m(:,:)
      rate(:,:, 492) = rate(:,:, 492) * m(:,:)
      rate(:,:, 493) = rate(:,:, 493) * m(:,:)
      rate(:,:, 494) = rate(:,:, 494) * m(:,:)
      rate(:,:, 495) = rate(:,:, 495) * m(:,:)
      rate(:,:, 496) = rate(:,:, 496) * m(:,:)
      rate(:,:, 497) = rate(:,:, 497) * m(:,:)
      rate(:,:, 498) = rate(:,:, 498) * m(:,:)
      rate(:,:, 499) = rate(:,:, 499) * m(:,:)
      rate(:,:, 500) = rate(:,:, 500) * m(:,:)
      rate(:,:, 501) = rate(:,:, 501) * m(:,:)
      rate(:,:, 502) = rate(:,:, 502) * m(:,:)
      rate(:,:, 503) = rate(:,:, 503) * m(:,:)
      rate(:,:, 504) = rate(:,:, 504) * m(:,:)
      rate(:,:, 505) = rate(:,:, 505) * m(:,:)
      rate(:,:, 506) = rate(:,:, 506) * m(:,:)
      rate(:,:, 507) = rate(:,:, 507) * m(:,:)
      rate(:,:, 508) = rate(:,:, 508) * m(:,:)
      rate(:,:, 509) = rate(:,:, 509) * m(:,:)
      rate(:,:, 510) = rate(:,:, 510) * m(:,:)
      rate(:,:, 511) = rate(:,:, 511) * m(:,:)
      rate(:,:, 512) = rate(:,:, 512) * m(:,:)
      rate(:,:, 513) = rate(:,:, 513) * m(:,:)
      rate(:,:, 514) = rate(:,:, 514) * m(:,:)
      rate(:,:, 515) = rate(:,:, 515) * m(:,:)
      rate(:,:, 516) = rate(:,:, 516) * m(:,:)
      rate(:,:, 517) = rate(:,:, 517) * m(:,:)
      rate(:,:, 518) = rate(:,:, 518) * m(:,:)
      rate(:,:, 519) = rate(:,:, 519) * m(:,:)
      rate(:,:, 520) = rate(:,:, 520) * m(:,:)
      rate(:,:, 521) = rate(:,:, 521) * m(:,:)
      rate(:,:, 522) = rate(:,:, 522) * m(:,:)
      rate(:,:, 523) = rate(:,:, 523) * m(:,:)
      rate(:,:, 524) = rate(:,:, 524) * m(:,:)
      rate(:,:, 525) = rate(:,:, 525) * m(:,:)
      rate(:,:, 526) = rate(:,:, 526) * m(:,:)
      rate(:,:, 527) = rate(:,:, 527) * m(:,:)
      rate(:,:, 528) = rate(:,:, 528) * m(:,:)
      rate(:,:, 529) = rate(:,:, 529) * m(:,:)
      rate(:,:, 530) = rate(:,:, 530) * m(:,:)
      rate(:,:, 531) = rate(:,:, 531) * m(:,:)
      rate(:,:, 532) = rate(:,:, 532) * m(:,:)
      rate(:,:, 533) = rate(:,:, 533) * m(:,:)
      rate(:,:, 534) = rate(:,:, 534) * m(:,:)
      rate(:,:, 535) = rate(:,:, 535) * m(:,:)
      rate(:,:, 536) = rate(:,:, 536) * m(:,:)
      rate(:,:, 537) = rate(:,:, 537) * m(:,:)
      rate(:,:, 538) = rate(:,:, 538) * m(:,:)
      rate(:,:, 539) = rate(:,:, 539) * m(:,:)
      rate(:,:, 540) = rate(:,:, 540) * m(:,:)
      rate(:,:, 543) = rate(:,:, 543) * m(:,:)
      rate(:,:, 544) = rate(:,:, 544) * m(:,:)
      rate(:,:, 545) = rate(:,:, 545) * m(:,:)
      rate(:,:, 546) = rate(:,:, 546) * m(:,:)
      rate(:,:, 547) = rate(:,:, 547) * m(:,:)
      rate(:,:, 548) = rate(:,:, 548) * m(:,:)
      rate(:,:, 549) = rate(:,:, 549) * m(:,:)
      rate(:,:, 550) = rate(:,:, 550) * m(:,:)
      rate(:,:, 551) = rate(:,:, 551) * m(:,:)
      rate(:,:, 552) = rate(:,:, 552) * m(:,:)
      rate(:,:, 553) = rate(:,:, 553) * m(:,:)
      rate(:,:, 554) = rate(:,:, 554) * m(:,:)
      rate(:,:, 555) = rate(:,:, 555) * m(:,:)
      rate(:,:, 556) = rate(:,:, 556) * m(:,:)
      rate(:,:, 557) = rate(:,:, 557) * m(:,:)
      rate(:,:, 558) = rate(:,:, 558) * m(:,:)
      rate(:,:, 559) = rate(:,:, 559) * m(:,:)
      rate(:,:, 560) = rate(:,:, 560) * m(:,:)
      rate(:,:, 561) = rate(:,:, 561) * m(:,:)
      rate(:,:, 562) = rate(:,:, 562) * m(:,:)
      rate(:,:, 563) = rate(:,:, 563) * m(:,:)
      rate(:,:, 564) = rate(:,:, 564) * m(:,:)
      rate(:,:, 565) = rate(:,:, 565) * m(:,:)
      rate(:,:, 567) = rate(:,:, 567) * m(:,:)
      rate(:,:, 568) = rate(:,:, 568) * m(:,:)
      rate(:,:, 569) = rate(:,:, 569) * m(:,:)
      rate(:,:, 570) = rate(:,:, 570) * m(:,:)
      rate(:,:, 571) = rate(:,:, 571) * m(:,:)
      rate(:,:, 572) = rate(:,:, 572) * m(:,:)
      rate(:,:, 573) = rate(:,:, 573) * m(:,:)
      rate(:,:, 574) = rate(:,:, 574) * m(:,:)
      rate(:,:, 575) = rate(:,:, 575) * m(:,:)
      rate(:,:, 576) = rate(:,:, 576) * m(:,:)
      rate(:,:, 577) = rate(:,:, 577) * m(:,:)
      rate(:,:, 578) = rate(:,:, 578) * m(:,:)
      rate(:,:, 579) = rate(:,:, 579) * m(:,:)
      rate(:,:, 580) = rate(:,:, 580) * m(:,:)
      rate(:,:, 581) = rate(:,:, 581) * m(:,:)
      rate(:,:, 582) = rate(:,:, 582) * m(:,:)
      rate(:,:, 583) = rate(:,:, 583) * m(:,:)
      rate(:,:, 584) = rate(:,:, 584) * m(:,:)
      rate(:,:, 585) = rate(:,:, 585) * m(:,:)
      rate(:,:, 586) = rate(:,:, 586) * m(:,:)
      rate(:,:, 587) = rate(:,:, 587) * m(:,:)
      rate(:,:, 588) = rate(:,:, 588) * m(:,:)
      rate(:,:, 589) = rate(:,:, 589) * m(:,:)
      rate(:,:, 590) = rate(:,:, 590) * m(:,:)
      rate(:,:, 591) = rate(:,:, 591) * m(:,:)
      rate(:,:, 592) = rate(:,:, 592) * m(:,:)
      rate(:,:, 593) = rate(:,:, 593) * m(:,:)
      rate(:,:, 594) = rate(:,:, 594) * m(:,:)
      rate(:,:, 595) = rate(:,:, 595) * m(:,:)
      rate(:,:, 596) = rate(:,:, 596) * m(:,:)
      rate(:,:, 597) = rate(:,:, 597) * m(:,:)
      rate(:,:, 598) = rate(:,:, 598) * m(:,:)
      rate(:,:, 599) = rate(:,:, 599) * m(:,:)
      rate(:,:, 600) = rate(:,:, 600) * m(:,:)
      rate(:,:, 601) = rate(:,:, 601) * m(:,:)
      rate(:,:, 602) = rate(:,:, 602) * m(:,:)
      rate(:,:, 603) = rate(:,:, 603) * m(:,:)
      rate(:,:, 604) = rate(:,:, 604) * m(:,:)
      rate(:,:, 605) = rate(:,:, 605) * m(:,:)
      rate(:,:, 606) = rate(:,:, 606) * m(:,:)
      rate(:,:, 607) = rate(:,:, 607) * m(:,:)
      rate(:,:, 608) = rate(:,:, 608) * m(:,:)
      rate(:,:, 609) = rate(:,:, 609) * m(:,:)
      rate(:,:, 610) = rate(:,:, 610) * m(:,:)
      rate(:,:, 611) = rate(:,:, 611) * m(:,:)
      rate(:,:, 612) = rate(:,:, 612) * m(:,:)
      rate(:,:, 613) = rate(:,:, 613) * m(:,:)
      rate(:,:, 614) = rate(:,:, 614) * m(:,:)
      rate(:,:, 615) = rate(:,:, 615) * m(:,:)
      rate(:,:, 616) = rate(:,:, 616) * m(:,:)
      rate(:,:, 617) = rate(:,:, 617) * m(:,:)
      rate(:,:, 618) = rate(:,:, 618) * m(:,:)
      rate(:,:, 619) = rate(:,:, 619) * m(:,:)
      rate(:,:, 620) = rate(:,:, 620) * m(:,:)
      rate(:,:, 621) = rate(:,:, 621) * m(:,:)
      rate(:,:, 622) = rate(:,:, 622) * m(:,:)
      rate(:,:, 623) = rate(:,:, 623) * m(:,:)
      rate(:,:, 624) = rate(:,:, 624) * m(:,:)
      rate(:,:, 625) = rate(:,:, 625) * m(:,:)
      rate(:,:, 626) = rate(:,:, 626) * m(:,:)
      rate(:,:, 627) = rate(:,:, 627) * m(:,:)
      rate(:,:, 628) = rate(:,:, 628) * m(:,:)
      rate(:,:, 629) = rate(:,:, 629) * m(:,:)
      rate(:,:, 630) = rate(:,:, 630) * m(:,:)
      rate(:,:, 631) = rate(:,:, 631) * m(:,:)
      rate(:,:, 632) = rate(:,:, 632) * m(:,:)
      rate(:,:, 633) = rate(:,:, 633) * m(:,:)
      rate(:,:, 634) = rate(:,:, 634) * m(:,:)
      rate(:,:, 635) = rate(:,:, 635) * m(:,:)
      rate(:,:, 636) = rate(:,:, 636) * m(:,:)
      rate(:,:, 637) = rate(:,:, 637) * m(:,:)
      rate(:,:, 638) = rate(:,:, 638) * m(:,:)
      rate(:,:, 639) = rate(:,:, 639) * m(:,:)
      rate(:,:, 640) = rate(:,:, 640) * m(:,:)
      rate(:,:, 641) = rate(:,:, 641) * m(:,:)
      rate(:,:, 642) = rate(:,:, 642) * m(:,:)
      rate(:,:, 643) = rate(:,:, 643) * m(:,:)
      rate(:,:, 644) = rate(:,:, 644) * m(:,:)
      rate(:,:, 645) = rate(:,:, 645) * m(:,:)
      rate(:,:, 646) = rate(:,:, 646) * m(:,:)
      rate(:,:, 647) = rate(:,:, 647) * m(:,:)
      rate(:,:, 648) = rate(:,:, 648) * m(:,:)
      rate(:,:, 649) = rate(:,:, 649) * m(:,:)
      rate(:,:, 650) = rate(:,:, 650) * m(:,:)
      rate(:,:, 651) = rate(:,:, 651) * m(:,:)
      rate(:,:, 652) = rate(:,:, 652) * m(:,:)
      rate(:,:, 653) = rate(:,:, 653) * m(:,:)
      rate(:,:, 654) = rate(:,:, 654) * m(:,:)
      rate(:,:, 655) = rate(:,:, 655) * m(:,:)
      rate(:,:, 656) = rate(:,:, 656) * m(:,:)
      rate(:,:, 659) = rate(:,:, 659) * m(:,:)
      rate(:,:, 660) = rate(:,:, 660) * m(:,:)
      rate(:,:, 661) = rate(:,:, 661) * m(:,:)
      rate(:,:, 662) = rate(:,:, 662) * m(:,:)
      rate(:,:, 663) = rate(:,:, 663) * m(:,:)
      rate(:,:, 664) = rate(:,:, 664) * m(:,:)
      rate(:,:, 665) = rate(:,:, 665) * m(:,:)
      rate(:,:, 666) = rate(:,:, 666) * m(:,:)
      rate(:,:, 667) = rate(:,:, 667) * m(:,:)
      rate(:,:, 668) = rate(:,:, 668) * m(:,:)
      rate(:,:, 669) = rate(:,:, 669) * m(:,:)
      rate(:,:, 670) = rate(:,:, 670) * m(:,:)
      rate(:,:, 671) = rate(:,:, 671) * m(:,:)
      rate(:,:, 672) = rate(:,:, 672) * m(:,:)
      rate(:,:, 673) = rate(:,:, 673) * m(:,:)
      rate(:,:, 674) = rate(:,:, 674) * m(:,:)
      rate(:,:, 675) = rate(:,:, 675) * m(:,:)
      rate(:,:, 676) = rate(:,:, 676) * m(:,:)
      rate(:,:, 677) = rate(:,:, 677) * m(:,:)
      rate(:,:, 678) = rate(:,:, 678) * m(:,:)
      rate(:,:, 679) = rate(:,:, 679) * m(:,:)
      rate(:,:, 680) = rate(:,:, 680) * m(:,:)
      rate(:,:, 681) = rate(:,:, 681) * m(:,:)
      rate(:,:, 682) = rate(:,:, 682) * m(:,:)
      rate(:,:, 683) = rate(:,:, 683) * m(:,:)
      rate(:,:, 684) = rate(:,:, 684) * m(:,:)
      rate(:,:, 685) = rate(:,:, 685) * m(:,:)
      rate(:,:, 686) = rate(:,:, 686) * m(:,:)
      rate(:,:, 687) = rate(:,:, 687) * m(:,:)
      rate(:,:, 688) = rate(:,:, 688) * m(:,:)
      rate(:,:, 689) = rate(:,:, 689) * m(:,:)
      rate(:,:, 690) = rate(:,:, 690) * m(:,:)
      rate(:,:, 691) = rate(:,:, 691) * m(:,:)
      rate(:,:, 692) = rate(:,:, 692) * m(:,:)
      rate(:,:, 693) = rate(:,:, 693) * m(:,:)
      rate(:,:, 694) = rate(:,:, 694) * m(:,:)
      rate(:,:, 695) = rate(:,:, 695) * m(:,:)
      rate(:,:, 696) = rate(:,:, 696) * m(:,:)
      rate(:,:, 697) = rate(:,:, 697) * m(:,:)
      rate(:,:, 698) = rate(:,:, 698) * m(:,:)
      rate(:,:, 699) = rate(:,:, 699) * m(:,:)
      rate(:,:, 700) = rate(:,:, 700) * m(:,:)
      rate(:,:, 701) = rate(:,:, 701) * m(:,:)
      rate(:,:, 702) = rate(:,:, 702) * m(:,:)
      rate(:,:, 703) = rate(:,:, 703) * m(:,:)
      rate(:,:, 704) = rate(:,:, 704) * m(:,:)
      rate(:,:, 705) = rate(:,:, 705) * m(:,:)
      rate(:,:, 706) = rate(:,:, 706) * m(:,:)
      rate(:,:, 707) = rate(:,:, 707) * m(:,:)
      rate(:,:, 708) = rate(:,:, 708) * m(:,:)
      rate(:,:, 709) = rate(:,:, 709) * m(:,:)
      rate(:,:, 710) = rate(:,:, 710) * m(:,:)
      rate(:,:, 711) = rate(:,:, 711) * m(:,:)
      rate(:,:, 713) = rate(:,:, 713) * m(:,:)
      rate(:,:, 714) = rate(:,:, 714) * m(:,:)
      rate(:,:, 715) = rate(:,:, 715) * m(:,:)
      rate(:,:, 716) = rate(:,:, 716) * m(:,:)
      rate(:,:, 717) = rate(:,:, 717) * m(:,:)
      rate(:,:, 719) = rate(:,:, 719) * m(:,:)
      rate(:,:, 720) = rate(:,:, 720) * m(:,:)
      rate(:,:, 721) = rate(:,:, 721) * m(:,:)
      rate(:,:, 722) = rate(:,:, 722) * m(:,:)
      rate(:,:, 723) = rate(:,:, 723) * m(:,:)
      rate(:,:, 724) = rate(:,:, 724) * m(:,:)
      rate(:,:, 725) = rate(:,:, 725) * m(:,:)
      rate(:,:, 726) = rate(:,:, 726) * m(:,:)
      rate(:,:, 727) = rate(:,:, 727) * m(:,:)
      rate(:,:, 728) = rate(:,:, 728) * m(:,:)
      rate(:,:, 756) = rate(:,:, 756) * m(:,:)
      rate(:,:, 757) = rate(:,:, 757) * m(:,:)
      rate(:,:, 758) = rate(:,:, 758) * m(:,:)
      rate(:,:, 759) = rate(:,:, 759) * m(:,:)
      rate(:,:, 760) = rate(:,:, 760) * m(:,:)
      rate(:,:, 761) = rate(:,:, 761) * m(:,:)
      rate(:,:, 762) = rate(:,:, 762) * m(:,:)
      rate(:,:, 763) = rate(:,:, 763) * m(:,:)
      rate(:,:, 764) = rate(:,:, 764) * m(:,:)
      rate(:,:, 765) = rate(:,:, 765) * m(:,:)
      rate(:,:, 766) = rate(:,:, 766) * m(:,:)
      rate(:,:, 767) = rate(:,:, 767) * m(:,:)
      rate(:,:, 768) = rate(:,:, 768) * m(:,:)
      rate(:,:, 769) = rate(:,:, 769) * m(:,:)
      rate(:,:, 770) = rate(:,:, 770) * m(:,:)
      rate(:,:, 771) = rate(:,:, 771) * m(:,:)
      rate(:,:, 772) = rate(:,:, 772) * m(:,:)
      rate(:,:, 773) = rate(:,:, 773) * m(:,:)
      rate(:,:, 774) = rate(:,:, 774) * m(:,:)
      rate(:,:, 775) = rate(:,:, 775) * m(:,:)
      rate(:,:, 776) = rate(:,:, 776) * m(:,:)
      rate(:,:, 777) = rate(:,:, 777) * m(:,:)
      rate(:,:, 778) = rate(:,:, 778) * m(:,:)
      rate(:,:, 779) = rate(:,:, 779) * m(:,:)
      rate(:,:, 780) = rate(:,:, 780) * m(:,:)
      rate(:,:, 781) = rate(:,:, 781) * m(:,:)
      rate(:,:, 782) = rate(:,:, 782) * m(:,:)
      rate(:,:, 783) = rate(:,:, 783) * m(:,:)
      rate(:,:, 785) = rate(:,:, 785) * m(:,:)
      rate(:,:, 790) = rate(:,:, 790) * m(:,:)
      rate(:,:, 791) = rate(:,:, 791) * m(:,:)
      rate(:,:, 792) = rate(:,:, 792) * m(:,:)
      rate(:,:, 795) = rate(:,:, 795) * m(:,:)
      rate(:,:, 796) = rate(:,:, 796) * m(:,:)
      rate(:,:, 797) = rate(:,:, 797) * m(:,:)
      rate(:,:, 800) = rate(:,:, 800) * m(:,:)
      rate(:,:, 807) = rate(:,:, 807) * m(:,:)
      rate(:,:, 813) = rate(:,:, 813) * m(:,:)
      rate(:,:, 818) = rate(:,:, 818) * m(:,:)
      end subroutine adjrxt
      end module mo_adjrxt
      module mo_phtadj
      private
      public :: phtadj
      contains
      subroutine phtadj( p_rate, inv, m, ncol, nlev )
      use chem_mods, only : nfs, phtcnt
      use shr_kind_mod, only : r8 => shr_kind_r8
      implicit none
!--------------------------------------------------------------------
! ... dummy arguments
!--------------------------------------------------------------------
      integer, intent(in) :: ncol, nlev
      real(r8), intent(in) :: inv(ncol,nlev,max(1,nfs))
      real(r8), intent(in) :: m(ncol,nlev)
      real(r8), intent(inout) :: p_rate(ncol,nlev,max(1,phtcnt))
!--------------------------------------------------------------------
! ... local variables
!--------------------------------------------------------------------
      integer :: k
      real(r8) :: im(ncol,nlev)
      do k = 1,nlev
         im(:ncol,k) = 1._r8 / m(:ncol,k)
         p_rate(:,k, 5) = p_rate(:,k, 5) * inv(:,k, 2) * im(:,k)
         p_rate(:,k, 6) = p_rate(:,k, 6) * inv(:,k, 2) * im(:,k)
      end do
      end subroutine phtadj
      end module mo_phtadj
      module mo_sim_dat
      private
      public :: set_sim_dat
      contains
      subroutine set_sim_dat
      use chem_mods, only : clscnt, cls_rxt_cnt, clsmap, permute, adv_mass, fix_mass, crb_mass
      use chem_mods, only : diag_map
      use chem_mods, only : phtcnt, rxt_tag_cnt, rxt_tag_lst, rxt_tag_map
      use chem_mods, only : pht_alias_lst, pht_alias_mult
      use chem_mods, only : extfrc_lst, inv_lst, slvd_lst
      use chem_mods, only : enthalpy_cnt, cph_enthalpy, cph_rid, num_rnts, rxntot
      use cam_abortutils,only : endrun
      use mo_tracname, only : solsym
      use chem_mods, only : frc_from_dataset
      use chem_mods, only : is_scalar, is_vector
      use shr_kind_mod, only : r8 => shr_kind_r8
      use cam_logfile, only : iulog
      implicit none
!--------------------------------------------------------------
! ... local variables
!--------------------------------------------------------------
      integer :: ios
      is_scalar = .false.
      is_vector = .true.
      clscnt(:) = (/ 3, 0, 0, 270, 0 /)
      cls_rxt_cnt(:,1) = (/ 16, 0, 0, 3 /)
      cls_rxt_cnt(:,4) = (/ 2, 233, 587, 270 /)
      solsym(:273) = (/ 'ACBZO2          ','ALKNIT          ','ALKO2           ','ALKOOH          ','AOA_NH          ', &
                        'bc_a1           ','bc_a4           ','BCARY           ','BCARYO2VBS      ','BENZENE         ', &
                        'BENZO2          ','BENZO2VBS       ','BENZOOH         ','BEPOMUC         ','BIGALD          ', &
                        'BIGALD1         ','BIGALD2         ','BIGALD3         ','BIGALD4         ','BIGALK          ', &
                        'BIGENE          ','BR              ','BRCL            ','BRO             ','BRONO2          ', &
                        'BRY             ','BZALD           ','BZOO            ','BZOOH           ','C2H2            ', &
                        'C2H4            ','C2H5O2          ','C2H5OH          ','C2H5OOH         ','C2H6            ', &
                        'C3H6            ','C3H7O2          ','C3H7OOH         ','C3H8            ','C6H5O2          ', &
                        'C6H5OOH         ','CCL4            ','CF2CLBR         ','CF3BR           ','CFC11           ', &
                        'CFC113          ','CFC114          ','CFC115          ','CFC12           ','CH2BR2          ', &
                        'CH2O            ','CH3BR           ','CH3CCL3         ','CH3CHO          ','CH3CL           ', &
                        'CH3CN           ','CH3CO3          ','CH3COCH3        ','CH3COCHO        ','CH3COOH         ', &
                        'CH3COOOH        ','CH3O2           ','CH3OH           ','CH3OOH          ','CH4             ', &
                        'CHBR3           ','CL              ','CL2             ','CL2O2           ','CLO             ', &
                        'CLONO2          ','CLY             ','CO              ','CO2             ','COF2            ', &
                        'COFCL           ','CRESOL          ','DICARBO2        ','DMS             ','dst_a1          ', &
                        'dst_a2          ','dst_a3          ','E90             ','ENEO2           ','EO              ', &
                        'EO2             ','EOOH            ','F               ','GLYALD          ','GLYOXAL         ', &
                        'H               ','H2              ','H2402           ','H2O2            ','H2SO4           ', &
                        'HBR             ','HCFC141B        ','HCFC142B        ','HCFC22          ','HCL             ', &
                        'HCN             ','HCOOH           ','HF              ','HNO3            ','HO2             ', &
                        'HO2NO2          ','HOBR            ','HOCH2OO         ','HOCL            ','HONITR          ', &
                        'HPALD           ','HYAC            ','HYDRALD         ','IEPOX           ','ISOP            ', &
                        'ISOPAO2         ','ISOPBO2         ','ISOPNITA        ','ISOPNITB        ','ISOPNO3         ', &
                        'ISOPNOOH        ','ISOPO2VBS       ','ISOPOOH         ','IVOC            ','IVOCO2VBS       ', &
                        'MACR            ','MACRO2          ','MACROOH         ','MALO2           ','MCO3            ', &
                        'MDIALO2         ','MEK             ','MEKO2           ','MEKOOH          ','MPAN            ', &
                        'MTERP           ','MTERPO2VBS      ','MVK             ','N               ','N2O             ', &
                        'N2O5            ','NC4CH2OH        ','NC4CHO          ','ncl_a1          ','ncl_a2          ', &
                        'ncl_a3          ','NH3             ','NH4             ','NH_5            ','NH_50           ', &
                        'NO              ','NO2             ','NO3             ','NOA             ','NTERPO2         ', &
                        'NTERPOOH        ','num_a1          ','num_a2          ','num_a3          ','num_a4          ', &
                        'O               ','O1D             ','O3              ','O3S             ','OCLO            ', &
                        'OCS             ','OH              ','ONITR           ','PAN             ','PBZNIT          ', &
                        'PHENO           ','PHENO2          ','PHENOL          ','PHENOOH         ','PO2             ', &
                        'pom_a1          ','pom_a4          ','POOH            ','RO2             ','ROOH            ', &
                        'S               ','SF6             ','SO              ','SO2             ','SO3             ', &
                        'so4_a1          ','so4_a2          ','so4_a3          ','soa1_a1         ','soa1_a2         ', &
                        'soa2_a1         ','soa2_a2         ','soa3_a1         ','soa3_a2         ','soa4_a1         ', &
                        'soa4_a2         ','soa5_a1         ','soa5_a2         ','SOAG0           ','SOAG1           ', &
                        'SOAG2           ','SOAG3           ','SOAG4           ','ST80_25         ','SVOC            ', &
                        'TEPOMUC         ','TERP2O2         ','TERP2OOH        ','TERPNIT         ','TERPO2          ', &
                        'TERPOOH         ','TERPROD1        ','TERPROD2        ','TOLO2           ','TOLOOH          ', &
                        'TOLUENE         ','TOLUO2VBS       ','XALKNIT         ','XBRONO2         ','XCLONO2         ', &
                        'XHNO3           ','XHO2NO2         ','XHONITR         ','XISOPNITA       ','XISOPNITB       ', &
                        'XISOPNO3        ','XISOPNOOH       ','XMPAN           ','XN              ','XNC4CH2OH       ', &
                        'XNC4CHO         ','XNDEP           ','XNO             ','XNO2            ','XNO2NO3         ', &
                        'XNO2XNO3        ','XNO3            ','XNO3NO2         ','XNOA            ','XNTERPO2        ', &
                        'XNTERPOOH       ','XO              ','XO1D            ','XO2             ','XO3             ', &
                        'XONITR          ','XOOH            ','XPAN            ','XPBZNIT         ','XTERPNIT        ', &
                        'XYLENES         ','XYLENO2         ','XYLENOOH        ','XYLEO2VBS       ','XYLOL           ', &
                        'XYLOLO2         ','XYLOLOOH        ','NHDEP           ','NDEP            ','H2O             ', &
                        'CO01            ','CO02            ','CO03            ','CO04            ','CO05            ', &
                        'CO06            ','CO07            ','CO08            ','CO09            ','CO10            ', &
                        'CO11            ','CO12            ','CO13            ' /)
      adv_mass(:273) = (/ 137.112200_r8, 133.141340_r8, 103.135200_r8, 104.142600_r8, 28.010400_r8, &
                             12.011000_r8, 12.011000_r8, 204.342600_r8, 253.348200_r8, 78.110400_r8, &
                            159.114800_r8, 159.114800_r8, 160.122200_r8, 126.108600_r8, 98.098200_r8, &
                             84.072400_r8, 98.098200_r8, 98.098200_r8, 112.124000_r8, 72.143800_r8, &
                             56.103200_r8, 79.904000_r8, 115.356700_r8, 95.903400_r8, 141.908940_r8, &
                             99.716850_r8, 106.120800_r8, 123.127600_r8, 124.135000_r8, 26.036800_r8, &
                             28.051600_r8, 61.057800_r8, 46.065800_r8, 62.065200_r8, 30.066400_r8, &
                             42.077400_r8, 75.083600_r8, 76.091000_r8, 44.092200_r8, 109.101800_r8, &
                            110.109200_r8, 153.821800_r8, 165.364506_r8, 148.910210_r8, 137.367503_r8, &
                            187.375310_r8, 170.921013_r8, 154.466716_r8, 120.913206_r8, 173.833800_r8, &
                             30.025200_r8, 94.937200_r8, 133.402300_r8, 44.051000_r8, 50.485900_r8, &
                             41.050940_r8, 75.042400_r8, 58.076800_r8, 72.061400_r8, 60.050400_r8, &
                             76.049800_r8, 47.032000_r8, 32.040000_r8, 48.039400_r8, 16.040600_r8, &
                            252.730400_r8, 35.452700_r8, 70.905400_r8, 102.904200_r8, 51.452100_r8, &
                             97.457640_r8, 100.916850_r8, 28.010400_r8, 44.009800_r8, 66.007206_r8, &
                             82.461503_r8, 108.135600_r8, 129.089600_r8, 62.132400_r8, 135.064039_r8, &
                            135.064039_r8, 135.064039_r8, 28.010400_r8, 105.108800_r8, 61.057800_r8, &
                             77.057200_r8, 78.064600_r8, 18.998403_r8, 60.050400_r8, 58.035600_r8, &
                              1.007400_r8, 2.014800_r8, 259.823613_r8, 34.013600_r8, 98.078400_r8, &
                             80.911400_r8, 116.948003_r8, 100.493706_r8, 86.467906_r8, 36.460100_r8, &
                             27.025140_r8, 46.024600_r8, 20.005803_r8, 63.012340_r8, 33.006200_r8, &
                             79.011740_r8, 96.910800_r8, 63.031400_r8, 52.459500_r8, 135.114940_r8, &
                            116.112400_r8, 74.076200_r8, 100.113000_r8, 118.127200_r8, 68.114200_r8, &
                            117.119800_r8, 117.119800_r8, 147.125940_r8, 147.125940_r8, 162.117940_r8, &
                            163.125340_r8, 117.119800_r8, 118.127200_r8, 184.350200_r8, 233.355800_r8, &
                             70.087800_r8, 119.093400_r8, 120.100800_r8, 115.063800_r8, 101.079200_r8, &
                            117.078600_r8, 72.102600_r8, 103.094000_r8, 104.101400_r8, 147.084740_r8, &
                            136.228400_r8, 185.234000_r8, 70.087800_r8, 14.006740_r8, 44.012880_r8, &
                            108.010480_r8, 147.125940_r8, 145.111140_r8, 58.442468_r8, 58.442468_r8, &
                             58.442468_r8, 17.028940_r8, 18.036340_r8, 28.010400_r8, 28.010400_r8, &
                             30.006140_r8, 46.005540_r8, 62.004940_r8, 119.074340_r8, 230.232140_r8, &
                            231.239540_r8, 1.007400_r8, 1.007400_r8, 1.007400_r8, 1.007400_r8, &
                             15.999400_r8, 15.999400_r8, 47.998200_r8, 47.998200_r8, 67.451500_r8, &
                             60.076400_r8, 17.006800_r8, 133.100140_r8, 121.047940_r8, 183.117740_r8, &
                             93.102400_r8, 175.114200_r8, 94.109800_r8, 176.121600_r8, 91.083000_r8, &
                             12.011000_r8, 12.011000_r8, 92.090400_r8, 89.068200_r8, 90.075600_r8, &
                             32.066000_r8, 146.056419_r8, 48.065400_r8, 64.064800_r8, 80.064200_r8, &
                            115.107340_r8, 115.107340_r8, 115.107340_r8, 250.445000_r8, 250.445000_r8, &
                            250.445000_r8, 250.445000_r8, 250.445000_r8, 250.445000_r8, 250.445000_r8, &
                            250.445000_r8, 250.445000_r8, 250.445000_r8, 250.445000_r8, 250.445000_r8, &
                            250.445000_r8, 250.445000_r8, 250.445000_r8, 28.010400_r8, 310.582400_r8, &
                            140.134400_r8, 199.218600_r8, 200.226000_r8, 215.240140_r8, 185.234000_r8, &
                            186.241400_r8, 168.227200_r8, 154.201400_r8, 173.140600_r8, 174.148000_r8, &
                             92.136200_r8, 173.140600_r8, 133.141340_r8, 141.908940_r8, 97.457640_r8, &
                             63.012340_r8, 79.011740_r8, 135.114940_r8, 147.125940_r8, 147.125940_r8, &
                            162.117940_r8, 163.125340_r8, 147.084740_r8, 14.006740_r8, 147.125940_r8, &
                            145.111140_r8, 14.006740_r8, 30.006140_r8, 46.005540_r8, 108.010480_r8, &
                            108.010480_r8, 62.004940_r8, 108.010480_r8, 119.074340_r8, 230.232140_r8, &
                            231.239540_r8, 15.999400_r8, 15.999400_r8, 149.118600_r8, 47.998200_r8, &
                            133.100140_r8, 150.126000_r8, 121.047940_r8, 183.117740_r8, 215.240140_r8, &
                            106.162000_r8, 187.166400_r8, 188.173800_r8, 187.166400_r8, 122.161400_r8, &
                            203.165800_r8, 204.173200_r8, 14.006740_r8, 14.006740_r8, 18.014200_r8, &
                             28.010400_r8, 28.010400_r8, 28.010400_r8, 28.010400_r8, 28.010400_r8, &
                             28.010400_r8, 28.010400_r8, 28.010400_r8, 28.010400_r8, 28.010400_r8, &
                             28.010400_r8, 28.010400_r8, 28.010400_r8 /)
      crb_mass(:273) = (/ 84.077000_r8, 60.055000_r8, 60.055000_r8, 60.055000_r8, 12.011000_r8, &
                             12.011000_r8, 12.011000_r8, 180.165000_r8, 180.165000_r8, 72.066000_r8, &
                             72.066000_r8, 72.066000_r8, 72.066000_r8, 72.066000_r8, 60.055000_r8, &
                             48.044000_r8, 60.055000_r8, 60.055000_r8, 72.066000_r8, 60.055000_r8, &
                             48.044000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 84.077000_r8, 84.077000_r8, 84.077000_r8, 24.022000_r8, &
                             24.022000_r8, 24.022000_r8, 24.022000_r8, 24.022000_r8, 24.022000_r8, &
                             36.033000_r8, 36.033000_r8, 36.033000_r8, 36.033000_r8, 72.066000_r8, &
                             72.066000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, &
                             24.022000_r8, 24.022000_r8, 24.022000_r8, 12.011000_r8, 12.011000_r8, &
                             12.011000_r8, 12.011000_r8, 24.022000_r8, 24.022000_r8, 12.011000_r8, &
                             24.022000_r8, 24.022000_r8, 36.033000_r8, 36.033000_r8, 24.022000_r8, &
                             24.022000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, &
                             12.011000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, &
                             12.011000_r8, 84.077000_r8, 60.055000_r8, 24.022000_r8, 0.000000_r8, &
                              0.000000_r8, 0.000000_r8, 12.011000_r8, 48.044000_r8, 24.022000_r8, &
                             24.022000_r8, 24.022000_r8, 0.000000_r8, 24.022000_r8, 24.022000_r8, &
                              0.000000_r8, 0.000000_r8, 24.022000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 24.022000_r8, 24.022000_r8, 12.011000_r8, 0.000000_r8, &
                             12.011000_r8, 12.011000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 0.000000_r8, 12.011000_r8, 0.000000_r8, 48.044000_r8, &
                             60.055000_r8, 36.033000_r8, 60.055000_r8, 60.055000_r8, 60.055000_r8, &
                             60.055000_r8, 60.055000_r8, 60.055000_r8, 60.055000_r8, 60.055000_r8, &
                             60.055000_r8, 60.055000_r8, 60.055000_r8, 156.143000_r8, 156.143000_r8, &
                             48.044000_r8, 48.044000_r8, 48.044000_r8, 48.044000_r8, 48.044000_r8, &
                             48.044000_r8, 48.044000_r8, 48.044000_r8, 48.044000_r8, 48.044000_r8, &
                            120.110000_r8, 120.110000_r8, 48.044000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 60.055000_r8, 60.055000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 0.000000_r8, 0.000000_r8, 12.011000_r8, 12.011000_r8, &
                              0.000000_r8, 0.000000_r8, 0.000000_r8, 36.033000_r8, 120.110000_r8, &
                            120.110000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, &
                             12.011000_r8, 0.000000_r8, 48.044000_r8, 24.022000_r8, 84.077000_r8, &
                             72.066000_r8, 72.066000_r8, 72.066000_r8, 72.066000_r8, 36.033000_r8, &
                             12.011000_r8, 12.011000_r8, 36.033000_r8, 36.033000_r8, 36.033000_r8, &
                              0.000000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 0.000000_r8, 0.000000_r8, 180.165000_r8, 180.165000_r8, &
                            180.165000_r8, 180.165000_r8, 180.165000_r8, 180.165000_r8, 180.165000_r8, &
                            180.165000_r8, 180.165000_r8, 180.165000_r8, 180.165000_r8, 180.165000_r8, &
                            180.165000_r8, 180.165000_r8, 180.165000_r8, 12.011000_r8, 264.242000_r8, &
                             84.077000_r8, 120.110000_r8, 120.110000_r8, 120.110000_r8, 120.110000_r8, &
                            120.110000_r8, 120.110000_r8, 108.099000_r8, 84.077000_r8, 84.077000_r8, &
                             84.077000_r8, 84.077000_r8, 60.055000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 0.000000_r8, 48.044000_r8, 60.055000_r8, 60.055000_r8, &
                             60.055000_r8, 60.055000_r8, 48.044000_r8, 0.000000_r8, 60.055000_r8, &
                             60.055000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, &
                              0.000000_r8, 0.000000_r8, 0.000000_r8, 36.033000_r8, 120.110000_r8, &
                            120.110000_r8, 0.000000_r8, 0.000000_r8, 60.055000_r8, 0.000000_r8, &
                             48.044000_r8, 60.055000_r8, 24.022000_r8, 84.077000_r8, 120.110000_r8, &
                             96.088000_r8, 96.088000_r8, 96.088000_r8, 96.088000_r8, 96.088000_r8, &
                             96.088000_r8, 96.088000_r8, 0.000000_r8, 0.000000_r8, 0.000000_r8, &
                             12.011000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, &
                             12.011000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, 12.011000_r8, &
                             12.011000_r8, 12.011000_r8, 12.011000_r8 /)
      fix_mass(: 3) = (/ 0.00000000_r8, 31.9988000_r8, 28.0134800_r8 /)
      clsmap(: 3,1) = (/ 232, 258, 259 /)
      clsmap(:270,4) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, &
                            11, 12, 13, 14, 15, 16, 17, 18, 19, 20, &
                            21, 22, 23, 24, 25, 26, 27, 28, 29, 30, &
                            31, 32, 33, 34, 35, 36, 37, 38, 39, 40, &
                            41, 42, 43, 44, 45, 46, 47, 48, 49, 50, &
                            51, 52, 53, 54, 55, 56, 57, 58, 59, 60, &
                            61, 62, 63, 64, 65, 66, 67, 68, 69, 70, &
                            71, 72, 73, 261, 262, 263, 264, 265, 266, 267, &
                           268, 269, 270, 271, 272, 273, 74, 75, 76, 77, &
                            78, 79, 80, 81, 82, 83, 84, 85, 86, 87, &
                            88, 89, 90, 91, 92, 93, 94, 95, 96, 97, &
                            98, 99, 100, 101, 102, 103, 104, 105, 106, 107, &
                           108, 109, 110, 111, 112, 113, 114, 115, 116, 117, &
                           118, 119, 120, 121, 122, 123, 124, 125, 126, 127, &
                           128, 129, 130, 131, 132, 133, 134, 135, 136, 137, &
                           138, 139, 140, 141, 142, 143, 144, 145, 146, 147, &
                           148, 149, 150, 151, 152, 153, 154, 155, 156, 157, &
                           158, 159, 160, 161, 162, 163, 164, 165, 166, 167, &
                           168, 169, 170, 171, 172, 173, 174, 175, 176, 177, &
                           178, 179, 180, 181, 182, 183, 184, 185, 186, 187, &
                           188, 189, 190, 191, 192, 193, 194, 195, 196, 197, &
                           198, 199, 200, 201, 202, 203, 204, 205, 206, 207, &
                           208, 209, 210, 211, 212, 213, 214, 215, 216, 217, &
                           218, 219, 220, 221, 222, 223, 224, 225, 226, 227, &
                           228, 229, 230, 231, 233, 234, 235, 236, 237, 238, &
                           239, 240, 241, 242, 243, 244, 245, 246, 247, 248, &
                           249, 250, 251, 252, 253, 254, 255, 256, 257, 260 /)
      permute(:270,4) = (/ 192, 190, 222, 189, 1, 2, 3, 225, 55, 88, &
                            179, 56, 153, 89, 152, 157, 125, 180, 137, 105, &
                            161, 252, 102, 264, 174, 4, 103, 175, 132, 123, &
                            169, 213, 112, 136, 124, 229, 209, 151, 69, 197, &
                            114, 70, 83, 84, 78, 85, 79, 86, 80, 166, &
                            261, 181, 71, 233, 143, 66, 251, 223, 244, 191, &
                            182, 256, 201, 155, 253, 158, 262, 87, 65, 260, &
                            212, 5, 239, 6, 7, 8, 9, 10, 11, 12, &
                             13, 14, 15, 16, 17, 18, 207, 121, 129, 92, &
                            203, 144, 19, 20, 21, 22, 194, 119, 205, 73, &
                            220, 234, 227, 254, 245, 74, 183, 75, 208, 128, &
                            120, 133, 268, 100, 224, 126, 255, 258, 154, 198, &
                            173, 206, 238, 101, 237, 139, 76, 214, 246, 247, &
                            184, 168, 242, 149, 57, 193, 59, 58, 243, 248, &
                            130, 200, 250, 219, 163, 196, 131, 177, 228, 60, &
                            249, 178, 111, 122, 147, 232, 23, 24, 25, 67, &
                             26, 27, 28, 257, 267, 259, 210, 230, 148, 29, &
                             30, 31, 32, 269, 265, 266, 33, 134, 142, 263, &
                            104, 170, 77, 176, 171, 90, 135, 204, 34, 35, &
                            172, 236, 150, 167, 36, 240, 211, 113, 37, 38, &
                             39, 40, 41, 42, 43, 44, 45, 46, 47, 48, &
                             49, 50, 51, 52, 53, 54, 61, 62, 93, 235, &
                            187, 186, 221, 159, 226, 231, 199, 188, 91, 63, &
                             72, 115, 160, 215, 94, 138, 116, 117, 145, 95, &
                             96, 81, 97, 140, 216, 217, 106, 127, 218, 82, &
                            107, 162, 118, 165, 146, 241, 164, 108, 98, 99, &
                             68, 141, 109, 202, 195, 64, 110, 185, 156, 270 /)
      diag_map(:270) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, &
                            11, 12, 13, 14, 15, 16, 17, 18, 19, 20, &
                            21, 22, 23, 24, 25, 26, 27, 28, 29, 30, &
                            31, 32, 33, 34, 35, 36, 37, 38, 39, 40, &
                            41, 42, 43, 44, 45, 46, 47, 48, 49, 50, &
                            51, 52, 53, 54, 60, 66, 72, 78, 80, 86, &
                            87, 93, 99, 105, 106, 109, 112, 115, 117, 121, &
                           125, 129, 131, 134, 139, 142, 145, 148, 153, 158, &
                           163, 165, 168, 174, 180, 186, 192, 195, 202, 206, &
                           212, 219, 224, 228, 231, 234, 237, 240, 243, 246, &
                           251, 256, 259, 262, 265, 268, 273, 275, 280, 289, &
                           294, 298, 303, 307, 311, 315, 320, 324, 328, 332, &
                           338, 342, 348, 354, 360, 363, 366, 372, 378, 383, &
                           388, 394, 400, 407, 412, 417, 422, 428, 431, 435, &
                           438, 441, 449, 457, 469, 472, 474, 480, 486, 492, &
                           498, 504, 510, 516, 522, 528, 534, 538, 545, 552, &
                           556, 568, 571, 577, 580, 583, 592, 598, 608, 616, &
                           625, 633, 641, 650, 660, 670, 675, 687, 698, 707, &
                           713, 722, 730, 737, 747, 756, 762, 775, 786, 797, &
                           807, 814, 823, 831, 846, 858, 869, 879, 891, 904, &
                           912, 922, 935, 946, 959, 970, 977, 982, 991,1003, &
                          1010,1019,1032,1048,1067,1079,1093,1104,1108,1121, &
                          1138,1157,1167,1174,1199,1224,1234,1260,1291,1315, &
                          1329,1343,1358,1369,1387,1405,1416,1428,1443,1456, &
                          1479,1503,1522,1538,1551,1580,1619,1648,1672,1699, &
                          1739,1757,1774,1791,1809,1869,1972,2092,2159,2192, &
                          2222,2269,2471,2501,2546,2614,2668,2698,2738,2767 /)
      extfrc_lst(: 13) = (/ 'NO2             ','bc_a4           ','num_a1          ','num_a2          ','num_a4          ', &
                            'pom_a4          ','SO2             ','so4_a1          ','so4_a2          ','NO              ', &
                            'XNO             ','N               ','AOA_NH          ' /)
      frc_from_dataset(: 13) = (/ .true., .true., .true., .true., .true., &
                                  .true., .true., .true., .true., .false., &
                                  .false., .false., .false. /)
      inv_lst(: 3) = (/ 'M               ', 'O2              ', 'N2              ' /)
      if( allocated( rxt_tag_lst ) ) then
         deallocate( rxt_tag_lst )
      end if
      allocate( rxt_tag_lst(rxt_tag_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate rxt_tag_lst; error = ',ios
         call endrun
      end if
      if( allocated( rxt_tag_map ) ) then
         deallocate( rxt_tag_map )
      end if
      allocate( rxt_tag_map(rxt_tag_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate rxt_tag_map; error = ',ios
         call endrun
      end if
      rxt_tag_lst( 1: 200) = (/ 'jh2o_b                          ', 'jh2o_c                          ', &
                                      'jh2o_a                          ', 'jh2o2                           ', &
                                      'jo2_b                           ', 'jo2_a                           ', &
                                      'jo3_b                           ', 'jo3_a                           ', &
                                      'jxo3_a                          ', 'jxo3_b                          ', &
                                      'jhno3                           ', 'jho2no2_b                       ', &
                                      'jho2no2_a                       ', 'jn2o                            ', &
                                      'jn2o5_b                         ', 'jn2o5_a                         ', &
                                      'jno                             ', 'jno2                            ', &
                                      'jno3_b                          ', 'jno3_a                          ', &
                                      'jxhno3                          ', 'jxho2no2_b                      ', &
                                      'jxho2no2_a                      ', 'jxno                            ', &
                                      'jxno2                           ', 'jxno2no3_b                      ', &
                                      'jxno2no3_a                      ', 'jxno2xno3_b                     ', &
                                      'jxno2xno3_a                     ', 'jxno3_a                         ', &
                                      'jxno3_b                         ', 'jxno3no2_b                      ', &
                                      'jxno3no2_a                      ', 'jalknit                         ', &
                                      'jalkooh                         ', 'jbenzooh                        ', &
                                      'jbepomuc                        ', 'jbigald                         ', &
                                      'jbigald1                        ', 'jbigald2                        ', &
                                      'jbigald3                        ', 'jbigald4                        ', &
                                      'jbzooh                          ', 'jc2h5ooh                        ', &
                                      'jc3h7ooh                        ', 'jc6h5ooh                        ', &
                                      'jch2o_b                         ', 'jch2o_a                         ', &
                                      'jch3cho                         ', 'jacet                           ', &
                                      'jmgly                           ', 'jch3co3h                        ', &
                                      'jch3ooh                         ', 'jch4_a                          ', &
                                      'jch4_b                          ', 'jco2                            ', &
                                      'jeooh                           ', 'jglyald                         ', &
                                      'jglyoxal                        ', 'jhonitr                         ', &
                                      'jhpald                          ', 'jhyac                           ', &
                                      'jisopnooh                       ', 'jisopooh                        ', &
                                      'jmacr_a                         ', 'jmacr_b                         ', &
                                      'jmek                            ', 'jmekooh                         ', &
                                      'jmpan                           ', 'jmvk                            ', &
                                      'jnc4cho                         ', 'jnoa                            ', &
                                      'jnterpooh                       ', 'jonitr                          ', &
                                      'jpan                            ', 'jphenooh                        ', &
                                      'jpooh                           ', 'jrooh                           ', &
                                      'jtepomuc                        ', 'jterp2ooh                       ', &
                                      'jterpnit                        ', 'jterpooh                        ', &
                                      'jterprd1                        ', 'jterprd2                        ', &
                                      'jtolooh                         ', 'jxalknit                        ', &
                                      'jxhonitr                        ', 'jxisopnooh                      ', &
                                      'jxmpan                          ', 'jxnc4cho                        ', &
                                      'jxnoa                           ', 'jxnterpooh                      ', &
                                      'jxonitr                         ', 'jxooh                           ', &
                                      'jxpan                           ', 'jxterpnit                       ', &
                                      'jxylenooh                       ', 'jxylolooh                       ', &
                                      'jbrcl                           ', 'jbro                            ', &
                                      'jbrono2_b                       ', 'jbrono2_a                       ', &
                                      'jccl4                           ', 'jcf2clbr                        ', &
                                      'jcf3br                          ', 'jcfcl3                          ', &
                                      'jcfc113                         ', 'jcfc114                         ', &
                                      'jcfc115                         ', 'jcf2cl2                         ', &
                                      'jch2br2                         ', 'jch3br                          ', &
                                      'jch3ccl3                        ', 'jch3cl                          ', &
                                      'jchbr3                          ', 'jcl2                            ', &
                                      'jcl2o2                          ', 'jclo                            ', &
                                      'jclono2_a                       ', 'jclono2_b                       ', &
                                      'jcof2                           ', 'jcofcl                          ', &
                                      'jh2402                          ', 'jhbr                            ', &
                                      'jhcfc141b                       ', 'jhcfc142b                       ', &
                                      'jhcfc22                         ', 'jhcl                            ', &
                                      'jhf                             ', 'jhobr                           ', &
                                      'jhocl                           ', 'joclo                           ', &
                                      'jsf6                            ', 'jxbrono2_a                      ', &
                                      'jxbrono2_b                      ', 'jxclono2_a                      ', &
                                      'jxclono2_b                      ', 'jh2so4                          ', &
                                      'jocs                            ', 'jso                             ', &
                                      'jso2                            ', 'jso3                            ', &
                                      'jsoa1_a1                        ', 'jsoa1_a2                        ', &
                                      'jsoa2_a1                        ', 'jsoa2_a2                        ', &
                                      'jsoa3_a1                        ', 'jsoa3_a2                        ', &
                                      'jsoa4_a1                        ', 'jsoa4_a2                        ', &
                                      'jsoa5_a1                        ', 'jsoa5_a2                        ', &
                                      'O1D_H2                          ', 'O1D_H2O                         ', &
                                      'O1D_N2                          ', 'O1D_O2ab                        ', &
                                      'O1D_O3                          ', 'O1D_XO3                         ', &
                                      'O_O3                            ', 'O_XO3                           ', &
                                      'usr_O_O                         ', 'usr_O_O2                        ', &
                                      'usr_XO_O                        ', 'usr_XO_O2                       ', &
                                      'XO1D_H2                         ', 'XO1D_H2O                        ', &
                                      'XO1D_N2                         ', 'XO1D_O2ab                       ', &
                                      'XO1D_O3                         ', 'XO_O3                           ', &
                                      'H2_O                            ', 'H2O2_O                          ', &
                                      'H2O2_XO                         ', 'H2_XO                           ', &
                                      'H_HO2                           ', 'H_HO2a                          ', &
                                      'H_HO2b                          ', 'H_O2                            ', &
                                      'HO2_O                           ', 'HO2_O3                          ', &
                                      'HO2_XO                          ', 'HO2_XO3                         ', &
                                      'H_O3                            ', 'H_XO3                           ', &
                                      'OH_H2                           ', 'OH_H2O2                         ', &
                                      'OH_HO2                          ', 'OH_O                            ', &
                                      'OH_O3                           ', 'OH_OH                           ', &
                                      'OH_OH_M                         ', 'OH_XO                           ', &
                                      'OH_XO3                          ', 'usr_HO2_HO2                     ', &
                                      'HO2NO2_OH                       ', 'N_NO                            ', &
                                      'N_NO2a                          ', 'N_NO2b                          ', &
                                      'N_NO2c                          ', 'N_O2                            ' /)
      rxt_tag_lst( 201: 400) = (/ 'NO2_O                           ', 'NO2_O3                          ', &
                                      'NO2_O_M                         ', 'NO2_XO                          ', &
                                      'NO2_XO3                         ', 'NO2_XO_M                        ', &
                                      'NO3_HO2                         ', 'NO3_NO                          ', &
                                      'NO3_O                           ', 'NO3_OH                          ', &
                                      'NO3_XNO                         ', 'NO3_XO                          ', &
                                      'N_OH                            ', 'NO_HO2                          ', &
                                      'NO_O3                           ', 'NO_O_M                          ', &
                                      'NO_XO3                          ', 'NO_XO_M                         ', &
                                      'N_XNO                           ', 'N_XNO2a                         ', &
                                      'N_XNO2b                         ', 'N_XNO2c                         ', &
                                      'O1D_N2Oa                        ', 'O1D_N2Ob                        ', &
                                      'tag_NO2_HO2                     ', 'tag_NO2_NO3                     ', &
                                      'tag_NO2_OH                      ', 'tag_NO2_XNO3                    ', &
                                      'tag_XNO2_HO2                    ', 'tag_XNO2_NO3                    ', &
                                      'tag_XNO2_OH                     ', 'tag_XNO2_XNO3                   ', &
                                      'usr_HNO3_OH                     ', 'usr_HO2NO2_M                    ', &
                                      'usr_N2O5_M                      ', 'usr_XHNO3_OH                    ', &
                                      'usr_XHO2NO2_M                   ', 'usr_XNO2NO3_M                   ', &
                                      'usr_XNO2XNO3_M                  ', 'usr_XNO3NO2_M                   ', &
                                      'XHO2NO2_OH                      ', 'XN_NO                           ', &
                                      'XN_NO2a                         ', 'XN_NO2b                         ', &
                                      'XN_NO2c                         ', 'XN_O2                           ', &
                                      'XNO2_O                          ', 'XNO2_O3                         ', &
                                      'XNO2_O_M                        ', 'XNO3_HO2                        ', &
                                      'XNO3_NO                         ', 'XNO3_O                          ', &
                                      'XNO3_OH                         ', 'XN_OH                           ', &
                                      'XNO_HO2                         ', 'XNO_O3                          ', &
                                      'XNO_O_M                         ', 'XO1D_N2Oa                       ', &
                                      'XO1D_N2Ob                       ', 'CL_CH2O                         ', &
                                      'CL_CH4                          ', 'CL_H2                           ', &
                                      'CL_H2O2                         ', 'CL_HO2a                         ', &
                                      'CL_HO2b                         ', 'CL_O3                           ', &
                                      'CLO_CH3O2                       ', 'CLO_CLOa                        ', &
                                      'CLO_CLOb                        ', 'CLO_CLOc                        ', &
                                      'CLO_HO2                         ', 'CLO_NO                          ', &
                                      'CLONO2_CL                       ', 'CLO_NO2_M                       ', &
                                      'CLONO2_O                        ', 'CLONO2_OH                       ', &
                                      'CLONO2_XO                       ', 'CLO_O                           ', &
                                      'CLO_OHa                         ', 'CLO_OHb                         ', &
                                      'CLO_XNO                         ', 'CLO_XNO2_M                      ', &
                                      'CLO_XO                          ', 'CL_XO3                          ', &
                                      'HCL_O                           ', 'HCL_OH                          ', &
                                      'HCL_XO                          ', 'HOCL_CL                         ', &
                                      'HOCL_O                          ', 'HOCL_OH                         ', &
                                      'HOCL_XO                         ', 'O1D_CCL4                        ', &
                                      'O1D_CF2CLBR                     ', 'O1D_CFC11                       ', &
                                      'O1D_CFC113                      ', 'O1D_CFC114                      ', &
                                      'O1D_CFC115                      ', 'O1D_CFC12                       ', &
                                      'O1D_HCLa                        ', 'O1D_HCLb                        ', &
                                      'tag_CLO_CLO_M                   ', 'usr_CL2O2_M                     ', &
                                      'XCLONO2_CL                      ', 'XCLONO2_O                       ', &
                                      'XCLONO2_OH                      ', 'XO1D_CCL4                       ', &
                                      'XO1D_CF2CLBR                    ', 'XO1D_CFC11                      ', &
                                      'XO1D_CFC113                     ', 'XO1D_CFC114                     ', &
                                      'XO1D_CFC115                     ', 'XO1D_CFC12                      ', &
                                      'XO1D_HCLa                       ', 'XO1D_HCLb                       ', &
                                      'BR_CH2O                         ', 'BR_HO2                          ', &
                                      'BR_O3                           ', 'BRO_BRO                         ', &
                                      'BRO_CLOa                        ', 'BRO_CLOb                        ', &
                                      'BRO_CLOc                        ', 'BRO_HO2                         ', &
                                      'BRO_NO                          ', 'BRO_NO2_M                       ', &
                                      'BRONO2_O                        ', 'BRONO2_XO                       ', &
                                      'BRO_O                           ', 'BRO_OH                          ', &
                                      'BRO_XNO2_M                      ', 'BRO_XO                          ', &
                                      'BR_XO3                          ', 'HBR_O                           ', &
                                      'HBR_OH                          ', 'HBR_XO                          ', &
                                      'HOBR_O                          ', 'HOBR_XO                         ', &
                                      'O1D_CF3BR                       ', 'O1D_CHBR3                       ', &
                                      'O1D_H2402                       ', 'O1D_HBRa                        ', &
                                      'O1D_HBRb                        ', 'XBRONO2_O                       ', &
                                      'XO1D_CF3BR                      ', 'XO1D_CHBR3                      ', &
                                      'XO1D_H2402                      ', 'XO1D_HBRa                       ', &
                                      'XO1D_HBRb                       ', 'F_CH4                           ', &
                                      'F_H2                            ', 'F_H2O                           ', &
                                      'F_HNO3                          ', 'F_XHNO3                         ', &
                                      'O1D_COF2                        ', 'O1D_COFCL                       ', &
                                      'XO1D_COF2                       ', 'XO1D_COFCL                      ', &
                                      'CH2BR2_CL                       ', 'CH2BR2_OH                       ', &
                                      'CH3BR_CL                        ', 'CH3BR_OH                        ', &
                                      'CH3CCL3_OH                      ', 'CH3CL_CL                        ', &
                                      'CH3CL_OH                        ', 'CHBR3_CL                        ', &
                                      'CHBR3_OH                        ', 'HCFC141B_OH                     ', &
                                      'HCFC142B_OH                     ', 'HCFC22_OH                       ', &
                                      'O1D_CH2BR2                      ', 'O1D_CH3BR                       ', &
                                      'O1D_HCFC141B                    ', 'O1D_HCFC142B                    ', &
                                      'O1D_HCFC22                      ', 'XO1D_CH2BR2                     ', &
                                      'XO1D_CH3BR                      ', 'XO1D_HCFC141B                   ', &
                                      'XO1D_HCFC142B                   ', 'XO1D_HCFC22                     ', &
                                      'CH2O_HO2                        ', 'CH2O_NO3                        ', &
                                      'CH2O_O                          ', 'CH2O_OH                         ', &
                                      'CH2O_XNO3                       ', 'CH2O_XO                         ', &
                                      'CH3O2_CH3O2a                    ', 'CH3O2_CH3O2b                    ', &
                                      'CH3O2_HO2                       ', 'CH3O2_NO                        ', &
                                      'CH3O2_XNO                       ', 'CH3OH_OH                        ', &
                                      'CH3OOH_OH                       ', 'CH4_OH                          ', &
                                      'CO_OH_M                         ', 'HCN_OH                          ', &
                                      'HCOOH_OH                        ', 'HOCH2OO_HO2                     ', &
                                      'HOCH2OO_M                       ', 'HOCH2OO_NO                      ', &
                                      'HOCH2OO_XNO                     ', 'O1D_CH4a                        ' /)
      rxt_tag_lst( 401: 600) = (/ 'O1D_CH4b                        ', 'O1D_CH4c                        ', &
                                      'O1D_HCN                         ', 'usr_CO_OH_b                     ', &
                                      'usr_CO01_OH                     ', 'CO01_OH_M                       ', &
                                      'usr_CO02_OH                     ', 'CO02_OH_M                       ', &
                                      'usr_CO03_OH                     ', 'CO03_OH_M                       ', &
                                      'usr_CO04_OH                     ', 'CO04_OH_M                       ', &
                                      'usr_CO05_OH                     ', 'CO05_OH_M                       ', &
                                      'usr_CO06_OH                     ', 'CO06_OH_M                       ', &
                                      'usr_CO07_OH                     ', 'CO07_OH_M                       ', &
                                      'usr_CO08_OH                     ', 'CO08_OH_M                       ', &
                                      'usr_CO09_OH                     ', 'CO09_OH_M                       ', &
                                      'usr_CO10_OH                     ', 'CO10_OH_M                       ', &
                                      'usr_CO11_OH                     ', 'CO11_OH_M                       ', &
                                      'usr_CO12_OH                     ', 'CO12_OH_M                       ', &
                                      'usr_CO13_OH                     ', 'CO13_OH_M                       ', &
                                      'XO1D_CH4a                       ', 'XO1D_CH4b                       ', &
                                      'XO1D_CH4c                       ', 'XO1D_HCN                        ', &
                                      'C2H2_CL_M                       ', 'C2H2_OH_M                       ', &
                                      'C2H4_CL_M                       ', 'C2H4_O3                         ', &
                                      'C2H4_XO3                        ', 'C2H5O2_C2H5O2                   ', &
                                      'C2H5O2_CH3O2                    ', 'C2H5O2_HO2                      ', &
                                      'C2H5O2_NO                       ', 'C2H5O2_XNO                      ', &
                                      'C2H5OH_OH                       ', 'C2H5OOH_OH                      ', &
                                      'C2H6_CL                         ', 'C2H6_OH                         ', &
                                      'CH3CHO_NO3                      ', 'CH3CHO_OH                       ', &
                                      'CH3CHO_XNO3                     ', 'CH3CN_OH                        ', &
                                      'CH3CO3_CH3CO3                   ', 'CH3CO3_CH3O2                    ', &
                                      'CH3CO3_HO2                      ', 'CH3CO3_NO                       ', &
                                      'CH3CO3_XNO                      ', 'CH3COOH_OH                      ', &
                                      'CH3COOOH_OH                     ', 'EO2_HO2                         ', &
                                      'EO2_NO                          ', 'EO2_XNO                         ', &
                                      'EO_M                            ', 'EO_O2                           ', &
                                      'GLYALD_OH                       ', 'GLYOXAL_OH                      ', &
                                      'PAN_OH                          ', 'tag_C2H4_OH                     ', &
                                      'tag_CH3CO3_NO2                  ', 'tag_CH3CO3_XNO2                 ', &
                                      'usr_PAN_M                       ', 'usr_XPAN_M                      ', &
                                      'XPAN_OH                         ', 'C3H6_NO3                        ', &
                                      'C3H6_O3                         ', 'C3H6_XNO3                       ', &
                                      'C3H6_XO3                        ', 'C3H7O2_CH3O2                    ', &
                                      'C3H7O2_HO2                      ', 'C3H7O2_NO                       ', &
                                      'C3H7O2_XNO                      ', 'C3H7OOH_OH                      ', &
                                      'C3H8_OH                         ', 'CH3COCHO_NO3                    ', &
                                      'CH3COCHO_OH                     ', 'CH3COCHO_XNO3                   ', &
                                      'HYAC_OH                         ', 'NOA_OH                          ', &
                                      'PO2_HO2                         ', 'PO2_NO                          ', &
                                      'PO2_XNO                         ', 'POOH_OH                         ', &
                                      'RO2_CH3O2                       ', 'RO2_HO2                         ', &
                                      'RO2_NO                          ', 'RO2_XNO                         ', &
                                      'ROOH_OH                         ', 'tag_C3H6_OH                     ', &
                                      'usr_CH3COCH3_OH                 ', 'XNOA_OH                         ', &
                                      'BIGENE_NO3                      ', 'BIGENE_OH                       ', &
                                      'BIGENE_XNO3                     ', 'ENEO2_NO                        ', &
                                      'ENEO2_NOb                       ', 'ENEO2_XNO                       ', &
                                      'ENEO2_XNOb                      ', 'HONITR_OH                       ', &
                                      'MACRO2_CH3CO3                   ', 'MACRO2_CH3O2                    ', &
                                      'MACRO2_HO2                      ', 'MACRO2_NO3                      ', &
                                      'MACRO2_NOa                      ', 'MACRO2_NOb                      ', &
                                      'MACRO2_XNO3                     ', 'MACRO2_XNOa                     ', &
                                      'MACRO2_XNOb                     ', 'MACR_O3                         ', &
                                      'MACR_OH                         ', 'MACROOH_OH                      ', &
                                      'MACR_XO3                        ', 'MCO3_CH3CO3                     ', &
                                      'MCO3_CH3O2                      ', 'MCO3_HO2                        ', &
                                      'MCO3_MCO3                       ', 'MCO3_NO                         ', &
                                      'MCO3_NO3                        ', 'MCO3_XNO                        ', &
                                      'MCO3_XNO3                       ', 'MEKO2_HO2                       ', &
                                      'MEKO2_NO                        ', 'MEKO2_XNO                       ', &
                                      'MEK_OH                          ', 'MEKOOH_OH                       ', &
                                      'MPAN_OH_M                       ', 'MVK_O3                          ', &
                                      'MVK_OH                          ', 'MVK_XO3                         ', &
                                      'usr_MCO3_NO2                    ', 'usr_MCO3_XNO2                   ', &
                                      'usr_MPAN_M                      ', 'usr_XMPAN_M                     ', &
                                      'XHONITR_OH                      ', 'XMPAN_OH_M                      ', &
                                      'ALKNIT_OH                       ', 'ALKO2_HO2                       ', &
                                      'ALKO2_NO                        ', 'ALKO2_NOb                       ', &
                                      'ALKO2_XNO                       ', 'ALKO2_XNOb                      ', &
                                      'ALKOOH_OH                       ', 'BIGALK_OH                       ', &
                                      'HPALD_OH                        ', 'HYDRALD_OH                      ', &
                                      'IEPOX_OH                        ', 'ISOPAO2_CH3CO3                  ', &
                                      'ISOPAO2_CH3O2                   ', 'ISOPAO2_HO2                     ', &
                                      'ISOPAO2_NO                      ', 'ISOPAO2_NO3                     ', &
                                      'ISOPAO2_XNO                     ', 'ISOPAO2_XNO3                    ', &
                                      'ISOPBO2_CH3CO3                  ', 'ISOPBO2_CH3O2                   ', &
                                      'ISOPBO2_HO2                     ', 'ISOPBO2_M                       ', &
                                      'ISOPBO2_NO                      ', 'ISOPBO2_NO3                     ', &
                                      'ISOPBO2_XNO                     ', 'ISOPBO2_XNO3                    ', &
                                      'ISOPNITA_OH                     ', 'ISOPNITB_OH                     ', &
                                      'ISOP_NO3                        ', 'ISOPNO3_CH3CO3                  ', &
                                      'ISOPNO3_CH3O2                   ', 'ISOPNO3_HO2                     ', &
                                      'ISOPNO3_NO                      ', 'ISOPNO3_NO3                     ', &
                                      'ISOPNO3_XNO                     ', 'ISOPNO3_XNO3                    ', &
                                      'ISOPNOOH_OH                     ', 'ISOP_O3                         ', &
                                      'ISOP_OH                         ', 'ISOPOOH_OH                      ', &
                                      'ISOP_XNO3                       ', 'ISOP_XO3                        ', &
                                      'NC4CH2OH_OH                     ', 'NC4CHO_OH                       ', &
                                      'XALKNIT_OH                      ', 'XISOPNITA_OH                    ', &
                                      'XISOPNITB_OH                    ', 'XISOPNO3_CH3CO3                 ', &
                                      'XISOPNO3_CH3O2                  ', 'XISOPNO3_HO2                    ', &
                                      'XISOPNO3_NO                     ', 'XISOPNO3_NO3                    ', &
                                      'XISOPNOOH_OH                    ', 'XNC4CH2OH_OH                    ', &
                                      'XNC4CHO_OH                      ', 'XO2_CH3CO3                      ' /)
      rxt_tag_lst( 601: 800) = (/ 'XO2_CH3O2                       ', 'XO2_HO2                         ', &
                                      'XO2_NO                          ', 'XO2_NO3                         ', &
                                      'XO2_XNO                         ', 'XO2_XNO3                        ', &
                                      'XOOH_OH                         ', 'ACBZO2_HO2                      ', &
                                      'ACBZO2_NO                       ', 'ACBZO2_XNO                      ', &
                                      'BENZENE_OH                      ', 'BENZO2_HO2                      ', &
                                      'BENZO2_NO                       ', 'BENZO2_XNO                      ', &
                                      'BENZOOH_OH                      ', 'BZALD_OH                        ', &
                                      'BZOO_HO2                        ', 'BZOOH_OH                        ', &
                                      'BZOO_NO                         ', 'BZOO_XNO                        ', &
                                      'C6H5O2_HO2                      ', 'C6H5O2_NO                       ', &
                                      'C6H5O2_XNO                      ', 'C6H5OOH_OH                      ', &
                                      'CRESOL_OH                       ', 'DICARBO2_HO2                    ', &
                                      'DICARBO2_NO                     ', 'DICARBO2_NO2                    ', &
                                      'DICARBO2_XNO                    ', 'DICARBO2_XNO2                   ', &
                                      'MALO2_HO2                       ', 'MALO2_NO                        ', &
                                      'MALO2_NO2                       ', 'MALO2_XNO                       ', &
                                      'MALO2_XNO2                      ', 'MDIALO2_HO2                     ', &
                                      'MDIALO2_NO                      ', 'MDIALO2_NO2                     ', &
                                      'MDIALO2_XNO                     ', 'MDIALO2_XNO2                    ', &
                                      'PHENO2_HO2                      ', 'PHENO2_NO                       ', &
                                      'PHENO2_XNO                      ', 'PHENOL_OH                       ', &
                                      'PHENO_NO2                       ', 'PHENO_O3                        ', &
                                      'PHENOOH_OH                      ', 'PHENO_XNO2                      ', &
                                      'PHENO_XO3                       ', 'tag_ACBZO2_NO2                  ', &
                                      'tag_ACBZO2_XNO2                 ', 'TOLO2_HO2                       ', &
                                      'TOLO2_NO                        ', 'TOLO2_XNO                       ', &
                                      'TOLOOH_OH                       ', 'TOLUENE_OH                      ', &
                                      'usr_PBZNIT_M                    ', 'usr_XPBZNIT_M                   ', &
                                      'XYLENES_OH                      ', 'XYLENO2_HO2                     ', &
                                      'XYLENO2_NO                      ', 'XYLENO2_XNO                     ', &
                                      'XYLENOOH_OH                     ', 'XYLOLO2_HO2                     ', &
                                      'XYLOLO2_NO                      ', 'XYLOLO2_XNO                     ', &
                                      'XYLOL_OH                        ', 'XYLOLOOH_OH                     ', &
                                      'BCARY_NO3                       ', 'BCARY_O3                        ', &
                                      'BCARY_OH                        ', 'BCARY_XNO3                      ', &
                                      'BCARY_XO3                       ', 'MTERP_NO3                       ', &
                                      'MTERP_O3                        ', 'MTERP_OH                        ', &
                                      'MTERP_XNO3                      ', 'MTERP_XO3                       ', &
                                      'NTERPO2_CH3O2                   ', 'NTERPO2_HO2                     ', &
                                      'NTERPO2_NO                      ', 'NTERPO2_NO3                     ', &
                                      'NTERPO2_XNO                     ', 'NTERPO2_XNO3                    ', &
                                      'NTERPOOH_OH                     ', 'TERP2O2_CH3O2                   ', &
                                      'TERP2O2_HO2                     ', 'TERP2O2_NO                      ', &
                                      'TERP2O2_XNO                     ', 'TERP2OOH_OH                     ', &
                                      'TERPNIT_OH                      ', 'TERPO2_CH3O2                    ', &
                                      'TERPO2_HO2                      ', 'TERPO2_NO                       ', &
                                      'TERPO2_XNO                      ', 'TERPOOH_OH                      ', &
                                      'TERPROD1_NO3                    ', 'TERPROD1_OH                     ', &
                                      'TERPROD1_XNO3                   ', 'TERPROD2_OH                     ', &
                                      'XNTERPO2_CH3O2                  ', 'XNTERPO2_HO2                    ', &
                                      'XNTERPO2_NO                     ', 'XNTERPO2_NO3                    ', &
                                      'XNTERPOOH_OH                    ', 'XTERPNIT_OH                     ', &
                                      'DMS_NO3                         ', 'DMS_OHa                         ', &
                                      'DMS_XNO3                        ', 'OCS_O                           ', &
                                      'OCS_OH                          ', 'S_O2                            ', &
                                      'S_O3                            ', 'SO_BRO                          ', &
                                      'SO_CLO                          ', 'S_OH                            ', &
                                      'SO_NO2                          ', 'SO_O2                           ', &
                                      'SO_O3                           ', 'SO_OCLO                         ', &
                                      'SO_OH                           ', 'SO_XNO2                         ', &
                                      'SO_XO3                          ', 'S_XO3                           ', &
                                      'usr_DMS_OH                      ', 'usr_SO2_OH                      ', &
                                      'usr_SO3_H2O                     ', 'NH3_OH                          ', &
                                      'usr_GLYOXAL_aer                 ', 'usr_HO2_aer                     ', &
                                      'usr_HONITR_aer                  ', 'usr_ISOPNITA_aer                ', &
                                      'usr_ISOPNITB_aer                ', 'usr_N2O5_aer                    ', &
                                      'usr_NC4CH2OH_aer                ', 'usr_NC4CHO_aer                  ', &
                                      'usr_NH4_strat_tau               ', 'usr_NO2_aer                     ', &
                                      'usr_NO3_aer                     ', 'usr_NTERPOOH_aer                ', &
                                      'usr_ONITR_aer                   ', 'usr_TERPNIT_aer                 ', &
                                      'usr_XHONITR_aer                 ', 'usr_XISOPNITA_aer               ', &
                                      'usr_XISOPNITB_aer               ', 'usr_XNC4CH2OH_aer               ', &
                                      'usr_XNC4CHO_aer                 ', 'usr_XNO2_aer                    ', &
                                      'usr_XNO2NO3_aer                 ', 'usr_XNO2XNO3_aer                ', &
                                      'usr_XNO3_aer                    ', 'usr_XNO3NO2_aer                 ', &
                                      'usr_XNTERPOOH_aer               ', 'usr_XONITR_aer                  ', &
                                      'usr_XTERPNIT_aer                ', 'BCARY_NO3_vbs                   ', &
                                      'BCARYO2_HO2_vbs                 ', 'BCARYO2_NO_vbs                  ', &
                                      'BCARY_O3_vbs                    ', 'BCARY_OH_vbs                    ', &
                                      'BENZENE_OH_vbs                  ', 'BENZO2_HO2_vbs                  ', &
                                      'BENZO2_NO_vbs                   ', 'ISOP_NO3_vbs                    ', &
                                      'ISOPO2_HO2_vbs                  ', 'ISOPO2_NO_vbs                   ', &
                                      'ISOP_O3_vbs                     ', 'ISOP_OH_vbs                     ', &
                                      'IVOCO2_HO2_vbs                  ', 'IVOCO2_NO_vbs                   ', &
                                      'IVOC_OH_vbs                     ', 'MTERP_NO3_vbs                   ', &
                                      'MTERPO2_HO2_vbs                 ', 'MTERPO2_NO_vbs                  ', &
                                      'MTERP_O3_vbs                    ', 'MTERP_OH_vbs                    ', &
                                      'SVOC_OH                         ', 'TOLUENE_OH_vbs                  ', &
                                      'TOLUO2_HO2_vbs                  ', 'TOLUO2_NO_vbs                   ', &
                                      'XYLENES_OH_vbs                  ', 'XYLEO2_HO2_vbs                  ', &
                                      'XYLEO2_NO_vbs                   ', 'het1                            ', &
                                      'het10                           ', 'het11                           ', &
                                      'het12                           ', 'het13                           ', &
                                      'het14                           ', 'het15                           ', &
                                      'het16                           ', 'het17                           ', &
                                      'het2                            ', 'het3                            ', &
                                      'het4                            ', 'het5                            ', &
                                      'het6                            ', 'het7                            ', &
                                      'het8                            ', 'het9                            ' /)
      rxt_tag_lst( 801: 822) = (/ 'xhet11                          ', 'xhet12a                         ', &
                                      'xhet12b                         ', 'xhet12c                         ', &
                                      'xhet13                          ', 'xhet14                          ', &
                                      'xhet15                          ', 'xhet1a                          ', &
                                      'xhet1b                          ', 'xhet1c                          ', &
                                      'xhet2                           ', 'xhet3                           ', &
                                      'xhet4                           ', 'xhet7a                          ', &
                                      'xhet7b                          ', 'xhet7c                          ', &
                                      'xhet8                           ', 'xhet9                           ', &
                                      'E90_tau                         ', 'NH_50_tau                       ', &
                                      'NH_5_tau                        ', 'ST80_25_tau                     ' /)
      rxt_tag_map(:rxt_tag_cnt) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, &
                                       11, 12, 13, 14, 15, 16, 17, 18, 19, 20, &
                                       21, 22, 23, 24, 25, 26, 27, 28, 29, 30, &
                                       31, 32, 33, 34, 35, 36, 37, 38, 39, 40, &
                                       41, 42, 43, 44, 45, 46, 47, 48, 49, 50, &
                                       51, 52, 53, 54, 55, 56, 57, 58, 59, 60, &
                                       61, 62, 63, 64, 65, 66, 67, 68, 69, 70, &
                                       71, 72, 73, 74, 75, 76, 77, 78, 79, 80, &
                                       81, 82, 83, 84, 85, 86, 87, 88, 89, 90, &
                                       91, 92, 93, 94, 95, 96, 97, 98, 99, 100, &
                                      101, 102, 103, 104, 105, 106, 107, 108, 109, 110, &
                                      111, 112, 113, 114, 115, 116, 117, 118, 119, 120, &
                                      121, 122, 123, 124, 125, 126, 127, 128, 129, 130, &
                                      131, 132, 133, 134, 135, 136, 137, 138, 139, 140, &
                                      141, 142, 143, 144, 145, 146, 147, 148, 149, 150, &
                                      151, 152, 153, 154, 155, 156, 157, 158, 159, 160, &
                                      161, 162, 163, 164, 165, 166, 167, 168, 169, 170, &
                                      171, 172, 173, 174, 175, 176, 177, 178, 179, 180, &
                                      181, 182, 183, 184, 185, 186, 187, 188, 189, 190, &
                                      191, 192, 193, 194, 195, 196, 197, 198, 199, 200, &
                                      201, 202, 203, 204, 205, 206, 207, 208, 209, 210, &
                                      211, 212, 213, 214, 215, 216, 217, 218, 219, 220, &
                                      221, 222, 223, 224, 225, 226, 227, 228, 229, 230, &
                                      231, 232, 233, 234, 235, 236, 237, 238, 239, 240, &
                                      241, 242, 243, 244, 245, 246, 247, 248, 249, 250, &
                                      251, 252, 253, 254, 255, 256, 257, 258, 259, 260, &
                                      261, 262, 263, 264, 265, 266, 267, 268, 269, 270, &
                                      271, 272, 273, 274, 275, 276, 277, 278, 279, 280, &
                                      281, 282, 283, 284, 285, 286, 287, 288, 289, 290, &
                                      291, 292, 293, 294, 295, 296, 297, 298, 299, 300, &
                                      301, 302, 303, 304, 305, 306, 307, 308, 309, 310, &
                                      311, 312, 313, 314, 315, 316, 317, 318, 319, 320, &
                                      321, 322, 323, 324, 325, 326, 327, 328, 329, 330, &
                                      331, 332, 333, 334, 335, 336, 337, 338, 339, 340, &
                                      341, 342, 343, 344, 345, 346, 347, 348, 349, 350, &
                                      351, 352, 353, 354, 355, 356, 357, 358, 359, 360, &
                                      361, 362, 363, 364, 365, 366, 367, 368, 369, 370, &
                                      371, 372, 373, 374, 375, 376, 377, 378, 379, 380, &
                                      381, 382, 383, 384, 385, 386, 387, 388, 389, 390, &
                                      391, 392, 393, 394, 395, 396, 397, 398, 399, 400, &
                                      401, 402, 403, 404, 405, 406, 407, 408, 409, 410, &
                                      411, 412, 413, 414, 415, 416, 417, 418, 419, 420, &
                                      421, 422, 423, 424, 425, 426, 427, 428, 429, 430, &
                                      431, 432, 433, 434, 435, 436, 437, 438, 439, 440, &
                                      441, 442, 443, 444, 445, 446, 447, 448, 449, 450, &
                                      451, 452, 453, 454, 455, 456, 457, 458, 459, 460, &
                                      461, 462, 463, 464, 465, 466, 467, 468, 469, 470, &
                                      471, 472, 473, 474, 475, 476, 477, 478, 479, 480, &
                                      481, 482, 483, 484, 485, 486, 487, 488, 489, 490, &
                                      491, 492, 493, 494, 495, 496, 497, 498, 499, 500, &
                                      501, 502, 503, 504, 505, 506, 507, 508, 509, 510, &
                                      511, 512, 513, 514, 515, 516, 517, 518, 519, 520, &
                                      521, 522, 523, 524, 525, 526, 527, 528, 529, 530, &
                                      531, 532, 533, 534, 535, 536, 537, 538, 539, 540, &
                                      541, 542, 543, 544, 545, 546, 547, 548, 549, 550, &
                                      551, 552, 553, 554, 555, 556, 557, 558, 559, 560, &
                                      561, 562, 563, 564, 565, 566, 567, 568, 569, 570, &
                                      571, 572, 573, 574, 575, 576, 577, 578, 579, 580, &
                                      581, 582, 583, 584, 585, 586, 587, 588, 589, 590, &
                                      591, 592, 593, 594, 595, 596, 597, 598, 599, 600, &
                                      601, 602, 603, 604, 605, 606, 607, 608, 609, 610, &
                                      611, 612, 613, 614, 615, 616, 617, 618, 619, 620, &
                                      621, 622, 623, 624, 625, 626, 627, 628, 629, 630, &
                                      631, 632, 633, 634, 635, 636, 637, 638, 639, 640, &
                                      641, 642, 643, 644, 645, 646, 647, 648, 649, 650, &
                                      651, 652, 653, 654, 655, 656, 657, 658, 659, 660, &
                                      661, 662, 663, 664, 665, 666, 667, 668, 669, 670, &
                                      671, 672, 673, 674, 675, 676, 677, 678, 679, 680, &
                                      681, 682, 683, 684, 685, 686, 687, 688, 689, 690, &
                                      691, 692, 693, 694, 695, 696, 697, 698, 699, 700, &
                                      701, 702, 703, 704, 705, 706, 707, 708, 709, 710, &
                                      711, 712, 713, 714, 715, 716, 717, 718, 719, 720, &
                                      721, 722, 723, 724, 725, 726, 727, 728, 729, 730, &
                                      731, 732, 733, 734, 735, 736, 737, 738, 739, 740, &
                                      741, 742, 743, 744, 745, 746, 747, 748, 749, 750, &
                                      751, 752, 753, 754, 755, 756, 757, 758, 759, 760, &
                                      761, 762, 763, 764, 765, 766, 767, 768, 769, 770, &
                                      771, 772, 773, 774, 775, 776, 777, 778, 779, 780, &
                                      781, 782, 783, 784, 785, 786, 787, 788, 789, 790, &
                                      791, 792, 793, 794, 795, 796, 797, 798, 799, 800, &
                                      801, 802, 803, 804, 805, 806, 807, 808, 809, 810, &
                                      811, 812, 813, 814, 815, 816, 817, 818, 819, 820, &
                                      821, 822 /)
      if( allocated( pht_alias_lst ) ) then
         deallocate( pht_alias_lst )
      end if
      allocate( pht_alias_lst(phtcnt,2),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate pht_alias_lst; error = ',ios
         call endrun
      end if
      if( allocated( pht_alias_mult ) ) then
         deallocate( pht_alias_mult )
      end if
      allocate( pht_alias_mult(phtcnt,2),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate pht_alias_mult; error = ',ios
         call endrun
      end if
      pht_alias_lst(:,1) = (/ '                ', '                ', '                ', '                ', &
                              'userdefined     ', 'userdefined     ', '                ', '                ', &
                              'jo3_a           ', 'jo3_b           ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              'userdefined     ', '                ', '                ', '                ', &
                              'jhno3           ', 'jho2no2_b       ', 'jho2no2_a       ', 'userdefined     ', &
                              'jno2            ', 'jn2o5_b         ', 'jn2o5_a         ', 'jn2o5_b         ', &
                              'jn2o5_a         ', 'jno3_a          ', 'jno3_b          ', 'jn2o5_b         ', &
                              'jn2o5_a         ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', 'jbrono2_a       ', 'jbrono2_b       ', 'jclono2_a       ', &
                              'jclono2_b       ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ' /)
      pht_alias_lst(:,2) = (/ '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              'jo3_a           ', 'jo3_b           ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              'jhno3           ', 'jho2no2_b       ', 'jho2no2_a       ', 'jno             ', &
                              'jno2            ', 'jn2o5_b         ', 'jn2o5_a         ', 'jn2o5_b         ', &
                              'jn2o5_a         ', 'jno3_a          ', 'jno3_b          ', 'jn2o5_b         ', &
                              'jn2o5_a         ', 'jch3ooh         ', 'jch3ooh         ', 'jch3ooh         ', &
                              'jno2            ', 'jno2            ', 'jno2            ', 'jno2            ', &
                              'jno2            ', 'jno2            ', 'jch3ooh         ', 'jch3ooh         ', &
                              'jch3ooh         ', 'jch3ooh         ', '                ', '                ', &
                              '                ', '                ', '                ', 'jh2o2           ', &
                              '                ', '                ', '                ', '                ', &
                              'jch3ooh         ', '                ', 'jmgly           ', 'jch2o_a         ', &
                              'jno2            ', '                ', 'jch3ooh         ', 'jch3ooh         ', &
                              '                ', '                ', 'jacet           ', 'jch3ooh         ', &
                              'jpan            ', '                ', 'jch2o_a         ', 'jch2o_a         ', &
                              'jch3ooh         ', 'jch3cho         ', '                ', 'jch3ooh         ', &
                              'jch3ooh         ', 'jch3ooh         ', 'jno2            ', 'jch3ooh         ', &
                              'jch3ooh         ', 'jch3ooh         ', 'jch3cho         ', 'jch3cho         ', &
                              'jch3ooh         ', 'jch3ooh         ', 'jch2o_a         ', 'jch3ooh         ', &
                              'jpan            ', 'jch2o_a         ', 'jch2o_a         ', 'jch3ooh         ', &
                              'jch3cho         ', 'jch3ooh         ', 'jpan            ', 'jch3ooh         ', &
                              'jch3ooh         ', 'jch3ooh         ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', '                ', '                ', '                ', &
                              '                ', 'jbrono2_a       ', 'jbrono2_b       ', 'jclono2_a       ', &
                              'jclono2_b       ', '                ', '                ', '                ', &
                              '                ', '                ', 'jno2            ', 'jno2            ', &
                              'jno2            ', 'jno2            ', 'jno2            ', 'jno2            ', &
                              'jno2            ', 'jno2            ', 'jno2            ', 'jno2            ' /)
      pht_alias_mult(:,1) = (/ 1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8 /)
      pht_alias_mult(:,2) = (/ 1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, .10_r8, 0.2_r8, .14_r8, .20_r8, &
                          .20_r8, .006_r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 0.28_r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          .006_r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, .10_r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, 1._r8, 1._r8, 1._r8, &
                          1._r8, 1._r8, .0004_r8, .0004_r8, .0004_r8, &
                          .0004_r8, .0004_r8, .0004_r8, .0004_r8, .0004_r8, &
                          .0004_r8, .0004_r8 /)
      allocate( cph_enthalpy(enthalpy_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate cph_enthalpy; error = ',ios
         call endrun
      end if
      allocate( cph_rid(enthalpy_cnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate cph_rid; error = ',ios
         call endrun
      end if
      cph_rid(:) = (/ 155, 159, 161, 162, 175, &
                                       178, 179, 180, 183, 187, &
                                       188, 189, 194, 196, 200, &
                                       201, 214, 215 /)
      cph_enthalpy(:) = (/ 189.810000_r8, 392.190000_r8, 493.580000_r8, 101.390000_r8, 232.590000_r8, &
                             203.400000_r8, 226.580000_r8, 120.100000_r8, 194.710000_r8, 293.620000_r8, &
                              67.670000_r8, 165.300000_r8, 165.510000_r8, 313.750000_r8, 133.750000_r8, &
                             193.020000_r8, 34.470000_r8, 199.170000_r8 /)
      allocate( num_rnts(rxntot-phtcnt),stat=ios )
      if( ios /= 0 ) then
         write(iulog,*) 'set_sim_dat: failed to allocate num_rnts; error = ',ios
         call endrun
      end if
      num_rnts(:) = (/ 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, &
                            3, 3, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 3, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 3, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            3, 2, 2, 3, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 3, 2, 3, 2, 2, 2, 2, &
                            2, 2, 3, 3, 3, 3, 3, 3, 3, 3, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 3, 2, 2, 2, &
                            2, 2, 2, 2, 3, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 3, 2, 2, 2, 2, 2, 2, 2, 3, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 3, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 3, 2, 2, 2, 2, 3, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            3, 3, 2, 2, 1, 2, 2, 2, 2, 2, &
                            2, 2, 2, 3, 2, 3, 2, 3, 2, 3, &
                            2, 3, 2, 3, 2, 3, 2, 3, 2, 3, &
                            2, 3, 2, 3, 2, 3, 2, 3, 2, 2, &
                            2, 2, 3, 3, 3, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            1, 2, 2, 2, 2, 3, 3, 3, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 3, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 3, 2, 2, 2, 3, 3, 2, 2, &
                            2, 3, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 1, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 3, 2, 3, 2, 2, &
                            3, 2, 3, 2, 2, 3, 2, 3, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 3, 3, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 1, 1, 1, 1, &
                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                            1, 1, 1, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
                            2, 1, 2, 1, 1, 1, 1, 2, 2, 2, &
                            1, 1, 2, 2, 2, 1, 1, 2, 1, 1, &
                            1, 1, 1, 1, 2, 1, 1, 1, 1, 1, &
                            2, 1, 1, 1, 1, 2, 1, 1, 1, 1 /)
      end subroutine set_sim_dat
      end module mo_sim_dat
module mo_imp_sol
  use shr_kind_mod, only : r8 => shr_kind_r8
  use chem_mods, only : clscnt4, gas_pcnst, clsmap, veclen
  use cam_logfile, only : iulog
  implicit none
  private
  public :: imp_slv_inti, imp_sol
  save
  real(r8), parameter :: rel_err = 1.e-3_r8
  real(r8), parameter :: high_rel_err = 1.e-4_r8
  !-----------------------------------------------------------------------
  ! Newton-Raphson iteration limits
  !-----------------------------------------------------------------------
  integer, parameter :: itermax = 11
  integer, parameter :: cut_limit = 5
  real(r8), parameter :: sol_min = 1.e-20_r8
  real(r8), parameter :: small = 1.e-40_r8
  real(r8) :: epsilon(clscnt4)
  logical :: factor(itermax)
contains
  subroutine imp_slv_inti
    !-----------------------------------------------------------------------
    ! ... Initialize the implict solver
    !-----------------------------------------------------------------------
    use mo_chem_utls, only : get_spc_ndx
    implicit none
    !-----------------------------------------------------------------------
    ! ... Local variables
    !-----------------------------------------------------------------------
    integer :: m, ox_ndx, o3a_ndx
    real(r8) :: eps(gas_pcnst)
    factor(:) = .true.
    eps(:) = rel_err
    ox_ndx = get_spc_ndx( 'OX' )
    if( ox_ndx < 1 ) then
       ox_ndx = get_spc_ndx( 'O3' )
    end if
    if( ox_ndx > 0 ) then
       eps(ox_ndx) = high_rel_err
    end if
    m = get_spc_ndx( 'NO' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'NO2' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'NO3' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'HNO3' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'HO2NO2' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'N2O5' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'OH' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'HO2' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    o3a_ndx = get_spc_ndx( 'O3A' )
    if( o3a_ndx > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'XNO' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'XNO2' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'XNO3' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'XHNO3' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'XHO2NO2' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'XNO2NO3' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    m = get_spc_ndx( 'NO2XNO3' )
    if( m > 0 ) then
       eps(m) = high_rel_err
    end if
    do m = 1,clscnt4
       epsilon(m) = eps(clsmap(m,4))
    end do
  end subroutine imp_slv_inti
  subroutine imp_sol( base_sol, reaction_rates, het_rates, extfrc, delt, &
                      ncol, nlev, lchnk, prod_out, loss_out )
    !-----------------------------------------------------------------------
    ! ... imp_sol advances the volumetric mixing ratio
    ! forward one time step via the fully implicit euler scheme.
    ! this source is meant for vector architectures such as the
    ! nec sx6 and cray x1
    !-----------------------------------------------------------------------
    use chem_mods, only : rxntot, extcnt, nzcnt, permute, cls_rxt_cnt
    use mo_tracname, only : solsym
    use mo_lin_matrix, only : linmat
    use mo_nln_matrix, only : nlnmat
    use mo_lu_factor, only : lu_fac
    use mo_lu_solve, only : lu_slv
    use mo_prod_loss, only : imp_prod_loss
    use mo_indprd, only : indprd
    use time_manager, only : get_nstep
    use perf_mod, only : t_startf, t_stopf
    implicit none
    !-----------------------------------------------------------------------
    ! ... dummy args
    !-----------------------------------------------------------------------
    integer, intent(in) :: ncol ! columns in chunck
    integer, intent(in) :: nlev
    integer, intent(in) :: lchnk ! chunk id
    real(r8), intent(in) :: delt ! time step (s)
    real(r8), intent(in) :: reaction_rates(ncol*nlev,max(1,rxntot)) ! rxt rates (1/cm^3/s)
    real(r8), intent(in) :: extfrc(ncol*nlev,max(1,extcnt)) ! external in-situ forcing (1/cm^3/s)
    real(r8), intent(in) :: het_rates(ncol*nlev,max(1,gas_pcnst)) ! washout rates (1/s)
    real(r8), intent(inout) :: base_sol(ncol*nlev,gas_pcnst) ! species mixing ratios (vmr)
    real(r8), intent(out) :: prod_out(ncol*nlev,max(1,clscnt4))
    real(r8), intent(out) :: loss_out(ncol*nlev,max(1,clscnt4))
    !-----------------------------------------------------------------------
    ! ... local variables
    !-----------------------------------------------------------------------
    integer :: nr_iter
    integer :: ofl
    integer :: ofu
    integer :: avec_len
    integer :: bndx ! base index
    integer :: cndx ! class index
    integer :: pndx ! permuted class index
    integer :: i,m
    integer :: fail_cnt(veclen)
    integer :: cut_cnt(veclen)
    integer :: stp_con_cnt(veclen)
    integer :: nstep
    real(r8) :: interval_done(veclen)
    real(r8) :: dt(veclen)
    real(r8) :: dti(veclen)
    real(r8) :: max_delta(max(1,clscnt4))
    real(r8) :: ind_prd(ncol*nlev,max(1,clscnt4))
    logical :: convergence
    integer :: chnkpnts ! total spatial points in chunk; ncol*ncol
    logical :: diags_out(ncol*nlev,max(1,clscnt4))
    real(r8) :: sys_jac_blk(veclen,max(1,nzcnt))
    real(r8) :: lin_jac_blk(veclen,max(1,nzcnt))
    real(r8) :: solution_blk(veclen,max(1,clscnt4))
    real(r8) :: forcing_blk(veclen,max(1,clscnt4))
    real(r8) :: iter_invariant_blk(veclen,max(1,clscnt4))
    real(r8) :: prod_blk(veclen,max(1,clscnt4))
    real(r8) :: loss_blk(veclen,max(1,clscnt4))
    real(r8) :: ind_prd_blk(veclen,max(1,clscnt4))
    real(r8) :: sbase_sol_blk(veclen,gas_pcnst)
    real(r8) :: wrk_blk(veclen)
    logical :: spc_conv_blk(veclen,max(1,clscnt4))
    logical :: cls_conv_blk(veclen)
    logical :: time_stp_done_blk(veclen)
    real(r8) :: reaction_rates_blk(veclen,max(1,rxntot))
    real(r8) :: extfrc_blk(veclen,max(1,extcnt))
    real(r8) :: het_rates_blk(veclen,max(1,gas_pcnst))
    real(r8) :: base_sol_blk(veclen,gas_pcnst)
    chnkpnts = ncol*nlev
    prod_out = 0._r8
    loss_out = 0._r8
    diags_out = .false.
    !-----------------------------------------------------------------------
    ! ... class independent forcing
    !-----------------------------------------------------------------------
    if( cls_rxt_cnt(1,4) > 0 .or. extcnt > 0 ) then
       call indprd( 4, ind_prd, clscnt4, base_sol, extfrc, &
            reaction_rates, chnkpnts )
    else
       do m = 1,clscnt4
          ind_prd(:,m) = 0._r8
       end do
    end if
    nstep = get_nstep()
    ofl = 1
    chnkpnts_loop : do
       ofu = min( chnkpnts,ofl + veclen - 1 )
       avec_len = (ofu - ofl) + 1
       reaction_rates_blk(1:avec_len,:) = reaction_rates(ofl:ofu,:)
       extfrc_blk(1:avec_len,:) = extfrc(ofl:ofu,:)
       het_rates_blk(1:avec_len,:) = het_rates(ofl:ofu,:)
       ind_prd_blk(1:avec_len,:) = ind_prd(ofl:ofu,:)
       base_sol_blk(1:avec_len,:) = base_sol(ofl:ofu,:)
       cls_conv_blk(1:avec_len) = .false.
       dt(1:avec_len) = delt
       cut_cnt(1:avec_len) = 0
       fail_cnt(1:avec_len) = 0
       stp_con_cnt(1:avec_len) = 0
       interval_done(1:avec_len) = 0._r8
       time_stp_done_blk(1:avec_len) = .false.
       !-----------------------------------------------------------------------
       ! ... time step loop
       !-----------------------------------------------------------------------
       time_step_loop : do
          dti(1:avec_len) = 1._r8 / dt(1:avec_len)
          !-----------------------------------------------------------------------
          ! ... transfer from base to class array
          !-----------------------------------------------------------------------
          do cndx = 1,clscnt4
             bndx = clsmap(cndx,4)
             pndx = permute(cndx,4)
             do i = 1, avec_len
                solution_blk(i,pndx) = base_sol_blk(i,bndx)
             end do
          end do
          do m = 1,gas_pcnst
            sbase_sol_blk(1:avec_len,m) = base_sol_blk(1:avec_len,m)
          end do
          !-----------------------------------------------------------------------
          ! ... set the iteration invariant part of the function f(y)
          !-----------------------------------------------------------------------
          if( cls_rxt_cnt(1,4) > 0 .or. extcnt > 0 ) then
             do m = 1,clscnt4
                do i = 1, avec_len
                   iter_invariant_blk(i,m) = dti(i) * solution_blk(i,m) + ind_prd_blk(i,m)
                end do
             end do
          else
             do m = 1,clscnt4
                do i = 1, avec_len
                    iter_invariant_blk(i,m) = dti(i) * solution_blk(i,m)
                end do
             end do
          end if
          !-----------------------------------------------------------------------
          ! ... the linear component
          !-----------------------------------------------------------------------
          if( cls_rxt_cnt(2,4) > 0 ) then
             call t_startf( 'lin_mat' )
             call linmat( avec_len, lin_jac_blk, base_sol_blk, &
                  reaction_rates_blk, het_rates_blk )
             call t_stopf( 'lin_mat' )
          end if
          !=======================================================================
          ! the newton-raphson iteration for f(y) = 0
          !=======================================================================
          iter_loop : do nr_iter = 1,itermax
             !-----------------------------------------------------------------------
             ! ... the non-linear component
             !-----------------------------------------------------------------------
             if( factor(nr_iter) ) then
                call t_startf( 'nln_mat' )
                call nlnmat( avec_len, sys_jac_blk, base_sol_blk, &
                     reaction_rates_blk, lin_jac_blk, dti )
                call t_stopf( 'nln_mat' )
                !-----------------------------------------------------------------------
                ! ... factor the "system" matrix
                !-----------------------------------------------------------------------
                call t_startf( 'lu_fac' )
                call lu_fac( avec_len, sys_jac_blk )
                call t_stopf( 'lu_fac' )
             end if
             !-----------------------------------------------------------------------
             ! ... form f(y)
             !-----------------------------------------------------------------------
             call t_startf( 'prod_loss' )
             call imp_prod_loss( avec_len, prod_blk, loss_blk, &
                  base_sol_blk, reaction_rates_blk, het_rates_blk )
             call t_stopf( 'prod_loss' )
             do m = 1,clscnt4
                do i = 1, avec_len
                   forcing_blk(i,m) = solution_blk(i,m)*dti(i) &
                                    - (iter_invariant_blk(i,m) + prod_blk(i,m) - loss_blk(i,m))
                end do
             end do
             !-----------------------------------------------------------------------
             ! ... solve for the mixing ratio at t(n+1)
             !-----------------------------------------------------------------------
             call t_startf( 'lu_slv' )
             call lu_slv( avec_len, sys_jac_blk, forcing_blk )
             call t_stopf( 'lu_slv' )
             do m = 1,clscnt4
                do i = 1, avec_len
                   if( .not. cls_conv_blk(i) )then
                      solution_blk(i,m) = solution_blk(i,m) + forcing_blk(i,m)
                   else
                      forcing_blk(i,m) = 0._r8
                   endif
                end do
             end do
             !-----------------------------------------------------------------------
             ! ... convergence measures and test
             !-----------------------------------------------------------------------
             conv_chk : if( nr_iter > 1 ) then
                !-----------------------------------------------------------------------
                ! ... check for convergence
                !-----------------------------------------------------------------------
                do cndx = 1,clscnt4
                   pndx = permute(cndx,4)
                   bndx = clsmap(cndx,4)
                   do i = 1, avec_len
                     if ( abs( solution_blk(i,pndx) ) > sol_min ) then
                        wrk_blk(i) = abs( forcing_blk(i,pndx)/solution_blk(i,pndx) )
                     else
                      wrk_blk(i) = 0._r8
                     endif
                   enddo
                   max_delta(cndx) = maxval( wrk_blk(1:avec_len) )
                   do i = 1, avec_len
                     solution_blk(i,pndx) = max( 0._r8,solution_blk(i,pndx) )
                     base_sol_blk(i,bndx) = solution_blk(i,pndx)
                     if ( abs( forcing_blk(i,pndx) ) > small ) then
                       spc_conv_blk(i,cndx) = abs(forcing_blk(i,pndx)) <= epsilon(cndx)*abs(solution_blk(i,pndx))
                     else
                       spc_conv_blk(i,cndx) = .true.
                     endif
                   enddo
                   where( spc_conv_blk(1:avec_len,cndx) .and. .not.diags_out(ofl:ofu,cndx) )
                      ! capture output production and loss diagnostics at converged ponits
                      prod_out(ofl:ofu,cndx) = prod_blk(1:avec_len,cndx) + ind_prd_blk(1:avec_len,cndx)
                      loss_out(ofl:ofu,cndx) = loss_blk(1:avec_len,cndx)
                      diags_out(ofl:ofu,cndx) = .true.
                   endwhere
                end do
                do i = 1, avec_len
                  if( .not. cls_conv_blk(i) ) then
                    cls_conv_blk(i) = all( spc_conv_blk(i,:) )
                  end if
                end do
                convergence = all( cls_conv_blk(:) )
                if( convergence ) then
                   exit iter_loop
                end if
             else conv_chk
!-----------------------------------------------------------------------
! ... limit iterate
!-----------------------------------------------------------------------
                do m = 1,clscnt4
                  do i = 1, avec_len
                    solution_blk(i,m) = max( 0._r8,solution_blk(i,m) )
                  end do
                end do
!-----------------------------------------------------------------------
! ... transfer latest solution back to base array
!-----------------------------------------------------------------------
                do cndx = 1,clscnt4
                   pndx = permute(cndx,4)
                   bndx = clsmap(cndx,4)
                   do i = 1, avec_len
                     base_sol_blk(i,bndx) = solution_blk(i,pndx)
                   end do
                end do
             end if conv_chk
          end do iter_loop
          !-----------------------------------------------------------------------
          ! ... check for newton-raphson convergence
          !-----------------------------------------------------------------------
          do i = 1,avec_len
            if( .not. cls_conv_blk(i) ) then
              fail_cnt(i) = fail_cnt(i) + 1
              write(iulog,'('' imp_sol: time step '',1p,g15.7,'' failed to converge @ (lchnk,vctrpos,nstep) = '',3i8)') &
                    dt(i),lchnk,ofl+i-1,nstep
              stp_con_cnt(i) = 0
              if( cut_cnt(i) < cut_limit ) then
                cut_cnt(i) = cut_cnt(i) + 1
                if( cut_cnt(i) < cut_limit ) then
                  dt(i) = .5_r8 * dt(i)
                else
                  dt(i) = .1_r8 * dt(i)
                end if
                base_sol_blk(i,:) = sbase_sol_blk(i,:)
              else
                write(iulog,'('' imp_sol: step failed to converge @ (lchnk,vctrpos,nstep,dt,time) = '',3i8,1p,2g15.7)') &
                      lchnk,ofl+i-1,nstep,dt(i),interval_done+dt(i)
                do m = 1,clscnt4
                   if( .not. spc_conv_blk(i,m) ) then
                      write(iulog,'(1x,a16,1x,1pe10.3)') solsym(clsmap(m,4)), max_delta(m)
                   end if
                end do
                cls_conv_blk(i) = .true.
                if( .not. time_stp_done_blk(i) ) then
                  interval_done(i) = interval_done(i) + dt(i)
                  time_stp_done_blk(i) = abs( delt - interval_done(i) ) <= .0001_r8
                endif
              end if
            elseif( .not. time_stp_done_blk(i) ) then
               interval_done(i) = interval_done(i) + dt(i)
               time_stp_done_blk(i) = abs( delt - interval_done(i) ) <= .0001_r8
               stp_con_cnt(i) = stp_con_cnt(i) + 1
               if( .not. time_stp_done_blk(i) ) then
                 if( stp_con_cnt(i) >= 2 ) then
                   dt(i) = 2._r8*dt(i)
                   stp_con_cnt(i) = 0
                 end if
                 dt(i) = min( dt(i),delt-interval_done(i) )
               else
                 base_sol(ofl+i-1,1:gas_pcnst) = base_sol_blk(i,1:gas_pcnst)
               endif
            endif
          end do
          convergence = all( cls_conv_blk(:) )
          do i = 1,avec_len
            if( cls_conv_blk(i) .and. .not. time_stp_done_blk(i) ) then
              cls_conv_blk(i) = .false.
            endif
          end do
          if( .not. convergence ) then
            cycle time_step_loop
          endif
          !-----------------------------------------------------------------------
          ! ... check for time step done
          !-----------------------------------------------------------------------
          if( all( time_stp_done_blk(1:avec_len) ) ) then
             exit time_step_loop
          end if
       end do time_step_loop
       ofl = ofu + 1
       if( ofl > chnkpnts ) then
          exit chnkpnts_loop
       end if
    end do chnkpnts_loop
  end subroutine imp_sol
end module mo_imp_sol
module mo_exp_sol
  private
  public :: exp_sol
  public :: exp_sol_inti
contains
  subroutine exp_sol_inti
    use mo_tracname, only : solsym
    use chem_mods, only : clscnt1, clsmap
    use cam_history, only : addfld
    implicit none
    integer :: i,j
    do i = 1,clscnt1
       j = clsmap(i,1)
       call addfld( trim(solsym(j))//'_CHMP', (/ 'lev' /), 'I', '/cm3/s', 'chemical production rate' )
       call addfld( trim(solsym(j))//'_CHML', (/ 'lev' /), 'I', '/cm3/s', 'chemical loss rate' )
    enddo
  end subroutine exp_sol_inti
  subroutine exp_sol( base_sol, reaction_rates, het_rates, extfrc, delt, xhnm, ncol, lchnk, ltrop )
    !-----------------------------------------------------------------------
    ! ... Exp_sol advances the volumetric mixing ratio
    ! forward one time step via the fully explicit
    ! Euler scheme
    !-----------------------------------------------------------------------
    use chem_mods, only : clscnt1, extcnt, gas_pcnst, clsmap, rxntot
    use ppgrid, only : pcols, pver
    use mo_prod_loss, only : exp_prod_loss
    use mo_indprd, only : indprd
    use shr_kind_mod, only : r8 => shr_kind_r8
    use cam_history, only : outfld
    use mo_tracname, only : solsym
    implicit none
    !-----------------------------------------------------------------------
    ! ... Dummy arguments
    !-----------------------------------------------------------------------
    integer, intent(in) :: ncol ! columns in chunck
    integer, intent(in) :: lchnk ! chunk id
    real(r8), intent(in) :: delt ! time step (s)
    real(r8), intent(in) :: het_rates(ncol,pver,max(1,gas_pcnst)) ! het rates (1/cm^3/s)
    real(r8), intent(in) :: reaction_rates(ncol,pver,rxntot) ! rxt rates (1/cm^3/s)
    real(r8), intent(in) :: extfrc(ncol,pver,extcnt) ! "external insitu forcing" (1/cm^3/s)
    real(r8), intent(in) :: xhnm(ncol,pver)
    integer, intent(in) :: ltrop(pcols) ! chemistry troposphere boundary (index)
    real(r8), intent(inout) :: base_sol(ncol,pver,gas_pcnst) ! working mixing ratios (vmr)
    !-----------------------------------------------------------------------
    ! ... Local variables
    !-----------------------------------------------------------------------
    integer :: i, k, l, m
    integer :: chnkpnts
    real(r8), dimension(ncol,pver,max(1,clscnt1)) :: &
         prod, &
         loss
    real(r8), dimension(ncol,pver,clscnt1) :: ind_prd
    real(r8), dimension(ncol,pver) :: wrk
    chnkpnts = ncol*pver
    !-----------------------------------------------------------------------
    ! ... Put "independent" production in the forcing
    !-----------------------------------------------------------------------
    call indprd( 1, ind_prd, clscnt1, base_sol, extfrc, &
                 reaction_rates, chnkpnts )
    !-----------------------------------------------------------------------
    ! ... Form F(y)
    !-----------------------------------------------------------------------
    call exp_prod_loss( 1, chnkpnts, prod, loss, base_sol, reaction_rates, &
                        het_rates, chnkpnts )
    !-----------------------------------------------------------------------
    ! ... Solve for the mixing ratio at t(n+1)
    !-----------------------------------------------------------------------
    do m = 1,clscnt1
       l = clsmap(m,1)
       do i = 1,ncol
          do k = ltrop(i)+1,pver
             base_sol(i,k,l) = base_sol(i,k,l) + delt * (prod(i,k,m) + ind_prd(i,k,m) - loss(i,k,m))
          end do
       end do
       wrk(:,:) = (prod(:,:,m) + ind_prd(:,:,m))*xhnm
       call outfld( trim(solsym(l))//'_CHMP', wrk(:,:), ncol, lchnk )
       wrk(:,:) = (loss(:,:,m))*xhnm
       call outfld( trim(solsym(l))//'_CHML', wrk(:,:), ncol, lchnk )
    end do
  end subroutine exp_sol
end module mo_exp_sol
