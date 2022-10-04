
      module mo_lin_matrix

      use chem_mods, only: veclen
      private
      public :: linmat

      contains

      subroutine linmat01( avec_len, mat, y, rxt, het_rates )
!----------------------------------------------
!       ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer,  intent(in)    ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(in)    ::  het_rates(veclen,gas_pcnst)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)
!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k


      do k = 1,avec_len

         mat(k,814) = -( het_rates(k,1) )
         mat(k,146) = rxt(k,657)

         mat(k,797) = -( rxt(k,34) + het_rates(k,2) )

         mat(k,1157) = -( het_rates(k,3) )

         mat(k,786) = -( rxt(k,35) + het_rates(k,4) )

         mat(k,1) = -( het_rates(k,5) )

         mat(k,2) = -( het_rates(k,6) )

         mat(k,3) = -( het_rates(k,7) )

         mat(k,1199) = -( het_rates(k,8) )

         mat(k,60) = -( het_rates(k,9) )

         mat(k,195) = -( het_rates(k,10) )

         mat(k,698) = -( het_rates(k,11) )

         mat(k,66) = -( het_rates(k,12) )

         mat(k,510) = -( rxt(k,36) + het_rates(k,13) )

         mat(k,202) = -( rxt(k,37) + het_rates(k,14) )

         mat(k,504) = -( rxt(k,38) + het_rates(k,15) )

         mat(k,534) = -( rxt(k,39) + het_rates(k,16) )
         mat(k,511) = .500_r8*rxt(k,36)
         mat(k,203) = rxt(k,37)
         mat(k,773) = .200_r8*rxt(k,85)
         mat(k,844) = .060_r8*rxt(k,97)

         mat(k,360) = -( rxt(k,40) + het_rates(k,17) )
         mat(k,772) = .200_r8*rxt(k,85)
         mat(k,842) = .200_r8*rxt(k,97)

         mat(k,707) = -( rxt(k,41) + het_rates(k,18) )
         mat(k,252) = rxt(k,61)
         mat(k,1339) = rxt(k,71)
         mat(k,774) = .200_r8*rxt(k,85)
         mat(k,845) = .150_r8*rxt(k,97)

         mat(k,422) = -( rxt(k,42) + het_rates(k,19) )
         mat(k,843) = .210_r8*rxt(k,97)

         mat(k,265) = -( het_rates(k,20) )

         mat(k,556) = -( het_rates(k,21) )

         mat(k,1757) = -( het_rates(k,22) )
         mat(k,257) = rxt(k,99)
         mat(k,2490) = rxt(k,100)
         mat(k,652) = rxt(k,102)
         mat(k,171) = rxt(k,104)
         mat(k,178) = rxt(k,105)
         mat(k,584) = 2.000_r8*rxt(k,111)
         mat(k,714) = rxt(k,112)
         mat(k,540) = 3.000_r8*rxt(k,115)
         mat(k,137) = 2.000_r8*rxt(k,123)
         mat(k,983) = rxt(k,124)
         mat(k,880) = rxt(k,130)

         mat(k,256) = -( rxt(k,99) + het_rates(k,23) )

         mat(k,2501) = -( rxt(k,100) + het_rates(k,24) )
         mat(k,655) = rxt(k,101)

         mat(k,650) = -( rxt(k,101) + rxt(k,102) + rxt(k,786) + rxt(k,789) + rxt(k,794) &
                 + het_rates(k,25) )

         mat(k,4) = -( het_rates(k,26) )

         mat(k,259) = -( het_rates(k,27) )
         mat(k,393) = rxt(k,43)

         mat(k,660) = -( het_rates(k,28) )

         mat(k,394) = -( rxt(k,43) + het_rates(k,29) )

         mat(k,348) = -( het_rates(k,30) )

         mat(k,608) = -( het_rates(k,31) )

         mat(k,1032) = -( het_rates(k,32) )
         mat(k,573) = rxt(k,67)

         mat(k,298) = -( het_rates(k,33) )

         mat(k,417) = -( rxt(k,44) + het_rates(k,34) )

         mat(k,354) = -( het_rates(k,35) )

         mat(k,1291) = -( het_rates(k,36) )
         mat(k,1666) = .700_r8*rxt(k,70)

         mat(k,991) = -( het_rates(k,37) )

         mat(k,498) = -( rxt(k,45) + het_rates(k,38) )

         mat(k,117) = -( het_rates(k,39) )

         mat(k,869) = -( het_rates(k,40) )

         mat(k,307) = -( rxt(k,46) + het_rates(k,41) )

         mat(k,121) = -( rxt(k,103) + het_rates(k,42) )

         mat(k,168) = -( rxt(k,104) + het_rates(k,43) )

         mat(k,174) = -( rxt(k,105) + het_rates(k,44) )

         mat(k,148) = -( rxt(k,106) + het_rates(k,45) )

         mat(k,180) = -( rxt(k,107) + het_rates(k,46) )

         mat(k,153) = -( rxt(k,108) + het_rates(k,47) )

         mat(k,186) = -( rxt(k,109) + het_rates(k,48) )

         mat(k,158) = -( rxt(k,110) + het_rates(k,49) )

         mat(k,583) = -( rxt(k,111) + het_rates(k,50) )

         mat(k,2222) = -( rxt(k,47) + rxt(k,48) + het_rates(k,51) )
         mat(k,804) = .100_r8*rxt(k,34)
         mat(k,794) = .100_r8*rxt(k,35)
         mat(k,525) = rxt(k,53)
         mat(k,1780) = .180_r8*rxt(k,55)
         mat(k,1372) = rxt(k,58)
         mat(k,1437) = .330_r8*rxt(k,60)
         mat(k,1420) = rxt(k,62)
         mat(k,828) = .690_r8*rxt(k,64)
         mat(k,1529) = 1.340_r8*rxt(k,65)
         mat(k,1006) = rxt(k,72)
         mat(k,638) = rxt(k,77)
         mat(k,495) = rxt(k,78)
         mat(k,770) = .375_r8*rxt(k,80)
         mat(k,550) = .400_r8*rxt(k,82)
         mat(k,1335) = .680_r8*rxt(k,84)
         mat(k,647) = rxt(k,397)
         mat(k,331) = 2.000_r8*rxt(k,463)

         mat(k,713) = -( rxt(k,112) + het_rates(k,52) )

         mat(k,125) = -( rxt(k,113) + het_rates(k,53) )

         mat(k,1358) = -( rxt(k,49) + het_rates(k,54) )
         mat(k,801) = .400_r8*rxt(k,34)
         mat(k,791) = .400_r8*rxt(k,35)
         mat(k,419) = rxt(k,44)
         mat(k,1424) = .330_r8*rxt(k,60)
         mat(k,390) = rxt(k,68)
         mat(k,635) = rxt(k,77)

         mat(k,449) = -( rxt(k,114) + het_rates(k,55) )

         mat(k,109) = -( het_rates(k,56) )

         mat(k,1739) = -( het_rates(k,57) )
         mat(k,508) = .130_r8*rxt(k,38)
         mat(k,425) = rxt(k,42)
         mat(k,1169) = rxt(k,50)
         mat(k,1539) = rxt(k,51)
         mat(k,1431) = .330_r8*rxt(k,60)
         mat(k,1418) = rxt(k,62)
         mat(k,1526) = 1.340_r8*rxt(k,65)
         mat(k,574) = rxt(k,67)
         mat(k,391) = rxt(k,68)
         mat(k,1674) = .300_r8*rxt(k,70)
         mat(k,1005) = rxt(k,72)
         mat(k,618) = .600_r8*rxt(k,75) + rxt(k,471)
         mat(k,494) = rxt(k,78)
         mat(k,226) = .500_r8*rxt(k,79)
         mat(k,1332) = .650_r8*rxt(k,84)

         mat(k,1167) = -( rxt(k,50) + het_rates(k,58) )
         mat(k,800) = .250_r8*rxt(k,34)
         mat(k,790) = .250_r8*rxt(k,35)
         mat(k,500) = .820_r8*rxt(k,45)
         mat(k,1423) = .170_r8*rxt(k,60)
         mat(k,764) = .300_r8*rxt(k,80)
         mat(k,547) = .050_r8*rxt(k,82)
         mat(k,1328) = .500_r8*rxt(k,84)

         mat(k,1538) = -( rxt(k,51) + het_rates(k,59) )
         mat(k,507) = .180_r8*rxt(k,38)
         mat(k,424) = rxt(k,42)
         mat(k,782) = .400_r8*rxt(k,85)
         mat(k,853) = .540_r8*rxt(k,97)
         mat(k,531) = .510_r8*rxt(k,98)

         mat(k,807) = -( het_rates(k,60) )

         mat(k,722) = -( rxt(k,52) + het_rates(k,61) )

         mat(k,1869) = -( het_rates(k,62) )
         mat(k,1362) = rxt(k,49)
         mat(k,1170) = rxt(k,50)
         mat(k,725) = rxt(k,52)
         mat(k,1777) = rxt(k,54)
         mat(k,1678) = .300_r8*rxt(k,70)
         mat(k,619) = .400_r8*rxt(k,75)
         mat(k,715) = rxt(k,112)
         mat(k,451) = rxt(k,114)

         mat(k,912) = -( het_rates(k,63) )

         mat(k,522) = -( rxt(k,53) + het_rates(k,64) )

         mat(k,1774) = -( rxt(k,54) + rxt(k,55) + het_rates(k,65) )

         mat(k,538) = -( rxt(k,115) + het_rates(k,66) )

         mat(k,2269) = -( het_rates(k,67) )
         mat(k,258) = rxt(k,99)
         mat(k,123) = 4.000_r8*rxt(k,103)
         mat(k,172) = rxt(k,104)
         mat(k,151) = 2.000_r8*rxt(k,106)
         mat(k,184) = 2.000_r8*rxt(k,107)
         mat(k,156) = 2.000_r8*rxt(k,108)
         mat(k,190) = rxt(k,109)
         mat(k,161) = 2.000_r8*rxt(k,110)
         mat(k,126) = 3.000_r8*rxt(k,113)
         mat(k,453) = rxt(k,114)
         mat(k,193) = 2.000_r8*rxt(k,116)
         mat(k,108) = 2.000_r8*rxt(k,117)
         mat(k,2194) = rxt(k,118)
         mat(k,1023) = rxt(k,119)
         mat(k,381) = rxt(k,122)
         mat(k,375) = rxt(k,125)
         mat(k,335) = rxt(k,126)
         mat(k,403) = rxt(k,127)
         mat(k,2692) = rxt(k,128)
         mat(k,972) = rxt(k,131)

         mat(k,192) = -( rxt(k,116) + het_rates(k,68) )

         mat(k,106) = -( rxt(k,117) + rxt(k,302) + het_rates(k,69) )

         mat(k,2192) = -( rxt(k,118) + het_rates(k,70) )
         mat(k,1022) = rxt(k,120)
         mat(k,410) = rxt(k,132)
         mat(k,107) = 2.000_r8*rxt(k,302)

         mat(k,1019) = -( rxt(k,119) + rxt(k,120) + rxt(k,788) + rxt(k,793) + rxt(k,799) &
                 + het_rates(k,71) )

         mat(k,5) = -( het_rates(k,72) )

         mat(k,1443) = -( het_rates(k,73) )
         mat(k,204) = 1.500_r8*rxt(k,37)
         mat(k,506) = .450_r8*rxt(k,38)
         mat(k,709) = .600_r8*rxt(k,41)
         mat(k,423) = rxt(k,42)
         mat(k,2211) = rxt(k,47) + rxt(k,48)
         mat(k,1359) = rxt(k,49)
         mat(k,1537) = rxt(k,51)
         mat(k,1772) = .380_r8*rxt(k,55)
         mat(k,978) = rxt(k,56)
         mat(k,1370) = rxt(k,58)
         mat(k,1235) = 2.000_r8*rxt(k,59)
         mat(k,1429) = .330_r8*rxt(k,60)
         mat(k,1521) = 1.340_r8*rxt(k,66)
         mat(k,1669) = .700_r8*rxt(k,70)
         mat(k,225) = 1.500_r8*rxt(k,79)
         mat(k,768) = .250_r8*rxt(k,80)
         mat(k,1228) = rxt(k,83)
         mat(k,1331) = 1.700_r8*rxt(k,84)
         mat(k,444) = rxt(k,139)

      end do

      end subroutine linmat01

      subroutine linmat02( avec_len, mat, y, rxt, het_rates )
!----------------------------------------------
!       ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer,  intent(in)    ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(in)    ::  het_rates(veclen,gas_pcnst)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)
!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k


      do k = 1,avec_len

         mat(k,6) = -( het_rates(k,261) )

         mat(k,7) = -( het_rates(k,262) )

         mat(k,8) = -( het_rates(k,263) )

         mat(k,9) = -( het_rates(k,264) )

         mat(k,10) = -( het_rates(k,265) )

         mat(k,11) = -( het_rates(k,266) )

         mat(k,12) = -( het_rates(k,267) )

         mat(k,13) = -( het_rates(k,268) )

         mat(k,14) = -( het_rates(k,269) )

         mat(k,15) = -( het_rates(k,270) )

         mat(k,16) = -( het_rates(k,271) )

         mat(k,17) = -( het_rates(k,272) )

         mat(k,18) = -( het_rates(k,273) )

         mat(k,977) = -( rxt(k,56) + het_rates(k,74) )
         mat(k,723) = rxt(k,52)
         mat(k,1770) = .440_r8*rxt(k,55)
         mat(k,617) = .400_r8*rxt(k,75)
         mat(k,763) = rxt(k,80)
         mat(k,1327) = .800_r8*rxt(k,84)

         mat(k,338) = -( rxt(k,121) + het_rates(k,75) )
         mat(k,169) = rxt(k,104)
         mat(k,175) = rxt(k,105)
         mat(k,181) = rxt(k,107)
         mat(k,154) = 2.000_r8*rxt(k,108)
         mat(k,187) = 2.000_r8*rxt(k,109)
         mat(k,159) = rxt(k,110)
         mat(k,135) = 2.000_r8*rxt(k,123)
         mat(k,333) = rxt(k,126)
         mat(k,399) = rxt(k,127)

         mat(k,378) = -( rxt(k,122) + het_rates(k,76) )
         mat(k,149) = rxt(k,106)
         mat(k,182) = rxt(k,107)
         mat(k,373) = rxt(k,125)

         mat(k,219) = -( het_rates(k,77) )

         mat(k,935) = -( het_rates(k,78) )
         mat(k,361) = .600_r8*rxt(k,40)

         mat(k,457) = -( het_rates(k,79) )

         mat(k,19) = -( het_rates(k,80) )

         mat(k,20) = -( het_rates(k,81) )

         mat(k,21) = -( het_rates(k,82) )

         mat(k,22) = -( rxt(k,819) + het_rates(k,83) )

         mat(k,831) = -( het_rates(k,84) )

         mat(k,328) = -( rxt(k,463) + rxt(k,464) + het_rates(k,85) )
         mat(k,132) = rxt(k,57)

         mat(k,959) = -( het_rates(k,86) )

         mat(k,131) = -( rxt(k,57) + het_rates(k,87) )

         mat(k,1121) = -( het_rates(k,88) )
         mat(k,177) = rxt(k,105)
         mat(k,189) = rxt(k,109)
         mat(k,340) = 2.000_r8*rxt(k,121)
         mat(k,380) = rxt(k,122)
         mat(k,364) = rxt(k,129)

         mat(k,1369) = -( rxt(k,58) + het_rates(k,89) )
         mat(k,1425) = .330_r8*rxt(k,60)
         mat(k,766) = .250_r8*rxt(k,80)
         mat(k,329) = rxt(k,464)

         mat(k,1234) = -( rxt(k,59) + rxt(k,729) + het_rates(k,90) )
         mat(k,513) = rxt(k,36)
         mat(k,505) = .130_r8*rxt(k,38)
         mat(k,414) = .700_r8*rxt(k,76)
         mat(k,780) = .600_r8*rxt(k,85)
         mat(k,851) = .340_r8*rxt(k,97)
         mat(k,530) = .170_r8*rxt(k,98)

         mat(k,1791) = -( rxt(k,178) + het_rates(k,91) )
         mat(k,2751) = 2.000_r8*rxt(k,2) + rxt(k,3)
         mat(k,2215) = 2.000_r8*rxt(k,48)
         mat(k,523) = rxt(k,53)
         mat(k,1775) = rxt(k,54) + .330_r8*rxt(k,55)
         mat(k,984) = rxt(k,124)
         mat(k,2684) = rxt(k,128)
         mat(k,365) = rxt(k,129)

         mat(k,1551) = -( het_rates(k,92) )
         mat(k,2748) = rxt(k,1)
         mat(k,2212) = rxt(k,47)
         mat(k,1773) = 1.440_r8*rxt(k,55)

         mat(k,134) = -( rxt(k,123) + het_rates(k,93) )

         mat(k,730) = -( rxt(k,4) + het_rates(k,94) )
         mat(k,2032) = .500_r8*rxt(k,730)

         mat(k,139) = -( rxt(k,138) + het_rates(k,95) )

         mat(k,982) = -( rxt(k,124) + het_rates(k,96) )

         mat(k,372) = -( rxt(k,125) + het_rates(k,97) )

         mat(k,332) = -( rxt(k,126) + het_rates(k,98) )

         mat(k,400) = -( rxt(k,127) + het_rates(k,99) )

         mat(k,2698) = -( rxt(k,128) + het_rates(k,100) )

         mat(k,246) = -( het_rates(k,101) )

         mat(k,1174) = -( het_rates(k,102) )

         mat(k,363) = -( rxt(k,129) + het_rates(k,103) )

         mat(k,1809) = -( rxt(k,11) + het_rates(k,104) )
         mat(k,1432) = rxt(k,731)
         mat(k,741) = rxt(k,732)
         mat(k,603) = rxt(k,733)
         mat(k,343) = 2.000_r8*rxt(k,734) + 2.000_r8*rxt(k,784) + 2.000_r8*rxt(k,787) &
                      + 2.000_r8*rxt(k,798)
         mat(k,477) = rxt(k,735)
         mat(k,1347) = rxt(k,736)
         mat(k,2656) = .500_r8*rxt(k,738)
         mat(k,2155) = rxt(k,739)
         mat(k,483) = rxt(k,740)
         mat(k,263) = rxt(k,741)
         mat(k,758) = rxt(k,742)
         mat(k,653) = rxt(k,786) + rxt(k,789) + rxt(k,794)
         mat(k,1020) = rxt(k,788) + rxt(k,793) + rxt(k,799)

         mat(k,2092) = -( rxt(k,730) + het_rates(k,105) )
         mat(k,517) = rxt(k,12) + rxt(k,234)
         mat(k,803) = rxt(k,34)
         mat(k,793) = .900_r8*rxt(k,35)
         mat(k,514) = rxt(k,36)
         mat(k,205) = 1.500_r8*rxt(k,37)
         mat(k,509) = .560_r8*rxt(k,38)
         mat(k,536) = rxt(k,39)
         mat(k,362) = .600_r8*rxt(k,40)
         mat(k,710) = .600_r8*rxt(k,41)
         mat(k,426) = rxt(k,42)
         mat(k,397) = rxt(k,43)
         mat(k,420) = rxt(k,44)
         mat(k,501) = rxt(k,45)
         mat(k,1363) = rxt(k,49)
         mat(k,1542) = rxt(k,51)
         mat(k,1371) = 2.000_r8*rxt(k,58)
         mat(k,1236) = 2.000_r8*rxt(k,59)
         mat(k,1435) = .670_r8*rxt(k,60)
         mat(k,254) = rxt(k,61)
         mat(k,1419) = rxt(k,62)
         mat(k,489) = rxt(k,63)
         mat(k,827) = rxt(k,64)
         mat(k,1528) = 1.340_r8*rxt(k,65) + .660_r8*rxt(k,66)
         mat(k,1350) = rxt(k,71)
         mat(k,415) = rxt(k,76)
         mat(k,637) = rxt(k,77)
         mat(k,227) = rxt(k,79)
         mat(k,769) = rxt(k,80)
         mat(k,759) = rxt(k,81)
         mat(k,549) = rxt(k,82)
         mat(k,1229) = rxt(k,83)
         mat(k,1334) = 1.200_r8*rxt(k,84)
         mat(k,783) = rxt(k,85)
         mat(k,855) = rxt(k,97)
         mat(k,532) = rxt(k,98)
         mat(k,1794) = rxt(k,178)
         mat(k,646) = rxt(k,397)
         mat(k,330) = rxt(k,463) + rxt(k,464)
         mat(k,1628) = rxt(k,566)

         mat(k,516) = -( rxt(k,12) + rxt(k,13) + rxt(k,234) + het_rates(k,106) )

         mat(k,879) = -( rxt(k,130) + het_rates(k,107) )
         mat(k,651) = rxt(k,786) + rxt(k,789) + rxt(k,794)

         mat(k,641) = -( rxt(k,397) + het_rates(k,108) )

         mat(k,970) = -( rxt(k,131) + het_rates(k,109) )
         mat(k,1018) = rxt(k,788) + rxt(k,793) + rxt(k,799)

         mat(k,1428) = -( rxt(k,60) + rxt(k,731) + het_rates(k,110) )

         mat(k,251) = -( rxt(k,61) + het_rates(k,111) )
         mat(k,1597) = rxt(k,566)

         mat(k,1416) = -( rxt(k,62) + het_rates(k,112) )
         mat(k,1427) = .170_r8*rxt(k,60)

         mat(k,431) = -( het_rates(k,113) )

         mat(k,142) = -( het_rates(k,114) )

         mat(k,1048) = -( het_rates(k,115) )

         mat(k,1580) = -( het_rates(k,116) )

         mat(k,1619) = -( rxt(k,566) + het_rates(k,117) )

         mat(k,737) = -( rxt(k,732) + het_rates(k,118) )

         mat(k,598) = -( rxt(k,733) + het_rates(k,119) )

         mat(k,1503) = -( het_rates(k,120) )

         mat(k,486) = -( rxt(k,63) + het_rates(k,121) )

         mat(k,72) = -( het_rates(k,122) )

         mat(k,823) = -( rxt(k,64) + het_rates(k,123) )
         mat(k,487) = rxt(k,63)

         mat(k,80) = -( het_rates(k,124) )

         mat(k,78) = -( het_rates(k,125) )

         mat(k,1522) = -( rxt(k,65) + rxt(k,66) + het_rates(k,126) )
         mat(k,825) = .288_r8*rxt(k,64)

         mat(k,1648) = -( het_rates(k,127) )

         mat(k,383) = -( het_rates(k,128) )

         mat(k,904) = -( het_rates(k,129) )
         mat(k,535) = .600_r8*rxt(k,39)

         mat(k,1699) = -( het_rates(k,130) )
         mat(k,1525) = .660_r8*rxt(k,65)
         mat(k,678) = rxt(k,69) + rxt(k,541)

         mat(k,1108) = -( het_rates(k,131) )
         mat(k,708) = .600_r8*rxt(k,41)

         mat(k,571) = -( rxt(k,67) + het_rates(k,132) )
         mat(k,796) = .800_r8*rxt(k,34)
         mat(k,785) = .800_r8*rxt(k,35)

         mat(k,858) = -( het_rates(k,133) )

         mat(k,388) = -( rxt(k,68) + het_rates(k,134) )

         mat(k,675) = -( rxt(k,69) + rxt(k,541) + het_rates(k,135) )

         mat(k,1260) = -( het_rates(k,136) )

         mat(k,86) = -( het_rates(k,137) )

         mat(k,1672) = -( rxt(k,70) + het_rates(k,138) )
         mat(k,826) = .402_r8*rxt(k,64)

         mat(k,687) = -( rxt(k,200) + het_rates(k,139) )
         mat(k,1915) = rxt(k,17)

         mat(k,294) = -( rxt(k,14) + het_rates(k,140) )

         mat(k,342) = -( rxt(k,15) + rxt(k,16) + rxt(k,235) + rxt(k,734) + rxt(k,784) &
                      + rxt(k,787) + rxt(k,798) + het_rates(k,141) )

         mat(k,474) = -( rxt(k,735) + het_rates(k,142) )

         mat(k,1343) = -( rxt(k,71) + rxt(k,736) + het_rates(k,143) )

         mat(k,23) = -( het_rates(k,144) )

         mat(k,24) = -( het_rates(k,145) )

         mat(k,25) = -( het_rates(k,146) )

         mat(k,112) = -( het_rates(k,147) )

         mat(k,26) = -( rxt(k,737) + het_rates(k,148) )

         mat(k,27) = -( rxt(k,821) + het_rates(k,149) )

      end do

      end subroutine linmat02

      subroutine linmat03( avec_len, mat, y, rxt, het_rates )
!----------------------------------------------
!       ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer,  intent(in)    ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(in)    ::  het_rates(veclen,gas_pcnst)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)
!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k


      do k = 1,avec_len

         mat(k,28) = -( rxt(k,820) + het_rates(k,150) )

         mat(k,1972) = -( rxt(k,17) + het_rates(k,151) )
         mat(k,344) = rxt(k,15)
         mat(k,2658) = rxt(k,18) + .500_r8*rxt(k,738)
         mat(k,2157) = rxt(k,19)
         mat(k,691) = rxt(k,200)

         mat(k,2668) = -( rxt(k,18) + rxt(k,738) + het_rates(k,152) )
         mat(k,1818) = rxt(k,11)
         mat(k,520) = rxt(k,12) + rxt(k,234)
         mat(k,346) = rxt(k,16) + rxt(k,235)
         mat(k,2167) = rxt(k,20)
         mat(k,806) = rxt(k,34)
         mat(k,1439) = rxt(k,60)
         mat(k,491) = rxt(k,63)
         mat(k,683) = rxt(k,69) + rxt(k,541)
         mat(k,1353) = rxt(k,71)
         mat(k,1008) = rxt(k,72)
         mat(k,485) = rxt(k,73)
         mat(k,264) = rxt(k,74)
         mat(k,623) = .600_r8*rxt(k,75) + rxt(k,471)
         mat(k,761) = rxt(k,81)
         mat(k,656) = rxt(k,101)
         mat(k,1025) = rxt(k,120)
         mat(k,147) = rxt(k,657)

         mat(k,2159) = -( rxt(k,19) + rxt(k,20) + rxt(k,739) + het_rates(k,153) )
         mat(k,518) = rxt(k,13)
         mat(k,345) = rxt(k,15) + rxt(k,16) + rxt(k,235)
         mat(k,620) = .400_r8*rxt(k,75)
         mat(k,654) = rxt(k,102)
         mat(k,1021) = rxt(k,119)

         mat(k,1003) = -( rxt(k,72) + het_rates(k,154) )

         mat(k,1315) = -( het_rates(k,155) )

         mat(k,480) = -( rxt(k,73) + rxt(k,740) + het_rates(k,156) )

         mat(k,29) = -( het_rates(k,157) )

         mat(k,30) = -( het_rates(k,158) )

         mat(k,31) = -( het_rates(k,159) )

         mat(k,32) = -( het_rates(k,160) )

         mat(k,2738) = -( rxt(k,162) + het_rates(k,161) )
         mat(k,2766) = rxt(k,2)
         mat(k,2617) = rxt(k,7)
         mat(k,347) = rxt(k,15)
         mat(k,1984) = rxt(k,17)
         mat(k,2670) = rxt(k,18)
         mat(k,2169) = rxt(k,20)
         mat(k,1785) = .180_r8*rxt(k,55)
         mat(k,979) = rxt(k,56)
         mat(k,2506) = rxt(k,100)
         mat(k,2201) = rxt(k,118)
         mat(k,411) = rxt(k,132)
         mat(k,1467) = rxt(k,140) + rxt(k,718)
         mat(k,1014) = rxt(k,141)
         mat(k,305) = rxt(k,142)
         mat(k,2550) = rxt(k,155) + rxt(k,156)
         mat(k,695) = rxt(k,200)
         mat(k,597) = rxt(k,712)

         mat(k,2546) = -( rxt(k,155) + rxt(k,156) + het_rates(k,162) )
         mat(k,2762) = rxt(k,1)
         mat(k,2613) = rxt(k,8)
         mat(k,297) = rxt(k,14)

         mat(k,2614) = -( rxt(k,7) + rxt(k,8) + het_rates(k,163) )
         mat(k,2735) = rxt(k,162)

         mat(k,33) = -( het_rates(k,164) )

         mat(k,407) = -( rxt(k,132) + het_rates(k,165) )

         mat(k,441) = -( rxt(k,139) + het_rates(k,166) )

         mat(k,2471) = -( het_rates(k,167) )
         mat(k,2760) = rxt(k,3)
         mat(k,733) = 2.000_r8*rxt(k,4)
         mat(k,1815) = rxt(k,11)
         mat(k,519) = rxt(k,13)
         mat(k,795) = rxt(k,35)
         mat(k,515) = rxt(k,36)
         mat(k,398) = rxt(k,43)
         mat(k,421) = rxt(k,44)
         mat(k,502) = rxt(k,45)
         mat(k,310) = rxt(k,46)
         mat(k,727) = rxt(k,52)
         mat(k,526) = rxt(k,53)
         mat(k,1782) = .330_r8*rxt(k,55)
         mat(k,133) = rxt(k,57)
         mat(k,255) = rxt(k,61)
         mat(k,392) = rxt(k,68)
         mat(k,484) = rxt(k,73)
         mat(k,416) = rxt(k,76)
         mat(k,639) = rxt(k,77)
         mat(k,496) = rxt(k,78)
         mat(k,771) = rxt(k,80)
         mat(k,551) = rxt(k,82)
         mat(k,784) = rxt(k,85)
         mat(k,242) = rxt(k,94)
         mat(k,856) = rxt(k,97)
         mat(k,533) = rxt(k,98)
         mat(k,882) = rxt(k,130)
         mat(k,973) = rxt(k,131)
         mat(k,2664) = .500_r8*rxt(k,738)

         mat(k,262) = -( rxt(k,74) + rxt(k,741) + het_rates(k,168) )

         mat(k,616) = -( rxt(k,75) + rxt(k,471) + het_rates(k,169) )

         mat(k,145) = -( rxt(k,657) + het_rates(k,170) )

         mat(k,670) = -( het_rates(k,171) )
         mat(k,308) = rxt(k,46)

         mat(k,625) = -( het_rates(k,172) )

         mat(k,206) = -( het_rates(k,173) )

         mat(k,412) = -( rxt(k,76) + het_rates(k,174) )

         mat(k,946) = -( het_rates(k,175) )

         mat(k,34) = -( het_rates(k,176) )

         mat(k,35) = -( het_rates(k,177) )

         mat(k,633) = -( rxt(k,77) + het_rates(k,178) )

         mat(k,1405) = -( het_rates(k,179) )
         mat(k,1330) = .150_r8*rxt(k,84)

         mat(k,492) = -( rxt(k,78) + het_rates(k,180) )

         mat(k,592) = -( rxt(k,712) + het_rates(k,181) )
         mat(k,442) = rxt(k,139)
         mat(k,1451) = rxt(k,140)

         mat(k,36) = -( rxt(k,133) + het_rates(k,182) )

         mat(k,1456) = -( rxt(k,140) + rxt(k,718) + het_rates(k,183) )
         mat(k,1011) = rxt(k,141)
         mat(k,593) = rxt(k,712)

         mat(k,1010) = -( rxt(k,141) + het_rates(k,184) )
         mat(k,304) = rxt(k,142)
         mat(k,1452) = rxt(k,718)

         mat(k,303) = -( rxt(k,142) + het_rates(k,185) )
         mat(k,140) = rxt(k,138)

         mat(k,37) = -( het_rates(k,186) )

         mat(k,38) = -( het_rates(k,187) )

         mat(k,39) = -( het_rates(k,188) )

         mat(k,40) = -( rxt(k,143) + het_rates(k,189) )

         mat(k,41) = -( rxt(k,144) + het_rates(k,190) )

         mat(k,42) = -( rxt(k,145) + het_rates(k,191) )

         mat(k,43) = -( rxt(k,146) + het_rates(k,192) )

         mat(k,44) = -( rxt(k,147) + het_rates(k,193) )

         mat(k,45) = -( rxt(k,148) + het_rates(k,194) )

         mat(k,46) = -( rxt(k,149) + het_rates(k,195) )

         mat(k,47) = -( rxt(k,150) + het_rates(k,196) )

         mat(k,48) = -( rxt(k,151) + het_rates(k,197) )

         mat(k,49) = -( rxt(k,152) + het_rates(k,198) )

         mat(k,50) = -( het_rates(k,199) )
         mat(k,1232) = rxt(k,729)

         mat(k,51) = -( het_rates(k,200) )

         mat(k,52) = -( het_rates(k,201) )

         mat(k,53) = -( het_rates(k,202) )

         mat(k,54) = -( het_rates(k,203) )

         mat(k,87) = -( rxt(k,822) + het_rates(k,204) )

         mat(k,93) = -( het_rates(k,205) )

         mat(k,224) = -( rxt(k,79) + het_rates(k,206) )

         mat(k,1387) = -( het_rates(k,207) )

         mat(k,762) = -( rxt(k,80) + het_rates(k,208) )

         mat(k,756) = -( rxt(k,81) + rxt(k,742) + het_rates(k,209) )

         mat(k,1138) = -( het_rates(k,210) )

         mat(k,545) = -( rxt(k,82) + het_rates(k,211) )

         mat(k,1224) = -( rxt(k,83) + het_rates(k,212) )
         mat(k,481) = rxt(k,73)
         mat(k,757) = rxt(k,81)
         mat(k,548) = rxt(k,82)

         mat(k,1329) = -( rxt(k,84) + het_rates(k,213) )
         mat(k,765) = rxt(k,80)
         mat(k,1226) = rxt(k,83)

         mat(k,891) = -( het_rates(k,214) )

         mat(k,775) = -( rxt(k,85) + het_rates(k,215) )

         mat(k,212) = -( het_rates(k,216) )

         mat(k,99) = -( het_rates(k,217) )

         mat(k,129) = -( rxt(k,86) + het_rates(k,218) )

         mat(k,311) = -( rxt(k,134) + rxt(k,135) + rxt(k,801) + rxt(k,806) + rxt(k,812) &
                 + het_rates(k,219) )

         mat(k,552) = -( rxt(k,136) + rxt(k,137) + rxt(k,805) + rxt(k,811) + rxt(k,817) &
                 + het_rates(k,220) )

         mat(k,1067) = -( rxt(k,21) + het_rates(k,221) )
         mat(k,429) = rxt(k,743)
         mat(k,317) = rxt(k,744)
         mat(k,322) = rxt(k,745)
         mat(k,239) = rxt(k,746)
         mat(k,436) = rxt(k,747)
         mat(k,1091) = .500_r8*rxt(k,748)
         mat(k,270) = rxt(k,749) + rxt(k,802) + rxt(k,808) + rxt(k,814)
         mat(k,368) = 2.000_r8*rxt(k,750) + 2.000_r8*rxt(k,804) + 2.000_r8*rxt(k,810) &
                      + 2.000_r8*rxt(k,816)
         mat(k,1101) = rxt(k,751)
         mat(k,166) = rxt(k,752) + rxt(k,803) + rxt(k,809) + rxt(k,815)
         mat(k,326) = rxt(k,753)
         mat(k,276) = rxt(k,754)
         mat(k,439) = rxt(k,755)
         mat(k,312) = rxt(k,801) + rxt(k,806) + rxt(k,812)
         mat(k,553) = rxt(k,805) + rxt(k,811) + rxt(k,817)

         mat(k,228) = -( rxt(k,22) + rxt(k,23) + rxt(k,237) + het_rates(k,222) )

         mat(k,428) = -( rxt(k,87) + rxt(k,743) + het_rates(k,223) )

         mat(k,315) = -( rxt(k,744) + het_rates(k,224) )

         mat(k,320) = -( rxt(k,745) + het_rates(k,225) )

         mat(k,469) = -( het_rates(k,226) )

         mat(k,231) = -( rxt(k,88) + het_rates(k,227) )

         mat(k,234) = -( rxt(k,89) + rxt(k,542) + het_rates(k,228) )

         mat(k,163) = -( rxt(k,246) + het_rates(k,229) )
         mat(k,1071) = rxt(k,24)

         mat(k,237) = -( rxt(k,746) + het_rates(k,230) )

         mat(k,435) = -( rxt(k,90) + rxt(k,747) + het_rates(k,231) )

         mat(k,1079) = -( rxt(k,24) + het_rates(k,233) )
         mat(k,1092) = rxt(k,25) + .500_r8*rxt(k,748)
         mat(k,271) = rxt(k,26)
         mat(k,369) = rxt(k,28)
         mat(k,1102) = rxt(k,31)
         mat(k,164) = rxt(k,246)

         mat(k,1093) = -( rxt(k,25) + rxt(k,748) + het_rates(k,234) )
         mat(k,1068) = rxt(k,21)
         mat(k,229) = rxt(k,22) + rxt(k,237)
         mat(k,272) = rxt(k,27) + rxt(k,238)
         mat(k,370) = rxt(k,29) + rxt(k,239)
         mat(k,1103) = rxt(k,30)
         mat(k,130) = rxt(k,86)
         mat(k,430) = rxt(k,87)
         mat(k,233) = rxt(k,88)
         mat(k,235) = rxt(k,89) + rxt(k,542)
         mat(k,437) = rxt(k,90)
         mat(k,274) = rxt(k,91)
         mat(k,327) = rxt(k,92)
         mat(k,277) = rxt(k,93)
         mat(k,244) = .600_r8*rxt(k,95) + rxt(k,472)
         mat(k,440) = rxt(k,96)
         mat(k,313) = rxt(k,135)
         mat(k,554) = rxt(k,137)
         mat(k,116) = rxt(k,658)

      end do

      end subroutine linmat03

      subroutine linmat04( avec_len, mat, y, rxt, het_rates )
!----------------------------------------------
!       ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer,  intent(in)    ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(in)    ::  het_rates(veclen,gas_pcnst)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)
!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k


      do k = 1,avec_len

         mat(k,268) = -( rxt(k,26) + rxt(k,27) + rxt(k,238) + rxt(k,749) + rxt(k,802) &
                      + rxt(k,808) + rxt(k,814) + het_rates(k,235) )

         mat(k,366) = -( rxt(k,28) + rxt(k,29) + rxt(k,239) + rxt(k,750) + rxt(k,804) &
                      + rxt(k,810) + rxt(k,816) + het_rates(k,236) )

         mat(k,1104) = -( rxt(k,30) + rxt(k,31) + rxt(k,751) + het_rates(k,237) )
         mat(k,230) = rxt(k,23)
         mat(k,371) = rxt(k,28) + rxt(k,29) + rxt(k,239)
         mat(k,167) = rxt(k,32) + rxt(k,33) + rxt(k,240)
         mat(k,245) = .400_r8*rxt(k,95)
         mat(k,314) = rxt(k,134)
         mat(k,555) = rxt(k,136)

         mat(k,165) = -( rxt(k,32) + rxt(k,33) + rxt(k,240) + rxt(k,752) + rxt(k,803) &
                      + rxt(k,809) + rxt(k,815) + het_rates(k,238) )

         mat(k,273) = -( rxt(k,91) + het_rates(k,239) )

         mat(k,568) = -( het_rates(k,240) )

         mat(k,324) = -( rxt(k,92) + rxt(k,753) + het_rates(k,241) )

         mat(k,580) = -( rxt(k,164) + het_rates(k,242) )
         mat(k,578) = rxt(k,10)
         mat(k,1077) = rxt(k,24)
         mat(k,1090) = rxt(k,25)
         mat(k,269) = rxt(k,26)
         mat(k,367) = rxt(k,28)
         mat(k,1100) = rxt(k,30)
         mat(k,473) = rxt(k,167) + rxt(k,168)

         mat(k,472) = -( rxt(k,167) + rxt(k,168) + het_rates(k,243) )
         mat(k,576) = rxt(k,9)

         mat(k,1479) = -( het_rates(k,244) )

         mat(k,577) = -( rxt(k,9) + rxt(k,10) + het_rates(k,245) )
         mat(k,579) = rxt(k,164)

         mat(k,275) = -( rxt(k,93) + rxt(k,754) + het_rates(k,246) )

         mat(k,240) = -( rxt(k,94) + het_rates(k,247) )

         mat(k,243) = -( rxt(k,95) + rxt(k,472) + het_rates(k,248) )

         mat(k,115) = -( rxt(k,658) + het_rates(k,249) )

         mat(k,438) = -( rxt(k,96) + rxt(k,755) + het_rates(k,250) )

         mat(k,280) = -( het_rates(k,251) )

         mat(k,922) = -( het_rates(k,252) )

         mat(k,846) = -( rxt(k,97) + het_rates(k,253) )

         mat(k,105) = -( het_rates(k,254) )

         mat(k,289) = -( het_rates(k,255) )

         mat(k,747) = -( het_rates(k,256) )

         mat(k,528) = -( rxt(k,98) + het_rates(k,257) )

         mat(k,2767) = -( rxt(k,1) + rxt(k,2) + rxt(k,3) + het_rates(k,260) )
         mat(k,1786) = .050_r8*rxt(k,55)
         mat(k,141) = rxt(k,138)

      end do

      end subroutine linmat04

      subroutine linmat( avec_len, mat, y, rxt, het_rates )
!----------------------------------------------
!       ... linear matrix entries for implicit species
!----------------------------------------------

      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      use shr_kind_mod, only : r8 => shr_kind_r8

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer,  intent(in)    ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(in)    ::  het_rates(veclen,gas_pcnst)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)

      call linmat01( avec_len, mat, y, rxt, het_rates )
      call linmat02( avec_len, mat, y, rxt, het_rates )
      call linmat03( avec_len, mat, y, rxt, het_rates )
      call linmat04( avec_len, mat, y, rxt, het_rates )

      end subroutine linmat

      end module mo_lin_matrix
