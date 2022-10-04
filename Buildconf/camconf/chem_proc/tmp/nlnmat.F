
      module mo_nln_matrix

      use shr_kind_mod, only : r8 => shr_kind_r8

      use chem_mods, only: veclen
      private
      public :: nlnmat

      contains

      subroutine     nlnmat01( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,814) = -(rxt(k,608)*y(k,105) + rxt(k,609)*y(k,151) + rxt(k,650) &
                      *y(k,152))
         mat(k,2038) = -rxt(k,608)*y(k,1)
         mat(k,1922) = -rxt(k,609)*y(k,1)
         mat(k,2633) = -rxt(k,650)*y(k,1)

         mat(k,260) = rxt(k,616)*y(k,167)
         mat(k,2401) = rxt(k,616)*y(k,27)

         mat(k,797) = -(rxt(k,545)*y(k,167))
         mat(k,2399) = -rxt(k,545)*y(k,2)

         mat(k,1151) = rxt(k,548)*y(k,151)
         mat(k,1921) = rxt(k,548)*y(k,3)

         mat(k,1157) = -(rxt(k,546)*y(k,105) + (rxt(k,547) + rxt(k,548)) * y(k,151))
         mat(k,2062) = -rxt(k,546)*y(k,3)
         mat(k,1942) = -(rxt(k,547) + rxt(k,548)) * y(k,3)

         mat(k,789) = rxt(k,551)*y(k,167)
         mat(k,266) = rxt(k,552)*y(k,167)
         mat(k,2430) = rxt(k,551)*y(k,4) + rxt(k,552)*y(k,20)

         mat(k,786) = -(rxt(k,551)*y(k,167))
         mat(k,2398) = -rxt(k,551)*y(k,4)

         mat(k,1150) = rxt(k,546)*y(k,105)
         mat(k,2036) = rxt(k,546)*y(k,3)




         mat(k,1199) = -(rxt(k,669)*y(k,153) + rxt(k,670)*y(k,163) + rxt(k,671) &
                      *y(k,167))
         mat(k,2126) = -rxt(k,669)*y(k,8)
         mat(k,2577) = -rxt(k,670)*y(k,8)
         mat(k,2433) = -rxt(k,671)*y(k,8)

         mat(k,60) = -(rxt(k,757)*y(k,105) + rxt(k,758)*y(k,151))
         mat(k,1991) = -rxt(k,757)*y(k,9)
         mat(k,1888) = -rxt(k,758)*y(k,9)

         mat(k,1185) = rxt(k,760)*y(k,167)
         mat(k,2296) = rxt(k,760)*y(k,8)

         mat(k,195) = -((rxt(k,611) + rxt(k,761)) * y(k,167))
         mat(k,2312) = -(rxt(k,611) + rxt(k,761)) * y(k,10)

         mat(k,698) = -(rxt(k,612)*y(k,105) + rxt(k,613)*y(k,151))
         mat(k,2030) = -rxt(k,612)*y(k,11)
         mat(k,1916) = -rxt(k,613)*y(k,11)

         mat(k,198) = .350_r8*rxt(k,611)*y(k,167)
         mat(k,512) = rxt(k,615)*y(k,167)
         mat(k,2388) = .350_r8*rxt(k,611)*y(k,10) + rxt(k,615)*y(k,13)

         mat(k,66) = -(rxt(k,762)*y(k,105) + rxt(k,763)*y(k,151))
         mat(k,1992) = -rxt(k,762)*y(k,12)
         mat(k,1889) = -rxt(k,763)*y(k,12)

         mat(k,194) = rxt(k,761)*y(k,167)
         mat(k,2297) = rxt(k,761)*y(k,10)

         mat(k,510) = -(rxt(k,615)*y(k,167))
         mat(k,2364) = -rxt(k,615)*y(k,13)

         mat(k,696) = rxt(k,612)*y(k,105)
         mat(k,2016) = rxt(k,612)*y(k,11)


         mat(k,196) = .120_r8*rxt(k,611)*y(k,167)
         mat(k,2313) = .120_r8*rxt(k,611)*y(k,10)


         mat(k,1187) = .100_r8*rxt(k,670)*y(k,163)
         mat(k,1246) = .100_r8*rxt(k,675)*y(k,163)
         mat(k,2559) = .100_r8*rxt(k,670)*y(k,8) + .100_r8*rxt(k,675)*y(k,136)


         mat(k,697) = .500_r8*rxt(k,613)*y(k,151)
         mat(k,1905) = .500_r8*rxt(k,613)*y(k,11) + .200_r8*rxt(k,653)*y(k,214)  &
                      + .060_r8*rxt(k,661)*y(k,252)
         mat(k,888) = .200_r8*rxt(k,653)*y(k,151)
         mat(k,918) = .060_r8*rxt(k,661)*y(k,151)


         mat(k,1899) = .200_r8*rxt(k,653)*y(k,214) + .200_r8*rxt(k,661)*y(k,252)
         mat(k,887) = .200_r8*rxt(k,653)*y(k,151)
         mat(k,916) = .200_r8*rxt(k,661)*y(k,151)


         mat(k,1917) = .200_r8*rxt(k,653)*y(k,214) + .150_r8*rxt(k,661)*y(k,252)
         mat(k,889) = .200_r8*rxt(k,653)*y(k,151)
         mat(k,919) = .150_r8*rxt(k,661)*y(k,151)


         mat(k,1900) = .210_r8*rxt(k,661)*y(k,252)
         mat(k,917) = .210_r8*rxt(k,661)*y(k,151)

         mat(k,265) = -(rxt(k,552)*y(k,167))
         mat(k,2328) = -rxt(k,552)*y(k,20)

         mat(k,1186) = .050_r8*rxt(k,670)*y(k,163)
         mat(k,1245) = .050_r8*rxt(k,675)*y(k,163)
         mat(k,2557) = .050_r8*rxt(k,670)*y(k,8) + .050_r8*rxt(k,675)*y(k,136)

         mat(k,556) = -(rxt(k,501)*y(k,153) + rxt(k,502)*y(k,167))
         mat(k,2113) = -rxt(k,501)*y(k,21)
         mat(k,2372) = -rxt(k,502)*y(k,21)

         mat(k,1757) = -(rxt(k,315)*y(k,51) + rxt(k,316)*y(k,105) + rxt(k,317) &
                      *y(k,163))
         mat(k,2213) = -rxt(k,315)*y(k,22)
         mat(k,2086) = -rxt(k,316)*y(k,22)
         mat(k,2600) = -rxt(k,317)*y(k,22)

         mat(k,2490) = 4.000_r8*rxt(k,318)*y(k,24) + (rxt(k,319)+rxt(k,320))*y(k,70)  &
                      + rxt(k,323)*y(k,151) + rxt(k,327)*y(k,161) + rxt(k,328) &
                      *y(k,167) + rxt(k,714)*y(k,183)
         mat(k,171) = rxt(k,293)*y(k,162)
         mat(k,178) = rxt(k,337)*y(k,162)
         mat(k,584) = 2.000_r8*rxt(k,357)*y(k,67) + 2.000_r8*rxt(k,369)*y(k,162)  &
                      + 2.000_r8*rxt(k,358)*y(k,167)
         mat(k,714) = rxt(k,359)*y(k,67) + rxt(k,370)*y(k,162) + rxt(k,360)*y(k,167)
         mat(k,540) = 3.000_r8*rxt(k,364)*y(k,67) + 3.000_r8*rxt(k,338)*y(k,162)  &
                      + 3.000_r8*rxt(k,365)*y(k,167)
         mat(k,2259) = 2.000_r8*rxt(k,357)*y(k,50) + rxt(k,359)*y(k,52)  &
                      + 3.000_r8*rxt(k,364)*y(k,66)
         mat(k,2185) = (rxt(k,319)+rxt(k,320))*y(k,24)
         mat(k,137) = 2.000_r8*rxt(k,339)*y(k,162)
         mat(k,983) = rxt(k,332)*y(k,161) + rxt(k,340)*y(k,162) + rxt(k,333)*y(k,167)
         mat(k,1967) = rxt(k,323)*y(k,24)
         mat(k,2721) = rxt(k,327)*y(k,24) + rxt(k,332)*y(k,96)
         mat(k,2533) = rxt(k,293)*y(k,43) + rxt(k,337)*y(k,44) + 2.000_r8*rxt(k,369) &
                      *y(k,50) + rxt(k,370)*y(k,52) + 3.000_r8*rxt(k,338)*y(k,66)  &
                      + 2.000_r8*rxt(k,339)*y(k,93) + rxt(k,340)*y(k,96)
         mat(k,2460) = rxt(k,328)*y(k,24) + 2.000_r8*rxt(k,358)*y(k,50) + rxt(k,360) &
                      *y(k,52) + 3.000_r8*rxt(k,365)*y(k,66) + rxt(k,333)*y(k,96)
         mat(k,1457) = rxt(k,714)*y(k,24)


         mat(k,2479) = rxt(k,321)*y(k,70)
         mat(k,2173) = rxt(k,321)*y(k,24)
         mat(k,2673) = (rxt(k,792)+rxt(k,797))*y(k,107)
         mat(k,877) = (rxt(k,792)+rxt(k,797))*y(k,100)

         mat(k,2501) = -(4._r8*rxt(k,318)*y(k,24) + (rxt(k,319) + rxt(k,320) + rxt(k,321) &
                      ) * y(k,70) + rxt(k,322)*y(k,105) + rxt(k,323)*y(k,151) &
                      + rxt(k,324)*y(k,152) + rxt(k,327)*y(k,161) + rxt(k,328) &
                      *y(k,167) + rxt(k,714)*y(k,183))
         mat(k,2196) = -(rxt(k,319) + rxt(k,320) + rxt(k,321)) * y(k,24)
         mat(k,2098) = -rxt(k,322)*y(k,24)
         mat(k,1979) = -rxt(k,323)*y(k,24)
         mat(k,2665) = -rxt(k,324)*y(k,24)
         mat(k,2733) = -rxt(k,327)*y(k,24)
         mat(k,2472) = -rxt(k,328)*y(k,24)
         mat(k,1464) = -rxt(k,714)*y(k,24)

         mat(k,1762) = rxt(k,317)*y(k,163)
         mat(k,655) = rxt(k,325)*y(k,161)
         mat(k,986) = rxt(k,341)*y(k,162)
         mat(k,883) = rxt(k,335)*y(k,161)
         mat(k,2733) = mat(k,2733) + rxt(k,325)*y(k,25) + rxt(k,335)*y(k,107)
         mat(k,2545) = rxt(k,341)*y(k,96)
         mat(k,2612) = rxt(k,317)*y(k,22)

         mat(k,650) = -(rxt(k,325)*y(k,161))
         mat(k,2707) = -rxt(k,325)*y(k,25)

         mat(k,2483) = rxt(k,324)*y(k,152)
         mat(k,2629) = rxt(k,324)*y(k,24)


         mat(k,259) = -(rxt(k,616)*y(k,167))
         mat(k,2326) = -rxt(k,616)*y(k,27)

         mat(k,658) = rxt(k,619)*y(k,151)
         mat(k,1896) = rxt(k,619)*y(k,28)

         mat(k,660) = -(rxt(k,617)*y(k,105) + rxt(k,619)*y(k,151))
         mat(k,2028) = -rxt(k,617)*y(k,28)
         mat(k,1913) = -rxt(k,619)*y(k,28)

         mat(k,395) = rxt(k,618)*y(k,167)
         mat(k,2384) = rxt(k,618)*y(k,29) + .070_r8*rxt(k,656)*y(k,216)  &
                      + .060_r8*rxt(k,659)*y(k,251)
         mat(k,215) = .070_r8*rxt(k,656)*y(k,167)
         mat(k,282) = .060_r8*rxt(k,659)*y(k,167)

         mat(k,394) = -(rxt(k,618)*y(k,167))
         mat(k,2347) = -rxt(k,618)*y(k,29)

         mat(k,659) = rxt(k,617)*y(k,105)
         mat(k,2008) = rxt(k,617)*y(k,28)

         mat(k,348) = -(rxt(k,435)*y(k,67) + rxt(k,436)*y(k,167))
         mat(k,2233) = -rxt(k,435)*y(k,30)
         mat(k,2341) = -rxt(k,436)*y(k,30)

         mat(k,608) = -(rxt(k,437)*y(k,67) + rxt(k,438)*y(k,163) + rxt(k,468)*y(k,167))
         mat(k,2241) = -rxt(k,437)*y(k,31)
         mat(k,2562) = -rxt(k,438)*y(k,31)
         mat(k,2380) = -rxt(k,468)*y(k,31)

         mat(k,1032) = -(4._r8*rxt(k,440)*y(k,32) + rxt(k,441)*y(k,62) + rxt(k,442) &
                      *y(k,105) + rxt(k,443)*y(k,151))
         mat(k,1838) = -rxt(k,441)*y(k,32)
         mat(k,2055) = -rxt(k,442)*y(k,32)
         mat(k,1935) = -rxt(k,443)*y(k,32)

         mat(k,418) = .500_r8*rxt(k,446)*y(k,167)
         mat(k,355) = rxt(k,447)*y(k,67) + rxt(k,448)*y(k,167)
         mat(k,2247) = rxt(k,447)*y(k,35)
         mat(k,2421) = .500_r8*rxt(k,446)*y(k,34) + rxt(k,448)*y(k,35)

         mat(k,298) = -(rxt(k,445)*y(k,167))
         mat(k,2333) = -rxt(k,445)*y(k,33)

         mat(k,1029) = .800_r8*rxt(k,440)*y(k,32) + .200_r8*rxt(k,441)*y(k,62)
         mat(k,1824) = .200_r8*rxt(k,441)*y(k,32)

         mat(k,417) = -(rxt(k,446)*y(k,167))
         mat(k,2350) = -rxt(k,446)*y(k,34)

         mat(k,1030) = rxt(k,442)*y(k,105)
         mat(k,2010) = rxt(k,442)*y(k,32)

         mat(k,354) = -(rxt(k,447)*y(k,67) + rxt(k,448)*y(k,167))
         mat(k,2234) = -rxt(k,447)*y(k,35)
         mat(k,2342) = -rxt(k,448)*y(k,35)

         mat(k,1291) = -(rxt(k,474)*y(k,153) + rxt(k,475)*y(k,163) + rxt(k,498) &
                      *y(k,167))
         mat(k,2130) = -rxt(k,474)*y(k,36)
         mat(k,2581) = -rxt(k,475)*y(k,36)
         mat(k,2437) = -rxt(k,498)*y(k,36)

         mat(k,1053) = .130_r8*rxt(k,582)*y(k,163)
         mat(k,2581) = mat(k,2581) + .130_r8*rxt(k,582)*y(k,115)

         mat(k,991) = -(rxt(k,478)*y(k,62) + rxt(k,479)*y(k,105) + rxt(k,480)*y(k,151))
         mat(k,1836) = -rxt(k,478)*y(k,37)
         mat(k,2053) = -rxt(k,479)*y(k,37)
         mat(k,1933) = -rxt(k,480)*y(k,37)

         mat(k,499) = rxt(k,482)*y(k,167)
         mat(k,118) = rxt(k,483)*y(k,167)
         mat(k,2417) = rxt(k,482)*y(k,38) + rxt(k,483)*y(k,39)

         mat(k,498) = -(rxt(k,482)*y(k,167))
         mat(k,2363) = -rxt(k,482)*y(k,38)

         mat(k,990) = rxt(k,479)*y(k,105)
         mat(k,2015) = rxt(k,479)*y(k,37)

         mat(k,117) = -(rxt(k,483)*y(k,167))
         mat(k,2307) = -rxt(k,483)*y(k,39)

         mat(k,869) = -(rxt(k,621)*y(k,105) + rxt(k,622)*y(k,151))
         mat(k,2042) = -rxt(k,621)*y(k,40)
         mat(k,1925) = -rxt(k,622)*y(k,40)

         mat(k,815) = .400_r8*rxt(k,608)*y(k,105) + rxt(k,609)*y(k,151)
         mat(k,309) = rxt(k,624)*y(k,167)
         mat(k,2042) = mat(k,2042) + .400_r8*rxt(k,608)*y(k,1)
         mat(k,1925) = mat(k,1925) + rxt(k,609)*y(k,1)
         mat(k,2565) = rxt(k,646)*y(k,171)
         mat(k,2406) = rxt(k,624)*y(k,41)
         mat(k,671) = rxt(k,646)*y(k,163)

      end do

      end subroutine     nlnmat01

      subroutine     nlnmat02( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,307) = -(rxt(k,624)*y(k,167))
         mat(k,2335) = -rxt(k,624)*y(k,41)

         mat(k,867) = rxt(k,621)*y(k,105)
         mat(k,2003) = rxt(k,621)*y(k,40)

         mat(k,121) = -(rxt(k,292)*y(k,162))
         mat(k,2508) = -rxt(k,292)*y(k,42)

         mat(k,168) = -(rxt(k,293)*y(k,162))
         mat(k,2513) = -rxt(k,293)*y(k,43)

         mat(k,174) = -(rxt(k,337)*y(k,162))
         mat(k,2514) = -rxt(k,337)*y(k,44)

         mat(k,148) = -(rxt(k,294)*y(k,162))
         mat(k,2510) = -rxt(k,294)*y(k,45)

         mat(k,180) = -(rxt(k,295)*y(k,162))
         mat(k,2515) = -rxt(k,295)*y(k,46)

         mat(k,153) = -(rxt(k,296)*y(k,162))
         mat(k,2511) = -rxt(k,296)*y(k,47)

         mat(k,186) = -(rxt(k,297)*y(k,162))
         mat(k,2516) = -rxt(k,297)*y(k,48)

         mat(k,158) = -(rxt(k,298)*y(k,162))
         mat(k,2512) = -rxt(k,298)*y(k,49)

         mat(k,583) = -(rxt(k,357)*y(k,67) + rxt(k,358)*y(k,167) + rxt(k,369)*y(k,162))
         mat(k,2240) = -rxt(k,357)*y(k,50)
         mat(k,2377) = -rxt(k,358)*y(k,50)
         mat(k,2528) = -rxt(k,369)*y(k,50)

         mat(k,2222) = -(rxt(k,260)*y(k,67) + rxt(k,315)*y(k,22) + rxt(k,379)*y(k,105) &
                      + rxt(k,380)*y(k,153) + rxt(k,381)*y(k,161) + rxt(k,382) &
                      *y(k,167))
         mat(k,2268) = -rxt(k,260)*y(k,51)
         mat(k,1760) = -rxt(k,315)*y(k,51)
         mat(k,2095) = -rxt(k,379)*y(k,51)
         mat(k,2161) = -rxt(k,380)*y(k,51)
         mat(k,2730) = -rxt(k,381)*y(k,51)
         mat(k,2469) = -rxt(k,382)*y(k,51)

         mat(k,804) = .400_r8*rxt(k,545)*y(k,167)
         mat(k,1164) = .100_r8*rxt(k,547)*y(k,151)
         mat(k,1214) = .340_r8*rxt(k,670)*y(k,163)
         mat(k,563) = .500_r8*rxt(k,501)*y(k,153)
         mat(k,613) = rxt(k,438)*y(k,163)
         mat(k,1039) = .700_r8*rxt(k,441)*y(k,62)
         mat(k,1302) = .500_r8*rxt(k,475)*y(k,163)
         mat(k,999) = rxt(k,478)*y(k,62)
         mat(k,1747) = rxt(k,454)*y(k,62) + rxt(k,556)*y(k,116) + .250_r8*rxt(k,509) &
                      *y(k,127) + rxt(k,522)*y(k,130) + .250_r8*rxt(k,600)*y(k,244)
         mat(k,726) = .500_r8*rxt(k,459)*y(k,167)
         mat(k,1874) = .700_r8*rxt(k,441)*y(k,32) + rxt(k,478)*y(k,37) + rxt(k,454) &
                      *y(k,57) + (4.000_r8*rxt(k,385)+2.000_r8*rxt(k,386))*y(k,62)  &
                      + rxt(k,267)*y(k,70) + 1.500_r8*rxt(k,557)*y(k,116)  &
                      + .750_r8*rxt(k,564)*y(k,117) + .800_r8*rxt(k,575)*y(k,120)  &
                      + .880_r8*rxt(k,510)*y(k,127) + 2.000_r8*rxt(k,523)*y(k,130)  &
                      + rxt(k,388)*y(k,151) + .750_r8*rxt(k,679)*y(k,155)  &
                      + .800_r8*rxt(k,493)*y(k,179) + .930_r8*rxt(k,686)*y(k,207)  &
                      + .950_r8*rxt(k,692)*y(k,210) + .800_r8*rxt(k,601)*y(k,244)
         mat(k,914) = rxt(k,390)*y(k,167)
         mat(k,525) = .300_r8*rxt(k,391)*y(k,167)
         mat(k,1780) = (rxt(k,401)+rxt(k,402))*y(k,162)
         mat(k,2193) = rxt(k,267)*y(k,62)
         mat(k,840) = .500_r8*rxt(k,504)*y(k,151)
         mat(k,965) = .500_r8*rxt(k,461)*y(k,151)
         mat(k,1372) = .800_r8*rxt(k,465)*y(k,167)
         mat(k,2095) = mat(k,2095) + .450_r8*rxt(k,524)*y(k,130) + .150_r8*rxt(k,494) &
                      *y(k,179)
         mat(k,1064) = .910_r8*rxt(k,582)*y(k,163)
         mat(k,1591) = rxt(k,556)*y(k,57) + 1.500_r8*rxt(k,557)*y(k,62)  &
                      + .920_r8*rxt(k,559)*y(k,151) + rxt(k,560)*y(k,153)
         mat(k,1630) = .750_r8*rxt(k,564)*y(k,62)
         mat(k,743) = .300_r8*rxt(k,571)*y(k,167)
         mat(k,1514) = .800_r8*rxt(k,575)*y(k,62)
         mat(k,1529) = .120_r8*rxt(k,518)*y(k,163)
         mat(k,1657) = .250_r8*rxt(k,509)*y(k,57) + .880_r8*rxt(k,510)*y(k,62)  &
                      + .250_r8*rxt(k,513)*y(k,151) + .250_r8*rxt(k,512)*y(k,153)
         mat(k,1707) = rxt(k,522)*y(k,57) + 2.000_r8*rxt(k,523)*y(k,62)  &
                      + .450_r8*rxt(k,524)*y(k,105) + 4.000_r8*rxt(k,525)*y(k,130)  &
                      + rxt(k,526)*y(k,151) + rxt(k,527)*y(k,153)
         mat(k,681) = .500_r8*rxt(k,535)*y(k,167)
         mat(k,1274) = .340_r8*rxt(k,675)*y(k,163)
         mat(k,1682) = .600_r8*rxt(k,536)*y(k,163)
         mat(k,1976) = .100_r8*rxt(k,547)*y(k,3) + rxt(k,388)*y(k,62)  &
                      + .500_r8*rxt(k,504)*y(k,84) + .500_r8*rxt(k,461)*y(k,86)  &
                      + .920_r8*rxt(k,559)*y(k,116) + .250_r8*rxt(k,513)*y(k,127)  &
                      + rxt(k,526)*y(k,130) + rxt(k,490)*y(k,175) + rxt(k,495) &
                      *y(k,179) + .340_r8*rxt(k,688)*y(k,207) + .320_r8*rxt(k,694) &
                      *y(k,210) + .250_r8*rxt(k,603)*y(k,244)
         mat(k,2161) = mat(k,2161) + .500_r8*rxt(k,501)*y(k,21) + rxt(k,560)*y(k,116)  &
                      + .250_r8*rxt(k,512)*y(k,127) + rxt(k,527)*y(k,130)
         mat(k,1324) = .750_r8*rxt(k,679)*y(k,62)
         mat(k,2542) = (rxt(k,401)+rxt(k,402))*y(k,65)
         mat(k,2609) = .340_r8*rxt(k,670)*y(k,8) + rxt(k,438)*y(k,31)  &
                      + .500_r8*rxt(k,475)*y(k,36) + .910_r8*rxt(k,582)*y(k,115)  &
                      + .120_r8*rxt(k,518)*y(k,126) + .340_r8*rxt(k,675)*y(k,136)  &
                      + .600_r8*rxt(k,536)*y(k,138)
         mat(k,2469) = mat(k,2469) + .400_r8*rxt(k,545)*y(k,2) + .500_r8*rxt(k,459) &
                      *y(k,61) + rxt(k,390)*y(k,63) + .300_r8*rxt(k,391)*y(k,64)  &
                      + .800_r8*rxt(k,465)*y(k,89) + .300_r8*rxt(k,571)*y(k,118)  &
                      + .500_r8*rxt(k,535)*y(k,135) + rxt(k,467)*y(k,169)  &
                      + .680_r8*rxt(k,700)*y(k,213)
         mat(k,621) = rxt(k,467)*y(k,167)
         mat(k,953) = rxt(k,490)*y(k,151)
         mat(k,1412) = .800_r8*rxt(k,493)*y(k,62) + .150_r8*rxt(k,494)*y(k,105)  &
                      + rxt(k,495)*y(k,151)
         mat(k,1395) = .930_r8*rxt(k,686)*y(k,62) + .340_r8*rxt(k,688)*y(k,151)
         mat(k,1145) = .950_r8*rxt(k,692)*y(k,62) + .320_r8*rxt(k,694)*y(k,151)
         mat(k,1335) = .680_r8*rxt(k,700)*y(k,167)
         mat(k,1487) = .250_r8*rxt(k,600)*y(k,57) + .800_r8*rxt(k,601)*y(k,62)  &
                      + .250_r8*rxt(k,603)*y(k,151)

         mat(k,713) = -(rxt(k,359)*y(k,67) + rxt(k,360)*y(k,167) + rxt(k,370)*y(k,162))
         mat(k,2242) = -rxt(k,359)*y(k,52)
         mat(k,2390) = -rxt(k,360)*y(k,52)
         mat(k,2529) = -rxt(k,370)*y(k,52)

         mat(k,125) = -(rxt(k,361)*y(k,167))
         mat(k,2308) = -rxt(k,361)*y(k,53)

         mat(k,1358) = -(rxt(k,449)*y(k,153) + rxt(k,450)*y(k,167))
         mat(k,2134) = -rxt(k,449)*y(k,54)
         mat(k,2441) = -rxt(k,450)*y(k,54)

         mat(k,801) = .800_r8*rxt(k,545)*y(k,167)
         mat(k,1159) = .400_r8*rxt(k,547)*y(k,151)
         mat(k,561) = rxt(k,501)*y(k,153)
         mat(k,1035) = 3.200_r8*rxt(k,440)*y(k,32) + .800_r8*rxt(k,441)*y(k,62)  &
                      + rxt(k,443)*y(k,151)
         mat(k,299) = rxt(k,445)*y(k,167)
         mat(k,419) = .500_r8*rxt(k,446)*y(k,167)
         mat(k,1292) = .500_r8*rxt(k,475)*y(k,163)
         mat(k,995) = .270_r8*rxt(k,480)*y(k,151)
         mat(k,1850) = .800_r8*rxt(k,441)*y(k,32)
         mat(k,836) = rxt(k,504)*y(k,151)
         mat(k,2069) = .200_r8*rxt(k,530)*y(k,133)
         mat(k,861) = .200_r8*rxt(k,530)*y(k,105) + rxt(k,531)*y(k,151)
         mat(k,1667) = .100_r8*rxt(k,536)*y(k,163)
         mat(k,1950) = .400_r8*rxt(k,547)*y(k,3) + rxt(k,443)*y(k,32)  &
                      + .270_r8*rxt(k,480)*y(k,37) + rxt(k,504)*y(k,84) + rxt(k,531) &
                      *y(k,133) + rxt(k,490)*y(k,175)
         mat(k,2134) = mat(k,2134) + rxt(k,501)*y(k,21)
         mat(k,2584) = .500_r8*rxt(k,475)*y(k,36) + .100_r8*rxt(k,536)*y(k,138)
         mat(k,2441) = mat(k,2441) + .800_r8*rxt(k,545)*y(k,2) + rxt(k,445)*y(k,33)  &
                      + .500_r8*rxt(k,446)*y(k,34)
         mat(k,949) = rxt(k,490)*y(k,151)

         mat(k,449) = -(rxt(k,362)*y(k,67) + rxt(k,363)*y(k,167))
         mat(k,2235) = -rxt(k,362)*y(k,55)
         mat(k,2356) = -rxt(k,363)*y(k,55)

         mat(k,109) = -(rxt(k,452)*y(k,167))
         mat(k,2305) = -rxt(k,452)*y(k,56)

         mat(k,1739) = -(4._r8*rxt(k,453)*y(k,57) + rxt(k,454)*y(k,62) + rxt(k,455) &
                      *y(k,105) + rxt(k,456)*y(k,151) + rxt(k,469)*y(k,152) + rxt(k,509) &
                      *y(k,127) + rxt(k,556)*y(k,116) + rxt(k,563)*y(k,117) + rxt(k,574) &
                      *y(k,120) + rxt(k,600)*y(k,244))
         mat(k,1865) = -rxt(k,454)*y(k,57)
         mat(k,2085) = -rxt(k,455)*y(k,57)
         mat(k,1966) = -rxt(k,456)*y(k,57)
         mat(k,2652) = -rxt(k,469)*y(k,57)
         mat(k,1650) = -rxt(k,509)*y(k,57)
         mat(k,1584) = -rxt(k,556)*y(k,57)
         mat(k,1623) = -rxt(k,563)*y(k,57)
         mat(k,1507) = -rxt(k,574)*y(k,57)
         mat(k,1481) = -rxt(k,600)*y(k,57)

         mat(k,1208) = .060_r8*rxt(k,670)*y(k,163)
         mat(k,1360) = rxt(k,449)*y(k,153) + rxt(k,450)*y(k,167)
         mat(k,1739) = mat(k,1739) + .530_r8*rxt(k,509)*y(k,127)
         mat(k,1539) = rxt(k,484)*y(k,153) + rxt(k,485)*y(k,167)
         mat(k,724) = .500_r8*rxt(k,459)*y(k,167)
         mat(k,1865) = mat(k,1865) + .260_r8*rxt(k,510)*y(k,127) + rxt(k,523)*y(k,130)  &
                      + .300_r8*rxt(k,493)*y(k,179)
         mat(k,2085) = mat(k,2085) + .450_r8*rxt(k,524)*y(k,130) + .200_r8*rxt(k,530) &
                      *y(k,133) + .150_r8*rxt(k,494)*y(k,179)
         mat(k,1060) = .080_r8*rxt(k,582)*y(k,163)
         mat(k,1526) = .100_r8*rxt(k,518)*y(k,163)
         mat(k,1650) = mat(k,1650) + .530_r8*rxt(k,509)*y(k,57) + .260_r8*rxt(k,510) &
                      *y(k,62) + .530_r8*rxt(k,513)*y(k,151) + .530_r8*rxt(k,512) &
                      *y(k,153)
         mat(k,1700) = rxt(k,523)*y(k,62) + .450_r8*rxt(k,524)*y(k,105)  &
                      + 4.000_r8*rxt(k,525)*y(k,130) + rxt(k,526)*y(k,151)  &
                      + rxt(k,527)*y(k,153)
         mat(k,862) = .200_r8*rxt(k,530)*y(k,105) + rxt(k,531)*y(k,151)
         mat(k,1268) = .060_r8*rxt(k,675)*y(k,163)
         mat(k,1674) = .280_r8*rxt(k,536)*y(k,163)
         mat(k,1966) = mat(k,1966) + .530_r8*rxt(k,513)*y(k,127) + rxt(k,526)*y(k,130)  &
                      + rxt(k,531)*y(k,133) + rxt(k,495)*y(k,179)
         mat(k,2151) = rxt(k,449)*y(k,54) + rxt(k,484)*y(k,59) + .530_r8*rxt(k,512) &
                      *y(k,127) + rxt(k,527)*y(k,130)
         mat(k,2599) = .060_r8*rxt(k,670)*y(k,8) + .080_r8*rxt(k,582)*y(k,115)  &
                      + .100_r8*rxt(k,518)*y(k,126) + .060_r8*rxt(k,675)*y(k,136)  &
                      + .280_r8*rxt(k,536)*y(k,138)
         mat(k,2459) = rxt(k,450)*y(k,54) + rxt(k,485)*y(k,59) + .500_r8*rxt(k,459) &
                      *y(k,61) + .650_r8*rxt(k,700)*y(k,213)
         mat(k,1408) = .300_r8*rxt(k,493)*y(k,62) + .150_r8*rxt(k,494)*y(k,105)  &
                      + rxt(k,495)*y(k,151)
         mat(k,1332) = .650_r8*rxt(k,700)*y(k,167)

         mat(k,1167) = -(rxt(k,499)*y(k,167))
         mat(k,2431) = -rxt(k,499)*y(k,58)

         mat(k,800) = .800_r8*rxt(k,545)*y(k,167)
         mat(k,1158) = .250_r8*rxt(k,547)*y(k,151)
         mat(k,1197) = .520_r8*rxt(k,670)*y(k,163)
         mat(k,560) = .500_r8*rxt(k,501)*y(k,153)
         mat(k,994) = .820_r8*rxt(k,478)*y(k,62) + .820_r8*rxt(k,480)*y(k,151)
         mat(k,1844) = .820_r8*rxt(k,478)*y(k,37) + .150_r8*rxt(k,686)*y(k,207)  &
                      + .025_r8*rxt(k,692)*y(k,210)
         mat(k,835) = .500_r8*rxt(k,504)*y(k,151)
         mat(k,1256) = .520_r8*rxt(k,675)*y(k,163)
         mat(k,1943) = .250_r8*rxt(k,547)*y(k,3) + .820_r8*rxt(k,480)*y(k,37)  &
                      + .500_r8*rxt(k,504)*y(k,84) + .270_r8*rxt(k,688)*y(k,207)  &
                      + .040_r8*rxt(k,694)*y(k,210)
         mat(k,2124) = .500_r8*rxt(k,501)*y(k,21)
         mat(k,2575) = .520_r8*rxt(k,670)*y(k,8) + .520_r8*rxt(k,675)*y(k,136)
         mat(k,2431) = mat(k,2431) + .800_r8*rxt(k,545)*y(k,2) + .500_r8*rxt(k,700) &
                      *y(k,213)
         mat(k,1384) = .150_r8*rxt(k,686)*y(k,62) + .270_r8*rxt(k,688)*y(k,151)
         mat(k,1139) = .025_r8*rxt(k,692)*y(k,62) + .040_r8*rxt(k,694)*y(k,151)
         mat(k,1328) = .500_r8*rxt(k,700)*y(k,167)

      end do

      end subroutine     nlnmat02

      subroutine     nlnmat03( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,1538) = -(rxt(k,484)*y(k,153) + rxt(k,485)*y(k,167))
         mat(k,2145) = -rxt(k,484)*y(k,59)
         mat(k,2452) = -rxt(k,485)*y(k,59)

         mat(k,1733) = .250_r8*rxt(k,509)*y(k,127) + .250_r8*rxt(k,600)*y(k,244)
         mat(k,1859) = .240_r8*rxt(k,510)*y(k,127) + .500_r8*rxt(k,493)*y(k,179)  &
                      + .100_r8*rxt(k,601)*y(k,244)
         mat(k,939) = .070_r8*rxt(k,626)*y(k,105) + .170_r8*rxt(k,627)*y(k,151)
         mat(k,2078) = .070_r8*rxt(k,626)*y(k,78) + .070_r8*rxt(k,636)*y(k,131)
         mat(k,1417) = rxt(k,487)*y(k,167)
         mat(k,1618) = .050_r8*rxt(k,567)*y(k,151) + .050_r8*rxt(k,568)*y(k,153)
         mat(k,1523) = .880_r8*rxt(k,518)*y(k,163)
         mat(k,1647) = .250_r8*rxt(k,509)*y(k,57) + .240_r8*rxt(k,510)*y(k,62)  &
                      + .250_r8*rxt(k,513)*y(k,151) + .250_r8*rxt(k,512)*y(k,153)
         mat(k,1111) = .070_r8*rxt(k,636)*y(k,105) + .170_r8*rxt(k,637)*y(k,151)
         mat(k,1670) = .500_r8*rxt(k,536)*y(k,163)
         mat(k,1960) = .170_r8*rxt(k,627)*y(k,78) + .050_r8*rxt(k,567)*y(k,117)  &
                      + .250_r8*rxt(k,513)*y(k,127) + .170_r8*rxt(k,637)*y(k,131)  &
                      + .400_r8*rxt(k,653)*y(k,214) + .250_r8*rxt(k,603)*y(k,244)  &
                      + .540_r8*rxt(k,661)*y(k,252) + .510_r8*rxt(k,665)*y(k,256)
         mat(k,2145) = mat(k,2145) + .050_r8*rxt(k,568)*y(k,117) + .250_r8*rxt(k,512) &
                      *y(k,127) + .250_r8*rxt(k,604)*y(k,244)
         mat(k,1004) = rxt(k,488)*y(k,167)
         mat(k,2593) = .880_r8*rxt(k,518)*y(k,126) + .500_r8*rxt(k,536)*y(k,138)
         mat(k,2452) = mat(k,2452) + rxt(k,487)*y(k,112) + rxt(k,488)*y(k,154)
         mat(k,1407) = .500_r8*rxt(k,493)*y(k,62)
         mat(k,899) = .400_r8*rxt(k,653)*y(k,151)
         mat(k,1480) = .250_r8*rxt(k,600)*y(k,57) + .100_r8*rxt(k,601)*y(k,62)  &
                      + .250_r8*rxt(k,603)*y(k,151) + .250_r8*rxt(k,604)*y(k,153)
         mat(k,929) = .540_r8*rxt(k,661)*y(k,151)
         mat(k,751) = .510_r8*rxt(k,665)*y(k,151)

         mat(k,807) = -(rxt(k,458)*y(k,167))
         mat(k,2400) = -rxt(k,458)*y(k,60)

         mat(k,1283) = .120_r8*rxt(k,475)*y(k,163)
         mat(k,1719) = .100_r8*rxt(k,454)*y(k,62) + .150_r8*rxt(k,455)*y(k,105)
         mat(k,1833) = .100_r8*rxt(k,454)*y(k,57)
         mat(k,2037) = .150_r8*rxt(k,455)*y(k,57) + .150_r8*rxt(k,524)*y(k,130)
         mat(k,1691) = .150_r8*rxt(k,524)*y(k,105)
         mat(k,2564) = .120_r8*rxt(k,475)*y(k,36)

         mat(k,722) = -(rxt(k,459)*y(k,167))
         mat(k,2391) = -rxt(k,459)*y(k,61)

         mat(k,1718) = .400_r8*rxt(k,455)*y(k,105)
         mat(k,2031) = .400_r8*rxt(k,455)*y(k,57) + .400_r8*rxt(k,524)*y(k,130)
         mat(k,1690) = .400_r8*rxt(k,524)*y(k,105)

         mat(k,1869) = -(rxt(k,267)*y(k,70) + (4._r8*rxt(k,385) + 4._r8*rxt(k,386) &
                      ) * y(k,62) + rxt(k,387)*y(k,105) + rxt(k,388)*y(k,151) &
                      + rxt(k,441)*y(k,32) + rxt(k,454)*y(k,57) + rxt(k,478)*y(k,37) &
                      + rxt(k,493)*y(k,179) + rxt(k,510)*y(k,127) + rxt(k,523) &
                      *y(k,130) + rxt(k,557)*y(k,116) + rxt(k,564)*y(k,117) + rxt(k,575) &
                      *y(k,120) + rxt(k,601)*y(k,244) + rxt(k,679)*y(k,155) + rxt(k,686) &
                      *y(k,207) + rxt(k,692)*y(k,210))
         mat(k,2188) = -rxt(k,267)*y(k,62)
         mat(k,2090) = -rxt(k,387)*y(k,62)
         mat(k,1971) = -rxt(k,388)*y(k,62)
         mat(k,1036) = -rxt(k,441)*y(k,62)
         mat(k,1743) = -rxt(k,454)*y(k,62)
         mat(k,996) = -rxt(k,478)*y(k,62)
         mat(k,1409) = -rxt(k,493)*y(k,62)
         mat(k,1653) = -rxt(k,510)*y(k,62)
         mat(k,1703) = -rxt(k,523)*y(k,62)
         mat(k,1587) = -rxt(k,557)*y(k,62)
         mat(k,1626) = -rxt(k,564)*y(k,62)
         mat(k,1510) = -rxt(k,575)*y(k,62)
         mat(k,1483) = -rxt(k,601)*y(k,62)
         mat(k,1320) = -rxt(k,679)*y(k,62)
         mat(k,1392) = -rxt(k,686)*y(k,62)
         mat(k,1142) = -rxt(k,692)*y(k,62)

         mat(k,1298) = .280_r8*rxt(k,475)*y(k,163)
         mat(k,1743) = mat(k,1743) + 4.000_r8*rxt(k,453)*y(k,57) + .900_r8*rxt(k,454) &
                      *y(k,62) + .450_r8*rxt(k,455)*y(k,105) + rxt(k,556)*y(k,116)  &
                      + rxt(k,563)*y(k,117) + rxt(k,574)*y(k,120) + rxt(k,509) &
                      *y(k,127) + rxt(k,522)*y(k,130) + rxt(k,456)*y(k,151)  &
                      + rxt(k,600)*y(k,244)
         mat(k,809) = rxt(k,458)*y(k,167)
         mat(k,1869) = mat(k,1869) + .900_r8*rxt(k,454)*y(k,57)
         mat(k,524) = .700_r8*rxt(k,391)*y(k,167)
         mat(k,1777) = rxt(k,261)*y(k,67) + rxt(k,348)*y(k,88) + rxt(k,400)*y(k,162)  &
                      + rxt(k,392)*y(k,167)
         mat(k,2263) = rxt(k,261)*y(k,65)
         mat(k,940) = .330_r8*rxt(k,626)*y(k,105) + .830_r8*rxt(k,627)*y(k,151)
         mat(k,1126) = rxt(k,348)*y(k,65)
         mat(k,2090) = mat(k,2090) + .450_r8*rxt(k,455)*y(k,57) + .330_r8*rxt(k,626) &
                      *y(k,78) + .070_r8*rxt(k,636)*y(k,131)
         mat(k,1061) = .050_r8*rxt(k,582)*y(k,163)
         mat(k,1587) = mat(k,1587) + rxt(k,556)*y(k,57)
         mat(k,1626) = mat(k,1626) + rxt(k,563)*y(k,57)
         mat(k,1510) = mat(k,1510) + rxt(k,574)*y(k,57)
         mat(k,1653) = mat(k,1653) + rxt(k,509)*y(k,57)
         mat(k,1703) = mat(k,1703) + rxt(k,522)*y(k,57)
         mat(k,1112) = .070_r8*rxt(k,636)*y(k,105) + .170_r8*rxt(k,637)*y(k,151)
         mat(k,1971) = mat(k,1971) + rxt(k,456)*y(k,57) + .830_r8*rxt(k,627)*y(k,78)  &
                      + .170_r8*rxt(k,637)*y(k,131)
         mat(k,2537) = rxt(k,400)*y(k,65)
         mat(k,2604) = .280_r8*rxt(k,475)*y(k,36) + .050_r8*rxt(k,582)*y(k,115)
         mat(k,2464) = rxt(k,458)*y(k,60) + .700_r8*rxt(k,391)*y(k,64) + rxt(k,392) &
                      *y(k,65)
         mat(k,1483) = mat(k,1483) + rxt(k,600)*y(k,57)

         mat(k,912) = -(rxt(k,390)*y(k,167))
         mat(k,2409) = -rxt(k,390)*y(k,63)

         mat(k,1031) = .300_r8*rxt(k,441)*y(k,62)
         mat(k,1834) = .300_r8*rxt(k,441)*y(k,32) + 2.000_r8*rxt(k,386)*y(k,62)  &
                      + .250_r8*rxt(k,557)*y(k,116) + .250_r8*rxt(k,564)*y(k,117)  &
                      + .200_r8*rxt(k,575)*y(k,120) + .250_r8*rxt(k,510)*y(k,127)  &
                      + .250_r8*rxt(k,679)*y(k,155) + .500_r8*rxt(k,493)*y(k,179)  &
                      + .250_r8*rxt(k,686)*y(k,207) + .250_r8*rxt(k,692)*y(k,210)  &
                      + .300_r8*rxt(k,601)*y(k,244)
         mat(k,1567) = .250_r8*rxt(k,557)*y(k,62)
         mat(k,1604) = .250_r8*rxt(k,564)*y(k,62)
         mat(k,1494) = .200_r8*rxt(k,575)*y(k,62)
         mat(k,1638) = .250_r8*rxt(k,510)*y(k,62)
         mat(k,1310) = .250_r8*rxt(k,679)*y(k,62)
         mat(k,1401) = .500_r8*rxt(k,493)*y(k,62)
         mat(k,1378) = .250_r8*rxt(k,686)*y(k,62)
         mat(k,1133) = .250_r8*rxt(k,692)*y(k,62)
         mat(k,1470) = .300_r8*rxt(k,601)*y(k,62)

         mat(k,522) = -(rxt(k,391)*y(k,167))
         mat(k,2366) = -rxt(k,391)*y(k,64)

         mat(k,1830) = rxt(k,387)*y(k,105)
         mat(k,2018) = rxt(k,387)*y(k,62)

         mat(k,1774) = -(rxt(k,261)*y(k,67) + rxt(k,348)*y(k,88) + rxt(k,392)*y(k,167) &
                      + (rxt(k,400) + rxt(k,401) + rxt(k,402)) * y(k,162))
         mat(k,2260) = -rxt(k,261)*y(k,65)
         mat(k,1123) = -rxt(k,348)*y(k,65)
         mat(k,2461) = -rxt(k,392)*y(k,65)
         mat(k,2534) = -(rxt(k,400) + rxt(k,401) + rxt(k,402)) * y(k,65)

         mat(k,1297) = .100_r8*rxt(k,475)*y(k,163)
         mat(k,2601) = .100_r8*rxt(k,475)*y(k,36)

         mat(k,538) = -(rxt(k,338)*y(k,162) + rxt(k,364)*y(k,67) + rxt(k,365)*y(k,167))
         mat(k,2525) = -rxt(k,338)*y(k,66)
         mat(k,2236) = -rxt(k,364)*y(k,66)
         mat(k,2369) = -rxt(k,365)*y(k,66)

         mat(k,2269) = -(rxt(k,260)*y(k,51) + rxt(k,261)*y(k,65) + rxt(k,262)*y(k,92) &
                      + rxt(k,263)*y(k,94) + (rxt(k,264) + rxt(k,265)) * y(k,105) &
                      + rxt(k,266)*y(k,163) + rxt(k,273)*y(k,71) + rxt(k,288)*y(k,109) &
                      + rxt(k,357)*y(k,50) + rxt(k,359)*y(k,52) + rxt(k,362)*y(k,55) &
                      + rxt(k,364)*y(k,66) + rxt(k,447)*y(k,35))
         mat(k,2223) = -rxt(k,260)*y(k,67)
         mat(k,1781) = -rxt(k,261)*y(k,67)
         mat(k,1557) = -rxt(k,262)*y(k,67)
         mat(k,732) = -rxt(k,263)*y(k,67)
         mat(k,2096) = -(rxt(k,264) + rxt(k,265)) * y(k,67)
         mat(k,2610) = -rxt(k,266)*y(k,67)
         mat(k,1023) = -rxt(k,273)*y(k,67)
         mat(k,972) = -rxt(k,288)*y(k,67)
         mat(k,585) = -rxt(k,357)*y(k,67)
         mat(k,717) = -rxt(k,359)*y(k,67)
         mat(k,453) = -rxt(k,362)*y(k,67)
         mat(k,541) = -rxt(k,364)*y(k,67)
         mat(k,356) = -rxt(k,447)*y(k,67)

         mat(k,2499) = rxt(k,320)*y(k,70)
         mat(k,123) = 4.000_r8*rxt(k,292)*y(k,162)
         mat(k,172) = rxt(k,293)*y(k,162)
         mat(k,151) = 2.000_r8*rxt(k,294)*y(k,162)
         mat(k,184) = 2.000_r8*rxt(k,295)*y(k,162)
         mat(k,156) = 2.000_r8*rxt(k,296)*y(k,162)
         mat(k,190) = rxt(k,297)*y(k,162)
         mat(k,161) = 2.000_r8*rxt(k,298)*y(k,162)
         mat(k,126) = 3.000_r8*rxt(k,361)*y(k,167)
         mat(k,453) = mat(k,453) + rxt(k,363)*y(k,167)
         mat(k,1875) = rxt(k,267)*y(k,70)
         mat(k,2194) = rxt(k,320)*y(k,24) + rxt(k,267)*y(k,62) + (4.000_r8*rxt(k,268) &
                       +2.000_r8*rxt(k,270))*y(k,70) + rxt(k,272)*y(k,151)  &
                      + rxt(k,278)*y(k,161) + rxt(k,279)*y(k,167) + rxt(k,715) &
                      *y(k,183)
         mat(k,381) = rxt(k,354)*y(k,162)
         mat(k,375) = rxt(k,371)*y(k,162) + rxt(k,366)*y(k,167)
         mat(k,335) = rxt(k,372)*y(k,162) + rxt(k,367)*y(k,167)
         mat(k,403) = rxt(k,373)*y(k,162) + rxt(k,368)*y(k,167)
         mat(k,2692) = rxt(k,285)*y(k,161) + rxt(k,299)*y(k,162) + rxt(k,286)*y(k,167)
         mat(k,1977) = rxt(k,272)*y(k,70)
         mat(k,2731) = rxt(k,278)*y(k,70) + rxt(k,285)*y(k,100)
         mat(k,2543) = 4.000_r8*rxt(k,292)*y(k,42) + rxt(k,293)*y(k,43)  &
                      + 2.000_r8*rxt(k,294)*y(k,45) + 2.000_r8*rxt(k,295)*y(k,46)  &
                      + 2.000_r8*rxt(k,296)*y(k,47) + rxt(k,297)*y(k,48)  &
                      + 2.000_r8*rxt(k,298)*y(k,49) + rxt(k,354)*y(k,76) + rxt(k,371) &
                      *y(k,97) + rxt(k,372)*y(k,98) + rxt(k,373)*y(k,99) + rxt(k,299) &
                      *y(k,100)
         mat(k,2470) = 3.000_r8*rxt(k,361)*y(k,53) + rxt(k,363)*y(k,55) + rxt(k,279) &
                      *y(k,70) + rxt(k,366)*y(k,97) + rxt(k,367)*y(k,98) + rxt(k,368) &
                      *y(k,99) + rxt(k,286)*y(k,100)
         mat(k,1462) = rxt(k,715)*y(k,70)


         mat(k,2232) = rxt(k,273)*y(k,71)
         mat(k,2172) = 2.000_r8*rxt(k,269)*y(k,70)
         mat(k,1016) = rxt(k,273)*y(k,67) + (rxt(k,790)+rxt(k,795)+rxt(k,800)) &
                      *y(k,100)
         mat(k,2672) = (rxt(k,790)+rxt(k,795)+rxt(k,800))*y(k,71) + (rxt(k,785) &
                       +rxt(k,791)+rxt(k,796))*y(k,109)
         mat(k,968) = (rxt(k,785)+rxt(k,791)+rxt(k,796))*y(k,100)


         mat(k,2171) = 2.000_r8*rxt(k,301)*y(k,70)

         mat(k,2192) = -(rxt(k,267)*y(k,62) + (4._r8*rxt(k,268) + 4._r8*rxt(k,269) &
                      + 4._r8*rxt(k,270) + 4._r8*rxt(k,301)) * y(k,70) + rxt(k,271) &
                      *y(k,105) + rxt(k,272)*y(k,151) + rxt(k,274)*y(k,152) + rxt(k,278) &
                      *y(k,161) + (rxt(k,279) + rxt(k,280)) * y(k,167) + (rxt(k,319) &
                      + rxt(k,320) + rxt(k,321)) * y(k,24) + rxt(k,715)*y(k,183))
         mat(k,1873) = -rxt(k,267)*y(k,70)
         mat(k,2094) = -rxt(k,271)*y(k,70)
         mat(k,1975) = -rxt(k,272)*y(k,70)
         mat(k,2661) = -rxt(k,274)*y(k,70)
         mat(k,2729) = -rxt(k,278)*y(k,70)
         mat(k,2468) = -(rxt(k,279) + rxt(k,280)) * y(k,70)
         mat(k,2497) = -(rxt(k,319) + rxt(k,320) + rxt(k,321)) * y(k,70)
         mat(k,1461) = -rxt(k,715)*y(k,70)

         mat(k,2267) = rxt(k,265)*y(k,105) + rxt(k,288)*y(k,109) + rxt(k,266)*y(k,163)
         mat(k,1022) = rxt(k,275)*y(k,161)
         mat(k,2690) = rxt(k,300)*y(k,162)
         mat(k,2094) = mat(k,2094) + rxt(k,265)*y(k,67)
         mat(k,971) = rxt(k,288)*y(k,67) + rxt(k,289)*y(k,161) + rxt(k,290)*y(k,167)
         mat(k,2729) = mat(k,2729) + rxt(k,275)*y(k,71) + rxt(k,289)*y(k,109)
         mat(k,2541) = rxt(k,300)*y(k,100)
         mat(k,2608) = rxt(k,266)*y(k,67)
         mat(k,410) = rxt(k,720)*y(k,183)
         mat(k,2468) = mat(k,2468) + rxt(k,290)*y(k,109)
         mat(k,1461) = mat(k,1461) + rxt(k,720)*y(k,165)

      end do

      end subroutine     nlnmat03

      subroutine     nlnmat04( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,1019) = -(rxt(k,273)*y(k,67) + rxt(k,275)*y(k,161) + rxt(k,276) &
                      *y(k,167) + (rxt(k,790) + rxt(k,795) + rxt(k,800)) * y(k,100))
         mat(k,2246) = -rxt(k,273)*y(k,71)
         mat(k,2713) = -rxt(k,275)*y(k,71)
         mat(k,2420) = -rxt(k,276)*y(k,71)
         mat(k,2679) = -(rxt(k,790) + rxt(k,795) + rxt(k,800)) * y(k,71)

         mat(k,2179) = rxt(k,274)*y(k,152)
         mat(k,2640) = rxt(k,274)*y(k,70)


         mat(k,1443) = -((rxt(k,393) + rxt(k,404)) * y(k,167))
         mat(k,2447) = -(rxt(k,393) + rxt(k,404)) * y(k,73)

         mat(k,1206) = .230_r8*rxt(k,670)*y(k,163)
         mat(k,1756) = rxt(k,315)*y(k,51)
         mat(k,351) = .350_r8*rxt(k,436)*y(k,167)
         mat(k,611) = .630_r8*rxt(k,438)*y(k,163)
         mat(k,1294) = .560_r8*rxt(k,475)*y(k,163)
         mat(k,2211) = rxt(k,315)*y(k,22) + rxt(k,260)*y(k,67) + rxt(k,380)*y(k,153)  &
                      + rxt(k,381)*y(k,161) + rxt(k,382)*y(k,167)
         mat(k,450) = rxt(k,362)*y(k,67)
         mat(k,1729) = .220_r8*rxt(k,509)*y(k,127) + .250_r8*rxt(k,600)*y(k,244)
         mat(k,1537) = rxt(k,484)*y(k,153) + rxt(k,485)*y(k,167)
         mat(k,1855) = .110_r8*rxt(k,510)*y(k,127) + .125_r8*rxt(k,686)*y(k,207)  &
                      + .200_r8*rxt(k,601)*y(k,244)
         mat(k,2256) = rxt(k,260)*y(k,51) + rxt(k,362)*y(k,55)
         mat(k,938) = .070_r8*rxt(k,626)*y(k,105) + .170_r8*rxt(k,627)*y(k,151)
         mat(k,1235) = rxt(k,466)*y(k,167)
         mat(k,2074) = .070_r8*rxt(k,626)*y(k,78) + .160_r8*rxt(k,631)*y(k,129)  &
                      + .140_r8*rxt(k,636)*y(k,131)
         mat(k,1054) = .620_r8*rxt(k,582)*y(k,163)
         mat(k,1521) = .650_r8*rxt(k,518)*y(k,163)
         mat(k,1646) = .220_r8*rxt(k,509)*y(k,57) + .110_r8*rxt(k,510)*y(k,62)  &
                      + .220_r8*rxt(k,513)*y(k,151) + .220_r8*rxt(k,512)*y(k,153)
         mat(k,908) = .160_r8*rxt(k,631)*y(k,105) + .400_r8*rxt(k,632)*y(k,151)
         mat(k,1110) = .140_r8*rxt(k,636)*y(k,105) + .350_r8*rxt(k,637)*y(k,151)
         mat(k,1266) = .230_r8*rxt(k,675)*y(k,163)
         mat(k,1669) = .560_r8*rxt(k,536)*y(k,163)
         mat(k,1956) = .170_r8*rxt(k,627)*y(k,78) + .220_r8*rxt(k,513)*y(k,127)  &
                      + .400_r8*rxt(k,632)*y(k,129) + .350_r8*rxt(k,637)*y(k,131)  &
                      + .225_r8*rxt(k,688)*y(k,207) + .250_r8*rxt(k,603)*y(k,244)
         mat(k,2140) = rxt(k,380)*y(k,51) + rxt(k,484)*y(k,59) + .220_r8*rxt(k,512) &
                      *y(k,127) + .500_r8*rxt(k,604)*y(k,244)
         mat(k,2718) = rxt(k,381)*y(k,51) + rxt(k,710)*y(k,166)
         mat(k,2589) = .230_r8*rxt(k,670)*y(k,8) + .630_r8*rxt(k,438)*y(k,31)  &
                      + .560_r8*rxt(k,475)*y(k,36) + .620_r8*rxt(k,582)*y(k,115)  &
                      + .650_r8*rxt(k,518)*y(k,126) + .230_r8*rxt(k,675)*y(k,136)  &
                      + .560_r8*rxt(k,536)*y(k,138)
         mat(k,444) = rxt(k,710)*y(k,161) + rxt(k,711)*y(k,167)
         mat(k,2447) = mat(k,2447) + .350_r8*rxt(k,436)*y(k,30) + rxt(k,382)*y(k,51)  &
                      + rxt(k,485)*y(k,59) + rxt(k,466)*y(k,90) + rxt(k,711)*y(k,166)  &
                      + .700_r8*rxt(k,700)*y(k,213)
         mat(k,1389) = .125_r8*rxt(k,686)*y(k,62) + .225_r8*rxt(k,688)*y(k,151)
         mat(k,1331) = .700_r8*rxt(k,700)*y(k,167)
         mat(k,1478) = .250_r8*rxt(k,600)*y(k,57) + .200_r8*rxt(k,601)*y(k,62)  &
                      + .250_r8*rxt(k,603)*y(k,151) + .500_r8*rxt(k,604)*y(k,153)

         mat(k,6) = -((rxt(k,405) + rxt(k,406)) * y(k,167))
         mat(k,2278) = -(rxt(k,405) + rxt(k,406)) * y(k,261)

         mat(k,7) = -((rxt(k,407) + rxt(k,408)) * y(k,167))
         mat(k,2279) = -(rxt(k,407) + rxt(k,408)) * y(k,262)

         mat(k,8) = -((rxt(k,409) + rxt(k,410)) * y(k,167))
         mat(k,2280) = -(rxt(k,409) + rxt(k,410)) * y(k,263)

         mat(k,9) = -((rxt(k,411) + rxt(k,412)) * y(k,167))
         mat(k,2281) = -(rxt(k,411) + rxt(k,412)) * y(k,264)

         mat(k,10) = -((rxt(k,413) + rxt(k,414)) * y(k,167))
         mat(k,2282) = -(rxt(k,413) + rxt(k,414)) * y(k,265)

         mat(k,11) = -((rxt(k,415) + rxt(k,416)) * y(k,167))
         mat(k,2283) = -(rxt(k,415) + rxt(k,416)) * y(k,266)

         mat(k,12) = -((rxt(k,417) + rxt(k,418)) * y(k,167))
         mat(k,2284) = -(rxt(k,417) + rxt(k,418)) * y(k,267)

         mat(k,13) = -((rxt(k,419) + rxt(k,420)) * y(k,167))
         mat(k,2285) = -(rxt(k,419) + rxt(k,420)) * y(k,268)

         mat(k,14) = -((rxt(k,421) + rxt(k,422)) * y(k,167))
         mat(k,2286) = -(rxt(k,421) + rxt(k,422)) * y(k,269)

         mat(k,15) = -((rxt(k,423) + rxt(k,424)) * y(k,167))
         mat(k,2287) = -(rxt(k,423) + rxt(k,424)) * y(k,270)

         mat(k,16) = -((rxt(k,425) + rxt(k,426)) * y(k,167))
         mat(k,2288) = -(rxt(k,425) + rxt(k,426)) * y(k,271)

         mat(k,17) = -((rxt(k,427) + rxt(k,428)) * y(k,167))
         mat(k,2289) = -(rxt(k,427) + rxt(k,428)) * y(k,272)

         mat(k,18) = -((rxt(k,429) + rxt(k,430)) * y(k,167))
         mat(k,2290) = -(rxt(k,429) + rxt(k,430)) * y(k,273)


         mat(k,1191) = .270_r8*rxt(k,670)*y(k,163)
         mat(k,1285) = .200_r8*rxt(k,475)*y(k,163)
         mat(k,1720) = 4.000_r8*rxt(k,453)*y(k,57) + .900_r8*rxt(k,454)*y(k,62)  &
                      + rxt(k,556)*y(k,116) + 2.000_r8*rxt(k,522)*y(k,130)  &
                      + rxt(k,456)*y(k,151) + rxt(k,600)*y(k,244)
         mat(k,808) = rxt(k,458)*y(k,167)
         mat(k,723) = .500_r8*rxt(k,459)*y(k,167)
         mat(k,1835) = .900_r8*rxt(k,454)*y(k,57) + rxt(k,523)*y(k,130)  &
                      + .500_r8*rxt(k,686)*y(k,207)
         mat(k,1442) = (rxt(k,393)+rxt(k,404))*y(k,167)
         mat(k,1367) = .800_r8*rxt(k,465)*y(k,167)
         mat(k,1233) = rxt(k,466)*y(k,167)
         mat(k,1173) = rxt(k,395)*y(k,167)
         mat(k,2051) = .450_r8*rxt(k,524)*y(k,130)
         mat(k,1568) = rxt(k,556)*y(k,57)
         mat(k,1692) = 2.000_r8*rxt(k,522)*y(k,57) + rxt(k,523)*y(k,62)  &
                      + .450_r8*rxt(k,524)*y(k,105) + 4.000_r8*rxt(k,525)*y(k,130)
         mat(k,676) = .500_r8*rxt(k,535)*y(k,167)
         mat(k,1250) = .270_r8*rxt(k,675)*y(k,163)
         mat(k,1664) = .100_r8*rxt(k,536)*y(k,163)
         mat(k,1932) = rxt(k,456)*y(k,57) + .900_r8*rxt(k,688)*y(k,207)
         mat(k,2567) = .270_r8*rxt(k,670)*y(k,8) + .200_r8*rxt(k,475)*y(k,36)  &
                      + .270_r8*rxt(k,675)*y(k,136) + .100_r8*rxt(k,536)*y(k,138)
         mat(k,2415) = rxt(k,458)*y(k,60) + .500_r8*rxt(k,459)*y(k,61) + (rxt(k,393) &
                       +rxt(k,404))*y(k,73) + .800_r8*rxt(k,465)*y(k,89) + rxt(k,466) &
                      *y(k,90) + rxt(k,395)*y(k,102) + .500_r8*rxt(k,535)*y(k,135)  &
                      + 1.800_r8*rxt(k,700)*y(k,213)
         mat(k,1379) = .500_r8*rxt(k,686)*y(k,62) + .900_r8*rxt(k,688)*y(k,151)
         mat(k,1327) = 1.800_r8*rxt(k,700)*y(k,167)
         mat(k,1471) = rxt(k,600)*y(k,57)

         mat(k,338) = -(rxt(k,353)*y(k,162))
         mat(k,2520) = -rxt(k,353)*y(k,75)

         mat(k,169) = rxt(k,293)*y(k,162)
         mat(k,175) = rxt(k,337)*y(k,162)
         mat(k,181) = rxt(k,295)*y(k,162)
         mat(k,154) = 2.000_r8*rxt(k,296)*y(k,162)
         mat(k,187) = 2.000_r8*rxt(k,297)*y(k,162)
         mat(k,159) = rxt(k,298)*y(k,162)
         mat(k,135) = 2.000_r8*rxt(k,339)*y(k,162)
         mat(k,333) = rxt(k,372)*y(k,162) + rxt(k,367)*y(k,167)
         mat(k,399) = rxt(k,373)*y(k,162) + rxt(k,368)*y(k,167)
         mat(k,2520) = mat(k,2520) + rxt(k,293)*y(k,43) + rxt(k,337)*y(k,44)  &
                      + rxt(k,295)*y(k,46) + 2.000_r8*rxt(k,296)*y(k,47)  &
                      + 2.000_r8*rxt(k,297)*y(k,48) + rxt(k,298)*y(k,49)  &
                      + 2.000_r8*rxt(k,339)*y(k,93) + rxt(k,372)*y(k,98) + rxt(k,373) &
                      *y(k,99)
         mat(k,2340) = rxt(k,367)*y(k,98) + rxt(k,368)*y(k,99)

         mat(k,378) = -(rxt(k,354)*y(k,162))
         mat(k,2522) = -rxt(k,354)*y(k,76)

         mat(k,149) = rxt(k,294)*y(k,162)
         mat(k,182) = rxt(k,295)*y(k,162)
         mat(k,373) = rxt(k,371)*y(k,162) + rxt(k,366)*y(k,167)
         mat(k,2522) = mat(k,2522) + rxt(k,294)*y(k,45) + rxt(k,295)*y(k,46)  &
                      + rxt(k,371)*y(k,97)
         mat(k,2344) = rxt(k,366)*y(k,97)

         mat(k,219) = -(rxt(k,625)*y(k,167))
         mat(k,2316) = -rxt(k,625)*y(k,77)

         mat(k,2316) = mat(k,2316) + .180_r8*rxt(k,656)*y(k,216)
         mat(k,213) = .180_r8*rxt(k,656)*y(k,167)

         mat(k,935) = -(rxt(k,626)*y(k,105) + rxt(k,627)*y(k,151) + rxt(k,628) &
                      *y(k,152))
         mat(k,2047) = -rxt(k,626)*y(k,78)
         mat(k,1929) = -rxt(k,627)*y(k,78)
         mat(k,2637) = -rxt(k,628)*y(k,78)

         mat(k,457) = -(rxt(k,707)*y(k,153) + (rxt(k,708) + rxt(k,725)) * y(k,167))
         mat(k,2111) = -rxt(k,707)*y(k,79)
         mat(k,2357) = -(rxt(k,708) + rxt(k,725)) * y(k,79)





         mat(k,831) = -((rxt(k,504) + rxt(k,505)) * y(k,151))
         mat(k,1923) = -(rxt(k,504) + rxt(k,505)) * y(k,84)

         mat(k,557) = rxt(k,502)*y(k,167)
         mat(k,2403) = rxt(k,502)*y(k,21)


         mat(k,958) = .750_r8*rxt(k,461)*y(k,151)
         mat(k,1898) = .750_r8*rxt(k,461)*y(k,86)

         mat(k,959) = -(rxt(k,460)*y(k,105) + rxt(k,461)*y(k,151))
         mat(k,2049) = -rxt(k,460)*y(k,86)
         mat(k,1931) = -rxt(k,461)*y(k,86)

         mat(k,609) = rxt(k,468)*y(k,167)
         mat(k,2413) = rxt(k,468)*y(k,31)


         mat(k,957) = rxt(k,460)*y(k,105)
         mat(k,1998) = rxt(k,460)*y(k,86)

         mat(k,1121) = -(rxt(k,348)*y(k,65) + rxt(k,349)*y(k,92) + rxt(k,350)*y(k,260) &
                      + rxt(k,351)*y(k,104))
         mat(k,1771) = -rxt(k,348)*y(k,88)
         mat(k,1550) = -rxt(k,349)*y(k,88)
         mat(k,2746) = -rxt(k,350)*y(k,88)
         mat(k,1805) = -rxt(k,351)*y(k,88)

         mat(k,177) = rxt(k,337)*y(k,162)
         mat(k,189) = rxt(k,297)*y(k,162)
         mat(k,340) = 2.000_r8*rxt(k,353)*y(k,162)
         mat(k,380) = rxt(k,354)*y(k,162)
         mat(k,2531) = rxt(k,337)*y(k,44) + rxt(k,297)*y(k,48) + 2.000_r8*rxt(k,353) &
                      *y(k,75) + rxt(k,354)*y(k,76)

         mat(k,1369) = -(rxt(k,465)*y(k,167))
         mat(k,2442) = -rxt(k,465)*y(k,89)

         mat(k,1727) = .530_r8*rxt(k,509)*y(k,127) + .250_r8*rxt(k,600)*y(k,244)
         mat(k,1851) = .260_r8*rxt(k,510)*y(k,127) + .125_r8*rxt(k,686)*y(k,207)  &
                      + .100_r8*rxt(k,601)*y(k,244)
         mat(k,1612) = .050_r8*rxt(k,567)*y(k,151) + .050_r8*rxt(k,568)*y(k,153)
         mat(k,738) = .700_r8*rxt(k,571)*y(k,167)
         mat(k,600) = .500_r8*rxt(k,572)*y(k,167)
         mat(k,1643) = .530_r8*rxt(k,509)*y(k,57) + .260_r8*rxt(k,510)*y(k,62)  &
                      + .530_r8*rxt(k,513)*y(k,151) + .530_r8*rxt(k,512)*y(k,153)
         mat(k,476) = rxt(k,587)*y(k,167)
         mat(k,1951) = .050_r8*rxt(k,567)*y(k,117) + .530_r8*rxt(k,513)*y(k,127)  &
                      + .225_r8*rxt(k,688)*y(k,207) + .250_r8*rxt(k,603)*y(k,244)
         mat(k,2135) = .050_r8*rxt(k,568)*y(k,117) + .530_r8*rxt(k,512)*y(k,127)  &
                      + .250_r8*rxt(k,604)*y(k,244)
         mat(k,2442) = mat(k,2442) + .700_r8*rxt(k,571)*y(k,118) + .500_r8*rxt(k,572) &
                      *y(k,119) + rxt(k,587)*y(k,142)
         mat(k,1386) = .125_r8*rxt(k,686)*y(k,62) + .225_r8*rxt(k,688)*y(k,151)
         mat(k,1476) = .250_r8*rxt(k,600)*y(k,57) + .100_r8*rxt(k,601)*y(k,62)  &
                      + .250_r8*rxt(k,603)*y(k,151) + .250_r8*rxt(k,604)*y(k,153)

         mat(k,1234) = -(rxt(k,466)*y(k,167))
         mat(k,2435) = -rxt(k,466)*y(k,90)

         mat(k,702) = rxt(k,613)*y(k,151)
         mat(k,350) = .650_r8*rxt(k,436)*y(k,167)
         mat(k,1725) = .250_r8*rxt(k,600)*y(k,244)
         mat(k,1846) = .100_r8*rxt(k,601)*y(k,244)
         mat(k,1368) = .200_r8*rxt(k,465)*y(k,167)
         mat(k,2066) = .160_r8*rxt(k,631)*y(k,129) + .070_r8*rxt(k,636)*y(k,131)
         mat(k,1611) = .050_r8*rxt(k,567)*y(k,151) + .050_r8*rxt(k,568)*y(k,153)
         mat(k,907) = .160_r8*rxt(k,631)*y(k,105) + .400_r8*rxt(k,632)*y(k,151)
         mat(k,1109) = .070_r8*rxt(k,636)*y(k,105) + .170_r8*rxt(k,637)*y(k,151)
         mat(k,1342) = rxt(k,588)*y(k,167)
         mat(k,1946) = rxt(k,613)*y(k,11) + .050_r8*rxt(k,567)*y(k,117)  &
                      + .400_r8*rxt(k,632)*y(k,129) + .170_r8*rxt(k,637)*y(k,131)  &
                      + .700_r8*rxt(k,642)*y(k,172) + .600_r8*rxt(k,653)*y(k,214)  &
                      + .250_r8*rxt(k,603)*y(k,244) + .340_r8*rxt(k,661)*y(k,252)  &
                      + .170_r8*rxt(k,665)*y(k,256)
         mat(k,2128) = .050_r8*rxt(k,568)*y(k,117) + .250_r8*rxt(k,604)*y(k,244)
         mat(k,2435) = mat(k,2435) + .650_r8*rxt(k,436)*y(k,30) + .200_r8*rxt(k,465) &
                      *y(k,89) + rxt(k,588)*y(k,143)
         mat(k,628) = .700_r8*rxt(k,642)*y(k,151)
         mat(k,897) = .600_r8*rxt(k,653)*y(k,151)
         mat(k,1475) = .250_r8*rxt(k,600)*y(k,57) + .100_r8*rxt(k,601)*y(k,62)  &
                      + .250_r8*rxt(k,603)*y(k,151) + .250_r8*rxt(k,604)*y(k,153)
         mat(k,927) = .340_r8*rxt(k,661)*y(k,151)
         mat(k,750) = .170_r8*rxt(k,665)*y(k,151)

      end do

      end subroutine     nlnmat04

      subroutine     nlnmat05( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,1791) = -((rxt(k,175) + rxt(k,176) + rxt(k,177)) * y(k,105) + rxt(k,183) &
                      *y(k,163))
         mat(k,2088) = -(rxt(k,175) + rxt(k,176) + rxt(k,177)) * y(k,91)
         mat(k,2602) = -rxt(k,183)*y(k,91)

         mat(k,2215) = rxt(k,382)*y(k,167)
         mat(k,1775) = rxt(k,401)*y(k,162)
         mat(k,2261) = rxt(k,262)*y(k,92)
         mat(k,1444) = rxt(k,404)*y(k,167)
         mat(k,1124) = rxt(k,349)*y(k,92)
         mat(k,1553) = rxt(k,262)*y(k,67) + rxt(k,349)*y(k,88) + rxt(k,171)*y(k,161)  &
                      + rxt(k,153)*y(k,162) + rxt(k,185)*y(k,167)
         mat(k,984) = rxt(k,341)*y(k,162)
         mat(k,2684) = rxt(k,300)*y(k,162)
         mat(k,690) = rxt(k,213)*y(k,167)
         mat(k,2723) = rxt(k,171)*y(k,92) + rxt(k,188)*y(k,167)
         mat(k,2535) = rxt(k,401)*y(k,65) + rxt(k,153)*y(k,92) + rxt(k,341)*y(k,96)  &
                      + rxt(k,300)*y(k,100)
         mat(k,446) = rxt(k,711)*y(k,167)
         mat(k,2462) = rxt(k,382)*y(k,51) + rxt(k,404)*y(k,73) + rxt(k,185)*y(k,92)  &
                      + rxt(k,213)*y(k,139) + rxt(k,188)*y(k,161) + rxt(k,711) &
                      *y(k,166) + rxt(k,716)*y(k,181) + rxt(k,721)*y(k,183)
         mat(k,594) = rxt(k,716)*y(k,167)
         mat(k,1458) = rxt(k,721)*y(k,167)

         mat(k,1551) = -(rxt(k,153)*y(k,162) + rxt(k,171)*y(k,161) + rxt(k,185) &
                      *y(k,167) + rxt(k,262)*y(k,67) + rxt(k,349)*y(k,88))
         mat(k,2532) = -rxt(k,153)*y(k,92)
         mat(k,2720) = -rxt(k,171)*y(k,92)
         mat(k,2453) = -rxt(k,185)*y(k,92)
         mat(k,2257) = -rxt(k,262)*y(k,92)
         mat(k,1122) = -rxt(k,349)*y(k,92)

         mat(k,1773) = rxt(k,402)*y(k,162)
         mat(k,1789) = rxt(k,175)*y(k,105)
         mat(k,2079) = rxt(k,175)*y(k,91)
         mat(k,2532) = mat(k,2532) + rxt(k,402)*y(k,65)

         mat(k,134) = -(rxt(k,339)*y(k,162))
         mat(k,2509) = -rxt(k,339)*y(k,93)

         mat(k,730) = -(rxt(k,172)*y(k,161) + rxt(k,186)*y(k,167) + rxt(k,263)*y(k,67))
         mat(k,2708) = -rxt(k,172)*y(k,94)
         mat(k,2392) = -rxt(k,186)*y(k,94)
         mat(k,2243) = -rxt(k,263)*y(k,94)

         mat(k,2032) = 2.000_r8*rxt(k,194)*y(k,105)
         mat(k,2392) = mat(k,2392) + 2.000_r8*rxt(k,191)*y(k,167)


         mat(k,302) = rxt(k,727)*y(k,260)
         mat(k,2740) = rxt(k,727)*y(k,185)

         mat(k,982) = -(rxt(k,332)*y(k,161) + rxt(k,333)*y(k,167) + (rxt(k,340) &
                      + rxt(k,341)) * y(k,162))
         mat(k,2711) = -rxt(k,332)*y(k,96)
         mat(k,2416) = -rxt(k,333)*y(k,96)
         mat(k,2530) = -(rxt(k,340) + rxt(k,341)) * y(k,96)

         mat(k,1755) = rxt(k,315)*y(k,51) + rxt(k,316)*y(k,105)
         mat(k,2205) = rxt(k,315)*y(k,22)
         mat(k,2052) = rxt(k,316)*y(k,22)

         mat(k,372) = -(rxt(k,366)*y(k,167) + rxt(k,371)*y(k,162))
         mat(k,2343) = -rxt(k,366)*y(k,97)
         mat(k,2521) = -rxt(k,371)*y(k,97)

         mat(k,332) = -(rxt(k,367)*y(k,167) + rxt(k,372)*y(k,162))
         mat(k,2339) = -rxt(k,367)*y(k,98)
         mat(k,2519) = -rxt(k,372)*y(k,98)

         mat(k,400) = -(rxt(k,368)*y(k,167) + rxt(k,373)*y(k,162))
         mat(k,2348) = -rxt(k,368)*y(k,99)
         mat(k,2523) = -rxt(k,373)*y(k,99)

         mat(k,2698) = -(rxt(k,285)*y(k,161) + rxt(k,286)*y(k,167) + (rxt(k,299) &
                      + rxt(k,300)) * y(k,162) + (rxt(k,785) + rxt(k,791) + rxt(k,796) &
                      ) * y(k,109) + (rxt(k,790) + rxt(k,795) + rxt(k,800)) * y(k,71) &
                      + (rxt(k,792) + rxt(k,797)) * y(k,107))
         mat(k,2737) = -rxt(k,285)*y(k,100)
         mat(k,2476) = -rxt(k,286)*y(k,100)
         mat(k,2549) = -(rxt(k,299) + rxt(k,300)) * y(k,100)
         mat(k,974) = -(rxt(k,785) + rxt(k,791) + rxt(k,796)) * y(k,100)
         mat(k,1026) = -(rxt(k,790) + rxt(k,795) + rxt(k,800)) * y(k,100)
         mat(k,884) = -(rxt(k,792) + rxt(k,797)) * y(k,100)

         mat(k,358) = rxt(k,447)*y(k,67)
         mat(k,588) = rxt(k,357)*y(k,67)
         mat(k,2229) = rxt(k,260)*y(k,67)
         mat(k,720) = rxt(k,359)*y(k,67)
         mat(k,455) = 2.000_r8*rxt(k,362)*y(k,67)
         mat(k,1784) = rxt(k,261)*y(k,67)
         mat(k,544) = rxt(k,364)*y(k,67)
         mat(k,2275) = rxt(k,447)*y(k,35) + rxt(k,357)*y(k,50) + rxt(k,260)*y(k,51)  &
                      + rxt(k,359)*y(k,52) + 2.000_r8*rxt(k,362)*y(k,55) + rxt(k,261) &
                      *y(k,65) + rxt(k,364)*y(k,66) + rxt(k,262)*y(k,92) + rxt(k,263) &
                      *y(k,94) + rxt(k,264)*y(k,105) + rxt(k,288)*y(k,109)
         mat(k,2200) = rxt(k,280)*y(k,167)
         mat(k,1560) = rxt(k,262)*y(k,67)
         mat(k,734) = rxt(k,263)*y(k,67)
         mat(k,2102) = rxt(k,264)*y(k,67)
         mat(k,974) = mat(k,974) + rxt(k,288)*y(k,67)
         mat(k,2476) = mat(k,2476) + rxt(k,280)*y(k,70)

         mat(k,246) = -(rxt(k,394)*y(k,167) + rxt(k,403)*y(k,162))
         mat(k,2324) = -rxt(k,394)*y(k,101)
         mat(k,2517) = -rxt(k,403)*y(k,101)

         mat(k,1174) = -(rxt(k,395)*y(k,167))
         mat(k,2432) = -rxt(k,395)*y(k,102)

         mat(k,1198) = .050_r8*rxt(k,670)*y(k,163)
         mat(k,349) = .350_r8*rxt(k,436)*y(k,167)
         mat(k,610) = .370_r8*rxt(k,438)*y(k,163)
         mat(k,1290) = .120_r8*rxt(k,475)*y(k,163)
         mat(k,2064) = rxt(k,396)*y(k,108)
         mat(k,644) = rxt(k,396)*y(k,105) + rxt(k,398)*y(k,151)
         mat(k,1052) = .110_r8*rxt(k,582)*y(k,163)
         mat(k,1520) = .330_r8*rxt(k,518)*y(k,163)
         mat(k,1257) = .050_r8*rxt(k,675)*y(k,163)
         mat(k,1665) = .120_r8*rxt(k,536)*y(k,163)
         mat(k,1944) = rxt(k,398)*y(k,108)
         mat(k,2576) = .050_r8*rxt(k,670)*y(k,8) + .370_r8*rxt(k,438)*y(k,31)  &
                      + .120_r8*rxt(k,475)*y(k,36) + .110_r8*rxt(k,582)*y(k,115)  &
                      + .330_r8*rxt(k,518)*y(k,126) + .050_r8*rxt(k,675)*y(k,136)  &
                      + .120_r8*rxt(k,536)*y(k,138)
         mat(k,2432) = mat(k,2432) + .350_r8*rxt(k,436)*y(k,30)


         mat(k,1767) = rxt(k,348)*y(k,88)
         mat(k,1117) = rxt(k,348)*y(k,65) + rxt(k,349)*y(k,92) + rxt(k,351)*y(k,104)  &
                      + rxt(k,350)*y(k,260)
         mat(k,1547) = rxt(k,349)*y(k,88)
         mat(k,1804) = rxt(k,351)*y(k,88)
         mat(k,2742) = rxt(k,350)*y(k,88)

         mat(k,1809) = -(rxt(k,233)*y(k,167) + rxt(k,351)*y(k,88))
         mat(k,2463) = -rxt(k,233)*y(k,104)
         mat(k,1125) = -rxt(k,351)*y(k,104)

         mat(k,2216) = rxt(k,380)*y(k,153)
         mat(k,1361) = rxt(k,449)*y(k,153)
         mat(k,1541) = rxt(k,484)*y(k,153)
         mat(k,1020) = (rxt(k,790)+rxt(k,795)+rxt(k,800))*y(k,100)
         mat(k,461) = rxt(k,707)*y(k,153)
         mat(k,2685) = (rxt(k,790)+rxt(k,795)+rxt(k,800))*y(k,71)
         mat(k,2656) = rxt(k,227)*y(k,167)
         mat(k,2155) = rxt(k,380)*y(k,51) + rxt(k,449)*y(k,54) + rxt(k,484)*y(k,59)  &
                      + rxt(k,707)*y(k,79)
         mat(k,2463) = mat(k,2463) + rxt(k,227)*y(k,152)

         mat(k,2092) = -((rxt(k,175) + rxt(k,176) + rxt(k,177)) * y(k,91) + rxt(k,179) &
                      *y(k,161) + rxt(k,180)*y(k,163) + rxt(k,187)*y(k,167) &
                      + 4._r8*rxt(k,194)*y(k,105) + rxt(k,207)*y(k,153) + rxt(k,214) &
                      *y(k,151) + rxt(k,225)*y(k,152) + (rxt(k,264) + rxt(k,265) &
                      ) * y(k,67) + rxt(k,271)*y(k,70) + rxt(k,316)*y(k,22) + rxt(k,322) &
                      *y(k,24) + rxt(k,379)*y(k,51) + rxt(k,387)*y(k,62) + rxt(k,396) &
                      *y(k,108) + rxt(k,442)*y(k,32) + rxt(k,455)*y(k,57) + rxt(k,460) &
                      *y(k,86) + rxt(k,479)*y(k,37) + rxt(k,489)*y(k,175) + rxt(k,494) &
                      *y(k,179) + rxt(k,511)*y(k,127) + rxt(k,524)*y(k,130) + rxt(k,530) &
                      *y(k,133) + rxt(k,546)*y(k,3) + rxt(k,558)*y(k,116) + rxt(k,565) &
                      *y(k,117) + rxt(k,576)*y(k,120) + rxt(k,602)*y(k,244) + rxt(k,608) &
                      *y(k,1) + rxt(k,612)*y(k,11) + rxt(k,617)*y(k,28) + rxt(k,621) &
                      *y(k,40) + rxt(k,626)*y(k,78) + rxt(k,631)*y(k,129) + rxt(k,636) &
                      *y(k,131) + rxt(k,641)*y(k,172) + rxt(k,652)*y(k,214) + rxt(k,660) &
                      *y(k,252) + rxt(k,664)*y(k,256) + rxt(k,680)*y(k,155) + rxt(k,687) &
                      *y(k,207) + rxt(k,693)*y(k,210))
         mat(k,1794) = -(rxt(k,175) + rxt(k,176) + rxt(k,177)) * y(k,105)
         mat(k,2727) = -rxt(k,179)*y(k,105)
         mat(k,2606) = -rxt(k,180)*y(k,105)
         mat(k,2466) = -rxt(k,187)*y(k,105)
         mat(k,2158) = -rxt(k,207)*y(k,105)
         mat(k,1973) = -rxt(k,214)*y(k,105)
         mat(k,2659) = -rxt(k,225)*y(k,105)
         mat(k,2265) = -(rxt(k,264) + rxt(k,265)) * y(k,105)
         mat(k,2190) = -rxt(k,271)*y(k,105)
         mat(k,1759) = -rxt(k,316)*y(k,105)
         mat(k,2495) = -rxt(k,322)*y(k,105)
         mat(k,2219) = -rxt(k,379)*y(k,105)
         mat(k,1871) = -rxt(k,387)*y(k,105)
         mat(k,646) = -rxt(k,396)*y(k,105)
         mat(k,1038) = -rxt(k,442)*y(k,105)
         mat(k,1745) = -rxt(k,455)*y(k,105)
         mat(k,964) = -rxt(k,460)*y(k,105)
         mat(k,998) = -rxt(k,479)*y(k,105)
         mat(k,952) = -rxt(k,489)*y(k,105)
         mat(k,1411) = -rxt(k,494)*y(k,105)
         mat(k,1655) = -rxt(k,511)*y(k,105)
         mat(k,1705) = -rxt(k,524)*y(k,105)
         mat(k,864) = -rxt(k,530)*y(k,105)
         mat(k,1163) = -rxt(k,546)*y(k,105)
         mat(k,1589) = -rxt(k,558)*y(k,105)
         mat(k,1628) = -rxt(k,565)*y(k,105)
         mat(k,1512) = -rxt(k,576)*y(k,105)
         mat(k,1485) = -rxt(k,602)*y(k,105)
         mat(k,819) = -rxt(k,608)*y(k,105)
         mat(k,704) = -rxt(k,612)*y(k,105)
         mat(k,665) = -rxt(k,617)*y(k,105)
         mat(k,873) = -rxt(k,621)*y(k,105)
         mat(k,942) = -rxt(k,626)*y(k,105)
         mat(k,910) = -rxt(k,631)*y(k,105)
         mat(k,1114) = -rxt(k,636)*y(k,105)
         mat(k,630) = -rxt(k,641)*y(k,105)
         mat(k,901) = -rxt(k,652)*y(k,105)
         mat(k,932) = -rxt(k,660)*y(k,105)
         mat(k,753) = -rxt(k,664)*y(k,105)
         mat(k,1322) = -rxt(k,680)*y(k,105)
         mat(k,1394) = -rxt(k,687)*y(k,105)
         mat(k,1144) = -rxt(k,693)*y(k,105)

         mat(k,1163) = mat(k,1163) + rxt(k,547)*y(k,151)
         mat(k,1212) = .570_r8*rxt(k,670)*y(k,163)
         mat(k,200) = .650_r8*rxt(k,611)*y(k,167)
         mat(k,704) = mat(k,704) + rxt(k,613)*y(k,151)
         mat(k,1759) = mat(k,1759) + rxt(k,315)*y(k,51)
         mat(k,2495) = mat(k,2495) + rxt(k,328)*y(k,167)
         mat(k,665) = mat(k,665) + rxt(k,619)*y(k,151)
         mat(k,352) = .350_r8*rxt(k,436)*y(k,167)
         mat(k,612) = .130_r8*rxt(k,438)*y(k,163)
         mat(k,1038) = mat(k,1038) + 2.400_r8*rxt(k,440)*y(k,32) + rxt(k,441)*y(k,62)  &
                      + rxt(k,443)*y(k,151)
         mat(k,300) = rxt(k,445)*y(k,167)
         mat(k,1300) = .280_r8*rxt(k,475)*y(k,163)
         mat(k,998) = mat(k,998) + rxt(k,478)*y(k,62) + rxt(k,480)*y(k,151)
         mat(k,2219) = mat(k,2219) + rxt(k,315)*y(k,22) + rxt(k,260)*y(k,67)  &
                      + rxt(k,380)*y(k,153) + rxt(k,381)*y(k,161)
         mat(k,716) = rxt(k,359)*y(k,67) + rxt(k,360)*y(k,167)
         mat(k,452) = rxt(k,362)*y(k,67) + rxt(k,363)*y(k,167)
         mat(k,110) = rxt(k,452)*y(k,167)
         mat(k,1745) = mat(k,1745) + .900_r8*rxt(k,454)*y(k,62) + rxt(k,556)*y(k,116)  &
                      + rxt(k,563)*y(k,117) + rxt(k,574)*y(k,120) + .470_r8*rxt(k,509) &
                      *y(k,127) + rxt(k,600)*y(k,244)
         mat(k,1871) = mat(k,1871) + rxt(k,441)*y(k,32) + rxt(k,478)*y(k,37)  &
                      + .900_r8*rxt(k,454)*y(k,57) + 4.000_r8*rxt(k,385)*y(k,62)  &
                      + rxt(k,267)*y(k,70) + rxt(k,557)*y(k,116) + rxt(k,564)*y(k,117)  &
                      + 1.200_r8*rxt(k,575)*y(k,120) + .730_r8*rxt(k,510)*y(k,127)  &
                      + rxt(k,523)*y(k,130) + rxt(k,388)*y(k,151) + .500_r8*rxt(k,679) &
                      *y(k,155) + .300_r8*rxt(k,493)*y(k,179) + rxt(k,686)*y(k,207)  &
                      + rxt(k,692)*y(k,210) + .800_r8*rxt(k,601)*y(k,244)
         mat(k,913) = rxt(k,390)*y(k,167)
         mat(k,1778) = rxt(k,401)*y(k,162)
         mat(k,2265) = mat(k,2265) + rxt(k,260)*y(k,51) + rxt(k,359)*y(k,52)  &
                      + rxt(k,362)*y(k,55) + rxt(k,263)*y(k,94)
         mat(k,2190) = mat(k,2190) + rxt(k,267)*y(k,62) + rxt(k,279)*y(k,167)
         mat(k,1445) = rxt(k,393)*y(k,167)
         mat(k,222) = .730_r8*rxt(k,625)*y(k,167)
         mat(k,942) = mat(k,942) + .070_r8*rxt(k,626)*y(k,105) + .170_r8*rxt(k,627) &
                      *y(k,151)
         mat(k,462) = .500_r8*rxt(k,725)*y(k,167)
         mat(k,839) = rxt(k,504)*y(k,151)
         mat(k,964) = mat(k,964) + .250_r8*rxt(k,461)*y(k,151)
         mat(k,1371) = rxt(k,465)*y(k,167)
         mat(k,1236) = rxt(k,466)*y(k,167)
         mat(k,731) = rxt(k,263)*y(k,67) + rxt(k,172)*y(k,161) + rxt(k,186)*y(k,167)
         mat(k,248) = rxt(k,394)*y(k,167)
         mat(k,1176) = rxt(k,395)*y(k,167)
         mat(k,2092) = mat(k,2092) + .070_r8*rxt(k,626)*y(k,78) + .160_r8*rxt(k,631) &
                      *y(k,129) + .330_r8*rxt(k,636)*y(k,131)
         mat(k,646) = mat(k,646) + rxt(k,398)*y(k,151)
         mat(k,1435) = rxt(k,508)*y(k,167)
         mat(k,1419) = rxt(k,487)*y(k,167)
         mat(k,1062) = .370_r8*rxt(k,582)*y(k,163)
         mat(k,1589) = mat(k,1589) + rxt(k,556)*y(k,57) + rxt(k,557)*y(k,62)  &
                      + .920_r8*rxt(k,559)*y(k,151) + rxt(k,560)*y(k,153)
         mat(k,1628) = mat(k,1628) + rxt(k,563)*y(k,57) + rxt(k,564)*y(k,62)  &
                      + .920_r8*rxt(k,567)*y(k,151) + rxt(k,568)*y(k,153)
         mat(k,742) = .300_r8*rxt(k,571)*y(k,167)
         mat(k,604) = rxt(k,572)*y(k,167)
         mat(k,1512) = mat(k,1512) + rxt(k,574)*y(k,57) + 1.200_r8*rxt(k,575)*y(k,62)  &
                      + rxt(k,577)*y(k,151) + rxt(k,578)*y(k,153)
         mat(k,489) = rxt(k,581)*y(k,167)
         mat(k,1528) = .140_r8*rxt(k,518)*y(k,163)
         mat(k,1655) = mat(k,1655) + .470_r8*rxt(k,509)*y(k,57) + .730_r8*rxt(k,510) &
                      *y(k,62) + .470_r8*rxt(k,513)*y(k,151) + .470_r8*rxt(k,512) &
                      *y(k,153)
         mat(k,386) = .200_r8*rxt(k,520)*y(k,167)
         mat(k,910) = mat(k,910) + .160_r8*rxt(k,631)*y(k,105) + .400_r8*rxt(k,632) &
                      *y(k,151)
         mat(k,1705) = mat(k,1705) + rxt(k,523)*y(k,62)
         mat(k,1114) = mat(k,1114) + .330_r8*rxt(k,636)*y(k,105) + .830_r8*rxt(k,637) &
                      *y(k,151)
         mat(k,679) = .500_r8*rxt(k,535)*y(k,167)
         mat(k,1272) = .570_r8*rxt(k,675)*y(k,163)
         mat(k,1680) = .280_r8*rxt(k,536)*y(k,163)
         mat(k,478) = rxt(k,587)*y(k,167)
         mat(k,1350) = rxt(k,588)*y(k,167)
         mat(k,1973) = mat(k,1973) + rxt(k,547)*y(k,3) + rxt(k,613)*y(k,11)  &
                      + rxt(k,619)*y(k,28) + rxt(k,443)*y(k,32) + rxt(k,480)*y(k,37)  &
                      + rxt(k,388)*y(k,62) + .170_r8*rxt(k,627)*y(k,78) + rxt(k,504) &
                      *y(k,84) + .250_r8*rxt(k,461)*y(k,86) + rxt(k,398)*y(k,108)  &
                      + .920_r8*rxt(k,559)*y(k,116) + .920_r8*rxt(k,567)*y(k,117)  &
                      + rxt(k,577)*y(k,120) + .470_r8*rxt(k,513)*y(k,127)  &
                      + .400_r8*rxt(k,632)*y(k,129) + .830_r8*rxt(k,637)*y(k,131)  &
                      + rxt(k,642)*y(k,172) + rxt(k,490)*y(k,175) + .900_r8*rxt(k,688) &
                      *y(k,207) + .800_r8*rxt(k,694)*y(k,210) + rxt(k,653)*y(k,214)  &
                      + rxt(k,603)*y(k,244) + rxt(k,661)*y(k,252) + rxt(k,665) &
                      *y(k,256)
         mat(k,2158) = mat(k,2158) + rxt(k,380)*y(k,51) + rxt(k,560)*y(k,116)  &
                      + rxt(k,568)*y(k,117) + rxt(k,578)*y(k,120) + .470_r8*rxt(k,512) &
                      *y(k,127) + rxt(k,210)*y(k,167) + rxt(k,604)*y(k,244)
         mat(k,1322) = mat(k,1322) + .500_r8*rxt(k,679)*y(k,62)
         mat(k,2727) = mat(k,2727) + rxt(k,381)*y(k,51) + rxt(k,172)*y(k,94)
         mat(k,2539) = rxt(k,401)*y(k,65)
         mat(k,2606) = mat(k,2606) + .570_r8*rxt(k,670)*y(k,8) + .130_r8*rxt(k,438) &
                      *y(k,31) + .280_r8*rxt(k,475)*y(k,36) + .370_r8*rxt(k,582) &
                      *y(k,115) + .140_r8*rxt(k,518)*y(k,126) + .570_r8*rxt(k,675) &
                      *y(k,136) + .280_r8*rxt(k,536)*y(k,138) + rxt(k,189)*y(k,167)
         mat(k,2466) = mat(k,2466) + .650_r8*rxt(k,611)*y(k,10) + rxt(k,328)*y(k,24)  &
                      + .350_r8*rxt(k,436)*y(k,30) + rxt(k,445)*y(k,33) + rxt(k,360) &
                      *y(k,52) + rxt(k,363)*y(k,55) + rxt(k,452)*y(k,56) + rxt(k,390) &
                      *y(k,63) + rxt(k,279)*y(k,70) + rxt(k,393)*y(k,73)  &
                      + .730_r8*rxt(k,625)*y(k,77) + .500_r8*rxt(k,725)*y(k,79)  &
                      + rxt(k,465)*y(k,89) + rxt(k,466)*y(k,90) + rxt(k,186)*y(k,94)  &
                      + rxt(k,394)*y(k,101) + rxt(k,395)*y(k,102) + rxt(k,508) &
                      *y(k,110) + rxt(k,487)*y(k,112) + .300_r8*rxt(k,571)*y(k,118)  &
                      + rxt(k,572)*y(k,119) + rxt(k,581)*y(k,121) + .200_r8*rxt(k,520) &
                      *y(k,128) + .500_r8*rxt(k,535)*y(k,135) + rxt(k,587)*y(k,142)  &
                      + rxt(k,588)*y(k,143) + rxt(k,210)*y(k,153) + rxt(k,189) &
                      *y(k,163) + .800_r8*rxt(k,644)*y(k,173) + rxt(k,726)*y(k,184)  &
                      + .200_r8*rxt(k,700)*y(k,213) + .280_r8*rxt(k,656)*y(k,216)  &
                      + .380_r8*rxt(k,659)*y(k,251) + .630_r8*rxt(k,667)*y(k,255)
         mat(k,630) = mat(k,630) + rxt(k,642)*y(k,151)
         mat(k,209) = .800_r8*rxt(k,644)*y(k,167)
         mat(k,952) = mat(k,952) + rxt(k,490)*y(k,151)
         mat(k,1411) = mat(k,1411) + .300_r8*rxt(k,493)*y(k,62)
         mat(k,1012) = rxt(k,726)*y(k,167)
         mat(k,1394) = mat(k,1394) + rxt(k,686)*y(k,62) + .900_r8*rxt(k,688)*y(k,151)
         mat(k,1144) = mat(k,1144) + rxt(k,692)*y(k,62) + .800_r8*rxt(k,694)*y(k,151)
         mat(k,1334) = .200_r8*rxt(k,700)*y(k,167)
         mat(k,901) = mat(k,901) + rxt(k,653)*y(k,151)
         mat(k,217) = .280_r8*rxt(k,656)*y(k,167)
         mat(k,1485) = mat(k,1485) + rxt(k,600)*y(k,57) + .800_r8*rxt(k,601)*y(k,62)  &
                      + rxt(k,603)*y(k,151) + rxt(k,604)*y(k,153)
         mat(k,287) = .380_r8*rxt(k,659)*y(k,167)
         mat(k,932) = mat(k,932) + rxt(k,661)*y(k,151)
         mat(k,292) = .630_r8*rxt(k,667)*y(k,167)
         mat(k,753) = mat(k,753) + rxt(k,665)*y(k,151)

      end do

      end subroutine     nlnmat05

      subroutine     nlnmat06( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,516) = -(rxt(k,195)*y(k,167))
         mat(k,2365) = -rxt(k,195)*y(k,106)

         mat(k,2017) = rxt(k,225)*y(k,152)
         mat(k,2625) = rxt(k,225)*y(k,105)

         mat(k,879) = -(rxt(k,335)*y(k,161) + (rxt(k,792) + rxt(k,797)) * y(k,100))
         mat(k,2709) = -rxt(k,335)*y(k,107)
         mat(k,2677) = -(rxt(k,792) + rxt(k,797)) * y(k,107)

         mat(k,2484) = rxt(k,322)*y(k,105)
         mat(k,2043) = rxt(k,322)*y(k,24)

         mat(k,641) = -(rxt(k,396)*y(k,105) + rxt(k,398)*y(k,151))
         mat(k,2027) = -rxt(k,396)*y(k,108)
         mat(k,1912) = -rxt(k,398)*y(k,108)

         mat(k,2204) = rxt(k,379)*y(k,105)
         mat(k,2027) = mat(k,2027) + rxt(k,379)*y(k,51)

         mat(k,970) = -(rxt(k,288)*y(k,67) + rxt(k,289)*y(k,161) + rxt(k,290)*y(k,167) &
                      + (rxt(k,785) + rxt(k,791) + rxt(k,796)) * y(k,100))
         mat(k,2245) = -rxt(k,288)*y(k,109)
         mat(k,2710) = -rxt(k,289)*y(k,109)
         mat(k,2414) = -rxt(k,290)*y(k,109)
         mat(k,2678) = -(rxt(k,785) + rxt(k,791) + rxt(k,796)) * y(k,109)

         mat(k,2177) = rxt(k,271)*y(k,105)
         mat(k,1018) = rxt(k,276)*y(k,167)
         mat(k,2050) = rxt(k,271)*y(k,70)
         mat(k,2414) = mat(k,2414) + rxt(k,276)*y(k,71)

         mat(k,1428) = -(rxt(k,508)*y(k,167))
         mat(k,2446) = -rxt(k,508)*y(k,110)

         mat(k,837) = rxt(k,505)*y(k,151)
         mat(k,740) = .300_r8*rxt(k,571)*y(k,167)
         mat(k,602) = .500_r8*rxt(k,572)*y(k,167)
         mat(k,1645) = rxt(k,514)*y(k,151)
         mat(k,1955) = rxt(k,505)*y(k,84) + rxt(k,514)*y(k,127)
         mat(k,2446) = mat(k,2446) + .300_r8*rxt(k,571)*y(k,118) + .500_r8*rxt(k,572) &
                      *y(k,119)

         mat(k,251) = -(rxt(k,553)*y(k,167))
         mat(k,2325) = -rxt(k,553)*y(k,111)

         mat(k,1416) = -(rxt(k,487)*y(k,167))
         mat(k,2445) = -rxt(k,487)*y(k,112)

         mat(k,1728) = .220_r8*rxt(k,509)*y(k,127) + .250_r8*rxt(k,600)*y(k,244)
         mat(k,1854) = .230_r8*rxt(k,510)*y(k,127) + .200_r8*rxt(k,493)*y(k,179)  &
                      + .100_r8*rxt(k,601)*y(k,244)
         mat(k,1613) = .050_r8*rxt(k,567)*y(k,151) + .050_r8*rxt(k,568)*y(k,153)
         mat(k,739) = .700_r8*rxt(k,571)*y(k,167)
         mat(k,601) = .500_r8*rxt(k,572)*y(k,167)
         mat(k,1644) = .220_r8*rxt(k,509)*y(k,57) + .230_r8*rxt(k,510)*y(k,62)  &
                      + .220_r8*rxt(k,513)*y(k,151) + .220_r8*rxt(k,512)*y(k,153)
         mat(k,677) = .500_r8*rxt(k,535)*y(k,167)
         mat(k,1954) = .050_r8*rxt(k,567)*y(k,117) + .220_r8*rxt(k,513)*y(k,127)  &
                      + .250_r8*rxt(k,603)*y(k,244)
         mat(k,2138) = .050_r8*rxt(k,568)*y(k,117) + .220_r8*rxt(k,512)*y(k,127)  &
                      + .250_r8*rxt(k,604)*y(k,244)
         mat(k,2445) = mat(k,2445) + .700_r8*rxt(k,571)*y(k,118) + .500_r8*rxt(k,572) &
                      *y(k,119) + .500_r8*rxt(k,535)*y(k,135) + .500_r8*rxt(k,492) &
                      *y(k,178)
         mat(k,636) = .500_r8*rxt(k,492)*y(k,167)
         mat(k,1406) = .200_r8*rxt(k,493)*y(k,62)
         mat(k,1477) = .250_r8*rxt(k,600)*y(k,57) + .100_r8*rxt(k,601)*y(k,62)  &
                      + .250_r8*rxt(k,603)*y(k,151) + .250_r8*rxt(k,604)*y(k,153)

         mat(k,431) = -(rxt(k,554)*y(k,167))
         mat(k,2352) = -rxt(k,554)*y(k,113)

         mat(k,1714) = rxt(k,563)*y(k,117)
         mat(k,1825) = .750_r8*rxt(k,564)*y(k,117)
         mat(k,1600) = rxt(k,563)*y(k,57) + .750_r8*rxt(k,564)*y(k,62)  &
                      + .870_r8*rxt(k,567)*y(k,151) + .950_r8*rxt(k,568)*y(k,153)
         mat(k,1901) = .870_r8*rxt(k,567)*y(k,117)
         mat(k,2109) = .950_r8*rxt(k,568)*y(k,117)

         mat(k,142) = -(rxt(k,555)*y(k,167))
         mat(k,2310) = -rxt(k,555)*y(k,114)

         mat(k,822) = .600_r8*rxt(k,584)*y(k,167)
         mat(k,2310) = mat(k,2310) + .600_r8*rxt(k,584)*y(k,123)

         mat(k,1048) = -(rxt(k,573)*y(k,153) + rxt(k,582)*y(k,163) + rxt(k,583) &
                      *y(k,167))
         mat(k,2119) = -rxt(k,573)*y(k,115)
         mat(k,2569) = -rxt(k,582)*y(k,115)
         mat(k,2422) = -rxt(k,583)*y(k,115)

         mat(k,1580) = -(rxt(k,556)*y(k,57) + rxt(k,557)*y(k,62) + rxt(k,558)*y(k,105) &
                      + rxt(k,559)*y(k,151) + rxt(k,560)*y(k,153))
         mat(k,1734) = -rxt(k,556)*y(k,116)
         mat(k,1860) = -rxt(k,557)*y(k,116)
         mat(k,2080) = -rxt(k,558)*y(k,116)
         mat(k,1961) = -rxt(k,559)*y(k,116)
         mat(k,2146) = -rxt(k,560)*y(k,116)

         mat(k,1057) = .600_r8*rxt(k,583)*y(k,167)
         mat(k,2454) = .600_r8*rxt(k,583)*y(k,115)

         mat(k,1619) = -(rxt(k,563)*y(k,57) + rxt(k,564)*y(k,62) + rxt(k,565)*y(k,105) &
                      + rxt(k,567)*y(k,151) + rxt(k,568)*y(k,153))
         mat(k,1735) = -rxt(k,563)*y(k,117)
         mat(k,1861) = -rxt(k,564)*y(k,117)
         mat(k,2081) = -rxt(k,565)*y(k,117)
         mat(k,1962) = -rxt(k,567)*y(k,117)
         mat(k,2147) = -rxt(k,568)*y(k,117)

         mat(k,1058) = .400_r8*rxt(k,583)*y(k,167)
         mat(k,2455) = .400_r8*rxt(k,583)*y(k,115)

         mat(k,737) = -(rxt(k,571)*y(k,167))
         mat(k,2393) = -rxt(k,571)*y(k,118)

         mat(k,1565) = .080_r8*rxt(k,559)*y(k,151)
         mat(k,1918) = .080_r8*rxt(k,559)*y(k,116)

         mat(k,598) = -(rxt(k,572)*y(k,167))
         mat(k,2379) = -rxt(k,572)*y(k,119)

         mat(k,1601) = .080_r8*rxt(k,567)*y(k,151)
         mat(k,1910) = .080_r8*rxt(k,567)*y(k,117)

         mat(k,1503) = -(rxt(k,574)*y(k,57) + rxt(k,575)*y(k,62) + rxt(k,576)*y(k,105) &
                      + rxt(k,577)*y(k,151) + rxt(k,578)*y(k,153))
         mat(k,1731) = -rxt(k,574)*y(k,120)
         mat(k,1857) = -rxt(k,575)*y(k,120)
         mat(k,2076) = -rxt(k,576)*y(k,120)
         mat(k,1958) = -rxt(k,577)*y(k,120)
         mat(k,2143) = -rxt(k,578)*y(k,120)

         mat(k,1055) = rxt(k,573)*y(k,153)
         mat(k,2143) = mat(k,2143) + rxt(k,573)*y(k,115)

         mat(k,486) = -(rxt(k,581)*y(k,167))
         mat(k,2361) = -rxt(k,581)*y(k,121)

         mat(k,2013) = rxt(k,576)*y(k,120)
         mat(k,1492) = rxt(k,576)*y(k,105)

         mat(k,72) = -(rxt(k,765)*y(k,105) + rxt(k,766)*y(k,151))
         mat(k,1993) = -rxt(k,765)*y(k,122)
         mat(k,1890) = -rxt(k,766)*y(k,122)

         mat(k,1044) = rxt(k,768)*y(k,167)
         mat(k,2298) = rxt(k,768)*y(k,115)

         mat(k,823) = -(rxt(k,584)*y(k,167))
         mat(k,2402) = -rxt(k,584)*y(k,123)

         mat(k,2039) = rxt(k,558)*y(k,116) + rxt(k,565)*y(k,117)
         mat(k,1566) = rxt(k,558)*y(k,105)
         mat(k,1603) = rxt(k,565)*y(k,105)

         mat(k,80) = -(rxt(k,771)*y(k,167))
         mat(k,2300) = -rxt(k,771)*y(k,124)

         mat(k,78) = -(rxt(k,769)*y(k,105) + rxt(k,770)*y(k,151))
         mat(k,1994) = -rxt(k,769)*y(k,125)
         mat(k,1891) = -rxt(k,770)*y(k,125)

         mat(k,79) = rxt(k,771)*y(k,167)
         mat(k,2299) = rxt(k,771)*y(k,124)

         mat(k,1522) = -(rxt(k,518)*y(k,163) + rxt(k,519)*y(k,167))
         mat(k,2592) = -rxt(k,518)*y(k,126)
         mat(k,2451) = -rxt(k,519)*y(k,126)

         mat(k,1732) = .390_r8*rxt(k,556)*y(k,116)
         mat(k,1858) = .310_r8*rxt(k,557)*y(k,116)
         mat(k,1056) = .300_r8*rxt(k,582)*y(k,163)
         mat(k,1578) = .390_r8*rxt(k,556)*y(k,57) + .310_r8*rxt(k,557)*y(k,62)  &
                      + .360_r8*rxt(k,559)*y(k,151) + .400_r8*rxt(k,560)*y(k,153)
         mat(k,1959) = .360_r8*rxt(k,559)*y(k,116)
         mat(k,2144) = .400_r8*rxt(k,560)*y(k,116)
         mat(k,2592) = mat(k,2592) + .300_r8*rxt(k,582)*y(k,115)

         mat(k,1648) = -(rxt(k,509)*y(k,57) + rxt(k,510)*y(k,62) + rxt(k,511)*y(k,105) &
                      + rxt(k,512)*y(k,153) + (rxt(k,513) + rxt(k,514)) * y(k,151))
         mat(k,1736) = -rxt(k,509)*y(k,127)
         mat(k,1862) = -rxt(k,510)*y(k,127)
         mat(k,2082) = -rxt(k,511)*y(k,127)
         mat(k,2148) = -rxt(k,512)*y(k,127)
         mat(k,1963) = -(rxt(k,513) + rxt(k,514)) * y(k,127)

         mat(k,1524) = .500_r8*rxt(k,519)*y(k,167)
         mat(k,384) = .200_r8*rxt(k,520)*y(k,167)
         mat(k,1671) = rxt(k,537)*y(k,167)
         mat(k,2456) = .500_r8*rxt(k,519)*y(k,126) + .200_r8*rxt(k,520)*y(k,128)  &
                      + rxt(k,537)*y(k,138)

         mat(k,383) = -(rxt(k,520)*y(k,167))
         mat(k,2345) = -rxt(k,520)*y(k,128)

         mat(k,2006) = rxt(k,511)*y(k,127)
         mat(k,1636) = rxt(k,511)*y(k,105)

         mat(k,904) = -(rxt(k,631)*y(k,105) + rxt(k,632)*y(k,151) + rxt(k,633) &
                      *y(k,152))
         mat(k,2045) = -rxt(k,631)*y(k,129)
         mat(k,1927) = -rxt(k,632)*y(k,129)
         mat(k,2636) = -rxt(k,633)*y(k,129)

         mat(k,1699) = -(rxt(k,522)*y(k,57) + rxt(k,523)*y(k,62) + rxt(k,524)*y(k,105) &
                      + 4._r8*rxt(k,525)*y(k,130) + rxt(k,526)*y(k,151) + rxt(k,527) &
                      *y(k,153) + rxt(k,539)*y(k,152))
         mat(k,1738) = -rxt(k,522)*y(k,130)
         mat(k,1864) = -rxt(k,523)*y(k,130)
         mat(k,2084) = -rxt(k,524)*y(k,130)
         mat(k,1965) = -rxt(k,526)*y(k,130)
         mat(k,2150) = -rxt(k,527)*y(k,130)
         mat(k,2651) = -rxt(k,539)*y(k,130)

         mat(k,1525) = .500_r8*rxt(k,519)*y(k,167)
         mat(k,385) = .500_r8*rxt(k,520)*y(k,167)
         mat(k,2458) = .500_r8*rxt(k,519)*y(k,126) + .500_r8*rxt(k,520)*y(k,128)

         mat(k,1108) = -(rxt(k,636)*y(k,105) + rxt(k,637)*y(k,151) + rxt(k,638) &
                      *y(k,152))
         mat(k,2060) = -rxt(k,636)*y(k,131)
         mat(k,1940) = -rxt(k,637)*y(k,131)
         mat(k,2645) = -rxt(k,638)*y(k,131)

         mat(k,571) = -(rxt(k,533)*y(k,167))
         mat(k,2374) = -rxt(k,533)*y(k,132)

         mat(k,1149) = .800_r8*rxt(k,547)*y(k,151)
         mat(k,1907) = .800_r8*rxt(k,547)*y(k,3)

         mat(k,858) = -(rxt(k,530)*y(k,105) + rxt(k,531)*y(k,151))
         mat(k,2041) = -rxt(k,530)*y(k,133)
         mat(k,1924) = -rxt(k,531)*y(k,133)

         mat(k,572) = rxt(k,533)*y(k,167)
         mat(k,389) = rxt(k,534)*y(k,167)
         mat(k,2405) = rxt(k,533)*y(k,132) + rxt(k,534)*y(k,134)

         mat(k,388) = -(rxt(k,534)*y(k,167))
         mat(k,2346) = -rxt(k,534)*y(k,134)

         mat(k,2007) = .800_r8*rxt(k,530)*y(k,133)
         mat(k,857) = .800_r8*rxt(k,530)*y(k,105)

         mat(k,675) = -(rxt(k,535)*y(k,167))
         mat(k,2386) = -rxt(k,535)*y(k,135)

         mat(k,1689) = rxt(k,539)*y(k,152)
         mat(k,2631) = rxt(k,539)*y(k,130)

         mat(k,1260) = -(rxt(k,674)*y(k,153) + rxt(k,675)*y(k,163) + rxt(k,676) &
                      *y(k,167))
         mat(k,2129) = -rxt(k,674)*y(k,136)
         mat(k,2580) = -rxt(k,675)*y(k,136)
         mat(k,2436) = -rxt(k,676)*y(k,136)

         mat(k,86) = -(rxt(k,773)*y(k,105) + rxt(k,774)*y(k,151))
         mat(k,1995) = -rxt(k,773)*y(k,137)
         mat(k,1892) = -rxt(k,774)*y(k,137)

         mat(k,1244) = rxt(k,776)*y(k,167)
         mat(k,2301) = rxt(k,776)*y(k,136)

      end do

      end subroutine     nlnmat06

      subroutine     nlnmat07( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,1672) = -(rxt(k,536)*y(k,163) + rxt(k,537)*y(k,167))
         mat(k,2597) = -rxt(k,536)*y(k,138)
         mat(k,2457) = -rxt(k,537)*y(k,138)

         mat(k,1737) = .610_r8*rxt(k,556)*y(k,116)
         mat(k,1863) = .440_r8*rxt(k,557)*y(k,116)
         mat(k,1059) = .200_r8*rxt(k,582)*y(k,163)
         mat(k,1582) = .610_r8*rxt(k,556)*y(k,57) + .440_r8*rxt(k,557)*y(k,62)  &
                      + .560_r8*rxt(k,559)*y(k,151) + .600_r8*rxt(k,560)*y(k,153)
         mat(k,1964) = .560_r8*rxt(k,559)*y(k,116)
         mat(k,2149) = .600_r8*rxt(k,560)*y(k,116)
         mat(k,2597) = mat(k,2597) + .200_r8*rxt(k,582)*y(k,115)

         mat(k,687) = -(rxt(k,196)*y(k,151) + (rxt(k,197) + rxt(k,198) + rxt(k,199) &
                      ) * y(k,152) + rxt(k,213)*y(k,167))
         mat(k,1915) = -rxt(k,196)*y(k,139)
         mat(k,2632) = -(rxt(k,197) + rxt(k,198) + rxt(k,199)) * y(k,139)
         mat(k,2387) = -rxt(k,213)*y(k,139)

         mat(k,294) = -((rxt(k,223) + rxt(k,224)) * y(k,162))
         mat(k,2518) = -(rxt(k,223) + rxt(k,224)) * y(k,140)

         mat(k,684) = rxt(k,197)*y(k,152)
         mat(k,2622) = rxt(k,197)*y(k,139)


         mat(k,2623) = rxt(k,226)*y(k,153)
         mat(k,2108) = rxt(k,226)*y(k,152)

         mat(k,474) = -(rxt(k,587)*y(k,167))
         mat(k,2359) = -rxt(k,587)*y(k,142)

         mat(k,1829) = .200_r8*rxt(k,575)*y(k,120)
         mat(k,1491) = .200_r8*rxt(k,575)*y(k,62)

         mat(k,1343) = -(rxt(k,588)*y(k,167))
         mat(k,2440) = -rxt(k,588)*y(k,143)

         mat(k,1726) = rxt(k,574)*y(k,120)
         mat(k,1849) = .800_r8*rxt(k,575)*y(k,120)
         mat(k,1499) = rxt(k,574)*y(k,57) + .800_r8*rxt(k,575)*y(k,62) + rxt(k,577) &
                      *y(k,151) + rxt(k,578)*y(k,153)
         mat(k,1949) = rxt(k,577)*y(k,120)
         mat(k,2133) = rxt(k,578)*y(k,120)




         mat(k,112) = -(rxt(k,728)*y(k,167))
         mat(k,2306) = -rxt(k,728)*y(k,147)




         mat(k,1972) = -(rxt(k,196)*y(k,139) + rxt(k,208)*y(k,153) + rxt(k,214) &
                      *y(k,105) + rxt(k,215)*y(k,163) + rxt(k,216)*y(k,161) + rxt(k,272) &
                      *y(k,70) + rxt(k,323)*y(k,24) + rxt(k,388)*y(k,62) + rxt(k,398) &
                      *y(k,108) + rxt(k,443)*y(k,32) + rxt(k,456)*y(k,57) + rxt(k,461) &
                      *y(k,86) + rxt(k,480)*y(k,37) + rxt(k,490)*y(k,175) + rxt(k,495) &
                      *y(k,179) + (rxt(k,504) + rxt(k,505)) * y(k,84) + (rxt(k,513) &
                      + rxt(k,514)) * y(k,127) + rxt(k,526)*y(k,130) + rxt(k,531) &
                      *y(k,133) + (rxt(k,547) + rxt(k,548)) * y(k,3) + rxt(k,559) &
                      *y(k,116) + rxt(k,567)*y(k,117) + rxt(k,577)*y(k,120) + rxt(k,603) &
                      *y(k,244) + rxt(k,609)*y(k,1) + rxt(k,613)*y(k,11) + rxt(k,619) &
                      *y(k,28) + rxt(k,622)*y(k,40) + rxt(k,627)*y(k,78) + rxt(k,632) &
                      *y(k,129) + rxt(k,637)*y(k,131) + rxt(k,642)*y(k,172) + rxt(k,653) &
                      *y(k,214) + rxt(k,661)*y(k,252) + rxt(k,665)*y(k,256) + rxt(k,681) &
                      *y(k,155) + rxt(k,688)*y(k,207) + rxt(k,694)*y(k,210))
         mat(k,691) = -rxt(k,196)*y(k,151)
         mat(k,2157) = -rxt(k,208)*y(k,151)
         mat(k,2091) = -rxt(k,214)*y(k,151)
         mat(k,2605) = -rxt(k,215)*y(k,151)
         mat(k,2726) = -rxt(k,216)*y(k,151)
         mat(k,2189) = -rxt(k,272)*y(k,151)
         mat(k,2494) = -rxt(k,323)*y(k,151)
         mat(k,1870) = -rxt(k,388)*y(k,151)
         mat(k,645) = -rxt(k,398)*y(k,151)
         mat(k,1037) = -rxt(k,443)*y(k,151)
         mat(k,1744) = -rxt(k,456)*y(k,151)
         mat(k,963) = -rxt(k,461)*y(k,151)
         mat(k,997) = -rxt(k,480)*y(k,151)
         mat(k,951) = -rxt(k,490)*y(k,151)
         mat(k,1410) = -rxt(k,495)*y(k,151)
         mat(k,838) = -(rxt(k,504) + rxt(k,505)) * y(k,151)
         mat(k,1654) = -(rxt(k,513) + rxt(k,514)) * y(k,151)
         mat(k,1704) = -rxt(k,526)*y(k,151)
         mat(k,863) = -rxt(k,531)*y(k,151)
         mat(k,1162) = -(rxt(k,547) + rxt(k,548)) * y(k,151)
         mat(k,1588) = -rxt(k,559)*y(k,151)
         mat(k,1627) = -rxt(k,567)*y(k,151)
         mat(k,1511) = -rxt(k,577)*y(k,151)
         mat(k,1484) = -rxt(k,603)*y(k,151)
         mat(k,818) = -rxt(k,609)*y(k,151)
         mat(k,703) = -rxt(k,613)*y(k,151)
         mat(k,664) = -rxt(k,619)*y(k,151)
         mat(k,872) = -rxt(k,622)*y(k,151)
         mat(k,941) = -rxt(k,627)*y(k,151)
         mat(k,909) = -rxt(k,632)*y(k,151)
         mat(k,1113) = -rxt(k,637)*y(k,151)
         mat(k,629) = -rxt(k,642)*y(k,151)
         mat(k,900) = -rxt(k,653)*y(k,151)
         mat(k,931) = -rxt(k,661)*y(k,151)
         mat(k,752) = -rxt(k,665)*y(k,151)
         mat(k,1321) = -rxt(k,681)*y(k,151)
         mat(k,1393) = -rxt(k,688)*y(k,151)
         mat(k,1143) = -rxt(k,694)*y(k,151)

         mat(k,691) = mat(k,691) + 2.000_r8*rxt(k,198)*y(k,152) + rxt(k,213)*y(k,167)
         mat(k,296) = 2.000_r8*rxt(k,223)*y(k,162)
         mat(k,2658) = 2.000_r8*rxt(k,198)*y(k,139) + rxt(k,201)*y(k,161) + rxt(k,717) &
                      *y(k,183)
         mat(k,2726) = mat(k,2726) + rxt(k,201)*y(k,152)
         mat(k,2538) = 2.000_r8*rxt(k,223)*y(k,140)
         mat(k,2465) = rxt(k,213)*y(k,139)
         mat(k,1459) = rxt(k,717)*y(k,152)

         mat(k,2668) = -((rxt(k,197) + rxt(k,198) + rxt(k,199)) * y(k,139) + (rxt(k,201) &
                      + rxt(k,203)) * y(k,161) + rxt(k,202)*y(k,163) + rxt(k,225) &
                      *y(k,105) + rxt(k,226)*y(k,153) + rxt(k,227)*y(k,167) + rxt(k,274) &
                      *y(k,70) + rxt(k,324)*y(k,24) + rxt(k,469)*y(k,57) + rxt(k,539) &
                      *y(k,130) + rxt(k,628)*y(k,78) + rxt(k,633)*y(k,129) + rxt(k,638) &
                      *y(k,131) + rxt(k,645)*y(k,171) + rxt(k,650)*y(k,1) + rxt(k,717) &
                      *y(k,183))
         mat(k,694) = -(rxt(k,197) + rxt(k,198) + rxt(k,199)) * y(k,152)
         mat(k,2736) = -(rxt(k,201) + rxt(k,203)) * y(k,152)
         mat(k,2615) = -rxt(k,202)*y(k,152)
         mat(k,2101) = -rxt(k,225)*y(k,152)
         mat(k,2167) = -rxt(k,226)*y(k,152)
         mat(k,2475) = -rxt(k,227)*y(k,152)
         mat(k,2199) = -rxt(k,274)*y(k,152)
         mat(k,2504) = -rxt(k,324)*y(k,152)
         mat(k,1750) = -rxt(k,469)*y(k,152)
         mat(k,1710) = -rxt(k,539)*y(k,152)
         mat(k,944) = -rxt(k,628)*y(k,152)
         mat(k,911) = -rxt(k,633)*y(k,152)
         mat(k,1116) = -rxt(k,638)*y(k,152)
         mat(k,674) = -rxt(k,645)*y(k,152)
         mat(k,821) = -rxt(k,650)*y(k,152)
         mat(k,1466) = -rxt(k,717)*y(k,152)

         mat(k,821) = mat(k,821) + rxt(k,609)*y(k,151)
         mat(k,806) = rxt(k,545)*y(k,167)
         mat(k,1166) = rxt(k,547)*y(k,151)
         mat(k,706) = rxt(k,613)*y(k,151)
         mat(k,565) = rxt(k,501)*y(k,153)
         mat(k,2504) = mat(k,2504) + rxt(k,323)*y(k,151)
         mat(k,667) = rxt(k,619)*y(k,151)
         mat(k,1041) = rxt(k,443)*y(k,151)
         mat(k,1001) = rxt(k,480)*y(k,151)
         mat(k,876) = rxt(k,622)*y(k,151)
         mat(k,1750) = mat(k,1750) + rxt(k,456)*y(k,151)
         mat(k,1879) = rxt(k,388)*y(k,151) + .500_r8*rxt(k,679)*y(k,155)
         mat(k,2199) = mat(k,2199) + rxt(k,272)*y(k,151)
         mat(k,944) = mat(k,944) + rxt(k,627)*y(k,151)
         mat(k,841) = rxt(k,504)*y(k,151)
         mat(k,967) = rxt(k,461)*y(k,151)
         mat(k,2101) = mat(k,2101) + rxt(k,214)*y(k,151) + rxt(k,207)*y(k,153)
         mat(k,520) = rxt(k,195)*y(k,167)
         mat(k,648) = rxt(k,398)*y(k,151)
         mat(k,1594) = .920_r8*rxt(k,559)*y(k,151) + rxt(k,560)*y(k,153)
         mat(k,1633) = .920_r8*rxt(k,567)*y(k,151) + rxt(k,568)*y(k,153)
         mat(k,745) = .700_r8*rxt(k,571)*y(k,167)
         mat(k,1516) = rxt(k,577)*y(k,151) + rxt(k,578)*y(k,153)
         mat(k,1659) = rxt(k,513)*y(k,151) + rxt(k,512)*y(k,153)
         mat(k,911) = mat(k,911) + rxt(k,632)*y(k,151)
         mat(k,1710) = mat(k,1710) + rxt(k,526)*y(k,151) + rxt(k,527)*y(k,153)
         mat(k,1116) = mat(k,1116) + rxt(k,637)*y(k,151)
         mat(k,866) = rxt(k,531)*y(k,151)
         mat(k,1982) = rxt(k,609)*y(k,1) + rxt(k,547)*y(k,3) + rxt(k,613)*y(k,11)  &
                      + rxt(k,323)*y(k,24) + rxt(k,619)*y(k,28) + rxt(k,443)*y(k,32)  &
                      + rxt(k,480)*y(k,37) + rxt(k,622)*y(k,40) + rxt(k,456)*y(k,57)  &
                      + rxt(k,388)*y(k,62) + rxt(k,272)*y(k,70) + rxt(k,627)*y(k,78)  &
                      + rxt(k,504)*y(k,84) + rxt(k,461)*y(k,86) + rxt(k,214)*y(k,105)  &
                      + rxt(k,398)*y(k,108) + .920_r8*rxt(k,559)*y(k,116)  &
                      + .920_r8*rxt(k,567)*y(k,117) + rxt(k,577)*y(k,120) + rxt(k,513) &
                      *y(k,127) + rxt(k,632)*y(k,129) + rxt(k,526)*y(k,130)  &
                      + rxt(k,637)*y(k,131) + rxt(k,531)*y(k,133)  &
                      + 2.000_r8*rxt(k,208)*y(k,153) + 1.600_r8*rxt(k,681)*y(k,155)  &
                      + rxt(k,216)*y(k,161) + rxt(k,215)*y(k,163) + rxt(k,642) &
                      *y(k,172) + rxt(k,490)*y(k,175) + rxt(k,495)*y(k,179)  &
                      + .900_r8*rxt(k,688)*y(k,207) + .800_r8*rxt(k,694)*y(k,210)  &
                      + rxt(k,653)*y(k,214) + rxt(k,603)*y(k,244) + rxt(k,661) &
                      *y(k,252) + rxt(k,665)*y(k,256)
         mat(k,2167) = mat(k,2167) + rxt(k,501)*y(k,21) + rxt(k,207)*y(k,105)  &
                      + rxt(k,560)*y(k,116) + rxt(k,568)*y(k,117) + rxt(k,578) &
                      *y(k,120) + rxt(k,512)*y(k,127) + rxt(k,527)*y(k,130)  &
                      + 2.000_r8*rxt(k,208)*y(k,151) + 2.000_r8*rxt(k,682)*y(k,155)  &
                      + rxt(k,209)*y(k,161) + rxt(k,210)*y(k,167) + rxt(k,604) &
                      *y(k,244)
         mat(k,1008) = rxt(k,488)*y(k,167)
         mat(k,1326) = .500_r8*rxt(k,679)*y(k,62) + 1.600_r8*rxt(k,681)*y(k,151)  &
                      + 2.000_r8*rxt(k,682)*y(k,153)
         mat(k,2736) = mat(k,2736) + rxt(k,216)*y(k,151) + rxt(k,209)*y(k,153)
         mat(k,2615) = mat(k,2615) + rxt(k,215)*y(k,151)
         mat(k,2475) = mat(k,2475) + rxt(k,545)*y(k,2) + rxt(k,195)*y(k,106)  &
                      + .700_r8*rxt(k,571)*y(k,118) + rxt(k,210)*y(k,153) + rxt(k,488) &
                      *y(k,154) + rxt(k,691)*y(k,209)
         mat(k,632) = rxt(k,642)*y(k,151)
         mat(k,955) = rxt(k,490)*y(k,151)
         mat(k,1414) = rxt(k,495)*y(k,151)
         mat(k,1397) = .900_r8*rxt(k,688)*y(k,151)
         mat(k,761) = rxt(k,691)*y(k,167)
         mat(k,1147) = .800_r8*rxt(k,694)*y(k,151)
         mat(k,903) = rxt(k,653)*y(k,151)
         mat(k,1489) = rxt(k,603)*y(k,151) + rxt(k,604)*y(k,153)
         mat(k,934) = rxt(k,661)*y(k,151)
         mat(k,755) = rxt(k,665)*y(k,151)

         mat(k,2159) = -(rxt(k,207)*y(k,105) + rxt(k,208)*y(k,151) + rxt(k,209) &
                      *y(k,161) + rxt(k,210)*y(k,167) + rxt(k,226)*y(k,152) + rxt(k,380) &
                      *y(k,51) + rxt(k,449)*y(k,54) + rxt(k,474)*y(k,36) + rxt(k,484) &
                      *y(k,59) + rxt(k,501)*y(k,21) + rxt(k,512)*y(k,127) + rxt(k,527) &
                      *y(k,130) + rxt(k,560)*y(k,116) + rxt(k,568)*y(k,117) + rxt(k,573) &
                      *y(k,115) + rxt(k,578)*y(k,120) + rxt(k,604)*y(k,244) + rxt(k,669) &
                      *y(k,8) + rxt(k,674)*y(k,136) + rxt(k,682)*y(k,155) + rxt(k,697) &
                      *y(k,212) + rxt(k,707)*y(k,79))
         mat(k,2093) = -rxt(k,207)*y(k,153)
         mat(k,1974) = -rxt(k,208)*y(k,153)
         mat(k,2728) = -rxt(k,209)*y(k,153)
         mat(k,2467) = -rxt(k,210)*y(k,153)
         mat(k,2660) = -rxt(k,226)*y(k,153)
         mat(k,2220) = -rxt(k,380)*y(k,153)
         mat(k,1364) = -rxt(k,449)*y(k,153)
         mat(k,1301) = -rxt(k,474)*y(k,153)
         mat(k,1543) = -rxt(k,484)*y(k,153)
         mat(k,562) = -rxt(k,501)*y(k,153)
         mat(k,1656) = -rxt(k,512)*y(k,153)
         mat(k,1706) = -rxt(k,527)*y(k,153)
         mat(k,1590) = -rxt(k,560)*y(k,153)
         mat(k,1629) = -rxt(k,568)*y(k,153)
         mat(k,1063) = -rxt(k,573)*y(k,153)
         mat(k,1513) = -rxt(k,578)*y(k,153)
         mat(k,1486) = -rxt(k,604)*y(k,153)
         mat(k,1213) = -rxt(k,669)*y(k,153)
         mat(k,1273) = -rxt(k,674)*y(k,153)
         mat(k,1323) = -rxt(k,682)*y(k,153)
         mat(k,1230) = -rxt(k,697)*y(k,153)
         mat(k,463) = -rxt(k,707)*y(k,153)

         mat(k,654) = rxt(k,325)*y(k,161)
         mat(k,2266) = rxt(k,273)*y(k,71)
         mat(k,1021) = rxt(k,273)*y(k,67) + rxt(k,275)*y(k,161) + rxt(k,276)*y(k,167)
         mat(k,1127) = rxt(k,351)*y(k,104)
         mat(k,1812) = rxt(k,351)*y(k,88) + rxt(k,233)*y(k,167)
         mat(k,680) = .500_r8*rxt(k,535)*y(k,167)
         mat(k,2660) = mat(k,2660) + rxt(k,203)*y(k,161) + rxt(k,202)*y(k,163)
         mat(k,2728) = mat(k,2728) + rxt(k,325)*y(k,25) + rxt(k,275)*y(k,71)  &
                      + rxt(k,203)*y(k,152)
         mat(k,2607) = rxt(k,202)*y(k,152)
         mat(k,2467) = mat(k,2467) + rxt(k,276)*y(k,71) + rxt(k,233)*y(k,104)  &
                      + .500_r8*rxt(k,535)*y(k,135) + rxt(k,467)*y(k,169)
         mat(k,620) = rxt(k,467)*y(k,167)

      end do

      end subroutine     nlnmat07

      subroutine     nlnmat08( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,1003) = -(rxt(k,488)*y(k,167))
         mat(k,2418) = -rxt(k,488)*y(k,154)

         mat(k,1286) = rxt(k,474)*y(k,153)
         mat(k,599) = .500_r8*rxt(k,572)*y(k,167)
         mat(k,488) = rxt(k,581)*y(k,167)
         mat(k,475) = rxt(k,587)*y(k,167)
         mat(k,1340) = rxt(k,588)*y(k,167)
         mat(k,2117) = rxt(k,474)*y(k,36)
         mat(k,2418) = mat(k,2418) + .500_r8*rxt(k,572)*y(k,119) + rxt(k,581)*y(k,121)  &
                      + rxt(k,587)*y(k,142) + rxt(k,588)*y(k,143)

         mat(k,1315) = -(rxt(k,679)*y(k,62) + rxt(k,680)*y(k,105) + rxt(k,681) &
                      *y(k,151) + rxt(k,682)*y(k,153))
         mat(k,1847) = -rxt(k,679)*y(k,155)
         mat(k,2067) = -rxt(k,680)*y(k,155)
         mat(k,1947) = -rxt(k,681)*y(k,155)
         mat(k,2131) = -rxt(k,682)*y(k,155)

         mat(k,1202) = rxt(k,669)*y(k,153)
         mat(k,1261) = rxt(k,674)*y(k,153)
         mat(k,2131) = mat(k,2131) + rxt(k,669)*y(k,8) + rxt(k,674)*y(k,136)  &
                      + .500_r8*rxt(k,697)*y(k,212)
         mat(k,482) = rxt(k,685)*y(k,167)
         mat(k,2438) = rxt(k,685)*y(k,156)
         mat(k,1225) = .500_r8*rxt(k,697)*y(k,153)

         mat(k,480) = -(rxt(k,685)*y(k,167))
         mat(k,2360) = -rxt(k,685)*y(k,156)

         mat(k,2012) = rxt(k,680)*y(k,155)
         mat(k,1308) = rxt(k,680)*y(k,105)





         mat(k,2738) = -(rxt(k,159)*y(k,163) + 4._r8*rxt(k,161)*y(k,161) + rxt(k,171) &
                      *y(k,92) + rxt(k,172)*y(k,94) + rxt(k,179)*y(k,105) + rxt(k,188) &
                      *y(k,167) + (rxt(k,201) + rxt(k,203)) * y(k,152) + rxt(k,209) &
                      *y(k,153) + rxt(k,216)*y(k,151) + rxt(k,275)*y(k,71) + rxt(k,278) &
                      *y(k,70) + rxt(k,285)*y(k,100) + rxt(k,289)*y(k,109) + rxt(k,325) &
                      *y(k,25) + rxt(k,327)*y(k,24) + rxt(k,332)*y(k,96) + rxt(k,335) &
                      *y(k,107) + rxt(k,381)*y(k,51) + rxt(k,710)*y(k,166))
         mat(k,2617) = -rxt(k,159)*y(k,161)
         mat(k,1561) = -rxt(k,171)*y(k,161)
         mat(k,735) = -rxt(k,172)*y(k,161)
         mat(k,2103) = -rxt(k,179)*y(k,161)
         mat(k,2477) = -rxt(k,188)*y(k,161)
         mat(k,2670) = -(rxt(k,201) + rxt(k,203)) * y(k,161)
         mat(k,2169) = -rxt(k,209)*y(k,161)
         mat(k,1984) = -rxt(k,216)*y(k,161)
         mat(k,1027) = -rxt(k,275)*y(k,161)
         mat(k,2201) = -rxt(k,278)*y(k,161)
         mat(k,2699) = -rxt(k,285)*y(k,161)
         mat(k,975) = -rxt(k,289)*y(k,161)
         mat(k,657) = -rxt(k,325)*y(k,161)
         mat(k,2506) = -rxt(k,327)*y(k,161)
         mat(k,988) = -rxt(k,332)*y(k,161)
         mat(k,885) = -rxt(k,335)*y(k,161)
         mat(k,2230) = -rxt(k,381)*y(k,161)
         mat(k,448) = -rxt(k,710)*y(k,161)

         mat(k,1802) = rxt(k,177)*y(k,105)
         mat(k,2103) = mat(k,2103) + rxt(k,177)*y(k,91)
         mat(k,695) = rxt(k,196)*y(k,151) + rxt(k,197)*y(k,152)
         mat(k,1984) = mat(k,1984) + rxt(k,196)*y(k,139)
         mat(k,2670) = mat(k,2670) + rxt(k,197)*y(k,139)
         mat(k,2477) = mat(k,2477) + 2.000_r8*rxt(k,190)*y(k,167)

         mat(k,2546) = -(rxt(k,153)*y(k,92) + rxt(k,154)*y(k,260) + rxt(k,157) &
                      *y(k,163) + (rxt(k,223) + rxt(k,224)) * y(k,140) + rxt(k,292) &
                      *y(k,42) + rxt(k,293)*y(k,43) + rxt(k,294)*y(k,45) + rxt(k,295) &
                      *y(k,46) + rxt(k,296)*y(k,47) + rxt(k,297)*y(k,48) + rxt(k,298) &
                      *y(k,49) + (rxt(k,299) + rxt(k,300)) * y(k,100) + rxt(k,337) &
                      *y(k,44) + rxt(k,338)*y(k,66) + rxt(k,339)*y(k,93) + (rxt(k,340) &
                      + rxt(k,341)) * y(k,96) + rxt(k,353)*y(k,75) + rxt(k,354) &
                      *y(k,76) + rxt(k,369)*y(k,50) + rxt(k,370)*y(k,52) + rxt(k,371) &
                      *y(k,97) + rxt(k,372)*y(k,98) + rxt(k,373)*y(k,99) + (rxt(k,400) &
                      + rxt(k,401) + rxt(k,402)) * y(k,65) + rxt(k,403)*y(k,101))
         mat(k,1559) = -rxt(k,153)*y(k,162)
         mat(k,2762) = -rxt(k,154)*y(k,162)
         mat(k,2613) = -rxt(k,157)*y(k,162)
         mat(k,297) = -(rxt(k,223) + rxt(k,224)) * y(k,162)
         mat(k,124) = -rxt(k,292)*y(k,162)
         mat(k,173) = -rxt(k,293)*y(k,162)
         mat(k,152) = -rxt(k,294)*y(k,162)
         mat(k,185) = -rxt(k,295)*y(k,162)
         mat(k,157) = -rxt(k,296)*y(k,162)
         mat(k,191) = -rxt(k,297)*y(k,162)
         mat(k,162) = -rxt(k,298)*y(k,162)
         mat(k,2695) = -(rxt(k,299) + rxt(k,300)) * y(k,162)
         mat(k,179) = -rxt(k,337)*y(k,162)
         mat(k,543) = -rxt(k,338)*y(k,162)
         mat(k,138) = -rxt(k,339)*y(k,162)
         mat(k,987) = -(rxt(k,340) + rxt(k,341)) * y(k,162)
         mat(k,341) = -rxt(k,353)*y(k,162)
         mat(k,382) = -rxt(k,354)*y(k,162)
         mat(k,587) = -rxt(k,369)*y(k,162)
         mat(k,719) = -rxt(k,370)*y(k,162)
         mat(k,377) = -rxt(k,371)*y(k,162)
         mat(k,337) = -rxt(k,372)*y(k,162)
         mat(k,405) = -rxt(k,373)*y(k,162)
         mat(k,1783) = -(rxt(k,400) + rxt(k,401) + rxt(k,402)) * y(k,162)
         mat(k,250) = -rxt(k,403)*y(k,162)

         mat(k,2614) = -(rxt(k,157)*y(k,162) + rxt(k,159)*y(k,161) + rxt(k,180) &
                      *y(k,105) + rxt(k,183)*y(k,91) + rxt(k,189)*y(k,167) + rxt(k,202) &
                      *y(k,152) + rxt(k,215)*y(k,151) + rxt(k,266)*y(k,67) + rxt(k,317) &
                      *y(k,22) + rxt(k,438)*y(k,31) + rxt(k,475)*y(k,36) + rxt(k,518) &
                      *y(k,126) + rxt(k,536)*y(k,138) + rxt(k,582)*y(k,115) + rxt(k,646) &
                      *y(k,171) + rxt(k,670)*y(k,8) + rxt(k,675)*y(k,136) + rxt(k,713) &
                      *y(k,181) + rxt(k,719)*y(k,183))
         mat(k,2547) = -rxt(k,157)*y(k,163)
         mat(k,2735) = -rxt(k,159)*y(k,163)
         mat(k,2100) = -rxt(k,180)*y(k,163)
         mat(k,1800) = -rxt(k,183)*y(k,163)
         mat(k,2474) = -rxt(k,189)*y(k,163)
         mat(k,2667) = -rxt(k,202)*y(k,163)
         mat(k,1981) = -rxt(k,215)*y(k,163)
         mat(k,2273) = -rxt(k,266)*y(k,163)
         mat(k,1764) = -rxt(k,317)*y(k,163)
         mat(k,615) = -rxt(k,438)*y(k,163)
         mat(k,1304) = -rxt(k,475)*y(k,163)
         mat(k,1531) = -rxt(k,518)*y(k,163)
         mat(k,1684) = -rxt(k,536)*y(k,163)
         mat(k,1066) = -rxt(k,582)*y(k,163)
         mat(k,673) = -rxt(k,646)*y(k,163)
         mat(k,1216) = -rxt(k,670)*y(k,163)
         mat(k,1276) = -rxt(k,675)*y(k,163)
         mat(k,596) = -rxt(k,713)*y(k,163)
         mat(k,1465) = -rxt(k,719)*y(k,163)

         mat(k,1749) = .150_r8*rxt(k,455)*y(k,105)
         mat(k,2100) = mat(k,2100) + .150_r8*rxt(k,455)*y(k,57) + .150_r8*rxt(k,524) &
                      *y(k,130)
         mat(k,1709) = .150_r8*rxt(k,524)*y(k,105)


         mat(k,407) = -(rxt(k,720)*y(k,183))
         mat(k,1448) = -rxt(k,720)*y(k,165)

         mat(k,2481) = rxt(k,319)*y(k,70)
         mat(k,2174) = rxt(k,319)*y(k,24) + 2.000_r8*rxt(k,270)*y(k,70)

         mat(k,441) = -(rxt(k,710)*y(k,161) + rxt(k,711)*y(k,167))
         mat(k,2702) = -rxt(k,710)*y(k,166)
         mat(k,2355) = -rxt(k,711)*y(k,166)

         mat(k,2471) = -(rxt(k,185)*y(k,92) + rxt(k,186)*y(k,94) + rxt(k,187)*y(k,105) &
                      + rxt(k,188)*y(k,161) + rxt(k,189)*y(k,163) + (4._r8*rxt(k,190) &
                      + 4._r8*rxt(k,191)) * y(k,167) + rxt(k,195)*y(k,106) + rxt(k,210) &
                      *y(k,153) + rxt(k,213)*y(k,139) + rxt(k,227)*y(k,152) + rxt(k,233) &
                      *y(k,104) + rxt(k,276)*y(k,71) + (rxt(k,279) + rxt(k,280) &
                      ) * y(k,70) + rxt(k,286)*y(k,100) + rxt(k,290)*y(k,109) &
                      + rxt(k,328)*y(k,24) + rxt(k,333)*y(k,96) + rxt(k,358)*y(k,50) &
                      + rxt(k,360)*y(k,52) + rxt(k,361)*y(k,53) + rxt(k,363)*y(k,55) &
                      + rxt(k,365)*y(k,66) + rxt(k,366)*y(k,97) + rxt(k,367)*y(k,98) &
                      + rxt(k,368)*y(k,99) + rxt(k,382)*y(k,51) + rxt(k,390)*y(k,63) &
                      + rxt(k,391)*y(k,64) + rxt(k,392)*y(k,65) + (rxt(k,393) &
                      + rxt(k,404)) * y(k,73) + rxt(k,394)*y(k,101) + rxt(k,395) &
                      *y(k,102) + rxt(k,436)*y(k,30) + rxt(k,445)*y(k,33) + rxt(k,446) &
                      *y(k,34) + rxt(k,448)*y(k,35) + rxt(k,450)*y(k,54) + rxt(k,452) &
                      *y(k,56) + rxt(k,458)*y(k,60) + rxt(k,459)*y(k,61) + rxt(k,465) &
                      *y(k,89) + rxt(k,466)*y(k,90) + rxt(k,467)*y(k,169) + rxt(k,468) &
                      *y(k,31) + rxt(k,482)*y(k,38) + rxt(k,483)*y(k,39) + rxt(k,485) &
                      *y(k,59) + rxt(k,487)*y(k,112) + rxt(k,488)*y(k,154) + rxt(k,492) &
                      *y(k,178) + rxt(k,497)*y(k,180) + rxt(k,498)*y(k,36) + rxt(k,499) &
                      *y(k,58) + rxt(k,502)*y(k,21) + rxt(k,508)*y(k,110) + rxt(k,519) &
                      *y(k,126) + rxt(k,520)*y(k,128) + rxt(k,533)*y(k,132) + rxt(k,534) &
                      *y(k,134) + rxt(k,535)*y(k,135) + rxt(k,537)*y(k,138) + rxt(k,545) &
                      *y(k,2) + rxt(k,551)*y(k,4) + rxt(k,552)*y(k,20) + rxt(k,553) &
                      *y(k,111) + rxt(k,554)*y(k,113) + rxt(k,555)*y(k,114) + rxt(k,571) &
                      *y(k,118) + rxt(k,572)*y(k,119) + rxt(k,581)*y(k,121) + rxt(k,583) &
                      *y(k,115) + rxt(k,584)*y(k,123) + rxt(k,587)*y(k,142) + rxt(k,588) &
                      *y(k,143) + rxt(k,607)*y(k,247) + (rxt(k,611) + rxt(k,761) &
                      ) * y(k,10) + rxt(k,615)*y(k,13) + rxt(k,616)*y(k,27) + rxt(k,618) &
                      *y(k,29) + rxt(k,624)*y(k,41) + rxt(k,625)*y(k,77) + rxt(k,644) &
                      *y(k,173) + rxt(k,647)*y(k,174) + rxt(k,655)*y(k,215) + (rxt(k,656) &
                      + rxt(k,778)) * y(k,216) + (rxt(k,659) + rxt(k,781)) * y(k,251) &
                      + rxt(k,663)*y(k,253) + rxt(k,667)*y(k,255) + rxt(k,668) &
                      *y(k,257) + rxt(k,671)*y(k,8) + rxt(k,676)*y(k,136) + rxt(k,685) &
                      *y(k,156) + rxt(k,690)*y(k,208) + rxt(k,691)*y(k,209) + rxt(k,696) &
                      *y(k,211) + rxt(k,698)*y(k,212) + rxt(k,700)*y(k,213) + (rxt(k,708) &
                      + rxt(k,725)) * y(k,79) + rxt(k,711)*y(k,166) + rxt(k,716) &
                      *y(k,181) + rxt(k,721)*y(k,183) + rxt(k,726)*y(k,184) + rxt(k,728) &
                      *y(k,147))
         mat(k,1558) = -rxt(k,185)*y(k,167)
         mat(k,733) = -rxt(k,186)*y(k,167)
         mat(k,2097) = -rxt(k,187)*y(k,167)
         mat(k,2732) = -rxt(k,188)*y(k,167)
         mat(k,2611) = -rxt(k,189)*y(k,167)
         mat(k,519) = -rxt(k,195)*y(k,167)
         mat(k,2163) = -rxt(k,210)*y(k,167)
         mat(k,692) = -rxt(k,213)*y(k,167)
         mat(k,2664) = -rxt(k,227)*y(k,167)
         mat(k,1815) = -rxt(k,233)*y(k,167)
         mat(k,1024) = -rxt(k,276)*y(k,167)
         mat(k,2195) = -(rxt(k,279) + rxt(k,280)) * y(k,167)
         mat(k,2693) = -rxt(k,286)*y(k,167)
         mat(k,973) = -rxt(k,290)*y(k,167)
         mat(k,2500) = -rxt(k,328)*y(k,167)
         mat(k,985) = -rxt(k,333)*y(k,167)
         mat(k,586) = -rxt(k,358)*y(k,167)
         mat(k,718) = -rxt(k,360)*y(k,167)
         mat(k,127) = -rxt(k,361)*y(k,167)
         mat(k,454) = -rxt(k,363)*y(k,167)
         mat(k,542) = -rxt(k,365)*y(k,167)
         mat(k,376) = -rxt(k,366)*y(k,167)
         mat(k,336) = -rxt(k,367)*y(k,167)
         mat(k,404) = -rxt(k,368)*y(k,167)
         mat(k,2224) = -rxt(k,382)*y(k,167)
         mat(k,915) = -rxt(k,390)*y(k,167)
         mat(k,526) = -rxt(k,391)*y(k,167)
         mat(k,1782) = -rxt(k,392)*y(k,167)
         mat(k,1446) = -(rxt(k,393) + rxt(k,404)) * y(k,167)
         mat(k,249) = -rxt(k,394)*y(k,167)
         mat(k,1177) = -rxt(k,395)*y(k,167)
         mat(k,353) = -rxt(k,436)*y(k,167)
         mat(k,301) = -rxt(k,445)*y(k,167)
         mat(k,421) = -rxt(k,446)*y(k,167)
         mat(k,357) = -rxt(k,448)*y(k,167)
         mat(k,1365) = -rxt(k,450)*y(k,167)
         mat(k,111) = -rxt(k,452)*y(k,167)
         mat(k,810) = -rxt(k,458)*y(k,167)
         mat(k,727) = -rxt(k,459)*y(k,167)
         mat(k,1373) = -rxt(k,465)*y(k,167)
         mat(k,1237) = -rxt(k,466)*y(k,167)
         mat(k,622) = -rxt(k,467)*y(k,167)
         mat(k,614) = -rxt(k,468)*y(k,167)
         mat(k,502) = -rxt(k,482)*y(k,167)
         mat(k,119) = -rxt(k,483)*y(k,167)
         mat(k,1544) = -rxt(k,485)*y(k,167)
         mat(k,1421) = -rxt(k,487)*y(k,167)
         mat(k,1007) = -rxt(k,488)*y(k,167)
         mat(k,639) = -rxt(k,492)*y(k,167)
         mat(k,496) = -rxt(k,497)*y(k,167)
         mat(k,1303) = -rxt(k,498)*y(k,167)
         mat(k,1171) = -rxt(k,499)*y(k,167)
         mat(k,564) = -rxt(k,502)*y(k,167)
         mat(k,1438) = -rxt(k,508)*y(k,167)
         mat(k,1530) = -rxt(k,519)*y(k,167)
         mat(k,387) = -rxt(k,520)*y(k,167)
         mat(k,575) = -rxt(k,533)*y(k,167)
         mat(k,392) = -rxt(k,534)*y(k,167)
         mat(k,682) = -rxt(k,535)*y(k,167)
         mat(k,1683) = -rxt(k,537)*y(k,167)
         mat(k,805) = -rxt(k,545)*y(k,167)
         mat(k,795) = -rxt(k,551)*y(k,167)
         mat(k,267) = -rxt(k,552)*y(k,167)
         mat(k,255) = -rxt(k,553)*y(k,167)
         mat(k,433) = -rxt(k,554)*y(k,167)
         mat(k,144) = -rxt(k,555)*y(k,167)
         mat(k,744) = -rxt(k,571)*y(k,167)
         mat(k,605) = -rxt(k,572)*y(k,167)
         mat(k,490) = -rxt(k,581)*y(k,167)
         mat(k,1065) = -rxt(k,583)*y(k,167)
         mat(k,829) = -rxt(k,584)*y(k,167)
         mat(k,479) = -rxt(k,587)*y(k,167)
         mat(k,1352) = -rxt(k,588)*y(k,167)
         mat(k,242) = -rxt(k,607)*y(k,167)
         mat(k,201) = -(rxt(k,611) + rxt(k,761)) * y(k,167)
         mat(k,515) = -rxt(k,615)*y(k,167)
         mat(k,261) = -rxt(k,616)*y(k,167)
         mat(k,398) = -rxt(k,618)*y(k,167)
         mat(k,310) = -rxt(k,624)*y(k,167)
         mat(k,223) = -rxt(k,625)*y(k,167)
         mat(k,210) = -rxt(k,644)*y(k,167)
         mat(k,416) = -rxt(k,647)*y(k,167)
         mat(k,784) = -rxt(k,655)*y(k,167)
         mat(k,218) = -(rxt(k,656) + rxt(k,778)) * y(k,167)
         mat(k,288) = -(rxt(k,659) + rxt(k,781)) * y(k,167)
         mat(k,856) = -rxt(k,663)*y(k,167)
         mat(k,293) = -rxt(k,667)*y(k,167)
         mat(k,533) = -rxt(k,668)*y(k,167)
         mat(k,1215) = -rxt(k,671)*y(k,167)
         mat(k,1275) = -rxt(k,676)*y(k,167)
         mat(k,484) = -rxt(k,685)*y(k,167)
         mat(k,771) = -rxt(k,690)*y(k,167)
         mat(k,760) = -rxt(k,691)*y(k,167)
         mat(k,551) = -rxt(k,696)*y(k,167)
         mat(k,1231) = -rxt(k,698)*y(k,167)
         mat(k,1336) = -rxt(k,700)*y(k,167)
         mat(k,464) = -(rxt(k,708) + rxt(k,725)) * y(k,167)
         mat(k,447) = -rxt(k,711)*y(k,167)
         mat(k,595) = -rxt(k,716)*y(k,167)
         mat(k,1463) = -rxt(k,721)*y(k,167)
         mat(k,1013) = -rxt(k,726)*y(k,167)
         mat(k,113) = -rxt(k,728)*y(k,167)

         mat(k,820) = .400_r8*rxt(k,608)*y(k,105)
         mat(k,1215) = mat(k,1215) + .630_r8*rxt(k,670)*y(k,163)
         mat(k,353) = mat(k,353) + .650_r8*rxt(k,436)*y(k,167)
         mat(k,614) = mat(k,614) + .130_r8*rxt(k,438)*y(k,163)
         mat(k,421) = mat(k,421) + .500_r8*rxt(k,446)*y(k,167)
         mat(k,1303) = mat(k,1303) + .360_r8*rxt(k,475)*y(k,163)
         mat(k,2224) = mat(k,2224) + rxt(k,381)*y(k,161)
         mat(k,1748) = .450_r8*rxt(k,455)*y(k,105)
         mat(k,526) = mat(k,526) + .300_r8*rxt(k,391)*y(k,167)
         mat(k,1782) = mat(k,1782) + rxt(k,400)*y(k,162)
         mat(k,2270) = rxt(k,265)*y(k,105)
         mat(k,943) = .400_r8*rxt(k,626)*y(k,105)
         mat(k,1128) = rxt(k,350)*y(k,260)
         mat(k,1798) = 2.000_r8*rxt(k,176)*y(k,105) + rxt(k,183)*y(k,163)
         mat(k,1558) = mat(k,1558) + rxt(k,171)*y(k,161) + rxt(k,153)*y(k,162)
         mat(k,733) = mat(k,733) + rxt(k,172)*y(k,161)
         mat(k,985) = mat(k,985) + rxt(k,332)*y(k,161) + rxt(k,340)*y(k,162)
         mat(k,2693) = mat(k,2693) + rxt(k,285)*y(k,161) + rxt(k,299)*y(k,162)
         mat(k,249) = mat(k,249) + rxt(k,403)*y(k,162)
         mat(k,2097) = mat(k,2097) + .400_r8*rxt(k,608)*y(k,1) + .450_r8*rxt(k,455) &
                      *y(k,57) + rxt(k,265)*y(k,67) + .400_r8*rxt(k,626)*y(k,78)  &
                      + 2.000_r8*rxt(k,176)*y(k,91) + .450_r8*rxt(k,524)*y(k,130)  &
                      + .400_r8*rxt(k,636)*y(k,131) + .200_r8*rxt(k,530)*y(k,133)  &
                      + rxt(k,214)*y(k,151) + rxt(k,207)*y(k,153) + rxt(k,179) &
                      *y(k,161) + rxt(k,180)*y(k,163) + .150_r8*rxt(k,494)*y(k,179)
         mat(k,882) = rxt(k,335)*y(k,161)
         mat(k,973) = mat(k,973) + rxt(k,289)*y(k,161)
         mat(k,1065) = mat(k,1065) + .320_r8*rxt(k,582)*y(k,163)
         mat(k,829) = mat(k,829) + .600_r8*rxt(k,584)*y(k,167)
         mat(k,1530) = mat(k,1530) + .240_r8*rxt(k,518)*y(k,163)
         mat(k,387) = mat(k,387) + .100_r8*rxt(k,520)*y(k,167)
         mat(k,1708) = .450_r8*rxt(k,524)*y(k,105)
         mat(k,1115) = .400_r8*rxt(k,636)*y(k,105)
         mat(k,865) = .200_r8*rxt(k,530)*y(k,105)
         mat(k,1275) = mat(k,1275) + .630_r8*rxt(k,675)*y(k,163)
         mat(k,1683) = mat(k,1683) + .360_r8*rxt(k,536)*y(k,163)
         mat(k,1978) = rxt(k,214)*y(k,105)
         mat(k,2163) = mat(k,2163) + rxt(k,207)*y(k,105)
         mat(k,2732) = mat(k,2732) + rxt(k,381)*y(k,51) + rxt(k,171)*y(k,92)  &
                      + rxt(k,172)*y(k,94) + rxt(k,332)*y(k,96) + rxt(k,285)*y(k,100)  &
                      + rxt(k,179)*y(k,105) + rxt(k,335)*y(k,107) + rxt(k,289) &
                      *y(k,109)
         mat(k,2544) = rxt(k,400)*y(k,65) + rxt(k,153)*y(k,92) + rxt(k,340)*y(k,96)  &
                      + rxt(k,299)*y(k,100) + rxt(k,403)*y(k,101)  &
                      + 2.000_r8*rxt(k,154)*y(k,260)
         mat(k,2611) = mat(k,2611) + .630_r8*rxt(k,670)*y(k,8) + .130_r8*rxt(k,438) &
                      *y(k,31) + .360_r8*rxt(k,475)*y(k,36) + rxt(k,183)*y(k,91)  &
                      + rxt(k,180)*y(k,105) + .320_r8*rxt(k,582)*y(k,115)  &
                      + .240_r8*rxt(k,518)*y(k,126) + .630_r8*rxt(k,675)*y(k,136)  &
                      + .360_r8*rxt(k,536)*y(k,138)
         mat(k,2471) = mat(k,2471) + .650_r8*rxt(k,436)*y(k,30) + .500_r8*rxt(k,446) &
                      *y(k,34) + .300_r8*rxt(k,391)*y(k,64) + .600_r8*rxt(k,584) &
                      *y(k,123) + .100_r8*rxt(k,520)*y(k,128) + .500_r8*rxt(k,492) &
                      *y(k,178) + .500_r8*rxt(k,607)*y(k,247)
         mat(k,639) = mat(k,639) + .500_r8*rxt(k,492)*y(k,167)
         mat(k,1413) = .150_r8*rxt(k,494)*y(k,105)
         mat(k,242) = mat(k,242) + .500_r8*rxt(k,607)*y(k,167)
         mat(k,2760) = rxt(k,350)*y(k,88) + 2.000_r8*rxt(k,154)*y(k,162)

      end do

      end subroutine     nlnmat08

      subroutine     nlnmat09( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len

         mat(k,1422) = rxt(k,508)*y(k,167)
         mat(k,1897) = .100_r8*rxt(k,688)*y(k,207)
         mat(k,2327) = rxt(k,508)*y(k,110)
         mat(k,1375) = .100_r8*rxt(k,688)*y(k,151)

         mat(k,616) = -(rxt(k,467)*y(k,167))
         mat(k,2381) = -rxt(k,467)*y(k,169)

         mat(k,1717) = rxt(k,469)*y(k,152)
         mat(k,2628) = rxt(k,469)*y(k,57)


         mat(k,813) = rxt(k,650)*y(k,152)
         mat(k,2619) = rxt(k,650)*y(k,1)

         mat(k,670) = -(rxt(k,645)*y(k,152) + rxt(k,646)*y(k,163))
         mat(k,2630) = -rxt(k,645)*y(k,171)
         mat(k,2563) = -rxt(k,646)*y(k,171)

         mat(k,868) = rxt(k,622)*y(k,151)
         mat(k,221) = .070_r8*rxt(k,625)*y(k,167)
         mat(k,1914) = rxt(k,622)*y(k,40)
         mat(k,2385) = .070_r8*rxt(k,625)*y(k,77) + .060_r8*rxt(k,644)*y(k,173)  &
                      + .070_r8*rxt(k,667)*y(k,255)
         mat(k,208) = .060_r8*rxt(k,644)*y(k,167)
         mat(k,290) = .070_r8*rxt(k,667)*y(k,167)

         mat(k,625) = -(rxt(k,641)*y(k,105) + rxt(k,642)*y(k,151))
         mat(k,2025) = -rxt(k,641)*y(k,172)
         mat(k,1911) = -rxt(k,642)*y(k,172)

         mat(k,220) = .200_r8*rxt(k,625)*y(k,167)
         mat(k,2382) = .200_r8*rxt(k,625)*y(k,77) + .140_r8*rxt(k,644)*y(k,173)  &
                      + rxt(k,647)*y(k,174)
         mat(k,207) = .140_r8*rxt(k,644)*y(k,167)
         mat(k,413) = rxt(k,647)*y(k,167)

         mat(k,206) = -(rxt(k,644)*y(k,167))
         mat(k,2314) = -rxt(k,644)*y(k,173)

         mat(k,197) = .530_r8*rxt(k,611)*y(k,167)
         mat(k,2314) = mat(k,2314) + .530_r8*rxt(k,611)*y(k,10)

         mat(k,412) = -(rxt(k,647)*y(k,167))
         mat(k,2349) = -rxt(k,647)*y(k,174)

         mat(k,2009) = rxt(k,641)*y(k,172)
         mat(k,624) = rxt(k,641)*y(k,105)

         mat(k,946) = -(rxt(k,489)*y(k,105) + rxt(k,490)*y(k,151))
         mat(k,2048) = -rxt(k,489)*y(k,175)
         mat(k,1930) = -rxt(k,490)*y(k,175)

         mat(k,1284) = rxt(k,498)*y(k,167)
         mat(k,2412) = rxt(k,498)*y(k,36) + .500_r8*rxt(k,492)*y(k,178)
         mat(k,634) = .500_r8*rxt(k,492)*y(k,167)



         mat(k,633) = -(rxt(k,492)*y(k,167))
         mat(k,2383) = -rxt(k,492)*y(k,178)

         mat(k,2026) = rxt(k,489)*y(k,175)
         mat(k,945) = rxt(k,489)*y(k,105)

         mat(k,1405) = -(rxt(k,493)*y(k,62) + rxt(k,494)*y(k,105) + rxt(k,495) &
                      *y(k,151))
         mat(k,1853) = -rxt(k,493)*y(k,179)
         mat(k,2072) = -rxt(k,494)*y(k,179)
         mat(k,1953) = -rxt(k,495)*y(k,179)

         mat(k,1205) = .060_r8*rxt(k,670)*y(k,163)
         mat(k,1168) = rxt(k,499)*y(k,167)
         mat(k,1265) = .060_r8*rxt(k,675)*y(k,163)
         mat(k,2587) = .060_r8*rxt(k,670)*y(k,8) + .060_r8*rxt(k,675)*y(k,136)
         mat(k,2444) = rxt(k,499)*y(k,58) + rxt(k,497)*y(k,180) + .150_r8*rxt(k,700) &
                      *y(k,213)
         mat(k,493) = rxt(k,497)*y(k,167)
         mat(k,1330) = .150_r8*rxt(k,700)*y(k,167)

         mat(k,492) = -(rxt(k,497)*y(k,167))
         mat(k,2362) = -rxt(k,497)*y(k,180)

         mat(k,2014) = .850_r8*rxt(k,494)*y(k,179)
         mat(k,1400) = .850_r8*rxt(k,494)*y(k,105)

         mat(k,592) = -(rxt(k,713)*y(k,163) + rxt(k,716)*y(k,167))
         mat(k,2561) = -rxt(k,713)*y(k,181)
         mat(k,2378) = -rxt(k,716)*y(k,181)


         mat(k,1456) = -(rxt(k,714)*y(k,24) + rxt(k,715)*y(k,70) + rxt(k,717)*y(k,152) &
                      + rxt(k,719)*y(k,163) + rxt(k,720)*y(k,165) + rxt(k,721) &
                      *y(k,167))
         mat(k,2489) = -rxt(k,714)*y(k,183)
         mat(k,2184) = -rxt(k,715)*y(k,183)
         mat(k,2649) = -rxt(k,717)*y(k,183)
         mat(k,2590) = -rxt(k,719)*y(k,183)
         mat(k,409) = -rxt(k,720)*y(k,183)
         mat(k,2448) = -rxt(k,721)*y(k,183)

         mat(k,2719) = rxt(k,710)*y(k,166)
         mat(k,2590) = mat(k,2590) + rxt(k,713)*y(k,181)
         mat(k,445) = rxt(k,710)*y(k,161)
         mat(k,2448) = mat(k,2448) + rxt(k,716)*y(k,181)
         mat(k,593) = rxt(k,713)*y(k,163) + rxt(k,716)*y(k,167)

         mat(k,1010) = -(rxt(k,726)*y(k,167))
         mat(k,2419) = -rxt(k,726)*y(k,184)

         mat(k,2485) = rxt(k,714)*y(k,183)
         mat(k,2178) = rxt(k,715)*y(k,183)
         mat(k,458) = rxt(k,707)*y(k,153) + (rxt(k,708)+.500_r8*rxt(k,725))*y(k,167)
         mat(k,2639) = rxt(k,717)*y(k,183)
         mat(k,2118) = rxt(k,707)*y(k,79)
         mat(k,2568) = rxt(k,719)*y(k,183)
         mat(k,408) = rxt(k,720)*y(k,183)
         mat(k,443) = rxt(k,711)*y(k,167)
         mat(k,2419) = mat(k,2419) + (rxt(k,708)+.500_r8*rxt(k,725))*y(k,79)  &
                      + rxt(k,711)*y(k,166) + rxt(k,721)*y(k,183)
         mat(k,1452) = rxt(k,714)*y(k,24) + rxt(k,715)*y(k,70) + rxt(k,717)*y(k,152)  &
                      + rxt(k,719)*y(k,163) + rxt(k,720)*y(k,165) + rxt(k,721) &
                      *y(k,167)

         mat(k,303) = -(rxt(k,727)*y(k,260))
         mat(k,2741) = -rxt(k,727)*y(k,185)

         mat(k,2334) = rxt(k,726)*y(k,184)
         mat(k,1009) = rxt(k,726)*y(k,167)















         mat(k,1180) = .2202005_r8*rxt(k,759)*y(k,163)
         mat(k,55) = .2202005_r8*rxt(k,757)*y(k,105) + .1279005_r8*rxt(k,758)*y(k,151)
         mat(k,61) = .0023005_r8*rxt(k,762)*y(k,105) + .0097005_r8*rxt(k,763)*y(k,151)
         mat(k,1986) = .2202005_r8*rxt(k,757)*y(k,9) + .0023005_r8*rxt(k,762)*y(k,12)  &
                      + .0031005_r8*rxt(k,765)*y(k,122) + .2381005_r8*rxt(k,769) &
                      *y(k,125) + .0508005_r8*rxt(k,773)*y(k,137)  &
                      + .1364005_r8*rxt(k,779)*y(k,217) + .1677005_r8*rxt(k,782) &
                      *y(k,254)
         mat(k,67) = .0031005_r8*rxt(k,765)*y(k,105) + .0003005_r8*rxt(k,766)*y(k,151)
         mat(k,73) = .2381005_r8*rxt(k,769)*y(k,105) + .1056005_r8*rxt(k,770)*y(k,151)
         mat(k,1239) = .0508005_r8*rxt(k,775)*y(k,163)
         mat(k,81) = .0508005_r8*rxt(k,773)*y(k,105) + .0245005_r8*rxt(k,774)*y(k,151)
         mat(k,1883) = .1279005_r8*rxt(k,758)*y(k,9) + .0097005_r8*rxt(k,763)*y(k,12)  &
                      + .0003005_r8*rxt(k,766)*y(k,122) + .1056005_r8*rxt(k,770) &
                      *y(k,125) + .0245005_r8*rxt(k,774)*y(k,137)  &
                      + .0154005_r8*rxt(k,780)*y(k,217) + .0063005_r8*rxt(k,783) &
                      *y(k,254)
         mat(k,2552) = .2202005_r8*rxt(k,759)*y(k,8) + .0508005_r8*rxt(k,775)*y(k,136)
         mat(k,2291) = .5931005_r8*rxt(k,777)*y(k,205)
         mat(k,88) = .5931005_r8*rxt(k,777)*y(k,167)
         mat(k,94) = .1364005_r8*rxt(k,779)*y(k,105) + .0154005_r8*rxt(k,780)*y(k,151)
         mat(k,100) = .1677005_r8*rxt(k,782)*y(k,105) + .0063005_r8*rxt(k,783) &
                      *y(k,151)


         mat(k,1181) = .2067005_r8*rxt(k,759)*y(k,163)
         mat(k,56) = .2067005_r8*rxt(k,757)*y(k,105) + .1792005_r8*rxt(k,758)*y(k,151)
         mat(k,62) = .0008005_r8*rxt(k,762)*y(k,105) + .0034005_r8*rxt(k,763)*y(k,151)
         mat(k,1987) = .2067005_r8*rxt(k,757)*y(k,9) + .0008005_r8*rxt(k,762)*y(k,12)  &
                      + .0035005_r8*rxt(k,765)*y(k,122) + .1308005_r8*rxt(k,769) &
                      *y(k,125) + .1149005_r8*rxt(k,773)*y(k,137)  &
                      + .0101005_r8*rxt(k,779)*y(k,217) + .0174005_r8*rxt(k,782) &
                      *y(k,254)
         mat(k,68) = .0035005_r8*rxt(k,765)*y(k,105) + .0003005_r8*rxt(k,766)*y(k,151)
         mat(k,74) = .1308005_r8*rxt(k,769)*y(k,105) + .1026005_r8*rxt(k,770)*y(k,151)
         mat(k,1240) = .1149005_r8*rxt(k,775)*y(k,163)
         mat(k,82) = .1149005_r8*rxt(k,773)*y(k,105) + .0082005_r8*rxt(k,774)*y(k,151)
         mat(k,1884) = .1792005_r8*rxt(k,758)*y(k,9) + .0034005_r8*rxt(k,763)*y(k,12)  &
                      + .0003005_r8*rxt(k,766)*y(k,122) + .1026005_r8*rxt(k,770) &
                      *y(k,125) + .0082005_r8*rxt(k,774)*y(k,137)  &
                      + .0452005_r8*rxt(k,780)*y(k,217) + .0237005_r8*rxt(k,783) &
                      *y(k,254)
         mat(k,2553) = .2067005_r8*rxt(k,759)*y(k,8) + .1149005_r8*rxt(k,775)*y(k,136)
         mat(k,2292) = .1534005_r8*rxt(k,777)*y(k,205)
         mat(k,89) = .1534005_r8*rxt(k,777)*y(k,167)
         mat(k,95) = .0101005_r8*rxt(k,779)*y(k,105) + .0452005_r8*rxt(k,780)*y(k,151)
         mat(k,101) = .0174005_r8*rxt(k,782)*y(k,105) + .0237005_r8*rxt(k,783) &
                      *y(k,151)


         mat(k,1182) = .0653005_r8*rxt(k,759)*y(k,163)
         mat(k,57) = .0653005_r8*rxt(k,757)*y(k,105) + .0676005_r8*rxt(k,758)*y(k,151)
         mat(k,63) = .0843005_r8*rxt(k,762)*y(k,105) + .1579005_r8*rxt(k,763)*y(k,151)
         mat(k,1988) = .0653005_r8*rxt(k,757)*y(k,9) + .0843005_r8*rxt(k,762)*y(k,12)  &
                      + .0003005_r8*rxt(k,765)*y(k,122) + .0348005_r8*rxt(k,769) &
                      *y(k,125) + .0348005_r8*rxt(k,773)*y(k,137)  &
                      + .0763005_r8*rxt(k,779)*y(k,217) + .086_r8*rxt(k,782)*y(k,254)
         mat(k,69) = .0003005_r8*rxt(k,765)*y(k,105) + .0073005_r8*rxt(k,766)*y(k,151)
         mat(k,75) = .0348005_r8*rxt(k,769)*y(k,105) + .0521005_r8*rxt(k,770)*y(k,151)
         mat(k,1241) = .0348005_r8*rxt(k,775)*y(k,163)
         mat(k,83) = .0348005_r8*rxt(k,773)*y(k,105) + .0772005_r8*rxt(k,774)*y(k,151)
         mat(k,1885) = .0676005_r8*rxt(k,758)*y(k,9) + .1579005_r8*rxt(k,763)*y(k,12)  &
                      + .0073005_r8*rxt(k,766)*y(k,122) + .0521005_r8*rxt(k,770) &
                      *y(k,125) + .0772005_r8*rxt(k,774)*y(k,137)  &
                      + .0966005_r8*rxt(k,780)*y(k,217) + .0025005_r8*rxt(k,783) &
                      *y(k,254)
         mat(k,2554) = .0653005_r8*rxt(k,759)*y(k,8) + .0348005_r8*rxt(k,775)*y(k,136)
         mat(k,2293) = .0459005_r8*rxt(k,777)*y(k,205)
         mat(k,90) = .0459005_r8*rxt(k,777)*y(k,167)
         mat(k,96) = .0763005_r8*rxt(k,779)*y(k,105) + .0966005_r8*rxt(k,780)*y(k,151)
         mat(k,102) = .086_r8*rxt(k,782)*y(k,105) + .0025005_r8*rxt(k,783)*y(k,151)


         mat(k,1183) = .1749305_r8*rxt(k,756)*y(k,153) + .1284005_r8*rxt(k,759) &
                      *y(k,163)
         mat(k,58) = .1284005_r8*rxt(k,757)*y(k,105) + .079_r8*rxt(k,758)*y(k,151)
         mat(k,64) = .0443005_r8*rxt(k,762)*y(k,105) + .0059005_r8*rxt(k,763)*y(k,151)
         mat(k,1989) = .1284005_r8*rxt(k,757)*y(k,9) + .0443005_r8*rxt(k,762)*y(k,12)  &
                      + .0271005_r8*rxt(k,765)*y(k,122) + .0076005_r8*rxt(k,769) &
                      *y(k,125) + .0554005_r8*rxt(k,773)*y(k,137)  &
                      + .2157005_r8*rxt(k,779)*y(k,217) + .0512005_r8*rxt(k,782) &
                      *y(k,254)
         mat(k,1042) = .0590245_r8*rxt(k,764)*y(k,153) + .0033005_r8*rxt(k,767) &
                      *y(k,163)
         mat(k,70) = .0271005_r8*rxt(k,765)*y(k,105) + .0057005_r8*rxt(k,766)*y(k,151)
         mat(k,76) = .0076005_r8*rxt(k,769)*y(k,105) + .0143005_r8*rxt(k,770)*y(k,151)
         mat(k,1242) = .1749305_r8*rxt(k,772)*y(k,153) + .0554005_r8*rxt(k,775) &
                      *y(k,163)
         mat(k,84) = .0554005_r8*rxt(k,773)*y(k,105) + .0332005_r8*rxt(k,774)*y(k,151)
         mat(k,1886) = .079_r8*rxt(k,758)*y(k,9) + .0059005_r8*rxt(k,763)*y(k,12)  &
                      + .0057005_r8*rxt(k,766)*y(k,122) + .0143005_r8*rxt(k,770) &
                      *y(k,125) + .0332005_r8*rxt(k,774)*y(k,137)  &
                      + .0073005_r8*rxt(k,780)*y(k,217) + .011_r8*rxt(k,783)*y(k,254)
         mat(k,2105) = .1749305_r8*rxt(k,756)*y(k,8) + .0590245_r8*rxt(k,764)*y(k,115)  &
                      + .1749305_r8*rxt(k,772)*y(k,136)
         mat(k,2555) = .1284005_r8*rxt(k,759)*y(k,8) + .0033005_r8*rxt(k,767)*y(k,115)  &
                      + .0554005_r8*rxt(k,775)*y(k,136)
         mat(k,2294) = .0085005_r8*rxt(k,777)*y(k,205)
         mat(k,91) = .0085005_r8*rxt(k,777)*y(k,167)
         mat(k,97) = .2157005_r8*rxt(k,779)*y(k,105) + .0073005_r8*rxt(k,780)*y(k,151)
         mat(k,103) = .0512005_r8*rxt(k,782)*y(k,105) + .011_r8*rxt(k,783)*y(k,151)


         mat(k,1184) = .5901905_r8*rxt(k,756)*y(k,153) + .114_r8*rxt(k,759)*y(k,163)
         mat(k,59) = .114_r8*rxt(k,757)*y(k,105) + .1254005_r8*rxt(k,758)*y(k,151)
         mat(k,65) = .1621005_r8*rxt(k,762)*y(k,105) + .0536005_r8*rxt(k,763)*y(k,151)
         mat(k,1990) = .114_r8*rxt(k,757)*y(k,9) + .1621005_r8*rxt(k,762)*y(k,12)  &
                      + .0474005_r8*rxt(k,765)*y(k,122) + .0113005_r8*rxt(k,769) &
                      *y(k,125) + .1278005_r8*rxt(k,773)*y(k,137)  &
                      + .0738005_r8*rxt(k,779)*y(k,217) + .1598005_r8*rxt(k,782) &
                      *y(k,254)
         mat(k,1043) = .0250245_r8*rxt(k,764)*y(k,153)
         mat(k,71) = .0474005_r8*rxt(k,765)*y(k,105) + .0623005_r8*rxt(k,766)*y(k,151)
         mat(k,77) = .0113005_r8*rxt(k,769)*y(k,105) + .0166005_r8*rxt(k,770)*y(k,151)
         mat(k,1243) = .5901905_r8*rxt(k,772)*y(k,153) + .1278005_r8*rxt(k,775) &
                      *y(k,163)
         mat(k,85) = .1278005_r8*rxt(k,773)*y(k,105) + .130_r8*rxt(k,774)*y(k,151)
         mat(k,1887) = .1254005_r8*rxt(k,758)*y(k,9) + .0536005_r8*rxt(k,763)*y(k,12)  &
                      + .0623005_r8*rxt(k,766)*y(k,122) + .0166005_r8*rxt(k,770) &
                      *y(k,125) + .130_r8*rxt(k,774)*y(k,137) + .238_r8*rxt(k,780) &
                      *y(k,217) + .1185005_r8*rxt(k,783)*y(k,254)
         mat(k,2106) = .5901905_r8*rxt(k,756)*y(k,8) + .0250245_r8*rxt(k,764)*y(k,115)  &
                      + .5901905_r8*rxt(k,772)*y(k,136)
         mat(k,2556) = .114_r8*rxt(k,759)*y(k,8) + .1278005_r8*rxt(k,775)*y(k,136)
         mat(k,2295) = .0128005_r8*rxt(k,777)*y(k,205)
         mat(k,92) = .0128005_r8*rxt(k,777)*y(k,167)
         mat(k,98) = .0738005_r8*rxt(k,779)*y(k,105) + .238_r8*rxt(k,780)*y(k,151)
         mat(k,104) = .1598005_r8*rxt(k,782)*y(k,105) + .1185005_r8*rxt(k,783) &
                      *y(k,151)

      end do

      end subroutine     nlnmat09

      subroutine     nlnmat10( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len

         mat(k,93) = -(rxt(k,777)*y(k,167))
         mat(k,2302) = -rxt(k,777)*y(k,205)


         mat(k,2317) = .100_r8*rxt(k,656)*y(k,216) + .230_r8*rxt(k,659)*y(k,251)
         mat(k,214) = .100_r8*rxt(k,656)*y(k,167)
         mat(k,279) = .230_r8*rxt(k,659)*y(k,167)

         mat(k,1387) = -(rxt(k,686)*y(k,62) + rxt(k,687)*y(k,105) + rxt(k,688) &
                      *y(k,151))
         mat(k,1852) = -rxt(k,686)*y(k,207)
         mat(k,2071) = -rxt(k,687)*y(k,207)
         mat(k,1952) = -rxt(k,688)*y(k,207)

         mat(k,2136) = .500_r8*rxt(k,697)*y(k,212)
         mat(k,2443) = rxt(k,690)*y(k,208) + rxt(k,698)*y(k,212)
         mat(k,767) = rxt(k,690)*y(k,167)
         mat(k,1227) = .500_r8*rxt(k,697)*y(k,153) + rxt(k,698)*y(k,167)

         mat(k,762) = -(rxt(k,690)*y(k,167))
         mat(k,2396) = -rxt(k,690)*y(k,208)

         mat(k,2034) = rxt(k,687)*y(k,207)
         mat(k,1377) = rxt(k,687)*y(k,105)

         mat(k,756) = -(rxt(k,691)*y(k,167))
         mat(k,2395) = -rxt(k,691)*y(k,209)

         mat(k,1832) = .500_r8*rxt(k,679)*y(k,155)
         mat(k,1920) = .200_r8*rxt(k,681)*y(k,155) + .200_r8*rxt(k,694)*y(k,210)
         mat(k,1309) = .500_r8*rxt(k,679)*y(k,62) + .200_r8*rxt(k,681)*y(k,151)
         mat(k,1132) = .200_r8*rxt(k,694)*y(k,151)

         mat(k,1138) = -(rxt(k,692)*y(k,62) + rxt(k,693)*y(k,105) + rxt(k,694) &
                      *y(k,151))
         mat(k,1843) = -rxt(k,692)*y(k,210)
         mat(k,2061) = -rxt(k,693)*y(k,210)
         mat(k,1941) = -rxt(k,694)*y(k,210)

         mat(k,1195) = rxt(k,671)*y(k,167)
         mat(k,1254) = rxt(k,676)*y(k,167)
         mat(k,2429) = rxt(k,671)*y(k,8) + rxt(k,676)*y(k,136) + rxt(k,696)*y(k,211)
         mat(k,546) = rxt(k,696)*y(k,167)

         mat(k,545) = -(rxt(k,696)*y(k,167))
         mat(k,2370) = -rxt(k,696)*y(k,211)

         mat(k,2021) = rxt(k,693)*y(k,210)
         mat(k,1131) = rxt(k,693)*y(k,105)

         mat(k,1224) = -(rxt(k,697)*y(k,153) + rxt(k,698)*y(k,167))
         mat(k,2127) = -rxt(k,697)*y(k,212)
         mat(k,2434) = -rxt(k,698)*y(k,212)

         mat(k,1200) = .330_r8*rxt(k,670)*y(k,163)
         mat(k,1845) = .500_r8*rxt(k,679)*y(k,155) + rxt(k,692)*y(k,210)
         mat(k,1258) = .330_r8*rxt(k,675)*y(k,163)
         mat(k,1945) = .800_r8*rxt(k,681)*y(k,155) + .800_r8*rxt(k,694)*y(k,210)
         mat(k,2127) = mat(k,2127) + rxt(k,682)*y(k,155)
         mat(k,1314) = .500_r8*rxt(k,679)*y(k,62) + .800_r8*rxt(k,681)*y(k,151)  &
                      + rxt(k,682)*y(k,153)
         mat(k,2578) = .330_r8*rxt(k,670)*y(k,8) + .330_r8*rxt(k,675)*y(k,136)
         mat(k,2434) = mat(k,2434) + rxt(k,691)*y(k,209)
         mat(k,757) = rxt(k,691)*y(k,167)
         mat(k,1140) = rxt(k,692)*y(k,62) + .800_r8*rxt(k,694)*y(k,151)

         mat(k,1329) = -(rxt(k,700)*y(k,167))
         mat(k,2439) = -rxt(k,700)*y(k,213)

         mat(k,1203) = .300_r8*rxt(k,670)*y(k,163)
         mat(k,1848) = rxt(k,686)*y(k,207)
         mat(k,1262) = .300_r8*rxt(k,675)*y(k,163)
         mat(k,1948) = .900_r8*rxt(k,688)*y(k,207)
         mat(k,2583) = .300_r8*rxt(k,670)*y(k,8) + .300_r8*rxt(k,675)*y(k,136)
         mat(k,1385) = rxt(k,686)*y(k,62) + .900_r8*rxt(k,688)*y(k,151)

         mat(k,891) = -(rxt(k,652)*y(k,105) + rxt(k,653)*y(k,151))
         mat(k,2044) = -rxt(k,652)*y(k,214)
         mat(k,1926) = -rxt(k,653)*y(k,214)

         mat(k,199) = rxt(k,761)*y(k,167)
         mat(k,2407) = rxt(k,761)*y(k,10) + rxt(k,655)*y(k,215) + (.650_r8*rxt(k,656) &
                       +rxt(k,778))*y(k,216) + rxt(k,781)*y(k,251)
         mat(k,776) = rxt(k,655)*y(k,167)
         mat(k,216) = (.650_r8*rxt(k,656)+rxt(k,778))*y(k,167)
         mat(k,283) = rxt(k,781)*y(k,167)

         mat(k,775) = -(rxt(k,655)*y(k,167))
         mat(k,2397) = -rxt(k,655)*y(k,215)

         mat(k,2035) = rxt(k,652)*y(k,214)
         mat(k,890) = rxt(k,652)*y(k,105)

         mat(k,212) = -((rxt(k,656) + rxt(k,778)) * y(k,167))
         mat(k,2315) = -(rxt(k,656) + rxt(k,778)) * y(k,216)

         mat(k,99) = -(rxt(k,779)*y(k,105) + rxt(k,780)*y(k,151))
         mat(k,1996) = -rxt(k,779)*y(k,217)
         mat(k,1893) = -rxt(k,780)*y(k,217)

         mat(k,2303) = rxt(k,778)*y(k,216)
         mat(k,211) = rxt(k,778)*y(k,167)

         mat(k,129) = -(rxt(k,589)*y(k,167))
         mat(k,2309) = -rxt(k,589)*y(k,218)

         mat(k,1148) = rxt(k,550)*y(k,233)
         mat(k,1070) = rxt(k,550)*y(k,3)

         mat(k,311) = -(rxt(k,342)*y(k,161))
         mat(k,2701) = -rxt(k,342)*y(k,219)

         mat(k,2480) = rxt(k,329)*y(k,234)
         mat(k,1087) = rxt(k,329)*y(k,24)

         mat(k,552) = -(rxt(k,303)*y(k,67) + rxt(k,304)*y(k,161) + rxt(k,305)*y(k,167) &
                      + (rxt(k,807) + rxt(k,813) + rxt(k,818)) * y(k,100))
         mat(k,2237) = -rxt(k,303)*y(k,220)
         mat(k,2703) = -rxt(k,304)*y(k,220)
         mat(k,2371) = -rxt(k,305)*y(k,220)
         mat(k,2675) = -(rxt(k,807) + rxt(k,813) + rxt(k,818)) * y(k,220)

         mat(k,2175) = rxt(k,282)*y(k,234)
         mat(k,1089) = rxt(k,282)*y(k,70)

         mat(k,1067) = -(rxt(k,236)*y(k,167) + rxt(k,352)*y(k,88))
         mat(k,2423) = -rxt(k,236)*y(k,221)
         mat(k,1118) = -rxt(k,352)*y(k,221)

         mat(k,2206) = rxt(k,383)*y(k,237)
         mat(k,1355) = rxt(k,451)*y(k,237)
         mat(k,1534) = rxt(k,486)*y(k,237)
         mat(k,459) = rxt(k,709)*y(k,237)
         mat(k,2680) = (rxt(k,807)+rxt(k,813)+rxt(k,818))*y(k,220)
         mat(k,2423) = mat(k,2423) + rxt(k,231)*y(k,234)
         mat(k,553) = (rxt(k,807)+rxt(k,813)+rxt(k,818))*y(k,100)
         mat(k,1091) = rxt(k,231)*y(k,167)
         mat(k,1101) = rxt(k,383)*y(k,51) + rxt(k,451)*y(k,54) + rxt(k,486)*y(k,59)  &
                      + rxt(k,709)*y(k,79)

         mat(k,228) = -(rxt(k,241)*y(k,167))
         mat(k,2318) = -rxt(k,241)*y(k,222)

         mat(k,1999) = rxt(k,229)*y(k,234)
         mat(k,1083) = rxt(k,229)*y(k,105)

         mat(k,428) = -(rxt(k,543)*y(k,167))
         mat(k,2351) = -rxt(k,543)*y(k,223)

         mat(k,830) = rxt(k,507)*y(k,233)
         mat(k,1637) = rxt(k,517)*y(k,233)
         mat(k,2351) = mat(k,2351) + .300_r8*rxt(k,590)*y(k,224) + .500_r8*rxt(k,591) &
                      *y(k,225)
         mat(k,316) = .300_r8*rxt(k,590)*y(k,167)
         mat(k,321) = .500_r8*rxt(k,591)*y(k,167)
         mat(k,1075) = rxt(k,507)*y(k,84) + rxt(k,517)*y(k,127)

         mat(k,315) = -(rxt(k,590)*y(k,167))
         mat(k,2336) = -rxt(k,590)*y(k,224)

         mat(k,1563) = .080_r8*rxt(k,561)*y(k,233)
         mat(k,1073) = .080_r8*rxt(k,561)*y(k,116)

         mat(k,320) = -(rxt(k,591)*y(k,167))
         mat(k,2337) = -rxt(k,591)*y(k,225)

         mat(k,1598) = .080_r8*rxt(k,569)*y(k,233)
         mat(k,1074) = .080_r8*rxt(k,569)*y(k,117)

         mat(k,469) = -(rxt(k,592)*y(k,57) + rxt(k,593)*y(k,62) + rxt(k,594)*y(k,105) &
                      + rxt(k,595)*y(k,151) + rxt(k,596)*y(k,153))
         mat(k,1716) = -rxt(k,592)*y(k,226)
         mat(k,1828) = -rxt(k,593)*y(k,226)
         mat(k,2011) = -rxt(k,594)*y(k,226)
         mat(k,1904) = -rxt(k,595)*y(k,226)
         mat(k,2112) = -rxt(k,596)*y(k,226)

         mat(k,1045) = rxt(k,585)*y(k,237)
         mat(k,1098) = rxt(k,585)*y(k,115)

         mat(k,231) = -(rxt(k,597)*y(k,167))
         mat(k,2319) = -rxt(k,597)*y(k,227)

         mat(k,2000) = rxt(k,594)*y(k,226)
         mat(k,465) = rxt(k,594)*y(k,105)

         mat(k,234) = -(rxt(k,544)*y(k,167))
         mat(k,2320) = -rxt(k,544)*y(k,228)

         mat(k,1688) = rxt(k,540)*y(k,234)
         mat(k,1084) = rxt(k,540)*y(k,130)

         mat(k,163) = -(rxt(k,242)*y(k,151) + (rxt(k,243) + rxt(k,244) + rxt(k,245) &
                      ) * y(k,152) + rxt(k,254)*y(k,167))
         mat(k,1895) = -rxt(k,242)*y(k,229)
         mat(k,2620) = -(rxt(k,243) + rxt(k,244) + rxt(k,245)) * y(k,229)
         mat(k,2311) = -rxt(k,254)*y(k,229)

         mat(k,237) = -(rxt(k,598)*y(k,167))
         mat(k,2321) = -rxt(k,598)*y(k,230)

         mat(k,1822) = .200_r8*rxt(k,593)*y(k,226)
         mat(k,466) = .200_r8*rxt(k,593)*y(k,62)

         mat(k,435) = -(rxt(k,599)*y(k,167))
         mat(k,2353) = -rxt(k,599)*y(k,231)

         mat(k,1715) = rxt(k,592)*y(k,226)
         mat(k,1826) = .800_r8*rxt(k,593)*y(k,226)
         mat(k,1902) = rxt(k,595)*y(k,226)
         mat(k,2110) = rxt(k,596)*y(k,226)
         mat(k,468) = rxt(k,592)*y(k,57) + .800_r8*rxt(k,593)*y(k,62) + rxt(k,595) &
                      *y(k,151) + rxt(k,596)*y(k,153)

         mat(k,1079) = -(rxt(k,211)*y(k,153) + rxt(k,219)*y(k,139) + rxt(k,255) &
                      *y(k,105) + rxt(k,256)*y(k,163) + rxt(k,257)*y(k,161) + rxt(k,281) &
                      *y(k,70) + rxt(k,389)*y(k,62) + rxt(k,399)*y(k,108) + rxt(k,444) &
                      *y(k,32) + rxt(k,457)*y(k,57) + rxt(k,462)*y(k,86) + rxt(k,481) &
                      *y(k,37) + rxt(k,491)*y(k,175) + rxt(k,496)*y(k,179) + (rxt(k,506) &
                      + rxt(k,507)) * y(k,84) + (rxt(k,516) + rxt(k,517)) * y(k,127) &
                      + rxt(k,528)*y(k,130) + rxt(k,532)*y(k,133) + (rxt(k,549) &
                      + rxt(k,550)) * y(k,3) + rxt(k,561)*y(k,116) + rxt(k,569) &
                      *y(k,117) + rxt(k,579)*y(k,120) + rxt(k,605)*y(k,244) + rxt(k,610) &
                      *y(k,1) + rxt(k,614)*y(k,11) + rxt(k,620)*y(k,28) + rxt(k,623) &
                      *y(k,40) + rxt(k,629)*y(k,78) + rxt(k,634)*y(k,129) + rxt(k,639) &
                      *y(k,131) + rxt(k,643)*y(k,172) + rxt(k,654)*y(k,214) + rxt(k,662) &
                      *y(k,252) + rxt(k,666)*y(k,256) + rxt(k,683)*y(k,155) + rxt(k,689) &
                      *y(k,207) + rxt(k,695)*y(k,210))
         mat(k,2121) = -rxt(k,211)*y(k,233)
         mat(k,688) = -rxt(k,219)*y(k,233)
         mat(k,2057) = -rxt(k,255)*y(k,233)
         mat(k,2571) = -rxt(k,256)*y(k,233)
         mat(k,2715) = -rxt(k,257)*y(k,233)
         mat(k,2181) = -rxt(k,281)*y(k,233)
         mat(k,1840) = -rxt(k,389)*y(k,233)
         mat(k,642) = -rxt(k,399)*y(k,233)
         mat(k,1033) = -rxt(k,444)*y(k,233)
         mat(k,1722) = -rxt(k,457)*y(k,233)
         mat(k,960) = -rxt(k,462)*y(k,233)
         mat(k,992) = -rxt(k,481)*y(k,233)
         mat(k,947) = -rxt(k,491)*y(k,233)
         mat(k,1402) = -rxt(k,496)*y(k,233)
         mat(k,833) = -(rxt(k,506) + rxt(k,507)) * y(k,233)
         mat(k,1640) = -(rxt(k,516) + rxt(k,517)) * y(k,233)
         mat(k,1693) = -rxt(k,528)*y(k,233)
         mat(k,859) = -rxt(k,532)*y(k,233)
         mat(k,1154) = -(rxt(k,549) + rxt(k,550)) * y(k,233)
         mat(k,1570) = -rxt(k,561)*y(k,233)
         mat(k,1607) = -rxt(k,569)*y(k,233)
         mat(k,1496) = -rxt(k,579)*y(k,233)
         mat(k,1472) = -rxt(k,605)*y(k,233)
         mat(k,816) = -rxt(k,610)*y(k,233)
         mat(k,700) = -rxt(k,614)*y(k,233)
         mat(k,662) = -rxt(k,620)*y(k,233)
         mat(k,870) = -rxt(k,623)*y(k,233)
         mat(k,936) = -rxt(k,629)*y(k,233)
         mat(k,905) = -rxt(k,634)*y(k,233)
         mat(k,1105) = -rxt(k,639)*y(k,233)
         mat(k,626) = -rxt(k,643)*y(k,233)
         mat(k,894) = -rxt(k,654)*y(k,233)
         mat(k,924) = -rxt(k,662)*y(k,233)
         mat(k,748) = -rxt(k,666)*y(k,233)
         mat(k,1311) = -rxt(k,683)*y(k,233)
         mat(k,1381) = -rxt(k,689)*y(k,233)
         mat(k,1135) = -rxt(k,695)*y(k,233)

         mat(k,688) = mat(k,688) + rxt(k,221)*y(k,234)
         mat(k,2642) = rxt(k,244)*y(k,229)
         mat(k,2715) = mat(k,2715) + rxt(k,247)*y(k,234)
         mat(k,2424) = rxt(k,254)*y(k,229)
         mat(k,1453) = rxt(k,722)*y(k,234)
         mat(k,164) = rxt(k,244)*y(k,152) + rxt(k,254)*y(k,167)
         mat(k,1092) = rxt(k,221)*y(k,139) + rxt(k,247)*y(k,161) + rxt(k,722)*y(k,183)

      end do

      end subroutine     nlnmat10

      subroutine     nlnmat11( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,1093) = -((rxt(k,220) + rxt(k,221) + rxt(k,222)) * y(k,139) + rxt(k,229) &
                      *y(k,105) + rxt(k,230)*y(k,153) + rxt(k,231)*y(k,167) + rxt(k,232) &
                      *y(k,237) + (rxt(k,247) + rxt(k,249)) * y(k,161) + rxt(k,248) &
                      *y(k,163) + rxt(k,282)*y(k,70) + rxt(k,329)*y(k,24) + rxt(k,470) &
                      *y(k,57) + rxt(k,540)*y(k,130) + rxt(k,630)*y(k,78) + rxt(k,635) &
                      *y(k,129) + rxt(k,640)*y(k,131) + rxt(k,648)*y(k,171) + rxt(k,651) &
                      *y(k,1) + rxt(k,722)*y(k,183))
         mat(k,689) = -(rxt(k,220) + rxt(k,221) + rxt(k,222)) * y(k,234)
         mat(k,2058) = -rxt(k,229)*y(k,234)
         mat(k,2122) = -rxt(k,230)*y(k,234)
         mat(k,2425) = -rxt(k,231)*y(k,234)
         mat(k,1103) = -rxt(k,232)*y(k,234)
         mat(k,2716) = -(rxt(k,247) + rxt(k,249)) * y(k,234)
         mat(k,2572) = -rxt(k,248)*y(k,234)
         mat(k,2182) = -rxt(k,282)*y(k,234)
         mat(k,2487) = -rxt(k,329)*y(k,234)
         mat(k,1723) = -rxt(k,470)*y(k,234)
         mat(k,1694) = -rxt(k,540)*y(k,234)
         mat(k,937) = -rxt(k,630)*y(k,234)
         mat(k,906) = -rxt(k,635)*y(k,234)
         mat(k,1106) = -rxt(k,640)*y(k,234)
         mat(k,672) = -rxt(k,648)*y(k,234)
         mat(k,817) = -rxt(k,651)*y(k,234)
         mat(k,1454) = -rxt(k,722)*y(k,234)

         mat(k,817) = mat(k,817) + rxt(k,610)*y(k,233)
         mat(k,1155) = rxt(k,549)*y(k,233)
         mat(k,701) = rxt(k,614)*y(k,233)
         mat(k,558) = rxt(k,503)*y(k,237)
         mat(k,663) = rxt(k,620)*y(k,233)
         mat(k,1034) = rxt(k,444)*y(k,233)
         mat(k,993) = rxt(k,481)*y(k,233)
         mat(k,871) = rxt(k,623)*y(k,233)
         mat(k,1723) = mat(k,1723) + rxt(k,457)*y(k,233)
         mat(k,1841) = rxt(k,389)*y(k,233) + .500_r8*rxt(k,701)*y(k,240)
         mat(k,2182) = mat(k,2182) + rxt(k,281)*y(k,233)
         mat(k,937) = mat(k,937) + rxt(k,629)*y(k,233)
         mat(k,834) = rxt(k,506)*y(k,233)
         mat(k,961) = rxt(k,462)*y(k,233)
         mat(k,2058) = mat(k,2058) + rxt(k,255)*y(k,233) + rxt(k,250)*y(k,237)
         mat(k,643) = rxt(k,399)*y(k,233)
         mat(k,1571) = .920_r8*rxt(k,561)*y(k,233) + rxt(k,562)*y(k,237)
         mat(k,1608) = .920_r8*rxt(k,569)*y(k,233) + rxt(k,570)*y(k,237)
         mat(k,1497) = rxt(k,579)*y(k,233) + rxt(k,580)*y(k,237)
         mat(k,1641) = rxt(k,516)*y(k,233) + rxt(k,515)*y(k,237)
         mat(k,906) = mat(k,906) + rxt(k,634)*y(k,233)
         mat(k,1694) = mat(k,1694) + rxt(k,528)*y(k,233) + rxt(k,529)*y(k,237)
         mat(k,1106) = mat(k,1106) + rxt(k,639)*y(k,233)
         mat(k,860) = rxt(k,532)*y(k,233)
         mat(k,1938) = rxt(k,251)*y(k,237) + .600_r8*rxt(k,703)*y(k,240)
         mat(k,2122) = mat(k,2122) + rxt(k,211)*y(k,233) + rxt(k,704)*y(k,240)
         mat(k,1312) = rxt(k,683)*y(k,233) + rxt(k,684)*y(k,237)
         mat(k,2716) = mat(k,2716) + rxt(k,257)*y(k,233) + rxt(k,252)*y(k,237)
         mat(k,2572) = mat(k,2572) + rxt(k,256)*y(k,233)
         mat(k,2425) = mat(k,2425) + rxt(k,589)*y(k,218) + rxt(k,241)*y(k,222)  &
                      + .700_r8*rxt(k,590)*y(k,224) + rxt(k,253)*y(k,237) + rxt(k,500) &
                      *y(k,239) + rxt(k,706)*y(k,250)
         mat(k,627) = rxt(k,643)*y(k,233)
         mat(k,948) = rxt(k,491)*y(k,233)
         mat(k,1403) = rxt(k,496)*y(k,233)
         mat(k,1382) = .900_r8*rxt(k,689)*y(k,233)
         mat(k,1136) = .800_r8*rxt(k,695)*y(k,233)
         mat(k,895) = rxt(k,654)*y(k,233)
         mat(k,130) = rxt(k,589)*y(k,167)
         mat(k,229) = rxt(k,241)*y(k,167)
         mat(k,318) = .700_r8*rxt(k,590)*y(k,167)
         mat(k,1080) = rxt(k,610)*y(k,1) + rxt(k,549)*y(k,3) + rxt(k,614)*y(k,11)  &
                      + rxt(k,620)*y(k,28) + rxt(k,444)*y(k,32) + rxt(k,481)*y(k,37)  &
                      + rxt(k,623)*y(k,40) + rxt(k,457)*y(k,57) + rxt(k,389)*y(k,62)  &
                      + rxt(k,281)*y(k,70) + rxt(k,629)*y(k,78) + rxt(k,506)*y(k,84)  &
                      + rxt(k,462)*y(k,86) + rxt(k,255)*y(k,105) + rxt(k,399)*y(k,108)  &
                      + .920_r8*rxt(k,561)*y(k,116) + .920_r8*rxt(k,569)*y(k,117)  &
                      + rxt(k,579)*y(k,120) + rxt(k,516)*y(k,127) + rxt(k,634) &
                      *y(k,129) + rxt(k,528)*y(k,130) + rxt(k,639)*y(k,131)  &
                      + rxt(k,532)*y(k,133) + rxt(k,211)*y(k,153) + rxt(k,683) &
                      *y(k,155) + rxt(k,257)*y(k,161) + rxt(k,256)*y(k,163)  &
                      + rxt(k,643)*y(k,172) + rxt(k,491)*y(k,175) + rxt(k,496) &
                      *y(k,179) + .900_r8*rxt(k,689)*y(k,207) + .800_r8*rxt(k,695) &
                      *y(k,210) + rxt(k,654)*y(k,214) + rxt(k,605)*y(k,244)  &
                      + rxt(k,662)*y(k,252) + rxt(k,666)*y(k,256)
         mat(k,1103) = mat(k,1103) + rxt(k,503)*y(k,21) + rxt(k,250)*y(k,105)  &
                      + rxt(k,562)*y(k,116) + rxt(k,570)*y(k,117) + rxt(k,580) &
                      *y(k,120) + rxt(k,515)*y(k,127) + rxt(k,529)*y(k,130)  &
                      + rxt(k,251)*y(k,151) + rxt(k,684)*y(k,155) + rxt(k,252) &
                      *y(k,161) + rxt(k,253)*y(k,167) + rxt(k,606)*y(k,244)
         mat(k,274) = rxt(k,500)*y(k,167)
         mat(k,570) = .500_r8*rxt(k,701)*y(k,62) + .600_r8*rxt(k,703)*y(k,151)  &
                      + rxt(k,704)*y(k,153)
         mat(k,1473) = rxt(k,605)*y(k,233) + rxt(k,606)*y(k,237)
         mat(k,440) = rxt(k,706)*y(k,167)
         mat(k,925) = rxt(k,662)*y(k,233)
         mat(k,749) = rxt(k,666)*y(k,233)


         mat(k,2107) = rxt(k,230)*y(k,234)
         mat(k,1086) = rxt(k,230)*y(k,153)


         mat(k,1088) = rxt(k,232)*y(k,237)
         mat(k,1097) = rxt(k,232)*y(k,234)

         mat(k,1104) = -(rxt(k,228)*y(k,152) + rxt(k,232)*y(k,234) + rxt(k,250) &
                      *y(k,105) + rxt(k,251)*y(k,151) + rxt(k,252)*y(k,161) + rxt(k,253) &
                      *y(k,167) + rxt(k,383)*y(k,51) + rxt(k,451)*y(k,54) + rxt(k,476) &
                      *y(k,36) + rxt(k,486)*y(k,59) + rxt(k,503)*y(k,21) + rxt(k,515) &
                      *y(k,127) + rxt(k,529)*y(k,130) + rxt(k,562)*y(k,116) + rxt(k,570) &
                      *y(k,117) + rxt(k,580)*y(k,120) + rxt(k,585)*y(k,115) + rxt(k,606) &
                      *y(k,244) + rxt(k,672)*y(k,8) + rxt(k,677)*y(k,136) + rxt(k,684) &
                      *y(k,155) + rxt(k,699)*y(k,212) + rxt(k,709)*y(k,79))
         mat(k,2644) = -rxt(k,228)*y(k,237)
         mat(k,1094) = -rxt(k,232)*y(k,237)
         mat(k,2059) = -rxt(k,250)*y(k,237)
         mat(k,1939) = -rxt(k,251)*y(k,237)
         mat(k,2717) = -rxt(k,252)*y(k,237)
         mat(k,2426) = -rxt(k,253)*y(k,237)
         mat(k,2209) = -rxt(k,383)*y(k,237)
         mat(k,1357) = -rxt(k,451)*y(k,237)
         mat(k,1289) = -rxt(k,476)*y(k,237)
         mat(k,1536) = -rxt(k,486)*y(k,237)
         mat(k,559) = -rxt(k,503)*y(k,237)
         mat(k,1642) = -rxt(k,515)*y(k,237)
         mat(k,1695) = -rxt(k,529)*y(k,237)
         mat(k,1572) = -rxt(k,562)*y(k,237)
         mat(k,1609) = -rxt(k,570)*y(k,237)
         mat(k,1498) = -rxt(k,580)*y(k,237)
         mat(k,1051) = -rxt(k,585)*y(k,237)
         mat(k,1474) = -rxt(k,606)*y(k,237)
         mat(k,1194) = -rxt(k,672)*y(k,237)
         mat(k,1253) = -rxt(k,677)*y(k,237)
         mat(k,1313) = -rxt(k,684)*y(k,237)
         mat(k,1223) = -rxt(k,699)*y(k,237)
         mat(k,460) = -rxt(k,709)*y(k,237)

         mat(k,2251) = rxt(k,303)*y(k,220)
         mat(k,1120) = rxt(k,352)*y(k,221)
         mat(k,2717) = mat(k,2717) + rxt(k,342)*y(k,219) + rxt(k,304)*y(k,220)  &
                      + rxt(k,249)*y(k,234)
         mat(k,2573) = rxt(k,248)*y(k,234)
         mat(k,2426) = mat(k,2426) + rxt(k,305)*y(k,220) + rxt(k,236)*y(k,221)  &
                      + .500_r8*rxt(k,544)*y(k,228) + rxt(k,473)*y(k,248)
         mat(k,314) = rxt(k,342)*y(k,161)
         mat(k,555) = rxt(k,303)*y(k,67) + rxt(k,304)*y(k,161) + rxt(k,305)*y(k,167)
         mat(k,1069) = rxt(k,352)*y(k,88) + rxt(k,236)*y(k,167)
         mat(k,236) = .500_r8*rxt(k,544)*y(k,167)
         mat(k,1094) = mat(k,1094) + rxt(k,249)*y(k,161) + rxt(k,248)*y(k,163)
         mat(k,245) = rxt(k,473)*y(k,167)


         mat(k,2621) = rxt(k,228)*y(k,237)
         mat(k,1095) = rxt(k,228)*y(k,152)

         mat(k,273) = -(rxt(k,500)*y(k,167))
         mat(k,2329) = -rxt(k,500)*y(k,239)

         mat(k,1280) = rxt(k,476)*y(k,237)
         mat(k,2329) = mat(k,2329) + .500_r8*rxt(k,591)*y(k,225) + rxt(k,597)*y(k,227)  &
                      + rxt(k,598)*y(k,230) + rxt(k,599)*y(k,231)
         mat(k,319) = .500_r8*rxt(k,591)*y(k,167)
         mat(k,232) = rxt(k,597)*y(k,167)
         mat(k,238) = rxt(k,598)*y(k,167)
         mat(k,434) = rxt(k,599)*y(k,167)
         mat(k,1096) = rxt(k,476)*y(k,36)

         mat(k,568) = -(rxt(k,701)*y(k,62) + rxt(k,702)*y(k,105) + rxt(k,703)*y(k,151) &
                      + rxt(k,704)*y(k,153))
         mat(k,1831) = -rxt(k,701)*y(k,240)
         mat(k,2022) = -rxt(k,702)*y(k,240)
         mat(k,1906) = -rxt(k,703)*y(k,240)
         mat(k,2114) = -rxt(k,704)*y(k,240)

         mat(k,1188) = rxt(k,672)*y(k,237)
         mat(k,1247) = rxt(k,677)*y(k,237)
         mat(k,2373) = rxt(k,705)*y(k,241)
         mat(k,1220) = .500_r8*rxt(k,699)*y(k,237)
         mat(k,1099) = rxt(k,672)*y(k,8) + rxt(k,677)*y(k,136) + .500_r8*rxt(k,699) &
                      *y(k,212)
         mat(k,325) = rxt(k,705)*y(k,167)

         mat(k,324) = -(rxt(k,705)*y(k,167))
         mat(k,2338) = -rxt(k,705)*y(k,241)

         mat(k,2004) = rxt(k,702)*y(k,240)
         mat(k,566) = rxt(k,702)*y(k,105)

         mat(k,580) = -(rxt(k,163)*y(k,161) + rxt(k,170)*y(k,163) + rxt(k,173)*y(k,94) &
                      + rxt(k,174)*y(k,92) + rxt(k,181)*y(k,105) + rxt(k,192)*y(k,167) &
                      + (rxt(k,204) + rxt(k,206)) * y(k,152) + rxt(k,212)*y(k,153) &
                      + rxt(k,218)*y(k,151) + rxt(k,277)*y(k,71) + rxt(k,283)*y(k,70) &
                      + rxt(k,287)*y(k,100) + rxt(k,291)*y(k,109) + rxt(k,326)*y(k,25) &
                      + rxt(k,330)*y(k,24) + rxt(k,334)*y(k,96) + rxt(k,336)*y(k,107) &
                      + rxt(k,384)*y(k,51))
         mat(k,2705) = -rxt(k,163)*y(k,242)
         mat(k,2560) = -rxt(k,170)*y(k,242)
         mat(k,729) = -rxt(k,173)*y(k,242)
         mat(k,1549) = -rxt(k,174)*y(k,242)
         mat(k,2024) = -rxt(k,181)*y(k,242)
         mat(k,2376) = -rxt(k,192)*y(k,242)
         mat(k,2627) = -(rxt(k,204) + rxt(k,206)) * y(k,242)
         mat(k,2115) = -rxt(k,212)*y(k,242)
         mat(k,1909) = -rxt(k,218)*y(k,242)
         mat(k,1017) = -rxt(k,277)*y(k,242)
         mat(k,2176) = -rxt(k,283)*y(k,242)
         mat(k,2676) = -rxt(k,287)*y(k,242)
         mat(k,969) = -rxt(k,291)*y(k,242)
         mat(k,649) = -rxt(k,326)*y(k,242)
         mat(k,2482) = -rxt(k,330)*y(k,242)
         mat(k,981) = -rxt(k,334)*y(k,242)
         mat(k,878) = -rxt(k,336)*y(k,242)
         mat(k,2203) = -rxt(k,384)*y(k,242)

         mat(k,686) = rxt(k,220)*y(k,234)
         mat(k,1090) = rxt(k,220)*y(k,139)

         mat(k,472) = -(rxt(k,165)*y(k,92) + rxt(k,166)*y(k,260) + rxt(k,169)*y(k,163) &
                      + (rxt(k,258) + rxt(k,259)) * y(k,140) + rxt(k,306)*y(k,42) &
                      + rxt(k,307)*y(k,43) + rxt(k,308)*y(k,45) + rxt(k,309)*y(k,46) &
                      + rxt(k,310)*y(k,47) + rxt(k,311)*y(k,48) + rxt(k,312)*y(k,49) &
                      + (rxt(k,313) + rxt(k,314)) * y(k,100) + rxt(k,343)*y(k,44) &
                      + rxt(k,344)*y(k,66) + rxt(k,345)*y(k,93) + (rxt(k,346) &
                      + rxt(k,347)) * y(k,96) + rxt(k,355)*y(k,75) + rxt(k,356) &
                      *y(k,76) + rxt(k,374)*y(k,50) + rxt(k,375)*y(k,52) + rxt(k,376) &
                      *y(k,97) + rxt(k,377)*y(k,98) + rxt(k,378)*y(k,99) + (rxt(k,431) &
                      + rxt(k,432) + rxt(k,433)) * y(k,65) + rxt(k,434)*y(k,101))
         mat(k,1548) = -rxt(k,165)*y(k,243)
         mat(k,2743) = -rxt(k,166)*y(k,243)
         mat(k,2558) = -rxt(k,169)*y(k,243)
         mat(k,295) = -(rxt(k,258) + rxt(k,259)) * y(k,243)
         mat(k,122) = -rxt(k,306)*y(k,243)
         mat(k,170) = -rxt(k,307)*y(k,243)
         mat(k,150) = -rxt(k,308)*y(k,243)
         mat(k,183) = -rxt(k,309)*y(k,243)
         mat(k,155) = -rxt(k,310)*y(k,243)
         mat(k,188) = -rxt(k,311)*y(k,243)
         mat(k,160) = -rxt(k,312)*y(k,243)
         mat(k,2674) = -(rxt(k,313) + rxt(k,314)) * y(k,243)
         mat(k,176) = -rxt(k,343)*y(k,243)
         mat(k,537) = -rxt(k,344)*y(k,243)
         mat(k,136) = -rxt(k,345)*y(k,243)
         mat(k,980) = -(rxt(k,346) + rxt(k,347)) * y(k,243)
         mat(k,339) = -rxt(k,355)*y(k,243)
         mat(k,379) = -rxt(k,356)*y(k,243)
         mat(k,581) = -rxt(k,374)*y(k,243)
         mat(k,711) = -rxt(k,375)*y(k,243)
         mat(k,374) = -rxt(k,376)*y(k,243)
         mat(k,334) = -rxt(k,377)*y(k,243)
         mat(k,401) = -rxt(k,378)*y(k,243)
         mat(k,1768) = -(rxt(k,431) + rxt(k,432) + rxt(k,433)) * y(k,243)
         mat(k,247) = -rxt(k,434)*y(k,243)

         mat(k,1479) = -(rxt(k,600)*y(k,57) + rxt(k,601)*y(k,62) + rxt(k,602)*y(k,105) &
                      + rxt(k,603)*y(k,151) + rxt(k,604)*y(k,153))
         mat(k,1730) = -rxt(k,600)*y(k,244)
         mat(k,1856) = -rxt(k,601)*y(k,244)
         mat(k,2075) = -rxt(k,602)*y(k,244)
         mat(k,1957) = -rxt(k,603)*y(k,244)
         mat(k,2142) = -rxt(k,604)*y(k,244)

         mat(k,253) = rxt(k,553)*y(k,167)
         mat(k,432) = rxt(k,554)*y(k,167)
         mat(k,143) = rxt(k,555)*y(k,167)
         mat(k,824) = .400_r8*rxt(k,584)*y(k,167)
         mat(k,2449) = rxt(k,553)*y(k,111) + rxt(k,554)*y(k,113) + rxt(k,555)*y(k,114)  &
                      + .400_r8*rxt(k,584)*y(k,123) + .500_r8*rxt(k,607)*y(k,247)
         mat(k,241) = .500_r8*rxt(k,607)*y(k,167)

      end do

      end subroutine     nlnmat11

      subroutine     nlnmat12( avec_len, mat, y, rxt )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,577) = -(rxt(k,158)*y(k,162) + rxt(k,160)*y(k,161) + rxt(k,182) &
                      *y(k,105) + rxt(k,184)*y(k,91) + rxt(k,193)*y(k,167) + rxt(k,205) &
                      *y(k,152) + rxt(k,217)*y(k,151) + rxt(k,284)*y(k,67) + rxt(k,331) &
                      *y(k,22) + rxt(k,439)*y(k,31) + rxt(k,477)*y(k,36) + rxt(k,521) &
                      *y(k,126) + rxt(k,538)*y(k,138) + rxt(k,586)*y(k,115) + rxt(k,649) &
                      *y(k,171) + rxt(k,673)*y(k,8) + rxt(k,678)*y(k,136) + rxt(k,723) &
                      *y(k,183) + rxt(k,724)*y(k,181))
         mat(k,2526) = -rxt(k,158)*y(k,245)
         mat(k,2704) = -rxt(k,160)*y(k,245)
         mat(k,2023) = -rxt(k,182)*y(k,245)
         mat(k,1787) = -rxt(k,184)*y(k,245)
         mat(k,2375) = -rxt(k,193)*y(k,245)
         mat(k,2626) = -rxt(k,205)*y(k,245)
         mat(k,1908) = -rxt(k,217)*y(k,245)
         mat(k,2238) = -rxt(k,284)*y(k,245)
         mat(k,1753) = -rxt(k,331)*y(k,245)
         mat(k,606) = -rxt(k,439)*y(k,245)
         mat(k,1281) = -rxt(k,477)*y(k,245)
         mat(k,1518) = -rxt(k,521)*y(k,245)
         mat(k,1662) = -rxt(k,538)*y(k,245)
         mat(k,1046) = -rxt(k,586)*y(k,245)
         mat(k,668) = -rxt(k,649)*y(k,245)
         mat(k,1189) = -rxt(k,673)*y(k,245)
         mat(k,1248) = -rxt(k,678)*y(k,245)
         mat(k,1449) = -rxt(k,723)*y(k,245)
         mat(k,590) = -rxt(k,724)*y(k,245)


         mat(k,2330) = rxt(k,543)*y(k,223)
         mat(k,1376) = .100_r8*rxt(k,689)*y(k,233)
         mat(k,427) = rxt(k,543)*y(k,167)
         mat(k,1072) = .100_r8*rxt(k,689)*y(k,207)

         mat(k,240) = -(rxt(k,607)*y(k,167))
         mat(k,2322) = -rxt(k,607)*y(k,247)

         mat(k,2001) = rxt(k,602)*y(k,244)
         mat(k,1469) = rxt(k,602)*y(k,105)

         mat(k,243) = -(rxt(k,473)*y(k,167))
         mat(k,2323) = -rxt(k,473)*y(k,248)

         mat(k,1713) = rxt(k,470)*y(k,234)
         mat(k,1085) = rxt(k,470)*y(k,57)


         mat(k,812) = rxt(k,651)*y(k,234)
         mat(k,1082) = rxt(k,651)*y(k,1)

         mat(k,438) = -(rxt(k,706)*y(k,167))
         mat(k,2354) = -rxt(k,706)*y(k,250)

         mat(k,1827) = .500_r8*rxt(k,701)*y(k,240)
         mat(k,1903) = .200_r8*rxt(k,703)*y(k,240)
         mat(k,1130) = .200_r8*rxt(k,695)*y(k,233)
         mat(k,1076) = .200_r8*rxt(k,695)*y(k,210)
         mat(k,567) = .500_r8*rxt(k,701)*y(k,62) + .200_r8*rxt(k,703)*y(k,151)

         mat(k,280) = -((rxt(k,659) + rxt(k,781)) * y(k,167))
         mat(k,2331) = -(rxt(k,659) + rxt(k,781)) * y(k,251)

         mat(k,922) = -(rxt(k,660)*y(k,105) + rxt(k,661)*y(k,151))
         mat(k,2046) = -rxt(k,660)*y(k,252)
         mat(k,1928) = -rxt(k,661)*y(k,252)

         mat(k,2410) = .560_r8*rxt(k,659)*y(k,251) + rxt(k,663)*y(k,253)
         mat(k,284) = .560_r8*rxt(k,659)*y(k,167)
         mat(k,848) = rxt(k,663)*y(k,167)

         mat(k,846) = -(rxt(k,663)*y(k,167))
         mat(k,2404) = -rxt(k,663)*y(k,253)

         mat(k,2040) = rxt(k,660)*y(k,252)
         mat(k,920) = rxt(k,660)*y(k,105)

         mat(k,105) = -(rxt(k,782)*y(k,105) + rxt(k,783)*y(k,151))
         mat(k,1997) = -rxt(k,782)*y(k,254)
         mat(k,1894) = -rxt(k,783)*y(k,254)

         mat(k,2304) = rxt(k,781)*y(k,251)
         mat(k,278) = rxt(k,781)*y(k,167)

         mat(k,289) = -(rxt(k,667)*y(k,167))
         mat(k,2332) = -rxt(k,667)*y(k,255)

         mat(k,2332) = mat(k,2332) + .150_r8*rxt(k,659)*y(k,251)
         mat(k,281) = .150_r8*rxt(k,659)*y(k,167)

         mat(k,747) = -(rxt(k,664)*y(k,105) + rxt(k,665)*y(k,151))
         mat(k,2033) = -rxt(k,664)*y(k,256)
         mat(k,1919) = -rxt(k,665)*y(k,256)

         mat(k,2394) = .300_r8*rxt(k,667)*y(k,255) + rxt(k,668)*y(k,257)
         mat(k,291) = .300_r8*rxt(k,667)*y(k,167)
         mat(k,529) = rxt(k,668)*y(k,167)

         mat(k,528) = -(rxt(k,668)*y(k,167))
         mat(k,2367) = -rxt(k,668)*y(k,257)

         mat(k,2019) = rxt(k,664)*y(k,256)
         mat(k,746) = rxt(k,664)*y(k,105)

         mat(k,2767) = -(rxt(k,154)*y(k,162) + rxt(k,350)*y(k,88) + rxt(k,727) &
                      *y(k,185))
         mat(k,2551) = -rxt(k,154)*y(k,260)
         mat(k,1129) = -rxt(k,350)*y(k,260)
         mat(k,306) = -rxt(k,727)*y(k,260)

         mat(k,359) = rxt(k,448)*y(k,167)
         mat(k,503) = rxt(k,482)*y(k,167)
         mat(k,120) = rxt(k,483)*y(k,167)
         mat(k,589) = rxt(k,358)*y(k,167)
         mat(k,2231) = rxt(k,382)*y(k,167)
         mat(k,721) = rxt(k,360)*y(k,167)
         mat(k,128) = rxt(k,361)*y(k,167)
         mat(k,1366) = rxt(k,450)*y(k,167)
         mat(k,456) = rxt(k,363)*y(k,167)
         mat(k,1172) = rxt(k,499)*y(k,167)
         mat(k,1546) = rxt(k,485)*y(k,167)
         mat(k,811) = rxt(k,458)*y(k,167)
         mat(k,728) = rxt(k,459)*y(k,167)
         mat(k,527) = rxt(k,391)*y(k,167)
         mat(k,1786) = rxt(k,392)*y(k,167)
         mat(k,1803) = rxt(k,177)*y(k,105)
         mat(k,1562) = rxt(k,185)*y(k,167)
         mat(k,736) = rxt(k,186)*y(k,167)
         mat(k,989) = rxt(k,333)*y(k,167)
         mat(k,406) = rxt(k,368)*y(k,167)
         mat(k,2700) = (rxt(k,792)+rxt(k,797))*y(k,107) + (rxt(k,785)+rxt(k,791) &
                       +rxt(k,796))*y(k,109) + rxt(k,286)*y(k,167)
         mat(k,1179) = rxt(k,395)*y(k,167)
         mat(k,1821) = rxt(k,233)*y(k,167)
         mat(k,2104) = rxt(k,177)*y(k,91) + rxt(k,187)*y(k,167)
         mat(k,521) = rxt(k,195)*y(k,167)
         mat(k,886) = (rxt(k,792)+rxt(k,797))*y(k,100)
         mat(k,976) = (rxt(k,785)+rxt(k,791)+rxt(k,796))*y(k,100) + rxt(k,290) &
                      *y(k,167)
         mat(k,1533) = .500_r8*rxt(k,519)*y(k,167)
         mat(k,114) = rxt(k,728)*y(k,167)
         mat(k,2478) = rxt(k,448)*y(k,35) + rxt(k,482)*y(k,38) + rxt(k,483)*y(k,39)  &
                      + rxt(k,358)*y(k,50) + rxt(k,382)*y(k,51) + rxt(k,360)*y(k,52)  &
                      + rxt(k,361)*y(k,53) + rxt(k,450)*y(k,54) + rxt(k,363)*y(k,55)  &
                      + rxt(k,499)*y(k,58) + rxt(k,485)*y(k,59) + rxt(k,458)*y(k,60)  &
                      + rxt(k,459)*y(k,61) + rxt(k,391)*y(k,64) + rxt(k,392)*y(k,65)  &
                      + rxt(k,185)*y(k,92) + rxt(k,186)*y(k,94) + rxt(k,333)*y(k,96)  &
                      + rxt(k,368)*y(k,99) + rxt(k,286)*y(k,100) + rxt(k,395)*y(k,102)  &
                      + rxt(k,233)*y(k,104) + rxt(k,187)*y(k,105) + rxt(k,195) &
                      *y(k,106) + rxt(k,290)*y(k,109) + .500_r8*rxt(k,519)*y(k,126)  &
                      + rxt(k,728)*y(k,147) + 2.000_r8*rxt(k,190)*y(k,167)  &
                      + rxt(k,492)*y(k,178) + rxt(k,497)*y(k,180)
         mat(k,640) = rxt(k,492)*y(k,167)
         mat(k,497) = rxt(k,497)*y(k,167)

      end do

      end subroutine     nlnmat12

      subroutine     nlnmat_finit( avec_len, mat, lmat, dti )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  dti(veclen)
      real(r8), intent(in)    ::  lmat(veclen,nzcnt)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)


!----------------------------------------------
!       ... local variables
!----------------------------------------------
      integer :: k

!----------------------------------------------
!       ... complete matrix entries implicit species
!----------------------------------------------

      do k = 1,avec_len
         mat(k,   1) = lmat(k,   1)
         mat(k,   2) = lmat(k,   2)
         mat(k,   3) = lmat(k,   3)
         mat(k,   4) = lmat(k,   4)
         mat(k,   5) = lmat(k,   5)
         mat(k,   6) = mat(k,   6) + lmat(k,   6)
         mat(k,   7) = mat(k,   7) + lmat(k,   7)
         mat(k,   8) = mat(k,   8) + lmat(k,   8)
         mat(k,   9) = mat(k,   9) + lmat(k,   9)
         mat(k,  10) = mat(k,  10) + lmat(k,  10)
         mat(k,  11) = mat(k,  11) + lmat(k,  11)
         mat(k,  12) = mat(k,  12) + lmat(k,  12)
         mat(k,  13) = mat(k,  13) + lmat(k,  13)
         mat(k,  14) = mat(k,  14) + lmat(k,  14)
         mat(k,  15) = mat(k,  15) + lmat(k,  15)
         mat(k,  16) = mat(k,  16) + lmat(k,  16)
         mat(k,  17) = mat(k,  17) + lmat(k,  17)
         mat(k,  18) = mat(k,  18) + lmat(k,  18)
         mat(k,  19) = lmat(k,  19)
         mat(k,  20) = lmat(k,  20)
         mat(k,  21) = lmat(k,  21)
         mat(k,  22) = lmat(k,  22)
         mat(k,  23) = lmat(k,  23)
         mat(k,  24) = lmat(k,  24)
         mat(k,  25) = lmat(k,  25)
         mat(k,  26) = lmat(k,  26)
         mat(k,  27) = lmat(k,  27)
         mat(k,  28) = lmat(k,  28)
         mat(k,  29) = lmat(k,  29)
         mat(k,  30) = lmat(k,  30)
         mat(k,  31) = lmat(k,  31)
         mat(k,  32) = lmat(k,  32)
         mat(k,  33) = lmat(k,  33)
         mat(k,  34) = lmat(k,  34)
         mat(k,  35) = lmat(k,  35)
         mat(k,  36) = lmat(k,  36)
         mat(k,  37) = lmat(k,  37)
         mat(k,  38) = lmat(k,  38)
         mat(k,  39) = lmat(k,  39)
         mat(k,  40) = lmat(k,  40)
         mat(k,  41) = lmat(k,  41)
         mat(k,  42) = lmat(k,  42)
         mat(k,  43) = lmat(k,  43)
         mat(k,  44) = lmat(k,  44)
         mat(k,  45) = lmat(k,  45)
         mat(k,  46) = lmat(k,  46)
         mat(k,  47) = lmat(k,  47)
         mat(k,  48) = lmat(k,  48)
         mat(k,  49) = lmat(k,  49)
         mat(k,  50) = lmat(k,  50)
         mat(k,  51) = lmat(k,  51)
         mat(k,  52) = lmat(k,  52)
         mat(k,  53) = lmat(k,  53)
         mat(k,  54) = lmat(k,  54)
         mat(k,  60) = mat(k,  60) + lmat(k,  60)
         mat(k,  66) = mat(k,  66) + lmat(k,  66)
         mat(k,  72) = mat(k,  72) + lmat(k,  72)
         mat(k,  78) = mat(k,  78) + lmat(k,  78)
         mat(k,  80) = mat(k,  80) + lmat(k,  80)
         mat(k,  86) = mat(k,  86) + lmat(k,  86)
         mat(k,  87) = lmat(k,  87)
         mat(k,  93) = mat(k,  93) + lmat(k,  93)
         mat(k,  99) = mat(k,  99) + lmat(k,  99)
         mat(k, 105) = mat(k, 105) + lmat(k, 105)
         mat(k, 106) = lmat(k, 106)
         mat(k, 107) = lmat(k, 107)
         mat(k, 108) = lmat(k, 108)
         mat(k, 109) = mat(k, 109) + lmat(k, 109)
         mat(k, 112) = mat(k, 112) + lmat(k, 112)
         mat(k, 115) = lmat(k, 115)
         mat(k, 116) = lmat(k, 116)
         mat(k, 117) = mat(k, 117) + lmat(k, 117)
         mat(k, 121) = mat(k, 121) + lmat(k, 121)
         mat(k, 123) = mat(k, 123) + lmat(k, 123)
         mat(k, 125) = mat(k, 125) + lmat(k, 125)
         mat(k, 126) = mat(k, 126) + lmat(k, 126)
         mat(k, 129) = mat(k, 129) + lmat(k, 129)
         mat(k, 130) = mat(k, 130) + lmat(k, 130)
         mat(k, 131) = lmat(k, 131)
         mat(k, 132) = lmat(k, 132)
         mat(k, 133) = lmat(k, 133)
         mat(k, 134) = mat(k, 134) + lmat(k, 134)
         mat(k, 135) = mat(k, 135) + lmat(k, 135)
         mat(k, 137) = mat(k, 137) + lmat(k, 137)
         mat(k, 139) = lmat(k, 139)
         mat(k, 140) = lmat(k, 140)
         mat(k, 141) = lmat(k, 141)
         mat(k, 142) = mat(k, 142) + lmat(k, 142)
         mat(k, 145) = lmat(k, 145)
         mat(k, 146) = lmat(k, 146)
         mat(k, 147) = lmat(k, 147)
         mat(k, 148) = mat(k, 148) + lmat(k, 148)
         mat(k, 149) = mat(k, 149) + lmat(k, 149)
         mat(k, 151) = mat(k, 151) + lmat(k, 151)
         mat(k, 153) = mat(k, 153) + lmat(k, 153)
         mat(k, 154) = mat(k, 154) + lmat(k, 154)
         mat(k, 156) = mat(k, 156) + lmat(k, 156)
         mat(k, 158) = mat(k, 158) + lmat(k, 158)
         mat(k, 159) = mat(k, 159) + lmat(k, 159)
         mat(k, 161) = mat(k, 161) + lmat(k, 161)
         mat(k, 163) = mat(k, 163) + lmat(k, 163)
         mat(k, 164) = mat(k, 164) + lmat(k, 164)
         mat(k, 165) = lmat(k, 165)
         mat(k, 166) = lmat(k, 166)
         mat(k, 167) = lmat(k, 167)
         mat(k, 168) = mat(k, 168) + lmat(k, 168)
         mat(k, 169) = mat(k, 169) + lmat(k, 169)
         mat(k, 171) = mat(k, 171) + lmat(k, 171)
         mat(k, 172) = mat(k, 172) + lmat(k, 172)
         mat(k, 174) = mat(k, 174) + lmat(k, 174)
         mat(k, 175) = mat(k, 175) + lmat(k, 175)
         mat(k, 177) = mat(k, 177) + lmat(k, 177)
         mat(k, 178) = mat(k, 178) + lmat(k, 178)
         mat(k, 180) = mat(k, 180) + lmat(k, 180)
         mat(k, 181) = mat(k, 181) + lmat(k, 181)
         mat(k, 182) = mat(k, 182) + lmat(k, 182)
         mat(k, 184) = mat(k, 184) + lmat(k, 184)
         mat(k, 186) = mat(k, 186) + lmat(k, 186)
         mat(k, 187) = mat(k, 187) + lmat(k, 187)
         mat(k, 189) = mat(k, 189) + lmat(k, 189)
         mat(k, 190) = mat(k, 190) + lmat(k, 190)
         mat(k, 192) = lmat(k, 192)
         mat(k, 193) = lmat(k, 193)
         mat(k, 195) = mat(k, 195) + lmat(k, 195)
         mat(k, 202) = lmat(k, 202)
         mat(k, 203) = lmat(k, 203)
         mat(k, 204) = lmat(k, 204)
         mat(k, 205) = lmat(k, 205)
         mat(k, 206) = mat(k, 206) + lmat(k, 206)
         mat(k, 212) = mat(k, 212) + lmat(k, 212)
         mat(k, 219) = mat(k, 219) + lmat(k, 219)
         mat(k, 224) = lmat(k, 224)
         mat(k, 225) = lmat(k, 225)
         mat(k, 226) = lmat(k, 226)
         mat(k, 227) = lmat(k, 227)
         mat(k, 228) = mat(k, 228) + lmat(k, 228)
         mat(k, 229) = mat(k, 229) + lmat(k, 229)
         mat(k, 230) = lmat(k, 230)
         mat(k, 231) = mat(k, 231) + lmat(k, 231)
         mat(k, 233) = lmat(k, 233)
         mat(k, 234) = mat(k, 234) + lmat(k, 234)
         mat(k, 235) = lmat(k, 235)
         mat(k, 237) = mat(k, 237) + lmat(k, 237)
         mat(k, 239) = lmat(k, 239)
         mat(k, 240) = mat(k, 240) + lmat(k, 240)
         mat(k, 242) = mat(k, 242) + lmat(k, 242)
         mat(k, 243) = mat(k, 243) + lmat(k, 243)
         mat(k, 244) = lmat(k, 244)
         mat(k, 245) = mat(k, 245) + lmat(k, 245)
         mat(k, 246) = mat(k, 246) + lmat(k, 246)
         mat(k, 251) = mat(k, 251) + lmat(k, 251)
         mat(k, 252) = lmat(k, 252)
         mat(k, 254) = lmat(k, 254)
         mat(k, 255) = mat(k, 255) + lmat(k, 255)
         mat(k, 256) = lmat(k, 256)
         mat(k, 257) = lmat(k, 257)
         mat(k, 258) = lmat(k, 258)
         mat(k, 259) = mat(k, 259) + lmat(k, 259)
         mat(k, 262) = lmat(k, 262)
         mat(k, 263) = lmat(k, 263)
         mat(k, 264) = lmat(k, 264)
         mat(k, 265) = mat(k, 265) + lmat(k, 265)
         mat(k, 268) = lmat(k, 268)
         mat(k, 269) = lmat(k, 269)
         mat(k, 270) = lmat(k, 270)
         mat(k, 271) = lmat(k, 271)
         mat(k, 272) = lmat(k, 272)
         mat(k, 273) = mat(k, 273) + lmat(k, 273)
         mat(k, 274) = mat(k, 274) + lmat(k, 274)
         mat(k, 275) = lmat(k, 275)
         mat(k, 276) = lmat(k, 276)
         mat(k, 277) = lmat(k, 277)
         mat(k, 280) = mat(k, 280) + lmat(k, 280)
         mat(k, 289) = mat(k, 289) + lmat(k, 289)
         mat(k, 294) = mat(k, 294) + lmat(k, 294)
         mat(k, 297) = mat(k, 297) + lmat(k, 297)
         mat(k, 298) = mat(k, 298) + lmat(k, 298)
         mat(k, 303) = mat(k, 303) + lmat(k, 303)
         mat(k, 304) = lmat(k, 304)
         mat(k, 305) = lmat(k, 305)
         mat(k, 307) = mat(k, 307) + lmat(k, 307)
         mat(k, 308) = lmat(k, 308)
         mat(k, 310) = mat(k, 310) + lmat(k, 310)
         mat(k, 311) = mat(k, 311) + lmat(k, 311)
         mat(k, 312) = lmat(k, 312)
         mat(k, 313) = lmat(k, 313)
         mat(k, 314) = mat(k, 314) + lmat(k, 314)
         mat(k, 315) = mat(k, 315) + lmat(k, 315)
         mat(k, 317) = lmat(k, 317)
         mat(k, 320) = mat(k, 320) + lmat(k, 320)
         mat(k, 322) = lmat(k, 322)
         mat(k, 324) = mat(k, 324) + lmat(k, 324)
         mat(k, 326) = lmat(k, 326)
         mat(k, 327) = lmat(k, 327)
         mat(k, 328) = lmat(k, 328)
         mat(k, 329) = lmat(k, 329)
         mat(k, 330) = lmat(k, 330)
         mat(k, 331) = lmat(k, 331)
         mat(k, 332) = mat(k, 332) + lmat(k, 332)
         mat(k, 333) = mat(k, 333) + lmat(k, 333)
         mat(k, 335) = mat(k, 335) + lmat(k, 335)
         mat(k, 338) = mat(k, 338) + lmat(k, 338)
         mat(k, 340) = mat(k, 340) + lmat(k, 340)
         mat(k, 342) = lmat(k, 342)
         mat(k, 343) = lmat(k, 343)
         mat(k, 344) = lmat(k, 344)
         mat(k, 345) = lmat(k, 345)
         mat(k, 346) = lmat(k, 346)
         mat(k, 347) = lmat(k, 347)
         mat(k, 348) = mat(k, 348) + lmat(k, 348)
         mat(k, 354) = mat(k, 354) + lmat(k, 354)
         mat(k, 360) = lmat(k, 360)
         mat(k, 361) = lmat(k, 361)
         mat(k, 362) = lmat(k, 362)
         mat(k, 363) = lmat(k, 363)
         mat(k, 364) = lmat(k, 364)
         mat(k, 365) = lmat(k, 365)
         mat(k, 366) = lmat(k, 366)
         mat(k, 367) = lmat(k, 367)
         mat(k, 368) = lmat(k, 368)
         mat(k, 369) = lmat(k, 369)
         mat(k, 370) = lmat(k, 370)
         mat(k, 371) = lmat(k, 371)
         mat(k, 372) = mat(k, 372) + lmat(k, 372)
         mat(k, 373) = mat(k, 373) + lmat(k, 373)
         mat(k, 375) = mat(k, 375) + lmat(k, 375)
         mat(k, 378) = mat(k, 378) + lmat(k, 378)
         mat(k, 380) = mat(k, 380) + lmat(k, 380)
         mat(k, 381) = mat(k, 381) + lmat(k, 381)
         mat(k, 383) = mat(k, 383) + lmat(k, 383)
         mat(k, 388) = mat(k, 388) + lmat(k, 388)
         mat(k, 390) = lmat(k, 390)
         mat(k, 391) = lmat(k, 391)
         mat(k, 392) = mat(k, 392) + lmat(k, 392)
         mat(k, 393) = lmat(k, 393)
         mat(k, 394) = mat(k, 394) + lmat(k, 394)
         mat(k, 397) = lmat(k, 397)
         mat(k, 398) = mat(k, 398) + lmat(k, 398)
         mat(k, 399) = mat(k, 399) + lmat(k, 399)
         mat(k, 400) = mat(k, 400) + lmat(k, 400)
         mat(k, 403) = mat(k, 403) + lmat(k, 403)
         mat(k, 407) = mat(k, 407) + lmat(k, 407)
         mat(k, 410) = mat(k, 410) + lmat(k, 410)
         mat(k, 411) = lmat(k, 411)
         mat(k, 412) = mat(k, 412) + lmat(k, 412)
         mat(k, 414) = lmat(k, 414)
         mat(k, 415) = lmat(k, 415)
         mat(k, 416) = mat(k, 416) + lmat(k, 416)
         mat(k, 417) = mat(k, 417) + lmat(k, 417)
         mat(k, 419) = mat(k, 419) + lmat(k, 419)
         mat(k, 420) = lmat(k, 420)
         mat(k, 421) = mat(k, 421) + lmat(k, 421)
         mat(k, 422) = lmat(k, 422)
         mat(k, 423) = lmat(k, 423)
         mat(k, 424) = lmat(k, 424)
         mat(k, 425) = lmat(k, 425)
         mat(k, 426) = lmat(k, 426)
         mat(k, 428) = mat(k, 428) + lmat(k, 428)
         mat(k, 429) = lmat(k, 429)
         mat(k, 430) = lmat(k, 430)
         mat(k, 431) = mat(k, 431) + lmat(k, 431)
         mat(k, 435) = mat(k, 435) + lmat(k, 435)
         mat(k, 436) = lmat(k, 436)
         mat(k, 437) = lmat(k, 437)
         mat(k, 438) = mat(k, 438) + lmat(k, 438)
         mat(k, 439) = lmat(k, 439)
         mat(k, 440) = mat(k, 440) + lmat(k, 440)
         mat(k, 441) = mat(k, 441) + lmat(k, 441)
         mat(k, 442) = lmat(k, 442)
         mat(k, 444) = mat(k, 444) + lmat(k, 444)
         mat(k, 449) = mat(k, 449) + lmat(k, 449)
         mat(k, 451) = lmat(k, 451)
         mat(k, 453) = mat(k, 453) + lmat(k, 453)
         mat(k, 457) = mat(k, 457) + lmat(k, 457)
         mat(k, 469) = mat(k, 469) + lmat(k, 469)
         mat(k, 472) = mat(k, 472) + lmat(k, 472)
         mat(k, 473) = lmat(k, 473)
         mat(k, 474) = mat(k, 474) + lmat(k, 474)
         mat(k, 477) = lmat(k, 477)
         mat(k, 480) = mat(k, 480) + lmat(k, 480)
         mat(k, 481) = lmat(k, 481)
         mat(k, 483) = lmat(k, 483)
         mat(k, 484) = mat(k, 484) + lmat(k, 484)
         mat(k, 485) = lmat(k, 485)
         mat(k, 486) = mat(k, 486) + lmat(k, 486)
         mat(k, 487) = lmat(k, 487)
         mat(k, 489) = mat(k, 489) + lmat(k, 489)
         mat(k, 491) = lmat(k, 491)
         mat(k, 492) = mat(k, 492) + lmat(k, 492)
         mat(k, 494) = lmat(k, 494)
         mat(k, 495) = lmat(k, 495)
         mat(k, 496) = mat(k, 496) + lmat(k, 496)
         mat(k, 498) = mat(k, 498) + lmat(k, 498)
         mat(k, 500) = lmat(k, 500)
         mat(k, 501) = lmat(k, 501)
         mat(k, 502) = mat(k, 502) + lmat(k, 502)
         mat(k, 504) = lmat(k, 504)
         mat(k, 505) = lmat(k, 505)
         mat(k, 506) = lmat(k, 506)
         mat(k, 507) = lmat(k, 507)
         mat(k, 508) = lmat(k, 508)
         mat(k, 509) = lmat(k, 509)
         mat(k, 510) = mat(k, 510) + lmat(k, 510)
         mat(k, 511) = lmat(k, 511)
         mat(k, 513) = lmat(k, 513)
         mat(k, 514) = lmat(k, 514)
         mat(k, 515) = mat(k, 515) + lmat(k, 515)
         mat(k, 516) = mat(k, 516) + lmat(k, 516)
         mat(k, 517) = lmat(k, 517)
         mat(k, 518) = lmat(k, 518)
         mat(k, 519) = mat(k, 519) + lmat(k, 519)
         mat(k, 520) = mat(k, 520) + lmat(k, 520)
         mat(k, 522) = mat(k, 522) + lmat(k, 522)
         mat(k, 523) = lmat(k, 523)
         mat(k, 525) = mat(k, 525) + lmat(k, 525)
         mat(k, 526) = mat(k, 526) + lmat(k, 526)
         mat(k, 528) = mat(k, 528) + lmat(k, 528)
         mat(k, 530) = lmat(k, 530)
         mat(k, 531) = lmat(k, 531)
         mat(k, 532) = lmat(k, 532)
         mat(k, 533) = mat(k, 533) + lmat(k, 533)
         mat(k, 534) = lmat(k, 534)
         mat(k, 535) = lmat(k, 535)
         mat(k, 536) = lmat(k, 536)
         mat(k, 538) = mat(k, 538) + lmat(k, 538)
         mat(k, 540) = mat(k, 540) + lmat(k, 540)
         mat(k, 545) = mat(k, 545) + lmat(k, 545)
         mat(k, 547) = lmat(k, 547)
         mat(k, 548) = lmat(k, 548)
         mat(k, 549) = lmat(k, 549)
         mat(k, 550) = lmat(k, 550)
         mat(k, 551) = mat(k, 551) + lmat(k, 551)
         mat(k, 552) = mat(k, 552) + lmat(k, 552)
         mat(k, 553) = mat(k, 553) + lmat(k, 553)
         mat(k, 554) = lmat(k, 554)
         mat(k, 555) = mat(k, 555) + lmat(k, 555)
         mat(k, 556) = mat(k, 556) + lmat(k, 556)
         mat(k, 568) = mat(k, 568) + lmat(k, 568)
         mat(k, 571) = mat(k, 571) + lmat(k, 571)
         mat(k, 573) = lmat(k, 573)
         mat(k, 574) = lmat(k, 574)
         mat(k, 576) = lmat(k, 576)
         mat(k, 577) = mat(k, 577) + lmat(k, 577)
         mat(k, 578) = lmat(k, 578)
         mat(k, 579) = lmat(k, 579)
         mat(k, 580) = mat(k, 580) + lmat(k, 580)
         mat(k, 583) = mat(k, 583) + lmat(k, 583)
         mat(k, 584) = mat(k, 584) + lmat(k, 584)
         mat(k, 592) = mat(k, 592) + lmat(k, 592)
         mat(k, 593) = mat(k, 593) + lmat(k, 593)
         mat(k, 597) = lmat(k, 597)
         mat(k, 598) = mat(k, 598) + lmat(k, 598)
         mat(k, 603) = lmat(k, 603)
         mat(k, 608) = mat(k, 608) + lmat(k, 608)
         mat(k, 616) = mat(k, 616) + lmat(k, 616)
         mat(k, 617) = lmat(k, 617)
         mat(k, 618) = lmat(k, 618)
         mat(k, 619) = lmat(k, 619)
         mat(k, 620) = mat(k, 620) + lmat(k, 620)
         mat(k, 623) = lmat(k, 623)
         mat(k, 625) = mat(k, 625) + lmat(k, 625)
         mat(k, 633) = mat(k, 633) + lmat(k, 633)
         mat(k, 635) = lmat(k, 635)
         mat(k, 637) = lmat(k, 637)
         mat(k, 638) = lmat(k, 638)
         mat(k, 639) = mat(k, 639) + lmat(k, 639)
         mat(k, 641) = mat(k, 641) + lmat(k, 641)
         mat(k, 646) = mat(k, 646) + lmat(k, 646)
         mat(k, 647) = lmat(k, 647)
         mat(k, 650) = mat(k, 650) + lmat(k, 650)
         mat(k, 651) = lmat(k, 651)
         mat(k, 652) = lmat(k, 652)
         mat(k, 653) = lmat(k, 653)
         mat(k, 654) = mat(k, 654) + lmat(k, 654)
         mat(k, 655) = mat(k, 655) + lmat(k, 655)
         mat(k, 656) = lmat(k, 656)
         mat(k, 660) = mat(k, 660) + lmat(k, 660)
         mat(k, 670) = mat(k, 670) + lmat(k, 670)
         mat(k, 675) = mat(k, 675) + lmat(k, 675)
         mat(k, 678) = lmat(k, 678)
         mat(k, 683) = lmat(k, 683)
         mat(k, 687) = mat(k, 687) + lmat(k, 687)
         mat(k, 691) = mat(k, 691) + lmat(k, 691)
         mat(k, 695) = mat(k, 695) + lmat(k, 695)
         mat(k, 698) = mat(k, 698) + lmat(k, 698)
         mat(k, 707) = lmat(k, 707)
         mat(k, 708) = lmat(k, 708)
         mat(k, 709) = lmat(k, 709)
         mat(k, 710) = lmat(k, 710)
         mat(k, 713) = mat(k, 713) + lmat(k, 713)
         mat(k, 714) = mat(k, 714) + lmat(k, 714)
         mat(k, 715) = lmat(k, 715)
         mat(k, 722) = mat(k, 722) + lmat(k, 722)
         mat(k, 723) = mat(k, 723) + lmat(k, 723)
         mat(k, 725) = lmat(k, 725)
         mat(k, 727) = mat(k, 727) + lmat(k, 727)
         mat(k, 730) = mat(k, 730) + lmat(k, 730)
         mat(k, 733) = mat(k, 733) + lmat(k, 733)
         mat(k, 737) = mat(k, 737) + lmat(k, 737)
         mat(k, 741) = lmat(k, 741)
         mat(k, 747) = mat(k, 747) + lmat(k, 747)
         mat(k, 756) = mat(k, 756) + lmat(k, 756)
         mat(k, 757) = mat(k, 757) + lmat(k, 757)
         mat(k, 758) = lmat(k, 758)
         mat(k, 759) = lmat(k, 759)
         mat(k, 761) = mat(k, 761) + lmat(k, 761)
         mat(k, 762) = mat(k, 762) + lmat(k, 762)
         mat(k, 763) = lmat(k, 763)
         mat(k, 764) = lmat(k, 764)
         mat(k, 765) = lmat(k, 765)
         mat(k, 766) = lmat(k, 766)
         mat(k, 768) = lmat(k, 768)
         mat(k, 769) = lmat(k, 769)
         mat(k, 770) = lmat(k, 770)
         mat(k, 771) = mat(k, 771) + lmat(k, 771)
         mat(k, 772) = lmat(k, 772)
         mat(k, 773) = lmat(k, 773)
         mat(k, 774) = lmat(k, 774)
         mat(k, 775) = mat(k, 775) + lmat(k, 775)
         mat(k, 780) = lmat(k, 780)
         mat(k, 782) = lmat(k, 782)
         mat(k, 783) = lmat(k, 783)
         mat(k, 784) = mat(k, 784) + lmat(k, 784)
         mat(k, 785) = lmat(k, 785)
         mat(k, 786) = mat(k, 786) + lmat(k, 786)
         mat(k, 790) = lmat(k, 790)
         mat(k, 791) = lmat(k, 791)
         mat(k, 793) = lmat(k, 793)
         mat(k, 794) = lmat(k, 794)
         mat(k, 795) = mat(k, 795) + lmat(k, 795)
         mat(k, 796) = lmat(k, 796)
         mat(k, 797) = mat(k, 797) + lmat(k, 797)
         mat(k, 800) = mat(k, 800) + lmat(k, 800)
         mat(k, 801) = mat(k, 801) + lmat(k, 801)
         mat(k, 803) = lmat(k, 803)
         mat(k, 804) = mat(k, 804) + lmat(k, 804)
         mat(k, 806) = mat(k, 806) + lmat(k, 806)
         mat(k, 807) = mat(k, 807) + lmat(k, 807)
         mat(k, 814) = mat(k, 814) + lmat(k, 814)
         mat(k, 823) = mat(k, 823) + lmat(k, 823)
         mat(k, 825) = lmat(k, 825)
         mat(k, 826) = lmat(k, 826)
         mat(k, 827) = lmat(k, 827)
         mat(k, 828) = lmat(k, 828)
         mat(k, 831) = mat(k, 831) + lmat(k, 831)
         mat(k, 842) = lmat(k, 842)
         mat(k, 843) = lmat(k, 843)
         mat(k, 844) = lmat(k, 844)
         mat(k, 845) = lmat(k, 845)
         mat(k, 846) = mat(k, 846) + lmat(k, 846)
         mat(k, 851) = lmat(k, 851)
         mat(k, 853) = lmat(k, 853)
         mat(k, 855) = lmat(k, 855)
         mat(k, 856) = mat(k, 856) + lmat(k, 856)
         mat(k, 858) = mat(k, 858) + lmat(k, 858)
         mat(k, 869) = mat(k, 869) + lmat(k, 869)
         mat(k, 879) = mat(k, 879) + lmat(k, 879)
         mat(k, 880) = lmat(k, 880)
         mat(k, 882) = mat(k, 882) + lmat(k, 882)
         mat(k, 891) = mat(k, 891) + lmat(k, 891)
         mat(k, 904) = mat(k, 904) + lmat(k, 904)
         mat(k, 912) = mat(k, 912) + lmat(k, 912)
         mat(k, 922) = mat(k, 922) + lmat(k, 922)
         mat(k, 935) = mat(k, 935) + lmat(k, 935)
         mat(k, 946) = mat(k, 946) + lmat(k, 946)
         mat(k, 959) = mat(k, 959) + lmat(k, 959)
         mat(k, 970) = mat(k, 970) + lmat(k, 970)
         mat(k, 972) = mat(k, 972) + lmat(k, 972)
         mat(k, 973) = mat(k, 973) + lmat(k, 973)
         mat(k, 977) = lmat(k, 977)
         mat(k, 978) = lmat(k, 978)
         mat(k, 979) = lmat(k, 979)
         mat(k, 982) = mat(k, 982) + lmat(k, 982)
         mat(k, 983) = mat(k, 983) + lmat(k, 983)
         mat(k, 984) = mat(k, 984) + lmat(k, 984)
         mat(k, 991) = mat(k, 991) + lmat(k, 991)
         mat(k,1003) = mat(k,1003) + lmat(k,1003)
         mat(k,1005) = lmat(k,1005)
         mat(k,1006) = lmat(k,1006)
         mat(k,1008) = mat(k,1008) + lmat(k,1008)
         mat(k,1010) = mat(k,1010) + lmat(k,1010)
         mat(k,1011) = lmat(k,1011)
         mat(k,1014) = lmat(k,1014)
         mat(k,1018) = mat(k,1018) + lmat(k,1018)
         mat(k,1019) = mat(k,1019) + lmat(k,1019)
         mat(k,1020) = mat(k,1020) + lmat(k,1020)
         mat(k,1021) = mat(k,1021) + lmat(k,1021)
         mat(k,1022) = mat(k,1022) + lmat(k,1022)
         mat(k,1023) = mat(k,1023) + lmat(k,1023)
         mat(k,1025) = lmat(k,1025)
         mat(k,1032) = mat(k,1032) + lmat(k,1032)
         mat(k,1048) = mat(k,1048) + lmat(k,1048)
         mat(k,1067) = mat(k,1067) + lmat(k,1067)
         mat(k,1068) = lmat(k,1068)
         mat(k,1071) = lmat(k,1071)
         mat(k,1077) = lmat(k,1077)
         mat(k,1079) = mat(k,1079) + lmat(k,1079)
         mat(k,1090) = mat(k,1090) + lmat(k,1090)
         mat(k,1091) = mat(k,1091) + lmat(k,1091)
         mat(k,1092) = mat(k,1092) + lmat(k,1092)
         mat(k,1093) = mat(k,1093) + lmat(k,1093)
         mat(k,1100) = lmat(k,1100)
         mat(k,1101) = mat(k,1101) + lmat(k,1101)
         mat(k,1102) = lmat(k,1102)
         mat(k,1103) = mat(k,1103) + lmat(k,1103)
         mat(k,1104) = mat(k,1104) + lmat(k,1104)
         mat(k,1108) = mat(k,1108) + lmat(k,1108)
         mat(k,1121) = mat(k,1121) + lmat(k,1121)
         mat(k,1138) = mat(k,1138) + lmat(k,1138)
         mat(k,1157) = mat(k,1157) + lmat(k,1157)
         mat(k,1167) = mat(k,1167) + lmat(k,1167)
         mat(k,1169) = lmat(k,1169)
         mat(k,1170) = lmat(k,1170)
         mat(k,1174) = mat(k,1174) + lmat(k,1174)
         mat(k,1199) = mat(k,1199) + lmat(k,1199)
         mat(k,1224) = mat(k,1224) + lmat(k,1224)
         mat(k,1226) = lmat(k,1226)
         mat(k,1228) = lmat(k,1228)
         mat(k,1229) = lmat(k,1229)
         mat(k,1232) = lmat(k,1232)
         mat(k,1234) = mat(k,1234) + lmat(k,1234)
         mat(k,1235) = mat(k,1235) + lmat(k,1235)
         mat(k,1236) = mat(k,1236) + lmat(k,1236)
         mat(k,1260) = mat(k,1260) + lmat(k,1260)
         mat(k,1291) = mat(k,1291) + lmat(k,1291)
         mat(k,1315) = mat(k,1315) + lmat(k,1315)
         mat(k,1327) = mat(k,1327) + lmat(k,1327)
         mat(k,1328) = mat(k,1328) + lmat(k,1328)
         mat(k,1329) = mat(k,1329) + lmat(k,1329)
         mat(k,1330) = mat(k,1330) + lmat(k,1330)
         mat(k,1331) = mat(k,1331) + lmat(k,1331)
         mat(k,1332) = mat(k,1332) + lmat(k,1332)
         mat(k,1334) = mat(k,1334) + lmat(k,1334)
         mat(k,1335) = mat(k,1335) + lmat(k,1335)
         mat(k,1339) = lmat(k,1339)
         mat(k,1343) = mat(k,1343) + lmat(k,1343)
         mat(k,1347) = lmat(k,1347)
         mat(k,1350) = mat(k,1350) + lmat(k,1350)
         mat(k,1353) = lmat(k,1353)
         mat(k,1358) = mat(k,1358) + lmat(k,1358)
         mat(k,1359) = lmat(k,1359)
         mat(k,1362) = lmat(k,1362)
         mat(k,1363) = lmat(k,1363)
         mat(k,1369) = mat(k,1369) + lmat(k,1369)
         mat(k,1370) = lmat(k,1370)
         mat(k,1371) = mat(k,1371) + lmat(k,1371)
         mat(k,1372) = mat(k,1372) + lmat(k,1372)
         mat(k,1387) = mat(k,1387) + lmat(k,1387)
         mat(k,1405) = mat(k,1405) + lmat(k,1405)
         mat(k,1416) = mat(k,1416) + lmat(k,1416)
         mat(k,1418) = lmat(k,1418)
         mat(k,1419) = mat(k,1419) + lmat(k,1419)
         mat(k,1420) = lmat(k,1420)
         mat(k,1423) = lmat(k,1423)
         mat(k,1424) = lmat(k,1424)
         mat(k,1425) = lmat(k,1425)
         mat(k,1427) = lmat(k,1427)
         mat(k,1428) = mat(k,1428) + lmat(k,1428)
         mat(k,1429) = lmat(k,1429)
         mat(k,1431) = lmat(k,1431)
         mat(k,1432) = lmat(k,1432)
         mat(k,1435) = mat(k,1435) + lmat(k,1435)
         mat(k,1437) = lmat(k,1437)
         mat(k,1439) = lmat(k,1439)
         mat(k,1443) = mat(k,1443) + lmat(k,1443)
         mat(k,1451) = lmat(k,1451)
         mat(k,1452) = mat(k,1452) + lmat(k,1452)
         mat(k,1456) = mat(k,1456) + lmat(k,1456)
         mat(k,1467) = lmat(k,1467)
         mat(k,1479) = mat(k,1479) + lmat(k,1479)
         mat(k,1503) = mat(k,1503) + lmat(k,1503)
         mat(k,1521) = mat(k,1521) + lmat(k,1521)
         mat(k,1522) = mat(k,1522) + lmat(k,1522)
         mat(k,1525) = mat(k,1525) + lmat(k,1525)
         mat(k,1526) = mat(k,1526) + lmat(k,1526)
         mat(k,1528) = mat(k,1528) + lmat(k,1528)
         mat(k,1529) = mat(k,1529) + lmat(k,1529)
         mat(k,1537) = mat(k,1537) + lmat(k,1537)
         mat(k,1538) = mat(k,1538) + lmat(k,1538)
         mat(k,1539) = mat(k,1539) + lmat(k,1539)
         mat(k,1542) = lmat(k,1542)
         mat(k,1551) = mat(k,1551) + lmat(k,1551)
         mat(k,1580) = mat(k,1580) + lmat(k,1580)
         mat(k,1597) = lmat(k,1597)
         mat(k,1619) = mat(k,1619) + lmat(k,1619)
         mat(k,1628) = mat(k,1628) + lmat(k,1628)
         mat(k,1648) = mat(k,1648) + lmat(k,1648)
         mat(k,1666) = lmat(k,1666)
         mat(k,1669) = mat(k,1669) + lmat(k,1669)
         mat(k,1672) = mat(k,1672) + lmat(k,1672)
         mat(k,1674) = mat(k,1674) + lmat(k,1674)
         mat(k,1678) = lmat(k,1678)
         mat(k,1699) = mat(k,1699) + lmat(k,1699)
         mat(k,1739) = mat(k,1739) + lmat(k,1739)
         mat(k,1757) = mat(k,1757) + lmat(k,1757)
         mat(k,1770) = lmat(k,1770)
         mat(k,1772) = lmat(k,1772)
         mat(k,1773) = mat(k,1773) + lmat(k,1773)
         mat(k,1774) = mat(k,1774) + lmat(k,1774)
         mat(k,1775) = mat(k,1775) + lmat(k,1775)
         mat(k,1777) = mat(k,1777) + lmat(k,1777)
         mat(k,1780) = mat(k,1780) + lmat(k,1780)
         mat(k,1782) = mat(k,1782) + lmat(k,1782)
         mat(k,1785) = lmat(k,1785)
         mat(k,1786) = mat(k,1786) + lmat(k,1786)
         mat(k,1791) = mat(k,1791) + lmat(k,1791)
         mat(k,1794) = mat(k,1794) + lmat(k,1794)
         mat(k,1809) = mat(k,1809) + lmat(k,1809)
         mat(k,1815) = mat(k,1815) + lmat(k,1815)
         mat(k,1818) = lmat(k,1818)
         mat(k,1869) = mat(k,1869) + lmat(k,1869)
         mat(k,1915) = mat(k,1915) + lmat(k,1915)
         mat(k,1972) = mat(k,1972) + lmat(k,1972)
         mat(k,1984) = mat(k,1984) + lmat(k,1984)
         mat(k,2032) = mat(k,2032) + lmat(k,2032)
         mat(k,2092) = mat(k,2092) + lmat(k,2092)
         mat(k,2155) = mat(k,2155) + lmat(k,2155)
         mat(k,2157) = mat(k,2157) + lmat(k,2157)
         mat(k,2159) = mat(k,2159) + lmat(k,2159)
         mat(k,2167) = mat(k,2167) + lmat(k,2167)
         mat(k,2169) = mat(k,2169) + lmat(k,2169)
         mat(k,2192) = mat(k,2192) + lmat(k,2192)
         mat(k,2194) = mat(k,2194) + lmat(k,2194)
         mat(k,2201) = mat(k,2201) + lmat(k,2201)
         mat(k,2211) = mat(k,2211) + lmat(k,2211)
         mat(k,2212) = lmat(k,2212)
         mat(k,2215) = mat(k,2215) + lmat(k,2215)
         mat(k,2222) = mat(k,2222) + lmat(k,2222)
         mat(k,2269) = mat(k,2269) + lmat(k,2269)
         mat(k,2471) = mat(k,2471) + lmat(k,2471)
         mat(k,2490) = mat(k,2490) + lmat(k,2490)
         mat(k,2501) = mat(k,2501) + lmat(k,2501)
         mat(k,2506) = mat(k,2506) + lmat(k,2506)
         mat(k,2546) = mat(k,2546) + lmat(k,2546)
         mat(k,2550) = lmat(k,2550)
         mat(k,2613) = mat(k,2613) + lmat(k,2613)
         mat(k,2614) = mat(k,2614) + lmat(k,2614)
         mat(k,2617) = mat(k,2617) + lmat(k,2617)
         mat(k,2656) = mat(k,2656) + lmat(k,2656)
         mat(k,2658) = mat(k,2658) + lmat(k,2658)
         mat(k,2664) = mat(k,2664) + lmat(k,2664)
         mat(k,2668) = mat(k,2668) + lmat(k,2668)
         mat(k,2670) = mat(k,2670) + lmat(k,2670)
         mat(k,2684) = mat(k,2684) + lmat(k,2684)
         mat(k,2692) = mat(k,2692) + lmat(k,2692)
         mat(k,2698) = mat(k,2698) + lmat(k,2698)
         mat(k,2735) = mat(k,2735) + lmat(k,2735)
         mat(k,2738) = mat(k,2738) + lmat(k,2738)
         mat(k,2748) = lmat(k,2748)
         mat(k,2751) = lmat(k,2751)
         mat(k,2760) = mat(k,2760) + lmat(k,2760)
         mat(k,2762) = mat(k,2762) + lmat(k,2762)
         mat(k,2766) = lmat(k,2766)
         mat(k,2767) = mat(k,2767) + lmat(k,2767)
         mat(k, 285) = 0._r8
         mat(k, 286) = 0._r8
         mat(k, 323) = 0._r8
         mat(k, 396) = 0._r8
         mat(k, 402) = 0._r8
         mat(k, 467) = 0._r8
         mat(k, 470) = 0._r8
         mat(k, 471) = 0._r8
         mat(k, 539) = 0._r8
         mat(k, 569) = 0._r8
         mat(k, 582) = 0._r8
         mat(k, 591) = 0._r8
         mat(k, 607) = 0._r8
         mat(k, 631) = 0._r8
         mat(k, 661) = 0._r8
         mat(k, 666) = 0._r8
         mat(k, 669) = 0._r8
         mat(k, 685) = 0._r8
         mat(k, 693) = 0._r8
         mat(k, 699) = 0._r8
         mat(k, 705) = 0._r8
         mat(k, 712) = 0._r8
         mat(k, 754) = 0._r8
         mat(k, 777) = 0._r8
         mat(k, 778) = 0._r8
         mat(k, 779) = 0._r8
         mat(k, 781) = 0._r8
         mat(k, 787) = 0._r8
         mat(k, 788) = 0._r8
         mat(k, 792) = 0._r8
         mat(k, 798) = 0._r8
         mat(k, 799) = 0._r8
         mat(k, 802) = 0._r8
         mat(k, 832) = 0._r8
         mat(k, 847) = 0._r8
         mat(k, 849) = 0._r8
         mat(k, 850) = 0._r8
         mat(k, 852) = 0._r8
         mat(k, 854) = 0._r8
         mat(k, 874) = 0._r8
         mat(k, 875) = 0._r8
         mat(k, 881) = 0._r8
         mat(k, 892) = 0._r8
         mat(k, 893) = 0._r8
         mat(k, 896) = 0._r8
         mat(k, 898) = 0._r8
         mat(k, 902) = 0._r8
         mat(k, 921) = 0._r8
         mat(k, 923) = 0._r8
         mat(k, 926) = 0._r8
         mat(k, 928) = 0._r8
         mat(k, 930) = 0._r8
         mat(k, 933) = 0._r8
         mat(k, 950) = 0._r8
         mat(k, 954) = 0._r8
         mat(k, 956) = 0._r8
         mat(k, 962) = 0._r8
         mat(k, 966) = 0._r8
         mat(k,1000) = 0._r8
         mat(k,1002) = 0._r8
         mat(k,1015) = 0._r8
         mat(k,1028) = 0._r8
         mat(k,1040) = 0._r8
         mat(k,1047) = 0._r8
         mat(k,1049) = 0._r8
         mat(k,1050) = 0._r8
         mat(k,1078) = 0._r8
         mat(k,1081) = 0._r8
         mat(k,1107) = 0._r8
         mat(k,1119) = 0._r8
         mat(k,1134) = 0._r8
         mat(k,1137) = 0._r8
         mat(k,1141) = 0._r8
         mat(k,1146) = 0._r8
         mat(k,1152) = 0._r8
         mat(k,1153) = 0._r8
         mat(k,1156) = 0._r8
         mat(k,1160) = 0._r8
         mat(k,1161) = 0._r8
         mat(k,1165) = 0._r8
         mat(k,1175) = 0._r8
         mat(k,1178) = 0._r8
         mat(k,1190) = 0._r8
         mat(k,1192) = 0._r8
         mat(k,1193) = 0._r8
         mat(k,1196) = 0._r8
         mat(k,1201) = 0._r8
         mat(k,1204) = 0._r8
         mat(k,1207) = 0._r8
         mat(k,1209) = 0._r8
         mat(k,1210) = 0._r8
         mat(k,1211) = 0._r8
         mat(k,1217) = 0._r8
         mat(k,1218) = 0._r8
         mat(k,1219) = 0._r8
         mat(k,1221) = 0._r8
         mat(k,1222) = 0._r8
         mat(k,1238) = 0._r8
         mat(k,1249) = 0._r8
         mat(k,1251) = 0._r8
         mat(k,1252) = 0._r8
         mat(k,1255) = 0._r8
         mat(k,1259) = 0._r8
         mat(k,1263) = 0._r8
         mat(k,1264) = 0._r8
         mat(k,1267) = 0._r8
         mat(k,1269) = 0._r8
         mat(k,1270) = 0._r8
         mat(k,1271) = 0._r8
         mat(k,1277) = 0._r8
         mat(k,1278) = 0._r8
         mat(k,1279) = 0._r8
         mat(k,1282) = 0._r8
         mat(k,1287) = 0._r8
         mat(k,1288) = 0._r8
         mat(k,1293) = 0._r8
         mat(k,1295) = 0._r8
         mat(k,1296) = 0._r8
         mat(k,1299) = 0._r8
         mat(k,1305) = 0._r8
         mat(k,1306) = 0._r8
         mat(k,1307) = 0._r8
         mat(k,1316) = 0._r8
         mat(k,1317) = 0._r8
         mat(k,1318) = 0._r8
         mat(k,1319) = 0._r8
         mat(k,1325) = 0._r8
         mat(k,1333) = 0._r8
         mat(k,1337) = 0._r8
         mat(k,1338) = 0._r8
         mat(k,1341) = 0._r8
         mat(k,1344) = 0._r8
         mat(k,1345) = 0._r8
         mat(k,1346) = 0._r8
         mat(k,1348) = 0._r8
         mat(k,1349) = 0._r8
         mat(k,1351) = 0._r8
         mat(k,1354) = 0._r8
         mat(k,1356) = 0._r8
         mat(k,1374) = 0._r8
         mat(k,1380) = 0._r8
         mat(k,1383) = 0._r8
         mat(k,1388) = 0._r8
         mat(k,1390) = 0._r8
         mat(k,1391) = 0._r8
         mat(k,1396) = 0._r8
         mat(k,1398) = 0._r8
         mat(k,1399) = 0._r8
         mat(k,1404) = 0._r8
         mat(k,1415) = 0._r8
         mat(k,1426) = 0._r8
         mat(k,1430) = 0._r8
         mat(k,1433) = 0._r8
         mat(k,1434) = 0._r8
         mat(k,1436) = 0._r8
         mat(k,1440) = 0._r8
         mat(k,1441) = 0._r8
         mat(k,1447) = 0._r8
         mat(k,1450) = 0._r8
         mat(k,1455) = 0._r8
         mat(k,1460) = 0._r8
         mat(k,1468) = 0._r8
         mat(k,1482) = 0._r8
         mat(k,1488) = 0._r8
         mat(k,1490) = 0._r8
         mat(k,1493) = 0._r8
         mat(k,1495) = 0._r8
         mat(k,1500) = 0._r8
         mat(k,1501) = 0._r8
         mat(k,1502) = 0._r8
         mat(k,1504) = 0._r8
         mat(k,1505) = 0._r8
         mat(k,1506) = 0._r8
         mat(k,1508) = 0._r8
         mat(k,1509) = 0._r8
         mat(k,1515) = 0._r8
         mat(k,1517) = 0._r8
         mat(k,1519) = 0._r8
         mat(k,1527) = 0._r8
         mat(k,1532) = 0._r8
         mat(k,1535) = 0._r8
         mat(k,1540) = 0._r8
         mat(k,1545) = 0._r8
         mat(k,1552) = 0._r8
         mat(k,1554) = 0._r8
         mat(k,1555) = 0._r8
         mat(k,1556) = 0._r8
         mat(k,1564) = 0._r8
         mat(k,1569) = 0._r8
         mat(k,1573) = 0._r8
         mat(k,1574) = 0._r8
         mat(k,1575) = 0._r8
         mat(k,1576) = 0._r8
         mat(k,1577) = 0._r8
         mat(k,1579) = 0._r8
         mat(k,1581) = 0._r8
         mat(k,1583) = 0._r8
         mat(k,1585) = 0._r8
         mat(k,1586) = 0._r8
         mat(k,1592) = 0._r8
         mat(k,1593) = 0._r8
         mat(k,1595) = 0._r8
         mat(k,1596) = 0._r8
         mat(k,1599) = 0._r8
         mat(k,1602) = 0._r8
         mat(k,1605) = 0._r8
         mat(k,1606) = 0._r8
         mat(k,1610) = 0._r8
         mat(k,1614) = 0._r8
         mat(k,1615) = 0._r8
         mat(k,1616) = 0._r8
         mat(k,1617) = 0._r8
         mat(k,1620) = 0._r8
         mat(k,1621) = 0._r8
         mat(k,1622) = 0._r8
         mat(k,1624) = 0._r8
         mat(k,1625) = 0._r8
         mat(k,1631) = 0._r8
         mat(k,1632) = 0._r8
         mat(k,1634) = 0._r8
         mat(k,1635) = 0._r8
         mat(k,1639) = 0._r8
         mat(k,1649) = 0._r8
         mat(k,1651) = 0._r8
         mat(k,1652) = 0._r8
         mat(k,1658) = 0._r8
         mat(k,1660) = 0._r8
         mat(k,1661) = 0._r8
         mat(k,1663) = 0._r8
         mat(k,1668) = 0._r8
         mat(k,1673) = 0._r8
         mat(k,1675) = 0._r8
         mat(k,1676) = 0._r8
         mat(k,1677) = 0._r8
         mat(k,1679) = 0._r8
         mat(k,1681) = 0._r8
         mat(k,1685) = 0._r8
         mat(k,1686) = 0._r8
         mat(k,1687) = 0._r8
         mat(k,1696) = 0._r8
         mat(k,1697) = 0._r8
         mat(k,1698) = 0._r8
         mat(k,1701) = 0._r8
         mat(k,1702) = 0._r8
         mat(k,1711) = 0._r8
         mat(k,1712) = 0._r8
         mat(k,1721) = 0._r8
         mat(k,1724) = 0._r8
         mat(k,1740) = 0._r8
         mat(k,1741) = 0._r8
         mat(k,1742) = 0._r8
         mat(k,1746) = 0._r8
         mat(k,1751) = 0._r8
         mat(k,1752) = 0._r8
         mat(k,1754) = 0._r8
         mat(k,1758) = 0._r8
         mat(k,1761) = 0._r8
         mat(k,1763) = 0._r8
         mat(k,1765) = 0._r8
         mat(k,1766) = 0._r8
         mat(k,1769) = 0._r8
         mat(k,1776) = 0._r8
         mat(k,1779) = 0._r8
         mat(k,1788) = 0._r8
         mat(k,1790) = 0._r8
         mat(k,1792) = 0._r8
         mat(k,1793) = 0._r8
         mat(k,1795) = 0._r8
         mat(k,1796) = 0._r8
         mat(k,1797) = 0._r8
         mat(k,1799) = 0._r8
         mat(k,1801) = 0._r8
         mat(k,1806) = 0._r8
         mat(k,1807) = 0._r8
         mat(k,1808) = 0._r8
         mat(k,1810) = 0._r8
         mat(k,1811) = 0._r8
         mat(k,1813) = 0._r8
         mat(k,1814) = 0._r8
         mat(k,1816) = 0._r8
         mat(k,1817) = 0._r8
         mat(k,1819) = 0._r8
         mat(k,1820) = 0._r8
         mat(k,1823) = 0._r8
         mat(k,1837) = 0._r8
         mat(k,1839) = 0._r8
         mat(k,1842) = 0._r8
         mat(k,1866) = 0._r8
         mat(k,1867) = 0._r8
         mat(k,1868) = 0._r8
         mat(k,1872) = 0._r8
         mat(k,1876) = 0._r8
         mat(k,1877) = 0._r8
         mat(k,1878) = 0._r8
         mat(k,1880) = 0._r8
         mat(k,1881) = 0._r8
         mat(k,1882) = 0._r8
         mat(k,1934) = 0._r8
         mat(k,1936) = 0._r8
         mat(k,1937) = 0._r8
         mat(k,1968) = 0._r8
         mat(k,1969) = 0._r8
         mat(k,1970) = 0._r8
         mat(k,1980) = 0._r8
         mat(k,1983) = 0._r8
         mat(k,1985) = 0._r8
         mat(k,2002) = 0._r8
         mat(k,2005) = 0._r8
         mat(k,2020) = 0._r8
         mat(k,2029) = 0._r8
         mat(k,2054) = 0._r8
         mat(k,2056) = 0._r8
         mat(k,2063) = 0._r8
         mat(k,2065) = 0._r8
         mat(k,2068) = 0._r8
         mat(k,2070) = 0._r8
         mat(k,2073) = 0._r8
         mat(k,2077) = 0._r8
         mat(k,2083) = 0._r8
         mat(k,2087) = 0._r8
         mat(k,2089) = 0._r8
         mat(k,2099) = 0._r8
         mat(k,2116) = 0._r8
         mat(k,2120) = 0._r8
         mat(k,2123) = 0._r8
         mat(k,2125) = 0._r8
         mat(k,2132) = 0._r8
         mat(k,2137) = 0._r8
         mat(k,2139) = 0._r8
         mat(k,2141) = 0._r8
         mat(k,2152) = 0._r8
         mat(k,2153) = 0._r8
         mat(k,2154) = 0._r8
         mat(k,2156) = 0._r8
         mat(k,2160) = 0._r8
         mat(k,2162) = 0._r8
         mat(k,2164) = 0._r8
         mat(k,2165) = 0._r8
         mat(k,2166) = 0._r8
         mat(k,2168) = 0._r8
         mat(k,2170) = 0._r8
         mat(k,2180) = 0._r8
         mat(k,2183) = 0._r8
         mat(k,2186) = 0._r8
         mat(k,2187) = 0._r8
         mat(k,2191) = 0._r8
         mat(k,2197) = 0._r8
         mat(k,2198) = 0._r8
         mat(k,2202) = 0._r8
         mat(k,2207) = 0._r8
         mat(k,2208) = 0._r8
         mat(k,2210) = 0._r8
         mat(k,2214) = 0._r8
         mat(k,2217) = 0._r8
         mat(k,2218) = 0._r8
         mat(k,2221) = 0._r8
         mat(k,2225) = 0._r8
         mat(k,2226) = 0._r8
         mat(k,2227) = 0._r8
         mat(k,2228) = 0._r8
         mat(k,2239) = 0._r8
         mat(k,2244) = 0._r8
         mat(k,2248) = 0._r8
         mat(k,2249) = 0._r8
         mat(k,2250) = 0._r8
         mat(k,2252) = 0._r8
         mat(k,2253) = 0._r8
         mat(k,2254) = 0._r8
         mat(k,2255) = 0._r8
         mat(k,2258) = 0._r8
         mat(k,2262) = 0._r8
         mat(k,2264) = 0._r8
         mat(k,2271) = 0._r8
         mat(k,2272) = 0._r8
         mat(k,2274) = 0._r8
         mat(k,2276) = 0._r8
         mat(k,2277) = 0._r8
         mat(k,2358) = 0._r8
         mat(k,2368) = 0._r8
         mat(k,2389) = 0._r8
         mat(k,2408) = 0._r8
         mat(k,2411) = 0._r8
         mat(k,2427) = 0._r8
         mat(k,2428) = 0._r8
         mat(k,2450) = 0._r8
         mat(k,2473) = 0._r8
         mat(k,2486) = 0._r8
         mat(k,2488) = 0._r8
         mat(k,2491) = 0._r8
         mat(k,2492) = 0._r8
         mat(k,2493) = 0._r8
         mat(k,2496) = 0._r8
         mat(k,2498) = 0._r8
         mat(k,2502) = 0._r8
         mat(k,2503) = 0._r8
         mat(k,2505) = 0._r8
         mat(k,2507) = 0._r8
         mat(k,2524) = 0._r8
         mat(k,2527) = 0._r8
         mat(k,2536) = 0._r8
         mat(k,2540) = 0._r8
         mat(k,2548) = 0._r8
         mat(k,2566) = 0._r8
         mat(k,2570) = 0._r8
         mat(k,2574) = 0._r8
         mat(k,2579) = 0._r8
         mat(k,2582) = 0._r8
         mat(k,2585) = 0._r8
         mat(k,2586) = 0._r8
         mat(k,2588) = 0._r8
         mat(k,2591) = 0._r8
         mat(k,2594) = 0._r8
         mat(k,2595) = 0._r8
         mat(k,2596) = 0._r8
         mat(k,2598) = 0._r8
         mat(k,2603) = 0._r8
         mat(k,2616) = 0._r8
         mat(k,2618) = 0._r8
         mat(k,2624) = 0._r8
         mat(k,2634) = 0._r8
         mat(k,2635) = 0._r8
         mat(k,2638) = 0._r8
         mat(k,2641) = 0._r8
         mat(k,2643) = 0._r8
         mat(k,2646) = 0._r8
         mat(k,2647) = 0._r8
         mat(k,2648) = 0._r8
         mat(k,2650) = 0._r8
         mat(k,2653) = 0._r8
         mat(k,2654) = 0._r8
         mat(k,2655) = 0._r8
         mat(k,2657) = 0._r8
         mat(k,2662) = 0._r8
         mat(k,2663) = 0._r8
         mat(k,2666) = 0._r8
         mat(k,2669) = 0._r8
         mat(k,2671) = 0._r8
         mat(k,2681) = 0._r8
         mat(k,2682) = 0._r8
         mat(k,2683) = 0._r8
         mat(k,2686) = 0._r8
         mat(k,2687) = 0._r8
         mat(k,2688) = 0._r8
         mat(k,2689) = 0._r8
         mat(k,2691) = 0._r8
         mat(k,2694) = 0._r8
         mat(k,2696) = 0._r8
         mat(k,2697) = 0._r8
         mat(k,2706) = 0._r8
         mat(k,2712) = 0._r8
         mat(k,2714) = 0._r8
         mat(k,2722) = 0._r8
         mat(k,2724) = 0._r8
         mat(k,2725) = 0._r8
         mat(k,2734) = 0._r8
         mat(k,2739) = 0._r8
         mat(k,2744) = 0._r8
         mat(k,2745) = 0._r8
         mat(k,2747) = 0._r8
         mat(k,2749) = 0._r8
         mat(k,2750) = 0._r8
         mat(k,2752) = 0._r8
         mat(k,2753) = 0._r8
         mat(k,2754) = 0._r8
         mat(k,2755) = 0._r8
         mat(k,2756) = 0._r8
         mat(k,2757) = 0._r8
         mat(k,2758) = 0._r8
         mat(k,2759) = 0._r8
         mat(k,2761) = 0._r8
         mat(k,2763) = 0._r8
         mat(k,2764) = 0._r8
         mat(k,2765) = 0._r8
         mat(k,   1) = mat(k,   1) - dti(k)
         mat(k,   2) = mat(k,   2) - dti(k)
         mat(k,   3) = mat(k,   3) - dti(k)
         mat(k,   4) = mat(k,   4) - dti(k)
         mat(k,   5) = mat(k,   5) - dti(k)
         mat(k,   6) = mat(k,   6) - dti(k)
         mat(k,   7) = mat(k,   7) - dti(k)
         mat(k,   8) = mat(k,   8) - dti(k)
         mat(k,   9) = mat(k,   9) - dti(k)
         mat(k,  10) = mat(k,  10) - dti(k)
         mat(k,  11) = mat(k,  11) - dti(k)
         mat(k,  12) = mat(k,  12) - dti(k)
         mat(k,  13) = mat(k,  13) - dti(k)
         mat(k,  14) = mat(k,  14) - dti(k)
         mat(k,  15) = mat(k,  15) - dti(k)
         mat(k,  16) = mat(k,  16) - dti(k)
         mat(k,  17) = mat(k,  17) - dti(k)
         mat(k,  18) = mat(k,  18) - dti(k)
         mat(k,  19) = mat(k,  19) - dti(k)
         mat(k,  20) = mat(k,  20) - dti(k)
         mat(k,  21) = mat(k,  21) - dti(k)
         mat(k,  22) = mat(k,  22) - dti(k)
         mat(k,  23) = mat(k,  23) - dti(k)
         mat(k,  24) = mat(k,  24) - dti(k)
         mat(k,  25) = mat(k,  25) - dti(k)
         mat(k,  26) = mat(k,  26) - dti(k)
         mat(k,  27) = mat(k,  27) - dti(k)
         mat(k,  28) = mat(k,  28) - dti(k)
         mat(k,  29) = mat(k,  29) - dti(k)
         mat(k,  30) = mat(k,  30) - dti(k)
         mat(k,  31) = mat(k,  31) - dti(k)
         mat(k,  32) = mat(k,  32) - dti(k)
         mat(k,  33) = mat(k,  33) - dti(k)
         mat(k,  34) = mat(k,  34) - dti(k)
         mat(k,  35) = mat(k,  35) - dti(k)
         mat(k,  36) = mat(k,  36) - dti(k)
         mat(k,  37) = mat(k,  37) - dti(k)
         mat(k,  38) = mat(k,  38) - dti(k)
         mat(k,  39) = mat(k,  39) - dti(k)
         mat(k,  40) = mat(k,  40) - dti(k)
         mat(k,  41) = mat(k,  41) - dti(k)
         mat(k,  42) = mat(k,  42) - dti(k)
         mat(k,  43) = mat(k,  43) - dti(k)
         mat(k,  44) = mat(k,  44) - dti(k)
         mat(k,  45) = mat(k,  45) - dti(k)
         mat(k,  46) = mat(k,  46) - dti(k)
         mat(k,  47) = mat(k,  47) - dti(k)
         mat(k,  48) = mat(k,  48) - dti(k)
         mat(k,  49) = mat(k,  49) - dti(k)
         mat(k,  50) = mat(k,  50) - dti(k)
         mat(k,  51) = mat(k,  51) - dti(k)
         mat(k,  52) = mat(k,  52) - dti(k)
         mat(k,  53) = mat(k,  53) - dti(k)
         mat(k,  54) = mat(k,  54) - dti(k)
         mat(k,  60) = mat(k,  60) - dti(k)
         mat(k,  66) = mat(k,  66) - dti(k)
         mat(k,  72) = mat(k,  72) - dti(k)
         mat(k,  78) = mat(k,  78) - dti(k)
         mat(k,  80) = mat(k,  80) - dti(k)
         mat(k,  86) = mat(k,  86) - dti(k)
         mat(k,  87) = mat(k,  87) - dti(k)
         mat(k,  93) = mat(k,  93) - dti(k)
         mat(k,  99) = mat(k,  99) - dti(k)
         mat(k, 105) = mat(k, 105) - dti(k)
         mat(k, 106) = mat(k, 106) - dti(k)
         mat(k, 109) = mat(k, 109) - dti(k)
         mat(k, 112) = mat(k, 112) - dti(k)
         mat(k, 115) = mat(k, 115) - dti(k)
         mat(k, 117) = mat(k, 117) - dti(k)
         mat(k, 121) = mat(k, 121) - dti(k)
         mat(k, 125) = mat(k, 125) - dti(k)
         mat(k, 129) = mat(k, 129) - dti(k)
         mat(k, 131) = mat(k, 131) - dti(k)
         mat(k, 134) = mat(k, 134) - dti(k)
         mat(k, 139) = mat(k, 139) - dti(k)
         mat(k, 142) = mat(k, 142) - dti(k)
         mat(k, 145) = mat(k, 145) - dti(k)
         mat(k, 148) = mat(k, 148) - dti(k)
         mat(k, 153) = mat(k, 153) - dti(k)
         mat(k, 158) = mat(k, 158) - dti(k)
         mat(k, 163) = mat(k, 163) - dti(k)
         mat(k, 165) = mat(k, 165) - dti(k)
         mat(k, 168) = mat(k, 168) - dti(k)
         mat(k, 174) = mat(k, 174) - dti(k)
         mat(k, 180) = mat(k, 180) - dti(k)
         mat(k, 186) = mat(k, 186) - dti(k)
         mat(k, 192) = mat(k, 192) - dti(k)
         mat(k, 195) = mat(k, 195) - dti(k)
         mat(k, 202) = mat(k, 202) - dti(k)
         mat(k, 206) = mat(k, 206) - dti(k)
         mat(k, 212) = mat(k, 212) - dti(k)
         mat(k, 219) = mat(k, 219) - dti(k)
         mat(k, 224) = mat(k, 224) - dti(k)
         mat(k, 228) = mat(k, 228) - dti(k)
         mat(k, 231) = mat(k, 231) - dti(k)
         mat(k, 234) = mat(k, 234) - dti(k)
         mat(k, 237) = mat(k, 237) - dti(k)
         mat(k, 240) = mat(k, 240) - dti(k)
         mat(k, 243) = mat(k, 243) - dti(k)
         mat(k, 246) = mat(k, 246) - dti(k)
         mat(k, 251) = mat(k, 251) - dti(k)
         mat(k, 256) = mat(k, 256) - dti(k)
         mat(k, 259) = mat(k, 259) - dti(k)
         mat(k, 262) = mat(k, 262) - dti(k)
         mat(k, 265) = mat(k, 265) - dti(k)
         mat(k, 268) = mat(k, 268) - dti(k)
         mat(k, 273) = mat(k, 273) - dti(k)
         mat(k, 275) = mat(k, 275) - dti(k)
         mat(k, 280) = mat(k, 280) - dti(k)
         mat(k, 289) = mat(k, 289) - dti(k)
         mat(k, 294) = mat(k, 294) - dti(k)
         mat(k, 298) = mat(k, 298) - dti(k)
         mat(k, 303) = mat(k, 303) - dti(k)
         mat(k, 307) = mat(k, 307) - dti(k)
         mat(k, 311) = mat(k, 311) - dti(k)
         mat(k, 315) = mat(k, 315) - dti(k)
         mat(k, 320) = mat(k, 320) - dti(k)
         mat(k, 324) = mat(k, 324) - dti(k)
         mat(k, 328) = mat(k, 328) - dti(k)
         mat(k, 332) = mat(k, 332) - dti(k)
         mat(k, 338) = mat(k, 338) - dti(k)
         mat(k, 342) = mat(k, 342) - dti(k)
         mat(k, 348) = mat(k, 348) - dti(k)
         mat(k, 354) = mat(k, 354) - dti(k)
         mat(k, 360) = mat(k, 360) - dti(k)
         mat(k, 363) = mat(k, 363) - dti(k)
         mat(k, 366) = mat(k, 366) - dti(k)
         mat(k, 372) = mat(k, 372) - dti(k)
         mat(k, 378) = mat(k, 378) - dti(k)
         mat(k, 383) = mat(k, 383) - dti(k)
         mat(k, 388) = mat(k, 388) - dti(k)
         mat(k, 394) = mat(k, 394) - dti(k)
         mat(k, 400) = mat(k, 400) - dti(k)
         mat(k, 407) = mat(k, 407) - dti(k)
         mat(k, 412) = mat(k, 412) - dti(k)
         mat(k, 417) = mat(k, 417) - dti(k)
         mat(k, 422) = mat(k, 422) - dti(k)
         mat(k, 428) = mat(k, 428) - dti(k)
         mat(k, 431) = mat(k, 431) - dti(k)
         mat(k, 435) = mat(k, 435) - dti(k)
         mat(k, 438) = mat(k, 438) - dti(k)
         mat(k, 441) = mat(k, 441) - dti(k)
         mat(k, 449) = mat(k, 449) - dti(k)
         mat(k, 457) = mat(k, 457) - dti(k)
         mat(k, 469) = mat(k, 469) - dti(k)
         mat(k, 472) = mat(k, 472) - dti(k)
         mat(k, 474) = mat(k, 474) - dti(k)
         mat(k, 480) = mat(k, 480) - dti(k)
         mat(k, 486) = mat(k, 486) - dti(k)
         mat(k, 492) = mat(k, 492) - dti(k)
         mat(k, 498) = mat(k, 498) - dti(k)
         mat(k, 504) = mat(k, 504) - dti(k)
         mat(k, 510) = mat(k, 510) - dti(k)
         mat(k, 516) = mat(k, 516) - dti(k)
         mat(k, 522) = mat(k, 522) - dti(k)
         mat(k, 528) = mat(k, 528) - dti(k)
         mat(k, 534) = mat(k, 534) - dti(k)
         mat(k, 538) = mat(k, 538) - dti(k)
         mat(k, 545) = mat(k, 545) - dti(k)
         mat(k, 552) = mat(k, 552) - dti(k)
         mat(k, 556) = mat(k, 556) - dti(k)
         mat(k, 568) = mat(k, 568) - dti(k)
         mat(k, 571) = mat(k, 571) - dti(k)
         mat(k, 577) = mat(k, 577) - dti(k)
         mat(k, 580) = mat(k, 580) - dti(k)
         mat(k, 583) = mat(k, 583) - dti(k)
         mat(k, 592) = mat(k, 592) - dti(k)
         mat(k, 598) = mat(k, 598) - dti(k)
         mat(k, 608) = mat(k, 608) - dti(k)
         mat(k, 616) = mat(k, 616) - dti(k)
         mat(k, 625) = mat(k, 625) - dti(k)
         mat(k, 633) = mat(k, 633) - dti(k)
         mat(k, 641) = mat(k, 641) - dti(k)
         mat(k, 650) = mat(k, 650) - dti(k)
         mat(k, 660) = mat(k, 660) - dti(k)
         mat(k, 670) = mat(k, 670) - dti(k)
         mat(k, 675) = mat(k, 675) - dti(k)
         mat(k, 687) = mat(k, 687) - dti(k)
         mat(k, 698) = mat(k, 698) - dti(k)
         mat(k, 707) = mat(k, 707) - dti(k)
         mat(k, 713) = mat(k, 713) - dti(k)
         mat(k, 722) = mat(k, 722) - dti(k)
         mat(k, 730) = mat(k, 730) - dti(k)
         mat(k, 737) = mat(k, 737) - dti(k)
         mat(k, 747) = mat(k, 747) - dti(k)
         mat(k, 756) = mat(k, 756) - dti(k)
         mat(k, 762) = mat(k, 762) - dti(k)
         mat(k, 775) = mat(k, 775) - dti(k)
         mat(k, 786) = mat(k, 786) - dti(k)
         mat(k, 797) = mat(k, 797) - dti(k)
         mat(k, 807) = mat(k, 807) - dti(k)
         mat(k, 814) = mat(k, 814) - dti(k)
         mat(k, 823) = mat(k, 823) - dti(k)
         mat(k, 831) = mat(k, 831) - dti(k)
         mat(k, 846) = mat(k, 846) - dti(k)
         mat(k, 858) = mat(k, 858) - dti(k)
         mat(k, 869) = mat(k, 869) - dti(k)
         mat(k, 879) = mat(k, 879) - dti(k)
         mat(k, 891) = mat(k, 891) - dti(k)
         mat(k, 904) = mat(k, 904) - dti(k)
         mat(k, 912) = mat(k, 912) - dti(k)
         mat(k, 922) = mat(k, 922) - dti(k)
         mat(k, 935) = mat(k, 935) - dti(k)
         mat(k, 946) = mat(k, 946) - dti(k)
         mat(k, 959) = mat(k, 959) - dti(k)
         mat(k, 970) = mat(k, 970) - dti(k)
         mat(k, 977) = mat(k, 977) - dti(k)
         mat(k, 982) = mat(k, 982) - dti(k)
         mat(k, 991) = mat(k, 991) - dti(k)
         mat(k,1003) = mat(k,1003) - dti(k)
         mat(k,1010) = mat(k,1010) - dti(k)
         mat(k,1019) = mat(k,1019) - dti(k)
         mat(k,1032) = mat(k,1032) - dti(k)
         mat(k,1048) = mat(k,1048) - dti(k)
         mat(k,1067) = mat(k,1067) - dti(k)
         mat(k,1079) = mat(k,1079) - dti(k)
         mat(k,1093) = mat(k,1093) - dti(k)
         mat(k,1104) = mat(k,1104) - dti(k)
         mat(k,1108) = mat(k,1108) - dti(k)
         mat(k,1121) = mat(k,1121) - dti(k)
         mat(k,1138) = mat(k,1138) - dti(k)
         mat(k,1157) = mat(k,1157) - dti(k)
         mat(k,1167) = mat(k,1167) - dti(k)
         mat(k,1174) = mat(k,1174) - dti(k)
         mat(k,1199) = mat(k,1199) - dti(k)
         mat(k,1224) = mat(k,1224) - dti(k)
         mat(k,1234) = mat(k,1234) - dti(k)
         mat(k,1260) = mat(k,1260) - dti(k)
         mat(k,1291) = mat(k,1291) - dti(k)
         mat(k,1315) = mat(k,1315) - dti(k)
         mat(k,1329) = mat(k,1329) - dti(k)
         mat(k,1343) = mat(k,1343) - dti(k)
         mat(k,1358) = mat(k,1358) - dti(k)
         mat(k,1369) = mat(k,1369) - dti(k)
         mat(k,1387) = mat(k,1387) - dti(k)
         mat(k,1405) = mat(k,1405) - dti(k)
         mat(k,1416) = mat(k,1416) - dti(k)
         mat(k,1428) = mat(k,1428) - dti(k)
         mat(k,1443) = mat(k,1443) - dti(k)
         mat(k,1456) = mat(k,1456) - dti(k)
         mat(k,1479) = mat(k,1479) - dti(k)
         mat(k,1503) = mat(k,1503) - dti(k)
         mat(k,1522) = mat(k,1522) - dti(k)
         mat(k,1538) = mat(k,1538) - dti(k)
         mat(k,1551) = mat(k,1551) - dti(k)
         mat(k,1580) = mat(k,1580) - dti(k)
         mat(k,1619) = mat(k,1619) - dti(k)
         mat(k,1648) = mat(k,1648) - dti(k)
         mat(k,1672) = mat(k,1672) - dti(k)
         mat(k,1699) = mat(k,1699) - dti(k)
         mat(k,1739) = mat(k,1739) - dti(k)
         mat(k,1757) = mat(k,1757) - dti(k)
         mat(k,1774) = mat(k,1774) - dti(k)
         mat(k,1791) = mat(k,1791) - dti(k)
         mat(k,1809) = mat(k,1809) - dti(k)
         mat(k,1869) = mat(k,1869) - dti(k)
         mat(k,1972) = mat(k,1972) - dti(k)
         mat(k,2092) = mat(k,2092) - dti(k)
         mat(k,2159) = mat(k,2159) - dti(k)
         mat(k,2192) = mat(k,2192) - dti(k)
         mat(k,2222) = mat(k,2222) - dti(k)
         mat(k,2269) = mat(k,2269) - dti(k)
         mat(k,2471) = mat(k,2471) - dti(k)
         mat(k,2501) = mat(k,2501) - dti(k)
         mat(k,2546) = mat(k,2546) - dti(k)
         mat(k,2614) = mat(k,2614) - dti(k)
         mat(k,2668) = mat(k,2668) - dti(k)
         mat(k,2698) = mat(k,2698) - dti(k)
         mat(k,2738) = mat(k,2738) - dti(k)
         mat(k,2767) = mat(k,2767) - dti(k)
      end do

      end subroutine nlnmat_finit

      subroutine     nlnmat( avec_len, mat, y, rxt, lmat, dti )

      use chem_mods, only : gas_pcnst, rxntot, nzcnt

      implicit none

!----------------------------------------------
!       ... dummy arguments
!----------------------------------------------
      integer, intent(in) ::  avec_len
      real(r8), intent(in)    ::  dti(veclen)
      real(r8), intent(in)    ::  lmat(veclen,nzcnt)
      real(r8), intent(in)    ::  y(veclen,gas_pcnst)
      real(r8), intent(in)    ::  rxt(veclen,rxntot)
      real(r8), intent(inout) ::  mat(veclen,nzcnt)

      call     nlnmat01( avec_len, mat, y, rxt )
      call     nlnmat02( avec_len, mat, y, rxt )
      call     nlnmat03( avec_len, mat, y, rxt )
      call     nlnmat04( avec_len, mat, y, rxt )
      call     nlnmat05( avec_len, mat, y, rxt )
      call     nlnmat06( avec_len, mat, y, rxt )
      call     nlnmat07( avec_len, mat, y, rxt )
      call     nlnmat08( avec_len, mat, y, rxt )
      call     nlnmat09( avec_len, mat, y, rxt )
      call     nlnmat10( avec_len, mat, y, rxt )
      call     nlnmat11( avec_len, mat, y, rxt )
      call     nlnmat12( avec_len, mat, y, rxt )
      call     nlnmat_finit( avec_len, mat, lmat, dti )

      end subroutine nlnmat

      end module mo_nln_matrix
