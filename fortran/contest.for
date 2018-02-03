!      /*
!       * CSCI3180 Principles of Programming Languages
!       *
!       * --- Declaration ---
!       *
!       * I declare that the assignment here submitted is original except for source
!       * material explicitly acknowledged. I also acknowledge that I am aware of
!       * University policy and regulations on honesty in academic work, and of the
!       * disciplinary guidelines and procedures applicable to breaches of such policy
!       * and regulations, as contained in the website
!       * http://www.cuhk.edu.hk/policy/academichonesty/
!       *
!       * Assignment 1
!       * Name : Choi Man Kin
!       * Student ID : 1155077469
!       * Email Addr : mkchoi6@cse.cuhk.edu.hk
!       */

       program your_program_name
       implicit none

       integer iostat
       integer i

       character*15 tTeam
       integer srProcessed

       character*255 tFileN
       character*255 srFileN

       srProcessed = 1

       call GETARG(1,tFileN)
       call GETARG(2,srFileN)

       open(unit=11, file=tFileN, status="old")
       open(unit=12, file=srFileN, status="old")

       open(unit=20, file="reportfor.txt", status="unknown")

50     format("2018 CUHK CSE Programming Contest\r")
51     format("Team Score Report\r")
52     format("\r")
       write(unit=20, fmt=50)
       write(unit=20, fmt=51)
       write(unit=20, fmt=52)

!      loop start

15     format(A15, $)
10     read(unit=11, fmt=15, iostat=iostat) tTeam

       if (iostat .ne. 0) then
              goto 30
       endif

       if (index(tTeam, "\r") .gt. 0) then
              tTeam = tTeam(1:index(tTeam, "\r") - 1)
       endif

20     format(A15, $)
       write(unit=20, fmt=20) tTeam

       call processTeam(tTeam, srProcessed)

       tTeam = "               "
       i = i + 1

       goto 10

!      loop end

30     close(unit=11)
       close(unit=12)
       close(unit=20)

       end



!      processTeam
       subroutine processTeam(tTeam, srProcessed)
       implicit none

       character*15 tTeam
       integer srProcessed

       integer iostat

       character*15 srTeam
       integer srPId
       character*19 srOC
       integer srScor


       integer pMin(10)
       integer pMax(10)
       integer pBase(10)
       integer pNSubs(10)
       integer pTotal(10)

       integer i

       integer allPSco

       call resetTeamVariables(
     +        pMin, pMax, pBase, pNSubs, pTotal,
     +        allPSco)

10     format(A15, I1, A19, I3)
20     if (srProcessed .eq. 1) then
              read(unit=12, fmt=10, iostat=iostat)
     +               srTeam, srPid, srOc, srScor
              srProcessed = 0
       endif

       if (tTeam .ne. srTeam) then
              goto 30
       endif

       if (iostat .ne. 0) then
              goto 30
       endif
       
       i = srPid + 1
       if (tTeam .eq. srTeam) then
              pBase(i) = srScor

              if (srScor .lt. pMin(i)) then
                     pMin(i) = srScor
              endif
              if (srScor .gt. pMax(i)) then
                     pMax(i) = srScor
              endif

              pNSubs(i) =
     +               pNSubs(i) + 1
              pTotal(i) =
     +               pTotal(i) + srScor
       endif
       srProcessed = 1

       goto 20

30     call displayTeamResults(
     +        pMin, pMax, pBase, pNSubs, pTotal,
     +        allPSco)

       end



!      resetTeamVariables
       subroutine displayTeamResults(
     +        pMin, pMax, pBase, pNSubs, pTotal,
     +        allPSco)
       implicit none

       integer pMin(10)
       integer pMax(10)
       integer pBase(10)
       integer pNSubs(10)
       integer pTotal(10)

       integer allPSco

       real bScore
       real decay
       real rScore

       integer curPId
       integer curPSco

       integer i

       curPId = 0

40     i = curPId + 1

       curPSco = 0
       if (pMax(i) .gt. 0) then
              bScore = pBase(i)

              if (pBase(i) .eq. 100) then
                     decay = 1
              endif
              if (pBase(i) .lt. 100) then
                     decay = int(100.0/pNSubs(i))
                     decay = decay/100.0
              endif

              if (pMax(i) .le. 30) then
                     rScore = 0
              endif
              if (pMax(i) .gt. 30) then
                     rScore = 100-(pMax(i)-pMin(i))
              endif

              curPSco = 0.6*bScore*decay+
     +        0.3*pTotal(i)/pNSubs(i)+
     +        0.1*rScore
       endif

       curPSco = int(curPSco)
       allPSco = allPSco + curPSco

50     format(A1, I1, A1, I3, A1, $)
       write (unit=20, fmt=50) "(", curPId, ")", int(curPSco) , " "

       curPId = curPId+1
       if (curPId .lt. 10) then
              goto 40
       endif

60     format(A2, I4, A1)
       write (unit=20, fmt=60) "T:", int(allPSco), "\r"

       end




!      resetTeamVariables
       subroutine resetTeamVariables(
     +        pMin, pMax, pBase, pNSubs, pTotal,
     +        allPSco)
       implicit none

       integer pMin(10)
       integer pMax(10)
       integer pBase(10)
       integer pNSubs(10)
       integer pTotal(10)

       integer allPSco

       allPSco = 0

       pMin(1) = 100
       pMin(2) = 100
       pMin(3) = 100
       pMin(4) = 100
       pMin(5) = 100
       pMin(6) = 100
       pMin(7) = 100
       pMin(8) = 100
       pMin(9) = 100
       pMin(10) = 100
       
       pMax(1) = 0
       pMax(2) = 0
       pMax(3) = 0
       pMax(4) = 0
       pMax(5) = 0
       pMax(6) = 0
       pMax(7) = 0
       pMax(8) = 0
       pMax(9) = 0
       pMax(10) = 0
       
       pBase(1) = 0
       pBase(2) = 0
       pBase(3) = 0
       pBase(4) = 0
       pBase(5) = 0
       pBase(6) = 0
       pBase(7) = 0
       pBase(8) = 0
       pBase(9) = 0
       pBase(10) = 0
       
       pNSubs(1) = 0
       pNSubs(2) = 0
       pNSubs(3) = 0
       pNSubs(4) = 0
       pNSubs(5) = 0
       pNSubs(6) = 0
       pNSubs(7) = 0
       pNSubs(8) = 0
       pNSubs(9) = 0
       pNSubs(10) = 0
       
       pTotal(1) = 0
       pTotal(2) = 0
       pTotal(3) = 0
       pTotal(4) = 0
       pTotal(5) = 0
       pTotal(6) = 0
       pTotal(7) = 0
       pTotal(8) = 0
       pTotal(9) = 0
       pTotal(10) = 0

       end
