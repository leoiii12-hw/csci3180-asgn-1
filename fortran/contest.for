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

       character*15 tTeam

       open(unit=11, file="teams.txt", status="old")
       open(unit=20, file="reportcob.txt", status="unknown")

50     format("2018 CUHK CSE Programming Contest")
51     format("Team Score Report")
52     format("")
       write(unit=20, fmt=50)
       write(unit=20, fmt=51)
       write(unit=20, fmt=52)

10     read(unit=11, *, end=30) tTeam

20     format(A15, $)
       write (unit=20, fmt=20) tTeam

       call processTeam(tTeam)

       goto 10

30     close(unit=11)
       close(unit=20)

       end



!      processTeam
       subroutine processTeam(tTeam)
       implicit none

       character*15 tTeam

       integer curPId

       real curPSco
       real tScore

       curPId = 0

       curPSco = 0
       tScore = 0

10     call processProblem(tTeam, curPId, curPSco)

       tScore = tScore + curPSco

       if (curPId .eq. 9) then
              goto 30
       endif

       curPId = curPId + 1

       goto 10

20     format(A2, I4)
30     write (unit=20, fmt=20) "T:", int(tScore)

       end



!      processProblem
       subroutine processProblem(tTeam, curPId, score)
       implicit none

       character*15 tTeam
       integer curPid

       integer iostat

       character*15 srTeam
       integer srPId
       character*19 srOC
       integer srScor

       integer pMin
       integer pMax
       integer pBase

       integer pNSubs
       integer pTotal

       real score

       pNSubs = 0
       pTotal = 0

       score = 0

       pMin = 100
       pMax = 0
       pBase = 0

       open(unit=12, file="submission-records.txt", status="old")

10     format(A15, I1, A19, I3)
20     read(unit=12, fmt=10, iostat=iostat) srTeam, srPid, srOc, srScor

       if (tTeam .eq. srTeam) then
              if (srPId .eq. curPId) then
                     pBase = srScor

                     if (srScor .lt. pMin) then
                            pMin = srScor
                     endif
                     if (srScor .gt. pMax) then
                            pMax = srScor
                     endif

                     pNSubs = pNSubs + 1
                     pTotal = pTotal + srScor
              endif
       endif

       if (iostat .ne. 0) then
              goto 30
       endif

       goto 20

!      Calculate the problem score

30     score = 0

       if (pMax .gt. 0) then

              score = score + 0.6 * pBase

              if (pBase < 100) then
                     score = score / pNSubs
              endif

              score = score + 0.3 * pTotal / pNSubs

              if (pMax > 30) then
                     score = score + 0.1 * (100 - pMax + pMin)
              endif

       endif

40     format(A1, I1, A1, I3, A1, $)
       write (unit=20, fmt=40) "(", curPId, ")", int(score), " "

       close(unit=12)

       end

