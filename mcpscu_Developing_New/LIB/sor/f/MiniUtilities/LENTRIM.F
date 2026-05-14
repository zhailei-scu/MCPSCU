       subroutine LENTRIM(str,length)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  This function is get lebgth of trimed string
!  Author: Hou Qing
!  Update(Lei Zhai): To fix the problem of bitcast with differing sizes      -4, we changed from 
!  istr_old to istr 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       implicit none
       !---Dummy Vars---
       character(*),intent(in)::str
       integer,intent(out)::length
       !---Local Vars---
       integer::I
       integer::tempLength
       !---Body---
       tempLength = len_trim(str)


       DO I = tempLength,1,-1
        if(ICHAR(str(I:I)) .GT. 31 .and. str(I:I) .ne. CHAR(32)
     -   .and. str(I:I) .ne. CHAR(34) .and.
     -   ICHAR(str(I:I)) .LT. 127) then
            exit
        end if
       END DO

      length = I

      return
      end subroutine

       function LENTRIM_Old(str) result(length)
       implicit none
       !---Dummy Vars---
       character(*),intent(in)::str
       integer,intent(out)::length
       !---Local Vars---
       integer::I
       integer::tempLength
       !---Body---
       tempLength = len_trim(str)


       DO I = tempLength,1,-1
        if(ICHAR(str(I:I)) .GT. 31 .and. str(I:I) .ne. CHAR(32)
     -   .and. str(I:I) .ne. CHAR(34) .and.
     -   ICHAR(str(I:I)) .LT. 127) then
            exit
        end if
       END DO

      length = I

      return
      end function
