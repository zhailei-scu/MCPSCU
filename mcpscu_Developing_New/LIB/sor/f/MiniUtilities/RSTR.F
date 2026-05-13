      subroutine rstr(Text,result)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  This function is set to convert a text into a real number !
!  with double precision                                     !
!  Update(Lei Zhai): To fix the problem of 'bitcast with     !
!  differing sizes      -4', we changed from rstr_old to istr! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      double precision tem
      character *(*) Text
      real result

       read(text,*)tem
       result = tem

       return
      end subroutine

      real function rstr_old(Text)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  This function is set to convert a text into a real number !
!  with double precision                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      double precision tem
      character *(*) Text

       read(text,*)tem
       rstr_old = tem

       return
      end function      
