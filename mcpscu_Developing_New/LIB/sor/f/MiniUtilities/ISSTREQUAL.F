       function ISSTREQUAL(SubjectStr,ObjectStr) result(theResult)
       implicit none
       !---Dummy Vars---
       character*(*),intent(in)::SubjectStr
       character*(*),intent(in)::ObjectStr
       logical,intent(out)::theResult
       !---Local Vars---
       character(len=256)::tempSubjectStr
       character(len=256)::tempObjectStr
       character,dimension(:),allocatable::tempSubjectStrAllo
       character,dimension(:),allocatable::tempObjectStrAllo
       integer::SubjectStrlength
       integer::ObjectStrlength
       logical::SubjectuseArray
       logical::ObjectuseArray
       integer::I
       !---Body---

       theResult = .true.

       SubjectuseArray = .false.

       ObjectuseArray = .false.

       SubjectStrlength = LENTRIM(adjustl(SubjectStr))

       ObjectStrlength = LENTRIM(adjustl(ObjectStr))

       if(SubjectStrlength .ne. ObjectStrlength) then
          theResult = .false.
          return
       end if

       if(SubjectStrlength .GT. len(tempSubjectStr)) then
            allocate(tempSubjectStrAllo(SubjectStrlength))
            tempSubjectStrAllo = trim(adjustl(SubjectStr))
            SubjectuseArray = .true.
       end if

       if(ObjectStrlength .GT. len(tempObjectStr)) then
            allocate(tempObjectStrAllo(ObjectStrlength))
            tempObjectStrAllo = trim(adjustl(ObjectStr))
            ObjectuseArray = .true.
       end if

       tempSubjectStr = adjustl(trim(SubjectStr))
       tempObjectStr = adjustl(trim(ObjectStr))

       if(SubjectuseArray .AND. (.not. ObjectuseArray)) then
          DO I = 1,SubjectStrlength
            if(tempSubjectStrAllo(I) .ne. tempObjectStr(I:I)) then
                theResult = .false.
                exit
            end if
          END DO

       else if((.not. SubjectuseArray) .AND. ObjectuseArray) then
          DO I = 1,SubjectStrlength
            if(tempSubjectStr(I:I) .ne. tempObjectStrAllo(I)) then
                theResult = .false.
                exit
            end if
          END DO

       else if(SubjectuseArray .AND. ObjectuseArray) then
          DO I = 1,SubjectStrlength
            if(tempSubjectStrAllo(I) .ne. tempObjectStrAllo(I)) then
                theResult = .false.
                exit
            end if
          END DO
       else
          if(tempSubjectStr(1:SubjectStrlength) .ne.
     -     tempObjectStr(1:ObjectStrlength)) then
            theResult = .false.
          end if
       end if

       if(allocated(tempSubjectStrAllo)) deallocate(tempSubjectStrAllo)

       if(allocated(tempObjectStrAllo)) deallocate(tempObjectStrAllo)
       return
       end function ISSTREQUAL
