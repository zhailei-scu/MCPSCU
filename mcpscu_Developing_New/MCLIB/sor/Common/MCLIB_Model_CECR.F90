module MCLIB_MODEL_CECR
    use MCLIB_TYPEDEF_ACLUSTER

    implicit none

    real(kind=KINDDF),parameter,private::a0 = 1.55888
    real(kind=KINDDF),parameter,private::b0 = 0.18233
    real(kind=KINDDF),parameter,private::c0 = 0.04502
    real(kind=KINDDF),parameter,private::d0 = 6.86935

    contains

    !*************************************************************
    function Cal_ECR_ByCECRModel(TheClusterList,LatticeLength) result(TheECR)
        !---Dummy Vars---
        type(AClusterList),intent(in)::TheClusterList
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF)::TheECR
        !---Local Vars---
        real(kind=KINDDF)::TheDimLength(3)
        !---Body---

        TheDimLength = Cal_CascadeShapeRecognize(TheClusterList,TheEffectCount)/LatticeLength

        if(TheEffectCount .LE. 0) then
            TheECR = TheDimLength(1)*LatticeLength
        else
                    !---Shape adjust---
            TheDimLength(1) = SQRT(((TheDimLength(1)**2)*(1+SQRT(1-(TheDimLength(2)/TheDimLength(1))**2)))/2);
            TheDimLength(2) = SQRT(((TheDimLength(1)**2)*(1-SQRT(1-(TheDimLength(2)/TheDimLength(1))**2)))/2);
            TheDimLength(3) = SQRT(((TheDimLength(2)**2)*(1-SQRT(1-(TheDimLength(3)/TheDimLength(2))**2)))/2);

            TheECR = (a0*(TheEffectCount**(-b0/TheDimLength(1)-2*b0/(TheDimLength(2)/2))) + &
                      c0*((TheDimLength(1))**(-d0/TheEffectCount+1)+2*(TheDimLength(2) /2)**(-d0/TheEffectCount+1)))* &
                      (3-exp(-TheDimLength(1)*TheEffectCount)-2*exp(-(TheDimLength(2) /2)*TheEffectCount))

        end if

        return
    end function Cal_ECR_ByCECRModel

    !***************************************************************
    function Cal_CascadeShapeRecognize(TheClusterList) result(TheDimLength)
        implicit none
        !---Dummy Vars---
        type(AClusterList),intent(in),target::TheClusterList
        real(kind=KINDDF)::TheDimLength(3)
        !---Local Vars---
        type(AClusterList),pointer::cursor=>null()
        type(AClusterList),pointer::cursorOther=>null()
        type(AClusterList),pointer::cursor_LeftMaxDist_Dim3=>null()
        type(AClusterList),pointer::cursor_RightMaxDist_Dim3=>null()
        integer::IDLeftMaxDist_Dim2
        integer::IDRightMaxDist_Dim2
        integer::IDLeftMaxDist_Dim1
        integer::IDRightMaxDist_Dim1
        type(AClusterList),target::tempClusterList
        integer::TheCount
        real(kind=KINDDF)::SEP(3)
        real(kind=KINDDF)::Distance
        real(kind=KINDDF)::maxSEP_Dim3(3)
        real(kind=KINDDF)::maxSEP_Dim2(3)
        real(kind=KINDDF)::maxSEP_Dim1(3)
        real(kind=KINDDF)::maxLenPW2(3)
        real(kind=KINDDF)::tempSep(3)
        real(kind=KINDDF),dimension(:,:),allocatable::projectPos_Dim2
        real(kind=KINDDF),dimension(:,:),allocatable::projectPos_Dim1
        real(kind=KINDDF)::Ratio
        real(kind=KINDDF)::maxDistance_Dim3
        real(kind=KINDDF)::maxDistance_Dim2
        real(kind=KINDDF)::maxDistance_Dim1
        integer::IC
        integer::JC
        integer::I
        integer::J
        real(kind=KINDDF)::tempValue
        !-----------Body--------------

        TheDimLength = 0.D0

        cursor=>TheClusterList

        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: The ClusterList had not been allocated"
            pause
            stop
        end if

        if(cursor%GetList_Count() .LE. 0) then
            return
        end if

        !*******Get Information from configuration ****************************************
        call tempClusterList%Clean_ClusterList()
        Do while(associated(cursor))

            if(cursor%TheCluster%m_Statu .eq. p_ACTIVEFREE_STATU .or. cursor%TheCluster%m_Statu .eq. p_ACTIVEINGB_STATU) then
                call tempClusterList%AppendOneCluster(cursor%TheCluster)
            end if

            cursor=>cursor%next
        End Do

        if(tempClusterList%GetList_Count() .LE. 0) then
            return
        else if(tempClusterList%GetList_Count() .LE. 1) then
            TheDimLength = tempClusterList%TheCluster%m_RAD
            return
        end if

        !************************Dim3**************************
        maxDistance_Dim3 = -1.D0

        call DeAllocateArray_Host(projectPos_Dim2,"projectPos_Dim2")
        call AllocateArray_Host(projectPos_Dim2,tempClusterList%GetList_Count(),3,"projectPos_Dim2")

        call DeAllocateArray_Host(projectPos_Dim1,"projectPos_Dim1")
        call AllocateArray_Host(projectPos_Dim1,tempClusterList%GetList_Count(),3,"projectPos_Dim1")

        cursor=>tempClusterList
        Do while(associated(cursor))

            cursorOther=>cursor%next

            Do while(associated(cursorOther))


                SEP = cursor%TheCluster%m_POS - cursorOther%TheCluster%m_POS
                Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                Distance = DSQRT(Distance)

                if(Distance .GT. maxDistance_Dim3) then
                    maxDistance_Dim3 = Distance
                    cursor_LeftMaxDist_Dim3=>cursor
                    cursor_RightMaxDist_Dim3=>cursorOther
                end if

                cursorOther=>cursorOther%next
            End Do

            cursor=>cursor%next
        End Do

        maxSEP_Dim3 = cursor_LeftMaxDist_Dim3%TheCluster%m_POS - cursor_RightMaxDist_Dim3%TheCluster%m_POS

        maxLenPW2(3) = sum(maxSEP_Dim3**2)

        TheCount = 0
        cursor=>tempClusterList
        Do while(associated(cursor))

            tempSep = cursor_LeftMaxDist_Dim3%TheCluster%m_POS - cursor%TheCluster%m_POS

            Ratio = sum(maxSEP_Dim3*tempSep)/maxLenPW2(3)

            TheCount = TheCount + 1

            projectPos_Dim2(TheCount,1:3) = cursor%TheCluster%m_POS + Ratio*maxSEP_Dim3

            cursor=>cursor%next
        END DO

        !************************Dim2**************************
        maxDistance_Dim2 = -1.D0
        IDLeftMaxDist_Dim2 = 0
        IDRightMaxDist_Dim2 = 0

        DO IC = 1,TheCount

            DO JC = IC+1,TheCount

                SEP = projectPos_Dim2(IC,1:3) - projectPos_Dim2(JC,1:3)
                Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                Distance = DSQRT(Distance)

                if(Distance .GT. maxDistance_Dim2) then
                    maxDistance_Dim2 = Distance
                    IDLeftMaxDist_Dim2 = IC
                    IDRightMaxDist_Dim2 = JC
                end if

            END DO

        END DO

        maxSEP_Dim2 = projectPos_Dim2(IDLeftMaxDist_Dim2,1:3) - projectPos_Dim2(IDRightMaxDist_Dim2,1:3)

        maxLenPW2(2) = sum(maxSEP_Dim2**2)

        DO IC = 1,TheCount
            tempSep = projectPos_Dim2(IDLeftMaxDist_Dim2,1:3) - projectPos_Dim2(IC,1:3)

            Ratio = sum(maxSEP_Dim2*tempSep)/maxLenPW2(2)

            projectPos_Dim1(IC,1:3) = projectPos_Dim2(IC,1:3) + Ratio*maxSEP_Dim2
        END DO


        !************************Dim1**************************
        maxDistance_Dim1 = -1.D0
        IDLeftMaxDist_Dim1 = 0
        IDRightMaxDist_Dim1 = 0

        DO IC = 1,TheCount

            DO JC = IC+1,TheCount

                SEP = projectPos_Dim1(IC,1:3) - projectPos_Dim1(JC,1:3)
                Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                Distance = DSQRT(Distance)

                if(Distance .GT. maxDistance_Dim1) then
                    maxDistance_Dim1 = Distance
                    IDLeftMaxDist_Dim1 = IC + 1
                    IDRightMaxDist_Dim1 = JC + 1
                end if

            END DO

        END DO

        maxSEP_Dim1 = projectPos_Dim1(IDLeftMaxDist_Dim1,1:3) - projectPos_Dim1(IDRightMaxDist_Dim1,1:3)

        maxLenPW2(1) = sum(maxSEP_Dim1**2)

        TheDimLength = DSQRT(maxLenPW2)

        DO I = 1,size(TheDimLength)
            DO J = 1,size(TheDimLength) - 1
                if(TheDimLength(J) .LT. TheDimLength(J+1)) then
                    tempValue = TheDimLength(J+1)
                    TheDimLength(J+1) = TheDimLength(J)
                    TheDimLength(J) = tempValue
                end if
            END DO
        END DO

        call DeAllocateArray_Host(projectPos_Dim2,"projectPos_Dim2")
        call DeAllocateArray_Host(projectPos_Dim1,"projectPos_Dim1")

        Nullify(cursor)
        cursor=>null()
        Nullify(cursorOther)
        cursorOther=>null()
        Nullify(cursor_LeftMaxDist_Dim3)
        cursor_LeftMaxDist_Dim3=>null()
        Nullify(cursor_RightMaxDist_Dim3)
        cursor_RightMaxDist_Dim3=>null()

        return
    end function Cal_CascadeShapeRecognize



end module MCLIB_MODEL_CECR
