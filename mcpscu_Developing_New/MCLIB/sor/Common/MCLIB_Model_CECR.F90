module MCLIB_MODEL_CECR
    use MCLIB_TYPEDEF_ACLUSTER

    implicit none


    contains

    !*************************************************************
    function Cal_ECR_ByCECRModel(TheClusterList,LatticeLength) result(TheECR)
        !---Dummy Vars---
        type(AClusterList),intent(in)::TheClusterList
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF)::TheECR

    end function Cal_ECR_ByCECRModel

    !***************************************************************
    function Cal_CascadeShapeRecognize(TheClusterList,LatticeLength) result(TheDimLength)
        implicit none
        !---Dummy Vars---
        type(AClusterList),intent(in),target::TheClusterList
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF)::TheDimLength(3)
        !---Local Vars---
        type(AClusterList),pointer::cursor=>null()
        type(AClusterList),pointer::cursorOther=>null()
        type(AClusterList),target::tempClusterList
        real(kind=KINDDF)::SEP(3)
        real(kind=KINDDF)::Distance
        integer::I
        integer::J
        real(kind=KINDDF)::Sep_X
        real(kind=KINDDF)::Sep_Y
        real(kind=KINDDF)::Sep_Z
        real(kind=KINDDF)::RadSum
        real(kind=KINDDF)::Dist
        integer::NCUsed
        integer::NC
        integer::RecordIndex
        character*30::TheVersion
        real(kind=KINDDF)::ArrowLen
        real(kind=KINDDF)::Vector(3)
        logical::exitFlag
        real(kind=KINDDF)::maxSEP_Dim3(3)
        real(kind=KINDDF)::maxSEP_Dim2(3)
        real(kind=KINDDF)::maxSEP_Dim1(3)
        real(kind=KINDDF)::maxLenPW2(3)
        real(kind=KINDDF)::maxLen(3)
        real(kind=KINDDF)::tempSep(3)
        real(kind=KINDDF),dimension(:,:),allocatable::projectPos_Dim2
        real(kind=KINDDF),dimension(:,:),allocatable::projectPos_Dim1
        real(kind=KINDDF)::Ratio
        real(kind=KINDDF)::maxDistance_Dim3
        integer::IDLeftMaxDist_Dim3
        integer::IDRightMaxDist_Dim3
        real(kind=KINDDF)::maxDistance_Dim2
        integer::IDLeftMaxDist_Dim2
        integer::IDRightMaxDist_Dim2
        real(kind=KINDDF)::maxDistance_Dim1
        integer::IDLeftMaxDist_Dim1
        integer::IDRightMaxDist_Dim1
        integer::JC
        real(kind=KINDDF)::threshold
        integer::TotalEffectDim
        real(kind=KINDDF)::tempValue
        real(kind=KINDDF)::gap
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
        end if



        !************************Dim3**************************
        maxDistance_Dim3 = -1.D0
        IDLeftMaxDist_Dim3 = 0
        IDRightMaxDist_Dim3 = 0

        call DeAllocateArray_Host(projectPos_Dim2,"projectPos_Dim2")
        call AllocateArray_Host(projectPos_Dim2,tempClusterList%GetList_Count(),3,"projectPos_Dim2")

        call DeAllocateArray_Host(projectPos_Dim1,"projectPos_Dim1")
        call AllocateArray_Host(projectPos_Dim1,tempClusterList%GetList_Count(),3,"projectPos_Dim1")

        cursor=>tempClusterList
        Do while(associated(cursor))

            cursorOther=>cursor%next

            Do while(associated(cursorOther))


                SEP = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS
                Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                Distance = DSQRT(Distance)

                if(Distance .GT. maxDistance_Dim3) then
                    maxDistance_Dim3 = Distance
                    IDLeftMaxDist_Dim3 = IC
                    IDRightMaxDist_Dim3 = JC
                end if

                cursorOther=>cursorOther%next
            End Do

            cursor=>cursor%next
        End Do


            maxSEP_Dim3 = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IDLeftMaxDist_Dim3)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IDRightMaxDist_Dim3)%m_POS

            maxLenPW2(3) = sum(maxSEP_Dim3**2)

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    tempSep = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IDLeftMaxDist_Dim3)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS

                    Ratio = sum(maxSEP_Dim3*tempSep)/maxLenPW2(3)

                    projectPos_Dim2(IC-ICFrom+1,1:3) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS + Ratio*maxSEP_Dim3
                end if

            END DO


            !************************Dim2**************************
            maxDistance_Dim2 = -1.D0
            IDLeftMaxDist_Dim2 = 0
            IDRightMaxDist_Dim2 = 0

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    DO JC = IC+1,ICTo
                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                            (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                             Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                            SEP = projectPos_Dim2(IC-ICFrom+1,1:3) - projectPos_Dim2(JC-ICFrom+1,1:3)
                            Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                            Distance = DSQRT(Distance)

                            if(Distance .GT. maxDistance_Dim2) then
                                maxDistance_Dim2 = Distance
                                IDLeftMaxDist_Dim2 = IC - ICFrom + 1
                                IDRightMaxDist_Dim2 = JC - ICFrom + 1
                            end if

                        end if

                    END DO

                end if
            END DO

            maxSEP_Dim2 = projectPos_Dim2(IDLeftMaxDist_Dim2,1:3) - projectPos_Dim2(IDRightMaxDist_Dim2,1:3)

            maxLenPW2(2) = sum(maxSEP_Dim2**2)

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    tempSep = projectPos_Dim2(IDLeftMaxDist_Dim2,1:3) - projectPos_Dim2(IC - ICFrom + 1,1:3)

                    Ratio = sum(maxSEP_Dim2*tempSep)/maxLenPW2(2)

                    projectPos_Dim1(IC-ICFrom+1,1:3) = projectPos_Dim2(IC - ICFrom + 1,1:3) + Ratio*maxSEP_Dim2
                end if

            END DO


            !************************Dim1**************************
            maxDistance_Dim1 = -1.D0
            IDLeftMaxDist_Dim1 = 0
            IDRightMaxDist_Dim1 = 0

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    DO JC = IC+1,ICTo
                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                            (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                             Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                            SEP = projectPos_Dim1(IC-ICFrom+1,1:3) - projectPos_Dim1(JC-ICFrom+1,1:3)
                            Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                            Distance = DSQRT(Distance)

                            if(Distance .GT. maxDistance_Dim1) then
                                maxDistance_Dim1 = Distance
                                IDLeftMaxDist_Dim1 = IC - ICFrom + 1
                                IDRightMaxDist_Dim1 = JC - ICFrom + 1
                            end if

                        end if

                    END DO

                end if
            END DO

            maxSEP_Dim1 = projectPos_Dim1(IDLeftMaxDist_Dim1,1:3) - projectPos_Dim1(IDRightMaxDist_Dim1,1:3)

            maxLenPW2(1) = sum(maxSEP_Dim1**2)

            maxLen = DSQRT(maxLenPW2)


            DO I = 1,size(maxLen)

                DO J = 1,size(maxLen) - 1

                    if(maxLen(J) .LT. maxLen(J+1)) then
                        tempValue = maxLen(J+1)
                        maxLen(J+1) = maxLen(J)
                        maxLen(J) = tempValue
                    end if
                END DO
            END DO

            TotalEffectDim = 1
            DO I = 1,size(maxLen) - 1
                gap = maxLen(I) - maxLen(I+1)

                if(abs(gap)/maxLen(I) .GT. threshold) then
                    TotalEffectDim = TotalEffectDim + 1
                end if
            END DO


            TheFormat = "(I30,1x,3(1PE30.10,1x),I30,1x,1(1PE30.10,1x))"
            TheFormat = adjustl(TheFormat)
            write(hOutInfo,fmt=TheFormat(1:LENTRIM(TheFormat))) IBox,                                        &
                                                                maxLen(1:3)/Host_Boxes%LatticeLength,        &
                                                                TotalEffectDim,                              &
                                                                threshold

            call DeAllocateArray_Host(projectPos_Dim2,"projectPos_Dim2")
            call DeAllocateArray_Host(projectPos_Dim1,"projectPos_Dim1")
        END DO


        !************Out the Capture info***************


        DO IBox = 1,MultiBox

        END DO


        call Host_Boxes%Clean()


        close(hOutInfo)

        return
    end function Cal_CascadeShapeRecognize



end module MCLIB_MODEL_CECR
