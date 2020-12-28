#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_DATARELATIONPOOL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none

    CLASS(SimulationBoxes),pointer,private::m_MC_SimulationBox=>null()

    type,public::DataRelationRegister
        Class(ObjectsCollection),pointer::TheObjectsCollection=>null()
        character*100::TheName PREASSIGN ""

        contains

        procedure,public,non_overridable,pass::CopyFromOther=>CopyObjectsCollectionRegister
        procedure,public,non_overridable,pass::Clean=>Clean_ObjectsCollectionRegister
        Generic::Assignment(=)=>CopyObjectsCollectionRegister
        Final::CleanObjectsCollectionRegister
    end type DataRelationRegister

    !****Define a list DataRelationRegisterList with value type(DataRelationRegister),pointer**************
    DefGeneralList(DataRelationRegisterList,type(DataRelationRegister))

    type,public::DataRelationPool
        type(DataRelationRegisterList)::TheDataRelationRegisterList
        contains
        procedure,public,non_overridable,pass::Init=>InitDataRelationPool
        procedure,public,non_overridable,pass::RegisterOne=>RegisterOneDataRelation
        procedure,public,non_overridable,pass::CopyFromOther=>CopyDataRelationPool
        procedure,public,non_overridable,pass::Clean=>Clean_DataRelationPool
        Generic::Assignment(=)=>CopyDataRelationPool
        Final::CleanDataRelationPool
    end type

    private::CopyObjectsCollectionRegister
    private::Clean_ObjectsCollectionRegister
    private::CleanObjectsCollectionRegister
    private::InitDataRelationPool
    private::RegisterOneDataRelation
    private::CopyDataRelationPool
    private::CleanDataRelationPool

    contains

    !***************************************************************************
    subroutine CopyObjectsCollectionRegister(this,other)
        implicit none
        !---Dummy Vars---
        Class(DataRelationRegister),intent(out)::this
        type(DataRelationRegister),intent(in)::other
        !---Body----

        this%TheObjectsCollection => other%TheObjectsCollection

        this%TheName = other%TheName

        return
    end subroutine CopyObjectsCollectionRegister
    !***************************************************************************
    subroutine Clean_ObjectsCollectionRegister(this)
        implicit none
        !---Dummy Vars---
        Class(DataRelationRegister)::this
        !---Body---
        Nullify(this%TheObjectsCollection)
        this%TheObjectsCollection => Null()

        this%TheName = ""
        return
    end subroutine Clean_ObjectsCollectionRegister

    !***************************************************************************
    subroutine CleanObjectsCollectionRegister(this)
        implicit none
        !---Dummy Vars---
        type(DataRelationRegister)::this
        !---Body---
        call this%Clean()
        return
    end subroutine CleanObjectsCollectionRegister

    !****The member function of list DataRelationRegisterList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(DataRelationRegisterList,type(DataRelationRegister),Clean)

    !**********************************************
    subroutine InitDataRelationPool(this)
        implicit none
        !---Dummy Vars---
        Class(DataRelationPool)::this
        !---Body---

        call this%Clean()

        !---User need to register each event model---
        call this%RegisterOne("MC_SimulationBox",m_MC_SimulationBox)

        return
    end subroutine InitDataRelationPool

    !**********************************************
    subroutine RegisterOneDataRelation(this,ObjectsCollectionName,TheObjectsCollection)
        implicit none
        !---Dummy Vars---
        Class(DataRelationPool)::this
        character*(*),intent(in)::ObjectsCollectionName
        Class(ObjectsCollection),pointer::TheObjectsCollection
        !---Local Vars---
        integer::I
        type(DataRelationRegister)::newDataRelationRegister
        type(DataRelationRegister)::tempDataRelationRegister
        !---Body---

        DO I = 1,this%TheDataRelationRegisterList%GetListCount()
            tempDataRelationRegister = this%TheDataRelationRegisterList%GetValueByListIndex(I)

            if(ISSTREQUAL(tempDataRelationRegister%TheName,adjustl(trim(ObjectsCollectionName)))) then
                write(*,*) "MCPSCUERROR: The ObjectsCollection had been registed , please do not registe it again: ",adjustl(trim(ObjectsCollectionName))
                pause
                stop
            end if
        END DO

        call newDataRelationRegister%Clean()
        newDataRelationRegister%TheName = adjustl(trim(ObjectsCollectionName))
        newDataRelationRegister%TheObjectsCollection=>TheObjectsCollection

        call this%TheDataRelationRegisterList%AppendOne(newDataRelationRegister)

        return
    end subroutine RegisterOneDataRelation

    !************************************************
    subroutine CopyDataRelationPool(this,other)
        implicit none
        !---Dummy Vars---
        Class(DataRelationPool),intent(out)::this
        Class(DataRelationPool),intent(in)::other
        !---The Assignment(=) had been overridable
        this%TheDataRelationRegisterList = other%TheDataRelationRegisterList

    end subroutine CopyDataRelationPool

    !**********************************************
    subroutine Clean_DataRelationPool(this)
        implicit none
        !---Dummy Vars---
        Class(DataRelationPool)::this
        !---Body---

        call this%TheDataRelationRegisterList%CleanList()

        return
    end subroutine Clean_DataRelationPool

    !***********************************************

    subroutine CleanDataRelationPool(this)
        implicit none
        !---Dummy Vars---
        type(DataRelationPool)::this
        !---Body---

        call this%Clean()

        return
    end subroutine CleanDataRelationPool

end module SIMULATION_TYPEDEF_DATARELATIONPOOL
