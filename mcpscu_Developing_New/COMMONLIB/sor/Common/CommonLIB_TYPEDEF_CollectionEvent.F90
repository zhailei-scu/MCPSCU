#include "../../../MACRO/Macro"
module COMMONLIB_TYPEDEF_COLLECTIONEVENT
    use COMMONLIB_TYPEDEF_EVENTMODEL
    use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
    implicit none

    !****************************************************************************
    type,public::SingleCollectionEvent
        type(ObjectsCollection),pointer::TheCollection=>null()
        type(EventModel),public::TheEventModel
        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopySingleCollectionEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_SingleCollectionEvent
        Generic::Assignment(=)=>CopySingleCollectionEventFromOther
        Final::CleanSingleCollectionEvent
    end type SingleCollectionEvent

    !****Define a list SingleCollectionEventsList with value type(SingleCollectionEvent)**************
    DefGeneralList(SingleCollectionEventsList,type(SingleCollectionEvent))

    !****************************************************************************
    type,public::CrossCollectionEvent
        type(ObjectsCollection),pointer::TheLeftObjectsCollection=>null()
        type(ObjectsCollection),pointer::TheRightObjectsCollection=>null()
        type(EventModel),public::TheEventModel

        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopyCrossCollectionEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_CrossCollectionEvent
        Generic::Assignment(=)=>CopyCrossCollectionEventFromOther
        Final::CleanCrossCollectionEvent
    end type CrossCollectionEvent

    !****Define a list CrossCollectionEventsList with value type(CrossCollectionEvent)**************
    DefGeneralList(CrossCollectionEventsList,type(CrossCollectionEvent))

    private::CopySingleCollectionEventFromOther
    private::Clean_SingleCollectionEvent
    private::CleanSingleCollectionEvent
    private::CopyCrossCollectionEventFromOther
    private::Clean_CrossCollectionEvent
    private::CleanCrossCollectionEvent

    contains
    !***************************************************
    subroutine CopySingleCollectionEventFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(SingleCollectionEvent),intent(out)::this
        CLASS(SingleCollectionEvent),intent(in)::other
        !---Body---

        this%TheCollection => other%TheCollection
        !---The Assignment(=) had been overideded
        this%TheEventModel = other%TheEventModel

        return
    end subroutine CopySingleCollectionEventFromOther

    !***************************************************
    subroutine Clean_SingleCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        CLASS(SingleCollectionEvent)::this
        !---Body---
        Nullify(this%TheCollection)
        this%TheCollection => null()

        call this%TheEventModel%Clean()
        return
    end subroutine Clean_SingleCollectionEvent

    !***************************************************
    subroutine CleanSingleCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        type(SingleCollectionEvent)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine CleanSingleCollectionEvent

    !****The member function of list SingleCollectionEventsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(SingleCollectionEventsList,type(SingleCollectionEvent),Clean)

    !***************************************************
    subroutine CopyCrossCollectionEventFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(CrossCollectionEvent),intent(out)::this
        CLASS(CrossCollectionEvent),intent(in)::other
        !---Body---

        this%TheLeftObjectsCollection => other%TheLeftObjectsCollection
        this%TheRightObjectsCollection => other%TheRightObjectsCollection
        this%TheEventModel = other%TheEventModel

        return
    end subroutine CopyCrossCollectionEventFromOther

    !***************************************************
    subroutine Clean_CrossCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        CLASS(CrossCollectionEvent)::this
        !---Body---
        Nullify(this%TheLeftObjectsCollection)
        this%TheLeftObjectsCollection => null()

        Nullify(this%TheRightObjectsCollection)
        this%TheRightObjectsCollection => null()

        call this%TheEventModel%Clean()
        return
    end subroutine Clean_CrossCollectionEvent

    !***************************************************
    subroutine CleanCrossCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        type(CrossCollectionEvent)::this
        !---Local Vars---
        call this%Clean()
        return
    end subroutine CleanCrossCollectionEvent

    !****The member function of list CrossCollectionEventsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(CrossCollectionEventsList,type(CrossCollectionEvent),Clean)

end module COMMONLIB_TYPEDEF_COLLECTIONEVENT