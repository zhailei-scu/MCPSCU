#include "../../../MACRO/Macro"
module COMMONLIB_TYPEDEF_COLLECTIONEVENT
    use COMMONLIB_TYPEDEF_EVENTMODEL
    use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
    implicit none

    !****************************************************************************

    type,public::CollectionEvent

    end type CollectionEvent


    type,abstract,extends(CollectionEvent),public::SingleCollectionEvent
        CLASS(ObjectsCollection),pointer::TheCollection=>null()
        type(EventModel),public::TheEventModel
        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopySingleCollectionEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_SingleCollectionEvent
        !---abstract method---
        procedure(DefSingleCollectionEventConstructProc),pass,deferred::TheDefSingleCollectionEventonstructProc
        procedure(DefSingleCollectionEventCleanProc),pass,deferred,private::TheDefSingleCollectionEventCleanProc
        procedure(DefSingleCollectionEventCopyProc),pass,deferred,private::TheDefSingleCollectionEventCopyProc
        !---Based On Our test, the abstract cannot override (=), other wise his childrens' assginment(=) would be invalid
        !Generic::Assignment(=)=>CopySingleCollectionEventFromOther
        !---Based On Our test, the abstract cannot own Final (de-constructor)---
        !Final::CleanSingleCollectionEvent
    end type SingleCollectionEvent

    !****Define a list SingleCollectionEventsList_P with value class(SingleCollectionEvent),pointer**************
    DefGeneralList_P(SingleCollectionEventsList_P,class(SingleCollectionEvent))

    !****************************************************************************
    type,abstract,extends(CollectionEvent),public::CrossCollectionEvent
        CLASS(ObjectsCollection),pointer::TheLeftObjectsCollection=>null()
        CLASS(ObjectsCollection),pointer::TheRightObjectsCollection=>null()
        type(EventModel),public::TheEventModel
        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopyCrossCollectionEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_CrossCollectionEvent
        !---abstract method---
        procedure(DefCrossCollectionEventConstructProc),pass,deferred::TheDefCrossCollectionEventConstructProc
        procedure(DefCrossCollectionEventCleanProc),pass,deferred,private::TheDefCrossCollectionEventCleanProc
        procedure(DefCrossCollectionEventCopyProc),pass,deferred,private::TheDefCrossCollectionEventCopyProc
        !---Based On Our test, the abstract cannot override (=), other wise his childrens' assginment(=) would be invalid
        !Generic::Assignment(=)=>CopyCrossCollectionEventFromOther
        !---Based On Our test, the abstract cannot own Final (de-constructor)---
        !Final::CleanCrossCollectionEvent
    end type CrossCollectionEvent

    !****Define a list CrossCollectionEventsList_P with value calss(CrossCollectionEvent) ,pointer**************
    DefGeneralList_P(CrossCollectionEventsList_P,class(CrossCollectionEvent))

    private::CopySingleCollectionEventFromOther
    private::Clean_SingleCollectionEvent
    !private::CleanSingleCollectionEvent
    private::CopyCrossCollectionEventFromOther
    private::Clean_CrossCollectionEvent
    !private::CleanCrossCollectionEvent

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

    !*****The declare for abstract method***************
    !*****Note: this is not same with the "Fortran 95/2003 For Scientists and Engineers, Third Edition (chinese version)(P668)"
    !*****Because the abstract useage way in this book is not depended on PGFORTRAN compiler. The  following is based on our test
    !*****and verified to suit for pgfortran.
    subroutine DefSingleCollectionEventConstructProc(this)
        implicit none
        CLASS(SingleCollectionEvent)::this
    end subroutine DefSingleCollectionEventConstructProc

    subroutine DefSingleCollectionEventCleanProc(this)
        implicit none
        CLASS(SingleCollectionEvent)::this
    end subroutine DefSingleCollectionEventCleanProc

    subroutine DefSingleCollectionEventCopyProc(this,Other)
        implicit none
        CLASS(SingleCollectionEvent),intent(out)::this
        CLASS(SingleCollectionEvent),intent(in)::Other
    end subroutine DefSingleCollectionEventCopyProc

    !***************************************************
    !subroutine CleanSingleCollectionEvent(this)
    !    implicit none
    !    !---Dummy Vars---
    !    type(SingleCollectionEvent)::this
    !    !---Local Vars---
    !    call this%Clean()
    !
    !    return
    !end subroutine CleanSingleCollectionEvent

    !****The member function of list SingleCollectionEventsList_P**************
    DefGeneralListFuncSpan_WithValueCleanMethod_P(SingleCollectionEventsList_P,class(SingleCollectionEvent),Clean)

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

    !*****The declare for abstract method***************
    !*****Note: this is not same with the "Fortran 95/2003 For Scientists and Engineers, Third Edition (chinese version)(P668)"
    !*****Because the abstract useage way in this book is not depended on PGFORTRAN compiler. The  following is based on our test
    !*****and verified to suit for pgfortran.
    subroutine DefCrossCollectionEventConstructProc(this)
        implicit none
        CLASS(CrossCollectionEvent)::this
    end subroutine DefCrossCollectionEventConstructProc

    subroutine DefCrossCollectionEventCleanProc(this)
        implicit none
        CLASS(CrossCollectionEvent)::this
    end subroutine DefCrossCollectionEventCleanProc

    subroutine DefCrossCollectionEventCopyProc(this,Other)
        implicit none
        CLASS(CrossCollectionEvent),intent(out)::this
        CLASS(CrossCollectionEvent),intent(in)::Other
    end subroutine DefCrossCollectionEventCopyProc

    !***************************************************
    !subroutine CleanCrossCollectionEvent(this)
    !   implicit none
    !    !---Dummy Vars---
    !    type(CrossCollectionEvent)::this
    !    !---Local Vars---
    !    call this%Clean()
    !    return
    !end subroutine CleanCrossCollectionEvent

    !****The member function of list CrossCollectionEventsList_P**************
    DefGeneralListFuncSpan_WithValueCleanMethod_P(CrossCollectionEventsList_P,class(CrossCollectionEvent),Clean)

end module COMMONLIB_TYPEDEF_COLLECTIONEVENT