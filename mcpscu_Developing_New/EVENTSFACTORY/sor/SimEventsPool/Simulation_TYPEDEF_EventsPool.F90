module SIMULATION_TYPEDEF_EVENTSPOOL
    use MC_CollectionEvent_MIGCOALE_CLUSTER_GPU
    implicit none

    Class(MC_MIGCOALE_CLUSTER_GPU),pointer,private::m_MC_MIGCOALE_CLUSTER_GPU=>null()

    type,public::EventsPool
        type(SingleCollectionEventsList_P)::TheSingleEventsList
        type(CrossCollectionEventsList_P)::TheCrossCollectionEventsList


    end type












    contains





end module SIMULATION_TYPEDEF_EVENTSPOOL
