#include "../../../Macro"
module HYBRIDMCRT_TYPEDEF_EVCCLUSTER
    use MCLIB_TYPEDEF_ACLUSTER
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none


    TYPE,extends(ACluster),public::EVCCluster
        type(AClusterList),public::TheList
    end type

    TYPE,extends(SecondOrder_ClusterLists),public::EVCClustersList
        integer,public::Identify = 0

        integer,private::ListCount = 0
        type(EVCClustersList),pointer::next=>null()

        contains

        procedure,public,pass,non_overridable::AppendOneEVCCluster
        procedure,public,pass,non_overridable::AppendOtherEVCClustersList
        procedure,public,pass,non_overridable::GetList_Count=>GetEVCClustersList_Count
        procedure,public,pass,non_overridable::Find=>FindEVCClustersListByIdentify
        procedure,public,pass,non_overridable::CopyEVCClustersListFromOther
        procedure,public,pass,non_overridable::Clean_EVCClustersList
        Generic::Assignment(=)=>CopyEVCClustersListFromOther
        Final::CleanEVCClustersList
    END TYPE EVCClustersList


    private::AppendOneEVCCluster
    private::AppendOtherEVCClustersList
    private::GetEVCClustersList_Count
    private::FindEVCClustersListByIdentify
    private::CopyEVCClustersListFromOther
    private::Clean_EVCClustersList
    private::CleanEVCClustersList

    contains



end module HYBRIDMCRT_TYPEDEF_EVCCLUSTER
