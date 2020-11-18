module COMMONLIB_TYPEDEF_GENERALLIST
  implicit none

  #define DefGeneralListStart(Name) type,public::Name

!--Based on our test, 1: the two members must located in different line ,however, when macro are exported, all in one line , it
!--would make mistake, so we have to use ';' to seperate two lines
!--                   2: when we use ';' plus '\' in macro, we must ensure that nothing placed after '\' and , there are nothing before #define (even a blank)
!--                   3: However, the usage of ';' still make all source not true in different line , so we use ourself definded ENTER to make a mark ,
!--                      and replace ENTER to \n after preprocess during compiling the source
#define ConstructGeneralListValMember(Name,ValueType) type(Name),pointer::Next=>null()[ENTER] \
  integer::ListCount = 0[ENTER] \
  ValueType::TheValue
#define ConstructGeneralListFuncMember(Name,ValueType) contains; \
    procedure,non_overridable,public,pass::CopyListFromOther=>Copy##Name##ListFromOther[ENTER] \
    procedure,non_overridable,public,pass::AppendOne=>AppendOne_##Name##List[ENTER] \
    procedure,non_overridable,public,pass::AppendArray=>AppendArray_##Name##List[ENTER] \
    procedure,non_overridable,public,pass::GetValueByListIndex=>GetValueBy##Name##ListIndex[ENTER] \
    procedure,non_overridable,public,pass::GetListCount=>Get##Name##List_Count[ENTER] \
    procedure,non_overridable,public,pass::CleanList=>Clean_##Name##List[ENTER] \
    Generic::Assignment(=)=>Copy##Name##ListFromOther[ENTER] \
    Final::Clean##Name##List

  #define DefGeneralListEnd(Name) end type Name



#define MethodDomain(Name) private::Copy##Name##ListFromOther[ENTER] \
                           private::AppendOne_##Name##List[ENTER] \
                           private::AppendArray_##Name##List[ENTER] \
                           private::GetValueBy##Name##ListIndex[ENTER] \
                           private::Get##Name##List_Count[ENTER] \
                           private::Clean_##Name##List[ENTER] \
                           private::Clean##Name##List

!--Based on our test, 1: the 'end type' must located in different line with 'type,public',however, when macro are exported, all in one line , it
!--would make mistake, so we have to use ';' to seperate two lines, or use two macro span in different line like
!--g(G)eneralListStart(Name)
!--g(G)eneralListEnd(Name)  to create a type
!--                   2: when we use ';' plus '\' in macro, we must ensure that nothing placed after '\' and , there are nothing before #define (even a blank)
!--                   3: However, the usage of ';' still make all source not true in different line , so we use ourself definded ENTER to make a mark ,
!--                      and replace ENTER to \n after preprocess during compiling the source
#define DefGeneralList(Name,ValueType) DefGeneralListStart(Name); \
                                       ConstructGeneralListValMember(Name,ValueType)[ENTER] \
                                       ConstructGeneralListFuncMember(Name,ValueType)[ENTER] \
                                       DefGeneralListEnd(Name)[ENTER] \
                                       MethodDomain(Name)


#define DefGeneralListFuncSpan(Name,ValueType) subroutine Copy##Name##ListFromOther(this)[ENTER] \
    implicit none[ENTER] \
    CLASS(Name)::this[ENTER] \
    return[ENTER] \
  end subroutine Copy##Name##ListFromOther[ENTER] \
  subroutine AppendOne_##Name##List(this)[ENTER] \
    implicit none[ENTER] \
    CLASS(Name)::this[ENTER] \
    return[ENTER] \
  end subroutine AppendOne_##Name##List[ENTER] \
  subroutine AppendArray_##Name##List(this)[ENTER] \
    implicit none[ENTER] \
    CLASS(Name)::this[ENTER] \
    return[ENTER] \
  end subroutine AppendArray_##Name##List[ENTER] \
  subroutine GetValueBy##Name##ListIndex(this)[ENTER] \
    implicit none[ENTER] \
    CLASS(Name)::this[ENTER] \
    return[ENTER] \
  end subroutine GetValueBy##Name##ListIndex[ENTER] \
  subroutine Get##Name##List_Count(this)[ENTER] \
    implicit none[ENTER] \
    CLASS(Name)::this[ENTER] \
    return[ENTER] \
  end subroutine Get##Name##List_Count[ENTER] \
  subroutine Clean_##Name##List(this)[ENTER] \
    implicit none[ENTER] \
    CLASS(Name)::this[ENTER] \
    return[ENTER] \
  end subroutine Clean_##Name##List[ENTER] \
  subroutine Clean##Name##List(this)[ENTER] \
    implicit none[ENTER] \
    type(Name)::this[ENTER] \
    return[ENTER] \
  end subroutine Clean##Name##List
















  !---Way one to generate a type auto---
  DefGeneralListStart(CrossEventList2)
  !constructGeneralListValMember(CrossEventList2,type(STRList))
  !constructGeneralListFuncMember(CrossEventList2,type(STRList))
  ConstructGeneralListValMember(CrossEventList2,integer)
  ConstructGeneralListFuncMember(CrossEventList2,integer)
  DefGeneralListEnd(CrossEventList2)

  !---Way two to generate a type auto---
  !defGeneralList(CrossEventList3,type(STRList))
  DefGeneralList(CrossEventList3,integer)

  contains

  DefGeneralListFuncSpan(CrossEventList2,integer)
  DefGeneralListFuncSpan(CrossEventList3,integer)

!  defGeneralListFuncSpan(CrossEventList2,type(STRList))
!  defGeneralListFuncSpan(CrossEventList3,type(STRList))

end module COMMONLIB_TYPEDEF_GENERALLIST
