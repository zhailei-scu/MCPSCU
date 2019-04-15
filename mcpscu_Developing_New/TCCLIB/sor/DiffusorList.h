#ifndef __DIFFUSORLIST_H
#define __DIFFUSORLIST_H

#ifdef __cplusplus
struct CDiffusorDef{
#else
typedef struct{
#endif
  char symbol[20];

  int DiffusorValueType;
  double DiffuseCoefficient_Value;

  // If the DiffuseCoefficient type is by Arrhenius or BCluster(bigger cluster),use this
  double PreFactor;
  double ActEnergy;

  int ECRValueType;
  double ECR;
#ifdef __cplusplus
};
#else
}CDiffusorDef;
#endif

#ifdef __cplusplus
struct CDiffusorList{
#else
typedef struct CDiffusorList_s{
#endif

  CDiffusorDef data;

  #ifdef __cplusplus
  CDiffusorList* next;
  #else
  struct CDiffusorList_s* next;
  #endif

  int size;

#ifdef __cplusplus
};
#else
}CDiffusorList;
#endif

void init(CDiffusorList **list);

void append(CDiffusorList *list, CDiffusorDef *element);

#endif
