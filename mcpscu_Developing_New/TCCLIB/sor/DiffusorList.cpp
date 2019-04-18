#include "DiffusorList.h"
#ifdef __cplusplus
#include <iostream>
#include <cstring>
#else
#include <string.h>
#include <stdlib.h>
#endif

#ifdef __cplusplus
using namespace std;
#endif

void copyCDiffusorDef(CDiffusorDef *Dest,CDiffusorDef *Source){

  memset(Dest->symbol,0,20);
  strcpy(Dest->symbol,Source->symbol);
  Dest->DiffusorValueType = Source->DiffusorValueType;
  Dest->DiffuseCoefficient_Value = Source->DiffuseCoefficient_Value;
  Dest->PreFactor = Source->PreFactor;
  Dest->ActEnergy = Source->ActEnergy;
  Dest->ECRValueType = Source->ECRValueType;
  Dest->ECR = Source->ECR;
}

#ifdef __cplusplus
void InitDiffusorList(CDiffusorList **list){
  if(NULL == *list){
    *list = new CDiffusorList;
  }

  (*list)->size = 0;
  (*list)->next = NULL;
}
#else
void InitDiffusorList(CDiffusorList **list){
  if(NULL == *list){
    *list = malloc(sizeof(CDiffusorList));
  }

  (*list)->size = 0;
  (*list)->next = NULL;
}
#endif

#ifdef __cplusplus
void AppendDiffusor(CDiffusorList *list, CDiffusorDef *element){
  CDiffusorList *cursor = NULL;
  if(0==list->size){
    copyCDiffusorDef(&(list->data),element);
    list->size++;
  }else{
    cursor = list;
    while(NULL != cursor->next){
      cursor = cursor->next;
    }
    cursor->next = new CDiffusorList[1];
    copyCDiffusorDef(&(cursor->next->data),element);
    cursor->next->next = NULL;
    list->size++;
    cursor->next->size = list->size;
  }

}
#else
void AppendDiffusor(CDiffusorList *list, CDiffusorDef *element){
  CDiffusorList *cursor = NULL;
  if(0==list->size){
    copyCDiffusorDef(&(list->data),element);
    list->size++;
  }else{
    cursor = list;
    while(NULL != cursor->next){
      cursor = cursor->next;
    }
    cursor->next = malloc(sizeof(CDiffusorList));
    copyCDiffusorDef(&(cursor->next->data),element);
    cursor->next->next = NULL;
    list->size++;
    cursor->next->size = list->size;
  }

}
#endif
