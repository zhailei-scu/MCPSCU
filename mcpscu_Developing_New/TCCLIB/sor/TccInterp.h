#ifndef _TccInterp_H
#define _TccInterp_H

#include "DiffusorList.h"

#ifdef __cplusplus
extern "C" {
#endif
int InterpCScript(char* scriptStr);
#ifdef __cplusplus
}
#endif


#ifdef __cplusplus
extern "C"{
#endif
void GetInterpedArray(CDiffusorDef* theArray);
#ifdef __cplusplus
}
#endif

#endif
