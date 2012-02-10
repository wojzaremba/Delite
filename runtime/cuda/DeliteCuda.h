#ifndef _DELITE_CUDA_H_
#define _DELITE_CUDA_H_

#include <list>
#include <map>
#include <queue>
#include <iostream>
#include <cuda_runtime.h>

using namespace std;

struct FreeItem {
    cudaEvent_t event;
    list<void*>* keys;
};

extern cudaStream_t h2dStream;
extern cudaStream_t d2hStream;
extern cudaStream_t kernelStream;

extern list<void*>* lastAlloc;
extern queue<FreeItem>* freeList;
extern map<void*,list<void*>*>* cudaMemoryMap;

extern void freeCudaMemory(FreeItem item);
extern void DeliteCudaMalloc(void** ptr, size_t size);
extern void hostInit();
extern void DeliteCudaMallocHost(void** ptr, size_t size);
extern void DeliteCudaMemcpyHtoDAsync(void* dptr, void* sptr, size_t size);
extern void DeliteCudaMemcpyDtoHAsync(void* dptr, void* sptr, size_t size);
extern void DeliteCudaMemcpyDtoDAsync(void *dptr, void* sptr, size_t size);
extern void DeliteCudaMemset(void *ptr, int value, size_t count);

#endif
