/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

__global__ void strongestNeighborScan_gpu(int * src, int * oldDst, int * newDst, int * oldWeight, int * newWeight, int * madeChanges, int distance, int numEdges) {
	int tid = blockIdx.x * blockDim.x + threadIdx.x;
	unsigned int totalThreads = blockDim.x * gridDim.x;
	if(totalThreads >= numEdges){
		if(tid >= numEdges){
			
			return;
		}
		if(tid - distance < 0){
			newWeight[tid] = oldWeight[tid];
			newDst[tid] = oldDst[tid];
			
			return;
		}
		if(src[tid] == src[tid - distance]){
			if(oldWeight[tid - distance] >= oldWeight[tid]){
				newWeight[tid] = oldWeight[tid - distance];
				newDst[tid] = oldDst[tid - distance];
				(*madeChanges) = 1;
				
			} else{
				newWeight[tid] = oldWeight[tid];
				newDst[tid] = oldDst[tid];
			}
			
			return;
		}else{
			newWeight[tid] = oldWeight[tid];
			newDst[tid] = oldDst[tid];
			return;
		}
	} else{
		int tasks = (numEdges / totalThreads) + 1;
		int i;
		for(i = 0; i < tasks; i++){
			int taskId = tid + (i * totalThreads);
			if(taskId >= numEdges){
				
				return;
			}
			if(taskId - distance < 0){
				newWeight[taskId] = oldWeight[taskId];
				newDst[taskId] = oldDst[taskId];
				
				
			}
			if(src[taskId] == src[taskId - distance]){
				if(oldWeight[taskId - distance] >= oldWeight[taskId]){
					newWeight[taskId] = oldWeight[taskId - distance];
					newDst[taskId] = oldDst[taskId - distance];
					(*madeChanges) = 1;
				
				} else{
					newWeight[taskId] = oldWeight[taskId];
					newDst[taskId] = oldDst[taskId];
				}
				
				
			}else{
				newWeight[taskId] = oldWeight[taskId];
				newDst[taskId] = oldDst[taskId];
				
			}
		}
	}
}
