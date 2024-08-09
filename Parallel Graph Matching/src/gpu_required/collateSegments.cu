/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

__global__ void collateSegments_gpu(int * src, int * scanResult, int * output, int numEdges) {
	unsigned int tid = blockIdx.x * blockDim.x + threadIdx.x;
	unsigned int totalThreads = blockDim.x * gridDim.x;
	if(totalThreads >= numEdges){
		if(tid >= numEdges){
			return;
		}
		if(tid == (numEdges - 1)){
			output[src[tid]] = scanResult[tid];
			return;
		}
		if(src[tid] == src[tid + 1]){
			return;
		}
		output[src[tid]] = scanResult[tid];
	} else{
		int tasks = (numEdges / totalThreads) + 1;
		int i;
		for(i = 0; i < tasks; i++){
			unsigned int taskId = tid + (i * totalThreads);
			if(taskId >= numEdges){
				return;
			}
			if(taskId == (numEdges - 1)){
				output[src[taskId]] = scanResult[taskId];
			}else if(src[taskId] != src[taskId + 1]){
				output[src[taskId]] = scanResult[taskId];
			} 
			
		}
	}
}
