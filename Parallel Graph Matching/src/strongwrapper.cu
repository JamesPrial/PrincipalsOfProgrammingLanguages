/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include "utils.hpp"
#include "gpuHeaders.cuh"
#include <iostream>

using namespace std;

#define threadsPerBlock 256

void findStrongNeighbor(GraphData graph, int *& strongNeighbor, int numthreads)
{
	int num_thread_blocks = (numthreads + threadsPerBlock - 1) / threadsPerBlock;
	
	int numVertices = graph.numNodes;
	int numEdges = graph.numEdges;
	
	//Prepare various GPU arrays that we're going to need:
	
	int * strongNeighbor_gpu;//will hold strongest neighbor for each vertex
	cudaMalloc((void **)&strongNeighbor_gpu, numVertices * sizeof(int));
	cudaMemcpy(strongNeighbor_gpu, strongNeighbor, numVertices * sizeof(int), cudaMemcpyHostToDevice);
	
	int * src_gpu;//holds the src nodes in edge list
	int * dst_gpu;//holds the dst nodes in edge list
	int * weight_gpu;//holds the edge weights in edge list
	cudaMalloc((void **)&src_gpu, numEdges * sizeof(int));
	cudaMalloc((void **)&dst_gpu, numEdges * sizeof(int));
	cudaMalloc((void **)&weight_gpu, numEdges * sizeof(int));
	cudaMemcpy(src_gpu, graph.src, numEdges * sizeof(int), cudaMemcpyHostToDevice);
	cudaMemcpy(dst_gpu, graph.dst, numEdges * sizeof(int), cudaMemcpyHostToDevice);
	cudaMemcpy(weight_gpu, graph.weight, numEdges * sizeof(int), cudaMemcpyHostToDevice);
	
	int * temp1_gpu;//a temporary array for data we don't need to keep for long
	int * temp2_gpu;//a temporary array for data we don't need to keep for long
	int * temp3_gpu;//a temporary array for data we don't need to keep for long
	int * temp4_gpu;//a temporary array for data we don't need to keep for long
	cudaMalloc((void **)&temp1_gpu, (1+numEdges) * sizeof(int));
	cudaMalloc((void **)&temp2_gpu, (1+numEdges) * sizeof(int));
	cudaMalloc((void **)&temp3_gpu, (1+numEdges) * sizeof(int));
	cudaMalloc((void **)&temp4_gpu, (1+numEdges) * sizeof(int));
	
	int * madeChanges_gpu; //1-element array that strongestNeighborScan_gpu will mark to indicate whether it made changes
	cudaMalloc((void **)&madeChanges_gpu, sizeof(int));
	
	//Step 1a: prepare initial values for segment scan:
	cudaMemcpy(temp1_gpu, dst_gpu, numEdges * sizeof(int), cudaMemcpyDeviceToDevice);
	cudaMemcpy(temp3_gpu, weight_gpu, numEdges * sizeof(int), cudaMemcpyDeviceToDevice);
	
	//Step 1b: segment scan; each vertex extends a hand to its strongest neighbor
	int distance = 1;
	while(true) {
		if(distance > numEdges * 2 + 1) {
			cerr << "ERROR: failed to stop segment scan on-time.\n";
			break;
		}
		
		cudaMemset(madeChanges_gpu, 0, sizeof(int));
		strongestNeighborScan_gpu<<<num_thread_blocks, threadsPerBlock>>>(src_gpu, temp1_gpu, temp2_gpu, temp3_gpu, temp4_gpu, madeChanges_gpu, distance, numEdges);
		swapArray((void**) &temp1_gpu, (void**) &temp2_gpu);
		swapArray((void**) &temp3_gpu, (void**) &temp4_gpu);
		
		//break from segment scan if it's no longer doing anything
		int madeChanges = 0;
		cudaMemcpy(&madeChanges, madeChanges_gpu, sizeof(int), cudaMemcpyDeviceToHost);
		if(madeChanges == 0) {
			break;
		}
		
		distance *= 2;
	}
	int * strongestDst_gpu = temp1_gpu;
	temp1_gpu = NULL;
	//int * strongestWeight_gpu = temp3_gpu;
	
	//Step 1c: Collate strongest neighbors into strongNeighbor array
	collateSegments_gpu<<<num_thread_blocks, threadsPerBlock>>>(src_gpu,strongestDst_gpu, strongNeighbor_gpu, numEdges);
	
	temp1_gpu = strongestDst_gpu;
	strongestDst_gpu = NULL;
	
	cudaMemcpy(strongNeighbor, strongNeighbor_gpu, numVertices * sizeof(int), cudaMemcpyDeviceToHost);
	
	//Wait until pending GPU operations are complete:
	cudaDeviceSynchronize();
	
	//free GPU arrays
	cudaFree(strongNeighbor_gpu);
	cudaFree(src_gpu);
	cudaFree(dst_gpu);
	cudaFree(weight_gpu);
	cudaFree(temp1_gpu);
	cudaFree(temp2_gpu);
	cudaFree(temp3_gpu);
	cudaFree(temp4_gpu);
	
	cudaError_t cudaError;
	cudaError = cudaGetLastError();
	if(cudaError != cudaSuccess) {
		cerr << "Warning: one or more CUDA errors occurred. Try using cuda-gdb to debug. Error message: \n\t" <<cudaGetErrorString(cudaError) << "\n";
	}
}

void strongNeighbor_wrapper(GraphData graph, int *& strongNeighbor, int numthreads)
{
	fprintf(stderr, "Start Searching For Strongest Neighbor ... \n");

    struct timeval beginTime, endTime;

    setTime(&beginTime);

	findStrongNeighbor(graph, strongNeighbor, numthreads);

    setTime(&endTime);

    fprintf(stderr, "Done searching.\n");

    fprintf(stderr, "Strong Neighbor Search Time: %.2f ms\n",
            getTime(&beginTime, &endTime));
}
