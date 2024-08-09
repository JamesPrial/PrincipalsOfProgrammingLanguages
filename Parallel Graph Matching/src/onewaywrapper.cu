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

int one_way_handshake(GraphData graph, int *& matches, int numthreads)
{
	int num_thread_blocks = (numthreads + threadsPerBlock - 1) / threadsPerBlock;
	
	int numVertices = graph.numNodes;
	int numEdges = graph.numEdges;
	
	//Prepare various GPU arrays that we're going to need:
	
	int * strongNeighbor_gpu;//will hold strongest neighbor for each vertex
	cudaMalloc((void **)&strongNeighbor_gpu, numVertices * sizeof(int));
	cudaMemcpy(strongNeighbor_gpu, matches, numVertices * sizeof(int), cudaMemcpyHostToDevice);//init to all -1
	
	int * matches_gpu;//will hold the output
	cudaMalloc((void **)&matches_gpu, numVertices * sizeof(int));
	cudaMemcpy(matches_gpu, matches, numVertices * sizeof(int), cudaMemcpyHostToDevice);
	
	int * src_gpu;//holds the src nodes in edge list
	int * dst_gpu;//holds the dst nodes in edge list
	int * weight_gpu;//holds the edge weights in edge list
	cudaMalloc((void **)&src_gpu, (1+numEdges) * sizeof(int));
	cudaMalloc((void **)&dst_gpu, (1+numEdges) * sizeof(int));
	cudaMalloc((void **)&weight_gpu, (1+numEdges) * sizeof(int));
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
	
    /* Start matching */
    int iter;
    for (iter = 0; ; iter++) {
		
		//Step 1: Get strongest neighbor for each vertex/node
		
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
		
		//reminder: expected first iteration strongNeighbor_gpu: 3 4 5 4 1 2 3 4 7
		
        //Step 2: Each vertex checks if there is a handshaking
        check_handshaking_gpu<<<num_thread_blocks, threadsPerBlock>>>(strongNeighbor_gpu, matches_gpu, numVertices);
		
		//Step 3: filter
		
		//Step 3a: decide which edges to keep (marked with a 1) and filter (marked with a 0)
		int * keepEdges_gpu = temp1_gpu;
		temp1_gpu = NULL;
		markFilterEdges_gpu<<<num_thread_blocks, threadsPerBlock>>>(src_gpu, dst_gpu, matches_gpu, keepEdges_gpu, numEdges);
		
		
		//Step 3b: get new indices in edge list for the edges we're going to keep
		int * newEdgeLocs_gpu = keepEdges_gpu;
		keepEdges_gpu = NULL;
		for(int distance = 0; distance <= numEdges; distance = max(1, distance * 2)) {
			exclusive_prefix_sum_gpu<<<num_thread_blocks, threadsPerBlock>>>(newEdgeLocs_gpu, temp2_gpu, distance, numEdges+1);
			swapArray((void**) &newEdgeLocs_gpu, (void**) &temp2_gpu);
		}
		
		//note: temp1 is still in use, until we're done with newEdgeLocs_gpu
		
		//Step 3c: check if we're done matching
		int lastLoc = 0;
		cudaMemcpy(&lastLoc, &(newEdgeLocs_gpu[numEdges]), sizeof(int), cudaMemcpyDeviceToHost);
		if(lastLoc < 2) {
			//termination: fewer than two nodes remain unmatched
			break;
		} else if(lastLoc == numEdges) {
			//termination: no additional matches are possible
			break;
		}
		
		//Step 3d: pack the src, dst, and weight arrays in accordance with new edge locations
		packGraph_gpu<<<num_thread_blocks, threadsPerBlock>>>(temp2_gpu, src_gpu, temp3_gpu, dst_gpu, temp4_gpu, weight_gpu, newEdgeLocs_gpu, numEdges);
		swapArray((void**) &temp2_gpu, (void**) &src_gpu);
		swapArray((void**) &temp3_gpu, (void**) &dst_gpu);
		swapArray((void**) &temp4_gpu, (void**) &weight_gpu);
		
		temp1_gpu = newEdgeLocs_gpu;
		newEdgeLocs_gpu = NULL;
		
		//note: we're done with the current contents of all the temporary arrays
		
		//Set new number of edges:
		numEdges = lastLoc;
		
		if(iter > numVertices) {
			cerr << "Error: matching has been running too long; breaking loop now\n";
			break;
		}
    }
	
	cudaMemcpy(matches, matches_gpu, numVertices * sizeof(int), cudaMemcpyDeviceToHost);
	
	//Wait until pending GPU operations are complete:
	cudaDeviceSynchronize();
	
	//free GPU arrays
	cudaFree(strongNeighbor_gpu);
	cudaFree(matches_gpu);
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
	
	return iter + 1;
}

void one_way_handshake_wrapper(GraphData graph, int *& matches, int numthreads)
{
	fprintf(stderr, "Start One Way Matching ... \n");

    struct timeval beginTime, endTime;

    setTime(&beginTime);

	int iter = one_way_handshake(graph, matches, numthreads);

    setTime(&endTime);

    fprintf(stderr, "Done matching.\n");

    fprintf(stderr, "Performed matching for %ld iterations\n", iter);
    fprintf(stderr, "One Way Handshaking Matching Time: %.2f ms\n",
            getTime(&beginTime, &endTime));
}
