/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include <stdio.h>
#include <stdlib.h>

#ifndef gpu_headers_h
#define gpu_headers_h

/**
 * Performs segment scan to find strongest neighbor for each src node
 * @param src The source array in the edge list
 * @param oldDst The current dst array in the edge list
 * @param newDst The modified dst array produced by this GPU kernel function
 * @param oldWeight The current weight array in the edge list
 * @param newWeight The modified weight array produced by this GPU kernel function
 * @param madeChanges If our output is different than our input then we must set *madeChanges to 1, so the host will know to launch another step of the scan.
 * @param distance The distance between array locations being examined. This is always a power of 2.
 * @param numEdges The size of the index, weight, and flags arrays.
*/
__global__ void strongestNeighborScan_gpu(int * src, int * oldDst, int * newDst, int * oldWeight, int * newWeight, int * madeChanges, int distance, int numEdges);

/**
 * Collates results of segment scan (or segment prefix sum), putting last value from each segment into an array
 * Note that behavior for empty segments is undefined. E.g. if there's no segment with source node 2, then output[2] might contain garbage data.
 * @param src The segment ID for each edge in the scan result
 * @param scanResult The scan result or prefix sum result that we're collating
 * @param output The output
 * @param numEdges The size of the src and scanResult arrays.
*/
__global__ void collateSegments_gpu(int * src, int * scanResult, int * output, int numEdges);

/**
 * Updates matches based on strongNeighbor
 * @param strongNeighbor The strongest neighbor for every segment, or -1 for empty segments.
 * @param matches The matches (so far). Has been initialized with -1 values for each unmatched node.
 * @param numNodes The size of the strongNeighbor and res arrays.
 */
__global__ void check_handshaking_gpu(int * strongNeighbor, int * matches, int numNodes);

/**
 * Marks whether to keep or filter each edge: 1 to keep, 0 to filter.
 * @param src The source array for the edge list
 * @param dst The destination array for the edge list
 * @param matches The matches we've found so far (with -1 for unmatched nodes)
 * @param keepEdges The output of this GPU kernel function
 * @param numEdges The size of the src, dst, and keepEdges arrays.
 */
__global__ void markFilterEdges_gpu(int * src, int * dst, int * matches, int * keepEdges, int numEdges);

/**
 * Performs one step of an exclusive prefix sum. This version is NOT segmented.
 * To implement exclusivity, when distance == 0 it should copy the value from distance of 1.
 * @param oldSum The prefix sum (so far) from the previous step
 * @param newSum The output of this GPU kernel function
 * @param distance The distance between elements being added together in this step, or 0 to shift right
 * @param numElements The size of each array
 */
__global__ void exclusive_prefix_sum_gpu(int * oldSum, int * newSum, int distance, int numElements);

/**
 * Repacks the edge list (i.e. the source, destination, and weight arrays), thereby filtering out some edges
 * @param newSrc The new source array produced by this GPU kernel function
 * @param oldSrc The old source array
 * @param newDst The new destination array produced by this GPU kernel function
 * @param oldDst The old destination array
 * @param newWeight The new weight array produced by this GPU kernel function
 * @param oldWeight The old weight array
 * @param edgeMap List of new indices for the old edges
 * @param numEdges The size of the oldSrc, oldDst, oldWeight, and edgeMap arrays.
 */
__global__ void packGraph_gpu(int * newSrc, int * oldSrc, int * newDst, int * oldDst, int * newWeight, int * oldWeight, int * edgeMap, int numEdges);

#endif