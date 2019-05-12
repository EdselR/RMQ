#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "timerc.h"

/**
Prof. Bento
04/02/19
Programmer: Edsel Rudy
Course: CSCI 3396

Description: This program aims to parallelize the Range Minimum Query algorithm

**/

__global__ void warmup(){

}

//function to get right child
int get_right(int index, int NUM_NODES){
	
	if(index != -1 && ((2*index) + 1)<= NUM_NODES){
		return (2*index) + 1;
	}

	return -1;
}

	//function to get left child
int get_left(int index, int NUM_NODES){
	
	if(index != -1 && ((2*index)<= NUM_NODES)){
		return (2*index);
	}

	return -1;
}

//parallelize the merge list
void merge_list(int* dest_list, int dest_list_length, int* array1, int array1_length, int* array2){

	/*
	//for debugging
	printf("Copying: ");
	for(int i = 1; i <= array1_length;i++){
		printf("%d,", array1[i]);
	}

	printf(" and ");

	for(int i = 1; i <= array2_length;i++){
		printf("%d,", array2[i]);
	}

	printf(": \n");
	*/
	
	int x = 1;
	for(int i = 1; i <= array1_length;i++){
		dest_list[x] = array1[i];
		x++; 
	}

	x = 1;
	for(int i = array1_length+1; i <= dest_list_length;i++){

		int last_elem = array1[array1_length];

		if(last_elem <= array2[x]){
			dest_list[i] = last_elem;
		}else{
			dest_list[i] = array2[x];
		}

		x++;
	}

	/*
	//for debugging
	for(int i = 1; i <= dest_list_length;i++){
		printf("%d,", dest_list[i]);
	}
	printf(": \n");
	*/
}

//returns the tree array offset for a specific level and node
int chooseNode(int numNodes,int level, int node){

	double height = ceil(log2(numNodes));

	int offset = (numNodes/pow(2,height-level)) + node;

	return offset;
}

//num of children for each node
int numOfChildren(int node, int numInputs){

	int level = floor(log2(node));
	int numChildren = numInputs/ pow(2,level);

	//printf("Node: %d Num Children: %d\n", node, numChildren);

	return numChildren;
}

void print_list(int** list, int numInputs){

	int numNodes = 2 * numInputs - 1;	


	for(int i = 1; i <= numNodes; i++){
		
		printf("Index %d: ", i);

		for(int x = 1; x <= (numOfChildren(i,numInputs)); x++){

			printf("%d, ", list[i][x]);
		}

		printf("\n");
	}
}

void fillInLists(int* inputArray, int numNodes, int numInputs, int** prefixTree, int** suffixTree){

	double height = ceil(log2(numNodes));

	//printf("\nTesting fill leaves\n");


	for(int i = 1; i <= pow(2,height -1); i++){
		int index = chooseNode(numNodes,height-1,i);

		int * prefixList = prefixTree[index];
		prefixList[1] = inputArray[i-1];

		int* suffixList = suffixTree[index];
		suffixList[1] = inputArray[i-1];
	}

	for(int i = height-2; i>=0; i--){
		for(int x = 1; x <= pow(2,i);x++){

			int index = chooseNode(numNodes,i,x);
			//int *prefixList = prefixTree[index];
			//int *suffixList = suffixTree[index];

			int left_index = get_left(index, numNodes);
			int right_index = get_right(index, numNodes);

			float merge_list_time = 0;
			
			//do this so that the timer only counts once
			if((i == 0)){
				cstart();
			}

			merge_list(prefixTree[index], numOfChildren(index,numInputs), prefixTree[left_index], 
				numOfChildren(left_index,numInputs),prefixTree[right_index]);

			if((i == 0)){
				cend(&merge_list_time);
				printf("Merge List Time: %f\n",merge_list_time);
			}

			merge_list(suffixTree[index], numOfChildren(index,numInputs), suffixTree[right_index],
			 numOfChildren(right_index,numInputs), suffixTree[left_index]);
		}
	}
}

int LevelOrder_LCA(int num1, int num2){

	int binaryLength = log2(num1);
	int pos = binaryLength;
	int mask = 1 << binaryLength;

	//printf("\nMask: %d, Binary Length: %d\n", mask, binaryLength);
	
	int num1Offset = mask & num1;
	int num2Offset = mask & num2;
	
	while((num2Offset ^ num1Offset) == 0){
		// printf("Index %d: same, Mask: %d,Num 1 Off: %d, Num 2 Off: %d\n"
		// 	,pos, mask, num1Offset, num2Offset);

		pos--;
		mask = mask >> 1;

		num1Offset = mask & num1;
		num2Offset = mask & num2;
	}
	pos++;



	//printf("Pos of change: %d\n Calculating LCA: \n", pos);
	int result = 0;
	int power = 0;
	mask = 1 << pos;

	for(int i = pos; i <= binaryLength;i++){
		
		if((mask & num1) != 0){
			//printf("incrementing result\n");
			result += pow(2,power);
		}

		// printf("Index: %d, Mask: %d,Power: %d, Pos and Num: %d,Result: %d\n"
		// 	,i, mask, power,(mask & num1),result);

		mask = mask << 1;
		power++;
	}
	
	return result;
}

//do this one
int shiftBits(int num, int offset){

	if(num == 0){
		return 1;
	}

	int binaryLength = log2(num);
	int length = binaryLength - offset;

	int mask = 1;
	int result = 0;

	//printf("Length: %d, Offset: %d", binaryLength,length);

	for(int i = 0; i <= length;i++){

		if((mask & num) != 0){
			result += pow(2,i);
		}

		mask = mask << 1;
	}

	//printf("\nAfter Bit Shift: %d by %d bits\n", result,offset);

	result++;

	return result;
}

int RMQ(int index1, int index2, int numInputs,int** prefixTree, int** suffixTree){

	int minQuery = 0;
	int numNodes = 2* numInputs - 1;
	double height = ceil(log2(numNodes));
	

	//if index1 and 2 is right next to each other, only compare the to numbers
	if(abs(index2 - index1) == 1){
		
		int num1 = prefixTree[chooseNode(numNodes,height-1,index1 + 1)][1];
		int num2 = prefixTree[chooseNode(numNodes,height-1,index2 + 1)][1]; 
		
		if(num1 <= num2){

			return num1;
		}else{

			return num2;
		}
	}

	if(index1 == index2){
		return prefixTree[chooseNode(numNodes,height-1,index1 + 1)][1];
	}

	
	int leftIndex = chooseNode(numNodes, height-1, index1 + 1);
	int rightIndex = chooseNode(numNodes, height-1, index2 + 1);

	int LCA_index = LevelOrder_LCA(leftIndex,rightIndex);
	int leftChild = get_left(LCA_index,numNodes);
	int rightChild = get_right(LCA_index,numNodes);


	//printf("leftIndex: %d, rightIndex: %d, LCA_index: %d\n",leftIndex,rightIndex, LCA_index);
	
	/**
	printf("Prefix List: \n");
	print_list(prefixTree, numInputs);

	printf("\nSuffix List: \n");
	print_list(suffixTree, numInputs);
	**/

	//change to shift bits
	int level_left = floor(log2(leftChild));
	int index1_suffix = shiftBits(index1,level_left);

	//printf("Left Child Index: %d ",index1_suffix);

	int level_right = floor(log2(rightChild));
	int index2_prefix = shiftBits(index2,level_right);

	//printf("Right Child Index: %d\n", index2_prefix);

	int minSuffix = suffixTree[leftChild][index1_suffix];
	int minPrefix = prefixTree[rightChild][index2_prefix];

	printf("Min Suffix: %d from Node %d, Min Prefix: %d from Node %d\n"
		,minSuffix,leftChild,minPrefix,rightChild);

	if(minPrefix <= minSuffix){
		minQuery = minPrefix;
	}
	else{
		minQuery = minSuffix;
	}

	return minQuery;
}

int main(){

	/*
		Range Minima Query:
									 h:
					   1              - 0
				  /        \
				 2          3         - 1
			   /  \       /   \
		      4    5     6     7	  - 2
			 / \  / \   / \   / \
			8  9 10 11 12 13 14 15    - 3

		input array:
		5,10,3,4,7,1,8,2

		All arrays will be indexed at 1
	*/

	/**
	//Test RMQ query correctness
	int input[] = {5,10,4,3,7,1,8,2};
	
	int numInputs = sizeof(input)/sizeof(int);
	
	int numNodes = 2 * numInputs - 1;	
	
	
	printf("\nInput Array: ");
	for(int i = 0; i < numInputs;i++){
		printf("%d, ",input[i] );
	}
	printf("\n");
	**/

	//test log of input sizes for report
	int numInputs = 1<<14;
	int input[numInputs];

	for(int i = 0; i < numInputs; i++){
		input[i] = i;
		
		if(i == numInputs/2){
			input[i] = 1;
		}
	}
	int numNodes = 2 * numInputs - 1;

	warmup<<<1,1>>>();

	float build_list_time;
	cstart();

	//RMQ Preprocessing
	int** prefixTree = (int**)malloc((numNodes+1) * sizeof(int*));

	for(int i = 1; i <= numNodes;i++){
		prefixTree[i] = (int*) malloc((numOfChildren(i,numInputs)+1) * sizeof(int*));
	}

	int** suffixTree = (int**)malloc((numNodes+1) * sizeof(int*));

	for(int i = 1; i <= numNodes;i++){
		suffixTree[i] = (int*) malloc((numOfChildren(i,numInputs)+1) * sizeof(int*));
	}

	cend(&build_list_time);
	printf("Build List Time: %f\n",build_list_time);

	float fill_list_time;
	cstart();
	fillInLists(input,numNodes, numInputs, prefixTree, suffixTree);
	cend(&fill_list_time);
	
	printf("Fill List Time: %f\n",fill_list_time);


	//test RMQ query 
	int index1 = numInputs/4;
	int index2 = numInputs-2;
	printf("\nMin from index: %d to %d\n", index1,index2);

	float RMQ_query_time;

	cstart();
	int RangeMinima = RMQ(index1,index2,numInputs,prefixTree,suffixTree);
	cend(&RMQ_query_time);

	printf("Min Query: %d\n" ,RangeMinima);

	printf("RMQ Query Time: %f\n",RMQ_query_time);
	/**
	//test RMQ query correctness
	index1 = 0;
	index2 = 2;

	printf("\nMin from %d to %d\n", index1,index2);
	RangeMinima = RMQ(index1,index2,numInputs,prefixTree,suffixTree);
	printf("Min Query: %d\n" ,RangeMinima);

	index1 = 0;
	index2 = 4;

	printf("\nMin from %d to %d\n", index1,index2);
	RangeMinima = RMQ(index1,index2,numInputs,prefixTree,suffixTree);
	printf("Min Query: %d\n" ,RangeMinima);

	index1 = 4;
	index2 = 6;

	printf("\nMin from %d to %d\n", index1,index2);
	RangeMinima = RMQ(index1,index2,numInputs,prefixTree,suffixTree);
	printf("Min Query: %d\n" ,RangeMinima);
	**/
	
	return 0;
}