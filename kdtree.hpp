/**
 * @file kdtree.cpp
 * Implementation of KDTree class.
 */

#include <utility>
#include <algorithm>
#include <deque>
#include <cfloat>
using namespace std;

template <int Dim>
bool smallerDimVal(const Point<Dim>& first,
                                const Point<Dim>& second, int curDim)
{
   if (first[curDim] < second[curDim]) {
        return true;
    } else if (first[curDim] == second[curDim]) {
        return first < second;
    } else {
        return false;
    }
}

template <int Dim>
bool shouldReplace(const Point<Dim>& target,
const Point<Dim>& currentBest,
                                const Point<Dim>& potential)
{
    double distCurrentBest = 0.0;
    for (int i = 0; i < Dim; i++) {
        double diff = target[i] - currentBest[i];
        distCurrentBest += diff * diff;
    }
    double distPotential = 0.0;
    for (int i = 0; i < Dim; i++) {
        double diff = target[i] - potential[i];
        distPotential += diff * diff;
    }

    if (distPotential < distCurrentBest || (distPotential == distCurrentBest && potential < currentBest)) {
        return true;  // potential is closer or equal and smaller
    } else {
        return false;  // currentBest is closer 
    }
}


template <int Dim>
KDTree<Dim>::KDTree(const KDTree<Dim>& other) {
  root = copyTree(other.root);
}

template <int Dim>
const KDTree<Dim>& KDTree<Dim>::operator=(const KDTree<Dim>& rhs) {
  if (this != &rhs) {
        clearTree(root);
        root = copyTree(rhs.root);
    }
    return *this;
}

template <int Dim>
KDTree<Dim>::~KDTree() {
  clearTree(root);
}
template <int Dim>
void KDTree<Dim>::clearTree(KDTreeNode* node) {
    if (node == nullptr) {
        return;
    }

    clearTree(node->left);
    clearTree(node->right);
    delete node;
}
template <int Dim>
typename KDTree<Dim>::KDTreeNode* KDTree<Dim>::copyTree(const KDTreeNode* node) {
    if (node == nullptr) {
        return nullptr;
    }
    KDTreeNode* newNode = new KDTreeNode(node->point);
    newNode->left = copyTree(node->left);
    newNode->right = copyTree(node->right);

    return newNode;
}
template <int Dim>
KDTree<Dim>::KDTree(const vector<Point<Dim>>& newPoints)
{
		if(newPoints.size() == 0){
				root=nullptr; 
		} 


		vector<Point<Dim>> vec;

        
		for(int i=0;i<static_cast<int>(newPoints.size());i++){
				vec.push_back(newPoints[i]);
		}
		root=buildTree(vec,0,newPoints.size()-1,0);

}


template<int Dim>
typename KDTree<Dim>::KDTreeNode* KDTree<Dim>::buildTree(vector<Point<Dim>> &  newPoints,int left,int right,int currDim){
		
		if(right<left){
			return nullptr;
		}
		int size  = 0; 
		if((right-left+1)%2==0){	
			size =(int) (right-left+1)/2;
		} 
        else{
			size=(int) ((right-left+1)/2)+1;
		}
		int mid = QuickSelect(newPoints,left,right,size,currDim);


		KDTreeNode* bestNode= new KDTreeNode(newPoints[mid]);
		bestNode->left=buildTree(newPoints,left,mid-1,(currDim+1)%Dim);
		bestNode->right=buildTree(newPoints,mid+1,right,(currDim+1)%Dim);
		return bestNode;

}


template <int Dim>
Point<Dim> KDTree<Dim>::findNearestNeighbor(const Point<Dim>& query) const {
    if(root == nullptr ){
        return Point<Dim>(); 
    }
    Point<Dim> bestNode = root->point; 
    findNearestNeighborHelper(root,query,bestNode,0);
    return bestNode;
}
template <int Dim>
void KDTree<Dim>::findNearestNeighborHelper(
    const KDTreeNode*root, const Point<Dim>& target, Point<Dim>& r_point, int depth) const {

    if (root == nullptr) {
        return; 
    }
    KDTreeNode* nextBranch;
    KDTreeNode* otherBranch;

    if(smallerDimVal(target,root->point,(depth%Dim))){
        nextBranch = root->left;
        otherBranch = root->right;
    } 
    else {
        nextBranch = root->right;
        otherBranch = root->left;
    }
    
    if(shouldReplace(target,r_point, root->point)){
        r_point = root->point; 
    }
    
    findNearestNeighborHelper(nextBranch, target, r_point, depth+1); 
    double radiusSquared = calculateDistance(target,r_point);
    double dist = (target[depth%Dim] - root->point[depth%Dim]); 

    if (radiusSquared >= dist * dist) {
        findNearestNeighborHelper(otherBranch,target,r_point, depth+1); 
    }
}

template <int Dim>
double KDTree<Dim>::calculateDistance(const Point<Dim> &a, const Point<Dim> &b) const {
    double distance = 0.0;
    for (int i = 0; i < Dim; i++) {
        distance += (a[i] - b[i]) * (a[i] - b[i]);
    }
    return distance;
}

template <typename RandIter, typename Comparator>
void select(RandIter start, RandIter end, RandIter k, Comparator cmp)
{
    if (start >= end)
        return;

    while (start < end) {
        RandIter pivot = start;
        RandIter left = start;
        RandIter right = end - 1;

        while (left <= right) {
            while (left < end && cmp(*left, *pivot))
                left++;
            while (right >= start && !cmp(*right, *pivot))
                right--;

            if (left <= right) {
                std::swap(*left, *right);
                left++;
                right--;
            }
        }
        std::swap(*pivot, *right);
        if (k == right)
            return;
        else if (k < right)
            end = right;
        else
            start = right + 1;
    }
}
template <int Dim>
int KDTree<Dim>::QuickSelect(vector<Point<Dim>>& newPoints, int left, int right, int size , int currDim) {
    if (left == right) {
        return left;
    }
    int a = left - 1;
    Point<Dim> pivot_point = newPoints[right];
    for (int i = left; i < right; i++) {
        if (smallerDimVal(newPoints[i], pivot_point, currDim)) {
            a++;
            std::swap(newPoints[a], newPoints[i]);
        }
    }
    std::swap(newPoints[a + 1],newPoints[right]);
    int pivot_index = a + 1;
    if (pivot_index -  left == size - 1) {
        return pivot_index;
    } 
    else if (pivot_index - left > size - 1) {
        return QuickSelect(newPoints, left, pivot_index - 1, size, currDim);
    } 
    else {
        return QuickSelect(newPoints, pivot_index + 1, right, size - (pivot_index - left) - 1, currDim);
    }
}
