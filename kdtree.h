/**
 * @file kdtree.h
 * KDTree implementation using Points in k-dimensional space.
 *
 * @author Zesheng Wang
 * @author Wade Fagen-Ulmschneider
 * @author Cinda Heeren
 * @author Jack Toole
 * @author Sean Massung
 */

#pragma once

#include <vector>
#include <iterator>

#include "util/coloredout.h"

#include "cs225/point.h"

using std::vector;
using std::string;
using std::ostream;
using std::cout;
using std::endl;

template <int Dim>
class KDTree
{
  private:
    struct KDTreeNode
    {
      Point<Dim> point;
      KDTreeNode *left, *right;

      KDTreeNode() : point(), left(NULL), right(NULL) {}
      KDTreeNode(const Point<Dim> &point) : point(point), left(NULL), right(NULL) {}
    };

  public:
    KDTree(const vector<Point<Dim>>& newPoints);
    KDTreeNode* buildTree(vector<Point<Dim>> & newPoints,int left_node,int right_node,int currDim);
		int QuickSelect(vector<Point<Dim>> & newPoints,int left,int right,int size ,int currDim);
    void findNearestNeighborHelper(
    const KDTreeNode*root, const Point<Dim>& target, Point<Dim>&r_point, int depth) const;


    KDTree(const KDTree<Dim>& other);
    KDTree const &operator=(const KDTree<Dim>& rhs);
   
    KDTreeNode* copyTree(const KDTreeNode* node);
    void clearTree(KDTreeNode* node);
    ~KDTree();
    Point<Dim> findNearestNeighbor(const Point<Dim>& query) const;
 
    double calculateDistance(const Point<Dim> &a, const Point<Dim> &b) const;
    void printTree(ostream& out = cout,
                   colored_out::enable_t enable_bold = colored_out::COUT,
                   int modWidth = -1) const;

  private:

    /** Internal representation, root and size **/
    KDTreeNode *root;
    size_t size;

    /** Helper function for grading */
    int getPrintData(KDTreeNode * subroot) const;

    /** Helper function for grading */
    void printTree(KDTreeNode * subroot, std::vector<std::string>& output,
                   int left, int top, int width, int currd) const;

    /**
     * @todo Add your helper functions here.
     */
};

/**
  * Determines if a Point is closer to the target Point than another
  * reference Point. Takes three points: target, currentBest, and
  * potential, and returns whether or not potential is closer to
  * target than currentBest.
  *
  * We are using Euclidean distance to compare k-dimensional
  * points: \f$\sqrt{(p_1-q_1)^2+(p_2-q_2)^2+\ldots+(p_n-q_n)^2} =
  * \sqrt{\sum_{i=1}^n (p_i-q_i)^2}\f$. Note, however, that
  * **minimizing distance is the same as minimizing squared
  * distance**, so you can **avoid invoking the square root** and
  * just compare squared distances in your code.
  *
  * For example:
  *
  *     Point<3> target(1, 3, 5);
  *     Point<3> currentBest1(1, 3, 2);
  *     Point<3> possibleBest1(2, 4, 4);
  *     Point<3> currentBest2(1, 3, 6);
  *     Point<3> possibleBest2(2, 4, 4);
  *     Point<3> currentBest3(0, 2, 4);
  *     Point<3> possibleBest3(2, 4, 6);
  *     cout << shouldReplace(target, currentBest1, possibleBest1) << endl; // true
  *     cout << shouldReplace(target, currentBest2, possibleBest2) << endl; // false
  *     cout << shouldReplace(target, currentBest3, possibleBest3) << endl;
  *      // based on operator<, this should be false
  *
  * @todo This function is required for Part 1.
  * @param target The Point we want to be close to.
  * @param currentBest The Point that is currently our closest Point
  *    to target.
  * @param potential Our Point that is a candidate to replace
  *    currentBest (that is, it may be closer to target than
  *    currentBest).
  * @return A boolean value indicating whether potential is closer
  *  to target than currentBest. Ties should be broken with
  *  Point::operator<().
  */
template <int Dim>
bool shouldReplace(const Point<Dim>& target, const Point<Dim>& currentBest,
                    const Point<Dim>& potential);


/**
  * Determines if Point a is smaller than Point b in a given dimension d.
  * If there is a tie, break it with Point::operator<().
  *
  * For example:
  *
  *     Point<3> a(1, 2, 3);
  *     Point<3> b(3, 2, 1);
  *     cout << smallerDimVal(a, b, 0) << endl; // should print true
  *     cout << smallerDimVal(a, b, 2) << endl; // should print false
  *     cout << smallerDimVal(a, b, 1) << endl; // based on operator<, this should be true
  *
  * @todo This function is required for Part 1.
  * @param first Point to compare.
  * @param second Second point to compare.
  * @param curDim Dimension these points are being compared in.
  * @return A boolean value indicating whether the first Point is smaller
  *  than the second Point in the curDim dimension.
  */
  
template <int Dim>
bool smallerDimVal(const Point<Dim>& first, const Point<Dim>& second,
                    int curDim);



/**
  * This function uses the quickselect algorithm to partition the given
  * range such that the k-th element is in the k-th position and all
  * elements that compare as less by the provided function are to the
  * left and all larger elements are to the right. Note this does not
  * sort the range and runs in expected O(n) time.
  *
  * Reference (https://en.wikipedia.org/wiki/Quickselect)
  *
  * @param begin iterator to the start of the range inclusive
  * @param end  iterator to one past the end of the range
  * @param k iterator to the location in the range to find
  * @param cmp compare function true if arg1 is less than arg2
  */
template <typename RandIter, typename Comparator>
void select(RandIter begin, RandIter end, RandIter k, Comparator cmp);


#include "kdtree.hpp"
#include "kdtree_extras.hpp"
