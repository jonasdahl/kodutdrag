#ifndef TSP_H
#define TSP_H

#include <vector>
#include <list>
#include <string>
using namespace std;

/**
 * Struct for storing a point.
 * Has pointers to next and previous in path, besides coordinates.
 */
struct Point {
    Point(double a, double b, int n) : x(a), y(b), index(n), next(n+1), prev(n-1) { };
    double x;
    double y;
    int index;
    int next;
    int prev;
};

/**
 * Struct for storing edges between points (their index).
 */
struct Edge {
    int a;
    int b;
    double dist;
    Edge(int a_, int b_, int dist_) : a(a_), b(b_), dist(dist_) {}

    /**
     * Generates a string representation of this object.
     * @return a string representation of the edge (a, b, (dist))
     */
    string to_string() const {
        return "(" + std::to_string(a) + ", " + std::to_string(b) + " (" + std::to_string(dist) + "))";
    }
};

/**
 * The main TSP class.
 */
class TSP {
    private:
        // All points that exists
        vector<Point> points;

        // Stores all neighbours to point i in [i]
        vector<vector<int>> neighbours;

        // Number of points
        int n;

        // Longest distance
        int longest_distance = 0;

    public:
        // Creates a new TSP problem
        TSP() : n(0) {};

        // Adds a point to the world we know
        void add_point(double, double);

        // Computes the neighbour list
        // Must be called before calling shortest edge
        void compute_neighbour_list(int m);

        // Various prining functions
        void print_result() const;
        
        // Calculates total distance for whole path
        int total_dist() const;

        // Calculates total distance for the given list
        int total_dist(const vector<Point>& a) const;

        // Calculates total disance between two indices in point list
        int dist(int, int) const;
        
        // Calculates costs for swapping a to b
        int swap_cost(const int a, const int b, const vector<Point>& list) const;
        int two_opt_swap_cost(const int a, const int b, const vector<Point>& list) const;

        // Performs swaps between a and b in given lists
        void swap(int a, int b, vector<Point>& list);
        void two_opt_swap(int i, int j, vector<Point>& list);
        
        // Returns true if edge will make list cyclic
        int is_cyclic(list<Edge> &, Edge) const;

        // Our tour-finding algorithms
        void exec_shortest_edge();
        void exec_naive();

        // Our improving algorithms
        void improve_simulated_annealing(int k_max);
        void improve_v2_simulated_annealing(int k_max, int m);
        int  improve_two_opt(int k_max);
        void improve_two_opt_neighbour(int k_max, int m);

        // Heals the list
        void heal_list() {
            points[points.size() - 1].next = 0;
            points[0].prev = n-1;
        };
};
#endif