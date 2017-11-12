#include "tsp.h"
#include <math.h>
#include <iostream>
#include <cstdlib> // random
#include <time.h>
#include <list>
#include <algorithm>
using namespace std;

/**
 * Swaps i and j in given vector
 * @param i    index i
 * @param j    index j
 * @param list the list to swap in
 */
void TSP::swap(int i, int j, vector<Point>& list) {
    // There are three cases (two special cases), prepare for all
    if (i == list[j].next) {
        int tmp = i;
        i = j;
        j = tmp;
    }
    if (j == list[i].next) {
        int jp1 = list[j].next;

        // Set i-1
        list[list[i].prev].next = j;

        // Set j
        list[j].prev = list[i].prev;
        list[j].next = i;

        // Set i
        list[i].prev = j;
        list[i].next = jp1;
        
        // Set j+1
        list[jp1].prev = i;
    } else {
        int iprev = list[i].prev;
        int inext = list[i].next;

        // i = j
        list[i].next = list[j].next;
        list[i].prev = list[j].prev;
        list[list[j].prev].next = i;
        list[list[j].next].prev = i;

        // j == ipn
        list[j].next = inext;
        list[j].prev = iprev;
        list[iprev].next = j;
        list[inext].prev = j;
    }
}

/**
 * Swaps i and j in given vector. Simply reverses the path between i and j.
 * @param i    index i from points, starting at 0
 * @param j    index j from points, starting at 0
 * @param list the list to swap in
 */
 void TSP::two_opt_swap(int i, int j, vector<Point>& list) {
    int jNext = list[j].next;

    // Relink indices
    list[list[i].prev].next = j;
    list[j].next = list[j].prev;
    list[j].prev = list[i].prev;

    int t = list[j].next;
    int oldT = j;

    // Reverse the middle part
    while(t != i) {
        int tmp = list[t].next;
        list[t].next = list[t].prev;
        list[t].prev = tmp;
        oldT = t;
        t = list[t].next;
    }
    
    // Heal the list
    list[i].prev = oldT;
    list[i].next = jNext;
    list[jNext].prev = i;

}

/**
 * Calculates swapping cost between a and b.
 * @param  a    the first index
 * @param  b    the first index
 * @param  list the list to swap in
 * @return      the cost
 */
 int TSP::swap_cost(const int a, const int b, const vector<Point>& list) const {
    const Point & ap = list[a];
    const Point & bp = list[b];

    int am1 = list[ap.prev].index;
    int ap1 = list[ap.next].index;
    int bm1 = list[bp.prev].index;
    int bp1 = list[bp.next].index;
    int at = ap.index;
    int bt = bp.index;

    // Calculate for all different scenarios
    if (at == bp1) {
        int before = dist(ap1, at) + dist(bt, bm1);
        int after = dist(ap1, bt) + dist(at, bm1);

        return after - before;
    } else if (at == bm1) {
        int before = dist(am1, at) + dist(bt, bp1);
        int after = dist(am1, bt) + dist(at, bp1);

        return after - before;
    } else {
        int before = dist(at, am1) + dist(at, ap1) +
        dist(bt, bm1) + dist(bt, bp1);
        int after = dist(bt, am1) + dist(bt, ap1) +
        dist(at, bm1) + dist(at, bp1);
        
        return after - before;
    }
}

/**
 * Calculate the cost of swapping with two opt.
 * @param  a    the first index
 * @param  b    the first index
 * @param  list the list to swap in
 * @return      the cost
 */
int TSP::two_opt_swap_cost(const int a, const int b, const vector<Point>& list) const {
    int cost = 0;

    cost -= dist(a, list[a].prev);
    cost -= dist(b, list[b].next);
    cost += dist(b, list[a].prev);
    cost += dist(a, list[b].next);
    
    return cost;
}

/**
 * Adds a point to our list of points to visit later when running the algorithm.
 * @param x the x coordinate    
 * @param y the y coordinate
 */
 void TSP::add_point(double x, double y) {
    Point p(x, y, n);
    points.push_back(p);
    n++;
}

/**
 * Populates the neighbour list with neighbours to the entries.
 * Each neighbour list consists of maximum m entries.
 * 
 * @param m maximum number of entries
 */
 void TSP::compute_neighbour_list(int m) {
    neighbours.reserve(n);
    vector<Edge> edges;

    // Create all possible edges (O(n^2)) in the graph and add them to the edge
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < i; j++) {
            if (i == j) continue;
            Edge e(i, j, dist(i, j));
            edges.push_back(e);
        }     
    }

    // Sort the list, shortest edges first!
    sort(edges.begin(), edges.end(), [](const Edge & a, const Edge & b) {
        return a.dist < b.dist;
    });

    longest_distance = edges.back().dist; // used by SA

    // Reserve some space for neighbours
    neighbours.reserve(n);

    // Loop through each neighbour list and add the vector
    for(int i = 0; i < n; i++) {
        neighbours.push_back(vector<int>()); 
    }

    // Now, loop through the sorted list of edges and insert the neighbors to the list
    for (auto & edge : edges) {
        if (neighbours[edge.a].size() < m) {
            neighbours[edge.a].push_back(edge.b);
        }
        if (neighbours[edge.b].size() < m) {
            neighbours[edge.b].push_back(edge.a);
        }
    }
}

/**
 * Returns a number of the score of the solution.
 * @return the distance of the solutional path
 */
 int TSP::total_dist() const {
    int distance = 0;
    for(int i = 0; i < n; ++i) {
        distance += dist(i, points[i].next);
    }

    return distance;
}

/**
 * Returns a number of the score of the given path.
 * @return the distance of the given path
 */
int TSP::total_dist(const vector<Point>& vec) const {
    int distance = 0;
    for(int i = 1; i < n; ++i) {
        distance += dist(i, vec[i].next);
    }

    return distance;
}

/**
 * Returns the euclidian distance between the two points a and b.
 * @param  a point
 * @param  b point
 * @return   a euclidian distance rounded to integer
 */
int TSP::dist(int a, int b) const {
    return (int) (sqrt((points[a].x-points[b].x)*(points[a].x-points[b].x) + (points[a].y-points[b].y)*(points[a].y-points[b].y)) + 0.5);
}

/**
 * Prints the result. Must be called _after_ running any of the executing algorithms.
 * Just prints the points list, one on each line.
 */
 void TSP::print_result() const{
    int i = 0;
    do {
        cout << i << endl;
        i = points[i].next;
    } while(i != 0);
}

// ########################################################################################
// ########################################################################################
// ####################### Shortest egde ##################################################
// ########################################################################################
// ########################################################################################
/**
 * Executes shortest edge algorithm.
 */
void TSP::exec_shortest_edge()  {
    cerr << "Kör shortest edge." << endl;

    // Create a list of edges
    vector<Edge> edges;

    // Create all possible edges (n^2) in the graph and add them to the list
    int x = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < i; j++) {
            Edge e(i, j, dist(i, j));
            edges.push_back(e);
            x++;
        }     
    }

    // Sort the list, shortest edges first!
    sort(edges.begin(), edges.end(), [](const Edge & a, const Edge & b) {
        return a.dist < b.dist; 
    });

    // The new preliminary tour is saved in tour_prel
    list<Edge> tour_prel;

    // Stores the degree of every node (max 2), default it to 0
    int degree[n]; 
    for (int i = 0; i < n; ++i) {
        degree[i] = 0;
    }

    // The top smallest edges are examined and added to the path if 
    // it does not create a cycle or makes the degree > 2, since that
    // would not make a hamiltonian cycle
    for (auto & edge : edges) {
        if (tour_prel.size() >= n) {
            break;
        }
        if (degree[edge.a] > 1 || degree[edge.b] > 1 || (tour_prel.size() < n-1 && is_cyclic(tour_prel, edge)))
            continue;
        tour_prel.push_back(edge);
        degree[edge.a]++;
        degree[edge.b]++;
    }

    bool visited[n];
    for (int i = 0; i < n; i++) { visited[i] = false; }
    // Assign the tour to the linked list
    int node = 0;
    visited[0] = true;
    for (int i = 0; i < n; i++) {
        for (auto & edge : tour_prel) {
            if (edge.a == node && !visited[edge.b]) {
                visited[node] = true;
                points[node].next = edge.b;
                points[edge.b].prev = node;
                node = edge.b;
                break;
            } else if (edge.b == node && !visited[edge.a]) {
                visited[node] = true;
                points[node].next = edge.a;
                points[edge.a].prev = node;
                node = edge.a;
                break;
            }
        }
    }
    points[node].next = 0;
    points[0].prev = node;
}

/**
 * Assumes that prel is already non-cyclic
 * @param  prel    [description]
 * @param  e       [description]
 * @param  visited [description]
 * @param  v       [description]
 * @param  parent  [description]
 * @return         [description]
 */
 int TSP::is_cyclic(list<Edge> & prel, Edge e) const {
    int visited[n];
    for (int i = 0; i < n; i++) {
        visited[i] = false;
    }
    int node = e.a;

    while (true) {
        visited[node] = true;
        if (node == e.b) {
            return true;
        }
        bool found = false;
        for (auto & n : prel) {
            if (n.a == node && !visited[n.b]) {
                node = n.b;
                found = true;
                break;
            }
            if (n.b == node && !visited[n.a]) {
                node = n.a;
                found = true;
                break;
            }
        }
        if (!found) {
            break;
        }
    }
    return false;
}

// ########################################################################################
// ########################################################################################
// ####################### Closest neighbours #############################################
// ########################################################################################
// ########################################################################################
/**
 * Naive algorithm.
 */
void TSP::exec_naive() {
    cerr << "Kör naiva." << endl;
    // Init some arrays where we will store the tour
    // tour[x] will, when we have calculated the tour up to x, contain a tour from 0 to x.
    //          The other values are undefined.
    int tour[n];
    // used[x] will be true if x is in the tour[] array yet
    bool used[n]; for (int i = 0; i < n; i++) { used[i] = false; }

    // Always start at first place
    tour[0] = 0; 
    used[0] = true;

    // Best will always save the, to every point, best next place
    int best;

    // Loop through all entrys in the tour and select one place for every stop
    for (int i = 1; i < n; ++i)
    {
        best = -1;
        for (int j = 0; j < n; ++j)
        {
            // If it is closer to 
            if (!used[j] && (best == -1 || dist(tour[i-1], j) < dist(tour[i-1], best))) 
            {
                best = j;
            }
        }
        tour[i] = best;
        used[best] = true;
    }

    for (int i = 1; i < n-1; i++) {
        points[tour[i]].next = tour[i+1];
        points[tour[i]].prev = tour[i-1];
    }
    points[tour[n-1]].next = tour[0];
    points[tour[n-1]].prev = tour[n-2];
    points[tour[0]].prev = tour[n-1];
    points[tour[0]].next = tour[1];
}

// ########################################################################################
// ########################################################################################
// ####################### 2-OPT ##########################################################
// ########################################################################################
// ########################################################################################
/**
 * Improves a solution with 2-OPT
 * @param  k_max maximum number of iterations
 * @return       number of iterations
 */
int TSP::improve_two_opt(int k_max) {
    cerr << "Kör 2-opt (vanlig)." << endl;

    // Set some initial state variables
    srand(time(NULL));
    int current_score = total_dist();
    bool improvement = true;
    int max_iter = k_max;
    int iter = 0;

    // Loop until no improvement can be made, but maximum max_iter times
    for (; improvement && iter < max_iter; ++iter) { 
        improvement = false;
        int i = 0;
        int iMax = 0, jMax = 0, valMax = 0;
        // Loop and find the best move, then do the move after the loop
        do {
            int j = points[i].next;

            // Find j's neighbours
            while(j != 0 && !improvement) {
                int swapCost = 0;

                if (j == points[i].prev) {
                    // We are swapping everythinng
                    swapCost = 0;
                } else if (j == points[i].next) {
                    swapCost = swap_cost(i, j, points);
                } else {
                    swapCost = two_opt_swap_cost(i, j, points);
                }
                
                if (swapCost < valMax) {
                    // If we found new max, save it
                    valMax = swapCost;
                    iMax = i;
                    jMax = j;
                }

                j = points[j].next;
            }
            i = points[i].next;
        } while (i != 0 && !improvement);

        // Perform max if it is good enough
        if (valMax < 0) {
            two_opt_swap(iMax, jMax, points);
            current_score += valMax;
            improvement = true;
        }
    }
    return iter;
}

/**
 * Improves a solution with 2-OPT
 * @param  k_max maximum number of iterations
 * @param  m     maximum number of neighbours in neighbour list
 * @return       number of iterations
 */
void TSP::improve_two_opt_neighbour(int k_max, int m) {
    cerr << "Kör 2-opt med grannlista." << endl;

    // Make sure m is smaller or equal to n-1, because we cant find other neighbours 
    m = m < n-1 ? m : n-1;

    // Now set all neighbours
    compute_neighbour_list(m);

    // Loop until no improvement can be made, but maximum max_iter times
    bool improvement = true;
    for (int iter = 0; improvement && iter < k_max; ++iter) { 
        improvement = false; // Set to false to demand improvement until next lap in loop
        int i = 0, iMax = 0, jMax = 0, valMax = 0;

        // Loop through each node, and look at its neighbours
        do {

            for (int x = 0; x < m; x++) {
                int j = neighbours[i][x];
                // Inv: there exist an edge between i and j, and it is on the top m shortest from i
                
                int cost = 0;
                if (j == points[i].prev) {
                    cost = 0;
                } else if (j == points[i].next) {
                    cost = swap_cost(i, j, points);
                } else {
                    cost = two_opt_swap_cost(i, j, points);
                }

                if (cost < valMax) {
                    valMax = cost;
                    iMax = i;
                    jMax = j;
                }
            }
            i = points[i].next;
        } while (i != 0 && !improvement);

        if (valMax < 0) {
            two_opt_swap(iMax, jMax, points);
            improvement = true;
        }
    }
}

// ########################################################################################
// ########################################################################################
// ####################### Simulated annealing ############################################
// ########################################################################################
// ########################################################################################
/**
 * Performs simulated annealing.
 * @param k_max the maximum number of iterations
 */
void TSP::improve_simulated_annealing(int k_max) {
    cerr << "Kör simulated annealing." << endl;

    // First seed the random generator with current time
    srand(time(NULL));

    // Set start values for variables
    double t_max = 100;
    double t     = t_max;

    vector<Point> current(points);
    int current_score = total_dist(current);
    int iter = 0;
    // Do the annealing
    for (int k = 0; k < k_max; k++) {
        // Decrease the temperature according to out formula
        t -= t * (10.0 / k_max);

        // Change two random entries
        int r1 = rand() % n;
        int r2 = rand() % n;

        // If numbers are same, it is not value to continue this iteration
        // because it won't change anythin
        if (r1 == r2) continue;

        int cost = swap_cost(r1, r2, current);

        //cerr << "Changing " << r2 << " for " << r1 << "\t";
        //cerr << "Neighbour score: " << current_score - cost << ". " << "Diff: "<< cost << ".   \t";
        
        if (cost <= 0) {
            //cerr << "Accepted" << endl;
            // This solution is better than what we have, go for it!

            swap(r1, r2, current);

            current_score += cost;

            if (current_score < total_dist()) {
                // If best ever
                points = vector<Point>(current);
                //cerr << "Found best ever! \t" << "Dist: " << total_dist() << endl;
            }
        } else if (cost / t < (rand() % 100) / 100.0) { // TODO improve
            //cerr << "Accepted a worse solution by probability" << endl;
            // The neighbor solution, copy of the one we have

            swap(r1, r2, current);

            current_score += cost;
        } else {
            //cerr << "Not accepted" << endl;
        }
    }

    cerr << "Iter: " << iter << endl;
    cerr << "T: " << t << endl;
}

/**
 * An improved version of simulated annealing.
 * @param k_max maximum number of iterations
 * @param m     maximum number of neighbours in list
 */
void TSP::improve_v2_simulated_annealing(int k_max, int m) {
    // First seed the random generator with current time
    srand(time(NULL));

    int naive_score = total_dist();

    // Set start values for variables
    double t_max = naive_score * 0.9; // TODO improve
    double t     = t_max; // TODO improve

    vector<Point> current(points);
    int current_score = total_dist(current);

    // Do the annealing
    for (int k = 0; k < k_max; k++) {
        // Decrease the temperature according to out formula
        t -= t * (10.0 / k_max);

        // Change two random entries
        int r1 = rand() % n;
        int r2 = rand() % n;
        if (neighbours[r1].size() != 0) {
            r2 = rand() % neighbours[r1].size();
            r2 = neighbours[r1][r2];
        }

        int cost = swap_cost(r1, r2, current);
        int twooptCost = cost + 1;
        if (r2 != current[r1].prev && r2 != current[r1].next) {
            twooptCost = two_opt_swap_cost(r1, r2, current);

        }

        if (cost <= 0 || twooptCost <= 0) {
            // This solution is better than what we have, go for it!
            if (cost < twooptCost) {
                swap(r1, r2, current);
            } else {
                two_opt_swap(r1, r2, current);
                cost =  twooptCost;
            }
            current_score += cost;



            if (current_score < total_dist()) {
                // If best ever
                points = vector<Point>(current);
            }
        } else if (cost / t < (rand() % 100) / 100.0) {
            // The neighbor solution, copy of the one we have

            swap(r1, r2, current);

            current_score += cost;
        }

    }
}