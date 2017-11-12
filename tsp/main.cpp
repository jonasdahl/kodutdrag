#include <iostream>
#include "tsp.h"
using namespace std;

/**
 * Main function that is run on startup.
 * @param  argc number of command line parameters, should be 1 for testing, 0 for kattis
 * @param  argv the command line parameters, number between 1 and 7 for different algorithms, m for default
 *                  1 - Closest neighbour
 *                  2 - Closest neighbour and two opt with neighbour list
 *                  3 - Closest neighbour and simulated annealing
 *                  4 - Closest neighbour and two opt with neighbour list and simulated annealing
 *                  5 - Shortest edge
 *                  6 - Shortest edge and two opt with neighbour list
 *                  7 - Shortest edge and simulated annealing
 *                  8 - Shortest edge and two opt with neighbour list and simulated annealing
 *                  9 - Shortest edge and two opt with neighbour list and simulated annealing or closest neighbour and picking best
 * @return      0
 */
int main(int argc, char *argv[]) {
    // Default algorithm
    int algorithm = 6;

    // Read algorithm from command line
    if (argc == 2) {
        algorithm = (int) (*argv[1] == 'm' ? algorithm : *argv[1]) - 48;
    }

    // Create the TSP problem
    TSP tsp;

    // Create space to read to
    int n;
    double x, y;

    // Read first how many coordinates we should read, and then read and add them
    cin >> n;
    for (int i = 0; i < n; ++i) {
    	cin >> x;
    	cin >> y;
    	tsp.add_point(x, y);
    }
    tsp.heal_list();

    // Special cases for small n, which makes us being able to skip
    // taking care of these in other parts of the program.
    switch (n) {
        case 0:
            exit(0);
        case 1:
            cout << "0" << endl;
            exit(0);
        case 2:
            cout << "0" << endl;
            cout << "1" << endl;
            exit(0);
        case 3:
            cout << "0" << endl;
            cout << "1" << endl;
            cout << "2" << endl;
            exit(0);
    }

    // Chose algorithm to run
    switch (algorithm) {
        case 1:
            tsp.exec_naive();
            break;
        case 2:
            tsp.exec_naive();
            tsp.improve_two_opt_neighbour(300, 200);
            break;
        case 3:
            tsp.exec_naive();
            tsp.improve_simulated_annealing(10000);
            break;
        case 4:
            tsp.exec_naive();
            tsp.improve_two_opt_neighbour(300, 200);
            tsp.improve_simulated_annealing(4000);
            break;
        case 5:
            tsp.exec_shortest_edge();
            break;
        case 6:
            tsp.exec_shortest_edge();
            tsp.improve_two_opt_neighbour(10000, 300);
            break;
        case 7:
            tsp.exec_shortest_edge();
            tsp.improve_simulated_annealing(1000000);
            break;
        case 8:
            tsp.exec_shortest_edge();
            tsp.improve_two_opt_neighbour(10000, 300);
            tsp.improve_simulated_annealing(4000);
            break;
        case 9:
            tsp.exec_naive();
            int dist1 = tsp.total_dist();

            tsp.exec_shortest_edge();
            tsp.improve_two_opt_neighbour(10000, 300);
            tsp.improve_simulated_annealing(10000);
            int dist2 = tsp.total_dist();

            if (dist1 < dist2) {
                tsp.exec_naive();
            }
            break;
    }

    // Print the result
    tsp.print_result();

    // Log the final distance
    cerr << "Final distance: " << tsp.total_dist() << endl;

    return 0;
}