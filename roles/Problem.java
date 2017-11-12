import java.util.*;
import java.lang.*;

/**
 * Representerar ett problem, med data kopplat till det.
 * @author Jonas Dahl.
 */
public class Problem {
	// Antal roller ["vertex"]
	public int n;
	// Antal scener ["kanter"]
	public int s;
	// Antal skådespelare ["färger"]
	public int k;
	// Möjliga skådespelare för en viss roll
	public ArrayList<LinkedList<Integer>> possibleActors;
	// De roller som spelar mot varandra
	public ArrayList<HashSet<Integer>> neighbours;
	// De roller som spelar mot varandra
	public ArrayList<HashSet<Integer>> possibleRoles;

	/**
	 * Default constructor, creates empty lists.
	 */
	public Problem(int n, int s, int k, ArrayList<LinkedList<Integer>> possibleActors, ArrayList<HashSet<Integer>> neighbours, ArrayList<HashSet<Integer>> possibleRoles) {
		// Save all parameters
		this.n = n; 
		this.s = s; 
		this.k = k; 
		this.possibleActors = possibleActors; 
		this.neighbours = neighbours;
		this.possibleRoles = possibleRoles;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (int i = 1; i <= n; i++) {
			sb.append(i + ": ");
			for (Integer neighbour : neighbours.get(i)) {
				sb.append(neighbour + " ");
			}
			sb.append("\n");
		}
		return sb.toString();
	}
}