import java.util.*;
import java.lang.*;

/**
 * Representerar en tilldelning, en lösning.
 * @author Jonas Dahl.
 */
public class BadSolver {
	// The problem data
	private Problem problem;
	// The problem solution we are working with
	private Solution solution;

	/**
	 * Constructor that initiates solution
	 */
	public BadSolver(Problem p) {
		problem = p;
		solution = new Solution(p);
	}

	/**
	 * Call to solve
	 */
	public Solution solve() {
		findRoleForP1();
		populate();

		return solution;
	}

	/**
	 * Finds a role for p2 in this solution.
	 */
	private boolean findRoleForP2(int p1Role) {
		PriorityQueue<Prio> queue = new PriorityQueue<Prio>();
		for (int i = 1; i <= problem.n; ++i) { 
			if (i != p1Role && problem.possibleActors.get(i).contains(2)) {
				queue.add(new Prio(problem.possibleActors.get(i).size(), i));
			}
		}

		while (queue.size() > 0) {
			int i = queue.remove().value;
			// Roll i skulle kunna spelas av p2
			// Kontrollera nu bara att det inte finns en scen där roll i och p1role spelar i båda två
			if (problem.neighbours.get(i).contains(p1Role)) {
				continue;
			}
			if (solution.setActor(p1Role, 1) && solution.setActor(i, 2))
				return true;
		}
		return false;
	}

	/**
	 * Finds a role for p1 in the solution.
	 */
	public void findRoleForP1() {
		PriorityQueue<Prio> queue = new PriorityQueue<Prio>();
		for (int i = 1; i <= problem.n; ++i) { 
			if (problem.possibleActors.get(i).contains(1)) {
				queue.add(new Prio(problem.possibleActors.get(i).size(), i));
			}
		}

		while (queue.size() > 0) {
			if (findRoleForP2(queue.remove().value)) {
				break;
			}
		}
	}

	/**
	 * Populates all empty roles with actor
	 */
	private void populate() {
		for (int i = problem.n; i > 0; --i) {
			if (solution.actorForRole[i] != 0) 
				continue;

			int max = 0;
			for (int actor : problem.possibleActors.get(i)) {
				boolean found = false;
				int z = 1;
				int j = problem.neighbours.get(i).size();
				for (int neighbour : problem.neighbours.get(i)) {
					if (solution.actorForRole[neighbour] == actor) {
						found = true;
						break;
					}
				}
				if (!found && solution.setActor(i, actor))
					break;
			}
		}
	}

	/**
	 * Class for saving prioritized values in Priority Queue.
	 * @author Jonas Dahl
	 * @date 2016-01-01
	 */
	class Prio implements Comparable<Prio> {
		public int prio;
		public int value;

		@Override
		public int compareTo(Prio p) {
			return prio - p.prio;
		}

		public Prio(int prio, int value) {
			this.prio = prio;
			this.value = value;
		}
	}
}