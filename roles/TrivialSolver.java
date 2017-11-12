import java.util.*;
import java.lang.*;

/**
 * Representerar en tilldelning, en lösning.
 * @author Jonas Dahl.
 */
public class TrivialSolver {
	// The problem data
	private Problem problem;
	// The problem solution we are working with
	private Solution solution;

	/**
	 * Constructor that initiates solution
	 */
	public TrivialSolver(Problem p) {
		problem = p;
		solution = new Solution(p);
	}

	/**
	 * Call to solve
	 */
	public Solution solve() {
		findRoleForP1();
		addLones();
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
	 * Goes through whole tree and adds actors that are the single ones for the role.
	 */
	private void addLones() {
		for (int i = 1; i <= problem.n; ++i) {
			if (solution.actorForRole[i] != 0) {
				continue;
			}

			if (problem.possibleActors.get(i).size() == 2) {
				solution.setActor(i, problem.possibleActors.get(i).getFirst());
			}
		}
	}

	/**
	 * Finds a priority queue of the best actors for the role
	 */
	private PriorityQueue<Prio> findBestChoise(int i) {
		PriorityQueue<Prio> queue = new PriorityQueue<Prio>();
		int z = 0;
		Prio[] actors = new Prio[problem.possibleActors.get(i).size()];
		
		for (Integer actor : problem.possibleActors.get(i)) {
			actors[z] = new Prio(0, actor);
			z++;
		}
		// Now, the different possible actors are in actors[]

		for (Integer neighbour : problem.neighbours.get(i)) {
			for (Integer neighbourPossibleActor : problem.possibleActors.get(neighbour)) { 
				// För varje möjlig ska vi ge poäng
				int s = problem.possibleActors.get(neighbour).size();
				int v = s * s;
				for (int j = 0; j < actors.length; j++) {
					if (neighbourPossibleActor == actors[j].value) {
						actors[j].prio += v;
					}
				}
			}
		}

		for (int j = 0; j < actors.length; j++) {
			if (solution.usedActor[actors[j].value] > 0)
				actors[j].prio -= 10;
			if (actors[j].value > problem.k) 
				actors[j].prio += 10000000;
			queue.add(actors[j]);
		}
		return queue;
	}

	/**
	 * Populates all empty roles with actor
	 */
	private void populate() {
		for (int i = 1; i <= problem.n; ++i) {
			if (solution.actorForRole[i] != 0) 
				continue;

			PriorityQueue<Prio> queue = findBestChoise(i);
			while (queue.size() > 0) {
				int act = queue.remove().value;
				if (solution.setActor(i, act)) {
					break;
				}
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