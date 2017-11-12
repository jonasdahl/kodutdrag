import java.util.*;
import java.lang.*;

/**
 * Representerar en tilldelning, en lösning.
 * @author Jonas Dahl.
 */
public class Solution {
	// Innehåller vem som spelar vilken roll
	public int[] actorForRole;
	// Sparar vilka roller varje person har
	public ArrayList<LinkedList<Integer>> rolesForActor;
	// Sparar om person i använts redan eller inte
	public int[] usedActor;
	// Information om problemet
	private Problem problem;

	public String name;

	/**
	 * Default constructor, creates empty lists.
	 */
	public Solution(Problem p) {
		name = UUID.randomUUID().toString();;
		problem = p;

		// Initiate solution vectors
		usedActor = new int[problem.n + problem.k + 1];
		actorForRole = new int[problem.n + 1];
		rolesForActor = new ArrayList<LinkedList<Integer>>(problem.n + problem.k + 1);
		for (int i = 0; i <= problem.n + problem.k; i++) {
			rolesForActor.add(new LinkedList<Integer>());
		}
	}

	/**
	 * Copy constructor, pass another solution and it will be copied.
	 */ 
	public Solution(Solution sol) {
		// Copy problem variables
		problem = sol.problem;

		// Copy solution data
		usedActor = new int[sol.usedActor.length];
		System.arraycopy(sol.usedActor, 0, usedActor, 0, sol.usedActor.length);
		actorForRole = new int[sol.actorForRole.length];
		System.arraycopy(sol.actorForRole, 0, actorForRole, 0, sol.actorForRole.length);
		rolesForActor = new ArrayList<LinkedList<Integer>>(sol.rolesForActor.size());
		for (LinkedList<Integer> el : sol.rolesForActor) {
			rolesForActor.add(new LinkedList<Integer>(el));
		}
	}

	/**
	 * Sets actor to role if possible and don't break anything
	 */
	public boolean setActor(int role, int actor) {
		if (actorForRole[role] == actor)
			return false;
		// It should not be possible to assign an actor to a role that it can't play
		if (!problem.possibleActors.get(role).contains(actor)) {
			return false;
		}

		// It should not be possible to set the actor to a role if the role plays agains the actor (or p1 vs p2)
		for (Integer neighbour : problem.neighbours.get(role)) {
			if (actorForRole[neighbour] == actor || actor == 1 && actorForRole[neighbour] == 2 || actor == 2 && actorForRole[neighbour] == 1) {
				return false;
			}
		}

		if (rolesForActor.get(actorForRole[role]).remove((Integer) role)) {
			usedActor[actorForRole[role]]--; // The actor that had that role before now has one less
		}
		usedActor[actor]++; // Increase role counter for actor
		actorForRole[role] = actor;
		rolesForActor.get(actor).add(role);
		return true;
	}

	/**
	 * Forces actor for role. Will change neighbors if necessary to get valid solution.
	 */
	public boolean forceSetActor(int role, int actor) {
		if (actorForRole[role] == actor) {
			// Skådespelaren har redan rollen, onödigt att "byta"
			return false;
		}

		if ((actorForRole[role] == 1 && usedActor[1] == 1) || 
			(actorForRole[role] == 2 && usedActor[2] == 1)) {
			// Skådespelare 1 eller 2 har rollen sedan tidigare och den som har rollen har bara den
			return false;
		}

		if (!problem.possibleActors.get(role).contains(actor)) {
			// Man kan inte ge en skådis en roll som den inte kan ha enligt listan
			return false;
		}

		for (Integer neighbour : problem.neighbours.get(role)) {
			if (actor < 3 && actorForRole[neighbour] < 3 && actorForRole[neighbour] != actor) {
				// Man ska inte kunna tvinga p1 och p2 att flytta om de är grannar
				return false;
			}
		}

		// Sätt värdet för den aktuella rollen
		if (rolesForActor.get(actorForRole[role]).remove((Integer) role)) {
			usedActor[actorForRole[role]]--; // The actor that had that role before now has one less
		}
		usedActor[actor]++; // Increase role counter for actor
		actorForRole[role] = actor;
		rolesForActor.get(actor).add(role);

		// Uppdatera grannarna
		for (Integer neighbour : problem.neighbours.get(role)) {
			if (actorForRole[neighbour] == actor || actor == 1 && actorForRole[neighbour] == 2 || actor == 2 && actorForRole[neighbour] == 1) {
				// Den här grannen conflictar, ändra den
				for (Integer val : problem.possibleActors.get(neighbour)) {
					// Testa val. För att göra det måste vi gå igenom alla grannens grannar för att se att ingen är val
					// ingen av grannes grannar får heller vara 1 om val är 2 eller 2 om val är 1
					boolean found = false;
					for (Integer neighboursNeighbour : problem.neighbours.get(neighbour)) {
						if (actorForRole[neighboursNeighbour] == val 
							|| (actorForRole[neighboursNeighbour] == 1 && val == 2) 
							|| (actorForRole[neighboursNeighbour] == 2 && val == 1)) {
							found = true;
							break;
						}
					}
					if (!found) {
						if (rolesForActor.get(actorForRole[neighbour]).remove((Integer) neighbour)) {
							usedActor[actorForRole[neighbour]]--; // The actor that had that role before now has one less
						}
						usedActor[val]++; // Increase role counter for actor
						actorForRole[neighbour] = val;
						rolesForActor.get(val).add(neighbour);
						break;
					}
				}

			}
		}

		return true;
	}

	/**
	 * The size of the solution. 
	 */
	public int size() {
		int count = 0;
		for (LinkedList<Integer> roles : rolesForActor) {
			if (roles.size() > 0) {
				count++;
			}
		}
		return count;
	}

	/**
	 * The energy of the solution. 
	 */
	public int energy() {
		int count = 0;
		int actor = 1;
		for (int val : usedActor) {
			count += 10000000 * val * val;
		}
		return count;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(size() + "\n");
		int i = 0, rest = 0;
		for (LinkedList<Integer> roles : rolesForActor) {
			if (roles.size() > 0) {
				sb.append((i - rest) + " " + roles.size());
				for (Integer role : roles) {
					sb.append(" " + role);
				}
				sb.append("\n");
			} else if (i > problem.n) {
				rest++;
			}
			i++;
		}
		return sb.toString();
	}
}