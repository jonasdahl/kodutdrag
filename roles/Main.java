import java.util.*;
import java.lang.*;

/**
 * Använder simulated annealling för att förbättra en trivial lösning.
 * @author: Jonas Dahl
 */
public class Main {
	// Kattis-io-instansen
	private Kattio io;
	// Probleminstansen
	private Problem problem;
	// Den bästa hittills funna lösningen
	private Solution bestSolution;
	// Max temperature
	private final static int maxTemp = 100;//100;
	// Max temperature
	private double coolingRate = 0.007;//0.007;
	// Max temperature
	private final static double minTemp = 0.01;//0.01;

	/**
	 * Reads input and initiates the Problem problem.
	 */
	private void readInput() {
		int n = io.getInt(); // roller
		int s = io.getInt(); // scener
		int k = io.getInt(); // skådespelare

		ArrayList<LinkedList<Integer>> possibleActors = new ArrayList<LinkedList<Integer>>(n + 1);
		possibleActors.add(0, new LinkedList<Integer>()); // Vi använder aldrig index 0, men det måste finnas något där

		ArrayList<HashSet<Integer>> possibleRoles = new ArrayList<HashSet<Integer>>(k + 1);
		possibleRoles.add(0, new HashSet<Integer>()); // Vi använder aldrig index 0, men det måste finnas något där

		for (int i = 1; i <= k; ++i) { // Initiera alla möjliga skådespelare-listor
			possibleRoles.add(i, new HashSet<Integer>());
		}

		for (int i = 1; i <= n; ++i) { // Initiera alla möjliga skådespelare-listor
			possibleActors.add(i, new LinkedList<Integer>());
			int t = io.getInt();
			for (int j = 0; j < t; ++j) { // Lägg till de skådespelare som kan ha rollen
				int x = io.getInt();
				possibleActors.get(i).add(x);
				possibleRoles.get(x).add(i);
			}
			// Lägg till en dummy-skådis som är en superskådis
			possibleActors.get(i).add(i + k);
		}


		ArrayList<HashSet<Integer>> neighbours = new ArrayList<HashSet<Integer>>(n + 1);
		for (int i = 0; i <= n; ++i) {
			neighbours.add(i, new HashSet<Integer>());
		}

		for (int i = 1; i <= s; ++i) { // Initiera alla grannlistor
			int t = io.getInt();
			int[] p = new int[t];
			for (int j = 0; j < t; ++j) {
				p[j] = io.getInt();
			}

			// Lägg till olika kombinationer av dem
			for (int a = 0; a < t; ++a) {
				for (int b = 0; b < t; ++b) {
					if (p[a] != p[b])
						neighbours.get(p[a]).add(p[b]);
				}
			}
		}
		problem = new Problem(n, s, k, possibleActors, neighbours, possibleRoles);
	}

	/**
	 * Prints output (bestSolution) in the specified format
	 */
	public void printOutput() {
		System.out.print(bestSolution);
	}

	/**
	 * Räknar ut ett accepterat värde för lösningen.
	 */
	public static double accept(int value, int newValue, double temperature) {
        // Om lösningen är bättre, acceptera direkt
		if (newValue > value) {
			return 1.0;
		}

        // Om nya lösningen är sämre, acceptera kanske
        double diff = ((double) value / (value + value - newValue));
		double val = Math.exp((newValue - value) / temperature);
		//System.err.println(value);
		//System.err.println(newValue);
		//System.err.println("temp: " + temperature + "\tval: " + val);
		return val;
	}

	public Solution changeSomething(Solution currentSolution) {
		// Skapa ny lösning utifrån den gamla (en enkel kopia)
		Solution newSolution = new Solution(currentSolution);

		while (true) {
			// Ta fram en random roll
			int role = (int) (1 + problem.n * Math.random());

			// Ta fram en random skådis som kan spela den rollen
			int size = problem.possibleActors.get(role).size();
			int actorIndex = (int) ((size - 2) * Math.random());
			int actor = problem.possibleActors.get(role).get(actorIndex);
			if (newSolution.actorForRole[role] == actor) {
				actor = problem.possibleActors.get(role).get(actorIndex + 1);
			}

			//System.err.println("roll: " + role + " skådis: " + actor);
			// Sätt skådisen till rollen, om det går
			if (newSolution.forceSetActor(role, actor))
				break;
			//System.err.println("Det gick inte så bra, försöker igen, tror jag blir bra");
			//System.err.print("Ändrade roll " + role + " till skådis " + actor);
			//System.err.println("\toch ändrade roll " + role2 + " till skådis " + actor2);
		}
		/*
		try {
			System.out.println(newSolution);
			Thread.sleep(100);
			System.out.print("\u001b[2J");
			System.out.print("\f");
			System.out.flush();
		} catch (Exception e) {
			System.out.println("Thrown");
		}
		*/
		return newSolution;
	}

	/**
	 * Skapar en trivial lösning, och försöker förbättra den
	 */
	public void processData() {
		/*System.out.println(problem);
		System.out.println("***");
		System.out.println("***");
		System.out.println("***");
		System.out.println("***");
		*/
		BadSolver solver = new BadSolver(problem);
		Solution currentSolution = solver.solve();

		// Den aktuella lösningen är ju också den hittills bästa
		bestSolution = new Solution(currentSolution);

        // Vi väljer lite värden för nedkylningen
		//double temp = 0.000002*100000;
		//double coolingRate = 0.0005*100;
		double temp = maxTemp;

        // Sedan gör vi ändringar tills att är mindre än 1 grad
		int a = 0;
		while (temp > minTemp) {
			++a;
			
            // Minska temperaturen successivt
			temp *= 1 - coolingRate;

            // Skapa ny lösning utifrån den gamla (en enkel kopia)
			Solution newSolution = changeSomething(currentSolution);

            // Få de olika värdena för lösningarna
			int currentVal = currentSolution.energy();
			int newVal = newSolution.energy();

            // Decide if we should accept the neighbour
            //System.err.println(accept(currentVal, newVal, temp));
			if (accept(currentVal, newVal, temp) > Math.random()) {
				//System.err.println("                                       ("+currentSolution.size()+")");
				currentSolution = new Solution(newSolution);
			} else {
				//System.err.println("Bytte inte currentSolution");
			}

            // Keep track of the best solution found
			if (currentSolution.size() < bestSolution.size()) {
				//System.err.println("Hittade ny bästa xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
				bestSolution = new Solution(currentSolution);
			}
		}
		System.err.println(a);
	}

	Main() {
		io = new Kattio(System.in);
		readInput();
		processData();
		printOutput();
	}

	public static void main(String args[]) {
		new Main();
	}
}