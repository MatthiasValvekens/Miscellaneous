package grammar;


import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * 
 * @author Matthias Valvekens
 * @version 1.0
 */
public abstract class Grammar {
	public static final String DEFAULT_DELIM=" ";
	private final Set<String> terminal;
	private final Set<String> nonterminal;
	private final Set<Rule> rules;
	private final String start;
	public Grammar(Set<String> terminal, Set<String> nonterminal, Set<Rule> rules){
		this(terminal,nonterminal, rules, (String) nonterminal.toArray()[0]);
	}
	public Grammar(Set<String> terminal, Set<String> nonterminal, Set<Rule> rules, String start){
		this.terminal=Collections.unmodifiableSet(terminal);
		this.nonterminal=Collections.unmodifiableSet(nonterminal);
		this.start=start;
		if(!nonterminal.contains(start)) throw new IllegalArgumentException("Start symbol not recognised.");
		if(!Collections.disjoint(terminal, nonterminal)) throw new IllegalArgumentException("Terminals and nonterminals have nonempty intersection.");
		for(Rule r: rules) if(!isValidRule(r)) throw new IllegalArgumentException("Invalid rule "+r);
		this.rules=Collections.unmodifiableSet(rules);
	}
	
	public Set<String> getTerminalSymbols(){
		return terminal;
	}
	public String getStartSymbol(){
		return start;
	}
	public Set<String> getNonterminalSymbols(){
		return nonterminal;
	}
	public Set<Rule> getRules(){
		return rules;
	}
	public abstract boolean admits(String[] tokens);
	
	public boolean admits(String s, String delim){
		return admits(s.split(delim));
	}
	public boolean admits(String s){
		return admits(s,DEFAULT_DELIM);
	}
	public boolean isValidRule(Rule r){
		//every rule must have at least one nonterminal input symbol
		List<String> s=r.getInput();
		for(String n: nonterminal) if(s.indexOf(n)!=-1) return true;
		return false;
	}
}
