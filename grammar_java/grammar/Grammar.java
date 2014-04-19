package grammar;


import grammar.util.Tree;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
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
	private final Set<String> start;
	protected static class NotSupportedException extends RuntimeException{
		private static final long serialVersionUID = -9053053522674942445L;

		NotSupportedException(String s){
			super(s);
		}
	}
	private static class PlainGrammar extends Grammar{
		
		public PlainGrammar(Set<String> terminal, Set<String> nonterminal,
				Set<Rule> rules, Set<String> start) {
			super(terminal, nonterminal, rules, start);
		}

		public PlainGrammar(Set<String> terminal, Set<String> nonterminal,
				Set<Rule> rules, String start) {
			super(terminal,nonterminal,rules,start);
		}

		@Override
		public boolean admits(String[] tokens) {
			throw new NotSupportedException("Parsing not implemented for this grammar.");
		}

		@Override
		public Tree<String> parse(List<String> s) {
			throw new NotSupportedException("Parsing not implemented for this grammar.");
		}
		
	}
	protected Grammar(Set<String> terminal, Set<String> nonterminal, Set<Rule> rules, String start){
		this(terminal,nonterminal, rules, new HashSet<String>(Arrays.asList(start)));
	}
	protected Grammar(Set<String> terminal, Set<String> nonterminal, Set<Rule> rules, Set<String> start){
		this.terminal=Collections.unmodifiableSet(terminal);
		this.nonterminal=Collections.unmodifiableSet(nonterminal);
		this.start=start;
		for(String starts: start) if(!nonterminal.contains(starts)) throw new IllegalArgumentException("Start symbol not recognised.");
		if(!Collections.disjoint(terminal, nonterminal)) throw new IllegalArgumentException("Terminals and nonterminals have nonempty intersection.");
		for(Rule r: rules) if(!isValidRule(r)) throw new IllegalArgumentException("Invalid rule "+r);
		this.rules=Collections.unmodifiableSet(rules);
	}
	
	public Set<String> getTerminalSymbols(){
		return terminal;
	}
	public Set<String> getStartSymbols(){
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
	public abstract grammar.util.Tree<String> parse(List<String> s);
	public grammar.util.Tree<String> parse(String[] s){
		return parse(java.util.Arrays.asList(s));
	}
	public static Grammar createGrammar(Set<String> terminal, Set<String> nonterminal,
			Set<Rule> rules, Set<String> start){
		return new PlainGrammar(terminal,nonterminal,rules,start);
	}
	public static Grammar createGrammar(Set<String> terminal, Set<String> nonterminal,
			Set<Rule> rules,String start){
		return new PlainGrammar(terminal,nonterminal,rules,start);
	}
	@Override
	public String toString(){
		String res=this.getClass().getName()+"\nTerminals: "+terminal.toString()+"\nNonterminals:"+nonterminal.toString()+"\nRules:";
		StringBuilder sb=new StringBuilder(res);
		for(Rule r: rules){
			sb.append("\n"+r.toString());
		}
		return sb.toString();
	}
}
