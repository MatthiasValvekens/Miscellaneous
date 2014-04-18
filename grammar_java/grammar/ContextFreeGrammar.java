package grammar;

import grammar.util.Tree;
import grammar.util.*;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.ArrayList;
import java.util.Stack;

public abstract class ContextFreeGrammar extends Grammar {
	
	/**
	 * Bogus implementation of ContextFreeGrammar with abstract methods not implemented.
	 * @author Matthias Valvekens
	 * @version 1.0
	 */
	private static class PlainGrammar extends ContextFreeGrammar{
		
		public PlainGrammar(Set<String> terminal, Set<String> nonterminal,
				Set<Rule> rules, String start) {
			super(terminal, nonterminal, rules, start);
		}

		@Override
		public boolean admits(String[] tokens) {
			throw new Grammar.NotSupportedException("Parsing not implemented for this grammar.");
		}

		@Override
		public Tree<String> parse(List<String> s) {
			throw new Grammar.NotSupportedException("Parsing not implemented for this grammar.");
		}
		
	}
	public ContextFreeGrammar(Set<String> terminal, Set<String> nonterminal,
			Set<Rule> rules, String start) {
		super(terminal, nonterminal, rules, start);
	}

	@Override
	public boolean isValidRule(Rule r) {
		if(r.getInput().size()!=1) return false;
		return super.isValidRule(r);
	}
	public ChomskyNormalGrammar toCNF(){
		return toCNF(new StandardSymbolGenerator());
	}
	public ChomskyNormalGrammar toCNF(SymbolGenerator sym){
		//save ourselves some work
		if(this instanceof ChomskyNormalGrammar) return (ChomskyNormalGrammar) this;
		Set<String> newnonterm = new java.util.HashSet<String>(getNonterminalSymbols());
		Set<Rule> newrules=new java.util.HashSet<Rule>(getRules());
		//add new start symbol
		String newstart = sym.generateNewSymbol(getStartSymbol(),getTerminalSymbols(),getNonterminalSymbols());
		newnonterm.add(newstart);
		newrules.add(new Rule(Arrays.asList(newstart),Arrays.asList(getStartSymbol())));
		//add dummy terminals & rules where necessary
		for(String a : getTerminalSymbols()){
			//check if a rule of the form A->a already exists
			boolean found=false;
			for(Rule r: getRules()){
				if(r.getOutput().size()==1 && r.getOutput().get(0).equals(a)){
					found=true;
					break;
				}
			}
			if(found) continue;
			//create the dummy symbol and edit all rules to reflect the change
			String newdummy=sym.generateNewSymbol(a,getTerminalSymbols(),getNonterminalSymbols());
			newnonterm.add(newdummy);
			newrules.add(new Rule(Arrays.asList(newdummy),Arrays.asList(a)));
			for(Rule r: getRules()){
				if(r.getOutput().size()<2) continue;
				List<String> routput=new ArrayList<String>(r.getOutput());
				Collections.replaceAll(routput, a, newdummy);
				newrules.remove(r);
				newrules.add(new Rule(r.getInput(),routput));
			}
		}
		//break up rules that are too long
		for(Rule r : new HashSet<Rule>(newrules)){
			int size=r.getOutput().size();
			if(size<=2) continue; //already OK
			newrules.remove(r);
			//we need size-2 new variables
			String[] newsymb= new String[size-2];
			int rulehash=r.hashCode();
			for(int j = 0;j<size-2;j++){
				newsymb[j]=sym.generateNewSymbol("X"+rulehash,getTerminalSymbols(),newnonterm);
			}
			newnonterm.addAll(Arrays.asList(newsymb));
			//add new rules
			newrules.add(new Rule(r.getInput(),Arrays.asList(r.getOutput().get(0),newsymb[0])));
			for(int i= 0 ;i<size-3;i++){
				newrules.add(new Rule(Arrays.asList(newsymb[i]),Arrays.asList(r.getOutput().get(i+1),newsymb[i+1])));
			}
			newrules.add(new Rule(Arrays.asList(newsymb[size-3]),Arrays.asList(r.getOutput().get(size-2),r.getOutput().get(size-1))));
		}
		//remove epsilons
		//identify all epsilon-generating nonterminals
		Set<String> epsgen=new HashSet<String>();
		Set<String> newgenerators=new HashSet<String>();
		Set<Rule> rulecopy = newrules;
		
		// TODO: this implementation is probably fairly inefficient
		do {
			
			epsgen.addAll(newgenerators);
			//replace all instances of A that occur in A->eps with nothing
			Set<Rule> temp = new HashSet<Rule>();
			
			for(Rule r: rulecopy){
				List<String> newout=new ArrayList<String>();
				//rebuild the output with appropriate symbols only
				for(int i=0;i<r.getOutput().size();i++){
					String symb=r.getOutput().get(i);
					if(!newgenerators.contains(symb)) newout.add(symb);
				}
				temp.add(new Rule(r.getInput(),newout));
			}
			rulecopy=temp;
			//find all rules that can currently generate epsilon in one step
			newgenerators=new HashSet<String>();
			for(Rule r: new HashSet<Rule>(rulecopy)){
				if(r.getOutput().size()!=0) continue;
				newgenerators.add(r.getInput().get(0));
				if(!r.getInput().get(0).equals(newstart)) rulecopy.remove(r);
			}
		} while(newgenerators.size()!=0);
		//actually propagate the removal
		Set<Rule> tobeadded=new HashSet<Rule>();
		Set<Rule> toberemoved=new HashSet<Rule>();
		for(Rule r: newrules){
			//remove empty rules that are not S->eps
			if(r.getOutput().size()==0 && !r.getInput().get(0).equals(newstart)) toberemoved.add(r);
		}
		newrules.removeAll(toberemoved);
		for(String s : epsgen){
			//generate new rules A->B for all rules of the form A->BC or A->CB where C->eps
			for(Rule r: newrules){
				if(r.getOutput().size()<2) continue;
				if(r.getOutput().get(0).equals(s)) tobeadded.add(new Rule(r.getInput(),Arrays.asList(r.getOutput().get(1))));
				//exclude the A->CC case with else
				else if(r.getOutput().get(1).equals(s)) tobeadded.add(new Rule(r.getInput(),Arrays.asList(r.getOutput().get(0))));
			} 
		}
		newrules.addAll(tobeadded);
		//remove unit rules
		
		//collapse all strong components of the directed graph of unit rules, and remove the remaining redundancies
		//(i.e. removing the cycles from the graph)
		//e.g. A->B and B->A gets removed, and all instances of A are replaced by B
		Set<Rule> unitrules=new HashSet<Rule>();
		for(Rule r: newrules){
			if(r.getOutput().size()==1 && newnonterm.contains(r.getOutput().get(0))) unitrules.add(r);
		}
		Set<List<String>> cycles=GrammarUtils.tarjan(newnonterm,unitrules);
		for(List<String> c: cycles){
			//System.out.println(c);
			if(c.size()==1) continue;
			String rep=c.get(0);
			for(Rule r : new HashSet<Rule>(newrules)){
				List<String> newin= c.contains(r.getInput().get(0)) ? Arrays.asList(rep) : Arrays.asList(r.getInput().get(0));
				newrules.remove(r);
				List<String> newout=new ArrayList<String>(r.getOutput().size());
				for(int i = 0; i<r.getOutput().size();i++){
					String oldval=r.getOutput().get(i);
					if(c.contains(oldval)) newout.add(rep);
					else newout.add(oldval);
				}
				if(!newin.equals(newout)){
					newrules.add(new Rule(newin,newout));
					
				}
			}
		}
		//extract unit rules again
		unitrules=new HashSet<Rule>();
		for(Rule r: newrules){
			if(r.getOutput().size()==1 && newnonterm.contains(r.getOutput().get(0))) unitrules.add(r);
		}
		//propagate remaining unit rules
		
		Stack<List<String>> stack = new Stack<List<String>>();
		//start at terminals and work backwards to find all nonterminals that generate a given terminal by unit rules only
		for(String t: getTerminalSymbols()){
			//find the (unique) nonterminal that generates this symbol in one step
			
			for(String nt: newnonterm){
				Rule r=new Rule(Arrays.asList(nt),Arrays.asList(t));
				
				//check all rules, unitrules doesn't contain the terminal-final rules
				if(newrules.contains(r)) {
					stack.push(Arrays.asList(r.getInput().get(0)));
					break;
				}
			}
			
			do {
				List<String> current=stack.peek();
				String lastnode=current.get(current.size()-1);
				//find and enqueue all generating rules
				boolean isleaf=true;
				for(String nt: newnonterm){
					Rule r= new Rule(Arrays.asList(nt),Arrays.asList(lastnode));
					if(unitrules.contains(r)){
						List<String> newpath= new ArrayList<String>(current);
						newpath.add(nt);
						stack.push(newpath);
						isleaf=false;
					}
				}
				if(isleaf){
					
					newrules.add(new Rule(Arrays.asList(lastnode),Arrays.asList(t)));
					//pop things off the stack till the previous branch
					try {
						while(stack.pop().size()==stack.peek().size()+1);
					} catch(java.util.EmptyStackException e) {}
				}
			} while(!stack.isEmpty());
			
		}
		//remove all unit rules in one go
		for(Rule r: unitrules){
			if(newnonterm.contains(r.getOutput().get(0)))newrules.remove(r);
			
		}
		
		newnonterm=GrammarUtils.pruneUnusedNonterminals(newnonterm, newrules);
		return new ChomskyNormalGrammar(getTerminalSymbols(),newnonterm,newrules,newstart);
	}

	public static ContextFreeGrammar createContextFreeGrammar(Set<String> terminal, Set<String> nonterminal,
			Set<Rule> rules, String start){
		return new PlainGrammar(terminal,nonterminal,rules,start);
	}
}