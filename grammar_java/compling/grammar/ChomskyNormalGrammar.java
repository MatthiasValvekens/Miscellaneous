package compling.grammar;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import compling.grammar.util.Tree;

public class ChomskyNormalGrammar extends ExtendedChomskyNormalGrammar {
	//operates on the assumption that the grammar in question is meant to be Chomsky normal
	static ChomskyNormalGrammar getSymbolsFromRules(Set<Rule> rules,String start){
		HashSet<String> term= new HashSet<String>();
		HashSet<String> nonterm= new HashSet<String>();
		for(Rule r: rules){
			if(r.getInput().size()!=1) throw new IllegalArgumentException("This grammar is not context-free.");
			nonterm.add(r.getInput().get(0));
			switch(r.getOutput().size()){
			case 0:
				if(!start.equals(r.getInput().get(0))) throw new IllegalArgumentException("Illegal epsilon rule for Chomsky normal form."+r);
			case 1:
				term.add(r.getOutput().get(0));
				break;
			case 2:
				nonterm.addAll(r.getOutput());
				break;
			default:
				throw new IllegalArgumentException("Too many branches for Chomsky normal grammar."+r); 
			}
		}
		ChomskyNormalGrammar res=null;
		 try{
			 res=new ChomskyNormalGrammar(term, nonterm, rules, start);
		 } catch(Exception e){
			 throw new RuntimeException("Failed to read symbols.\n"+term+"\n"+nonterm,e);
		 }
		 return res;
	}
	public ChomskyNormalGrammar(Set<String> terminal,
			Set<String> nonterminal, Set<Rule> rules, String start) {
		super(terminal, nonterminal, rules, start);
	}
	public ChomskyNormalGrammar(Set<String> terminal,
			Set<String> nonterminal, Set<Rule> rules, Set<String> startSymbols) {
		super(terminal,nonterminal,rules,startSymbols);
	}
	public Tree<String> parse(String[] input){
		return parse(Arrays.asList(input));
	}
	
	@Override
	public boolean isValidRule(Rule r){
		if(!super.isValidRule(r)) return false;
		
//		List<String> inp=r.getInput();
		List<String> outp=r.getOutput();
		if(outp.size()==0) return false;
//		//Start symbol only allowed when output is empty.
//		if(inp.get(0).equals(getStartSymbol()) && outp.size()!=0) return false;
		if(outp.size()>2) return false;
		//only terminal symbols allowed in A->a replacements
		if(outp.size()==1 && !getTerminalSymbols().contains(outp.get(0))) return false;
		//only nonterminal symbols allowed in A->BC replacements
		if(outp.size()==2 && (!getNonterminalSymbols().contains(outp.get(0)) || !getNonterminalSymbols().contains(outp.get(1)))) return false;
		return true;
	}

}
