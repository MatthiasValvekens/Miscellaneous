package compling.grammar.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Set;
import java.util.HashSet;
import java.io.*;

import compling.grammar.*;
public class GrammarUtils {
	public static Set<String> stringToAlphabet(String s){
		Set<String> res= new HashSet<String>();
		for(char c: s.toCharArray()){
			res.add(Character.toString(c));
		}
		return res;
	}
	//assume everything is a nonterminal
	public static Set<String> extractNonterminals(Set<Rule> syntaxrules){
		HashSet<String> s = new HashSet<String>();
		for(Rule r: syntaxrules){
			for(String symb : r.getInput()) s.add(symb);
			for(String symb : r.getOutput()) s.add(symb);
		}
		return s;
	}
	public static <T extends Grammar> T renameSymbol(T grammar, String oldsym, String newsym, Class<T> buildAs){
		
		// TODO: implement this
		throw new UnsupportedOperationException("Not implemented.");
	}
	public static ContextFreeGrammar processUnitRules(ContextFreeGrammar g, boolean generalizeTerminals, boolean deleteUnits){
		//remove unit rules in a manner that does not affect the generated language
		
		//collapse all strong components of the directed graph of unit rules, and remove the remaining redundancies
		//(i.e. removing the cycles from the graph)
		//e.g. A->B and B->A gets removed, and all instances of A are replaced by B
		Set<Rule> newrules=new HashSet<Rule>(g.getRules());
		Set<Rule> unitrules=new HashSet<Rule>();
		Set<String> startlist=new HashSet<String>(g.getStartSymbols());
		for(Rule r: newrules){
			if(r.getOutput().size()==1 && g.getNonterminalSymbols().contains(r.getOutput().get(0))) {
				unitrules.add(r);
				if(startlist.contains(r.getInput().get(0))) startlist.add(r.getOutput().get(0));
			}
		}
		Set<List<String>> cycles=GrammarUtils.tarjan(g.getNonterminalSymbols(),unitrules);
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
			if(r.getOutput().size()==1 && g.getNonterminalSymbols().contains(r.getOutput().get(0))) unitrules.add(r);
		}
		
//		for(Rule r: new HashSet<Rule>(newrules)){
//			if(r.getOutput().size()>1) {
//				for(Rule u: unitrules){
//					List<String> newout=new ArrayList<String>(r.getOutput());
//					Collections.replaceAll(newout, u.getInput().get(0), u.getOutput().get(0));
//					newrules.add(new Rule(r.getInput(),newout));
//				}
//			}
//		}

		//propagate remaining unit rules
		
		Stack<List<String>> stack = new Stack<List<String>>();
		//start at terminals and work backwards to find all nonterminals that generate a given terminal by unit rules only
		Set<Rule> extraunits=new HashSet<Rule>();
		for(String t: g.getTerminalSymbols()){
			//find the (unique) nonterminal that generates this symbol in one step
			
			for(String nt: g.getNonterminalSymbols()){
				Rule r=new Rule(Arrays.asList(nt),Arrays.asList(t));
				
				//check all rules, unitrules doesn't contain the terminal-final rules
				if(newrules.contains(r)) {
					stack.push(Arrays.asList(r.getInput().get(0)));
					break;
				}
			}
			
			while(!stack.isEmpty()) {
				List<String> current=stack.peek();
				String lastnode=current.get(current.size()-1);
				//find and enqueue all generating rules
				boolean isleaf=true;
				for(String nt: g.getNonterminalSymbols()){
					Rule r= new Rule(Arrays.asList(nt),Arrays.asList(lastnode));
					if(unitrules.contains(r)){
						List<String> newpath= new ArrayList<String>(current);
						newpath.add(nt);
						stack.push(newpath);
						isleaf=false;
					}
				}
				if(isleaf){
					//in practice, generalizing the "classes" of terminals up the tree creates a multitude of new possible parses that are essentially identical
					if(generalizeTerminals || (!deleteUnits && !g.getTerminalSymbols().contains(t))){
						//	add new productions
						for(String symbol: current) if(!symbol.equals(t)) extraunits.add(new Rule(symbol,t));
					}
					//pop things off the stack till the previous branch
					List<String> top=null;
					try {
						while((top=stack.pop()).size()==stack.peek().size()+1){
							if(!deleteUnits){
								String topsym= top.get(top.size()-1);
							
								for(String symbol: top) {if(!symbol.equals(topsym))extraunits.add(new Rule(topsym,symbol));}
							}
						}
					} catch(java.util.EmptyStackException e) {
						if(top!=null){
							String topsym= top.get(top.size()-1);
							if(!deleteUnits){
								for(String symbol: top) if(!symbol.equals(topsym))extraunits.add(new Rule(topsym,symbol));
							}
						}
					}
				}
			} 
			//System.out.println(extraunits);
		}
		//remove all unit rules in one go
		if(deleteUnits){
			for(Rule r: unitrules){
				if(g.getNonterminalSymbols().contains(r.getOutput().get(0)))newrules.remove(r);
			}
		}	
		unitrules.addAll(extraunits);
		newrules.addAll(extraunits); //if deleteUnits is on, this set will never contain anything other than terminal generalisations
		//propagate unit rules accross composite rules --- this is included to support nested word classes in an easier manner
		//(we can only restrict the word class of the output too to avoid absurdities. Restricting preconditions only is obviously not allowed.)
		//X-> B A and A -> C leads to X -> B C
		
		//input symbols may be generalised
		//C-> X Y and A -> C leads to A -> X Y
		
		// TODO check whether supporting C -> X A ---> A -> X C is really necessary
		boolean changes;
		do {
			changes=false;
			for(Rule r: new HashSet<Rule>(newrules)){
				if(r.getOutput().size()>1) {
					for(Rule u: unitrules){
					
						if(r.getOutput().contains(u.getInput().get(0))){
							//restrict output
							List<String> newout =new ArrayList<String>(r.getOutput());
							Collections.replaceAll(newout, u.getInput().get(0), u.getOutput().get(0));
	//						if(r.getInput().equals(u.getInput())) newrules.add(new Rule(u.getOutput(),newout));
							changes |= newrules.add(new Rule(r.getInput(),newout));
						}
						if(r.getInput().equals(u.getOutput().get(0))){
							changes |= newrules.add(new Rule(u.getInput(),r.getOutput()));
						}
					}
				}
			}
		}while(changes);
		return ContextFreeGrammar.createContextFreeGrammar(g.getTerminalSymbols(), g.getNonterminalSymbols(), newrules, startlist);
	}
	public static Set<Rule> rulesFromFile(String fname) throws IOException{
		return rulesFromFile(fname,Rule.DEFAULT_TRANSFORM_SYMBOL,Grammar.DEFAULT_DELIM);
	}
	public static Set<Rule> rulesFromFile(String fname,String transformSymbol, String delimiter) throws IOException {
		Set<Rule> result=new HashSet<Rule>();
		BufferedReader r=new BufferedReader(new InputStreamReader(new FileInputStream(fname),"UTF-8"));
		String line;
		while((line=r.readLine())!=null){
			try{
				result.add(Rule.fromString(line,transformSymbol,delimiter));
			}catch(Exception e){
				
			}
		}
		return result;
	}
	//return one representative element of component in question
	public static Set<String> pruneUnusedNonterminals(Collection<String> nonterm, Collection<Rule> rules){
		Set<String> newlist= new HashSet<String>();
		for(Rule r: rules){
			if(nonterm.contains(r.getInput().get(0))) newlist.add(r.getInput().get(0));
			for(String s : r.getOutput()) {
				if(nonterm.contains(s))
					newlist.add(s);
			}
		}
		return newlist;
	}
	private static class TarjanCalculator{
		List<String> nonterm;
		List<Rule> unitrules;
		HashMap<String,Integer> indexgetter= new HashMap<String,Integer>();
		Stack<String> stack = new Stack<String>();
		int[] ixlist;
		int[] lowlinklist;
		Set<List<String>> components;
		int index=1;
		TarjanCalculator(Set<String> nonterm, Set<Rule> unitrules, Set<List<String>> components){
			this.nonterm=new java.util.ArrayList<String>(nonterm);
			this.unitrules=new java.util.ArrayList<Rule>(unitrules);
			this.components=components;
			ixlist=new int[nonterm.size()];
			lowlinklist= new int[ixlist.length];
			for(int i = 0; i<ixlist.length;i++){
				indexgetter.put(this.nonterm.get(i),i);
			}
		}
		void run(){
			for(String v: nonterm){
				find(v);
			}
		}
		
		private void find(String v){
			int rawv=indexgetter.get(v);
			ixlist[rawv]=index;
			lowlinklist[rawv]=index;
			index++;
			stack.push(v);
			for(Rule r : unitrules){
				//assume all rules are unit rules
				//if(r.getInput().size()!=1 || r.getOutput().size()!=1) continue;
				if(!r.getInput().get(0).equals(v)) continue;
				String w=r.getOutput().get(0);
				if(!nonterm.contains(w)) continue;
				int raww=indexgetter.get(w);
				if(ixlist[raww]==0) {
					find(w);
					//take minimum
					if(lowlinklist[raww]<lowlinklist[rawv]) lowlinklist[rawv]=lowlinklist[raww];
				}
				else if (stack.contains(w)){
					//w is in stack-> in the same component
					if(ixlist[raww]<lowlinklist[rawv])lowlinklist[rawv]=ixlist[raww];
				}
			}
			if(lowlinklist[rawv]==ixlist[rawv]){
				String w;
				List<String> comp=new java.util.ArrayList<String>();
				do{
					w=stack.pop();
					comp.add(w);
				} while(!v.equals(w));
				components.add(comp);
			}
		}
	}

	public static Set<List<String>> tarjan(Set<String> nonterm, Set<Rule> unitrules){
		
		Set<List<String>> components = new HashSet<List<String>>();
		(new TarjanCalculator(nonterm,unitrules, components)).run();
		return components;
	}
	public static final int EQUAL=0;
	public static final int NOT_COMPARABLE=-1;
	public static final int ONE_GREATER=1;
	public static final int OTHER_GREATER=2;
	/**
	 * Checks whether the nodes of <code>one</code> is more general than those of <code>other</code>, according to the rules from <code>unitrules</code>.
	 * While leaving out non-unit rules improves speed, this is not strictly necessary.
	 * @param one
	 * @param other
	 * @param unitrules
	 * @return
	 */
	public static int treeCompare(Tree<String> one, Tree<String> other, Set<Rule> unitrules){
		int size=one.getChildren().size();
		if(size!=other.getChildren().size()) return NOT_COMPARABLE;
		int upTillNow=0;
		for(int i = 0;i<size;i++){
			int rec=treeCompare(one.getChildren().get(i),other.getChildren().get(i),unitrules);
			if(rec==EQUAL) continue;
			if(rec==NOT_COMPARABLE || (upTillNow!=0 && rec!=upTillNow)) return NOT_COMPARABLE;
			upTillNow=rec;
		}
		//compare the values of the root nodes
		if(one.value().equals(other.value())) return upTillNow;
		Rule r1=new Rule(one.value(),other.value());
		Rule r2=new Rule(other.value(),one.value());
		if(unitrules.contains(r1)) return ONE_GREATER;
		else if(unitrules.contains(r2)) return OTHER_GREATER;
		return NOT_COMPARABLE;
	}
//	public static Tree<String> essentialDistinguish(Tree<String> a, Tree<String> b, Set<Rule> unitrules){
//		int size=a.getChildren().size();
//		if(size!=b.getChildren().size()) return null;
//		//distinguish/generalise children
//		for(int i = 0;i<size;i++){
//			Tree<String> t = essentialDistinguish(a.getChildren().get(i),b.getChildren().get(i),unitrules);
//			if(t==null) return null;
//			a.setChild(i,t);
//			b.setChild(i,t);
//		}
//		//compare the values of the root nodes
//		if(a.value().equals(b.value())) return a;
//		Rule r1=new Rule(a.value(),b.value());
//		Rule r2=new Rule(b.value(),a.value());
//		if(unitrules.contains(r1)) return a;
//		else if(unitrules.contains(r2)) return b;
//		return null;
//	}
	public static SyntaxForest reduce(SyntaxForest f,Set<Rule> rules){
		if(f.size()==0) return f;
		SyntaxForest res=new SyntaxForest();
		for(Tree<String> t: f){
			boolean isNew=true;
			for(Tree<String> other: new SyntaxForest(res)){
				int comp=treeCompare(t,other,rules);
				if(comp==OTHER_GREATER){
					isNew=false;
					break;
				}
				if(comp==ONE_GREATER){
					res.remove(other);
					break;
				}
			}
			if(isNew) res.add(t);
		}
		return res;
	}
	
	public static final Map<String,String> UNIVERSAL_IDENTITY = new Map<String,String>() {

		@Override
		public void clear() {
		}

		@Override
		public boolean containsKey(Object key) {
			return true;
		}

		@Override
		public boolean containsValue(Object value) {
			return true;
		}

		@Override
		public Set<java.util.Map.Entry<String, String>> entrySet() {
			return null;
		}

		@Override
		public String get(Object key) {
			// TODO Auto-generated method stub
			return (String)key;
		}

		@Override
		public boolean isEmpty() {
			return false;
		}

		@Override
		public Set<String> keySet() {
			return null;
		}

		@Override
		public String put(String key, String value) {
			return null;
		}

		@Override
		public void putAll(Map<? extends String, ? extends String> m) {
			
		}

		@Override
		public String remove(Object key) {
			return null;
		}

		@Override
		public int size() {
			return 0;
		}

		@Override
		public Collection<String> values() {
			return null;
		}			
	};
	
	
}
