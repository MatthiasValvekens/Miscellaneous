package grammar.util;
import grammar.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Stack;
import java.util.Set;
import java.util.HashSet;
public class GrammarUtils {

	public static <T extends Grammar> T renameSymbol(T grammar, String oldsym, String newsym, Class<T> buildAs){
		
		// TODO: implement this
		return null;
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
	
	
}
