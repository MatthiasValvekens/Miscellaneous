package grammar;

import grammar.util.Tree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ChomskyNormalGrammar extends ContextFreeGrammar {
	//operates on the assumption that the grammar in question is meant to be Chomsky normal
	public static ChomskyNormalGrammar getSymbolsFromRules(Set<Rule> rules,String start){
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
	@Override
	public boolean admits(String[] input) {
		return parse(input)!=null;
	}
	public Tree<String> parse(String[] input){
		return parse(Arrays.asList(input));
	}
	public Tree<String> parse(List<String> input){
		final ArrayList<String> nonterm=new ArrayList<String>();//we need to fix an order on the nonterm symbols
		nonterm.addAll(getNonterminalSymbols());
		int inputsize=input.size();
		final SyntaxTree[][][] P=new SyntaxTree[inputsize][inputsize][nonterm.size()];
		//make a lookup table for nonterm symbol indices
		Map<String, Integer> lut=new HashMap<String,Integer>();
		for(int i=0;i<nonterm.size();i++){
			lut.put(nonterm.get(i),i);
		}
		//process unit rules (leaves of tree)
		for(int i=0;i<inputsize;i++){
			//For the ith symbol of the input, we need to find all rules transforming a nonterminal into that symbol
			for(Rule r: getRules()){
				String term=input.get(i);
				if(r.getOutput().size()==1 && r.getOutput().get(0).equals(term)) {
					SyntaxTree t =new SyntaxTree(null,r.getInput().get(0));
					t.addChild(term);
					P[i][0][lut.get(r.getInput().get(0))]=t;
				}
			}
		}
		//process compound rules
		for(int spanlen=2;spanlen<=inputsize;spanlen++){
			for(int start=0;start<inputsize-spanlen+1; start++){
				//we need to split the subsequence of symbols [j,..., j+spanlen-1] into two parts, and consider every such partition
				for(int part=1;part<=spanlen-1;part++){
					for(Rule r: getRules()){
						//r: N_A -> N_B N_C
						if(r.getOutput().size()==2){
							int A=lut.get(r.getInput().get(0));
							int B=lut.get(r.getOutput().get(0));
							int C=lut.get(r.getOutput().get(1));
							//if the first part can be generated from N_B and the second can be generated from N_C
							//then the whole substring can be generated from N_A by means of rule r
							SyntaxTree t1=P[start][part-1][B];
							SyntaxTree t2=P[start+part][spanlen-part-1][C];

							if(t1!=null && t2!=null) {
								SyntaxTree t= new SyntaxTree(r.getInput().get(0));
								t.addChild(t1);
								t.addChild(t2);
								P[start][spanlen-1][A]=t;
							}
						}
					}
				}
			}
		}
		return P[0][inputsize-1][lut.get(getStartSymbol())];
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
