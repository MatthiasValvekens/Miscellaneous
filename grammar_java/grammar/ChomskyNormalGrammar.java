package grammar;

import grammar.util.Tree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ChomskyNormalGrammar extends ContextFreeGrammar {
	private class SyntaxTree extends Tree<String>{
		//nodes in syntax trees are unaware of their parents because this complicates the algorithm 
		//while at the same time being totally irrelevant to the analysis
		
		public SyntaxTree(Tree<String> parent, String value) {
			super(null, value);
		}
		public SyntaxTree(String value){
			super(value);
		}
		
		
		@Override
		public final void setParent(Tree<String> parent){
			
		}
	}
	public ChomskyNormalGrammar(Set<String> terminal,
			Set<String> nonterminal, Set<Rule> rules, String start) {
		super(terminal, nonterminal, rules, start);
	}
	public ChomskyNormalGrammar(Set<String> terminal,
			Set<String> nonterminal, Set<Rule> rules) {
		super(terminal, nonterminal, rules);
	}
	@Override
	public boolean admits(String[] input) {
		return parse(input)!=null;
	}
	public Tree<String> parse(String[] input){
		final ArrayList<String> nonterm=new ArrayList<String>();//we need to fix an order on the nonterm symbols
		nonterm.addAll(getNonterminalSymbols());
		final SyntaxTree[][][] P=new SyntaxTree[input.length][input.length][nonterm.size()];
		//make a lookup table for nonterm symbol indices
		Map<String, Integer> lut=new HashMap<String,Integer>();
		for(int i=0;i<nonterm.size();i++){
			lut.put(nonterm.get(i),i);
		}
		//process unit rules (leaves of tree)
		for(int i=0;i<input.length;i++){
			//For the ith symbol of the input, we need to find all rules transforming a nonterminal into that symbol
			for(Rule r: getRules()){
				if(r.getOutput().size()==1 && r.getOutput().get(0).equals(input[i])) {
					SyntaxTree t =new SyntaxTree(null,r.getInput().get(0));
					t.addChild(input[i]);
					P[i][0][lut.get(r.getInput().get(0))]=t;
				}
			}
		}
		//process compound rules
		for(int spanlen=2;spanlen<=input.length;spanlen++){
			for(int start=0;start<input.length-spanlen+1; start++){
				//we need to split the subsequence of symbols [j,..., j+spanlen-1] into two parts, and consider every such partition
				for(int part=1;part<=spanlen-1;part++){
					for(Rule r: getRules()){
						//r: N_A -> N_B N_C
						if(r.getOutput().size()==2){
							int A=lut.get(r.getInput().get(0));
							int B=lut.get(r.getOutput().get(0));
							int C=lut.get(r.getOutput().get(1));
//							if(r.toString().equals("VP->VP PP")||r.toString().equals("PP->P NP")){
//								System.err.println(r.toString()+":");
//								System.err.println(A+" "+B+" "+C+" "+P[start][part-1][B]);
//								System.err.println(P[start+part][spanlen-part-1][C]);
//							}
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
		return P[0][input.length-1][lut.get(getStartSymbol())];
	}
	
	@Override
	public boolean isValidRule(Rule r){
		if(!super.isValidRule(r)) return false;
//		List<String> inp=r.getInput();
		List<String> outp=r.getOutput();
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
