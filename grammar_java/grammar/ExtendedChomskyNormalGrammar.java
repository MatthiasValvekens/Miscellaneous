package grammar;

import grammar.util.Partitioner;
import grammar.util.Partitioner.PartitionTriple;
import grammar.util.Tree;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;


public class ExtendedChomskyNormalGrammar extends ContextFreeGrammar {
	public ExtendedChomskyNormalGrammar(Set<String> terminal,
			Set<String> nonterminal, Set<Rule> rules, String start) {
		super(terminal, nonterminal, rules, start);
	}

	@Override
	public boolean admits(String[] tokens) {
		return false;
	}

	@Override
	public Tree<String> parse(List<String> input) {
		//extended CYK
		final ArrayList<String> nonterm=new ArrayList<String>();//we need to fix an order on the nonterm symbols
		nonterm.addAll(getNonterminalSymbols());
		int inputsize=input.size();
		final SyntaxTree[][][] P=new SyntaxTree[inputsize][inputsize][nonterm.size()];
		//compute maximal branching factor
		int maxbranch=0;
		for(Rule r: getRules()){
			int len=r.getOutput().size();
			if(len>maxbranch) maxbranch=len;
		}
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
				if(!getTerminalSymbols().contains(term)) throw new IllegalArgumentException("Unknown terminal symbol "+term);
				if(r.getOutput().size()==1 && r.getOutput().get(0).equals(term)) {
					SyntaxTree t =new SyntaxTree(null,r.getInput().get(0));
					t.addChild(term);
					P[i][0][lut.get(r.getInput().get(0))]=t;
				}
			}
		}
		//preprocess "true" rules into size categories
		List<List<Rule>> rulecat=new ArrayList<List<Rule>>(maxbranch);
		for(int i = 0;i<maxbranch;i++) rulecat.add(new ArrayList<Rule>());
		for(Rule r: getRules()){
			int outputsize=r.getOutput().size();
			if(outputsize==1){
				if(getNonterminalSymbols().contains(r.getOutput().get(0))) rulecat.get(0).add(r);
			}
			else rulecat.get(outputsize-1).add(r);
		}
		//process compound rules
		for(int spanlen=1;spanlen<=inputsize;spanlen++){
			int maxsym=Math.min(maxbranch,spanlen);
			for(int start=0;start<inputsize-spanlen+1; start++){
				for(int symbolcount=1;symbolcount<=Math.min(maxsym,spanlen);symbolcount++){
					//we need to split the subsequence of symbols [j,..., j+spanlen-1] into symbolcount parts, and consider every such partition
					Partitioner<String> partitioner=new Partitioner<String>(input.subList(start,start+spanlen),symbolcount);
					for(PartitionTriple<String> triple:partitioner){
						for(Rule r: rulecat.get(symbolcount-1)){
						//r: N_I -> N_O1 N_O2 N_O3... N_O(symbolcount-1)
							//look up the indices of the relevant variables
							int inputix=lut.get(r.getInput().get(0));
							int[] outputix=new int[symbolcount];
							for(int i=0;i<symbolcount;i++)outputix[i]=lut.get(r.getOutput().get(i));
							//if the first part can be generated from N_B and the second can be generated from N_C
							//then the whole substring can be generated from N_A by means of rule r
							SyntaxTree[] children = new SyntaxTree[symbolcount];
							boolean isGenerated=true;
							for(int i =0;i<symbolcount;i++){
								int partstart=triple.starts.get(i);
								int partend=i!=symbolcount-1 ? triple.starts.get(i+1) : spanlen;
								children[i] = P[start+partstart][partend-partstart-1][outputix[i]];
								if(children[i]==null){
									isGenerated=false;
									break;
								}
							}

							if(isGenerated) {
								SyntaxTree t= new SyntaxTree(r.getInput().get(0));
								for(SyntaxTree c: children) t.addChild(c);
								P[start][spanlen-1][inputix]=t;
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
//		//Start symbol only allowed when output is empty.
//		if(inp.get(0).equals(getStartSymbol()) && outp.size()!=0) return false;
		if(outp.size()==0) return false;
		
		if(outp.size()>1){
			for(String s: outp){
				if(!getNonterminalSymbols().contains(s)) return false;
			}
		}
		return true;
	}
}
