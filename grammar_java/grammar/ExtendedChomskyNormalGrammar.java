package grammar;

import grammar.util.Pair;
import grammar.util.Partitioner;
import grammar.util.SyntaxForest;
import grammar.util.Partitioner.PartitionTriple;
import grammar.util.Tree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;


public class ExtendedChomskyNormalGrammar extends ContextFreeGrammar {
	public ExtendedChomskyNormalGrammar(Set<String> terminal,
			Set<String> nonterminal, Set<Rule> rules, Set<String> start) {
		super(terminal, nonterminal, rules, start);
	}

	public ExtendedChomskyNormalGrammar(Set<String> terminal,
			Set<String> nonterminal, Set<Rule> rules, String start) {
		super(terminal,nonterminal,rules,start);
	}
	@Override
	public boolean admits(String[] tokens) {
		return parse(Arrays.asList(tokens))!=null;
	}
	public SyntaxForest allParses(List<String> input/*, boolean alwaysPropagateUnits*/){
		// TODO implement alwayspropagateunits without somehow kludging up the whole thing
		//extended CYK
		final ArrayList<String> nonterm=new ArrayList<String>();//we need to fix an order on the nonterm symbols
		nonterm.addAll(getNonterminalSymbols());
		int inputsize=input.size();
		final SyntaxForest[][][] P=new SyntaxForest[inputsize][inputsize][nonterm.size()];
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
					SyntaxForest f =new SyntaxForest();
					f.add(t);
					P[i][0][lut.get(r.getInput().get(0))]=f;
					
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
		for(int spanlen=2;spanlen<=inputsize;spanlen++){
			final int maxsym=Math.min(maxbranch,spanlen);
			for(int start=0;start<inputsize-spanlen+1; start++){

//				Iterator<Integer> nextcount=new Iterator<Integer>(){
//					private int counter=1;
//					public boolean hasNext(){
//						
//					}
//				};
//				if(alwaysPropagateUnits){
//					// ensure propagation of unit rules after every step
//					nextcount=new Iterator<Integer>() {
//						private int counter=1;
//						private int status=1;
//						private boolean hasNext=true;
//						@Override
//						public boolean hasNext() {
//							return hasNext;
//						}
//	
//						@Override
//						public Integer next() {
//							if(maxsym==1){
//								hasNext=false;
//								return 1;
//							}
//							if(status==1) {
//								counter++;
//								status=counter;
//								return counter;
//							}
//							else {
//								if(counter==maxsym){
//									hasNext=false;
//								}
//								return (status=1);
//							}
//						}
//	
//						@Override
//						public void remove() {
//							
//						}
//						
//					};
//				}
				for(int symbolcount=2;symbolcount<=Math.min(maxsym,spanlen);symbolcount++){
				//while(nextcount.hasNext()){
					//int symbolcount=nextcount.next();
					//when propagating unit rules we have to be extra careful
					//Set<Pair<Rule,SyntaxForest>> usedunits=new HashSet<Pair<Rule,SyntaxForest>>();
					
						//we need to split the subsequence of symbols [j,..., j+spanlen-1] into symbolcount parts, and consider every such partition
					Partitioner<String> partitioner=new Partitioner<String>(input.subList(start,start+spanlen),symbolcount);
					for(PartitionTriple<String> triple:partitioner){
						for(Rule r: rulecat.get(symbolcount-1)){

						//r: N_I -> N_O1 N_O2 N_O3... N_O(symbolcount-1)
							//look up the indices of the relevant variables
							int inputix=lut.get(r.getInput().get(0));

							int[] outputix=new int[symbolcount];
							for(int i=0;i<symbolcount;i++)outputix[i]=lut.get(r.getOutput().get(i));
							//have we had this one already? (check only necessary when propagating unit rules)
							//if(symbolcount==1&&P[start][spanlen-1][inputix]!=null && usedunits.contains(new Pair<Rule,SyntaxForest>(r,P[start][spanlen-1][inputix]))) continue;
							//if the first part can be generated from N_B and the second can be generated from N_C
							//then the whole substring can be generated from N_A by means of rule r
							SyntaxForest[] children = new SyntaxForest[symbolcount];
							
							boolean isGenerated=true;
							for(int i =0;i<symbolcount;i++){
								int partstart=triple.starts.get(i);
								int partend=i!=symbolcount-1 ? triple.starts.get(i+1) : spanlen;
								children[i] = P[start+partstart][partend-partstart-1][outputix[i]];
//									if(inputix==lut.get("S")&&spanlen==5/*&&r.toString().equals("S->IMP")*/){
//										System.out.println(children[i]+" "+r+" "+nonterm.get(outputix[i])+outputix[i]+" "+start+" "+symbolcount+" "+(start+partstart)+" "+(partend-partstart-1));
//									}
								if(children[i]==null){
									isGenerated=false;
									break;
								}
							}

							if(isGenerated) {
								//add ALL the options!
								int posscount=1;
								final int[] choicedomain=new int[children.length];
								for(int i =0; i<children.length;i++) {
									int size=children[i].size();
									posscount*=size;
									choicedomain[i]=size;
								}
								final int totaloptions=posscount;
								//totally awesome option selector
								Iterator<int[]> selector = new Iterator<int[]>(){
									int counter=0;
									@Override
									public boolean hasNext() {
										return counter<totaloptions;
									}

									@Override
									public int[] next() {
										int[] res=new int[choicedomain.length];
										int curcount=counter;
										for(int i = 0; i<res.length;i++){
											res[i]=curcount%choicedomain[i];
											curcount/=choicedomain[i];
										}
										counter++;
										return res;
									}

									@Override
									public void remove() {
										
									}
								};

								SyntaxForest alloptions=new SyntaxForest(posscount);
								while(selector.hasNext()){
									int[] selection=selector.next();
									SyntaxTree t= new SyntaxTree(r.getInput().get(0));
									for(int i = 0; i<children.length;i++){
										t.addChild(children[i].get(selection[i]));
									}
									alloptions.add(t);
								}
								if(P[start][spanlen-1][inputix]==null){
									P[start][spanlen-1][inputix]=new SyntaxForest();
								}
								P[start][spanlen-1][inputix].addAll(alloptions);
								//System.out.println(P[start][spanlen-1][inputix]);
								//mark unit rule-forest pair as used for this round, we won't need it again, assuming there are no circles (even then, this is the way to go)
								// TODO : enforce this assumption
								//if(symbolcount==1) usedunits.add(new Pair<Rule,SyntaxForest>(r,P[start][spanlen-1][inputix]));
							}
						}
					}
				
				}
			}
		}
		SyntaxForest results= new SyntaxForest();
		for(String start: getStartSymbols()){ 
			SyntaxForest f = P[0][inputsize-1][lut.get(start)];
			if(f!=null) results.addAll(f);
		}
		return !results.isEmpty()? results : null;
	}
	@Override
	public Tree<String> parse(List<String> input) {
		SyntaxForest f = this.allParses(input);
		return f!=null? f.get(0) : null;
	}
	@Override
	public boolean isValidRule(Rule r){
		if(!super.isValidRule(r)) return false;
		
//		List<String> inp=r.getInput();
		List<String> outp=r.getOutput();
//		//Start symbol only allowed when output is empty.
//		if(inp.get(0).equals(getStartSymbol()) && outp.size()!=0) return false;
		if(outp.size()==0) return false;
		//only terminal symbols allowed in A->a replacements
		if(outp.size()==1 && !getTerminalSymbols().contains(outp.get(0))) return false;
		if(outp.size()>1){
			for(String s: outp){
				if(!getNonterminalSymbols().contains(s)) return false;
			}
		}
		return true;
	}
}
