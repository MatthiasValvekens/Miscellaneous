package grammar.util;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

public class SyntaxForest extends ArrayList<Tree<String>>{
	public SyntaxForest(){
		super();
	}
	public SyntaxForest(int capacity) {
		super(capacity);
	}
	@Override
	public boolean add(Tree<String> element) {
		if(contains(element)) return false;
		return super.add(element);
	}
	@Override
	public boolean addAll(Collection<? extends Tree<String>> els){
		for(Tree<String> e : els){
			if(contains(e)) return false;
		}
		return super.addAll(els);
	}
	public String toDot(String graphname, String... labelopts){
		// XXX hardcoded font 
		StringBuilder sb = new StringBuilder("digraph {\ncharset=\"UTF-8\";\ngraph [fontname=\"MS Mincho\",labelloc=\"t\",label=\""+graphname+"\"];\n");
		StringBuilder opts=new StringBuilder();
		for(String op:labelopts){
			if(op.contains("=") && !op.contains("color")) opts.append(op+" ");
		}
		Iterator<Integer> markergen=new Iterator<Integer>(){
			int counter=0;
			@Override
			public boolean hasNext() {
				return true;
			}

			@Override
			public Integer next() {
				return counter++;
			}

			@Override
			public void remove() {
				
			}
		};
		
		
		for(int i = 0; i<size();i++){
			int rootkey=markergen.next();
			sb.append(rootkey+" [label=\""+get(i).value()+"\" "+opts+"];\n");
			sb.append(get(i).dotbody(rootkey,markergen,labelopts));
		}
		sb.append("}");
		return sb.toString();
	}
	private static final long serialVersionUID = 3049544206397464398L;
	
}
