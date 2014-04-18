package grammar.util;
import java.util.ArrayList;
import java.util.Iterator;

public class SyntaxForest extends ArrayList<Tree<String>>{
	public SyntaxForest(){
		super();
	}
	public SyntaxForest(int capacity) {
		super(capacity);
	}
	public String toDot(String graphname, String... labelopts){
		StringBuilder sb = new StringBuilder("digraph "+graphname+" {\n");
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