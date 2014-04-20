package compling.grammar.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
public class Tree<T> {
	private final T value;
	private final List<Tree<T>> children;
	private boolean valid=true;

	public List<Tree<T>> getChildren(){
		return Collections.unmodifiableList(children);
	}
	public boolean isValid(){
		return valid;
	}
	public void addChild(T value){
		if(!valid) throw new IllegalStateException("Tree has been destroyed.");
		children.add(new Tree<T>(value));
	}
	public void setChild(int index,Tree<T> t){
		if(!valid || !t.valid) throw new IllegalStateException("Tree has been destroyed.");
		children.set(index,t);
	}
	public void addChild(Tree<T> t) throws IllegalArgumentException{
		if(!valid || !t.valid) throw new IllegalStateException("Tree has been destroyed.");
		children.add(t);
	}
	public void removeChild(Tree<T> child){
		child.destroy();
		children.remove(child);
	}
	public T value(){
		return value;
	}
	public Tree(T value){
		this.value=value;
		children=new ArrayList<Tree<T>>();
	}

	public void destroy(){
		valid=false;
		for(Tree<T> t: children){
			removeChild(t);
		}
	}
	public String toString(){
		return toString(false);	
	}
	public String toString(boolean leavesOnly){
		if(children.size()==0) return value.toString();
		StringBuilder sb=new StringBuilder();
		String delim="";
		for(Tree<T> child: children) {
			sb.append(delim+child.toString());
			delim=",";
		}
		String result="("+sb.toString()+")";
		if(!leavesOnly)
			return value.toString()+"-> "+result;
		else return result;
	}
	public String toDot(String graphname, String... labelopts){
		// XXX:hardcoded font
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
				return ++counter;
			}

			@Override
			public void remove() {
				
			}
		};
		sb.append("0 [label=\""+value+"\" "+opts+"];\n");
		sb.append(dotbody(0,markergen,labelopts));
		sb.append("}");
		return sb.toString();
	}
	String dotbody(int myindex,Iterator<Integer> gen, String... labelopts){
		StringBuilder sb=new StringBuilder();
		StringBuilder opts=new StringBuilder();
		for(String op:labelopts){
			if(op.contains("=") && !op.contains("color")) opts.append(op+" ");
		}
		for(Tree<T> c:children){
			int childlabel=gen.next();
			sb.append(childlabel+"[label=\""+c.value+"\""+((c.children.size()!=0)? "": " color=red fontcolor=red ")+opts+"];\n");
			sb.append(myindex+"->"+childlabel+";\n");
			sb.append(c.dotbody(childlabel, gen,labelopts));
		}
		return sb.toString();
	}
	
	public boolean equals(Object o){
		if(o==this) return true;
		if(!(o instanceof Tree<?>))return false;
		Tree<?> t=(Tree<?>) o;
		
		return t.value.equals(value)&&t.children.equals(children);
	}
	public int hashCode(){
		return value.hashCode()*31+children.hashCode();
	}
}
