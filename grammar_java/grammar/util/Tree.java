package grammar.util;

import java.util.ArrayList;
import java.util.List;
public class Tree<T> {
	private Tree<T> parent;
	private final T value;
	private final List<Tree<T>> children;
	private boolean valid=true;
	private final boolean valueEquivalence=true;
	public Tree<T> getParent(){
		return parent;
	}
	public List<Tree<T>> getChildren(){
		return new ArrayList<Tree<T>>(children);
	}

	protected void setParent(Tree<T> t) throws IllegalStateException{
		if(!valid) throw new IllegalStateException("Tree has been destroyed.");
		if(this.parent!=null) {
			if(valueEquivalence && this.parent.equals(t))
				return;
			else throw new IllegalStateException("Can only attach entire trees. Tried to attach "+this+" which has parent "+this.parent+" to "+t);
		}
		else parent=t;
	}
	public void addChild(T value){
		if(!valid) throw new IllegalStateException("Tree has been destroyed.");
		children.add(new Tree<T>(this,value));
	}

	public void addChild(Tree<T> t) throws IllegalArgumentException{
		if(!valid) throw new IllegalStateException("Tree has been destroyed.");
		t.setParent(this);
		children.add(t);
	}
	public void removeChild(Tree<T> child){
		child.destroy();
		children.remove(child);
	}
	public T value(){
		return value;
	}
	public Tree(Tree<T> parent, T value){
		this.parent=parent;
		this.value=value;
		children=new ArrayList<Tree<T>>();
	}
	public Tree(T value){
		this(null, value);
	}
	public void destroy(){
		valid=false;
		this.parent=null;
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
	public boolean equals(Object o){
		if(!(o instanceof Tree<?>))return false;
		Tree<?> t=(Tree<?>) o;
		
		return t.value.equals(value)&&t.children.equals(children);
	}
}
