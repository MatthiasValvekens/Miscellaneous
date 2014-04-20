package compling.grammar.util;

public class BacklinkedTree<T> extends Tree<T> {
	private BacklinkedTree<T> parent;
	public BacklinkedTree(BacklinkedTree<T> parent, T value) {
		super(value);
		this.parent=parent;
	}
	public BacklinkedTree<T> getParent(){
		return parent;
	}
	public void addChild(Tree<T> t){
		if(!(t instanceof BacklinkedTree<?>)) throw new IllegalArgumentException("Types not compatible.");
		BacklinkedTree<T> child=(BacklinkedTree<T>) t;
		if(child.parent!=null) throw new IllegalStateException("Can only set parent once.");
		child.parent=this;
		super.addChild(t);
	}
	public void addChild(T val){
		BacklinkedTree<T> child=new BacklinkedTree<T>(this,val);
		super.addChild(child);
	}
	public boolean equals(Object o){
		if(!(o instanceof BacklinkedTree<?>)) return false;
		if(!super.equals(o)) return false;
		return parent.equals(((BacklinkedTree<?>) o).parent);
	}
	

}
