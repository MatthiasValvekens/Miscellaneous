package compling.grammar.util;

public class Pair<T,E> {
	public final T X;
	public final E Y;
	private final int hashCode;
	public Pair(T X, E Y){
		this.X=X;
		this.Y=Y;
		hashCode=X.hashCode()*31+Y.hashCode();
	}
	public boolean equals(Object o){
		if(o==this) return true;
		if(!(o instanceof Pair<?,?>)) return false;
		Pair<?,?> p=(Pair<?,?>) o;
		return p.X.equals(X) && p.Y.equals(Y);
	}
	public String toString(){
		return "<"+X+","+Y+">";
	}
	public int hashCode(){
		return hashCode;
	}
}
