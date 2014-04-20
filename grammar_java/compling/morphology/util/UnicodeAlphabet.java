package compling.morphology.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

public class UnicodeAlphabet implements Set<String> {
	@Override
	public boolean add(String e) {
		return false;
	}

	@Override
	public boolean addAll(Collection<? extends String> c) {
		return false;
	}

	@Override
	public void clear() {
		throw new UnsupportedOperationException("No can do.");
	}

	@Override
	public boolean contains(Object o) {
		if(!(o instanceof String)) return false;
		String s = (String) o;
		return s.length()==1;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		for(Object o : c){
			if(!contains(o)) return false;
		}
		return true;
	}

	@Override
	public boolean isEmpty() {
		return false;
	}

	@Override
	public Iterator<String> iterator() {
		return new Iterator<String>() {
			private int counter=0;
			@Override
			public boolean hasNext() {
				return counter<0xFFFF;
			}

			@Override
			public String next() {
				return Character.toString((char) counter++);
			}

			@Override
			public void remove() {
				
			}
			
		};
	}

	@Override
	public boolean remove(Object o) {
		return false;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		return false;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		return false;
	}

	@Override
	public int size() {
		return 0x10000;
	}

	@Override
	public Object[] toArray() {
		throw new UnsupportedOperationException("I'm not converting all of Unicode to an array. Rewrite your code, please.");
	}

	@Override
	public <T> T[] toArray(T[] a) {
		throw new UnsupportedOperationException("I'm not converting all of Unicode to an array. Rewrite your code, please.");
	}

}
