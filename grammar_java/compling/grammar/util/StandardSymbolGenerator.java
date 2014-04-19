package compling.grammar.util;

import java.util.Set;
import java.util.Random;

public class StandardSymbolGenerator implements SymbolGenerator {
	private Random rng;
	public StandardSymbolGenerator(Random r){
		this.rng=r;
	}
	public StandardSymbolGenerator(){
		this(new Random());
	}
	public String generateNewSymbol(String old, Set<String> term,Set<String> nonterm){
		String newsymb;
		int oldhash=old.hashCode();
		do {
			int r = rng.nextInt();
			newsymb=old+"_"+oldhash+"_"+r;
		} while(term.contains(newsymb) || nonterm.contains(newsymb));
		return newsymb;
	}
}