package grammar.util;

import java.util.Set;

public interface SymbolGenerator {
	public String generateNewSymbol(String old, Set<String> term,Set<String> nonterm);
}
