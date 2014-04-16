package grammar;

import java.util.Set;

public abstract class ContextFreeGrammar extends Grammar {

	public ContextFreeGrammar(Set<String> terminal, Set<String> nonterminal,
			Set<Rule> rules) {
		super(terminal, nonterminal, rules);
	}
	public ContextFreeGrammar(Set<String> terminal, Set<String> nonterminal,
			Set<Rule> rules, String start) {
		super(terminal, nonterminal, rules, start);
	}

	@Override
	public boolean isValidRule(Rule r) {
		if(r.getInput().size()!=1) return false;
		return super.isValidRule(r);
	}

}
