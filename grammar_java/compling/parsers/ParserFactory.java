package compling.parsers;

import java.util.HashSet;
import compling.grammar.util.*;
import java.util.Set;
import java.io.*;

import compling.grammar.ContextFreeGrammar;
import compling.grammar.ExtendedChomskyNormalGrammar;
import compling.grammar.Grammar;
import compling.grammar.Rule;

public class ParserFactory {
	private static volatile PrintStream errorstream=System.err;
	
	public static synchronized void setErrorStream(PrintStream p){
		errorstream=p;
	}
	public static Parser createECNFParser(String syntax, String morphsetup, Set<String> alphabet, String transformSymbol, String delimiter, Set<String> start){
		Set<Rule> syntaxrules= new HashSet<Rule>();
		for(String rule : syntax.split("\n")) {
			try {
				syntaxrules.add(Rule.fromString(rule, transformSymbol, delimiter));
			} catch(IllegalArgumentException e) {
				errorstream.println("Warning: undecipherable rule "+rule);
			}
		}
		Set<String> nonterm=GrammarUtils.extractNonterminals(syntaxrules);
		Set<String> term=new HashSet<String>(); //empty on purpose
		ContextFreeGrammar basegram=new ExtendedChomskyNormalGrammar(term, nonterm, syntaxrules, start);
		//refine
		ContextFreeGrammar syntaxgram=GrammarUtils.processUnitRules(basegram, false, false);
		return null;
	}
}
